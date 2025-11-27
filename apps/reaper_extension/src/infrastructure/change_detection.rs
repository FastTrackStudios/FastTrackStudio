//! Change Detection Infrastructure
//!
//! Uses ChangeDetectionMiddleware + ControlSurfaceRxMiddleware to listen for ALL changes
//! (tracks, play state, tempo, bookmarks, etc.) reactively instead of polling.
//! This is much more efficient than polling every 100ms.

use reaper_high::{
    ChangeDetectionMiddleware, ControlSurfaceEvent, ControlSurfaceMiddleware,
    MiddlewareControlSurface,
};
use reaper_medium::ReaperSession;
use reaper_rx::{ControlSurfaceRx, ControlSurfaceRxMiddleware};
use tracing::{debug, info, trace, warn};

/// Control surface middleware that processes ALL change events reactively
/// 
/// Note: This struct contains `ControlSurfaceRx` which is NOT `Send`/`Sync`.
/// It should only be used on the main thread and not stored in thread-safe statics.
#[derive(Debug)]
pub struct ComprehensiveChangeDetectionMiddleware {
    change_detection: ChangeDetectionMiddleware,
    rx: ControlSurfaceRx,
    rx_middleware: ControlSurfaceRxMiddleware,
}

impl ComprehensiveChangeDetectionMiddleware {
    pub fn new() -> Self {
        let rx = ControlSurfaceRx::new();
        let rx_middleware = ControlSurfaceRxMiddleware::new(rx.clone());
        let change_detection = ChangeDetectionMiddleware::new();
        
        let mut middleware = Self {
            change_detection,
            rx,
            rx_middleware,
        };
        
        // Initialize the change detection middleware by calling reset
        // This captures the initial project/track/transport state
        middleware.change_detection.reset(|event| {
            trace!(event = ?event, "Initial change detection reset event");
            // We'll handle these events in run() after the middleware is fully constructed
        });
        
        // Now that middleware is constructed, we can process the reset events
        // by calling run() which will process any pending events
        middleware.change_detection.run(&mut |event| {
            trace!(event = ?event, "Processing initial reset event");
            middleware.rx_middleware.handle_change(event);
        });
        
        middleware
    }
    
    /// Get the ControlSurfaceRx instance for subscribing to reactive streams
    /// Note: This is only safe to call from the main thread (ControlSurfaceRx uses RefCell)
    pub fn rx(&self) -> &ControlSurfaceRx {
        &self.rx
    }
}

impl ControlSurfaceMiddleware for ComprehensiveChangeDetectionMiddleware {
    fn run(&mut self) {
        // Process any pending change events (polling-based detection for things like visibility)
        // run() takes &mut self and a closure that receives ChangeEvent
        self.change_detection.run(&mut |event| {
            trace!(event = ?event, "Change detection run event");
            // Convert ChangeEvent to reactive stream via rx middleware
            self.rx_middleware.handle_change(event);
        });
    }

    fn handle_event(&self, event: ControlSurfaceEvent) -> bool {
        // Process the control surface event through change detection middleware
        // This converts ControlSurfaceEvent into ChangeEvent, then to reactive streams
        self.change_detection.process(&event, |change_event| {
            trace!(
                control_event = ?event,
                change_event = ?change_event,
                "Processing change event"
            );
            // Convert ChangeEvent to reactive stream via rx middleware
            self.rx_middleware.handle_change(change_event);
        })
    }
}

/// Helper function to find the song index for a track's project
/// Returns None if the project doesn't match any song in the setlist
/// 
/// This is a synchronous helper that gets the project name from the track.
/// The actual setlist lookup is done asynchronously in broadcast_track_update_for_song.
fn get_project_name_for_track(track: &reaper_high::Track) -> Option<String> {
    use reaper_high::Reaper;
    use reaper_medium::ProjectRef;
    
    // Get the track's project
    let project = track.project();
    let medium_reaper = Reaper::get().medium_reaper();
    let project_raw = project.raw();
    
    // Find the project name by enumerating tabs
    let mut found_name = None;
    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            if result.project == project_raw {
                found_name = result.file_path.as_ref()
                    .and_then(|p| p.as_std_path().file_stem())
                    .and_then(|s| s.to_str())
                    .map(|s| s.to_string());
                break;
            }
        }
    }
    found_name
}

/// Helper function to broadcast SongTracks update for a track
/// Finds the song containing the track and broadcasts the updated tracks
fn broadcast_track_update_for_song(track: &reaper_high::Track) {
    use setlist::infra::stream::{get_broadcast_sender, get_state_provider, SetlistUpdateMessage};
    use tracing::warn;
    
    // Get the project name for this track
    let project_name = match get_project_name_for_track(track) {
        Some(name) => name,
        None => {
            // Can't determine project name - skip
            return;
        }
    };
    
    // Get the broadcast sender and state provider
    let broadcast_tx = match get_broadcast_sender() {
        Some(tx) => tx,
        None => {
            warn!("Broadcast sender not available yet - setlist stream not initialized");
            return;
        }
    };
    
    let state_provider = match get_state_provider() {
        Some(sp) => sp,
        None => {
            warn!("State provider not available yet - setlist stream not initialized");
            return;
        }
    };
    
    // Spawn async task to get setlist, find song, and broadcast
    // We use spawn instead of block_on to avoid deadlocks (we're in a reactive stream callback)
    if let Ok(handle) = tokio::runtime::Handle::try_current() {
        let project_name_clone = project_name.clone();
        handle.spawn(async move {
            if let Ok(setlist_api) = state_provider.get_setlist_api().await {
                let setlist = setlist_api.get_setlist();
                // Find the song index that matches this project name
                if let Some(song_index) = setlist.songs.iter().position(|song| {
                    song.project_name_from_metadata() == project_name_clone
                }) {
                    if let Some(song) = setlist.songs.get(song_index) {
                        if let Some(project) = &song.project {
                            let tracks = project.tracks().to_vec();
                            let update = SetlistUpdateMessage::SongTracks {
                                song_index,
                                tracks,
                            };
                            if broadcast_tx.send(update).is_err() {
                                warn!("Failed to broadcast track update - no subscribers");
                            }
                        }
                    }
                }
            }
        });
    } else {
        warn!("No tokio runtime handle available - cannot broadcast track update");
    }
}

/// Initialize reactive stream subscriptions for setlist updates
/// This subscribes to all track-related reactive streams and broadcasts updates
/// 
/// Note: This must be called from the main thread after the middleware is created
pub fn init_reactive_stream_subscriptions(rx: &ControlSurfaceRx) {
    use crate::implementation::tracks::{invalidate_track_cache, invalidate_all_track_caches};
    use setlist::infra::stream::{get_broadcast_sender, get_state_provider, SetlistUpdateMessage};
    use reaper_high::Reaper;
    use rxrust::prelude::*;
    
    // Subscribe to all track-related reactive streams and broadcast updates
    // Project switched - invalidate all caches and trigger full setlist update
    rx.project_switched().subscribe(move |_project| {
        debug!("Project switched - invalidating all caches");
        invalidate_all_track_caches();
        
        // Broadcast FullSetlist update
        if let Some(broadcast_tx) = get_broadcast_sender() {
            if let Some(state_provider) = get_state_provider() {
                if let Ok(handle) = tokio::runtime::Handle::try_current() {
                    handle.spawn(async move {
                        if let Ok(setlist_api) = state_provider.get_setlist_api().await {
                            let mut setlist = setlist_api.get_setlist().clone();
                            // Clear tracks from each song's project (tracks sent separately)
                            for song in &mut setlist.songs {
                                if let Some(ref mut project) = song.project {
                                    project.set_tracks(Vec::new());
                                }
                            }
                            let update = SetlistUpdateMessage::FullSetlist {
                                setlist,
                                active_song_index: setlist_api.active_song_index(),
                                active_section_index: setlist_api.active_section_index(),
                                active_slide_index: setlist_api.active_slide_index(),
                            };
                            if broadcast_tx.send(update).is_err() {
                                warn!("Failed to broadcast project switch update - no subscribers");
                            }
                        }
                    });
                }
            }
        }
    });
    
    // Track structure changes - invalidate cache and broadcast track updates
    rx.track_added().subscribe(move |track| {
        debug!(track = ?track, "Track added - invalidating cache and broadcasting update");
        let reaper = Reaper::get();
        let project = reaper.current_project();
        invalidate_track_cache(&project);
        broadcast_track_update_for_song(&track);
    });
    
    rx.track_removed().subscribe(move |track| {
        debug!(track = ?track, "Track removed - invalidating cache and broadcasting update");
        let reaper = Reaper::get();
        let project = reaper.current_project();
        invalidate_track_cache(&project);
        broadcast_track_update_for_song(&track);
    });
    
    rx.tracks_reordered().subscribe(move |project| {
        debug!(project = ?project, "Tracks reordered - invalidating cache and broadcasting update");
        invalidate_track_cache(&project);
        
        // Broadcast tracks for all songs in the current project
        // (tracks_reordered gives us the project, so we need to find all songs for that project)
        if let Some(broadcast_tx) = get_broadcast_sender() {
            if let Some(state_provider) = get_state_provider() {
                if let Ok(handle) = tokio::runtime::Handle::try_current() {
                    handle.spawn(async move {
                        if let Ok(setlist_api) = state_provider.get_setlist_api().await {
                            let setlist = setlist_api.get_setlist();
                            // Find project name
                            let medium_reaper = Reaper::get().medium_reaper();
                            let project_name = {
                                let mut found_name = None;
                                for i in 0..128u32 {
                                    if let Some(result) = medium_reaper.enum_projects(reaper_medium::ProjectRef::Tab(i), 512) {
                                        if result.project == project.raw() {
                                            found_name = result.file_path.as_ref()
                                                .and_then(|p| p.as_std_path().file_stem())
                                                .and_then(|s| s.to_str())
                                                .map(|s| s.to_string());
                                            break;
                                        }
                                    }
                                }
                                found_name
                            };
                            
                            if let Some(proj_name) = project_name {
                                // Find all songs matching this project and broadcast their tracks
                                for (song_index, song) in setlist.songs.iter().enumerate() {
                                    if song.project_name_from_metadata() == proj_name {
                                        if let Some(project_data) = &song.project {
                                            let tracks = project_data.tracks().to_vec();
                                            let update = SetlistUpdateMessage::SongTracks {
                                                song_index,
                                                tracks,
                                            };
                                            if broadcast_tx.send(update).is_err() {
                                                warn!("Failed to broadcast tracks reordered update - no subscribers");
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    });
                }
            }
        }
    });
    
    // Track property changes - broadcast track updates reactively
    rx.track_volume_changed().subscribe(move |track| {
        trace!(track = ?track, "Track volume changed - broadcasting update");
        broadcast_track_update_for_song(&track);
    });
    
    rx.track_pan_changed().subscribe(move |track| {
        trace!(track = ?track, "Track pan changed - broadcasting update");
        broadcast_track_update_for_song(&track);
    });
    
    rx.track_mute_changed().subscribe(move |track| {
        trace!(track = ?track, "Track mute changed - broadcasting update");
        broadcast_track_update_for_song(&track);
    });
    
    rx.track_solo_changed().subscribe(move |track| {
        trace!(track = ?track, "Track solo changed - broadcasting update");
        broadcast_track_update_for_song(&track);
    });
    
    rx.track_arm_changed().subscribe(move |track| {
        trace!(track = ?track, "Track arm changed - broadcasting update");
        broadcast_track_update_for_song(&track);
    });
    
    rx.track_name_changed().subscribe(move |track| {
        debug!(track = ?track, "Track name changed - broadcasting update");
        broadcast_track_update_for_song(&track);
    });
    
    rx.track_input_changed().subscribe(move |track| {
        trace!(track = ?track, "Track input changed - broadcasting update");
        broadcast_track_update_for_song(&track);
    });
    
    rx.track_input_monitoring_changed().subscribe(move |track| {
        trace!(track = ?track, "Track input monitoring changed - broadcasting update");
        broadcast_track_update_for_song(&track);
    });
    
    rx.track_automation_mode_changed().subscribe(move |track| {
        trace!(track = ?track, "Track automation mode changed - broadcasting update");
        broadcast_track_update_for_song(&track);
    });
    
    // Route changes - get track from route
    rx.track_route_volume_changed().subscribe(move |route| {
        trace!(route = ?route, "Track route volume changed - broadcasting update");
        let track = route.track();
        broadcast_track_update_for_song(&track);
    });
    
    rx.track_route_pan_changed().subscribe(move |route| {
        trace!(route = ?route, "Track route pan changed - broadcasting update");
        let track = route.track();
        broadcast_track_update_for_song(&track);
    });
    
    rx.receive_count_changed().subscribe(move |track| {
        trace!(track = ?track, "Receive count changed - broadcasting update");
        broadcast_track_update_for_song(&track);
    });
    
    rx.track_send_count_changed().subscribe(move |track| {
        trace!(track = ?track, "Track send count changed - broadcasting update");
        broadcast_track_update_for_song(&track);
    });
    
    rx.hardware_output_send_count_changed().subscribe(move |track| {
        trace!(track = ?track, "Hardware output send count changed - broadcasting update");
        broadcast_track_update_for_song(&track);
    });
    
    info!("✅ Initialized reactive stream subscriptions for all track-related events");
}

/// Initialize the reactive logger (deferred until first project is loaded)
/// This should be called after the first setlist update completes
/// 
/// Note: This must be called from the main thread and requires access to the middleware
pub fn init_deferred_reactive_logger(middleware: &ComprehensiveChangeDetectionMiddleware) {
    super::reactive_logger::init_reactive_stream_logger(middleware.rx());
    info!("✅ Deferred reactive stream logger initialized (after first project loaded)");
}

/// Initialize reactive streams for a middleware instance
/// 
/// This sets up subscriptions and logging for a ComprehensiveChangeDetectionMiddleware.
/// Should be called after creating the middleware, before registering the control surface.
/// 
/// Note: This must be called from the main thread (ControlSurfaceRx is not thread-safe)
/// 
/// Note: Logger initialization is deferred until the first project is loaded to avoid
/// RefCell borrow panics during initial setup.
pub fn init_middleware_reactive_streams(middleware: &ComprehensiveChangeDetectionMiddleware) {
    // Initialize reactive stream subscriptions (these are needed for broadcasting updates)
    init_reactive_stream_subscriptions(middleware.rx());
    
    // Logger initialization is deferred - will be called after first setlist update
    // This avoids RefCell borrow panics during initial project loading
}

/// Change detection infrastructure
#[derive(Debug)]
pub struct ChangeDetection {
    middleware: ComprehensiveChangeDetectionMiddleware,
    logger_initialized: std::cell::Cell<bool>,
}

impl ChangeDetection {
    /// Create a new change detection instance
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            middleware: ComprehensiveChangeDetectionMiddleware::new(),
            logger_initialized: std::cell::Cell::new(false),
        })
    }
    
    /// Initialize the reactive logger (deferred until first project is loaded)
    /// This should be called after the first setlist update completes
    /// Safe to call multiple times (idempotent)
    pub fn init_deferred_logger(&self) {
        if !self.logger_initialized.get() {
            init_deferred_reactive_logger(&self.middleware);
            self.logger_initialized.set(true);
        }
    }
    
    /// Register the change detection control surface
    pub fn register(&self, session: &mut ReaperSession) -> Result<(), Box<dyn std::error::Error>> {
        // Create a new middleware instance for the control surface
        // (we can't clone it, so we create a new one)
        let middleware = ComprehensiveChangeDetectionMiddleware::new();
        
        // Initialize reactive streams (subscriptions only - logger is deferred)
        init_middleware_reactive_streams(&middleware);
        
        let control_surface = MiddlewareControlSurface::new(middleware);
        session.plugin_register_add_csurf_inst(Box::new(control_surface))?;
        info!("✅ Change detection control surface registered (logger deferred until first project loaded)");
        Ok(())
    }
}
