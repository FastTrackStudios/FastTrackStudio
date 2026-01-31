mod actions;
mod implementation;

mod app;
mod core;
mod infrastructure;
#[cfg(feature = "input")]
mod input;
mod services;
mod extension_host;

#[cfg(feature = "auto_color")]
mod auto_color;
#[cfg(feature = "keyflow")]
mod chart;
#[cfg(feature = "chart_window")]
mod chart_window;
#[cfg(feature = "chord_overlay")]
mod chord_overlay;
#[cfg(feature = "dynamic_template")]
mod dynamic_template;
#[cfg(feature = "embed_test")]
mod embed_test;
#[cfg(feature = "key_overlay")]
mod key_overlay;
#[cfg(feature = "live")]
mod live;
#[cfg(feature = "lyrics")]
mod lyrics;
#[cfg(feature = "measure_overlay")]
mod measure_overlay;
#[cfg(feature = "trackname_overlay")]
mod trackname_overlay;
#[cfg(feature = "visibility_manager")]
mod visibility_manager;

/// Polling state management for continuous updates
pub mod polling_state {
    use std::sync::atomic::{AtomicBool, Ordering};
    static POLLING_ENABLED: AtomicBool = AtomicBool::new(true);

    pub fn is_enabled() -> bool {
        POLLING_ENABLED.load(Ordering::Relaxed)
    }

    pub fn set_enabled(enabled: bool) {
        POLLING_ENABLED.store(enabled, Ordering::Relaxed);
    }

    pub fn toggle() -> bool {
        let new_state = !POLLING_ENABLED.load(Ordering::Relaxed);
        POLLING_ENABLED.store(new_state, Ordering::Relaxed);
        new_state
    }
}

use crate::app::App;
use crate::infrastructure::action_registry::MidiEditorActionHook;
use fragile::Fragile;
use reaper_high::Reaper as HighReaper;
use reaper_low::{PluginContext, Swell};
use reaper_macros::reaper_extension_plugin;
use reaper_medium::ReaperSession;
use std::error::Error;
use std::sync::OnceLock;
use tracing::{debug, info, warn};

/// Global App instance (wrapped in Fragile to ensure main-thread-only access)
static APP_INSTANCE: OnceLock<Fragile<App>> = OnceLock::new();

/// Get the global App instance (for timer callback)
/// Fragile ensures this can only be called from the main thread
fn get_app() -> Option<&'static Fragile<App>> {
    APP_INSTANCE.get()
}

/// REAPER will call this extension entry-point function once when it's starting.
#[reaper_extension_plugin]
fn plugin_main(context: PluginContext) -> Result<(), Box<dyn Error>> {
    // Initialize tracing FIRST, before anything else
    crate::infrastructure::tracing_config::init_tracing();
    info!("FastTrackStudio REAPER Extension starting...");

    // Make Swell available globally (required for swell-ui menu operations)
    // This must be done before registering menus, as swell-ui functions call Swell::get()
    let _ = Swell::make_available_globally(Swell::load(context));

    // Initialize REAPER high-level API (required for action registration)
    match HighReaper::load(context).setup() {
        Ok(_) => {
            info!("REAPER high-level API initialized");
        }
        Err(_) => {
            // API already initialized (e.g., by another extension)
            debug!("REAPER high-level API already initialized");
        }
    }

    // Create a medium-level API session (keep it alive for timer registration)
    let mut session = ReaperSession::load(context);

    // Initialize TaskSupport for dispatching work to main thread
    if let Err(e) = crate::infrastructure::task_support::init_task_support() {
        warn!(error = %e, "Failed to initialize TaskSupport");
    } else {
        info!("TaskSupport initialized");
    }

    info!("Extension host initialized - services provided by daw-reaper extension");

    // Register hookcommand2 for MIDI editor actions (must be done before action registration)
    if let Err(e) = session.plugin_register_add_hook_command_2::<MidiEditorActionHook>() {
        warn!(error = %e, "Failed to register hookcommand2 for MIDI editor actions");
    } else {
        info!("Registered hookcommand2 for MIDI editor actions");
    }

    let reaper = session.reaper();

    // Create and initialize App
    debug!("Creating application container...");
    let app = App::new(session)?;

    // Initialize the application (register actions, menu, IROH server, etc.)
    app.initialize()?;

    // Store app globally wrapped in Fragile (ensures main-thread-only access)
    // Fragile will panic if accessed from wrong thread, which is safe
    APP_INSTANCE
        .set(Fragile::new(app))
        .expect("App already initialized");

    // Register REAPER timer callback (only when core feature is enabled)
    #[cfg(feature = "core")]
    {
        // Register REAPER timer callback (upgraded to 60Hz on first tick)
        // This runs on REAPER's main thread automatically, so we can safely call REAPER APIs
        extern "C" fn polling_timer_callback() {
            use crate::infrastructure::timer::{increment_tick_count, log_first_timer_call};
            use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

            // Upgrade to 60fps on first timer tick (after REAPER's timer is initialized)
            static UPGRADED_60FPS: AtomicBool = AtomicBool::new(false);
            if !UPGRADED_60FPS.swap(true, Ordering::SeqCst) {
                crate::infrastructure::timer::upgrade_to_60fps();
            }

            // Check for and hook MIDI editor and arrange view windows (if input feature is enabled)
            #[cfg(feature = "input")]
            {
                if crate::input::handler::InputHandler::is_enabled() {
                    crate::input::wheel_hook::check_and_hook_midi_editors();
                    crate::input::wheel_hook::check_and_hook_arrange_view();
                }
            }

            // Process main thread tasks from TaskSupport
            crate::infrastructure::task_support::run_middleware();

            // Process deferred toolbar operations
            crate::infrastructure::toolbar::process_deferred_ops();

            // Log first call and increment tick count
            log_first_timer_call();
            increment_tick_count();

            // Periodic logging to verify timer is running (every 1000 ticks = ~33 seconds at 30Hz)
            static LOG_COUNTER: AtomicU64 = AtomicU64::new(0);
            let count = LOG_COUNTER.fetch_add(1, Ordering::Relaxed);
            if count % 1000 == 0 && count > 0 {
                debug!("⏰ Timer running: {} ticks processed", count);
            }

            // Track polling counter - poll all tracks every ~5 seconds (150 ticks at 30Hz)
            static TRACK_POLL_COUNTER: AtomicU64 = AtomicU64::new(0);
            let track_poll_count = TRACK_POLL_COUNTER.fetch_add(1, Ordering::Relaxed);
            const TRACK_POLL_INTERVAL: u64 = 150; // 150 ticks = ~5 seconds at 30Hz

            // Get app instance (Fragile ensures this is main thread)
            if let Some(app_fragile) = get_app() {
                let app = app_fragile.get();

                // Update setlist state - ALWAYS runs every timer tick
                // This reads transport info and updates active song/section indices
                // The setlist is rebuilt with fresh transport info for each song
                // Now it will also update reactive polling with active indices
                let setlist_was_ready = app.setlist_service.get_setlist().is_some();
                match app
                    .setlist_service
                    .update_setlist_with_polling(app.reactive_polling.clone())
                {
                    Ok(_) => {
                        // Check if this is the first time setlist became ready
                        let setlist_is_now_ready = app.setlist_service.get_setlist().is_some();
                        if !setlist_was_ready && setlist_is_now_ready {
                            // First project is loaded - initialize deferred reactive loggers
                            // This avoids RefCell borrow panics during initial setup
                            app.change_detection.init_deferred_logger();
                            crate::infrastructure::reactive_logger::init_reactive_polling_logger(
                                app.reactive_polling.streams(),
                            );
                            info!(
                                "✅ All reactive loggers initialized (after first project loaded)"
                            );
                        }

                        // Update reactive setlist service with current setlist and active indices
                        if let Some(setlist_api) = app.setlist_service.get_setlist() {
                            let setlist = setlist_api.get_setlist();
                            app.reactive_state
                                .setlist_service
                                .update_setlist(setlist.clone());
                            app.reactive_state.setlist_service.update_active_indices(
                                setlist_api.active_song_index(),
                                setlist_api.active_section_index(),
                                setlist_api.active_slide_index(),
                            );

                            // Update lyrics reactive service with active slide index
                            #[cfg(feature = "lyrics")]
                            {
                                if let Some(song_index) = setlist_api.active_song_index() {
                                    if let Some(song) = setlist.songs.get(song_index) {
                                        let song_name = song.name.clone();
                                        let slide_index = setlist_api.active_slide_index();

                                        // Update lyrics if they exist
                                        if let Some(lyrics) = song.lyrics.as_ref() {
                                            app.reactive_state
                                                .lyrics_service
                                                .update_lyrics(&song_name, lyrics.clone());
                                        }

                                        // Update active slide index
                                        app.reactive_state
                                            .lyrics_service
                                            .update_active_slide_index(&song_name, slide_index);
                                    }
                                }
                            }
                        }

                        // Success - only log periodically to avoid spam
                        if count % 1000 == 0 {
                            debug!("✅ Setlist state updated successfully (tick {})", count);
                        }
                    }
                    Err(e) => {
                        warn!(error = %e, "Failed to update setlist state");
                    }
                }

                // Poll cursor positions and transport - emits reactive events only when values change
                // Do this AFTER setlist update to avoid nested borrows
                // Only poll after setlist is ready to avoid RefCell borrow issues
                if let Some(setlist_api) = app.setlist_service.get_setlist() {
                    app.reactive_polling.poll();

                    // Update transport reactive service from REAPER
                    // Use the concrete REAPER service instance if available
                    if let Some(ref transport_service) = app.reactive_state.reaper_transport_service
                    {
                        transport_service.update_from_reaper();
                    }

                    // Update roam transport service from REAPER
                    // This keeps the roam RPC service in sync with REAPER state
                    app.roam_transport.update_from_reaper();

                    // Update shared playback state for chart window
                    // This provides transport info without direct REAPER queries
                    #[cfg(feature = "chart_window")]
                    {
                        use reaper_high::Reaper;
                        let reaper = Reaper::get();
                        let project = reaper.current_project();
                        let play_state =
                            reaper.medium_reaper().get_play_state_ex(project.context());

                        let position = if play_state.is_playing {
                            project.play_position_latency_compensated().get()
                        } else {
                            project
                                .edit_cursor_position()
                                .map(|p| p.get())
                                .unwrap_or(0.0)
                        };

                        // Get song timing info
                        let (song_start, song_end, tempo, time_sig) =
                            if let Some(song) = setlist_api.active_song() {
                                (
                                    song.effective_start(),
                                    song.effective_end(),
                                    song.starting_tempo.unwrap_or(120.0),
                                    song.starting_time_signature.clone().unwrap_or_else(|| {
                                        daw::primitives::TimeSignature::new(4, 4)
                                    }),
                                )
                            } else {
                                (0.0, 0.0, 120.0, daw::primitives::TimeSignature::new(4, 4))
                            };

                        crate::chart_window::update_shared_playback_state(
                            position,
                            play_state.is_playing,
                            play_state.is_paused,
                            play_state.is_recording,
                            setlist_api.active_song_index(),
                            setlist_api.active_section_index(),
                            setlist_api.song_progress.unwrap_or(0.0),
                            setlist_api.section_progress.unwrap_or(0.0),
                            tempo,
                            time_sig,
                            song_start,
                            song_end,
                        );
                    }

                    // Poll all tracks every ~5 seconds to catch changes that don't have reactive events
                    // (e.g., folder collapse state, which isn't detected reactively)
                    if track_poll_count % TRACK_POLL_INTERVAL == 0 && track_poll_count > 0 {
                        use reaper_high::Reaper;
                        let reaper = Reaper::get();
                        let current_project = reaper.current_project();

                        // Update all tracks from REAPER (will only emit if changes detected)
                        if let Some(ref track_service) = app.reactive_state.reaper_track_service {
                            track_service.update_tracks_from_reaper(current_project);
                            debug!(
                                "📊 Polled all tracks for changes (tick {})",
                                track_poll_count
                            );
                        }
                    }
                }

                // Process pending seek requests from async tasks
                app.seek_service.process_pending_seeks();

                // Process pending command execution requests from async tasks
                app.command_service.process_pending_commands();

                // Process pending DAW commands from extensions (Transport, etc.)
                #[cfg(feature = "core")]
                {
                    if let Some(ref processor) = app.daw_command_processor.borrow().as_ref() {
                        processor.process_pending();
                    }
                }

                // Process smooth seek queue (check if we should execute queued seeks)
                app.smooth_seek_service.process_smooth_seek_queue();

                // Process plugin host operations (hot-reload events)
                app.process_plugin_operations();

                // Refresh trackname overlay (if enabled and open)
                #[cfg(feature = "trackname_overlay")]
                {
                    crate::trackname_overlay::refresh_overlay();
                }

                // Refresh measure overlay (if enabled and open)
                #[cfg(feature = "measure_overlay")]
                {
                    crate::measure_overlay::refresh_overlay();
                }

                // Refresh chord overlay (if enabled and open)
                #[cfg(feature = "chord_overlay")]
                {
                    crate::chord_overlay::refresh_overlay();
                }

                // Refresh key overlay (if enabled and open)
                #[cfg(feature = "key_overlay")]
                {
                    crate::key_overlay::refresh_overlay();
                }
            } else {
                warn!("⚠️ App instance not available in timer callback");
            }
        }

        // Register the timer callback with REAPER (runs on main thread automatically)
        use crate::infrastructure::timer::register_timer;
        let app = APP_INSTANCE.get().expect("App should be initialized").get();
        register_timer(&mut app.session_mut(), polling_timer_callback)?;
    }

    #[cfg(not(feature = "core"))]
    {
        // Minimal timer callback for TaskSupport only (when no features enabled)
        extern "C" fn minimal_timer_callback() {
            // Process main thread tasks from TaskSupport (minimal overhead)
            crate::infrastructure::task_support::run_middleware();
        }

        use crate::infrastructure::timer::register_timer;
        let app = APP_INSTANCE.get().expect("App should be initialized").get();
        register_timer(&mut app.session_mut(), minimal_timer_callback)?;
    }

    info!("✅ FastTrackStudio REAPER Extension initialized successfully");
    info!("🔧 Final step: Returning from plugin_main");
    Ok(())
}
