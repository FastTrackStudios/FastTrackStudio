//! Reactive Application State
//!
//! Centralized reactive state management for the REAPER extension.
//! This module composes domain-level reactive services from `daw` and `setlist` crates.
//! The domain services own their state and reactive behavior, and this module
//! provides a unified interface for the application layer.
//!
//! This architecture allows:
//! - Components to subscribe only to what they need
//! - IRPC streams to send only changed data
//! - Minimal REAPER API calls (read once, distribute reactively)
//! - Efficient updates (no unnecessary rerenders)
//! - Composable architecture where each domain defines its own reactive behavior

use std::sync::Arc;
use daw::transport::Transport;
use daw::{DawStreams, ProjectStreams};
use daw::transport::reactive::TransportReactiveService;
use daw::tracks::reactive::TrackReactiveService;
use setlist::SetlistApi;
use setlist::reactive::{SetlistReactiveStreams, SetlistReactiveService};
use lyrics::{Lyrics, LyricsAnnotations};
use lyrics::reactive::{LyricsReactiveService, LyricsStreams};

/// Centralized application state
/// This is a lightweight wrapper that composes domain services
/// The actual state is managed by the domain services themselves
#[derive(Debug)]
pub struct ReactiveAppState {
    /// Current setlist API (songs, sections, active indices)
    /// This is kept here for convenience, but the reactive state is in SetlistReactiveService
    pub setlist: Option<SetlistApi>,
}

/// Reactive streams for granular state updates
/// Composes domain-level streams from `daw` and `setlist` crates
#[derive(Clone, Debug)]
pub struct ReactiveAppStateStreams {
    /// DAW domain streams (transport, tracks, project)
    pub daw: DawStreams,
    
    /// Setlist domain streams (setlist, song, lyrics, active indices)
    pub setlist: SetlistReactiveStreams,
}

impl ReactiveAppStateStreams {
    pub fn new() -> Self {
        Self {
            daw: DawStreams::new(),
            setlist: SetlistReactiveStreams::new(),
        }
    }
    
    // Convenience accessors that delegate to domain streams
    
    pub fn setlist_structure_changed(&self) -> rxrust::prelude::LocalSubject<'static, setlist::Setlist, ()> {
        self.setlist.setlist.setlist_structure_changed.borrow().clone()
    }
    
    pub fn active_indices_changed(&self) -> rxrust::prelude::LocalSubject<'static, (Option<usize>, Option<usize>, Option<usize>), ()> {
        self.setlist.active_indices.active_indices_changed.borrow().clone()
    }
    
    pub fn tracks_changed(&self) -> rxrust::prelude::LocalSubject<'static, (String, Vec<daw::tracks::Track>), ()> {
        self.daw.tracks.tracks_changed.borrow().clone()
    }
    
    pub fn transport_changed(&self) -> rxrust::prelude::LocalSubject<'static, (String, Transport), ()> {
        self.daw.transport.transport_changed.borrow().clone()
    }
    
    pub fn song_lyrics_changed(&self) -> rxrust::prelude::LocalSubject<'static, (usize, Lyrics), ()> {
        self.setlist.lyrics.lyrics_changed.borrow().clone()
    }
    
    pub fn song_lyrics_annotations_changed(&self) -> rxrust::prelude::LocalSubject<'static, (usize, LyricsAnnotations), ()> {
        self.setlist.lyrics.lyrics_annotations_changed.borrow().clone()
    }
    
    pub fn song_added(&self) -> rxrust::prelude::LocalSubject<'static, (usize, setlist::Song), ()> {
        self.setlist.setlist.song_added.borrow().clone()
    }
    
    pub fn song_removed(&self) -> rxrust::prelude::LocalSubject<'static, usize, ()> {
        self.setlist.setlist.song_removed.borrow().clone()
    }
    
    pub fn songs_reordered(&self) -> rxrust::prelude::LocalSubject<'static, setlist::Setlist, ()> {
        self.setlist.setlist.songs_reordered.borrow().clone()
    }
}

impl Default for ReactiveAppStateStreams {
    fn default() -> Self {
        Self::new()
    }
}

/// Service that manages reactive application state
/// 
/// This service composes domain-level reactive services.
/// All state management and reactive behavior is delegated to the domain services.
/// The services are exposed directly for use by the application layer.
pub struct ReactiveAppStateService {
    /// Current setlist API (convenience wrapper)
    pub setlist: Option<SetlistApi>,
    
    /// Transport reactive service
    /// Note: Uses `Arc` to allow cloning for IRPC API creation
    /// Reactive services are not `Send` or `Sync` (reactive streams use `Rc<RefCell<...>>` and are main-thread only)
    pub transport_service: Arc<dyn TransportReactiveService>,
    
    /// Track reactive service
    /// Note: Uses `Arc` to allow cloning for IRPC API creation
    pub track_service: Arc<dyn TrackReactiveService>,
    
    /// Concrete REAPER transport service (for calling REAPER-specific methods)
    /// This is the same instance as transport_service, but typed for convenience
    pub(crate) reaper_transport_service: Option<Arc<crate::infrastructure::reaper_transport_reactive::ReaperTransportReactiveService>>,
    
    /// Concrete REAPER track service (for calling REAPER-specific methods)
    /// This is the same instance as track_service, but typed for convenience
    pub reaper_track_service: Option<Arc<crate::infrastructure::reaper_track_reactive::ReaperTrackReactiveService>>,
    
    /// Setlist reactive service (manages setlist, song, lyrics, active indices)
    pub setlist_service: SetlistReactiveService,
    
    /// Lyrics reactive service (manages lyrics and active slide for songs)
    /// Note: Uses `Arc` to allow cloning for IRPC API creation
    /// Reactive services are not `Send` or `Sync` (reactive streams use `Rc<RefCell<...>>` and are main-thread only)
    pub lyrics_service: Arc<dyn LyricsReactiveService>,
    
    /// Concrete REAPER lyrics service (for calling REAPER-specific methods)
    /// This is the same instance as lyrics_service, but typed for convenience
    pub(crate) reaper_lyrics_service: Option<Arc<crate::infrastructure::reaper_lyrics_reactive::ReaperLyricsReactiveService>>,
    
    /// Reactive streams (composed from domain services)
    pub streams: ReactiveAppStateStreams,
}

impl std::fmt::Debug for ReactiveAppStateService {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReactiveAppStateService")
            .field("setlist", &self.setlist)
            .field("streams", &self.streams)
            .finish_non_exhaustive()
    }
}

impl ReactiveAppStateService {
    /// Create with REAPER implementations
    pub fn new_with_reaper(
        setlist_service: Arc<crate::services::SetlistService>,
    ) -> Self {
        let daw_streams = DawStreams::new();
        let setlist_streams = SetlistReactiveStreams::new();
        
        let reaper_transport = Arc::new(
            crate::infrastructure::reaper_transport_reactive::ReaperTransportReactiveService::new(
                daw_streams.transport.clone(),
                setlist_service.clone(),
            )
        );
        let reaper_track = Arc::new(
            crate::infrastructure::reaper_track_reactive::ReaperTrackReactiveService::new(
                daw_streams.tracks.clone(),
                setlist_service.clone(),
            )
        );
        
        let lyrics_streams = LyricsStreams::new();
        let reaper_lyrics = Arc::new(
            crate::infrastructure::reaper_lyrics_reactive::ReaperLyricsReactiveService::new(
                lyrics_streams.clone(),
            )
        );
        
        Self {
            setlist: None,
            transport_service: reaper_transport.clone() as Arc<dyn TransportReactiveService>,
            track_service: reaper_track.clone() as Arc<dyn TrackReactiveService>,
            reaper_transport_service: Some(reaper_transport),
            reaper_track_service: Some(reaper_track),
            setlist_service: SetlistReactiveService::new(setlist_streams.clone()),
            lyrics_service: reaper_lyrics.clone() as Arc<dyn LyricsReactiveService>,
            reaper_lyrics_service: Some(reaper_lyrics),
            streams: ReactiveAppStateStreams {
                daw: daw_streams,
                setlist: setlist_streams,
            },
        }
    }
    
    /// Create with custom reactive service implementations
    pub fn with_services(
        transport_service: Arc<dyn TransportReactiveService>,
        track_service: Arc<dyn TrackReactiveService>,
    ) -> Self {
        let setlist_streams = SetlistReactiveStreams::new();
        let daw_streams = DawStreams {
            transport: transport_service.streams().clone(),
            tracks: track_service.streams().clone(),
            project: ProjectStreams::new(),
        };
        
        let lyrics_streams = LyricsStreams::new();
        let default_lyrics = Arc::new(
            lyrics::reactive::DefaultLyricsReactiveService::new(lyrics_streams)
        ) as Arc<dyn LyricsReactiveService>;
        
        Self {
            setlist: None,
            transport_service,
            track_service,
            reaper_transport_service: None,
            reaper_track_service: None,
            setlist_service: SetlistReactiveService::new(setlist_streams.clone()),
            lyrics_service: default_lyrics,
            reaper_lyrics_service: None,
            streams: ReactiveAppStateStreams {
                daw: daw_streams,
                setlist: setlist_streams,
            },
        }
    }
}

