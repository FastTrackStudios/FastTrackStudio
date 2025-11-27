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

use daw::transport::Transport;
use daw::{DawStreams, ProjectStreams};
use daw::transport::reactive::{TransportReactiveService, DefaultTransportReactiveService};
use daw::tracks::reactive::{TrackReactiveService, DefaultTrackReactiveService};
use setlist::SetlistApi;
use setlist::reactive::{SetlistReactiveStreams, SetlistReactiveService};
use lyrics::{Lyrics, LyricsAnnotations};
use std::sync::Arc;
use uuid::Uuid;

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
    
    pub fn tracks_changed(&self) -> rxrust::prelude::LocalSubject<'static, (uuid::Uuid, Vec<daw::tracks::Track>), ()> {
        self.daw.tracks.tracks_changed.borrow().clone()
    }
    
    pub fn transport_changed(&self) -> rxrust::prelude::LocalSubject<'static, (uuid::Uuid, Transport), ()> {
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
    /// Note: Uses `Box` because reactive services are not `Send` or `Sync`
    /// (reactive streams use `Rc<RefCell<...>>` and are main-thread only)
    pub transport_service: Box<dyn TransportReactiveService>,
    
    /// Track reactive service
    /// Note: Uses `Box` because reactive services are not `Send` or `Sync`
    pub track_service: Box<dyn TrackReactiveService>,
    
    /// Setlist reactive service (manages setlist, song, lyrics, active indices)
    pub setlist_service: SetlistReactiveService,
    
    /// Reactive streams (composed from domain services)
    pub streams: ReactiveAppStateStreams,
}

impl ReactiveAppStateService {
    pub fn new() -> Self {
        let daw_streams = DawStreams::new();
        let setlist_streams = SetlistReactiveStreams::new();
        
        Self {
            setlist: None,
            transport_service: Box::new(DefaultTransportReactiveService::new(daw_streams.transport.clone())),
            track_service: Box::new(DefaultTrackReactiveService::new(daw_streams.tracks.clone())),
            setlist_service: SetlistReactiveService::new(setlist_streams.clone()),
            streams: ReactiveAppStateStreams {
                daw: daw_streams,
                setlist: setlist_streams,
            },
        }
    }
    
    /// Create with custom reactive service implementations
    pub fn with_services(
        transport_service: Box<dyn TransportReactiveService>,
        track_service: Box<dyn TrackReactiveService>,
    ) -> Self {
        let setlist_streams = SetlistReactiveStreams::new();
        let daw_streams = DawStreams {
            transport: transport_service.streams().clone(),
            tracks: track_service.streams().clone(),
            project: ProjectStreams::new(),
        };
        
        Self {
            setlist: None,
            transport_service,
            track_service,
            setlist_service: SetlistReactiveService::new(setlist_streams.clone()),
            streams: ReactiveAppStateStreams {
                daw: daw_streams,
                setlist: setlist_streams,
            },
        }
    }
}

