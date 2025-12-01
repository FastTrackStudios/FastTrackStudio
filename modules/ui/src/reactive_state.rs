//! Reactive State Access for UI Components
//!
//! Provides unified access to reactive state from the new streaming connections.
//! This module bridges the gap between the connection modules and UI components.
//!
//! Note: The UI crate is separate from the desktop app, so we can't directly access
//! the desktop connection modules. Instead, we provide hooks that components can use
//! to access the reactive state. The desktop app should pass the necessary state
//! through props or context, or we can use the existing setlist signals which are
//! already being updated by the setlist_connection.

use dioxus::prelude::*;

// Re-export setlist signals for convenience (they're still being updated by setlist_connection)
pub use fts::fts::setlist::{SETLIST_STRUCTURE, ACTIVE_INDICES, ACTIVE_SLIDE_INDEX, SONG_TRACKS, SONG_TRANSPORT};

/// Hook to get transport state for the active song from SONG_TRANSPORT
/// 
/// This uses the granular SONG_TRANSPORT signal which is updated by the setlist connection.
/// It's reactive - components using this will automatically rerender when transport changes.
pub fn use_transport_for_active_song() -> Memo<Option<daw::transport::Transport>> {
    use_memo(move || {
        let active_indices = ACTIVE_INDICES.read();
        let active_song_idx = active_indices.0?;
        
        let song_transport = SONG_TRANSPORT.read();
        song_transport.get(&active_song_idx).cloned()
    })
}

/// Hook to get tracks for the active song from SONG_TRACKS
/// 
/// This uses the granular SONG_TRACKS signal which is updated by the setlist connection.
/// It's reactive - components using this will automatically rerender when tracks change.
pub fn use_tracks_for_active_song() -> Memo<Option<Vec<daw::tracks::Track>>> {
    use_memo(move || {
        let active_indices = ACTIVE_INDICES.read();
        let active_song_idx = active_indices.0?;
        
        let song_tracks = SONG_TRACKS.read();
        song_tracks.get(&active_song_idx).cloned()
    })
}

/// Hook to get lyrics for the active song
/// 
/// This gets the lyrics from the active song in the setlist structure.
/// It's reactive - components using this will automatically rerender when lyrics change.
pub fn use_lyrics_for_active_song() -> Memo<Option<fts::lyrics::core::Lyrics>> {
    use_memo(move || {
        let active_indices = ACTIVE_INDICES.read();
        let active_song_idx = active_indices.0?;
        
        let setlist_structure = SETLIST_STRUCTURE.read();
        let setlist = setlist_structure.as_ref()?;
        let song = setlist.songs.get(active_song_idx)?;
        
        song.lyrics.clone()
    })
}

/// Hook to get active slide index for the active song
/// 
/// This uses the ACTIVE_SLIDE_INDEX signal which is updated by the setlist connection.
/// It's reactive - components using this will automatically rerender when the active slide changes.
pub fn use_active_slide_for_active_song() -> Memo<Option<usize>> {
    use_memo(move || {
        ACTIVE_SLIDE_INDEX.read().clone()
    })
}

