//! Reactive streams and services for lyrics state
//!
//! This module provides reactive streams and services for lyrics-related state changes.
//! All lyrics operations are scoped to a specific song.

#[cfg(not(target_arch = "wasm32"))]
pub mod irpc;

use super::core::Lyrics;
use crate::lyrics::source::LyricsAnnotations;
use rxrust::prelude::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Re-export EventStreamSubject for convenience
pub type EventStreamSubject<T> = RefCell<LocalSubject<'static, T, ()>>;

/// Reactive streams for lyrics state
#[derive(Clone, Default, Debug)]
pub struct LyricsStreams {
    /// Lyrics changed for a specific song
    pub lyrics_changed: EventStreamSubject<(String, Lyrics)>, // song_name, lyrics
    
    /// Active slide index changed for a specific song
    pub active_slide_index_changed: EventStreamSubject<(String, Option<usize>)>, // song_name, slide_index
    
    /// Lyrics annotations changed for a specific song
    pub lyrics_annotations_changed: EventStreamSubject<(String, LyricsAnnotations)>, // song_name, annotations
}

impl LyricsStreams {
    pub fn new() -> Self {
        fn default<T>() -> EventStreamSubject<T> {
            RefCell::new(LocalSubject::new())
        }
        Self {
            lyrics_changed: default(),
            active_slide_index_changed: default(),
            lyrics_annotations_changed: default(),
        }
    }
}

/// State managed by the lyrics reactive service
#[derive(Debug, Clone)]
pub struct LyricsReactiveState {
    /// Lyrics for each song (song_name -> lyrics)
    pub lyrics: HashMap<String, Lyrics>,
    
    /// Active slide index for each song (song_name -> slide_index)
    pub active_slide_index: HashMap<String, Option<usize>>,
}

impl Default for LyricsReactiveState {
    fn default() -> Self {
        Self {
            lyrics: HashMap::new(),
            active_slide_index: HashMap::new(),
        }
    }
}

/// Trait for lyrics reactive service implementations
/// 
/// Different backends (REAPER, etc.) can implement this trait
/// to provide reactive streams using their own event systems.
/// 
/// Note: This trait is not `Send` or `Sync` because reactive streams
/// use `Rc<RefCell<...>>` internally and are designed for single-threaded use.
/// Implementations should only be used on the main thread.
pub trait LyricsReactiveService {
    /// Get the reactive streams
    fn streams(&self) -> &LyricsStreams;
    
    /// Get current state (for reading)
    fn get_state(&self) -> Arc<Mutex<LyricsReactiveState>>;
    
    /// Update lyrics for a specific song
    fn update_lyrics(&self, song_name: &str, lyrics: Lyrics);
    
    /// Update active slide index for a specific song
    fn update_active_slide_index(&self, song_name: &str, slide_index: Option<usize>);
    
    /// Update lyrics annotations for a specific song
    fn update_lyrics_annotations(&self, song_name: &str, annotations: LyricsAnnotations);
}

/// Default implementation of LyricsReactiveService
/// 
/// This manages state and emits to reactive streams.
/// Backend-specific implementations can wrap this and subscribe to backend events.
#[derive(Debug)]
pub struct DefaultLyricsReactiveService {
    /// Current state
    state: Arc<Mutex<LyricsReactiveState>>,
    
    /// Reactive streams
    streams: LyricsStreams,
}

impl DefaultLyricsReactiveService {
    pub fn new(streams: LyricsStreams) -> Self {
        Self {
            state: Arc::new(Mutex::new(LyricsReactiveState::default())),
            streams,
        }
    }
    
    /// Emit lyrics changed event
    fn emit_lyrics_changed(&self, song_name: String, lyrics: Lyrics) {
        self.streams.lyrics_changed.borrow_mut().next((song_name, lyrics));
    }
    
    /// Emit active slide index changed event
    fn emit_active_slide_index_changed(&self, song_name: String, slide_index: Option<usize>) {
        self.streams.active_slide_index_changed.borrow_mut().next((song_name, slide_index));
    }
    
    /// Emit lyrics annotations changed event
    fn emit_lyrics_annotations_changed(&self, song_name: String, annotations: LyricsAnnotations) {
        self.streams.lyrics_annotations_changed.borrow_mut().next((song_name, annotations));
    }
}

impl LyricsReactiveService for DefaultLyricsReactiveService {
    fn streams(&self) -> &LyricsStreams {
        &self.streams
    }
    
    fn get_state(&self) -> Arc<Mutex<LyricsReactiveState>> {
        self.state.clone()
    }
    
    fn update_lyrics(&self, song_name: &str, lyrics: Lyrics) {
        let changed = {
            let mut state = self.state.lock().unwrap();
            let changed = state.lyrics.get(song_name).map(|old| old != &lyrics).unwrap_or(true);
            if changed {
                state.lyrics.insert(song_name.to_string(), lyrics.clone());
            }
            changed
        };
        
        if changed {
            #[cfg(not(target_arch = "wasm32"))]
            {
                use tracing::info;
                info!(song_name = %song_name, "🎵 Lyrics changed");
            }
            self.emit_lyrics_changed(song_name.to_string(), lyrics);
        }
    }
    
    fn update_active_slide_index(&self, song_name: &str, slide_index: Option<usize>) {
        let changed = {
            let mut state = self.state.lock().unwrap();
            let old_index = state.active_slide_index.get(song_name).copied();
            let changed = old_index != Some(slide_index);
            if changed {
                state.active_slide_index.insert(song_name.to_string(), slide_index);
            }
            (changed, old_index)
        };
        
        if changed.0 {
            #[cfg(not(target_arch = "wasm32"))]
            {
                use tracing::info;
                info!(
                    song_name = %song_name,
                    slide_index = ?slide_index,
                    previous_slide_index = ?changed.1,
                    "📄 Active slide changed"
                );
            }
            self.emit_active_slide_index_changed(song_name.to_string(), slide_index);
        }
    }
    
    fn update_lyrics_annotations(&self, song_name: &str, annotations: LyricsAnnotations) {
        // For now, always emit annotations changes (they may not have Eq implemented)
        #[cfg(not(target_arch = "wasm32"))]
        {
            use tracing::info;
            info!(song_name = %song_name, "🎵 Lyrics annotations changed");
        }
        self.emit_lyrics_annotations_changed(song_name.to_string(), annotations);
    }
}

