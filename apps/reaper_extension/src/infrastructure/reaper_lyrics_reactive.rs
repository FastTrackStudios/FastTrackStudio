//! REAPER implementation of lyrics reactive service
//!
//! This module provides a REAPER-specific implementation of the LyricsReactiveService trait.
//! It reads lyrics state from REAPER and updates reactive streams.

use lyrics::reactive::{LyricsReactiveService, LyricsReactiveState, LyricsStreams};
use lyrics::core::Lyrics;
use lyrics::source::LyricsAnnotations;
use std::sync::{Arc, Mutex};
use rxrust::prelude::Observer;
use tracing::info;
use crate::infrastructure::formatted_logging::{format_lyrics_change, format_active_slide_change};

/// REAPER implementation of LyricsReactiveService
/// 
/// This service reads lyrics state from REAPER and emits reactive events.
/// It's designed to be used on the main thread only.
#[derive(Debug)]
pub struct ReaperLyricsReactiveService {
    /// Current state
    state: Arc<Mutex<LyricsReactiveState>>,
    
    /// Reactive streams
    streams: LyricsStreams,
}

impl ReaperLyricsReactiveService {
    /// Create a new REAPER lyrics reactive service
    pub fn new(streams: LyricsStreams) -> Self {
        Self {
            state: Arc::new(Mutex::new(LyricsReactiveState::default())),
            streams,
        }
    }
    
    /// Emit lyrics changed event
    fn emit_lyrics(&self, song_name: String, lyrics: Lyrics) {
        self.streams.lyrics_changed.borrow_mut().next((song_name, lyrics));
    }
    
    /// Emit active slide index changed event
    fn emit_active_slide_index(&self, song_name: String, slide_index: Option<usize>) {
        self.streams.active_slide_index_changed.borrow_mut().next((song_name, slide_index));
    }
    
    /// Emit lyrics annotations changed event
    fn emit_lyrics_annotations(&self, song_name: String, annotations: LyricsAnnotations) {
        self.streams.lyrics_annotations_changed.borrow_mut().next((song_name, annotations));
    }
}

impl LyricsReactiveService for ReaperLyricsReactiveService {
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
            let message = format_lyrics_change(song_name);
            info!("{}", message);
            self.emit_lyrics(song_name.to_string(), lyrics);
        }
    }
    
    fn update_active_slide_index(&self, song_name: &str, slide_index: Option<usize>) {
        let (changed, old_index) = {
            let mut state = self.state.lock().unwrap();
            let old_index = state.active_slide_index.get(song_name).copied().flatten();
            let changed = state.active_slide_index.get(song_name).copied() != Some(slide_index);
            if changed {
                state.active_slide_index.insert(song_name.to_string(), slide_index);
            }
            (changed, old_index)
        };
        
        if changed {
            let message = format_active_slide_change(song_name, slide_index, old_index);
            info!("{}", message);
            self.emit_active_slide_index(song_name.to_string(), slide_index);
        }
    }
    
    fn update_lyrics_annotations(&self, song_name: &str, annotations: LyricsAnnotations) {
        // For now, always emit annotations changes (they may not have Eq implemented)
        info!(
            song_name = %song_name,
            "🎵 Lyrics annotations changed"
        );
        self.emit_lyrics_annotations(song_name.to_string(), annotations);
    }
}

