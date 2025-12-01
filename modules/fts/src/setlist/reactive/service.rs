//! Reactive service for setlist domain types
//!
//! This module provides a service that manages reactive state and provides APIs
//! for updating setlist, song, lyrics, and active indices. This allows the domain
//! to own its reactive behavior and state management.

use crate::setlist::core::{Setlist, Song, Section};
use super::{SetlistReactiveStreams, SetlistStreams, SongStreams, LyricsStreams, ActiveIndicesStreams, EventStreamSubject};
use crate::lyrics::{Lyrics, LyricsAnnotations};
use rxrust::prelude::Observer;
use std::cell::RefCell;
use std::sync::{Arc, Mutex};
#[cfg(not(target_arch = "wasm32"))]
use tracing::info;

/// State managed by the setlist reactive service
#[derive(Debug, Clone)]
pub struct SetlistReactiveState {
    /// Current setlist structure
    pub setlist: Option<Setlist>,
    
    /// Active song index
    pub active_song_index: Option<usize>,
    
    /// Active section index
    pub active_section_index: Option<usize>,
    
    /// Active slide index
    pub active_slide_index: Option<usize>,
}

impl Default for SetlistReactiveState {
    fn default() -> Self {
        Self {
            setlist: None,
            active_song_index: None,
            active_section_index: None,
            active_slide_index: None,
        }
    }
}

/// Service that manages setlist reactive state and streams
/// 
/// This service:
/// 1. Maintains the current setlist state in memory
/// 2. Compares new state with old state to detect changes
/// 3. Emits granular updates via reactive streams
/// 4. Provides APIs for updating state
#[derive(Debug)]
pub struct SetlistReactiveService {
    /// Current state
    state: Arc<Mutex<SetlistReactiveState>>,
    
    /// Reactive streams
    streams: SetlistReactiveStreams,
    
    /// Guard to prevent nested next() calls
    emitting: RefCell<bool>,
}

impl SetlistReactiveService {
    pub fn new(streams: SetlistReactiveStreams) -> Self {
        Self {
            state: Arc::new(Mutex::new(SetlistReactiveState::default())),
            streams,
            emitting: RefCell::new(false),
        }
    }
    
    /// Get the reactive streams
    pub fn streams(&self) -> &SetlistReactiveStreams {
        &self.streams
    }
    
    /// Get current state (for reading)
    pub fn get_state(&self) -> Arc<Mutex<SetlistReactiveState>> {
        self.state.clone()
    }
    
    /// Update setlist structure
    /// Emits event only if setlist structure actually changed
    pub fn update_setlist(&self, setlist: Setlist) {
        let (changed, changes) = {
            let mut state = self.state.lock().unwrap();
            let (changed, changes) = if let Some(old_setlist) = state.setlist.as_ref() {
                // Compare setlists and track what changed
                let mut changes = Vec::new();
                
                if old_setlist.songs.len() != setlist.songs.len() {
                    changes.push(format!("song_count: {} → {}", old_setlist.songs.len(), setlist.songs.len()));
                }
                
                // Check for song name changes
                for (i, (old_song, new_song)) in old_setlist.songs.iter().zip(setlist.songs.iter()).enumerate() {
                    if old_song.name != new_song.name {
                        changes.push(format!("song[{}] name: {} → {}", i, old_song.name, new_song.name));
                    }
                }
                
                // Check for new songs
                for (i, new_song) in setlist.songs.iter().enumerate().skip(old_setlist.songs.len()) {
                    changes.push(format!("song[{}] {} (new)", i, new_song.name));
                }
                
                let any_changed = !changes.is_empty();
                (any_changed, changes)
            } else {
                // New setlist - initial state
                (true, vec![format!("initial setlist: {} songs", setlist.songs.len())])
            };
            
            if changed {
                state.setlist = Some(setlist.clone());
            }
            (changed, changes)
        };
        
        if changed {
            #[cfg(not(target_arch = "wasm32"))]
            {
                let changes_str = if changes.is_empty() {
                    "".to_string()
                } else {
                    // Limit to first few changes to avoid log spam
                    let display_changes: Vec<_> = changes.iter().take(3).cloned().collect();
                    let more = if changes.len() > 3 {
                        format!(" (+{} more)", changes.len() - 3)
                    } else {
                        String::new()
                    };
                    format!(" | {}", display_changes.join(", ").to_string() + &more)
                };
                info!("📋 SETLIST{}", changes_str);
            }
            self.emit_setlist_structure(setlist);
        }
    }
    
    /// Add a song to the setlist
    pub fn add_song(&self, song_index: usize, song: Song) {
        {
            let mut state = self.state.lock().unwrap();
            if let Some(ref mut setlist) = state.setlist {
                // Note: The setlist should already have the song added, this just emits the event
            }
        }
        
        self.emit_song_added(song_index, song);
    }
    
    /// Remove a song from the setlist
    pub fn remove_song(&self, song_index: usize) {
        {
            let mut state = self.state.lock().unwrap();
            if let Some(ref mut setlist) = state.setlist {
                // Note: The setlist should already have the song removed, this just emits the event
            }
        }
        
        self.emit_song_removed(song_index);
    }
    
    /// Update songs order (reorder)
    pub fn reorder_songs(&self, setlist: Setlist) {
        {
            let mut state = self.state.lock().unwrap();
            state.setlist = Some(setlist.clone());
        }
        
        self.emit_songs_reordered(setlist);
    }
    
    /// Update a song
    pub fn update_song(&self, song_index: usize, song: Song) {
        {
            let mut state = self.state.lock().unwrap();
            if let Some(ref mut setlist) = state.setlist {
                if let Some(existing_song) = setlist.songs.get_mut(song_index) {
                    *existing_song = song.clone();
                }
            }
        }
        
        self.emit_song_changed(song_index, song);
    }
    
    /// Add a section to a song
    pub fn add_section(&self, song_index: usize, section_index: usize, section: Section) {
        {
            let mut state = self.state.lock().unwrap();
            if let Some(ref mut setlist) = state.setlist {
                if let Some(song) = setlist.songs.get_mut(song_index) {
                    if section_index <= song.sections.len() {
                        song.sections.insert(section_index, section.clone());
                    }
                }
            }
        }
        
        self.emit_section_added(song_index, section_index, section);
    }
    
    /// Remove a section from a song
    pub fn remove_section(&self, song_index: usize, section_index: usize) {
        {
            let mut state = self.state.lock().unwrap();
            if let Some(ref mut setlist) = state.setlist {
                if let Some(song) = setlist.songs.get_mut(song_index) {
                    if section_index < song.sections.len() {
                        song.sections.remove(section_index);
                    }
                }
            }
        }
        
        self.emit_section_removed(song_index, section_index);
    }
    
    /// Update a section in a song
    pub fn update_section(&self, song_index: usize, section_index: usize, section: Section) {
        {
            let mut state = self.state.lock().unwrap();
            if let Some(ref mut setlist) = state.setlist {
                if let Some(song) = setlist.songs.get_mut(song_index) {
                    if let Some(existing_section) = song.sections.get_mut(section_index) {
                        *existing_section = section.clone();
                    }
                }
            }
        }
        
        self.emit_section_changed(song_index, section_index, section);
    }
    
    /// Update lyrics for a specific song
    /// Lyrics are stored in the Song struct within the setlist
    pub fn update_lyrics(&self, song_index: usize, lyrics: Lyrics) {
        {
            let mut state = self.state.lock().unwrap();
            if let Some(ref mut setlist) = state.setlist {
                if let Some(song) = setlist.songs.get_mut(song_index) {
                    song.lyrics = Some(lyrics.clone());
                }
            }
        }
        
        self.emit_lyrics_changed(song_index, lyrics);
    }
    
    /// Update lyrics annotations for a specific song
    pub fn update_lyrics_annotations(&self, song_index: usize, annotations: LyricsAnnotations) {
        // Annotations may be stored separately, but we still emit the event
        self.emit_lyrics_annotations_changed(song_index, annotations);
    }
    
    /// Update active indices
    /// Emits event only if indices actually changed
    pub fn update_active_indices(
        &self,
        song_index: Option<usize>,
        section_index: Option<usize>,
        slide_index: Option<usize>,
    ) {
        let (changed, changes) = {
            let mut state = self.state.lock().unwrap();
            let mut changes = Vec::new();
            
            if state.active_song_index != song_index {
                changes.push(format!("song: {:?} → {:?}", state.active_song_index, song_index));
            }
            if state.active_section_index != section_index {
                changes.push(format!("section: {:?} → {:?}", state.active_section_index, section_index));
            }
            if state.active_slide_index != slide_index {
                changes.push(format!("slide: {:?} → {:?}", state.active_slide_index, slide_index));
            }
            
            let changed = !changes.is_empty();
            if changed {
                state.active_song_index = song_index;
                state.active_section_index = section_index;
                state.active_slide_index = slide_index;
            }
            (changed, changes)
        };
        
        if changed {
            #[cfg(not(target_arch = "wasm32"))]
            {
                let changes_str = if changes.is_empty() {
                    "".to_string()
                } else {
                    format!(" | {}", changes.join(", "))
                };
                info!("📍 INDICES{}", changes_str);
            }
            
            self.emit_active_indices(song_index, section_index, slide_index);
            
            // Also emit individual index changes
            if let Some(song_idx) = song_index {
                self.emit_active_song_index_changed(Some(song_idx));
            } else {
                self.emit_active_song_index_changed(None);
            }
            
            if let Some(section_idx) = section_index {
                self.emit_active_section_index_changed(Some(section_idx));
            } else {
                self.emit_active_section_index_changed(None);
            }
            
            if let Some(slide_idx) = slide_index {
                self.emit_active_slide_index_changed(Some(slide_idx));
            } else {
                self.emit_active_slide_index_changed(None);
            }
        }
    }
    
    // Private emit methods (with guard to prevent nested calls)
    
    fn emit_setlist_structure(&self, setlist: Setlist) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.setlist.setlist_structure_changed.borrow_mut().next(setlist);
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_song_added(&self, song_index: usize, song: Song) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.setlist.song_added.borrow_mut().next((song_index, song));
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_song_removed(&self, song_index: usize) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.setlist.song_removed.borrow_mut().next(song_index);
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_songs_reordered(&self, setlist: Setlist) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.setlist.songs_reordered.borrow_mut().next(setlist);
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_song_changed(&self, song_index: usize, song: Song) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.song.song_changed.borrow_mut().next((song_index, song));
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_section_added(&self, song_index: usize, section_index: usize, section: Section) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.song.section_added.borrow_mut().next((song_index, section_index, section));
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_section_removed(&self, song_index: usize, section_index: usize) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.song.section_removed.borrow_mut().next((song_index, section_index));
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_section_changed(&self, song_index: usize, section_index: usize, section: Section) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.song.section_changed.borrow_mut().next((song_index, section_index, section));
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_lyrics_changed(&self, song_index: usize, lyrics: Lyrics) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.lyrics.lyrics_changed.borrow_mut().next((song_index, lyrics));
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_lyrics_annotations_changed(&self, song_index: usize, annotations: LyricsAnnotations) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.lyrics.lyrics_annotations_changed.borrow_mut().next((song_index, annotations));
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_active_indices(&self, song: Option<usize>, section: Option<usize>, slide: Option<usize>) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.active_indices.active_indices_changed.borrow_mut().next((song, section, slide));
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_active_song_index_changed(&self, index: Option<usize>) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.active_indices.active_song_index_changed.borrow_mut().next(index);
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_active_section_index_changed(&self, index: Option<usize>) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.active_indices.active_section_index_changed.borrow_mut().next(index);
        *self.emitting.borrow_mut() = false;
    }
    
    fn emit_active_slide_index_changed(&self, index: Option<usize>) {
        if *self.emitting.borrow() {
            return;
        }
        *self.emitting.borrow_mut() = true;
        self.streams.active_indices.active_slide_index_changed.borrow_mut().next(index);
        *self.emitting.borrow_mut() = false;
    }
}

