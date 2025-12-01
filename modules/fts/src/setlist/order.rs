//! Setlist Order Management
//!
//! Manages the order of songs across multiple projects/tabs for live performance.
//! This is separate from the single-project Setlist, which represents songs within one project.

use serde::{Deserialize, Serialize};

/// Represents a song entry in the setlist order
/// This maps to a specific project tab and contains playback position information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetlistEntry {
    /// Tab index (0-based) - identifies which project tab this song is in
    pub tab_index: usize,
    /// Project file path (if available) - for persistence across sessions
    pub project_path: Option<String>,
    /// Count-in position in seconds (if "Count-In" marker exists)
    pub count_in_position: Option<f64>,
    /// Start position in seconds (if "SONGSTART" marker exists)
    pub start_position: Option<f64>,
}

/// Complete setlist order - represents the sequence of songs across project tabs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetlistOrder {
    /// Ordered list of songs/tabs in the setlist
    pub entries: Vec<SetlistEntry>,
    /// Current position in setlist (0-based index)
    pub current_position: usize,
}

impl SetlistOrder {
    /// Create a new empty setlist order
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            current_position: 0,
        }
    }
    
    /// Get the current song entry
    pub fn current_entry(&self) -> Option<&SetlistEntry> {
        self.entries.get(self.current_position)
    }
    
    /// Get the next song entry (wraps around)
    pub fn next_entry(&self) -> Option<&SetlistEntry> {
        if self.entries.is_empty() {
            return None;
        }
        
        if self.current_position + 1 < self.entries.len() {
            self.entries.get(self.current_position + 1)
        } else {
            // Wrap around to first
            self.entries.first()
        }
    }
    
    /// Get the previous song entry (wraps around)
    pub fn prev_entry(&self) -> Option<&SetlistEntry> {
        if self.entries.is_empty() {
            return None;
        }
        
        if self.current_position > 0 {
            self.entries.get(self.current_position - 1)
        } else {
            // Wrap around to last
            self.entries.last()
        }
    }
    
    /// Get entry by tab index
    pub fn entry_by_tab(&self, tab_index: usize) -> Option<&SetlistEntry> {
        self.entries.iter().find(|e| e.tab_index == tab_index)
    }
    
    /// Get entry index by tab index
    pub fn index_by_tab(&self, tab_index: usize) -> Option<usize> {
        self.entries.iter()
            .position(|e| e.tab_index == tab_index)
    }
    
    /// Advance to next song (wraps around)
    pub fn advance(&mut self) {
        if !self.entries.is_empty() {
            self.current_position = (self.current_position + 1) % self.entries.len();
        }
    }
    
    /// Go to previous song (wraps around)
    pub fn retreat(&mut self) {
        if !self.entries.is_empty() {
            if self.current_position > 0 {
                self.current_position -= 1;
            } else {
                self.current_position = self.entries.len() - 1;
            }
        }
    }
    
    /// Jump to a specific position (safely bounds-checked)
    pub fn jump_to(&mut self, position: usize) {
        if position < self.entries.len() {
            self.current_position = position;
        }
    }
    
    /// Jump to entry with specific tab index
    pub fn jump_to_tab(&mut self, tab_index: usize) -> bool {
        if let Some(index) = self.index_by_tab(tab_index) {
            self.current_position = index;
            true
        } else {
            false
        }
    }
    
    /// Get total number of entries
    pub fn len(&self) -> usize {
        self.entries.len()
    }
    
    /// Check if setlist is empty
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

impl Default for SetlistOrder {
    fn default() -> Self {
        Self::new()
    }
}

