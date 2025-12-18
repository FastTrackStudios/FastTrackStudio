//! REAPER Visibility Manager implementation
//!
//! Manages track visibility based on group modes and visibility configurations.
//! Works at the drum_kit level (parent group) to control visibility of all child tracks.

use crate::smart_template::features::visibility::traits::{VisibilityManager, VisibilityState};
use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::group_mode::GroupMode;
use crate::smart_template::core::models::types::TrackName;
use std::collections::HashMap;
use std::sync::Mutex;

/// REAPER Visibility Manager
///
/// Manages track visibility in REAPER based on group modes.
/// Tracks are identified by name or GUID, and visibility is controlled
/// at the group level (e.g., drum_kit) rather than individual track level.
pub struct ReaperVisibilityManager {
    /// Track visibility states (track_id -> visibility state)
    track_states: Mutex<HashMap<String, VisibilityState>>,
    
    /// Track name to ID mapping
    track_name_to_id: Mutex<HashMap<TrackName, String>>,
}

impl ReaperVisibilityManager {
    /// Create a new REAPER visibility manager
    pub fn new() -> Self {
        Self {
            track_states: Mutex::new(HashMap::new()),
            track_name_to_id: Mutex::new(HashMap::new()),
        }
    }
    
    /// Register a track with the visibility manager
    ///
    /// This creates a mapping from track name to track ID (GUID or other identifier)
    pub fn register_track(&self, track_name: impl Into<TrackName>, track_id: String) {
        self.track_name_to_id.lock().unwrap().insert(track_name.into(), track_id);
    }
    
    /// Get track ID from track name
    fn get_track_id(&self, track_name: &TrackName) -> Option<String> {
        self.track_name_to_id.lock().unwrap().get(track_name).cloned()
    }
    
    /// Get all track IDs for a template
    fn get_template_track_ids(&self, template: &Template) -> Vec<(TrackName, String)> {
        template.tracks
            .iter()
            .filter_map(|track| {
                self.get_track_id(&track.name)
                    .map(|id| (track.name.clone(), id))
            })
            .collect()
    }
}

impl VisibilityManager for ReaperVisibilityManager {
    type Error = ReaperVisibilityError;
    
    fn get_track_visibility(&self, track_id: &str) -> Result<VisibilityState, Self::Error> {
        self.track_states
            .lock()
            .unwrap()
            .get(track_id)
            .copied()
            .ok_or(ReaperVisibilityError::TrackNotFound(track_id.to_string()))
    }
    
    fn set_track_visibility(&self, track_id: &str, state: VisibilityState) -> Result<(), Self::Error> {
        // In a real implementation, this would call REAPER API to set visibility
        // For now, we just store the state
        self.track_states.lock().unwrap().insert(track_id.to_string(), state);
        Ok(())
    }
    
    fn apply_mode_visibility(
        &self,
        template: &Template,
        mode: GroupMode,
    ) -> Result<(), Self::Error> {
        // Use filter_by_mode_with_content to dynamically include tracks with content
        // This allows tracks like Trig, Sub, Verb to be included if they have audio
        let filtered_template = template.filter_by_mode_with_content(mode, |_track| {
            // Check if track has content (items/audio)
            // In a real implementation, this would query REAPER API
            // For now, return false (no dynamic inclusion) - can be extended later
            // TODO: Implement REAPER API call to check if track has items
            // 1. Get track by name or GUID
            // 2. Check if track has any items (media items)
            // 3. Return true if items exist, false otherwise
            false
        });
        
        // Get all track IDs from the original template
        let _all_track_ids: Vec<String> = self.get_template_track_ids(template)
            .into_iter()
            .map(|(_, id)| id)
            .collect();
        
        // Get track IDs that should be visible in this mode
        let _visible_track_ids: Vec<String> = self.get_template_track_ids(&filtered_template)
            .into_iter()
            .map(|(_, id)| id)
            .collect();
        
        // In a real implementation, we would:
        // 1. Hide all tracks in the template
        // 2. Show only tracks that match the mode (or have content)
        // 3. Apply TCP/MCP visibility based on VisibilityState
        
        // For now, this is a placeholder that demonstrates the logic
        // The actual REAPER API calls would go here
        
        Ok(())
    }
    
    
    fn name(&self) -> &str {
        "reaper"
    }
}

impl Default for ReaperVisibilityManager {
    fn default() -> Self {
        Self::new()
    }
}

/// REAPER Visibility Manager Error
#[derive(Debug, thiserror::Error)]
pub enum ReaperVisibilityError {
    #[error("Track not found: {0}")]
    TrackNotFound(String),
    
    #[error("REAPER API error: {0}")]
    ReaperApiError(String),
    
    #[error("Visibility error: {0}")]
    Other(String),
}
