//! Group configuration
//!
//! Represents a naming convention group with all its patterns, settings, and behaviors.
//! This matches the structure used in the REAPER Lua scripts' defaults.json.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use daw::tracks::Track;

/// Insert mode for tracks
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum InsertMode {
    /// Increment mode - tracks are numbered sequentially
    Increment,
    /// Append mode - tracks are appended without numbering
    Append,
}

/// Pattern category configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatternCategory {
    /// Patterns for this category
    pub patterns: Vec<String>,
    /// Whether this category is required for matching
    pub required: bool,
}

/// Group configuration matching the REAPER script format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroupConfig {
    /// Group name
    pub name: String,
    
    /// Group prefix (e.g., "D" for Drums)
    pub prefix: String,
    
    /// Patterns that match this group
    pub patterns: Vec<String>,
    
    /// Negative patterns (exclude matches)
    pub negative_patterns: Vec<String>,
    
    /// Component patterns organized by type
    pub component_patterns: Option<HashMap<String, Vec<String>>>,
    
    /// Child groups (nested hierarchy)
    pub children: Option<Vec<GroupConfig>>,
    
    /// Parent group name (for nested groups)
    pub parent_group: Option<String>,
    
    // Import script features
    /// Parent track reference
    pub parent_track: Option<Track>,
    
    /// Destination track reference
    pub destination_track: Option<Track>,
    
    /// Insert mode (increment, append, etc.)
    pub insert_mode: Option<InsertMode>,
    
    /// Starting number for increment mode
    pub increment_start: Option<u32>,
    
    /// Only number tracks when multiple exist
    pub only_number_when_multiple: Option<bool>,
    
    /// Create track if missing
    pub create_if_missing: Option<bool>,
    
    /// Extract number from track name for incrementing
    pub extract_number: Option<bool>,
    
    /// Track name variations (alternative names)
    pub track_name_variations: Option<Vec<String>>,
    
    /// Stereo pair mode
    pub stereo_pair_mode: Option<bool>,
    
    /// Pattern categories with required/optional flags
    pub pattern_categories: Option<HashMap<String, PatternCategory>>,
    
    /// Whether to rename tracks
    pub rename_track: Option<bool>,
    
    // Visibility Manager features
    /// Group color (ARGB format)
    pub color: Option<u32>,
    
    /// Group icon name/path
    pub icon: Option<String>,
    
    /// Whether this is a global group
    pub is_global: Option<bool>,
    
    /// Priority for matching (higher = more important)
    pub priority: Option<i32>,
    
    /// Group type behavior
    pub group_type: Option<String>, // "Static", "Increment", "Dynamic-Hierarchy"
}

impl GroupConfig {
    /// Create a minimal track reference from name and/or GUID
    pub fn create_track_reference(name: Option<String>, guid: Option<String>) -> Option<Track> {
        if name.is_none() && guid.is_none() {
            return None;
        }
        
        let mut track = Track::new(name.unwrap_or_default());
        track.guid = guid;
        Some(track)
    }
}

impl Default for GroupConfig {
    fn default() -> Self {
        Self {
            name: String::new(),
            prefix: String::new(),
            patterns: Vec::new(),
            negative_patterns: Vec::new(),
            component_patterns: None,
            children: None,
            parent_group: None,
            parent_track: None,
            destination_track: None,
            insert_mode: Some(InsertMode::Increment),
            increment_start: Some(1),
            only_number_when_multiple: Some(true),
            create_if_missing: Some(true),
            extract_number: None,
            track_name_variations: None,
            stereo_pair_mode: None,
            pattern_categories: None,
            rename_track: Some(false),
            color: Some(0x4444FFFF), // Default blue
            icon: None,
            is_global: Some(false),
            priority: Some(0),
            group_type: None,
        }
    }
}
