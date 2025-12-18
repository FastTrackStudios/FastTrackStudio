//! Visibility group configuration
//!
//! Configuration for visibility groups used by the Visibility Manager.

use serde::{Deserialize, Serialize};

/// View mode for visibility groups
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ViewMode {
    /// Toggle mode - clicking toggles visibility
    Toggle,
    /// Exclusive mode - only one group visible at a time
    Exclusive,
    /// Limited exclusive - exclusive within a subset
    LimitedExclusive,
}

/// Visibility group configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VisibilityGroupConfig {
    /// Group name
    pub name: String,
    
    /// Parent track GUID
    pub parent_track_guid: Option<String>,
    
    /// Parent track name (for matching)
    pub parent_track_name: Option<String>,
    
    /// Additional track GUIDs in scope
    pub additional_track_guids: Vec<String>,
    
    /// Whether this group is currently active
    pub active: bool,
    
    /// Whether this is a global group
    pub is_global: bool,
    
    /// Group color (ARGB format)
    pub color: Option<u32>,
    
    /// Group icon name/path
    pub icon: Option<String>,
    
    /// Selected TCP snapshot name
    pub selected_tcp: Option<String>,
    
    /// Selected MCP snapshot name
    pub selected_mcp: Option<String>,
    
    /// View mode
    pub view_mode: Option<ViewMode>,
}

impl Default for VisibilityGroupConfig {
    fn default() -> Self {
        Self {
            name: String::new(),
            parent_track_guid: None,
            parent_track_name: None,
            additional_track_guids: Vec::new(),
            active: false,
            is_global: false,
            color: Some(0x4444FFFF), // Default blue
            icon: None,
            selected_tcp: None,
            selected_mcp: None,
            view_mode: Some(ViewMode::Toggle),
        }
    }
}
