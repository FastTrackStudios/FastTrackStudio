//! Visibility Manager traits
//!
//! Traits for visibility management operations.

use crate::smart_template::core::models::GroupMode;
use crate::smart_template::core::models::template::Template;

/// Trait for visibility management
///
/// This trait defines operations for managing track visibility, snapshots,
/// and group management.
pub trait VisibilityManager: Send + Sync {
    /// Error type for visibility operations
    type Error: std::error::Error + Send + Sync;
    
    /// Get track visibility state
    fn get_track_visibility(&self, track_id: &str) -> Result<VisibilityState, Self::Error>;
    
    /// Set track visibility state
    fn set_track_visibility(&self, track_id: &str, state: VisibilityState) -> Result<(), Self::Error>;
    
    /// Apply visibility based on group mode
    ///
    /// Sets visibility for all tracks in a template based on the specified mode.
    /// Tracks that don't belong to the mode will be hidden.
    fn apply_mode_visibility(
        &self,
        template: &Template,
        mode: GroupMode,
    ) -> Result<(), Self::Error>;
    
    /// Get the name/identifier of this visibility manager
    fn name(&self) -> &str;
}

/// Track visibility state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VisibilityState {
    /// Visible in TCP (Track Control Panel)
    pub tcp_visible: bool,
    /// Visible in MCP (Mixer Control Panel)
    pub mcp_visible: bool,
}

impl Default for VisibilityState {
    fn default() -> Self {
        Self {
            tcp_visible: true,
            mcp_visible: true,
        }
    }
}
