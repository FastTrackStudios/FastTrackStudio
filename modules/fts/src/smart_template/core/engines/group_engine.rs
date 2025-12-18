//! Group engine
//!
//! Main engine for group/naming convention operations.

use crate::smart_template::core::models::group_config::GroupConfig;

/// Main group engine
///
/// This engine coordinates parsing and formatting operations for track names.
pub struct GroupEngine {
    // Groups configuration
    groups: Vec<GroupConfig>,
}

impl GroupEngine {
    /// Create a new group engine with groups
    pub fn new(groups: Vec<GroupConfig>) -> Self {
        Self { groups }
    }
    
    /// Get the groups
    pub fn groups(&self) -> &[GroupConfig] {
        &self.groups
    }
    
    /// Get a mutable reference to groups
    pub fn groups_mut(&mut self) -> &mut Vec<GroupConfig> {
        &mut self.groups
    }
}
