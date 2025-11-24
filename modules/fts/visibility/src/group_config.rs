//! Configuration struct for defining sorting groups declaratively

use super::track_structure::{TrackStructure, GroupingStrategy};

/// Configuration for a sorting group
/// All fields are named for clarity and self-documentation
#[derive(Debug, Clone)]
pub struct GroupConfig {
    /// Unique identifier for the group (e.g., "DRUMS")
    pub id: &'static str,
    
    /// Human-readable display name (e.g., "Drums")
    pub name: &'static str,
    
    /// Prefix from naming convention (e.g., "GTR", "D", "Bass")
    /// Used for matching tracks to this group
    pub prefix: &'static str,
    
    /// Child group names (nested groups will be created automatically)
    /// Example: ["Kick", "Snare"] creates children with IDs "DRUMS_KICK", "DRUMS_SNARE"
    pub children: Vec<&'static str>,
    
    /// Track structure template for this group (optional)
    /// Defines the expected hierarchy and organization of tracks
    pub track_structure: Option<TrackStructure>,
    
    /// Grouping strategy for organizing entries within this group
    pub grouping_strategy: Option<GroupingStrategy>,
}

impl GroupConfig {
    /// Create a new group configuration with all named fields
    pub fn new(
        id: &'static str,
        name: &'static str,
        prefix: &'static str,
        children: Vec<&'static str>,
    ) -> Self {
        Self {
            id,
            name,
            prefix,
            children,
            track_structure: None,
            grouping_strategy: None,
        }
    }
    
    /// Create a simple group with no children
    pub fn simple(id: &'static str, name: &'static str, prefix: &'static str) -> Self {
        Self {
            id,
            name,
            prefix,
            children: Vec::new(),
            track_structure: None,
            grouping_strategy: None,
        }
    }
    
    /// Set the track structure template
    pub fn with_track_structure(mut self, structure: TrackStructure) -> Self {
        self.track_structure = Some(structure);
        self
    }
    
    /// Set the grouping strategy
    pub fn with_grouping_strategy(mut self, strategy: GroupingStrategy) -> Self {
        self.grouping_strategy = Some(strategy);
        self
    }
}

