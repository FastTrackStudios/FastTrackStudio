//! Track structure definitions
//!
//! Represents the expected hierarchy of tracks in a DAW project.
//! This is used to define how tracks should be organized and sorted.
//!
//! Example:
//! ```
//! Drums
//! ├── Kick (BUS)
//! │   ├── Kick (SUM)
//! │   │   ├── Kick In
//! │   │   ├── Kick Out
//! │   │   └── Kick Trig
//! │   ├── Kick Sub
//! │   └── Kick Ambient
//! └── Snare (BUS)
//!     ├── Snare (SUM)
//!     │   ├── Snare Top
//!     │   ├── Snare Bottom
//!     │   └── Snare Trig
//!     └── Snare Verb
//! ```

/// Represents a node in the track structure hierarchy
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TrackStructure {
    /// Name of the track (e.g., "Kick", "Snare", "Kick In")
    pub name: String,
    
    /// Optional track type (e.g., "BUS", "SUM", "DI", "NOFX")
    /// If None, this is a regular audio track
    pub track_type: Option<String>,
    
    /// Child tracks in this structure
    pub children: Vec<TrackStructure>,
}

impl TrackStructure {
    /// Create a new track structure node
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            track_type: None,
            children: Vec::new(),
        }
    }
    
    /// Create a new track structure node with a track type
    pub fn with_track_type(name: impl Into<String>, track_type: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            track_type: Some(track_type.into()),
            children: Vec::new(),
        }
    }
    
    /// Add a child track structure
    pub fn add_child(&mut self, child: TrackStructure) {
        self.children.push(child);
    }
    
    /// Check if this structure has children
    pub fn has_children(&self) -> bool {
        !self.children.is_empty()
    }
    
    /// Find a child by name (case-insensitive)
    pub fn find_child(&self, name: &str) -> Option<&TrackStructure> {
        self.children.iter()
            .find(|child| child.name.eq_ignore_ascii_case(name))
    }
    
    /// Find a child by name (case-insensitive) - mutable
    pub fn find_child_mut(&mut self, name: &str) -> Option<&mut TrackStructure> {
        self.children.iter_mut()
            .find(|child| child.name.eq_ignore_ascii_case(name))
    }
    
    /// Get the full path of this track structure (e.g., "Drums > Kick > Kick In")
    pub fn path(&self) -> String {
        self.name.clone()
    }
    
    /// Get the full path with track types (e.g., "Drums > Kick (BUS) > Kick (SUM) > Kick In")
    pub fn path_with_types(&self) -> String {
        if let Some(track_type) = &self.track_type {
            format!("{} ({})", self.name, track_type)
        } else {
            self.name.clone()
        }
    }
    
    /// Get all leaf nodes (tracks with no children)
    pub fn leaf_nodes(&self) -> Vec<&TrackStructure> {
        let mut leaves = Vec::new();
        self.collect_leaves(&mut leaves);
        leaves
    }
    
    fn collect_leaves<'a>(&'a self, leaves: &mut Vec<&'a TrackStructure>) {
        if self.children.is_empty() {
            leaves.push(self);
        } else {
            for child in &self.children {
                child.collect_leaves(leaves);
            }
        }
    }
    
    /// Get the depth of this structure (0 for root, 1 for first level children, etc.)
    pub fn depth(&self) -> usize {
        if self.children.is_empty() {
            0
        } else {
            1 + self.children.iter()
                .map(|child| child.depth())
                .max()
                .unwrap_or(0)
        }
    }
    
    /// Count total number of nodes in this structure (including self)
    pub fn node_count(&self) -> usize {
        1 + self.children.iter()
            .map(|child| child.node_count())
            .sum::<usize>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_track_structure() {
        let structure = TrackStructure::new("Kick");
        assert_eq!(structure.name, "Kick");
        assert_eq!(structure.track_type, None);
        assert!(structure.children.is_empty());
    }

    #[test]
    fn test_create_with_track_type() {
        let structure = TrackStructure::with_track_type("Kick", "BUS");
        assert_eq!(structure.name, "Kick");
        assert_eq!(structure.track_type, Some("BUS".to_string()));
    }

    #[test]
    fn test_add_child() {
        let mut kick = TrackStructure::new("Kick");
        let kick_in = TrackStructure::new("Kick In");
        kick.add_child(kick_in);
        
        assert!(kick.has_children());
        assert_eq!(kick.children.len(), 1);
        assert_eq!(kick.children[0].name, "Kick In");
    }

    #[test]
    fn test_find_child() {
        let mut kick = TrackStructure::new("Kick");
        kick.add_child(TrackStructure::new("Kick In"));
        kick.add_child(TrackStructure::new("Kick Out"));
        
        assert!(kick.find_child("Kick In").is_some());
        assert!(kick.find_child("kick in").is_some()); // case-insensitive
        assert!(kick.find_child("Kick Trig").is_none());
    }

    #[test]
    fn test_path_with_types() {
        let structure = TrackStructure::with_track_type("Kick", "BUS");
        assert_eq!(structure.path_with_types(), "Kick (BUS)");
        
        let regular = TrackStructure::new("Kick In");
        assert_eq!(regular.path_with_types(), "Kick In");
    }

    #[test]
    fn test_leaf_nodes() {
        let mut kick = TrackStructure::new("Kick");
        kick.add_child(TrackStructure::new("Kick In"));
        kick.add_child(TrackStructure::new("Kick Out"));
        
        let leaves = kick.leaf_nodes();
        assert_eq!(leaves.len(), 2);
        assert_eq!(leaves[0].name, "Kick In");
        assert_eq!(leaves[1].name, "Kick Out");
    }

    #[test]
    fn test_depth() {
        let mut drums = TrackStructure::new("Drums");
        let mut kick = TrackStructure::new("Kick");
        kick.add_child(TrackStructure::new("Kick In"));
        drums.add_child(kick);
        
        assert_eq!(drums.depth(), 2);
        assert_eq!(drums.children[0].depth(), 1);
    }

    #[test]
    fn test_node_count() {
        let mut drums = TrackStructure::new("Drums");
        let mut kick = TrackStructure::new("Kick");
        kick.add_child(TrackStructure::new("Kick In"));
        kick.add_child(TrackStructure::new("Kick Out"));
        drums.add_child(kick);
        
        // Drums (1) + Kick (1) + Kick In (1) + Kick Out (1) = 4
        assert_eq!(drums.node_count(), 4);
    }
}

