//! Director — top-level manager of racks.
//!
//! The [`Director`] oversees the entire signal routing hierarchy. It holds
//! racks, which hold rigs, which hold engines, which hold layers.

use crate::id::{DirectorId, RackId};
use crate::rack::Rack;
use crate::tags::{Taggable, Tags};

// ─────────────────────────────────────────────────────────────────────────────
// Director
// ─────────────────────────────────────────────────────────────────────────────

/// Top-level manager of the rig hierarchy.
///
/// A director holds racks (which hold rigs, engines, layers, modules, blocks).
/// There is typically one director per session.
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct Director {
    pub id: DirectorId,
    pub name: String,
    pub racks: Vec<Rack>,
    pub tags: Tags,
}

impl Director {
    /// Create a new director with no racks.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: DirectorId::new(),
            name: name.into(),
            racks: Vec::new(),
            tags: Tags::new(),
        }
    }

    /// Add a rack to this director.
    pub fn add_rack(&mut self, rack: Rack) {
        self.racks.push(rack);
    }

    /// Find a rack by ID.
    pub fn rack(&self, id: RackId) -> Option<&Rack> {
        self.racks.iter().find(|r| r.id == id)
    }

    /// Find a rack by ID (mutable).
    pub fn rack_mut(&mut self, id: RackId) -> Option<&mut Rack> {
        self.racks.iter_mut().find(|r| r.id == id)
    }

    /// Number of racks.
    pub fn rack_count(&self) -> usize {
        self.racks.len()
    }
}

impl Taggable for Director {
    fn tags(&self) -> &Tags {
        &self.tags
    }

    fn tags_mut(&mut self) -> &mut Tags {
        &mut self.tags
    }

    fn name(&self) -> &str {
        &self.name
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn director_starts_empty() {
        let director = Director::new("Session Director");
        assert_eq!(director.name, "Session Director");
        assert_eq!(director.rack_count(), 0);
    }

    #[test]
    fn director_add_racks() {
        let mut director = Director::new("Main");
        director.add_rack(Rack::new("Guitars"));
        director.add_rack(Rack::new("Vocals"));
        assert_eq!(director.rack_count(), 2);
    }

    #[test]
    fn director_find_rack_by_id() {
        let mut director = Director::new("Main");
        let rack = Rack::new("Test");
        let rack_id = rack.id;
        director.add_rack(rack);

        assert!(director.rack(rack_id).is_some());
        assert!(director.rack(RackId::new()).is_none());
    }
}
