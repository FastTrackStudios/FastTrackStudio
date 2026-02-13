//! Rack — a container for related rigs.
//!
//! A rack groups rigs that serve a common purpose (e.g. "All Vocal Rigs",
//! "Guitar Rig Pool"). Racks can hold scenes and presets that manage
//! their constituent rigs.

use crate::id::{RackId, RigId};
use crate::rig::Rig;
use crate::tags::{Taggable, Tags};

// ─────────────────────────────────────────────────────────────────────────────
// Rack
// ─────────────────────────────────────────────────────────────────────────────

/// A container for related rigs.
///
/// Unlike `Rig` and `Engine`, a rack can be empty — it's a grouping
/// container, not a structural requirement.
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct Rack {
    pub id: RackId,
    pub name: String,
    pub rigs: Vec<Rig>,
    pub tags: Tags,
}

impl Rack {
    /// Create a new empty rack.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: RackId::new(),
            name: name.into(),
            rigs: Vec::new(),
            tags: Tags::new(),
        }
    }

    /// Add a rig to this rack.
    pub fn add_rig(&mut self, rig: Rig) {
        self.rigs.push(rig);
    }

    /// Find a rig by ID.
    pub fn rig(&self, id: RigId) -> Option<&Rig> {
        self.rigs.iter().find(|r| r.id == id)
    }

    /// Find a rig by ID (mutable).
    pub fn rig_mut(&mut self, id: RigId) -> Option<&mut Rig> {
        self.rigs.iter_mut().find(|r| r.id == id)
    }

    /// Number of rigs in this rack.
    pub fn rig_count(&self) -> usize {
        self.rigs.len()
    }
}

impl Taggable for Rack {
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
    use crate::engine::Engine;
    use crate::layer::Layer;
    use crate::rig::InstrumentType;
    use crate::version::LayerIndex;

    fn test_rig(name: &str) -> Rig {
        let engine = Engine::new(
            "Main",
            InstrumentType::Guitar,
            Layer::new("Layer 1", LayerIndex::new(1)),
        );
        Rig::new(name, InstrumentType::Guitar, engine)
    }

    #[test]
    fn rack_starts_empty() {
        let rack = Rack::new("Vocal Rigs");
        assert_eq!(rack.name, "Vocal Rigs");
        assert_eq!(rack.rig_count(), 0);
    }

    #[test]
    fn rack_add_rigs() {
        let mut rack = Rack::new("Guitar Pool");
        rack.add_rig(test_rig("Clean Rig"));
        rack.add_rig(test_rig("Dirty Rig"));
        assert_eq!(rack.rig_count(), 2);
    }

    #[test]
    fn rack_find_rig_by_id() {
        let mut rack = Rack::new("Test");
        let rig = test_rig("Main");
        let rig_id = rig.id;
        rack.add_rig(rig);

        assert!(rack.rig(rig_id).is_some());
        assert!(rack.rig(RigId::new()).is_none());
    }
}
