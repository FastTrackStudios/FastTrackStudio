//! Rig — a complete instrument setup.
//!
//! A [`Rig`] holds one or more [`Engine`]s (always at least one, enforced by
//! [`NonEmptyVec`]). Each engine contains layers with modules and blocks.

use std::fmt;

use crate::engine::Engine;
use crate::id::{EngineId, RigId};
use crate::non_empty::NonEmptyVec;
use crate::tags::{Taggable, Tags};

// ─────────────────────────────────────────────────────────────────────────────
// InstrumentType
// ─────────────────────────────────────────────────────────────────────────────

/// The instrument category for a rig.
#[derive(Debug, Clone, PartialEq, Eq, Hash, ::facet::Facet)]
#[repr(u8)]
pub enum InstrumentType {
    Guitar = 0,
    Keys = 1,
    Bass = 2,
    Vocals = 3,
    Drums = 4,
    Synth = 5,
    Custom(String) = 6,
}

impl fmt::Display for InstrumentType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Guitar => f.write_str("Guitar"),
            Self::Keys => f.write_str("Keys"),
            Self::Bass => f.write_str("Bass"),
            Self::Vocals => f.write_str("Vocals"),
            Self::Drums => f.write_str("Drums"),
            Self::Synth => f.write_str("Synth"),
            Self::Custom(name) => f.write_str(name),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Rig
// ─────────────────────────────────────────────────────────────────────────────

/// A complete instrument setup — one or more engines.
///
/// Always has at least one engine (enforced by `NonEmptyVec`).
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct Rig {
    pub id: RigId,
    pub name: String,
    pub instrument_type: InstrumentType,
    pub engines: NonEmptyVec<Engine>,
    pub tags: Tags,
}

impl Rig {
    /// Create a rig with its first (required) engine.
    pub fn new(
        name: impl Into<String>,
        instrument_type: InstrumentType,
        first_engine: Engine,
    ) -> Self {
        Self {
            id: RigId::new(),
            name: name.into(),
            instrument_type,
            engines: NonEmptyVec::new(first_engine),
            tags: Tags::new(),
        }
    }

    /// Add an engine to this rig.
    pub fn add_engine(&mut self, engine: Engine) {
        self.engines.push(engine);
    }

    /// Find an engine by ID.
    pub fn engine(&self, id: EngineId) -> Option<&Engine> {
        self.engines.iter().find(|e| e.id == id)
    }

    /// Find an engine by ID (mutable).
    pub fn engine_mut(&mut self, id: EngineId) -> Option<&mut Engine> {
        self.engines.iter_mut().find(|e| e.id == id)
    }

    /// Number of engines in this rig.
    pub fn engine_count(&self) -> usize {
        self.engines.len()
    }
}

impl Taggable for Rig {
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
    use crate::layer::Layer;
    use crate::version::LayerIndex;

    fn test_engine(name: &str) -> Engine {
        Engine::new(
            name,
            InstrumentType::Guitar,
            Layer::new("Main", LayerIndex::new(1)),
        )
    }

    #[test]
    fn rig_always_has_at_least_one_engine() {
        let rig = Rig::new("Guitar Rig", InstrumentType::Guitar, test_engine("Main"));
        assert_eq!(rig.engines.len(), 1);
        assert_eq!(rig.engines.first().name, "Main");
    }

    #[test]
    fn rig_add_engines() {
        let mut rig = Rig::new("Guitar Rig", InstrumentType::Guitar, test_engine("Clean"));
        rig.add_engine(test_engine("Dirty"));
        assert_eq!(rig.engine_count(), 2);
    }

    #[test]
    fn rig_find_engine_by_id() {
        let engine = test_engine("Main");
        let engine_id = engine.id;
        let rig = Rig::new("Guitar Rig", InstrumentType::Guitar, engine);

        assert!(rig.engine(engine_id).is_some());
        assert!(rig.engine(EngineId::new()).is_none());
    }

    #[test]
    fn instrument_type_display() {
        assert_eq!(format!("{}", InstrumentType::Guitar), "Guitar");
        assert_eq!(format!("{}", InstrumentType::Keys), "Keys");
        assert_eq!(format!("{}", InstrumentType::Bass), "Bass");
        assert_eq!(format!("{}", InstrumentType::Vocals), "Vocals");
        assert_eq!(format!("{}", InstrumentType::Drums), "Drums");
        assert_eq!(format!("{}", InstrumentType::Synth), "Synth");
        assert_eq!(
            format!("{}", InstrumentType::Custom("Theremin".into())),
            "Theremin"
        );
    }
}
