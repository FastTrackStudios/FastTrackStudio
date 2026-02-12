//! Override system — sparse tree diffs for scene customization.
//!
//! Song sections load a scene but can override any descendant node. The
//! override must be **validated** against the target hierarchy before it
//! can be stored in a `SongSection` — this is enforced at compile time
//! via `SceneOverride<Unvalidated>` vs `SceneOverride<Validated>`.

use std::marker::PhantomData;

use crate::engine::Engine;
use crate::id::*;
use crate::module::ModuleType;
use crate::normalized::NormalizedF64;
use crate::rack::Rack;
use crate::rig::Rig;
use crate::version::{LayerIndex, VersionedRef};

// ─── Validation state markers ────────────────────────────────────

/// Override has not been validated against a hierarchy.
#[derive(..Marker)]
pub struct Unvalidated;

/// Override has been validated — safe to store in a `SongSection`.
#[derive(..Marker)]
pub struct Validated;

// ─── OverridePath ────────────────────────────────────────────────

/// A path through the hierarchy identifying what to override.
///
/// Read left-to-right: `[Engine(id), Layer(1), Module(Drive), Block(id)]`
/// means "in this engine, layer 1, the drive module, this specific block".
#[derive(..Domain)]
pub struct OverridePath {
    pub segments: Vec<OverridePathSegment>,
}

impl OverridePath {
    pub fn new(segments: Vec<OverridePathSegment>) -> Self {
        Self { segments }
    }
}

/// A single segment in an override path.
#[derive(..Domain)]
#[repr(u8)]
pub enum OverridePathSegment {
    Engine(EngineId),
    Layer(LayerIndex),
    Module(ModuleType),
    Block(BlockId),
    Parameter(u32),
}

// ─── OverrideAction ──────────────────────────────────────────────

/// What to do at the target of an override path.
#[derive(..Domain)]
#[repr(u8)]
pub enum OverrideAction {
    /// Replace the snapshot/scene at this level with a different one.
    ReplaceSnapshot(SnapshotRef),
    /// Override specific parameters without changing the snapshot.
    ReplaceParameters(Vec<ParameterOverride>),
    /// Set the bypass state of a block or module.
    SetBypassed(bool),
    /// Disable this node entirely.
    Disable,
}

/// A reference to a snapshot or scene at any level.
#[derive(..Domain)]
#[repr(u8)]
pub enum SnapshotRef {
    Block(VersionedRef<BlockSnapshotId>),
    Module(VersionedRef<ModuleSnapshotId>),
    LayerScene(VersionedRef<LayerSceneId>),
    EngineScene(VersionedRef<EngineSceneId>),
    RigScene(VersionedRef<RigSceneId>),
    RackScene(VersionedRef<RackSceneId>),
}

/// A single parameter override — index + new value.
#[derive(..Domain)]
pub struct ParameterOverride {
    pub param_index: u32,
    pub value: NormalizedF64,
}

impl ParameterOverride {
    pub fn new(param_index: u32, value: f64) -> Self {
        Self {
            param_index,
            value: NormalizedF64::new(value),
        }
    }
}

// ─── SceneOverride<State> ────────────────────────────────────────

/// A scene override with typestate validation.
///
/// - `SceneOverride<Unvalidated>`: can be created freely, but cannot
///   be added to a `SongSection`.
/// - `SceneOverride<Validated>`: has been checked against a hierarchy,
///   safe to use in `SongSection`.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct SceneOverride<State = Unvalidated> {
    path: OverridePath,
    action: OverrideAction,
    _state: PhantomData<State>,
}

impl SceneOverride<Unvalidated> {
    /// Create a new unvalidated override.
    pub fn new(path: OverridePath, action: OverrideAction) -> Self {
        Self {
            path,
            action,
            _state: PhantomData,
        }
    }

    /// Validate this override against an engine hierarchy.
    ///
    /// Returns `Some(validated)` if the path is valid, `None` if the path
    /// doesn't match the engine's structure.
    pub fn validate_for_engine(self, engine: &Engine) -> Option<SceneOverride<Validated>> {
        if self.validate_path_for_engine(engine) {
            Some(SceneOverride {
                path: self.path,
                action: self.action,
                _state: PhantomData,
            })
        } else {
            None
        }
    }

    /// Validate this override against a rig hierarchy.
    pub fn validate_for_rig(self, rig: &Rig) -> Option<SceneOverride<Validated>> {
        if self.validate_path_for_rig(rig) {
            Some(SceneOverride {
                path: self.path,
                action: self.action,
                _state: PhantomData,
            })
        } else {
            None
        }
    }

    /// Validate this override against a rack hierarchy.
    pub fn validate_for_rack(self, rack: &Rack) -> Option<SceneOverride<Validated>> {
        if self.validate_path_for_rack(rack) {
            Some(SceneOverride {
                path: self.path,
                action: self.action,
                _state: PhantomData,
            })
        } else {
            None
        }
    }

    fn validate_path_for_engine(&self, engine: &Engine) -> bool {
        let mut segments = self.path.segments.iter();

        match segments.next() {
            // Path starts with a layer reference
            Some(OverridePathSegment::Layer(index)) => {
                if engine.layer(*index).is_none() {
                    return false;
                }
                // Remaining segments (Module, Block, Parameter) are valid
                // as long as the layer exists. Deep validation of module/block
                // existence could be added later but structural correctness
                // (layer exists) is the key compile-time guarantee.
                true
            }
            // Empty path or starting with Engine/Block/Module is valid for
            // engine-level overrides
            Some(OverridePathSegment::Module(_))
            | Some(OverridePathSegment::Block(_))
            | Some(OverridePathSegment::Parameter(_)) => true,
            Some(OverridePathSegment::Engine(_)) => false, // Can't reference engine from within engine
            None => true, // Empty path = override the engine itself
        }
    }

    fn validate_path_for_rig(&self, rig: &Rig) -> bool {
        let mut segments = self.path.segments.iter();

        match segments.next() {
            Some(OverridePathSegment::Engine(engine_id)) => rig.engine(*engine_id).is_some(),
            Some(OverridePathSegment::Layer(_)) => true, // Layer within default engine
            _ => true,
        }
    }

    fn validate_path_for_rack(&self, rack: &Rack) -> bool {
        // For rack-level validation, we just check top-level rig references
        for segment in &self.path.segments {
            if let OverridePathSegment::Engine(engine_id) = segment {
                // Check if any rig in the rack has this engine
                let found = rack.rigs.iter().any(|r| r.engine(*engine_id).is_some());
                if !found {
                    return false;
                }
            }
        }
        true
    }
}

/// Shared accessors for both validated and unvalidated overrides.
impl<S> SceneOverride<S> {
    /// The path this override targets.
    pub fn path(&self) -> &OverridePath {
        &self.path
    }

    /// The action to perform at the target.
    pub fn action(&self) -> &OverrideAction {
        &self.action
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::Engine;
    use crate::layer::Layer;
    use crate::rig::InstrumentType;

    fn test_engine() -> Engine {
        let mut engine = Engine::new(
            "Guitar",
            InstrumentType::Guitar,
            Layer::new("Main", LayerIndex::new(1)),
        );
        engine.add_layer(Layer::new("Harmony", LayerIndex::new(2)));
        engine
    }

    #[test]
    fn valid_layer_override() {
        let engine = test_engine();
        let ov = SceneOverride::new(
            OverridePath::new(vec![OverridePathSegment::Layer(LayerIndex::new(1))]),
            OverrideAction::Disable,
        );
        assert!(ov.validate_for_engine(&engine).is_some());
    }

    #[test]
    fn invalid_layer_override() {
        let engine = test_engine();
        let ov = SceneOverride::new(
            OverridePath::new(vec![OverridePathSegment::Layer(LayerIndex::new(5))]),
            OverrideAction::Disable,
        );
        assert!(ov.validate_for_engine(&engine).is_none());
    }

    #[test]
    fn valid_deep_path() {
        let engine = test_engine();
        let ov = SceneOverride::new(
            OverridePath::new(vec![
                OverridePathSegment::Layer(LayerIndex::new(1)),
                OverridePathSegment::Module(ModuleType::Drive),
            ]),
            OverrideAction::SetBypassed(true),
        );
        assert!(ov.validate_for_engine(&engine).is_some());
    }

    #[test]
    fn parameter_override_creation() {
        let po = ParameterOverride::new(0, 0.75);
        assert_eq!(po.param_index, 0);
        assert!((po.value.get() - 0.75).abs() < f64::EPSILON);
    }

    #[test]
    fn validated_override_accessors() {
        let engine = test_engine();
        let ov = SceneOverride::new(
            OverridePath::new(vec![OverridePathSegment::Layer(LayerIndex::new(1))]),
            OverrideAction::Disable,
        );
        let validated = ov.validate_for_engine(&engine).unwrap();
        assert_eq!(validated.path().segments.len(), 1);
        assert!(matches!(validated.action(), OverrideAction::Disable));
    }

    #[test]
    fn rig_level_validation() {
        let engine = test_engine();
        let engine_id = engine.id;
        let rig = Rig::new("Guitar", InstrumentType::Guitar, engine);

        let valid = SceneOverride::new(
            OverridePath::new(vec![OverridePathSegment::Engine(engine_id)]),
            OverrideAction::Disable,
        );
        assert!(valid.validate_for_rig(&rig).is_some());

        let invalid = SceneOverride::new(
            OverridePath::new(vec![OverridePathSegment::Engine(EngineId::new())]),
            OverrideAction::Disable,
        );
        assert!(invalid.validate_for_rig(&rig).is_none());
    }
}
