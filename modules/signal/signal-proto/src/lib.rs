//! Signal2 protocol types — domain model for rig control.
//!
//! ## Hierarchy
//!
//! **Physical**: Block → Module → Layer → Engine → Rig
//!
//! **Performance**: Profile (Patches) → Song (Sections)
//!
//! **Templates**: Structural blueprints with [`Assignment::Unassigned`](template::Assignment)
//! placeholders at every level.

use facet::Facet;
use serde::{Deserialize, Serialize};

// ─── Domain modules ─────────────────────────────────────────────
pub mod engine;
pub mod layer;
pub mod metadata;
pub mod overrides;
pub mod profile;
pub mod rig;
pub mod song;
pub mod template;
pub mod traits;

/// Creates a branded string ID type with Display, From, and AsRef impls.
#[macro_export]
macro_rules! typed_string_id {
    ($(#[$meta:meta])* $name:ident) => {
        $(#[$meta])*
        #[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize, facet::Facet)]
        pub struct $name(String);

        impl $name {
            pub fn new(value: impl Into<String>) -> Self {
                Self(value.into())
            }

            pub fn as_str(&self) -> &str {
                &self.0
            }

            pub fn into_inner(self) -> String {
                self.0
            }
        }

        impl ::std::fmt::Display for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl From<String> for $name {
            fn from(value: String) -> Self {
                Self(value)
            }
        }

        impl From<&str> for $name {
            fn from(value: &str) -> Self {
                Self(value.to_string())
            }
        }

        impl AsRef<str> for $name {
            fn as_ref(&self) -> &str {
                &self.0
            }
        }
    };
}

typed_string_id!(
    /// Branded type for preset identifiers.
    PresetId
);
typed_string_id!(
    /// Branded type for snapshot identifiers.
    SnapshotId
);
typed_string_id!(
    /// Branded type for module preset identifiers.
    ModulePresetId
);
typed_string_id!(
    /// Branded type for module snapshot identifiers.
    ModuleSnapshotId
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum BlockType {
    Amp,
    Drive,
}

impl BlockType {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Amp => "amp",
            Self::Drive => "drive",
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn from_str(value: &str) -> Option<Self> {
        match value {
            "amp" => Some(Self::Amp),
            "drive" => Some(Self::Drive),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize, Facet)]
pub struct ParameterValue(f32);

impl ParameterValue {
    pub fn new(value: f32) -> Self {
        Self(value.clamp(0.0, 1.0))
    }

    pub fn get(self) -> f32 {
        self.0
    }
}

impl Default for ParameterValue {
    fn default() -> Self {
        Self(0.5)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct BlockParameter {
    id: String,
    name: String,
    value: ParameterValue,
}

impl BlockParameter {
    pub fn new(id: impl Into<String>, name: impl Into<String>, value: f32) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            value: ParameterValue::new(value),
        }
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn value(&self) -> ParameterValue {
        self.value
    }

    pub fn set_value(&mut self, value: f32) {
        self.value = ParameterValue::new(value);
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Block {
    parameters: Vec<BlockParameter>,
}

impl Block {
    pub fn new(param_1: f32, param_2: f32, param_3: f32) -> Self {
        Self::from_parameters(vec![
            BlockParameter::new("param_1", "Parameter 1", param_1),
            BlockParameter::new("param_2", "Parameter 2", param_2),
            BlockParameter::new("param_3", "Parameter 3", param_3),
        ])
    }

    pub fn from_parameters(parameters: Vec<BlockParameter>) -> Self {
        let parameters = if parameters.is_empty() {
            vec![BlockParameter::new("value", "Value", 0.5)]
        } else {
            parameters
        };

        Self { parameters }
    }

    pub fn parameters(&self) -> &[BlockParameter] {
        &self.parameters
    }

    pub fn set_parameter_value(&mut self, index: usize, value: f32) {
        if let Some(parameter) = self.parameters.get_mut(index) {
            parameter.set_value(value);
        }
    }

    pub fn first_value(&self) -> Option<f32> {
        self.parameters.first().map(|p| p.value().get())
    }

    pub fn set_first_value(&mut self, value: f32) {
        self.set_parameter_value(0, value);
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new(0.5, 0.5, 0.5)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Snapshot {
    id: SnapshotId,
    name: String,
    block: Block,
}

impl Snapshot {
    pub fn new(id: impl Into<SnapshotId>, name: impl Into<String>, block: Block) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            block,
        }
    }

    pub fn id(&self) -> &SnapshotId {
        &self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn block(&self) -> Block {
        self.block.clone()
    }
}

pub trait SnapshotLike {
    type Id;
    type State;

    fn id(&self) -> &Self::Id;
    fn name(&self) -> &str;
    fn state(&self) -> &Self::State;
}

impl SnapshotLike for Snapshot {
    type Id = SnapshotId;
    type State = Block;

    fn id(&self) -> &Self::Id {
        self.id()
    }

    fn name(&self) -> &str {
        self.name()
    }

    fn state(&self) -> &Self::State {
        &self.block
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Preset {
    id: PresetId,
    name: String,
    block_type: BlockType,
    default_snapshot: Snapshot,
    snapshots: Vec<Snapshot>,
}

impl Preset {
    pub fn new(
        id: impl Into<PresetId>,
        name: impl Into<String>,
        block_type: BlockType,
        default_snapshot: Snapshot,
        additional_snapshots: Vec<Snapshot>,
    ) -> Self {
        let mut snapshots = Vec::with_capacity(additional_snapshots.len() + 1);
        snapshots.push(default_snapshot.clone());
        snapshots.extend(
            additional_snapshots
                .into_iter()
                .filter(|s| s.id() != default_snapshot.id()),
        );

        Self {
            id: id.into(),
            name: name.into(),
            block_type,
            default_snapshot,
            snapshots,
        }
    }

    pub fn with_default_snapshot(
        id: impl Into<PresetId>,
        name: impl Into<String>,
        block_type: BlockType,
        default_snapshot: Snapshot,
    ) -> Self {
        Self::new(id, name, block_type, default_snapshot, Vec::new())
    }

    pub fn id(&self) -> &PresetId {
        &self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn block_type(&self) -> BlockType {
        self.block_type
    }

    pub fn default_snapshot(&self) -> Snapshot {
        self.default_snapshot.clone()
    }

    pub fn snapshots(&self) -> &[Snapshot] {
        &self.snapshots
    }

    pub fn snapshot(&self, snapshot_id: &SnapshotId) -> Option<Snapshot> {
        self.snapshots
            .iter()
            .find(|s| s.id() == snapshot_id)
            .cloned()
    }
}

pub trait PresetLike {
    type Id;
    type SnapshotId;
    type Snapshot: SnapshotLike<Id = Self::SnapshotId>;

    fn id(&self) -> &Self::Id;
    fn name(&self) -> &str;
    fn snapshots(&self) -> &[Self::Snapshot];
    fn default_snapshot_id(&self) -> &Self::SnapshotId;

    fn default_snapshot(&self) -> Option<&Self::Snapshot>
    where
        Self::SnapshotId: PartialEq,
    {
        self.snapshots()
            .iter()
            .find(|snapshot| snapshot.id() == self.default_snapshot_id())
    }
}

impl PresetLike for Preset {
    type Id = PresetId;
    type SnapshotId = SnapshotId;
    type Snapshot = Snapshot;

    fn id(&self) -> &Self::Id {
        self.id()
    }

    fn name(&self) -> &str {
        self.name()
    }

    fn snapshots(&self) -> &[Self::Snapshot] {
        self.snapshots()
    }

    fn default_snapshot_id(&self) -> &Self::SnapshotId {
        self.default_snapshot.id()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct BlockParameterOverride {
    parameter_id: String,
    value: ParameterValue,
}

impl BlockParameterOverride {
    pub fn new(parameter_id: impl Into<String>, value: f32) -> Self {
        Self {
            parameter_id: parameter_id.into(),
            value: ParameterValue::new(value),
        }
    }

    pub fn parameter_id(&self) -> &str {
        &self.parameter_id
    }

    pub fn value(&self) -> ParameterValue {
        self.value
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub enum ModuleBlockSource {
    PresetDefault {
        preset_id: PresetId,
    },
    PresetSnapshot {
        preset_id: PresetId,
        snapshot_id: SnapshotId,
    },
    Inline {
        block: Block,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ModuleBlock {
    id: String,
    label: String,
    block_type: BlockType,
    source: ModuleBlockSource,
    overrides: Vec<BlockParameterOverride>,
}

impl ModuleBlock {
    pub fn new(
        id: impl Into<String>,
        label: impl Into<String>,
        block_type: BlockType,
        source: ModuleBlockSource,
    ) -> Self {
        Self {
            id: id.into(),
            label: label.into(),
            block_type,
            source,
            overrides: Vec::new(),
        }
    }

    pub fn with_overrides(mut self, overrides: Vec<BlockParameterOverride>) -> Self {
        self.overrides = overrides;
        self
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn label(&self) -> &str {
        &self.label
    }

    pub fn block_type(&self) -> BlockType {
        self.block_type
    }

    pub fn source(&self) -> &ModuleBlockSource {
        &self.source
    }

    pub fn overrides(&self) -> &[BlockParameterOverride] {
        &self.overrides
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Module {
    blocks: Vec<ModuleBlock>,
}

impl Module {
    pub fn from_blocks(blocks: Vec<ModuleBlock>) -> Self {
        Self { blocks }
    }

    pub fn blocks(&self) -> &[ModuleBlock] {
        &self.blocks
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ModuleSnapshot {
    id: ModuleSnapshotId,
    name: String,
    module: Module,
}

impl ModuleSnapshot {
    pub fn new(id: impl Into<ModuleSnapshotId>, name: impl Into<String>, module: Module) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            module,
        }
    }

    pub fn id(&self) -> &ModuleSnapshotId {
        &self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn module(&self) -> &Module {
        &self.module
    }
}

impl SnapshotLike for ModuleSnapshot {
    type Id = ModuleSnapshotId;
    type State = Module;

    fn id(&self) -> &Self::Id {
        self.id()
    }

    fn name(&self) -> &str {
        self.name()
    }

    fn state(&self) -> &Self::State {
        self.module()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ModulePreset {
    id: ModulePresetId,
    name: String,
    default_snapshot: ModuleSnapshot,
    snapshots: Vec<ModuleSnapshot>,
}

impl ModulePreset {
    pub fn new(
        id: impl Into<ModulePresetId>,
        name: impl Into<String>,
        default_snapshot: ModuleSnapshot,
        additional_snapshots: Vec<ModuleSnapshot>,
    ) -> Self {
        let mut snapshots = Vec::with_capacity(additional_snapshots.len() + 1);
        snapshots.push(default_snapshot.clone());
        snapshots.extend(
            additional_snapshots
                .into_iter()
                .filter(|snapshot| snapshot.id() != default_snapshot.id()),
        );

        Self {
            id: id.into(),
            name: name.into(),
            default_snapshot,
            snapshots,
        }
    }

    pub fn id(&self) -> &ModulePresetId {
        &self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn snapshots(&self) -> &[ModuleSnapshot] {
        &self.snapshots
    }

    pub fn default_snapshot(&self) -> &ModuleSnapshot {
        &self.default_snapshot
    }

    pub fn snapshot(&self, snapshot_id: &ModuleSnapshotId) -> Option<ModuleSnapshot> {
        self.snapshots
            .iter()
            .find(|snapshot| snapshot.id() == snapshot_id)
            .cloned()
    }
}

impl PresetLike for ModulePreset {
    type Id = ModulePresetId;
    type SnapshotId = ModuleSnapshotId;
    type Snapshot = ModuleSnapshot;

    fn id(&self) -> &Self::Id {
        self.id()
    }

    fn name(&self) -> &str {
        self.name()
    }

    fn snapshots(&self) -> &[Self::Snapshot] {
        self.snapshots()
    }

    fn default_snapshot_id(&self) -> &Self::SnapshotId {
        self.default_snapshot.id()
    }
}

#[roam::service]
pub trait BlockService {
    async fn get_block(&self, block_type: BlockType) -> Block;
    async fn set_block(&self, block_type: BlockType, block: Block) -> Block;
    async fn list_presets(&self, block_type: BlockType) -> Vec<Preset>;
    async fn load_preset(&self, block_type: BlockType, preset_id: PresetId) -> Option<Snapshot>;
    async fn load_preset_snapshot(
        &self,
        block_type: BlockType,
        preset_id: PresetId,
        snapshot_id: SnapshotId,
    ) -> Option<Snapshot>;
    async fn list_module_presets(&self) -> Vec<ModulePreset>;
    async fn load_module_preset(&self, preset_id: ModulePresetId) -> Option<ModuleSnapshot>;
    async fn load_module_preset_snapshot(
        &self,
        preset_id: ModulePresetId,
        snapshot_id: ModuleSnapshotId,
    ) -> Option<ModuleSnapshot>;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preset_always_contains_default_snapshot() {
        let default = Snapshot::new("snap-default", "Default", Block::default());
        let duplicate = Snapshot::new("snap-default", "Duplicate", Block::new(0.1, 0.2, 0.3));
        let extra = Snapshot::new("snap-extra", "Extra", Block::new(0.8, 0.1, 0.6));

        let preset = Preset::new(
            "preset-a",
            "Preset A",
            BlockType::Amp,
            default.clone(),
            vec![duplicate, extra],
        );

        assert_eq!(preset.default_snapshot(), default);
        assert_eq!(preset.block_type(), BlockType::Amp);
        assert_eq!(preset.snapshots().len(), 2);
        assert_eq!(
            preset.snapshots()[0].id(),
            &SnapshotId::from("snap-default")
        );
        assert_eq!(preset.snapshots()[1].id(), &SnapshotId::from("snap-extra"));
    }
}
