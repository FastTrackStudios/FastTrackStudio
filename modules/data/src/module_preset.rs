//! Module Preset & Snapshot — loadable module configurations with parameter variations.
//!
//! A [`ModulePreset`] is a named configuration for a [`ModuleType`] — it defines
//! which blocks/plugins are loaded and their default parameters. Different presets
//! for the same module type can have **completely different plugins**.
//!
//! A [`ModuleSnapshot`] captures parameter variations within the same preset
//! (same blocks, different knob positions).
//!
//! [`ModuleOverride`] lets scenes, profiles, and songs replace a specific module
//! while inheriting the rest of the rig preset.
//!
//! [`GlobalModuleOverride`] locks a module globally so it never changes regardless
//! of preset/scene/song changes.

use facet::Facet;

use crate::id::{BlockId, ModulePresetId, ModuleSnapshotId};
use crate::module::{Module, ModuleBlock, ModuleMacro, ModuleType};
use crate::normalized::Order;
use crate::preset::BlockOverride;
use crate::tags::{Taggable, Tags};

// ─────────────────────────────────────────────────────────────────────────────
// ModulePreset
// ─────────────────────────────────────────────────────────────────────────────

/// A named, standalone configuration for a module type.
///
/// Different presets for the same module type can have completely different
/// plugins/blocks. Each preset owns parameter-variation snapshots.
///
/// # Examples
///
/// ```text
/// Drive Module:
///   "Blues Stack"         → Halfman, Halfman+Teal, Halfman+BluesBreaker
///   "Protein + JHS Kilt"  → Blue Light, Green Light, Blue Heavy, ...
/// ```
#[derive(Debug, Clone, Facet)]
pub struct ModulePreset {
    pub id: ModulePresetId,
    pub name: String,
    pub module_type: ModuleType,
    pub description: Option<String>,
    pub blocks: Vec<ModuleBlock>,
    pub macros: Vec<ModuleMacro>,
    pub snapshots: Vec<ModuleSnapshot>,
    pub default_snapshot_id: Option<ModuleSnapshotId>,
    pub tags: Tags,
}

impl ModulePreset {
    /// Create a new module preset with the given name and type.
    pub fn new(name: impl Into<String>, module_type: ModuleType) -> Self {
        Self {
            id: ModulePresetId::new(),
            name: name.into(),
            module_type,
            description: None,
            blocks: Vec::new(),
            macros: Vec::new(),
            snapshots: Vec::new(),
            default_snapshot_id: None,
            tags: Tags::new(),
        }
    }

    /// Set the description.
    #[must_use]
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Add a block to this module preset.
    pub fn add_block(&mut self, block: ModuleBlock) {
        self.blocks.push(block);
    }

    /// Add a macro knob.
    pub fn add_macro(&mut self, module_macro: ModuleMacro) {
        self.macros.push(module_macro);
    }

    /// Add a snapshot and return its ID.
    pub fn add_snapshot(&mut self, snapshot: ModuleSnapshot) -> ModuleSnapshotId {
        let id = snapshot.id;
        self.snapshots.push(snapshot);
        id
    }

    /// Set the default snapshot.
    pub fn set_default_snapshot(&mut self, id: ModuleSnapshotId) {
        self.default_snapshot_id = Some(id);
    }

    /// Find a snapshot by ID.
    pub fn snapshot(&self, id: ModuleSnapshotId) -> Option<&ModuleSnapshot> {
        self.snapshots.iter().find(|s| s.id == id)
    }

    /// Find a snapshot by name.
    pub fn snapshot_by_name(&self, name: &str) -> Option<&ModuleSnapshot> {
        self.snapshots.iter().find(|s| s.name == name)
    }

    /// Get the default snapshot, if one is set and exists.
    pub fn default_snapshot(&self) -> Option<&ModuleSnapshot> {
        self.default_snapshot_id.and_then(|id| self.snapshot(id))
    }

    /// Get a block by its underlying `BlockId`.
    pub fn get_block(&self, block_id: BlockId) -> Option<&ModuleBlock> {
        self.blocks.iter().find(|mb| mb.block.id == block_id)
    }
}

impl Taggable for ModulePreset {
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
// ModuleSnapshot
// ─────────────────────────────────────────────────────────────────────────────

/// A parameter variation within a module preset.
///
/// Same blocks, different knob positions. Block overrides specify which
/// parameters differ from the preset's defaults.
#[derive(Debug, Clone, Facet)]
pub struct ModuleSnapshot {
    pub id: ModuleSnapshotId,
    pub name: String,
    pub block_overrides: Vec<BlockOverride>,
    pub tags: Tags,
}

impl ModuleSnapshot {
    /// Create a new empty module snapshot.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: ModuleSnapshotId::new(),
            name: name.into(),
            block_overrides: Vec::new(),
            tags: Tags::new(),
        }
    }

    /// Add a block override (parameter tweak from preset default).
    pub fn add_block_override(&mut self, block_override: BlockOverride) {
        self.block_overrides.push(block_override);
    }

    /// Find a block override by block ID.
    pub fn block_override(&self, block_id: BlockId) -> Option<&BlockOverride> {
        self.block_overrides.iter().find(|o| o.block_id == block_id)
    }
}

impl Taggable for ModuleSnapshot {
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
// ModuleAssignment
// ─────────────────────────────────────────────────────────────────────────────

/// Links a module type to a specific module preset within a top-level Preset.
///
/// This tells the system: "For the Drive stage, load 'Blues Stack' preset
/// at snapshot 'Halfman + Teal', positioned at order 5."
#[derive(Debug, Clone, Facet)]
pub struct ModuleAssignment {
    pub module_type: ModuleType,
    pub module_preset_id: ModulePresetId,
    pub module_snapshot_id: Option<ModuleSnapshotId>,
    pub order: Order,
    pub enabled: bool,
}

impl ModuleAssignment {
    /// Create a new enabled module assignment.
    pub fn new(
        module_type: ModuleType,
        module_preset_id: ModulePresetId,
        order: Order,
    ) -> Self {
        Self {
            module_type,
            module_preset_id,
            module_snapshot_id: None,
            order,
            enabled: true,
        }
    }

    /// Set the snapshot for this assignment.
    #[must_use]
    pub fn with_snapshot(mut self, snapshot_id: ModuleSnapshotId) -> Self {
        self.module_snapshot_id = Some(snapshot_id);
        self
    }

    /// Disable this assignment.
    #[must_use]
    pub fn disabled(mut self) -> Self {
        self.enabled = false;
        self
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ModuleOverride
// ─────────────────────────────────────────────────────────────────────────────

/// Override a specific module within a preset/scene/song.
///
/// The rest of the modules come from the base preset — only the overridden
/// module type is replaced.
#[derive(Debug, Clone, Facet)]
pub struct ModuleOverride {
    pub module_type: ModuleType,
    pub override_type: ModuleOverrideType,
}

impl ModuleOverride {
    /// Swap to a different module preset (and optionally a snapshot).
    pub fn swap_preset(
        module_type: ModuleType,
        module_preset_id: ModulePresetId,
        module_snapshot_id: Option<ModuleSnapshotId>,
    ) -> Self {
        Self {
            module_type,
            override_type: ModuleOverrideType::SwapPreset {
                module_preset_id,
                module_snapshot_id,
            },
        }
    }

    /// Use a completely custom inline module (not linked to any preset).
    pub fn custom(module_type: ModuleType, module: Module) -> Self {
        Self {
            module_type,
            override_type: ModuleOverrideType::Custom(Box::new(module)),
        }
    }

    /// Disable this module entirely.
    pub fn disable(module_type: ModuleType) -> Self {
        Self {
            module_type,
            override_type: ModuleOverrideType::Disable,
        }
    }

    /// Whether this override disables the module.
    pub fn is_disabled(&self) -> bool {
        matches!(self.override_type, ModuleOverrideType::Disable)
    }

    /// Whether this override swaps to a different preset.
    pub fn is_swap(&self) -> bool {
        matches!(self.override_type, ModuleOverrideType::SwapPreset { .. })
    }

    /// Whether this override uses a custom inline module.
    pub fn is_custom(&self) -> bool {
        matches!(self.override_type, ModuleOverrideType::Custom(_))
    }
}

/// How a module override replaces the original module.
#[derive(Debug, Clone, Facet)]
#[repr(C)]
pub enum ModuleOverrideType {
    /// Use a different module preset/snapshot.
    SwapPreset {
        module_preset_id: ModulePresetId,
        module_snapshot_id: Option<ModuleSnapshotId>,
    },
    /// Use a completely custom inline module (not linked to any preset).
    Custom(Box<Module>),
    /// Disable this module entirely.
    Disable,
}

// ─────────────────────────────────────────────────────────────────────────────
// GlobalModuleOverride
// ─────────────────────────────────────────────────────────────────────────────

/// Locks a module globally — it stays the same regardless of preset/scene/song.
///
/// When `locked` is true, changing presets or scenes will not affect this module.
/// Useful for locking an amp tone while experimenting with effects.
#[derive(Debug, Clone, Facet)]
pub struct GlobalModuleOverride {
    pub module_type: ModuleType,
    pub module_preset_id: ModulePresetId,
    pub module_snapshot_id: Option<ModuleSnapshotId>,
    pub locked: bool,
}

impl GlobalModuleOverride {
    /// Create a locked global override for a module type.
    pub fn locked(
        module_type: ModuleType,
        module_preset_id: ModulePresetId,
    ) -> Self {
        Self {
            module_type,
            module_preset_id,
            module_snapshot_id: None,
            locked: true,
        }
    }

    /// Set the snapshot for this global override.
    #[must_use]
    pub fn with_snapshot(mut self, snapshot_id: ModuleSnapshotId) -> Self {
        self.module_snapshot_id = Some(snapshot_id);
        self
    }

    /// Unlock this override (still sets the module, but allows changes).
    pub fn unlock(&mut self) {
        self.locked = false;
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{Block, PluginId};
    use crate::normalized::Order;

    fn test_block() -> Block {
        Block::new("Test Block", PluginId::vst3("com.test.plugin", "Test"))
    }

    // ── ModulePreset ──────────────────────────────────────────────────

    #[test]
    fn module_preset_creation() {
        let preset = ModulePreset::new("Blues Stack", ModuleType::Drive);
        assert_eq!(preset.name, "Blues Stack");
        assert_eq!(preset.module_type, ModuleType::Drive);
        assert!(preset.description.is_none());
        assert!(preset.blocks.is_empty());
        assert!(preset.snapshots.is_empty());
        assert!(preset.default_snapshot_id.is_none());
    }

    #[test]
    fn module_preset_with_description() {
        let preset = ModulePreset::new("Blues Stack", ModuleType::Drive)
            .with_description("Classic blues drive tones");
        assert_eq!(preset.description.as_deref(), Some("Classic blues drive tones"));
    }

    #[test]
    fn module_preset_add_and_find_snapshot() {
        let mut preset = ModulePreset::new("Blues Stack", ModuleType::Drive);
        let snap = ModuleSnapshot::new("Halfman");
        let snap_id = snap.id;

        let returned_id = preset.add_snapshot(snap);
        assert_eq!(returned_id, snap_id);
        assert_eq!(preset.snapshots.len(), 1);

        let found = preset.snapshot(snap_id).unwrap();
        assert_eq!(found.name, "Halfman");
    }

    #[test]
    fn module_preset_snapshot_by_name() {
        let mut preset = ModulePreset::new("Blues Stack", ModuleType::Drive);
        preset.add_snapshot(ModuleSnapshot::new("Halfman"));
        preset.add_snapshot(ModuleSnapshot::new("Halfman + Teal"));

        assert!(preset.snapshot_by_name("Halfman").is_some());
        assert!(preset.snapshot_by_name("Halfman + Teal").is_some());
        assert!(preset.snapshot_by_name("Nonexistent").is_none());
    }

    #[test]
    fn module_preset_default_snapshot() {
        let mut preset = ModulePreset::new("Drive", ModuleType::Drive);
        assert!(preset.default_snapshot().is_none());

        let snap = ModuleSnapshot::new("Default");
        let snap_id = snap.id;
        preset.add_snapshot(snap);
        preset.set_default_snapshot(snap_id);

        assert!(preset.default_snapshot().is_some());
        assert_eq!(preset.default_snapshot().unwrap().name, "Default");
    }

    #[test]
    fn module_preset_add_block() {
        let mut preset = ModulePreset::new("EQ", ModuleType::Eq);
        let block = test_block();
        let block_id = block.id;
        preset.add_block(ModuleBlock::new(block, Order::new(0)));

        assert_eq!(preset.blocks.len(), 1);
        assert!(preset.get_block(block_id).is_some());
    }

    #[test]
    fn module_preset_implements_taggable() {
        let mut preset = ModulePreset::new("Test", ModuleType::Drive);
        let tag_id = crate::id::TagId::new();
        preset.add_tag(tag_id);
        assert!(preset.has_tag(tag_id));
        assert_eq!(preset.name(), "Test");
    }

    // ── ModuleSnapshot ────────────────────────────────────────────────

    #[test]
    fn module_snapshot_creation() {
        let snap = ModuleSnapshot::new("Halfman + Teal");
        assert_eq!(snap.name, "Halfman + Teal");
        assert!(snap.block_overrides.is_empty());
        assert!(snap.tags.is_empty());
    }

    #[test]
    fn module_snapshot_add_block_override() {
        let mut snap = ModuleSnapshot::new("Test");
        let block_id = BlockId::new();
        let override_ = BlockOverride::new(block_id).with_bypass(true);
        snap.add_block_override(override_);

        assert_eq!(snap.block_overrides.len(), 1);
        assert!(snap.block_override(block_id).is_some());
    }

    // ── ModuleAssignment ──────────────────────────────────────────────

    #[test]
    fn module_assignment_creation() {
        let preset_id = ModulePresetId::new();
        let assignment = ModuleAssignment::new(ModuleType::Drive, preset_id, Order::new(5));

        assert_eq!(assignment.module_type, ModuleType::Drive);
        assert_eq!(assignment.module_preset_id, preset_id);
        assert!(assignment.module_snapshot_id.is_none());
        assert_eq!(assignment.order, Order::new(5));
        assert!(assignment.enabled);
    }

    #[test]
    fn module_assignment_with_snapshot() {
        let preset_id = ModulePresetId::new();
        let snap_id = ModuleSnapshotId::new();
        let assignment = ModuleAssignment::new(ModuleType::Amp, preset_id, Order::new(0))
            .with_snapshot(snap_id);

        assert_eq!(assignment.module_snapshot_id, Some(snap_id));
    }

    #[test]
    fn module_assignment_disabled() {
        let assignment = ModuleAssignment::new(
            ModuleType::Special,
            ModulePresetId::new(),
            Order::new(0),
        )
        .disabled();
        assert!(!assignment.enabled);
    }

    // ── ModuleOverride ────────────────────────────────────────────────

    #[test]
    fn module_override_swap_preset() {
        let preset_id = ModulePresetId::new();
        let snap_id = ModuleSnapshotId::new();
        let ov = ModuleOverride::swap_preset(ModuleType::PreFx, preset_id, Some(snap_id));

        assert_eq!(ov.module_type, ModuleType::PreFx);
        assert!(ov.is_swap());
        assert!(!ov.is_disabled());
        assert!(!ov.is_custom());
    }

    #[test]
    fn module_override_custom() {
        let module = Module::new("Custom Time", ModuleType::PostFx);
        let ov = ModuleOverride::custom(ModuleType::PostFx, module);

        assert!(ov.is_custom());
        assert!(!ov.is_swap());
        assert!(!ov.is_disabled());
    }

    #[test]
    fn module_override_disable() {
        let ov = ModuleOverride::disable(ModuleType::Modulation);

        assert!(ov.is_disabled());
        assert!(!ov.is_swap());
        assert!(!ov.is_custom());
    }

    // ── GlobalModuleOverride ──────────────────────────────────────────

    #[test]
    fn global_override_locked() {
        let preset_id = ModulePresetId::new();
        let ov = GlobalModuleOverride::locked(ModuleType::Amp, preset_id);

        assert!(ov.locked);
        assert_eq!(ov.module_type, ModuleType::Amp);
        assert_eq!(ov.module_preset_id, preset_id);
        assert!(ov.module_snapshot_id.is_none());
    }

    #[test]
    fn global_override_with_snapshot() {
        let snap_id = ModuleSnapshotId::new();
        let ov = GlobalModuleOverride::locked(ModuleType::Amp, ModulePresetId::new())
            .with_snapshot(snap_id);

        assert_eq!(ov.module_snapshot_id, Some(snap_id));
    }

    #[test]
    fn global_override_unlock() {
        let mut ov = GlobalModuleOverride::locked(ModuleType::Amp, ModulePresetId::new());
        assert!(ov.locked);
        ov.unlock();
        assert!(!ov.locked);
    }

}
