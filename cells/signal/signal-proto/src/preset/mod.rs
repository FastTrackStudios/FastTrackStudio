//! Preset — a complete snapshot of rig state.
//!
//! A [`Preset`] captures everything needed to reproduce a sound:
//! section configuration, patch assignments, global block settings,
//! local block chains, and named [`Snapshot`]s for scene-level recall.

use facet::Facet;

use crate::block::{BlockType, PluginId};
use crate::category::PresetCategory;
use crate::id::{BlockId, GlobalBlockId, PatchId, PresetId, SectionId, SnapshotId, VariationId};
use crate::layer::SendLevel;
use crate::module::ModuleType;
use crate::module_preset::ModuleAssignment;
use crate::normalized::{NormalizedF64, Order, Rating};
use crate::parameter::ParameterValue;
use crate::routing::SectionRouting;
use crate::tags::{Taggable, Tags};

pub mod block_override;
pub mod builder;
pub mod config;
pub mod snapshot_diff;

pub use block_override::*;
pub use snapshot_diff::*;

// ─────────────────────────────────────────────────────────────────────────────
// Preset
// ─────────────────────────────────────────────────────────────────────────────

/// A complete rig preset — everything needed to reproduce a sound.
///
/// Presets capture section routing, patch assignments, global block
/// configuration, local block chains, and named snapshots for quick
/// scene switching.
#[derive(Debug, Clone, Facet)]
pub struct Preset {
    /// Unique identifier for this preset.
    pub id: PresetId,
    /// User-facing name.
    pub name: String,
    /// Hierarchical category (base tone, genre, archetype, etc.).
    pub category: PresetCategory,
    /// Tags for filtering and fallback resolution.
    pub tags: Tags,
    /// Star rating (0 = unrated, 1-5).
    pub rating: Rating,
    /// Optional user notes.
    pub notes: Option<String>,
    /// Long-form description of the preset's character and use cases.
    pub description: Option<String>,
    /// Credits for the preset (creator, sound designer, etc.).
    pub credits: Vec<Credit>,
    /// Inspirations for the preset (artists, songs, albums).
    pub inspirations: Vec<Inspiration>,
    /// Which module preset is loaded for each processing stage.
    pub module_assignments: Vec<ModuleAssignment>,
    /// Whether this preset is hidden from the main list.
    pub hidden: bool,
    /// Per-section configuration overrides.
    pub section_config: Vec<SectionConfig>,
    /// Which patch is loaded in each section/layer.
    pub patch_assignments: Vec<PatchAssignment>,
    /// Global block enable/order overrides.
    pub global_block_config: Vec<GlobalBlockConfig>,
    /// Preset-local blocks (not shared across presets).
    pub blocks: Vec<PresetBlock>,
    /// Named snapshots for scene-level recall.
    pub snapshots: Vec<Snapshot>,
    /// Default snapshot to load when the preset is activated.
    pub default_snapshot_id: Option<SnapshotId>,
}

impl Preset {
    /// Create a new preset with the given name and category.
    ///
    /// Starts with no overrides, no blocks, and no snapshots.
    pub fn new(name: impl Into<String>, category: PresetCategory) -> Self {
        Self {
            id: PresetId::new(),
            name: name.into(),
            category,
            tags: Tags::new(),
            rating: Rating::UNRATED,
            notes: None,
            description: None,
            credits: Vec::new(),
            inspirations: Vec::new(),
            module_assignments: Vec::new(),
            hidden: false,
            section_config: Vec::new(),
            patch_assignments: Vec::new(),
            global_block_config: Vec::new(),
            blocks: Vec::new(),
            snapshots: Vec::new(),
            default_snapshot_id: None,
        }
    }

    /// Add a section configuration override.
    pub fn add_section_config(&mut self, config: SectionConfig) {
        self.section_config.push(config);
    }

    /// Add a patch assignment.
    pub fn add_patch_assignment(&mut self, assignment: PatchAssignment) {
        self.patch_assignments.push(assignment);
    }

    /// Add a global block configuration override.
    pub fn add_global_block_config(&mut self, config: GlobalBlockConfig) {
        self.global_block_config.push(config);
    }

    /// Add a preset-local block.
    pub fn add_block(&mut self, block: PresetBlock) {
        self.blocks.push(block);
    }

    /// Add a snapshot and return its ID.
    pub fn add_snapshot(&mut self, snapshot: Snapshot) -> SnapshotId {
        let id = snapshot.id;
        self.snapshots.push(snapshot);
        id
    }

    /// Find a snapshot by ID.
    pub fn snapshot(&self, id: SnapshotId) -> Option<&Snapshot> {
        self.snapshots.iter().find(|s| s.id == id)
    }

    /// Find a snapshot by ID (mutable).
    pub fn snapshot_mut(&mut self, id: SnapshotId) -> Option<&mut Snapshot> {
        self.snapshots.iter_mut().find(|s| s.id == id)
    }

    /// Get the default snapshot, if one is set and exists.
    pub fn default_snapshot(&self) -> Option<&Snapshot> {
        self.default_snapshot_id.and_then(|id| self.snapshot(id))
    }

    /// Set the default snapshot ID.
    pub fn set_default_snapshot(&mut self, id: SnapshotId) {
        self.default_snapshot_id = Some(id);
    }

    /// Find a block by ID.
    pub fn block(&self, id: BlockId) -> Option<&PresetBlock> {
        self.blocks.iter().find(|b| b.id == id)
    }

    /// Add a module assignment.
    pub fn add_module_assignment(&mut self, assignment: ModuleAssignment) {
        self.module_assignments.push(assignment);
    }

    /// Get the module assignment for a specific module type.
    pub fn get_module_assignment(&self, module_type: ModuleType) -> Option<&ModuleAssignment> {
        self.module_assignments.iter().find(|a| a.module_type == module_type)
    }

    /// Add a credit.
    pub fn add_credit(&mut self, credit: Credit) {
        self.credits.push(credit);
    }

    /// Add an inspiration.
    pub fn add_inspiration(&mut self, inspiration: Inspiration) {
        self.inspirations.push(inspiration);
    }
}

impl Taggable for Preset {
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
// SectionConfig
// ─────────────────────────────────────────────────────────────────────────────

/// Per-section configuration within a preset.
///
/// Allows a preset to enable/disable sections and override their routing.
#[derive(Debug, Clone, Facet)]
pub struct SectionConfig {
    /// Which section this config applies to.
    pub section_id: SectionId,
    /// Whether the section is enabled in this preset.
    pub enabled: bool,
    /// Optional routing override (replaces the section's default routing).
    pub routing_override: Option<SectionRouting>,
}

impl SectionConfig {
    /// Create a section config that enables the section with no routing override.
    pub fn enabled(section_id: SectionId) -> Self {
        Self {
            section_id,
            enabled: true,
            routing_override: None,
        }
    }

    /// Create a section config that disables the section.
    pub fn disabled(section_id: SectionId) -> Self {
        Self {
            section_id,
            enabled: false,
            routing_override: None,
        }
    }

    /// Set a routing override.
    #[must_use]
    pub fn with_routing(mut self, routing: SectionRouting) -> Self {
        self.routing_override = Some(routing);
        self
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// PatchAssignment
// ─────────────────────────────────────────────────────────────────────────────

/// Assigns a patch (and optional variation) to a section layer.
#[derive(Debug, Clone, Facet)]
pub struct PatchAssignment {
    /// Target section.
    pub section_id: SectionId,
    /// Target layer index within the section (0-based).
    pub layer_index: u8,
    /// Patch to load.
    pub patch_id: PatchId,
    /// Optional specific variation to select.
    pub variation_id: Option<VariationId>,
    /// Whether this assignment is active.
    pub enabled: bool,
    /// Optional volume override for the layer.
    pub volume_override: Option<NormalizedF64>,
    /// Optional send level overrides for global blocks.
    pub global_sends_override: Option<Vec<SendLevel>>,
}

impl PatchAssignment {
    /// Create a basic patch assignment (enabled, no overrides).
    pub fn new(section_id: SectionId, layer_index: u8, patch_id: PatchId) -> Self {
        Self {
            section_id,
            layer_index,
            patch_id,
            variation_id: None,
            enabled: true,
            volume_override: None,
            global_sends_override: None,
        }
    }

    /// Set a specific variation.
    #[must_use]
    pub fn with_variation(mut self, variation_id: VariationId) -> Self {
        self.variation_id = Some(variation_id);
        self
    }

    /// Disable this assignment.
    #[must_use]
    pub fn disabled(mut self) -> Self {
        self.enabled = false;
        self
    }

    /// Set a volume override.
    #[must_use]
    pub fn with_volume(mut self, volume: NormalizedF64) -> Self {
        self.volume_override = Some(volume);
        self
    }

    /// Set global send level overrides.
    #[must_use]
    pub fn with_sends(mut self, sends: Vec<SendLevel>) -> Self {
        self.global_sends_override = Some(sends);
        self
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// GlobalBlockConfig
// ─────────────────────────────────────────────────────────────────────────────

/// Preset-level override for a global block.
///
/// Controls whether a global block is enabled and its position in the chain.
#[derive(Debug, Clone, Facet)]
pub struct GlobalBlockConfig {
    /// The global block this config applies to.
    pub global_block_id: GlobalBlockId,
    /// Whether the global block is enabled in this preset.
    pub enabled: bool,
    /// Optional order override (replaces the block's default order).
    pub order_override: Option<Order>,
}

impl GlobalBlockConfig {
    /// Create a config that enables the global block with no order override.
    pub fn enabled(global_block_id: GlobalBlockId) -> Self {
        Self {
            global_block_id,
            enabled: true,
            order_override: None,
        }
    }

    /// Create a config that disables the global block.
    pub fn disabled(global_block_id: GlobalBlockId) -> Self {
        Self {
            global_block_id,
            enabled: false,
            order_override: None,
        }
    }

    /// Set an order override.
    #[must_use]
    pub fn with_order(mut self, order: Order) -> Self {
        self.order_override = Some(order);
        self
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// PresetBlock
// ─────────────────────────────────────────────────────────────────────────────

/// A DSP block local to a preset.
///
/// Unlike global blocks (shared across presets), preset blocks belong
/// to a single preset and carry their own default parameters.
#[derive(Debug, Clone, Facet)]
pub struct PresetBlock {
    /// Unique block ID.
    pub id: BlockId,
    /// User-facing name.
    pub name: String,
    /// Functional category (amp, drive, eq, etc.).
    pub block_type: BlockType,
    /// Optional backing plugin.
    pub plugin_id: Option<PluginId>,
    /// Position in the signal chain.
    pub order: Order,
    /// Whether the block is bypassed.
    pub bypassed: bool,
    /// Default parameter values for this block.
    pub default_parameters: Vec<BlockParameter>,
}

impl PresetBlock {
    /// Create a new preset block with the given name and type.
    pub fn new(name: impl Into<String>, block_type: BlockType) -> Self {
        Self {
            id: BlockId::new(),
            name: name.into(),
            block_type,
            plugin_id: None,
            order: Order::default(),
            bypassed: false,
            default_parameters: Vec::new(),
        }
    }

    /// Set the backing plugin.
    #[must_use]
    pub fn with_plugin(mut self, plugin_id: PluginId) -> Self {
        self.plugin_id = Some(plugin_id);
        self
    }

    /// Set the signal chain order.
    #[must_use]
    pub fn with_order(mut self, order: Order) -> Self {
        self.order = order;
        self
    }

    /// Set the bypass state.
    #[must_use]
    pub fn bypassed(mut self) -> Self {
        self.bypassed = true;
        self
    }

    /// Add a default parameter.
    #[must_use]
    pub fn with_parameter(mut self, param: BlockParameter) -> Self {
        self.default_parameters.push(param);
        self
    }

    /// Find a parameter by ID.
    pub fn parameter(&self, id: &str) -> Option<&BlockParameter> {
        self.default_parameters.iter().find(|p| p.id == id)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// BlockParameter
// ─────────────────────────────────────────────────────────────────────────────

/// A named parameter with normalized value and display range.
#[derive(Debug, Clone, Facet)]
pub struct BlockParameter {
    /// Unique parameter identifier (e.g. "drive", "`eq_low`").
    pub id: String,
    /// Human-readable name.
    pub name: String,
    /// Normalized value in [0.0, 1.0].
    pub value: NormalizedF64,
    /// Minimum value in display units.
    pub min_display: f64,
    /// Maximum value in display units.
    pub max_display: f64,
    /// Display unit label (e.g. "dB", "Hz", "%").
    pub unit: String,
}

impl BlockParameter {
    /// Create a new block parameter.
    pub fn new(
        id: impl Into<String>,
        name: impl Into<String>,
        value: NormalizedF64,
        min_display: f64,
        max_display: f64,
        unit: impl Into<String>,
    ) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            value,
            min_display,
            max_display,
            unit: unit.into(),
        }
    }

    /// Compute the display value from the normalized value.
    ///
    /// Uses linear interpolation between `min_display` and `max_display`.
    pub fn display_value(&self) -> f64 {
        self.min_display + self.value.get() * (self.max_display - self.min_display)
    }

    /// Format the value with its unit for display.
    pub fn formatted(&self) -> String {
        if self.unit.is_empty() {
            format!("{:.1}", self.display_value())
        } else {
            format!("{:.1} {}", self.display_value(), self.unit)
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Snapshot
// ─────────────────────────────────────────────────────────────────────────────

/// A named snapshot within a preset.
///
/// Snapshots capture variation assignments and parameter overrides
/// for quick scene-level recall without changing the base preset.
#[derive(Debug, Clone, Facet)]
pub struct Snapshot {
    /// Unique snapshot ID.
    pub id: SnapshotId,
    /// User-facing name (e.g. "Verse", "Chorus", "Solo").
    pub name: String,
    /// Tags for filtering and organization.
    pub tags: Tags,
    /// Patch variation assignments for each section/layer.
    pub variation_assignments: Vec<PatchVariationAssignment>,
    /// Custom parameter overrides (by plugin parameter index).
    pub custom_overrides: Vec<ParameterValue>,
    /// Block-level overrides (bypass, parameter tweaks).
    pub block_overrides: Vec<BlockOverride>,
}

impl Snapshot {
    /// Create a new empty snapshot.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: SnapshotId::new(),
            name: name.into(),
            tags: Tags::new(),
            variation_assignments: Vec::new(),
            custom_overrides: Vec::new(),
            block_overrides: Vec::new(),
        }
    }

    /// Add a variation assignment.
    pub fn add_variation(&mut self, assignment: PatchVariationAssignment) {
        self.variation_assignments.push(assignment);
    }

    /// Add a custom parameter override.
    pub fn add_custom_override(&mut self, value: ParameterValue) {
        self.custom_overrides.push(value);
    }

    /// Add a block override.
    pub fn add_block_override(&mut self, block_override: BlockOverride) {
        self.block_overrides.push(block_override);
    }

    /// Find a block override by block ID.
    pub fn block_override(&self, block_id: BlockId) -> Option<&BlockOverride> {
        self.block_overrides.iter().find(|o| o.block_id == block_id)
    }
}

impl Taggable for Snapshot {
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
// Credit
// ─────────────────────────────────────────────────────────────────────────────

/// A credit for a preset (who made it, what role).
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
pub struct Credit {
    pub name: String,
    pub role: String,
}

impl Credit {
    /// Create a new credit.
    pub fn new(name: impl Into<String>, role: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            role: role.into(),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Inspiration
// ─────────────────────────────────────────────────────────────────────────────

/// An inspiration for a preset (artist, song, album).
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
pub struct Inspiration {
    pub name: String,
    pub source: Option<String>,
    pub notes: Option<String>,
}

impl Inspiration {
    /// Create a new inspiration.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            source: None,
            notes: None,
        }
    }

    /// Set the source (e.g. "Gravity - John Mayer").
    #[must_use]
    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }

    /// Set notes about the inspiration.
    #[must_use]
    pub fn with_notes(mut self, notes: impl Into<String>) -> Self {
        self.notes = Some(notes.into());
        self
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// PatchVariationAssignment
// ─────────────────────────────────────────────────────────────────────────────

/// Assigns a specific patch variation to a section/layer within a snapshot.
#[derive(Debug, Clone, Facet)]
pub struct PatchVariationAssignment {
    /// Target section.
    pub section_id: SectionId,
    /// Target layer index within the section.
    pub layer_index: u8,
    /// Variation to activate (None = use the default variation).
    pub variation_id: Option<VariationId>,
}

impl PatchVariationAssignment {
    /// Create a variation assignment.
    pub fn new(section_id: SectionId, layer_index: u8, variation_id: Option<VariationId>) -> Self {
        Self {
            section_id,
            layer_index,
            variation_id,
        }
    }

    /// Create a variation assignment with a specific variation.
    pub fn with_variation(section_id: SectionId, layer_index: u8, variation_id: VariationId) -> Self {
        Self {
            section_id,
            layer_index,
            variation_id: Some(variation_id),
        }
    }

    /// Create a variation assignment that uses the default.
    pub fn default_variation(section_id: SectionId, layer_index: u8) -> Self {
        Self {
            section_id,
            layer_index,
            variation_id: None,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use crate::category::BaseTone;

    use super::*;

    fn test_category() -> PresetCategory {
        PresetCategory::Generic {
            base_tone: BaseTone::Clean,
        }
    }

    // ── Preset ─────────────────────────────────────────────────────────

    #[test]
    fn preset_new_defaults() {
        let preset = Preset::new("My Clean", test_category());
        assert_eq!(preset.name, "My Clean");
        assert!(!preset.hidden);
        assert_eq!(preset.rating, Rating::UNRATED);
        assert!(preset.notes.is_none());
        assert!(preset.section_config.is_empty());
        assert!(preset.patch_assignments.is_empty());
        assert!(preset.global_block_config.is_empty());
        assert!(preset.blocks.is_empty());
        assert!(preset.snapshots.is_empty());
        assert!(preset.default_snapshot_id.is_none());
    }

    #[test]
    fn preset_add_and_find_snapshot() {
        let mut preset = Preset::new("Test", test_category());
        let snap = Snapshot::new("Verse");
        let snap_id = snap.id;

        let returned_id = preset.add_snapshot(snap);
        assert_eq!(returned_id, snap_id);
        assert_eq!(preset.snapshots.len(), 1);

        let found = preset.snapshot(snap_id).unwrap();
        assert_eq!(found.name, "Verse");
    }

    #[test]
    fn preset_default_snapshot() {
        let mut preset = Preset::new("Test", test_category());
        assert!(preset.default_snapshot().is_none());

        let snap = Snapshot::new("Default");
        let snap_id = snap.id;
        preset.add_snapshot(snap);
        preset.set_default_snapshot(snap_id);

        assert!(preset.default_snapshot().is_some());
        assert_eq!(preset.default_snapshot().unwrap().name, "Default");
    }

    #[test]
    fn preset_add_block() {
        let mut preset = Preset::new("Test", test_category());
        let block = PresetBlock::new("Drive", BlockType::Drive);
        let block_id = block.id;
        preset.add_block(block);

        assert_eq!(preset.blocks.len(), 1);
        assert!(preset.block(block_id).is_some());
    }

    #[test]
    fn preset_implements_taggable() {
        let mut preset = Preset::new("Test", test_category());
        let tag_id = TagId::new();
        preset.add_tag(tag_id);
        assert!(preset.has_tag(tag_id));
        assert_eq!(preset.name(), "Test");
    }

    // ── SectionConfig ──────────────────────────────────────────────────

    #[test]
    fn section_config_enabled() {
        let id = SectionId::new();
        let config = SectionConfig::enabled(id);
        assert!(config.enabled);
        assert!(config.routing_override.is_none());
    }

    #[test]
    fn section_config_disabled() {
        let id = SectionId::new();
        let config = SectionConfig::disabled(id);
        assert!(!config.enabled);
    }

    #[test]
    fn section_config_with_routing() {
        let id = SectionId::new();
        let config = SectionConfig::enabled(id).with_routing(SectionRouting::Parallel);
        assert!(config.routing_override.is_some());
    }

    // ── PatchAssignment ────────────────────────────────────────────────

    #[test]
    fn patch_assignment_defaults() {
        let section_id = SectionId::new();
        let patch_id = PatchId::new();
        let assignment = PatchAssignment::new(section_id, 0, patch_id);

        assert!(assignment.enabled);
        assert!(assignment.variation_id.is_none());
        assert!(assignment.volume_override.is_none());
        assert!(assignment.global_sends_override.is_none());
    }

    #[test]
    fn patch_assignment_builder_methods() {
        let section_id = SectionId::new();
        let patch_id = PatchId::new();
        let variation_id = VariationId::new();

        let assignment = PatchAssignment::new(section_id, 1, patch_id)
            .with_variation(variation_id)
            .with_volume(NormalizedF64::new(0.8))
            .with_sends(vec![]);

        assert_eq!(assignment.variation_id, Some(variation_id));
        assert!(assignment.volume_override.is_some());
        assert!(assignment.global_sends_override.is_some());
    }

    #[test]
    fn patch_assignment_disabled() {
        let assignment = PatchAssignment::new(SectionId::new(), 0, PatchId::new()).disabled();
        assert!(!assignment.enabled);
    }

    // ── GlobalBlockConfig ──────────────────────────────────────────────

    #[test]
    fn global_block_config_enabled() {
        let id = GlobalBlockId::new();
        let config = GlobalBlockConfig::enabled(id);
        assert!(config.enabled);
        assert!(config.order_override.is_none());
    }

    #[test]
    fn global_block_config_disabled() {
        let id = GlobalBlockId::new();
        let config = GlobalBlockConfig::disabled(id);
        assert!(!config.enabled);
    }

    #[test]
    fn global_block_config_with_order() {
        let id = GlobalBlockId::new();
        let config = GlobalBlockConfig::enabled(id).with_order(Order::new(5));
        assert_eq!(config.order_override, Some(Order::new(5)));
    }

    // ── PresetBlock ────────────────────────────────────────────────────

    #[test]
    fn preset_block_new_defaults() {
        let block = PresetBlock::new("EQ", BlockType::Eq);
        assert_eq!(block.name, "EQ");
        assert_eq!(block.block_type, BlockType::Eq);
        assert!(block.plugin_id.is_none());
        assert!(!block.bypassed);
        assert!(block.default_parameters.is_empty());
    }

    #[test]
    fn preset_block_builder_methods() {
        let plugin = PluginId::vst3("eq-uid", "Pro EQ");
        let param = BlockParameter::new("gain", "Gain", NormalizedF64::new(0.5), -12.0, 12.0, "dB");

        let block = PresetBlock::new("EQ", BlockType::Eq)
            .with_plugin(plugin)
            .with_order(Order::new(3))
            .bypassed()
            .with_parameter(param);

        assert!(block.plugin_id.is_some());
        assert_eq!(block.order, Order::new(3));
        assert!(block.bypassed);
        assert_eq!(block.default_parameters.len(), 1);
    }

    #[test]
    fn preset_block_find_parameter() {
        let param = BlockParameter::new("drive", "Drive", NormalizedF64::new(0.7), 0.0, 100.0, "%");
        let block = PresetBlock::new("Drive", BlockType::Drive).with_parameter(param);

        assert!(block.parameter("drive").is_some());
        assert_eq!(block.parameter("drive").unwrap().name, "Drive");
        assert!(block.parameter("nonexistent").is_none());
    }

    // ── BlockParameter ─────────────────────────────────────────────────

    #[test]
    fn block_parameter_display_value() {
        let param = BlockParameter::new("gain", "Gain", NormalizedF64::new(0.5), -12.0, 12.0, "dB");
        assert!((param.display_value() - 0.0).abs() < f64::EPSILON);
    }

    #[test]
    fn block_parameter_display_value_extremes() {
        let param_min = BlockParameter::new("gain", "Gain", NormalizedF64::ZERO, -12.0, 12.0, "dB");
        assert!((param_min.display_value() - (-12.0)).abs() < f64::EPSILON);

        let param_max = BlockParameter::new("gain", "Gain", NormalizedF64::ONE, -12.0, 12.0, "dB");
        assert!((param_max.display_value() - 12.0).abs() < f64::EPSILON);
    }

    #[test]
    fn block_parameter_formatted() {
        let param = BlockParameter::new("gain", "Gain", NormalizedF64::new(0.5), -12.0, 12.0, "dB");
        assert_eq!(param.formatted(), "0.0 dB");

        let no_unit = BlockParameter::new("mix", "Mix", NormalizedF64::new(0.5), 0.0, 100.0, "");
        assert_eq!(no_unit.formatted(), "50.0");
    }

    // ── Snapshot ───────────────────────────────────────────────────────

    #[test]
    fn snapshot_new_defaults() {
        let snap = Snapshot::new("Verse");
        assert_eq!(snap.name, "Verse");
        assert!(snap.tags.is_empty());
        assert!(snap.variation_assignments.is_empty());
        assert!(snap.custom_overrides.is_empty());
        assert!(snap.block_overrides.is_empty());
    }

    #[test]
    fn snapshot_add_variation() {
        let mut snap = Snapshot::new("Chorus");
        let section_id = SectionId::new();
        let variation_id = VariationId::new();

        snap.add_variation(PatchVariationAssignment::with_variation(section_id, 0, variation_id));
        assert_eq!(snap.variation_assignments.len(), 1);
        assert_eq!(snap.variation_assignments[0].variation_id, Some(variation_id));
    }

    #[test]
    fn snapshot_add_block_override() {
        let mut snap = Snapshot::new("Solo");
        let block_id = BlockId::new();
        let override_ = BlockOverride::new(block_id).with_bypass(true);

        snap.add_block_override(override_);
        assert_eq!(snap.block_overrides.len(), 1);
        assert!(snap.block_override(block_id).is_some());
    }

    #[test]
    fn snapshot_implements_taggable() {
        let mut snap = Snapshot::new("Test");
        let tag_id = TagId::new();
        snap.add_tag(tag_id);
        assert!(snap.has_tag(tag_id));
        assert_eq!(snap.name(), "Test");
    }

    // ── PatchVariationAssignment ───────────────────────────────────────

    #[test]
    fn patch_variation_assignment_constructors() {
        let section_id = SectionId::new();
        let variation_id = VariationId::new();

        let with = PatchVariationAssignment::with_variation(section_id, 0, variation_id);
        assert_eq!(with.variation_id, Some(variation_id));

        let default = PatchVariationAssignment::default_variation(section_id, 1);
        assert!(default.variation_id.is_none());
        assert_eq!(default.layer_index, 1);
    }

}
