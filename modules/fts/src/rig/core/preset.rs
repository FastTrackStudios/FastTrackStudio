//! Preset - Complete rig state
//!
//! A Preset combines patches from all sections into a complete sound.
//! Presets can have snapshots for song sections (Verse, Chorus, Bridge).

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use super::category::PresetCategory;
use super::tags::{Tags, Taggable, TagRegistry};
use super::{ParameterValue, PluginId, SectionRouting, SendLevel};

/// Complete rig state - combines patches from all sections.
///
/// A Preset defines the complete sound configuration, including which
/// patches are loaded in each layer, section routing overrides, and
/// global block configuration.
///
/// # Examples
///
/// - "Worship Lead" preset with clean amp + delay/reverb
/// - "Blues Clean" preset with warm amp + light chorus
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct Preset {
    /// Unique identifier
    pub id: Uuid,
    /// Preset name (e.g., "Worship Lead", "Blues Clean")
    pub name: String,
    /// Category for organization (5-level hierarchy with fallback)
    /// DEPRECATED: Use tags instead for more flexible organization
    pub category: PresetCategory,
    /// Tags for organization and fallback resolution
    #[serde(default)]
    pub tags: Tags,
    /// Star rating (1-5, 0 = unrated)
    #[serde(default)]
    pub rating: u8,
    /// User notes/description
    #[serde(default)]
    pub notes: Option<String>,
    /// Whether this preset is hidden from normal browsing
    #[serde(default)]
    pub hidden: bool,
    /// Section configurations (enable/disable, routing)
    pub section_config: Vec<SectionConfig>,
    /// Patch assignments for each layer
    pub patch_assignments: Vec<PatchAssignment>,
    /// Global block configurations
    pub global_block_config: Vec<GlobalBlockConfig>,
    /// Preset-specific block chain (each preset can have different blocks)
    #[serde(default)]
    pub blocks: Vec<PresetBlock>,
    /// Available snapshots (preset-level variations)
    pub snapshots: Vec<Snapshot>,
    /// Default snapshot to use
    pub default_snapshot_id: Option<Uuid>,
}

/// Configuration for a section within a preset.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct SectionConfig {
    /// Section ID this config applies to
    pub section_id: Uuid,
    /// Whether this section is enabled in this preset
    pub enabled: bool,
    /// Optional routing override (uses section default if None)
    pub routing_override: Option<SectionRouting>,
}

/// Assignment of a patch to a specific layer.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct PatchAssignment {
    /// Section ID
    pub section_id: Uuid,
    /// Layer index within the section
    pub layer_index: u8,
    /// Patch ID to load
    pub patch_id: Uuid,
    /// Variation to use (uses patch default if None)
    pub variation_id: Option<Uuid>,
    /// Whether this layer is enabled
    pub enabled: bool,
    /// Volume override (uses layer default if None)
    pub volume_override: Option<f64>,
    /// Global sends override (uses patch default if None)
    pub global_sends_override: Option<Vec<SendLevel>>,
}

/// Configuration for a global block within a preset.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct GlobalBlockConfig {
    /// Global block ID
    pub global_block_id: Uuid,
    /// Whether this block is enabled
    pub enabled: bool,
    /// Order override (reorder for this preset)
    pub order_override: Option<u8>,
}

/// A block definition within a preset's signal chain.
/// Each preset can have a completely different set of blocks.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct PresetBlock {
    /// Unique identifier for this block instance
    pub id: Uuid,
    /// Block name (e.g., "Drive", "Amp", "Delay")
    pub name: String,
    /// Block type for UI rendering (determines knob layout, colors, etc.)
    pub block_type: BlockType,
    /// Plugin identification (optional - can be virtual/UI-only blocks)
    pub plugin_id: Option<PluginId>,
    /// Position in the signal chain (lower = earlier)
    pub order: u8,
    /// Whether this block is bypassed by default
    #[serde(default)]
    pub bypassed: bool,
    /// Default parameter values for this block
    #[serde(default)]
    pub default_parameters: Vec<BlockParameter>,
}

/// Type of block for UI rendering purposes
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Facet, Default)]
pub enum BlockType {
    /// Input/DI block
    Input,
    /// Compressor/dynamics
    Compressor,
    /// Drive/overdrive/distortion pedal
    #[default]
    Drive,
    /// Amplifier
    Amp,
    /// Cabinet/IR loader
    Cabinet,
    /// EQ block
    Eq,
    /// Modulation effect (chorus, flanger, phaser, etc.)
    Modulation,
    /// Delay effect
    Delay,
    /// Reverb effect
    Reverb,
    /// Noise gate
    Gate,
    /// Volume/utility
    Volume,
    /// Custom/other
    Custom,
}

/// A named parameter for a preset block
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct BlockParameter {
    /// Parameter ID (unique within this block)
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Normalized value (0.0 - 1.0)
    pub value: f64,
    /// Minimum display value (for UI)
    #[serde(default)]
    pub min_display: f64,
    /// Maximum display value (for UI)
    #[serde(default = "default_max_display")]
    pub max_display: f64,
    /// Unit suffix (e.g., "ms", "dB", "%")
    #[serde(default)]
    pub unit: String,
}

fn default_max_display() -> f64 {
    100.0
}

/// A variation of a preset for song sections.
///
/// Snapshots allow different variations of a preset for different parts
/// of a song (Verse, Chorus, Bridge). They specify which patch variations
/// to use and can include custom parameter overrides.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct Snapshot {
    /// Unique identifier
    pub id: Uuid,
    /// Snapshot name (e.g., "Verse", "Chorus", "Bridge")
    pub name: String,
    /// Tags for organization (with auto-derived tracking)
    #[serde(default)]
    pub tags: Tags,
    /// Patch variation assignments per layer
    pub variation_assignments: Vec<PatchVariationAssignment>,
    /// Custom parameter overrides beyond the variations
    pub custom_overrides: Vec<ParameterValue>,
    /// Block parameter overrides for this snapshot
    /// Each entry maps block_id -> list of parameter overrides
    #[serde(default)]
    pub block_overrides: Vec<BlockOverride>,
}

/// Parameter overrides for a specific block in a snapshot
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct BlockOverride {
    /// Block ID this override applies to
    pub block_id: Uuid,
    /// Whether this block is bypassed in this snapshot (None = use preset default)
    pub bypassed: Option<bool>,
    /// Parameter value overrides (by parameter ID)
    pub parameters: Vec<ParameterOverride>,
}

/// A single parameter override
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct ParameterOverride {
    /// Parameter ID within the block
    pub param_id: String,
    /// Overridden value (0.0 - 1.0)
    pub value: f64,
}

/// Assignment of a patch variation within a snapshot.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct PatchVariationAssignment {
    /// Section ID
    pub section_id: Uuid,
    /// Layer index
    pub layer_index: u8,
    /// Variation ID to use (uses preset's assignment if None)
    pub variation_id: Option<Uuid>,
}

impl Preset {
    /// Create a new preset
    pub fn new(name: impl Into<String>, category: PresetCategory) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            category,
            tags: Tags::new(),
            rating: 0,
            notes: None,
            hidden: false,
            section_config: Vec::new(),
            patch_assignments: Vec::new(),
            global_block_config: Vec::new(),
            blocks: Vec::new(),
            snapshots: Vec::new(),
            default_snapshot_id: None,
        }
    }

    /// Create a new preset with tags (preferred method)
    pub fn with_tags(name: impl Into<String>, tags: Tags) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            category: PresetCategory::default(),
            tags,
            rating: 0,
            notes: None,
            hidden: false,
            section_config: Vec::new(),
            patch_assignments: Vec::new(),
            global_block_config: Vec::new(),
            blocks: Vec::new(),
            snapshots: Vec::new(),
            default_snapshot_id: None,
        }
    }

    /// Add a block to this preset's signal chain
    pub fn add_block(&mut self, block: PresetBlock) {
        self.blocks.push(block);
    }

    /// Get a block by ID
    pub fn get_block(&self, id: Uuid) -> Option<&PresetBlock> {
        self.blocks.iter().find(|b| b.id == id)
    }

    /// Get blocks sorted by order
    pub fn blocks_ordered(&self) -> Vec<&PresetBlock> {
        let mut blocks: Vec<_> = self.blocks.iter().collect();
        blocks.sort_by_key(|b| b.order);
        blocks
    }

    /// Set the rating (1-5, 0 = unrated)
    pub fn set_rating(&mut self, rating: u8) {
        self.rating = rating.min(5);
    }

    /// Set notes
    pub fn set_notes(&mut self, notes: impl Into<String>) {
        self.notes = Some(notes.into());
    }

    /// Hide this preset from normal browsing
    pub fn hide(&mut self) {
        self.hidden = true;
    }

    /// Unhide this preset
    pub fn unhide(&mut self) {
        self.hidden = false;
    }

    /// Add a section configuration
    pub fn add_section_config(&mut self, config: SectionConfig) {
        self.section_config.push(config);
    }

    /// Add a patch assignment
    pub fn add_patch_assignment(&mut self, assignment: PatchAssignment) {
        self.patch_assignments.push(assignment);
    }

    /// Add a global block configuration
    pub fn add_global_block_config(&mut self, config: GlobalBlockConfig) {
        self.global_block_config.push(config);
    }

    /// Add a snapshot
    pub fn add_snapshot(&mut self, snapshot: Snapshot) {
        if self.default_snapshot_id.is_none() {
            self.default_snapshot_id = Some(snapshot.id);
        }
        self.snapshots.push(snapshot);
    }

    /// Get a snapshot by ID
    pub fn get_snapshot(&self, id: Uuid) -> Option<&Snapshot> {
        self.snapshots.iter().find(|s| s.id == id)
    }

    /// Get the default snapshot
    pub fn default_snapshot(&self) -> Option<&Snapshot> {
        self.default_snapshot_id.and_then(|id| self.get_snapshot(id))
    }

    /// Get the patch assignment for a specific layer
    pub fn get_patch_assignment(&self, section_id: Uuid, layer_index: u8) -> Option<&PatchAssignment> {
        self.patch_assignments
            .iter()
            .find(|a| a.section_id == section_id && a.layer_index == layer_index)
    }

    /// Get the section configuration
    pub fn get_section_config(&self, section_id: Uuid) -> Option<&SectionConfig> {
        self.section_config.iter().find(|c| c.section_id == section_id)
    }
}

impl SectionConfig {
    /// Create a new section config
    pub fn new(section_id: Uuid, enabled: bool) -> Self {
        Self {
            section_id,
            enabled,
            routing_override: None,
        }
    }

    /// Enable with routing override
    pub fn with_routing(section_id: Uuid, routing: SectionRouting) -> Self {
        Self {
            section_id,
            enabled: true,
            routing_override: Some(routing),
        }
    }
}

impl PatchAssignment {
    /// Create a new patch assignment
    pub fn new(section_id: Uuid, layer_index: u8, patch_id: Uuid) -> Self {
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

    /// With a specific variation
    pub fn with_variation(mut self, variation_id: Uuid) -> Self {
        self.variation_id = Some(variation_id);
        self
    }

    /// With volume override
    pub fn with_volume(mut self, volume: f64) -> Self {
        self.volume_override = Some(volume.clamp(0.0, 1.0));
        self
    }
}

impl GlobalBlockConfig {
    /// Create a new global block config
    pub fn new(global_block_id: Uuid, enabled: bool) -> Self {
        Self {
            global_block_id,
            enabled,
            order_override: None,
        }
    }

    /// With order override
    pub fn with_order(mut self, order: u8) -> Self {
        self.order_override = Some(order);
        self
    }
}

impl Snapshot {
    /// Create a new snapshot
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            tags: Tags::new(),
            variation_assignments: Vec::new(),
            custom_overrides: Vec::new(),
            block_overrides: Vec::new(),
        }
    }

    /// Create a new snapshot with auto-tagging from its name
    pub fn new_with_auto_tags(name: impl Into<String>, registry: &TagRegistry) -> Self {
        let name = name.into();
        let auto_tags = super::tags::derive_tags_from_name(&name, registry);
        Self {
            id: Uuid::new_v4(),
            name,
            tags: Tags::with_auto(std::iter::empty::<Uuid>(), auto_tags),
            variation_assignments: Vec::new(),
            custom_overrides: Vec::new(),
            block_overrides: Vec::new(),
        }
    }

    /// Add a variation assignment
    pub fn add_variation_assignment(&mut self, assignment: PatchVariationAssignment) {
        self.variation_assignments.push(assignment);
    }

    /// Add a custom parameter override
    pub fn add_custom_override(&mut self, param: ParameterValue) {
        self.custom_overrides.push(param);
    }

    /// Add a block override for this snapshot
    pub fn add_block_override(&mut self, block_override: BlockOverride) {
        self.block_overrides.push(block_override);
    }

    /// Get the block override for a specific block
    pub fn get_block_override(&self, block_id: Uuid) -> Option<&BlockOverride> {
        self.block_overrides.iter().find(|o| o.block_id == block_id)
    }

    /// Common snapshot constructors
    pub fn verse() -> Self {
        Self::new("Verse")
    }
    pub fn chorus() -> Self {
        Self::new("Chorus")
    }
    pub fn bridge() -> Self {
        Self::new("Bridge")
    }
    pub fn intro() -> Self {
        Self::new("Intro")
    }
    pub fn outro() -> Self {
        Self::new("Outro")
    }
}

impl PatchVariationAssignment {
    /// Create a new variation assignment
    pub fn new(section_id: Uuid, layer_index: u8, variation_id: Option<Uuid>) -> Self {
        Self {
            section_id,
            layer_index,
            variation_id,
        }
    }
}

impl PresetBlock {
    /// Create a new preset block
    pub fn new(name: impl Into<String>, block_type: BlockType, order: u8) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            block_type,
            plugin_id: None,
            order,
            bypassed: false,
            default_parameters: Vec::new(),
        }
    }

    /// Create a drive pedal block
    pub fn drive(name: impl Into<String>, order: u8) -> Self {
        Self::new(name, BlockType::Drive, order)
    }

    /// Create an amp block
    pub fn amp(name: impl Into<String>, order: u8) -> Self {
        Self::new(name, BlockType::Amp, order)
    }

    /// Create a cabinet/IR block
    pub fn cabinet(name: impl Into<String>, order: u8) -> Self {
        Self::new(name, BlockType::Cabinet, order)
    }

    /// Create a delay block
    pub fn delay(name: impl Into<String>, order: u8) -> Self {
        Self::new(name, BlockType::Delay, order)
    }

    /// Create a reverb block
    pub fn reverb(name: impl Into<String>, order: u8) -> Self {
        Self::new(name, BlockType::Reverb, order)
    }

    /// Create a compressor block
    pub fn compressor(name: impl Into<String>, order: u8) -> Self {
        Self::new(name, BlockType::Compressor, order)
    }

    /// Create a modulation block
    pub fn modulation(name: impl Into<String>, order: u8) -> Self {
        Self::new(name, BlockType::Modulation, order)
    }

    /// Create an EQ block
    pub fn eq(name: impl Into<String>, order: u8) -> Self {
        Self::new(name, BlockType::Eq, order)
    }

    /// Set bypassed state
    pub fn bypassed(mut self, bypassed: bool) -> Self {
        self.bypassed = bypassed;
        self
    }

    /// Add a parameter to this block
    pub fn with_param(mut self, param: BlockParameter) -> Self {
        self.default_parameters.push(param);
        self
    }

    /// Add multiple parameters
    pub fn with_params(mut self, params: impl IntoIterator<Item = BlockParameter>) -> Self {
        self.default_parameters.extend(params);
        self
    }

    /// Get a parameter by ID
    pub fn get_param(&self, id: &str) -> Option<&BlockParameter> {
        self.default_parameters.iter().find(|p| p.id == id)
    }

    /// Get parameter value by ID, with optional snapshot override
    pub fn get_param_value(&self, id: &str, snapshot_override: Option<&BlockOverride>) -> Option<f64> {
        // Check snapshot override first
        if let Some(override_val) = snapshot_override
            .and_then(|o| o.parameters.iter().find(|p| p.param_id == id))
        {
            return Some(override_val.value);
        }
        // Fall back to default
        self.get_param(id).map(|p| p.value)
    }
}

impl BlockParameter {
    /// Create a new block parameter
    pub fn new(id: impl Into<String>, name: impl Into<String>, value: f64) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            value: value.clamp(0.0, 1.0),
            min_display: 0.0,
            max_display: 100.0,
            unit: String::new(),
        }
    }

    /// Set display range
    pub fn with_range(mut self, min: f64, max: f64) -> Self {
        self.min_display = min;
        self.max_display = max;
        self
    }

    /// Set unit suffix
    pub fn with_unit(mut self, unit: impl Into<String>) -> Self {
        self.unit = unit.into();
        self
    }

    /// Common parameter: Gain/Drive (0-100%)
    pub fn gain(value: f64) -> Self {
        Self::new("gain", "Gain", value).with_unit("%")
    }

    /// Common parameter: Level/Volume (0-100%)
    pub fn level(value: f64) -> Self {
        Self::new("level", "Level", value).with_unit("%")
    }

    /// Common parameter: Tone (0-100%)
    pub fn tone(value: f64) -> Self {
        Self::new("tone", "Tone", value).with_unit("%")
    }

    /// Common parameter: Mix (0-100%)
    pub fn mix(value: f64) -> Self {
        Self::new("mix", "Mix", value).with_unit("%")
    }

    /// Common parameter: Time in ms
    pub fn time_ms(id: &str, name: &str, value: f64, max_ms: f64) -> Self {
        Self::new(id, name, value).with_range(0.0, max_ms).with_unit("ms")
    }

    /// Common parameter: Decay/Size (0-100%)
    pub fn decay(value: f64) -> Self {
        Self::new("decay", "Decay", value).with_unit("%")
    }
}

impl BlockOverride {
    /// Create a new block override
    pub fn new(block_id: Uuid) -> Self {
        Self {
            block_id,
            bypassed: None,
            parameters: Vec::new(),
        }
    }

    /// Set bypass state
    pub fn with_bypass(mut self, bypassed: bool) -> Self {
        self.bypassed = Some(bypassed);
        self
    }

    /// Add a parameter override
    pub fn with_param(mut self, param_id: impl Into<String>, value: f64) -> Self {
        self.parameters.push(ParameterOverride {
            param_id: param_id.into(),
            value: value.clamp(0.0, 1.0),
        });
        self
    }
}

impl ParameterOverride {
    /// Create a new parameter override
    pub fn new(param_id: impl Into<String>, value: f64) -> Self {
        Self {
            param_id: param_id.into(),
            value: value.clamp(0.0, 1.0),
        }
    }
}

impl std::fmt::Display for BlockType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BlockType::Input => write!(f, "Input"),
            BlockType::Compressor => write!(f, "Compressor"),
            BlockType::Drive => write!(f, "Drive"),
            BlockType::Amp => write!(f, "Amp"),
            BlockType::Cabinet => write!(f, "Cabinet"),
            BlockType::Eq => write!(f, "EQ"),
            BlockType::Modulation => write!(f, "Modulation"),
            BlockType::Delay => write!(f, "Delay"),
            BlockType::Reverb => write!(f, "Reverb"),
            BlockType::Gate => write!(f, "Gate"),
            BlockType::Volume => write!(f, "Volume"),
            BlockType::Custom => write!(f, "Custom"),
        }
    }
}

// ============================================================================
// Taggable Implementations
// ============================================================================

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
