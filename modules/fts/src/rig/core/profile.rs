//! Profile - Purpose-built rig configuration with scene templates
//!
//! A Profile is a curated configuration for a specific context,
//! like "Worship" or "Jazz". Profiles contain scene templates that
//! REFERENCE presets from the global registry (not contain them).
//!
//! This enables:
//! - Updating a profile's preset reference updates all songs using it
//! - Parameter auto-save per scene template
//! - Flexible preset assignment via category (with fallback) or direct ID

use std::collections::HashMap;

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use super::category::PresetCategory;
use super::{ParameterValue, Preset};

/// A purpose-built rig configuration for a specific context.
///
/// Profiles are scene assignment containers that REFERENCE presets
/// (not contain them). Presets exist in the global category hierarchy;
/// profiles reference them via scene templates.
///
/// # Examples
///
/// ```ignore
/// Profile: "Worship"
/// ├── Scene Template: "Clean" → references "Blues Clean" preset
/// ├── Scene Template: "Lead" → references "John Mayer Lead" preset
/// └── Scene Template: "Solo" → references "Blues Lead" preset
///
/// When a song uses "Worship Profile - Clean", it loads "Blues Clean".
/// If Worship Profile is updated to point to "Jazz Clean", all songs update.
/// ```
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct Profile {
    /// Unique identifier
    pub id: Uuid,
    /// Profile name (e.g., "Worship", "Jazz", "Metal")
    pub name: String,
    /// Which rig this profile is designed for
    pub rig_id: Uuid,
    /// Optional description
    pub description: Option<String>,

    /// Scene templates that reference presets from the global registry.
    /// This is the NEW preferred way to organize profiles.
    pub scene_templates: Vec<SceneTemplate>,

    /// Default scene template to load when profile is activated
    pub default_scene_template_id: Option<Uuid>,

    /// Legacy: Presets directly contained in this profile.
    /// Kept for backward compatibility. New code should use scene_templates.
    #[serde(default)]
    pub presets: Vec<Preset>,

    /// Legacy: Default preset ID (for backward compatibility)
    #[serde(default)]
    pub default_preset_id: Option<Uuid>,

    /// Metadata (author, version, etc.)
    #[serde(default)]
    pub metadata: HashMap<String, String>,
}

/// A scene template that references a preset from the global registry.
///
/// Scene templates allow profiles to point to presets without containing them.
/// When a song uses a profile's scene, it resolves the preset reference.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct SceneTemplate {
    /// Unique identifier
    pub id: Uuid,
    /// Template name (e.g., "Clean", "Lead", "Solo")
    pub name: String,
    /// How to resolve the preset for this scene
    pub preset_reference: PresetReference,
    /// Default snapshot ID within the resolved preset
    pub default_snapshot_id: Option<Uuid>,
    /// Parameter state auto-saved for this scene
    pub parameter_state: SceneParameterState,
}

/// How a scene template references a preset.
#[derive(Debug, Clone, Serialize, Deserialize, Facet, PartialEq)]
#[repr(u8)]
pub enum PresetReference {
    /// Direct reference by preset ID.
    /// Use when you want a specific preset that won't change.
    Direct { preset_id: Uuid },

    /// Category-based reference with fallback resolution.
    /// Use when you want the best available preset in a category.
    /// Enables fallback: if "Gravity Lead" unavailable, use "Blues Lead".
    Category { category: PresetCategory },
}

/// Parameter state auto-saved for a scene template.
///
/// When a user adjusts knobs while in a scene, the state is saved here.
/// Loading the scene restores these parameter values.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct SceneParameterState {
    /// Unique identifier for this state snapshot
    pub id: Uuid,
    /// Timestamp of last modification (Unix epoch millis)
    pub last_modified: Option<u64>,
    /// Block parameter values by block ID
    /// Key: block UUID, Value: list of parameter values
    pub block_parameters: HashMap<Uuid, Vec<ParameterValue>>,
    /// Section states (enabled, volume)
    pub section_states: HashMap<Uuid, SectionState>,
    /// Global block bypass states
    pub global_block_bypass: HashMap<Uuid, bool>,
}

/// Saved state for a section within a scene.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct SectionState {
    /// Whether the section is enabled
    pub enabled: bool,
    /// Section volume (0.0 - 1.0)
    pub volume: f64,
    /// Layer states within this section
    pub layer_states: Vec<LayerState>,
}

/// Saved state for a layer within a section.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct LayerState {
    /// Whether the layer is enabled
    pub enabled: bool,
    /// Layer volume (0.0 - 1.0)
    pub volume: f64,
    /// Currently selected variation ID
    pub variation_id: Option<Uuid>,
}

// ============================================================================
// Profile Implementation
// ============================================================================

impl Profile {
    /// Create a new profile
    pub fn new(name: impl Into<String>, rig_id: Uuid) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            rig_id,
            description: None,
            scene_templates: Vec::new(),
            default_scene_template_id: None,
            presets: Vec::new(),
            default_preset_id: None,
            metadata: HashMap::new(),
        }
    }

    /// Add a scene template to the profile
    pub fn add_scene_template(&mut self, template: SceneTemplate) {
        if self.default_scene_template_id.is_none() {
            self.default_scene_template_id = Some(template.id);
        }
        self.scene_templates.push(template);
    }

    /// Get a scene template by ID
    pub fn get_scene_template(&self, id: Uuid) -> Option<&SceneTemplate> {
        self.scene_templates.iter().find(|t| t.id == id)
    }

    /// Get a mutable scene template by ID
    pub fn get_scene_template_mut(&mut self, id: Uuid) -> Option<&mut SceneTemplate> {
        self.scene_templates.iter_mut().find(|t| t.id == id)
    }

    /// Get a scene template by name (case-insensitive)
    pub fn get_scene_template_by_name(&self, name: &str) -> Option<&SceneTemplate> {
        let name_lower = name.to_lowercase();
        self.scene_templates
            .iter()
            .find(|t| t.name.to_lowercase() == name_lower)
    }

    /// Get the default scene template
    pub fn default_scene_template(&self) -> Option<&SceneTemplate> {
        self.default_scene_template_id
            .and_then(|id| self.get_scene_template(id))
    }

    // ========================================================================
    // Legacy methods for backward compatibility
    // ========================================================================

    /// Add a preset to the profile (legacy method)
    #[deprecated(note = "Use add_scene_template with PresetReference instead")]
    pub fn add_preset(&mut self, preset: Preset) {
        if self.default_preset_id.is_none() {
            self.default_preset_id = Some(preset.id);
        }
        self.presets.push(preset);
    }

    /// Get a preset by ID (legacy method)
    pub fn get_preset(&self, id: Uuid) -> Option<&Preset> {
        self.presets.iter().find(|p| p.id == id)
    }

    /// Get the default preset (legacy method)
    pub fn default_preset(&self) -> Option<&Preset> {
        self.default_preset_id.and_then(|id| self.get_preset(id))
    }
}

// ============================================================================
// SceneTemplate Implementation
// ============================================================================

impl SceneTemplate {
    /// Create a new scene template with a direct preset reference
    pub fn direct(name: impl Into<String>, preset_id: Uuid) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            preset_reference: PresetReference::Direct { preset_id },
            default_snapshot_id: None,
            parameter_state: SceneParameterState::default(),
        }
    }

    /// Create a new scene template with a category-based reference
    pub fn category(name: impl Into<String>, category: PresetCategory) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            preset_reference: PresetReference::Category { category },
            default_snapshot_id: None,
            parameter_state: SceneParameterState::default(),
        }
    }

    /// Set the default snapshot
    pub fn with_snapshot(mut self, snapshot_id: Uuid) -> Self {
        self.default_snapshot_id = Some(snapshot_id);
        self
    }

    /// Update a parameter in the scene's saved state
    pub fn save_parameter(&mut self, block_id: Uuid, param: ParameterValue) {
        let params = self.parameter_state.block_parameters.entry(block_id).or_default();

        // Replace existing parameter or add new
        if let Some(existing) = params.iter_mut().find(|p| p.index == param.index) {
            existing.value = param.value;
        } else {
            params.push(param);
        }

        // Update timestamp
        self.parameter_state.last_modified = Some(
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_millis() as u64)
                .unwrap_or(0),
        );
    }

    /// Get saved parameters for a block
    pub fn get_block_parameters(&self, block_id: Uuid) -> Option<&Vec<ParameterValue>> {
        self.parameter_state.block_parameters.get(&block_id)
    }

    /// Clear all saved parameters
    pub fn clear_parameters(&mut self) {
        self.parameter_state = SceneParameterState::default();
    }
}

// ============================================================================
// PresetReference Implementation
// ============================================================================

impl PresetReference {
    /// Create a direct preset reference
    pub fn direct(preset_id: Uuid) -> Self {
        Self::Direct { preset_id }
    }

    /// Create a category-based reference
    pub fn category(category: PresetCategory) -> Self {
        Self::Category { category }
    }

    /// Check if this is a direct reference
    pub fn is_direct(&self) -> bool {
        matches!(self, Self::Direct { .. })
    }

    /// Check if this is a category reference
    pub fn is_category(&self) -> bool {
        matches!(self, Self::Category { .. })
    }

    /// Get the direct preset ID if this is a direct reference
    pub fn preset_id(&self) -> Option<Uuid> {
        match self {
            Self::Direct { preset_id } => Some(*preset_id),
            Self::Category { .. } => None,
        }
    }

    /// Get the category if this is a category reference
    pub fn get_category(&self) -> Option<&PresetCategory> {
        match self {
            Self::Direct { .. } => None,
            Self::Category { category } => Some(category),
        }
    }
}

// ============================================================================
// SceneParameterState Implementation
// ============================================================================

impl SceneParameterState {
    /// Create a new empty parameter state
    pub fn new() -> Self {
        Self {
            id: Uuid::new_v4(),
            last_modified: None,
            block_parameters: HashMap::new(),
            section_states: HashMap::new(),
            global_block_bypass: HashMap::new(),
        }
    }

    /// Check if any parameters have been saved
    pub fn is_empty(&self) -> bool {
        self.block_parameters.is_empty()
            && self.section_states.is_empty()
            && self.global_block_bypass.is_empty()
    }

    /// Check if there are unsaved changes (modified after given timestamp)
    pub fn modified_since(&self, timestamp: u64) -> bool {
        self.last_modified.map(|t| t > timestamp).unwrap_or(false)
    }
}

impl Default for SceneParameterState {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// SectionState Implementation
// ============================================================================

impl SectionState {
    /// Create a new section state
    pub fn new(enabled: bool, volume: f64) -> Self {
        Self {
            enabled,
            volume: volume.clamp(0.0, 1.0),
            layer_states: Vec::new(),
        }
    }

    /// Add a layer state
    pub fn add_layer(&mut self, layer: LayerState) {
        self.layer_states.push(layer);
    }
}

impl Default for SectionState {
    fn default() -> Self {
        Self::new(true, 1.0)
    }
}

// ============================================================================
// LayerState Implementation
// ============================================================================

impl LayerState {
    /// Create a new layer state
    pub fn new(enabled: bool, volume: f64) -> Self {
        Self {
            enabled,
            volume: volume.clamp(0.0, 1.0),
            variation_id: None,
        }
    }

    /// With a specific variation
    pub fn with_variation(mut self, variation_id: Uuid) -> Self {
        self.variation_id = Some(variation_id);
        self
    }
}

impl Default for LayerState {
    fn default() -> Self {
        Self::new(true, 1.0)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rig::core::category::{BaseTone, Genre};

    #[test]
    fn test_scene_template_direct() {
        let preset_id = Uuid::new_v4();
        let template = SceneTemplate::direct("Clean", preset_id);

        assert_eq!(template.name, "Clean");
        assert!(template.preset_reference.is_direct());
        assert_eq!(template.preset_reference.preset_id(), Some(preset_id));
    }

    #[test]
    fn test_scene_template_category() {
        let category = PresetCategory::genre(BaseTone::Lead, Genre::Blues);
        let template = SceneTemplate::category("Lead", category.clone());

        assert_eq!(template.name, "Lead");
        assert!(template.preset_reference.is_category());
        assert_eq!(template.preset_reference.get_category(), Some(&category));
    }

    #[test]
    fn test_parameter_save() {
        let preset_id = Uuid::new_v4();
        let mut template = SceneTemplate::direct("Clean", preset_id);

        let block_id = Uuid::new_v4();
        template.save_parameter(block_id, ParameterValue::new(0, 0.5));
        template.save_parameter(block_id, ParameterValue::new(1, 0.75));

        let params = template.get_block_parameters(block_id).unwrap();
        assert_eq!(params.len(), 2);

        // Update existing parameter
        template.save_parameter(block_id, ParameterValue::new(0, 0.8));
        let params = template.get_block_parameters(block_id).unwrap();
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].value, 0.8);
    }

    #[test]
    fn test_profile_scene_templates() {
        let rig_id = Uuid::new_v4();
        let mut profile = Profile::new("Worship", rig_id);

        let clean_id = Uuid::new_v4();
        let lead_id = Uuid::new_v4();

        profile.add_scene_template(SceneTemplate::direct("Clean", clean_id));
        profile.add_scene_template(SceneTemplate::category(
            "Lead",
            PresetCategory::genre(BaseTone::Lead, Genre::Blues),
        ));

        assert_eq!(profile.scene_templates.len(), 2);
        assert!(profile.default_scene_template().is_some());
        assert_eq!(profile.default_scene_template().unwrap().name, "Clean");

        let lead = profile.get_scene_template_by_name("lead").unwrap();
        assert!(lead.preset_reference.get_category().is_some());
    }
}
