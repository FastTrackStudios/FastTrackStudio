//! Profile — scene template management tied to a rig.
//!
//! A [`Profile`] groups [`SceneTemplate`]s that each describe a starting point
//! for a scene: which preset to use (directly or by category), an optional
//! default snapshot, and saved parameter state.

use std::collections::HashMap;

use facet::Facet;

use uuid::Uuid;

use crate::category::PresetCategory;
use crate::id::{BlockId, GlobalBlockId, PresetId, ProfileId, RigId, SceneTemplateId, SectionId, SnapshotId, VariationId};
use crate::module_preset::ModuleOverride;
use crate::preset::BlockOverride;
use crate::parameter::ParameterValue;
use crate::tags::{Taggable, Tags};

// ─────────────────────────────────────────────────────────────────────────────
// Profile
// ─────────────────────────────────────────────────────────────────────────────

/// A named collection of scene templates associated with a rig.
///
/// Profiles let a player maintain multiple "template sets" for the same rig
/// (e.g. "Sunday Morning" vs "Wednesday Night" vs "Studio Session").
#[derive(Debug, Clone, Facet)]
pub struct Profile {
    pub id: ProfileId,
    pub name: String,
    pub rig_id: RigId,

    pub description: Option<String>,
    pub scene_templates: Vec<SceneTemplate>,

    pub default_scene_template_id: Option<SceneTemplateId>,

    pub metadata: HashMap<String, String>,
    pub tags: Tags,
}

impl Profile {
    /// Create a new profile with the given name and rig association.
    pub fn new(name: impl Into<String>, rig_id: RigId) -> Self {
        Self {
            id: ProfileId::new(),
            name: name.into(),
            rig_id,
            description: None,
            scene_templates: Vec::new(),
            default_scene_template_id: None,
            metadata: HashMap::new(),
            tags: Tags::new(),
        }
    }

    /// Add a scene template. If this is the first template, it becomes the default.
    pub fn add_scene_template(&mut self, template: SceneTemplate) {
        if self.scene_templates.is_empty() {
            self.default_scene_template_id = Some(template.id);
        }
        self.scene_templates.push(template);
    }

    /// Look up a scene template by ID.
    pub fn get_scene_template(&self, id: SceneTemplateId) -> Option<&SceneTemplate> {
        self.scene_templates.iter().find(|t| t.id == id)
    }

    /// Look up a scene template by ID (mutable).
    pub fn get_scene_template_mut(&mut self, id: SceneTemplateId) -> Option<&mut SceneTemplate> {
        self.scene_templates.iter_mut().find(|t| t.id == id)
    }

    /// Look up a scene template by name (first match).
    pub fn get_scene_template_by_name(&self, name: &str) -> Option<&SceneTemplate> {
        self.scene_templates.iter().find(|t| t.name == name)
    }

    /// Get the default scene template, if one is set and still exists.
    pub fn default_scene_template(&self) -> Option<&SceneTemplate> {
        self.default_scene_template_id
            .and_then(|id| self.get_scene_template(id))
    }
}

impl Taggable for Profile {
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
// SceneTemplate
// ─────────────────────────────────────────────────────────────────────────────

/// A template for creating a scene — preset reference, optional snapshot, and
/// saved parameter tweaks.
#[derive(Debug, Clone, Facet)]
pub struct SceneTemplate {
    pub id: SceneTemplateId,
    pub name: String,
    pub preset_reference: PresetReference,

    pub default_snapshot_id: Option<SnapshotId>,
    pub parameter_state: SceneParameterState,
    /// Module overrides for this scene template — replace specific modules
    /// while keeping the rest from the base preset.
    pub module_overrides: Vec<ModuleOverride>,
    /// Block-level parameter overrides — fine-grained tweaks on top of the
    /// loaded preset. Only stores what differs from the base.
    pub block_overrides: Vec<BlockOverride>,
    pub tags: Tags,
}

impl SceneTemplate {
    /// Create a template that directly references a specific preset.
    pub fn direct(name: impl Into<String>, preset_id: PresetId) -> Self {
        Self {
            id: SceneTemplateId::new(),
            name: name.into(),
            preset_reference: PresetReference::direct(preset_id),
            default_snapshot_id: None,
            parameter_state: SceneParameterState::new(),
            module_overrides: Vec::new(),
            block_overrides: Vec::new(),
            tags: Tags::new(),
        }
    }

    /// Create a template that references a preset by category.
    pub fn category(name: impl Into<String>, category: PresetCategory) -> Self {
        Self {
            id: SceneTemplateId::new(),
            name: name.into(),
            preset_reference: PresetReference::category(category),
            default_snapshot_id: None,
            parameter_state: SceneParameterState::new(),
            module_overrides: Vec::new(),
            block_overrides: Vec::new(),
            tags: Tags::new(),
        }
    }

    /// Builder: set the default snapshot.
    #[must_use]
    pub fn with_snapshot(mut self, snapshot_id: SnapshotId) -> Self {
        self.default_snapshot_id = Some(snapshot_id);
        self
    }

    /// Save a parameter value for a block.
    pub fn save_parameter(&mut self, block_id: BlockId, value: ParameterValue) {
        self.parameter_state
            .block_parameters
            .entry(block_id)
            .or_default()
            .push(value);
    }

    /// Get the saved parameters for a block.
    pub fn get_block_parameters(&self, block_id: BlockId) -> Option<&[ParameterValue]> {
        self.parameter_state
            .block_parameters
            .get(&block_id)
            .map(Vec::as_slice)
    }

    /// Clear all saved parameter state.
    pub fn clear_parameters(&mut self) {
        self.parameter_state = SceneParameterState::new();
    }

    /// Add a module override for this scene template.
    pub fn add_module_override(&mut self, module_override: ModuleOverride) {
        self.module_overrides.push(module_override);
    }

    /// Add a block-level parameter override for this scene template.
    pub fn add_block_override(&mut self, block_override: BlockOverride) {
        self.block_overrides.push(block_override);
    }
}

impl Taggable for SceneTemplate {
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
// PresetReference
// ─────────────────────────────────────────────────────────────────────────────

/// How a scene template locates its preset — either by exact ID or by category
/// (letting the system pick the best match at runtime).
#[derive(Debug, Clone, Facet)]
#[repr(C)]
pub enum PresetReference {
    /// Reference a specific preset by ID.
    Direct { preset_id: PresetId },
    /// Reference a preset by category — the system resolves the best match.
    Category { category: PresetCategory },
}

impl PresetReference {
    /// Create a direct preset reference.
    pub fn direct(preset_id: PresetId) -> Self {
        Self::Direct { preset_id }
    }

    /// Create a category-based preset reference.
    pub fn category(category: PresetCategory) -> Self {
        Self::Category { category }
    }

    /// Whether this is a direct reference.
    pub fn is_direct(&self) -> bool {
        matches!(self, Self::Direct { .. })
    }

    /// Whether this is a category reference.
    pub fn is_category(&self) -> bool {
        matches!(self, Self::Category { .. })
    }

    /// Get the preset ID if this is a direct reference.
    pub fn preset_id(&self) -> Option<PresetId> {
        match self {
            Self::Direct { preset_id } => Some(*preset_id),
            Self::Category { .. } => None,
        }
    }

    /// Get the category if this is a category reference.
    pub fn get_category(&self) -> Option<&PresetCategory> {
        match self {
            Self::Category { category } => Some(category),
            Self::Direct { .. } => None,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// SceneParameterState
// ─────────────────────────────────────────────────────────────────────────────

/// Saved parameter tweaks for a scene — block parameters, section states, and
/// global block bypass overrides.
#[derive(Debug, Clone, Facet)]
pub struct SceneParameterState {
    /// Internal identity for this state snapshot.
    pub id: Uuid,
    /// Unix timestamp (seconds) of last modification, if any.

    pub last_modified: Option<u64>,
    /// Per-block parameter overrides.

    pub block_parameters: HashMap<BlockId, Vec<ParameterValue>>,
    /// Per-section states (enabled, volume, layers).

    pub section_states: HashMap<SectionId, SectionState>,
    /// Global block bypass overrides.

    pub global_block_bypass: HashMap<GlobalBlockId, bool>,
}

impl SceneParameterState {
    /// Create a fresh, empty parameter state.
    pub fn new() -> Self {
        Self {
            id: Uuid::new_v4(),
            last_modified: None,
            block_parameters: HashMap::new(),
            section_states: HashMap::new(),
            global_block_bypass: HashMap::new(),
        }
    }

    /// Whether all parameter maps are empty (no overrides).
    pub fn is_empty(&self) -> bool {
        self.block_parameters.is_empty()
            && self.section_states.is_empty()
            && self.global_block_bypass.is_empty()
    }

    /// Whether this state has been modified since the given timestamp.
    pub fn modified_since(&self, timestamp: u64) -> bool {
        self.last_modified.is_some_and(|t| t > timestamp)
    }
}

impl Default for SceneParameterState {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// SectionState
// ─────────────────────────────────────────────────────────────────────────────

/// Per-section override: enabled flag, volume, and per-layer states.
#[derive(Debug, Clone, Facet)]
pub struct SectionState {
    pub enabled: bool,
    /// Volume level, clamped to [0.0, 1.0].
    pub volume: f64,

    pub layer_states: Vec<LayerState>,
}

impl SectionState {
    /// Create a new section state. Volume is clamped to [0.0, 1.0].
    pub fn new(enabled: bool, volume: f64) -> Self {
        Self {
            enabled,
            volume: volume.clamp(0.0, 1.0),
            layer_states: Vec::new(),
        }
    }

    /// Append a layer state.
    pub fn add_layer(&mut self, layer: LayerState) {
        self.layer_states.push(layer);
    }
}

impl Default for SectionState {
    fn default() -> Self {
        Self {
            enabled: true,
            volume: 1.0,
            layer_states: Vec::new(),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// LayerState
// ─────────────────────────────────────────────────────────────────────────────

/// Per-layer override: enabled flag, volume, and optional variation selection.
#[derive(Debug, Clone, Facet)]
pub struct LayerState {
    pub enabled: bool,
    /// Volume level, clamped to [0.0, 1.0].
    pub volume: f64,

    pub variation_id: Option<VariationId>,
}

impl LayerState {
    /// Create a new layer state. Volume is clamped to [0.0, 1.0].
    pub fn new(enabled: bool, volume: f64) -> Self {
        Self {
            enabled,
            volume: volume.clamp(0.0, 1.0),
            variation_id: None,
        }
    }

    /// Builder: set a variation selection.
    #[must_use]
    pub fn with_variation(mut self, variation_id: VariationId) -> Self {
        self.variation_id = Some(variation_id);
        self
    }
}

impl Default for LayerState {
    fn default() -> Self {
        Self {
            enabled: true,
            volume: 1.0,
            variation_id: None,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::category::{BaseTone, Genre, PresetCategory};

    // ── SceneTemplate creation ──────────────────────────────────────────

    #[test]
    fn scene_template_direct_creation() {
        let preset_id = PresetId::new();
        let template = SceneTemplate::direct("My Direct Scene", preset_id);

        assert_eq!(template.name, "My Direct Scene");
        assert!(template.preset_reference.is_direct());
        assert_eq!(template.preset_reference.preset_id(), Some(preset_id));
        assert!(template.default_snapshot_id.is_none());
        assert!(template.parameter_state.is_empty());
    }

    #[test]
    fn scene_template_category_creation() {
        let category = PresetCategory::Genre {
            base_tone: BaseTone::Lead,
            genre: Genre::Blues,
        };
        let template = SceneTemplate::category("Blues Lead Scene", category.clone());

        assert_eq!(template.name, "Blues Lead Scene");
        assert!(template.preset_reference.is_category());
        assert_eq!(template.preset_reference.get_category(), Some(&category));
    }

    #[test]
    fn scene_template_with_snapshot() {
        let preset_id = PresetId::new();
        let snapshot_id = SnapshotId::new();
        let template = SceneTemplate::direct("Test", preset_id).with_snapshot(snapshot_id);

        assert_eq!(template.default_snapshot_id, Some(snapshot_id));
    }

    // ── Parameter save/load ─────────────────────────────────────────────

    #[test]
    fn parameter_save_and_load() {
        let preset_id = PresetId::new();
        let mut template = SceneTemplate::direct("Test", preset_id);

        let block_id = BlockId::new();
        let pv = ParameterValue::new(0, 0.75);
        template.save_parameter(block_id, pv);

        let params = template.get_block_parameters(block_id);
        assert!(params.is_some());
        let params = params.unwrap();
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].index, 0);
        assert!((params[0].value.get() - 0.75).abs() < f64::EPSILON);
    }

    #[test]
    fn parameter_load_missing_block() {
        let template = SceneTemplate::direct("Test", PresetId::new());
        assert!(template.get_block_parameters(BlockId::new()).is_none());
    }

    #[test]
    fn clear_parameters() {
        let mut template = SceneTemplate::direct("Test", PresetId::new());
        template.save_parameter(BlockId::new(), ParameterValue::new(0, 0.5));
        assert!(!template.parameter_state.is_empty());

        template.clear_parameters();
        assert!(template.parameter_state.is_empty());
    }

    // ── Profile scene template management ───────────────────────────────

    #[test]
    fn profile_add_and_find_templates() {
        let mut profile = Profile::new("Test Profile", RigId::new());
        let t1 = SceneTemplate::direct("Scene A", PresetId::new());
        let t2 = SceneTemplate::direct("Scene B", PresetId::new());
        let t1_id = t1.id;
        let t2_id = t2.id;

        profile.add_scene_template(t1);
        profile.add_scene_template(t2);

        assert!(profile.get_scene_template(t1_id).is_some());
        assert_eq!(profile.get_scene_template(t1_id).unwrap().name, "Scene A");
        assert!(profile.get_scene_template(t2_id).is_some());
        assert_eq!(profile.get_scene_template(t2_id).unwrap().name, "Scene B");
    }

    #[test]
    fn profile_find_by_name() {
        let mut profile = Profile::new("Test Profile", RigId::new());
        profile.add_scene_template(SceneTemplate::direct("Lead", PresetId::new()));
        profile.add_scene_template(SceneTemplate::direct("Rhythm", PresetId::new()));

        assert!(profile.get_scene_template_by_name("Lead").is_some());
        assert!(profile.get_scene_template_by_name("Rhythm").is_some());
        assert!(profile.get_scene_template_by_name("Bass").is_none());
    }

    #[test]
    fn profile_mutable_template_access() {
        let mut profile = Profile::new("Test Profile", RigId::new());
        let template = SceneTemplate::direct("Mutable", PresetId::new());
        let template_id = template.id;
        profile.add_scene_template(template);

        let t = profile.get_scene_template_mut(template_id).unwrap();
        t.name = "Renamed".to_string();

        assert_eq!(
            profile.get_scene_template(template_id).unwrap().name,
            "Renamed"
        );
    }

    // ── Default scene template auto-assignment ──────────────────────────

    #[test]
    fn first_template_becomes_default() {
        let mut profile = Profile::new("Test", RigId::new());
        assert!(profile.default_scene_template().is_none());

        let t1 = SceneTemplate::direct("First", PresetId::new());
        let t1_id = t1.id;
        profile.add_scene_template(t1);

        assert_eq!(profile.default_scene_template_id, Some(t1_id));
        assert!(profile.default_scene_template().is_some());
        assert_eq!(profile.default_scene_template().unwrap().name, "First");
    }

    #[test]
    fn second_template_does_not_replace_default() {
        let mut profile = Profile::new("Test", RigId::new());
        let t1 = SceneTemplate::direct("First", PresetId::new());
        let t1_id = t1.id;
        profile.add_scene_template(t1);
        profile.add_scene_template(SceneTemplate::direct("Second", PresetId::new()));

        assert_eq!(profile.default_scene_template_id, Some(t1_id));
    }

    // ── PresetReference methods ─────────────────────────────────────────

    #[test]
    fn preset_reference_direct() {
        let pid = PresetId::new();
        let r = PresetReference::direct(pid);
        assert!(r.is_direct());
        assert!(!r.is_category());
        assert_eq!(r.preset_id(), Some(pid));
        assert!(r.get_category().is_none());
    }

    #[test]
    fn preset_reference_category() {
        let cat = PresetCategory::Generic {
            base_tone: BaseTone::Clean,
        };
        let r = PresetReference::category(cat.clone());
        assert!(r.is_category());
        assert!(!r.is_direct());
        assert!(r.preset_id().is_none());
        assert_eq!(r.get_category(), Some(&cat));
    }

    // ── SceneParameterState ─────────────────────────────────────────────

    #[test]
    fn scene_parameter_state_empty() {
        let state = SceneParameterState::new();
        assert!(state.is_empty());
        assert!(state.last_modified.is_none());
    }

    #[test]
    fn scene_parameter_state_not_empty_with_block_params() {
        let mut state = SceneParameterState::new();
        state
            .block_parameters
            .insert(BlockId::new(), vec![ParameterValue::new(0, 0.5)]);
        assert!(!state.is_empty());
    }

    #[test]
    fn scene_parameter_state_not_empty_with_section_states() {
        let mut state = SceneParameterState::new();
        state
            .section_states
            .insert(SectionId::new(), SectionState::default());
        assert!(!state.is_empty());
    }

    #[test]
    fn scene_parameter_state_not_empty_with_bypass() {
        let mut state = SceneParameterState::new();
        state.global_block_bypass.insert(GlobalBlockId::new(), true);
        assert!(!state.is_empty());
    }

    #[test]
    fn modified_since_no_timestamp() {
        let state = SceneParameterState::new();
        assert!(!state.modified_since(0));
    }

    #[test]
    fn modified_since_with_timestamp() {
        let mut state = SceneParameterState::new();
        state.last_modified = Some(100);
        assert!(state.modified_since(50));
        assert!(!state.modified_since(100));
        assert!(!state.modified_since(200));
    }

    #[test]
    fn scene_parameter_state_default_is_empty() {
        let state = SceneParameterState::default();
        assert!(state.is_empty());
    }

    // ── SectionState ────────────────────────────────────────────────────

    #[test]
    fn section_state_new_clamps_volume() {
        let s = SectionState::new(true, 1.5);
        assert_eq!(s.volume, 1.0);

        let s = SectionState::new(true, -0.5);
        assert_eq!(s.volume, 0.0);

        let s = SectionState::new(false, 0.7);
        assert!(!s.enabled);
        assert!((s.volume - 0.7).abs() < f64::EPSILON);
    }

    #[test]
    fn section_state_default() {
        let s = SectionState::default();
        assert!(s.enabled);
        assert_eq!(s.volume, 1.0);
        assert!(s.layer_states.is_empty());
    }

    #[test]
    fn section_state_add_layer() {
        let mut s = SectionState::new(true, 0.8);
        s.add_layer(LayerState::new(true, 0.5));
        s.add_layer(LayerState::new(false, 1.0));
        assert_eq!(s.layer_states.len(), 2);
    }

    // ── LayerState ──────────────────────────────────────────────────────

    #[test]
    fn layer_state_new_clamps_volume() {
        let l = LayerState::new(true, 2.0);
        assert_eq!(l.volume, 1.0);

        let l = LayerState::new(true, -1.0);
        assert_eq!(l.volume, 0.0);

        let l = LayerState::new(true, 0.42);
        assert!((l.volume - 0.42).abs() < f64::EPSILON);
    }

    #[test]
    fn layer_state_default() {
        let l = LayerState::default();
        assert!(l.enabled);
        assert_eq!(l.volume, 1.0);
        assert!(l.variation_id.is_none());
    }

    #[test]
    fn layer_state_with_variation() {
        let vid = VariationId::new();
        let l = LayerState::new(true, 0.8).with_variation(vid);
        assert_eq!(l.variation_id, Some(vid));
    }

}
