//! Patch — sound configuration for a layer.
//!
//! A [`Patch`] is a layer-level preset that defines the DSP chain and parameters
//! for a specific sound. Patches can have [`PatchVariation`]s that tweak
//! parameters without altering the plugin chain.
//!
//! The [`PatchCategory`] system enables hierarchical fallback: if a
//! "Metal Drive" patch is unavailable, the system can fall back to a generic
//! "Drive" category.
//!
//! # Examples
//!
//! ```
//! use rig_control::patch::{Patch, PatchCategory, PatchVariation};
//!
//! let mut patch = Patch::new("Grand Piano", PatchCategory::piano());
//! patch.add_variation(PatchVariation::soft());
//! patch.add_variation(PatchVariation::hard());
//!
//! // First variation is auto-set as default
//! assert!(patch.default_variation().is_some());
//! ```


use crate::block::Block;
use crate::id::{PatchId, VariationId};
use crate::parameter::ParameterValue;

// ---------------------------------------------------------------------------
// Patch
// ---------------------------------------------------------------------------

/// A sound configuration for a layer (layer-level preset).
///
/// Patches define the complete sound for a single layer, including the
/// plugin chain and all parameter values. They can have variations that
/// modify parameters without changing the plugin chain.
///
/// # Examples
///
/// - "Grand Piano" patch with "Soft", "Normal", "Hard" variations
/// - "Crunch Amp" patch with "Rhythm", "Lead Boost" variations
#[derive(Debug, Clone, ::facet::Facet)]
pub struct Patch {
    /// Unique identifier.
    pub id: PatchId,
    /// Patch name (e.g., "Grand Piano", "Crunch Amp").
    pub name: String,
    /// Category for organization and fallback.
    pub category: PatchCategory,
    /// The DSP chain for this patch.
    pub blocks: Vec<Block>,
    /// Base parameter values (before variation overrides).
    pub base_parameters: Vec<ParameterValue>,
    /// Available parameter variations.
    pub variations: Vec<PatchVariation>,
    /// Default variation to use.
    pub default_variation_id: Option<VariationId>,
}

impl Patch {
    /// Create a new patch with the given name and category.
    ///
    /// Starts with an empty DSP chain, no base parameters, no variations,
    /// and no default variation.
    pub fn new(name: impl Into<String>, category: PatchCategory) -> Self {
        Self {
            id: PatchId::new(),
            name: name.into(),
            category,
            blocks: Vec::new(),
            base_parameters: Vec::new(),
            variations: Vec::new(),
            default_variation_id: None,
        }
    }

    /// Add a block to the patch's DSP chain.
    pub fn add_block(&mut self, block: Block) {
        self.blocks.push(block);
    }

    /// Add a variation to the patch.
    ///
    /// If this is the first variation added, it is automatically set as the
    /// default variation.
    pub fn add_variation(&mut self, variation: PatchVariation) {
        if self.default_variation_id.is_none() {
            self.default_variation_id = Some(variation.id);
        }
        self.variations.push(variation);
    }

    /// Get a variation by its typed ID.
    pub fn get_variation(&self, id: VariationId) -> Option<&PatchVariation> {
        self.variations.iter().find(|v| v.id == id)
    }

    /// Get the default variation, if one has been set.
    pub fn default_variation(&self) -> Option<&PatchVariation> {
        self.default_variation_id
            .and_then(|id| self.get_variation(id))
    }

    /// Get the effective parameters for a given variation.
    ///
    /// Starts with a clone of [`base_parameters`](Self::base_parameters) and
    /// applies the variation's overrides on top. Overrides that match a base
    /// parameter index replace in-place; overrides with new indices are
    /// appended.
    ///
    /// If `variation_id` is `None` or the ID does not match any variation,
    /// the base parameters are returned unchanged.
    pub fn get_parameters_with_variation(
        &self,
        variation_id: Option<VariationId>,
    ) -> Vec<ParameterValue> {
        let mut params = self.base_parameters.clone();

        if let Some(variation) = variation_id.and_then(|id| self.get_variation(id)) {
            for override_param in &variation.parameter_overrides {
                if let Some(param) = params.iter_mut().find(|p| p.index == override_param.index) {
                    param.value = override_param.value;
                } else {
                    params.push(override_param.clone());
                }
            }
        }

        params
    }
}

// ---------------------------------------------------------------------------
// PatchCategory
// ---------------------------------------------------------------------------

/// Category for organizing patches with hierarchical fallback.
///
/// The category system enables fallback: if "Metal Drive" is not available,
/// fall back to generic "Drive".
///
/// # Display
///
/// - `PatchCategory::new("Drive")` displays as `"Drive"`
/// - `PatchCategory::with_style("Drive", "Metal")` displays as `"Metal Drive"`
#[derive(Debug, Clone, PartialEq, Eq, Hash, ::facet::Facet)]
pub struct PatchCategory {
    /// Base category (e.g., "Clean", "Crunch", "Drive", "Lead").
    pub base: String,
    /// Optional style modifier (e.g., "Blues", "Metal", "Jazz").
    pub style: Option<String>,
}

impl PatchCategory {
    /// Create a new category with just a base name.
    pub fn new(base: impl Into<String>) -> Self {
        Self {
            base: base.into(),
            style: None,
        }
    }

    /// Create a category with a base name and a style modifier.
    pub fn with_style(base: impl Into<String>, style: impl Into<String>) -> Self {
        Self {
            base: base.into(),
            style: Some(style.into()),
        }
    }

    /// Get the fallback category (removes style specificity).
    ///
    /// Returns `None` if there is no style to strip (already at base level).
    ///
    /// # Examples
    ///
    /// - `"Metal Drive"` -> `Some("Drive")`
    /// - `"Drive"` -> `None`
    pub fn fallback(&self) -> Option<PatchCategory> {
        self.style.as_ref().map(|_| PatchCategory {
            base: self.base.clone(),
            style: None,
        })
    }

    /// Get the full display name.
    ///
    /// If a style is present, returns `"{style} {base}"`.
    /// Otherwise returns just the base name.
    pub fn display_name(&self) -> String {
        match &self.style {
            Some(style) => format!("{style} {}", self.base),
            None => self.base.clone(),
        }
    }

    // -- Common guitar categories -----------------------------------------

    /// Create a "Clean" category.
    pub fn clean() -> Self {
        Self::new("Clean")
    }

    /// Create a "Crunch" category.
    pub fn crunch() -> Self {
        Self::new("Crunch")
    }

    /// Create a "Drive" category.
    pub fn drive() -> Self {
        Self::new("Drive")
    }

    /// Create a "Lead" category.
    pub fn lead() -> Self {
        Self::new("Lead")
    }

    // -- Common keyboard categories ---------------------------------------

    /// Create a "Piano" category.
    pub fn piano() -> Self {
        Self::new("Piano")
    }

    /// Create an "Organ" category.
    pub fn organ() -> Self {
        Self::new("Organ")
    }

    /// Create a "Synth" category.
    pub fn synth() -> Self {
        Self::new("Synth")
    }

    /// Create a "Pad" category.
    pub fn pad() -> Self {
        Self::new("Pad")
    }
}

// ---------------------------------------------------------------------------
// PatchVariation
// ---------------------------------------------------------------------------

/// Parameter variation of a patch (no plugin changes).
///
/// Variations allow quick parameter tweaks without changing the underlying
/// plugin chain. They store only the parameter overrides relative to the
/// patch's base parameters.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct PatchVariation {
    /// Unique identifier.
    pub id: VariationId,
    /// Variation name (e.g., "Soft", "Hard", "Bright").
    pub name: String,
    /// Parameter overrides from the base patch.
    pub parameter_overrides: Vec<ParameterValue>,
}

impl PatchVariation {
    /// Create a new variation with the given name and no overrides.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: VariationId::new(),
            name: name.into(),
            parameter_overrides: Vec::new(),
        }
    }

    /// Add a parameter override by index and normalized value.
    pub fn add_override(&mut self, index: u32, value: f64) {
        self.parameter_overrides
            .push(ParameterValue::new(index, value));
    }

    /// Create a "Soft" variation.
    pub fn soft() -> Self {
        Self::new("Soft")
    }

    /// Create a "Normal" variation.
    pub fn normal() -> Self {
        Self::new("Normal")
    }

    /// Create a "Hard" variation.
    pub fn hard() -> Self {
        Self::new("Hard")
    }

    /// Create a "Bright" variation.
    pub fn bright() -> Self {
        Self::new("Bright")
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::PluginId;

    // -- Patch creation ---------------------------------------------------

    #[test]
    fn patch_creation() {
        let patch = Patch::new("Grand Piano", PatchCategory::piano());

        assert_eq!(patch.name, "Grand Piano");
        assert_eq!(patch.category, PatchCategory::piano());
        assert!(patch.blocks.is_empty());
        assert!(patch.base_parameters.is_empty());
        assert!(patch.variations.is_empty());
        assert!(patch.default_variation_id.is_none());
    }

    // -- Add/get variations -----------------------------------------------

    #[test]
    fn add_and_get_variation() {
        let mut patch = Patch::new("Test", PatchCategory::clean());
        let var = PatchVariation::soft();
        let var_id = var.id;

        patch.add_variation(var);

        assert_eq!(patch.variations.len(), 1);
        let retrieved = patch.get_variation(var_id).unwrap();
        assert_eq!(retrieved.name, "Soft");
    }

    #[test]
    fn get_variation_missing_returns_none() {
        let patch = Patch::new("Test", PatchCategory::clean());
        let missing_id = VariationId::new();
        assert!(patch.get_variation(missing_id).is_none());
    }

    #[test]
    fn add_multiple_variations() {
        let mut patch = Patch::new("Test", PatchCategory::drive());
        patch.add_variation(PatchVariation::soft());
        patch.add_variation(PatchVariation::normal());
        patch.add_variation(PatchVariation::hard());
        patch.add_variation(PatchVariation::bright());

        assert_eq!(patch.variations.len(), 4);
    }

    // -- Default variation auto-set ---------------------------------------

    #[test]
    fn first_variation_becomes_default() {
        let mut patch = Patch::new("Test", PatchCategory::lead());
        let first = PatchVariation::soft();
        let first_id = first.id;

        patch.add_variation(first);
        patch.add_variation(PatchVariation::hard());

        assert_eq!(patch.default_variation_id, Some(first_id));
    }

    #[test]
    fn default_variation_accessor() {
        let mut patch = Patch::new("Test", PatchCategory::lead());
        assert!(patch.default_variation().is_none());

        let var = PatchVariation::normal();
        let var_id = var.id;
        patch.add_variation(var);

        let default = patch.default_variation().unwrap();
        assert_eq!(default.id, var_id);
        assert_eq!(default.name, "Normal");
    }

    #[test]
    fn second_variation_does_not_overwrite_default() {
        let mut patch = Patch::new("Test", PatchCategory::clean());
        let first = PatchVariation::soft();
        let second = PatchVariation::hard();
        let first_id = first.id;

        patch.add_variation(first);
        patch.add_variation(second);

        assert_eq!(patch.default_variation_id, Some(first_id));
    }

    // -- get_parameters_with_variation ------------------------------------

    #[test]
    fn parameters_without_variation_returns_base() {
        let mut patch = Patch::new("Test", PatchCategory::clean());
        patch.base_parameters.push(ParameterValue::new(0, 0.5));
        patch.base_parameters.push(ParameterValue::new(1, 0.75));

        let params = patch.get_parameters_with_variation(None);
        assert_eq!(params.len(), 2);
        assert!((params[0].value.get() - 0.5).abs() < f64::EPSILON);
        assert!((params[1].value.get() - 0.75).abs() < f64::EPSILON);
    }

    #[test]
    fn parameters_with_variation_overrides_existing() {
        let mut patch = Patch::new("Test", PatchCategory::drive());
        patch.base_parameters.push(ParameterValue::new(0, 0.5));
        patch.base_parameters.push(ParameterValue::new(1, 0.75));

        let mut var = PatchVariation::hard();
        var.add_override(0, 0.9); // Override index 0
        let var_id = var.id;
        patch.add_variation(var);

        let params = patch.get_parameters_with_variation(Some(var_id));
        assert_eq!(params.len(), 2);
        // Index 0 was overridden
        assert!((params[0].value.get() - 0.9).abs() < f64::EPSILON);
        // Index 1 unchanged
        assert!((params[1].value.get() - 0.75).abs() < f64::EPSILON);
    }

    #[test]
    fn parameters_with_variation_appends_new_index() {
        let mut patch = Patch::new("Test", PatchCategory::synth());
        patch.base_parameters.push(ParameterValue::new(0, 0.5));

        let mut var = PatchVariation::bright();
        var.add_override(5, 0.8); // New index not in base
        let var_id = var.id;
        patch.add_variation(var);

        let params = patch.get_parameters_with_variation(Some(var_id));
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].index, 0);
        assert!((params[0].value.get() - 0.5).abs() < f64::EPSILON);
        assert_eq!(params[1].index, 5);
        assert!((params[1].value.get() - 0.8).abs() < f64::EPSILON);
    }

    #[test]
    fn parameters_with_unknown_variation_returns_base() {
        let mut patch = Patch::new("Test", PatchCategory::pad());
        patch.base_parameters.push(ParameterValue::new(0, 0.3));

        let unknown_id = VariationId::new();
        let params = patch.get_parameters_with_variation(Some(unknown_id));
        assert_eq!(params.len(), 1);
        assert!((params[0].value.get() - 0.3).abs() < f64::EPSILON);
    }

    // -- Patch add_block --------------------------------------------------

    #[test]
    fn add_block() {
        let mut patch = Patch::new("Test", PatchCategory::drive());
        let block = Block::new("Amp", PluginId::vst3("com.example.amp", "Amp Sim"));

        patch.add_block(block);
        assert_eq!(patch.blocks.len(), 1);
        assert_eq!(patch.blocks[0].name, "Amp");
    }

    // -- PatchCategory fallback -------------------------------------------

    #[test]
    fn category_base_has_no_fallback() {
        let cat = PatchCategory::new("Drive");
        assert!(cat.fallback().is_none());
    }

    #[test]
    fn category_with_style_falls_back_to_base() {
        let cat = PatchCategory::with_style("Drive", "Metal");
        let fallback = cat.fallback().unwrap();

        assert_eq!(fallback.base, "Drive");
        assert!(fallback.style.is_none());
    }

    #[test]
    fn fallback_of_fallback_is_none() {
        let cat = PatchCategory::with_style("Clean", "Jazz");
        let fallback = cat.fallback().unwrap();
        assert!(fallback.fallback().is_none());
    }

    // -- PatchCategory display_name ---------------------------------------

    #[test]
    fn display_name_base_only() {
        let cat = PatchCategory::new("Piano");
        assert_eq!(cat.display_name(), "Piano");
    }

    #[test]
    fn display_name_with_style() {
        let cat = PatchCategory::with_style("Drive", "Metal");
        assert_eq!(cat.display_name(), "Metal Drive");
    }

    #[test]
    fn display_name_convenience_constructors() {
        assert_eq!(PatchCategory::clean().display_name(), "Clean");
        assert_eq!(PatchCategory::crunch().display_name(), "Crunch");
        assert_eq!(PatchCategory::drive().display_name(), "Drive");
        assert_eq!(PatchCategory::lead().display_name(), "Lead");
        assert_eq!(PatchCategory::piano().display_name(), "Piano");
        assert_eq!(PatchCategory::organ().display_name(), "Organ");
        assert_eq!(PatchCategory::synth().display_name(), "Synth");
        assert_eq!(PatchCategory::pad().display_name(), "Pad");
    }

    // -- PatchCategory equality -------------------------------------------

    #[test]
    fn category_equality() {
        assert_eq!(PatchCategory::clean(), PatchCategory::new("Clean"));
        assert_ne!(PatchCategory::clean(), PatchCategory::drive());
        assert_ne!(
            PatchCategory::new("Drive"),
            PatchCategory::with_style("Drive", "Metal")
        );
    }

    // -- PatchVariation convenience constructors --------------------------

    #[test]
    fn variation_convenience_constructors() {
        assert_eq!(PatchVariation::soft().name, "Soft");
        assert_eq!(PatchVariation::normal().name, "Normal");
        assert_eq!(PatchVariation::hard().name, "Hard");
        assert_eq!(PatchVariation::bright().name, "Bright");
    }

    #[test]
    fn variation_add_override() {
        let mut var = PatchVariation::new("Custom");
        var.add_override(3, 0.42);
        var.add_override(7, 0.99);

        assert_eq!(var.parameter_overrides.len(), 2);
        assert_eq!(var.parameter_overrides[0].index, 3);
        assert!((var.parameter_overrides[0].value.get() - 0.42).abs() < f64::EPSILON);
        assert_eq!(var.parameter_overrides[1].index, 7);
        assert!((var.parameter_overrides[1].value.get() - 0.99).abs() < f64::EPSILON);
    }

}
