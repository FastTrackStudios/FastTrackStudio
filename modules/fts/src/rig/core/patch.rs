//! Patch - Sound configuration for a layer
//!
//! A Patch is a layer-level preset that defines the DSP chain and parameters
//! for a specific sound. Patches can have variations (parameter tweaks).

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use super::{Block, ParameterValue};

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
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct Patch {
    /// Unique identifier
    pub id: Uuid,
    /// Patch name (e.g., "Grand Piano", "Crunch Amp")
    pub name: String,
    /// Category for organization and fallback
    pub category: PatchCategory,
    /// The DSP chain for this patch
    pub blocks: Vec<Block>,
    /// Base parameter values (before variation overrides)
    pub base_parameters: Vec<ParameterValue>,
    /// Available parameter variations
    pub variations: Vec<PatchVariation>,
    /// Default variation to use
    pub default_variation_id: Option<Uuid>,
}

/// Category for organizing patches with hierarchical fallback.
///
/// The category system enables fallback: if "Metal Drive" isn't available,
/// fall back to generic "Drive".
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub struct PatchCategory {
    /// Base category (e.g., "Clean", "Crunch", "Drive", "Lead")
    pub base: String,
    /// Optional style modifier (e.g., "Blues", "Metal", "Jazz")
    pub style: Option<String>,
}

/// Parameter variation of a patch (no plugin changes).
///
/// Variations allow quick parameter tweaks without changing the underlying
/// plugin chain. When a variation is updated, all presets using it are
/// automatically updated.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct PatchVariation {
    /// Unique identifier
    pub id: Uuid,
    /// Variation name (e.g., "Soft", "Hard", "Bright")
    pub name: String,
    /// Parameter overrides from base patch
    pub parameter_overrides: Vec<ParameterValue>,
}

impl Patch {
    /// Create a new patch
    pub fn new(name: impl Into<String>, category: PatchCategory) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            category,
            blocks: Vec::new(),
            base_parameters: Vec::new(),
            variations: Vec::new(),
            default_variation_id: None,
        }
    }

    /// Add a block to the patch
    pub fn add_block(&mut self, block: Block) {
        self.blocks.push(block);
    }

    /// Add a variation to the patch
    pub fn add_variation(&mut self, variation: PatchVariation) {
        if self.default_variation_id.is_none() {
            self.default_variation_id = Some(variation.id);
        }
        self.variations.push(variation);
    }

    /// Get a variation by ID
    pub fn get_variation(&self, id: Uuid) -> Option<&PatchVariation> {
        self.variations.iter().find(|v| v.id == id)
    }

    /// Get the default variation
    pub fn default_variation(&self) -> Option<&PatchVariation> {
        self.default_variation_id.and_then(|id| self.get_variation(id))
    }

    /// Get the effective parameters for a given variation
    pub fn get_parameters_with_variation(&self, variation_id: Option<Uuid>) -> Vec<ParameterValue> {
        let mut params = self.base_parameters.clone();

        if let Some(variation) = variation_id.and_then(|id| self.get_variation(id)) {
            // Apply variation overrides
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

impl PatchCategory {
    /// Create a new category with just a base
    pub fn new(base: impl Into<String>) -> Self {
        Self {
            base: base.into(),
            style: None,
        }
    }

    /// Create a category with base and style
    pub fn with_style(base: impl Into<String>, style: impl Into<String>) -> Self {
        Self {
            base: base.into(),
            style: Some(style.into()),
        }
    }

    /// Get the fallback category (removes style specificity)
    ///
    /// Example: "Metal Drive" -> "Drive"
    pub fn fallback(&self) -> Option<PatchCategory> {
        self.style.as_ref().map(|_| PatchCategory {
            base: self.base.clone(),
            style: None,
        })
    }

    /// Get the full display name
    pub fn display_name(&self) -> String {
        match &self.style {
            Some(style) => format!("{} {}", style, self.base),
            None => self.base.clone(),
        }
    }

    // Common guitar categories
    pub fn clean() -> Self {
        Self::new("Clean")
    }
    pub fn crunch() -> Self {
        Self::new("Crunch")
    }
    pub fn drive() -> Self {
        Self::new("Drive")
    }
    pub fn lead() -> Self {
        Self::new("Lead")
    }

    // Common keyboard categories
    pub fn piano() -> Self {
        Self::new("Piano")
    }
    pub fn organ() -> Self {
        Self::new("Organ")
    }
    pub fn synth() -> Self {
        Self::new("Synth")
    }
    pub fn pad() -> Self {
        Self::new("Pad")
    }
}

impl PatchVariation {
    /// Create a new variation
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            parameter_overrides: Vec::new(),
        }
    }

    /// Add a parameter override
    pub fn add_override(&mut self, index: u32, value: f64) {
        self.parameter_overrides.push(ParameterValue::new(index, value));
    }

    /// Create a "Soft" variation
    pub fn soft() -> Self {
        Self::new("Soft")
    }

    /// Create a "Normal" variation
    pub fn normal() -> Self {
        Self::new("Normal")
    }

    /// Create a "Hard" variation
    pub fn hard() -> Self {
        Self::new("Hard")
    }

    /// Create a "Bright" variation
    pub fn bright() -> Self {
        Self::new("Bright")
    }
}
