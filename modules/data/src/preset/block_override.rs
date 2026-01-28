//! Block and parameter overrides for snapshots.
//!
//! A [`BlockOverride`] captures per-snapshot changes to a block: toggling
//! bypass and tweaking individual parameters without altering the preset's
//! base block settings.

use facet::Facet;

use crate::id::BlockId;
use crate::normalized::NormalizedF64;

// ─────────────────────────────────────────────────────────────────────────────
// ParameterOverride
// ─────────────────────────────────────────────────────────────────────────────

/// A single parameter override within a block.
///
/// Identified by a string `param_id` (matching [`BlockParameter::id`](super::BlockParameter))
/// rather than a numeric index, for resilience against parameter reordering.
#[derive(Debug, Clone, Facet)]
pub struct ParameterOverride {
    /// Identifier of the parameter to override.
    pub param_id: String,
    /// New normalized value in [0.0, 1.0].
    pub value: NormalizedF64,
}

impl ParameterOverride {
    /// Create a new parameter override.
    pub fn new(param_id: impl Into<String>, value: NormalizedF64) -> Self {
        Self {
            param_id: param_id.into(),
            value,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// BlockOverride
// ─────────────────────────────────────────────────────────────────────────────

/// Per-snapshot overrides for a single block.
///
/// Allows a snapshot to toggle bypass and adjust specific parameters
/// without modifying the preset's base block configuration.
#[derive(Debug, Clone, Facet)]
pub struct BlockOverride {
    /// The block being overridden.
    pub block_id: BlockId,
    /// Optional bypass state override (`None` = use base preset's state).
    pub bypassed: Option<bool>,
    /// Parameter value overrides (sparse — only changed parameters).
    pub parameters: Vec<ParameterOverride>,
}

impl BlockOverride {
    /// Create a new block override with no changes.
    pub fn new(block_id: BlockId) -> Self {
        Self {
            block_id,
            bypassed: None,
            parameters: Vec::new(),
        }
    }

    /// Set the bypass override.
    #[must_use]
    pub fn with_bypass(mut self, bypassed: bool) -> Self {
        self.bypassed = Some(bypassed);
        self
    }

    /// Add a parameter override.
    #[must_use]
    pub fn with_param(mut self, param_id: impl Into<String>, value: NormalizedF64) -> Self {
        self.parameters.push(ParameterOverride::new(param_id, value));
        self
    }

    /// Check if this override has any actual changes.
    pub fn has_changes(&self) -> bool {
        self.bypassed.is_some() || !self.parameters.is_empty()
    }

    /// Find a parameter override by ID.
    pub fn parameter(&self, param_id: &str) -> Option<&ParameterOverride> {
        self.parameters.iter().find(|p| p.param_id == param_id)
    }

    /// Get the override value for a parameter, if set.
    pub fn parameter_value(&self, param_id: &str) -> Option<NormalizedF64> {
        self.parameter(param_id).map(|p| p.value)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn block_override_new_has_no_changes() {
        let block_id = BlockId::new();
        let override_ = BlockOverride::new(block_id);
        assert!(!override_.has_changes());
        assert!(override_.bypassed.is_none());
        assert!(override_.parameters.is_empty());
    }

    #[test]
    fn block_override_with_bypass() {
        let block_id = BlockId::new();
        let override_ = BlockOverride::new(block_id).with_bypass(true);
        assert!(override_.has_changes());
        assert_eq!(override_.bypassed, Some(true));
    }

    #[test]
    fn block_override_with_bypass_false() {
        let block_id = BlockId::new();
        let override_ = BlockOverride::new(block_id).with_bypass(false);
        assert!(override_.has_changes());
        assert_eq!(override_.bypassed, Some(false));
    }

    #[test]
    fn block_override_with_param() {
        let block_id = BlockId::new();
        let override_ = BlockOverride::new(block_id)
            .with_param("drive", NormalizedF64::new(0.8));

        assert!(override_.has_changes());
        assert_eq!(override_.parameters.len(), 1);
        assert_eq!(override_.parameters[0].param_id, "drive");
        assert!((override_.parameters[0].value.get() - 0.8).abs() < f64::EPSILON);
    }

    #[test]
    fn block_override_with_multiple_params() {
        let block_id = BlockId::new();
        let override_ = BlockOverride::new(block_id)
            .with_param("drive", NormalizedF64::new(0.8))
            .with_param("tone", NormalizedF64::new(0.6))
            .with_param("level", NormalizedF64::new(0.5));

        assert_eq!(override_.parameters.len(), 3);
    }

    #[test]
    fn block_override_combined() {
        let block_id = BlockId::new();
        let override_ = BlockOverride::new(block_id)
            .with_bypass(true)
            .with_param("gain", NormalizedF64::new(0.9));

        assert!(override_.has_changes());
        assert_eq!(override_.bypassed, Some(true));
        assert_eq!(override_.parameters.len(), 1);
    }

    #[test]
    fn block_override_find_parameter() {
        let block_id = BlockId::new();
        let override_ = BlockOverride::new(block_id)
            .with_param("drive", NormalizedF64::new(0.7))
            .with_param("tone", NormalizedF64::new(0.3));

        assert!(override_.parameter("drive").is_some());
        assert_eq!(override_.parameter("drive").unwrap().param_id, "drive");
        assert!(override_.parameter("nonexistent").is_none());
    }

    #[test]
    fn block_override_parameter_value() {
        let block_id = BlockId::new();
        let override_ = BlockOverride::new(block_id)
            .with_param("drive", NormalizedF64::new(0.7));

        let value = override_.parameter_value("drive");
        assert!(value.is_some());
        assert!((value.unwrap().get() - 0.7).abs() < f64::EPSILON);

        assert!(override_.parameter_value("missing").is_none());
    }

    #[test]
    fn parameter_override_new() {
        let p = ParameterOverride::new("freq", NormalizedF64::new(0.5));
        assert_eq!(p.param_id, "freq");
        assert!((p.value.get() - 0.5).abs() < f64::EPSILON);
    }

    #[test]
    fn parameter_override_clamps_value() {
        let p = ParameterOverride::new("gain", NormalizedF64::new(1.5));
        assert_eq!(p.value.get(), 1.0);

        let p = ParameterOverride::new("gain", NormalizedF64::new(-0.5));
        assert_eq!(p.value.get(), 0.0);
    }

}
