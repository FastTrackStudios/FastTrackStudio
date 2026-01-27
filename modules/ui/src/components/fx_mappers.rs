//! FX Widget Parameter Mapping.
//!
//! This module provides bidirectional mapping between `BlockDefinition`
//! parameters (normalized 0.0-1.0 values keyed by string ID) and the
//! concrete `*Params` structs used by audio-controls FX visualization
//! widgets (e.g., `CompressorParams`, `GateParams`, `ReverbParams`).
//!
//! The `FxMapper` trait defines the interface, and each effect type has
//! a concrete implementation that knows the parameter names and ranges.

use audio_controls::prelude::{BlockDefinition, BlockInstance, BlockRegistry};
use audio_controls::widgets::{
    compressor_graph::{CompressorMode, CompressorParams, DetectionMode, StereoLink},
    gate_graph::{GateMode, GateParams},
};

// ============================================================================
// FxMapper Trait
// ============================================================================

/// Trait for bidirectional mapping between `BlockInstance` parameter values
/// and a concrete FX widget params struct.
///
/// Each effect type implements this trait to convert between the normalized
/// parameter storage format (String ID → f32 0.0-1.0) and the real-value
/// params structs used by FX visualization widgets.
pub trait FxMapper {
    /// The concrete params type for this effect's FX widget.
    type Params;

    /// Convert a `BlockInstance`'s parameter values into the widget params struct.
    ///
    /// Missing parameters use defaults from the `BlockDefinition`.
    fn to_params(instance: &BlockInstance, definition: &BlockDefinition) -> Self::Params;

    /// The block type_id this mapper handles (e.g., "compressor", "gate").
    fn type_id() -> &'static str;
}

// ============================================================================
// Helper: denormalize a parameter from a BlockInstance
// ============================================================================

/// Get a parameter value from a BlockInstance, returning a default if not found.
fn get_param(instance: &BlockInstance, id: &str, default: f32) -> f32 {
    instance.get_param(id).max(0.0).min(1.0).max(default * 0.0 + instance.get_param(id))
}

/// Denormalize a 0-1 value to a real range.
fn denorm(value: f32, min: f32, max: f32) -> f32 {
    min + value * (max - min)
}

// ============================================================================
// CompressorMapper
// ============================================================================

/// Maps between `BlockDefinition("compressor")` and `CompressorParams`.
pub struct CompressorMapper;

impl FxMapper for CompressorMapper {
    type Params = CompressorParams;

    fn type_id() -> &'static str {
        "compressor"
    }

    fn to_params(instance: &BlockInstance, _definition: &BlockDefinition) -> CompressorParams {
        CompressorParams {
            mode: CompressorMode::Compress,
            threshold: denorm(instance.get_param("threshold"), -60.0, 0.0),
            ratio: denorm(instance.get_param("ratio"), 1.0, 20.0),
            knee: denorm(instance.get_param("knee"), 0.0, 24.0),
            attack: denorm(instance.get_param("attack"), 0.01, 100.0),
            release: denorm(instance.get_param("release"), 10.0, 1000.0),
            makeup: denorm(instance.get_param("makeup"), 0.0, 24.0),
            mix: denorm(instance.get_param("mix"), 0.0, 1.0),
            detection: DetectionMode::Rms,
            stereo_link: StereoLink::Linked,
            range: 0.0,
            hold: 0.0,
            lookahead: 0.0,
            curve: 0.0,
            bypass: instance.bypassed,
        }
    }
}

// ============================================================================
// GateMapper
// ============================================================================

/// Maps between `BlockDefinition("gate")` and `GateParams`.
pub struct GateMapper;

impl FxMapper for GateMapper {
    type Params = GateParams;

    fn type_id() -> &'static str {
        "gate"
    }

    fn to_params(instance: &BlockInstance, _definition: &BlockDefinition) -> GateParams {
        GateParams {
            mode: GateMode::Gate,
            threshold: denorm(instance.get_param("threshold"), -60.0, 0.0),
            ratio: denorm(instance.get_param("ratio"), 1.0, 20.0),
            range: denorm(instance.get_param("range"), 0.0, 80.0),
            knee: denorm(instance.get_param("knee"), 0.0, 24.0),
            attack: denorm(instance.get_param("attack"), 0.01, 50.0),
            hold: 0.0,
            release: denorm(instance.get_param("release"), 10.0, 500.0),
            lookahead: 0.0,
            bypass: instance.bypassed,
        }
    }
}

// ============================================================================
// Dispatch: resolve mapper by type_id
// ============================================================================

/// Resolve a `CompressorParams` from a block instance if it's a compressor.
pub fn resolve_compressor_params(
    instance: &BlockInstance,
    definition: &BlockDefinition,
) -> CompressorParams {
    CompressorMapper::to_params(instance, definition)
}

/// Resolve a `GateParams` from a block instance if it's a gate.
pub fn resolve_gate_params(instance: &BlockInstance, definition: &BlockDefinition) -> GateParams {
    GateMapper::to_params(instance, definition)
}

/// Resolve the FX widget type for a given block type_id.
///
/// Returns the widget kind so callers can decide which FX visualization to render.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FxWidgetKind {
    Compressor,
    Gate,
    Eq,
    Drive,
    Amp,
    Cabinet,
    Chorus,
    Flanger,
    Delay,
    Reverb,
    Saturator,
    DeEsser,
    Tuner,
    /// No specialized widget; use generic BlockView.
    Generic,
}

impl FxWidgetKind {
    /// Determine the widget kind from a block type_id.
    pub fn from_type_id(type_id: &str) -> Self {
        match type_id {
            "compressor" => Self::Compressor,
            "gate" => Self::Gate,
            "eq" => Self::Eq,
            "drive" => Self::Drive,
            "amp" => Self::Amp,
            "cabinet" => Self::Cabinet,
            "chorus" => Self::Chorus,
            "flanger" => Self::Flanger,
            "delay" => Self::Delay,
            "reverb" => Self::Reverb,
            "saturator" => Self::Saturator,
            "deesser" => Self::DeEsser,
            "tuner" => Self::Tuner,
            _ => Self::Generic,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use audio_controls::prelude::{block_definitions, BlockInstance};

    #[test]
    fn compressor_mapper_produces_valid_params() {
        let def = block_definitions::compressor();
        let instance = BlockInstance::from_definition(&def, "test-comp");

        let params = CompressorMapper::to_params(&instance, &def);

        // Default threshold at 0.3 normalized → denorm(-60, 0) = -60 + 0.3 * 60 = -42
        assert!((params.threshold - (-42.0)).abs() < 0.1);
        assert!(params.ratio >= 1.0);
        assert!(!params.bypass);
    }

    #[test]
    fn gate_mapper_produces_valid_params() {
        let def = block_definitions::gate();
        let instance = BlockInstance::from_definition(&def, "test-gate");

        let params = GateMapper::to_params(&instance, &def);

        assert!(params.threshold <= 0.0);
        assert!(params.threshold >= -60.0);
        assert!(params.ratio >= 1.0);
        assert!(!params.bypass);
    }

    #[test]
    fn fx_widget_kind_dispatch() {
        assert_eq!(FxWidgetKind::from_type_id("compressor"), FxWidgetKind::Compressor);
        assert_eq!(FxWidgetKind::from_type_id("gate"), FxWidgetKind::Gate);
        assert_eq!(FxWidgetKind::from_type_id("delay"), FxWidgetKind::Delay);
        assert_eq!(FxWidgetKind::from_type_id("reverb"), FxWidgetKind::Reverb);
        assert_eq!(FxWidgetKind::from_type_id("unknown"), FxWidgetKind::Generic);
    }

    #[test]
    fn all_registry_types_have_widget_kind() {
        let registry = BlockRegistry::global();
        for def in registry.all() {
            let kind = FxWidgetKind::from_type_id(&def.type_id);
            // All registered types should have a known widget kind (not Generic)
            assert_ne!(
                kind,
                FxWidgetKind::Generic,
                "Block type '{}' has no FxWidgetKind mapping",
                def.type_id
            );
        }
    }
}
