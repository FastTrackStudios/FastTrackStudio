//! Hooks for reading nih-plug parameter state.

use crate::core::modulation::ModulationRange;
use crate::core::normal::Normal;
use nih_plug::prelude::ParamPtr;

/// Read the current normalized value of a parameter.
///
/// Returns the modulated value (after host automation/modulation).
#[must_use]
pub fn use_param_value(param: ParamPtr) -> f32 {
    unsafe { param.modulated_normalized_value() }
}

/// Read the modulation range of a parameter.
///
/// Returns a `ModulationRange` that can be used to visualize
/// the extent of modulation on a control.
#[must_use]
pub fn use_param_modulation(param: ParamPtr) -> ModulationRange {
    let unmodulated = unsafe { param.unmodulated_normalized_value() };
    let modulated = unsafe { param.modulated_normalized_value() };

    ModulationRange::from_offset(Normal::new(unmodulated), modulated - unmodulated)
}
