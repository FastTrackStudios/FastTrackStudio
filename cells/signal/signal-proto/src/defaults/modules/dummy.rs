//! Dummy module presets — test/placeholder presets for every [`ModuleType`].
//!
//! Each dummy module preset has a descriptive name, an empty block chain,
//! and 4 snapshots named "1" through "4". Used for testing the module preset
//! browser, snapshot switching, and override workflows.

use crate::module::ModuleType;

/// All module type variants, in declaration order.
pub const ALL_MODULE_TYPES: &[ModuleType] = &[
    // Vocal chain
    ModuleType::Rescue,
    ModuleType::Correction,
    ModuleType::Tonal,
    ModuleType::VocalModulation,
    ModuleType::Sends,
    // Instrument chain
    ModuleType::Source,
    ModuleType::Eq,
    ModuleType::Dynamics,
    ModuleType::Special,
    ModuleType::Drive,
    ModuleType::PreFx,
    ModuleType::Volume,
    ModuleType::Amp,
    ModuleType::PostEq,
    ModuleType::Modulation,
    ModuleType::Time,
    ModuleType::Motion,
    ModuleType::Master,
    ModuleType::Custom,
];

/// Snapshot names for dummy module presets (shared with block dummy presets).
pub const DUMMY_SNAPSHOT_NAMES: &[&str] = &["1", "2", "3", "4"];

/// Metadata for a dummy module preset (used by the seed layer).
pub struct DummyModulePreset {
    pub name: String,
    pub module_type: ModuleType,
    pub description: String,
}

/// Create a dummy module preset definition for the given module type.
pub fn dummy_module(module_type: ModuleType) -> DummyModulePreset {
    let display = module_type.display_name();
    DummyModulePreset {
        name: format!("Dummy {display}"),
        module_type,
        description: format!("Dummy {display} module for testing"),
    }
}

/// Create dummy module preset definitions for all module types.
pub fn all_dummy_module_presets() -> Vec<DummyModulePreset> {
    ALL_MODULE_TYPES.iter().map(|&mt| dummy_module(mt)).collect()
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_module_types_is_exhaustive() {
        // Current count: 19 variants.
        assert_eq!(ALL_MODULE_TYPES.len(), 19);
    }

    #[test]
    fn dummy_module_names_are_unique() {
        let presets = all_dummy_module_presets();
        let names: Vec<&str> = presets.iter().map(|p| p.name.as_str()).collect();
        let mut unique = names.clone();
        unique.sort();
        unique.dedup();
        assert_eq!(names.len(), unique.len(), "duplicate dummy module names found");
    }

    #[test]
    fn all_dummy_module_presets_returns_all_types() {
        let presets = all_dummy_module_presets();
        assert_eq!(presets.len(), ALL_MODULE_TYPES.len());
    }
}
