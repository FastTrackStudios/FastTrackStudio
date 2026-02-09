//! Container mapping — bridges REAPER FX container names to signal ModuleType.
//!
//! In the FTS guitar rig setup, REAPER FX containers are named after signal
//! chain stages (INPUT, DRIVE, AMP, MODULATION, TIME, MOTION, etc.). This
//! module provides bidirectional mapping between those container names and
//! the signal-proto [`ModuleType`] enum.
//!
//! ## REAPER Track Structure
//!
//! A typical guitar rig track in REAPER looks like:
//!
//! ```text
//! <FXCHAIN
//!   <CONTAINER "INPUT">      → ModuleType::Source
//!   <CONTAINER "DRIVE">      → ModuleType::Drive
//!   <CONTAINER "PRE-FX">     → ModuleType::PreFx
//!   <CONTAINER "AMP">        → ModuleType::Amp
//!   <CONTAINER "EQ">         → ModuleType::Eq
//!   <CONTAINER "MODULATION"> → ModuleType::Modulation
//!   <CONTAINER "TIME">       → ModuleType::Time
//!   <CONTAINER "MOTION">     → ModuleType::Motion
//!   <CONTAINER "MASTER">     → ModuleType::Master
//! >
//! ```

use crate::module::ModuleType;

/// Known REAPER container names mapped to their ModuleType.
///
/// These match the standard FTS rig container naming convention.
const CONTAINER_MAP: &[(&str, ModuleType)] = &[
    ("INPUT", ModuleType::Source),
    ("SOURCE", ModuleType::Source),
    ("DRIVE", ModuleType::Drive),
    ("PRE-FX", ModuleType::PreFx),
    ("PREFX", ModuleType::PreFx),
    ("AMP", ModuleType::Amp),
    ("CABINET", ModuleType::Amp), // cabinet is part of the amp stage
    ("EQ", ModuleType::Eq),
    ("POST-EQ", ModuleType::PostEq),
    ("POSTEQ", ModuleType::PostEq),
    ("DYNAMICS", ModuleType::Dynamics),
    ("MODULATION", ModuleType::Modulation),
    ("MOD", ModuleType::Modulation),
    ("TIME", ModuleType::Time),
    ("DELAY", ModuleType::Time),
    ("REVERB", ModuleType::Time),
    ("MOTION", ModuleType::Motion),
    ("MASTER", ModuleType::Master),
    ("OUTPUT", ModuleType::Master),
    ("VOLUME", ModuleType::Volume),
    ("SPECIAL", ModuleType::Special),
    ("SENDS", ModuleType::Sends),
    // Vocal chain
    ("RESCUE", ModuleType::Rescue),
    ("CORRECTION", ModuleType::Correction),
    ("TONAL", ModuleType::Tonal),
    ("VOCAL MODULATION", ModuleType::VocalModulation),
];

/// Map a REAPER FX container name to a signal [`ModuleType`].
///
/// Performs case-insensitive matching against known container names.
/// Returns `None` for unrecognized names — the caller can then treat
/// the container as a custom/unknown module type.
///
/// # Examples
///
/// ```
/// use signal_proto::container::module_type_from_container_name;
/// use signal_proto::module::ModuleType;
///
/// assert_eq!(module_type_from_container_name("AMP"), Some(ModuleType::Amp));
/// assert_eq!(module_type_from_container_name("amp"), Some(ModuleType::Amp));
/// assert_eq!(module_type_from_container_name("MODULATION"), Some(ModuleType::Modulation));
/// assert_eq!(module_type_from_container_name("unknown"), None);
/// ```
pub fn module_type_from_container_name(name: &str) -> Option<ModuleType> {
    let upper = name.trim().to_uppercase();
    CONTAINER_MAP
        .iter()
        .find(|(key, _)| *key == upper)
        .map(|(_, mt)| *mt)
}

/// Map a signal [`ModuleType`] to the canonical REAPER container name.
///
/// Returns the standard FTS container name for a given module type.
pub fn container_name_from_module_type(module_type: ModuleType) -> &'static str {
    match module_type {
        ModuleType::Source => "INPUT",
        ModuleType::Drive => "DRIVE",
        ModuleType::PreFx => "PRE-FX",
        ModuleType::Amp => "AMP",
        ModuleType::Eq => "EQ",
        ModuleType::PostEq => "POST-EQ",
        ModuleType::Dynamics => "DYNAMICS",
        ModuleType::Modulation => "MODULATION",
        ModuleType::Time => "TIME",
        ModuleType::Motion => "MOTION",
        ModuleType::Master => "MASTER",
        ModuleType::Volume => "VOLUME",
        ModuleType::Special => "SPECIAL",
        ModuleType::Sends => "SENDS",
        ModuleType::Rescue => "RESCUE",
        ModuleType::Correction => "CORRECTION",
        ModuleType::Tonal => "TONAL",
        ModuleType::VocalModulation => "VOCAL MODULATION",
    }
}

/// Discovered rig layout entry — a container with its FX GUID and module type.
#[derive(Debug, Clone)]
pub struct ContainerEntry {
    /// The REAPER FX GUID of the container.
    pub fx_guid: String,
    /// The original container name from REAPER.
    pub container_name: String,
    /// The mapped signal module type, or `None` if unrecognized.
    pub module_type: Option<ModuleType>,
    /// Index in the FX chain.
    pub chain_index: u32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_container_names() {
        assert_eq!(
            module_type_from_container_name("INPUT"),
            Some(ModuleType::Source)
        );
        assert_eq!(
            module_type_from_container_name("DRIVE"),
            Some(ModuleType::Drive)
        );
        assert_eq!(
            module_type_from_container_name("AMP"),
            Some(ModuleType::Amp)
        );
        assert_eq!(
            module_type_from_container_name("MODULATION"),
            Some(ModuleType::Modulation)
        );
        assert_eq!(
            module_type_from_container_name("TIME"),
            Some(ModuleType::Time)
        );
        assert_eq!(
            module_type_from_container_name("MOTION"),
            Some(ModuleType::Motion)
        );
        assert_eq!(
            module_type_from_container_name("MASTER"),
            Some(ModuleType::Master)
        );
    }

    #[test]
    fn case_insensitive() {
        assert_eq!(
            module_type_from_container_name("amp"),
            Some(ModuleType::Amp)
        );
        assert_eq!(
            module_type_from_container_name("Modulation"),
            Some(ModuleType::Modulation)
        );
        assert_eq!(
            module_type_from_container_name("pre-fx"),
            Some(ModuleType::PreFx)
        );
    }

    #[test]
    fn unknown_returns_none() {
        assert_eq!(module_type_from_container_name("UNKNOWN"), None);
        assert_eq!(module_type_from_container_name(""), None);
        assert_eq!(module_type_from_container_name("MY_CUSTOM_FX"), None);
    }

    #[test]
    fn aliases() {
        assert_eq!(
            module_type_from_container_name("SOURCE"),
            Some(ModuleType::Source)
        );
        assert_eq!(
            module_type_from_container_name("PREFX"),
            Some(ModuleType::PreFx)
        );
        assert_eq!(
            module_type_from_container_name("MOD"),
            Some(ModuleType::Modulation)
        );
        assert_eq!(
            module_type_from_container_name("DELAY"),
            Some(ModuleType::Time)
        );
        assert_eq!(
            module_type_from_container_name("OUTPUT"),
            Some(ModuleType::Master)
        );
    }

    #[test]
    fn round_trip() {
        let types = [
            ModuleType::Source,
            ModuleType::Drive,
            ModuleType::PreFx,
            ModuleType::Amp,
            ModuleType::Eq,
            ModuleType::Modulation,
            ModuleType::Time,
            ModuleType::Motion,
            ModuleType::Master,
        ];
        for mt in types {
            let name = container_name_from_module_type(mt);
            let back = module_type_from_container_name(name);
            assert_eq!(back, Some(mt), "Round-trip failed for {name}");
        }
    }
}
