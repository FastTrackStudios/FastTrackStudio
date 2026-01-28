//! Vocal-specific default rig configurations.
//!
//! Submodules provide pre-built module presets, top-level presets,
//! profiles, and songs for the FTS vocal signal chain.
//!
//! Use [`build_vocal_rig`] to construct a fully-wired default vocal rig
//! with all module presets, presets, profiles, and songs.

use crate::performance::PerformanceSong;
use crate::preset::Preset;
use crate::profile::Profile;
use crate::rig::{InstrumentType, Rig};

pub mod modules;
pub mod presets;
pub mod profiles;
pub mod songs;

/// The complete default vocal rig with all its related data.
pub struct VocalRigDefaults {
    pub rig: Rig,
    pub presets: Vec<Preset>,
    pub profiles: Vec<Profile>,
    pub songs: Vec<PerformanceSong>,
}

/// Build the complete default FTS vocal rig.
///
/// Returns a [`VocalRigDefaults`] containing the rig (with module presets),
/// top-level presets, profiles, and songs — all cross-referenced by ID.
pub fn build_vocal_rig() -> VocalRigDefaults {
    // 1. Build all module presets (shared IDs across the whole rig)
    let module_presets = presets::VocalModulePresets::new();

    // 2. Build the rig shell (no global blocks for vocals — sends are routed separately)
    let mut rig = Rig::new("FTS Vocal", InstrumentType::Vocals);

    // Register all module presets in the rig
    for mp in module_presets.all() {
        rig.add_module_preset(mp.clone());
    }

    // 3. Build top-level presets
    let natural_clean = presets::natural_clean(&module_presets);
    let warm_worship = presets::warm_worship(&module_presets);
    let bright_pop = presets::bright_pop(&module_presets);
    let powerful_rock = presets::powerful_rock(&module_presets);
    let breathy_ballad = presets::breathy_ballad(&module_presets);

    // 4. Build profiles
    let rig_id = rig.id;

    let worship = profiles::worship(
        rig_id,
        &profiles::WorshipVocalPresets {
            natural_clean: &natural_clean,
            warm_worship: &warm_worship,
            breathy_ballad: &breathy_ballad,
        },
        &module_presets,
    );

    let pop_rnb = profiles::pop_rnb(
        rig_id,
        &profiles::PopRnbVocalPresets {
            breathy_ballad: &breathy_ballad,
            natural_clean: &natural_clean,
            bright_pop: &bright_pop,
            warm_worship: &warm_worship,
        },
        &module_presets,
    );

    let rock = profiles::rock(
        rig_id,
        &profiles::RockVocalPresets {
            natural_clean: &natural_clean,
            warm_worship: &warm_worship,
            powerful_rock: &powerful_rock,
            breathy_ballad: &breathy_ballad,
        },
        &module_presets,
    );

    // 5. Build songs
    let oceans = songs::oceans(
        &songs::OceansPresets {
            natural_clean: &natural_clean,
            warm_worship: &warm_worship,
        },
        &module_presets,
    );

    let thriller = songs::thriller(
        &songs::ThrillerVocalPresets {
            breathy_ballad: &breathy_ballad,
            bright_pop: &bright_pop,
            natural_clean: &natural_clean,
        },
        &module_presets,
    );

    let bohemian = songs::bohemian_rhapsody(
        &songs::BohemianPresets {
            breathy_ballad: &breathy_ballad,
            bright_pop: &bright_pop,
            powerful_rock: &powerful_rock,
            natural_clean: &natural_clean,
        },
        &module_presets,
    );

    VocalRigDefaults {
        rig,
        presets: vec![
            natural_clean,
            warm_worship,
            bright_pop,
            powerful_rock,
            breathy_ballad,
        ],
        profiles: vec![worship, pop_rnb, rock],
        songs: vec![oceans, thriller, bohemian],
    }
}
