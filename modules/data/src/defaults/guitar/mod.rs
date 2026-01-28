//! Guitar-specific default rig configurations.
//!
//! Submodules provide pre-built module presets, top-level presets,
//! profiles, and songs for the FTS guitar signal chain.
//!
//! Use [`build_guitar_rig`] to construct a fully-wired default guitar rig
//! with all module presets, presets, profiles, and songs.

use crate::block::GlobalBlock;
use crate::module_preset::ModulePreset;
use crate::normalized::Order;
use crate::performance::PerformanceSong;
use crate::preset::Preset;
use crate::profile::Profile;
use crate::rig::{InstrumentType, Rig};
use crate::tags::Tags;

pub mod modules;
pub mod presets;
pub mod profiles;
pub mod songs;

/// The complete default guitar rig with all its related data.
pub struct GuitarRigDefaults {
    pub rig: Rig,
    pub presets: Vec<Preset>,
    pub profiles: Vec<Profile>,
    pub songs: Vec<PerformanceSong>,
}

/// Build the complete default FTS guitar rig.
///
/// Returns a [`GuitarRigDefaults`] containing the rig (with module presets
/// and global blocks), top-level presets, profiles, and songs — all
/// cross-referenced by ID.
pub fn build_guitar_rig() -> GuitarRigDefaults {
    // 1. Build all module presets (shared IDs across the whole rig)
    let module_presets = presets::GuitarModulePresets::new();

    // 2. Build the rig shell with global blocks
    let mut rig = Rig::new("FTS Guitar", InstrumentType::Guitar);

    // Global blocks: EQ, Volume Pedal, Post EQ (fixed in the signal chain)
    rig.add_global_block(GlobalBlock {
        block: crate::defaults::blocks::eq::parametric_eq(),
        order: Order::new(1),
        tags: Tags::new(),
    });
    rig.add_global_block(GlobalBlock {
        block: crate::defaults::blocks::amp::volume_pedal(),
        order: Order::new(5),
        tags: Tags::new(),
    });
    rig.add_global_block(GlobalBlock {
        block: crate::defaults::blocks::eq::post_eq(),
        order: Order::new(7),
        tags: Tags::new(),
    });

    // Register all module presets in the rig
    for mp in module_presets.all() {
        rig.add_module_preset(mp.clone());
    }

    // 3. Build top-level presets
    let ac30_ambient_clean = presets::ac30_ambient_clean(&module_presets);
    let tremolo_swells = presets::tremolo_swells(&module_presets);
    let eighties_drive = presets::eighties_drive(&module_presets);
    let stank = presets::stank(&module_presets);
    let edge_of_breakup = presets::edge_of_breakup(&module_presets);

    // 4. Build profiles
    let rig_id = rig.id;

    let worship = profiles::worship(
        rig_id,
        &profiles::WorshipPresets {
            ac30_ambient_clean: &ac30_ambient_clean,
            tremolo_swells: &tremolo_swells,
            eighties_drive: &eighties_drive,
            stank: &stank,
            edge_of_breakup: &edge_of_breakup,
        },
        &module_presets,
    );

    let blues = profiles::blues(
        rig_id,
        &profiles::BluesPresets {
            ac30_ambient_clean: &ac30_ambient_clean,
            edge_of_breakup: &edge_of_breakup,
            eighties_drive: &eighties_drive,
            stank: &stank,
        },
        &module_presets,
    );

    let rock = profiles::rock(
        rig_id,
        &profiles::RockPresets {
            ac30_ambient_clean: &ac30_ambient_clean,
            eighties_drive: &eighties_drive,
            stank: &stank,
        },
        &module_presets,
    );

    // 5. Build songs
    let cryin = songs::cryin(
        &songs::CryinPresets {
            ac30_ambient_clean: &ac30_ambient_clean,
            edge_of_breakup: &edge_of_breakup,
            eighties_drive: &eighties_drive,
            stank: &stank,
        },
        &module_presets,
    );

    let thriller = songs::thriller(
        &songs::ThrillerPresets {
            ac30_ambient_clean: &ac30_ambient_clean,
            edge_of_breakup: &edge_of_breakup,
            stank: &stank,
        },
        &module_presets,
    );

    let girl_goodbye = songs::girl_goodbye(
        &songs::GirlGoodbyePresets {
            eighties_drive: &eighties_drive,
            edge_of_breakup: &edge_of_breakup,
            stank: &stank,
        },
        &module_presets,
    );

    GuitarRigDefaults {
        rig,
        presets: vec![
            ac30_ambient_clean,
            tremolo_swells,
            eighties_drive,
            stank,
            edge_of_breakup,
        ],
        profiles: vec![worship, blues, rock],
        songs: vec![cryin, thriller, girl_goodbye],
    }
}
