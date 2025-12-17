//! Default group configurations
//!
//! These are hardcoded default groups that can be used as the foundation
//! for naming convention parsing.

pub mod drum_kit;
pub mod bass;
pub mod guitar_electric;
pub mod guitar_acoustic;
pub mod keys;
pub mod synths;
pub mod vocals;
#[cfg(feature = "default-groups")]
pub mod track_structures;

pub use drum_kit::{
    create_kick_group, create_kick_track_structure,
    create_tom_group, create_tom_track_structure,
    create_cymbals_group, create_cymbals_track_structure,
    create_drums_group,
    create_snare_group, create_snare_track_structure,
    create_rooms_group, create_rooms_track_structure,
};
pub use bass::{create_bass_group, create_bass_track_structure};
pub use guitar_electric::create_guitar_electric_group;
pub use guitar_acoustic::create_guitar_acoustic_group;
pub use keys::create_keys_group;
pub use synths::create_synths_group;
pub use vocals::{create_vocals_group, create_bgvs_group};

#[cfg(feature = "default-groups")]
pub use track_structures::create_drums_track_structure;

use super::Group;

/// Create all default groups
pub fn create_default_groups() -> Vec<Group> {
    vec![
        create_drums_group(),
        drum_kit::rooms::create_rooms_group(), // Rooms is a separate top-level group
        create_bass_group(),
        create_guitar_electric_group(),
        create_guitar_acoustic_group(),
        create_keys_group(),
        create_synths_group(),
        create_vocals_group(),
        create_bgvs_group(), // BGVs is a separate top-level group, not a child of Vocals
        // TODO: Add more groups if needed
    ]
}

