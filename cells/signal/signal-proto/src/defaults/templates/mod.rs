//! Default rig templates — structural blueprints for instrument rigs.
//!
//! Templates define which modules and block slots exist without binding
//! to specific plugins. Use [`Templatable::from_template`] to instantiate
//! real instances from a template.
//!
//! ## Available Templates
//!
//! | Template | Modules | Blocks | Engines |
//! |------------------|---------|--------|---------|
//! | Guitar | 11 | 28 | 1 |
//! | Bass | 8 | 18 | 1 |
//! | Vocal | 5 | 13 | 1 |
//! | Synth | 5+5+5 | 21 | 3 (Keys, Organ, Synth) |
//! | Keys | 4 eng | 50 | 4 (Keys, Synth, Organ, Pad) |
//! | Drums | 6 | 12 | 1 |
//! | Drum Replacement | 7 | 14 | 1 |
//! | Synth Bass | 7 | 14 | 1 |
//!
//! ## Profiles
//!
//! Guitar profiles (Worship, Blues, Rock, Funk) define named patch
//! collections for common genre tones.
//!
//! ## Racks
//!
//! The vocal rack template bundles 3 vocal rigs (Lead, Harmony, Background)
//! with different processing levels.

pub mod bass;
pub mod drum_replacement;
pub mod drums;
pub mod guitar;
pub mod keys;
pub mod profiles;
pub mod synth;
pub mod synth_bass;
pub mod vocal;
pub mod vocal_rack;

// Re-export all template factory functions for convenience.
pub use bass::bass_rig_template;
pub use drum_replacement::drum_replacement_rig_template;
pub use drums::drums_rig_template;
pub use guitar::guitar_rig_template;
pub use keys::keys_rig_template;
pub use profiles::{
    all_guitar_profiles, guitar_blues_profile, guitar_funk_profile, guitar_rock_profile,
    guitar_worship_profile,
};
pub use synth::synth_rig_template;
pub use synth_bass::synth_bass_rig_template;
pub use vocal::vocal_rig_template;
pub use vocal_rack::{
    background_vocal_template, harmony_vocal_template, lead_vocal_template, vocal_rack_template,
};
