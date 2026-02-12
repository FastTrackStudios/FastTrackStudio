//! Default rig templates — structural blueprints for instrument rigs.
//!
//! Templates define which modules and block slots exist without binding
//! to specific plugins. Use [`Templatable::from_template`] to instantiate
//! real instances from a template.
//!
//! ## Available Templates
//!
//! | Template | Modules | Blocks | Engines |
//! |----------|---------|--------|---------|
//! | Guitar | 11 | 28 | 1 |
//! | Vocal | 5 | 13 | 1 |
//! | Synth | 5+5+5 | 21 | 3 (Keys, Organ, Synth) |
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

pub mod guitar;
pub mod profiles;
pub mod synth;
pub mod vocal;
pub mod vocal_rack;

// Re-export all template factory functions for convenience.
pub use guitar::guitar_rig_template;
pub use profiles::{
    all_guitar_profiles, guitar_blues_profile, guitar_funk_profile, guitar_rock_profile,
    guitar_worship_profile,
};
pub use synth::synth_rig_template;
pub use vocal::vocal_rig_template;
pub use vocal_rack::{
    background_vocal_template, harmony_vocal_template, lead_vocal_template, vocal_rack_template,
};
