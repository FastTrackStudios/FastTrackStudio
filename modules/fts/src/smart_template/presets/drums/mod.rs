//! Drums Preset
//!
//! Consolidates all drum-related logic: naming conventions, template generators, and track lists.

pub mod kick;
pub mod snare;
pub mod tom;
pub mod cymbals;
pub mod room;

pub mod drum_kit;

use daw::tracks::Track;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::traits::{Group, TemplateSource};

// Re-export instrument-specific structs
pub use kick::Kick;
pub use snare::Snare;
pub use tom::{Tom, TomMapper};
pub use cymbals::Cymbals;
pub use room::Room;

// Re-export kit-level logic
pub use drum_kit::DrumKit;
