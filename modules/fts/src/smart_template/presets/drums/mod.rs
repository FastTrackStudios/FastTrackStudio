//! Drums Preset
//!
//! Consolidates all drum-related logic: naming conventions, template generators, and track lists.

pub mod kick;
pub mod snare;
pub mod tom;
pub mod cymbals;
pub mod room;

pub mod drum_kit;

pub use kick::Kick;
pub use snare::Snare;
pub use tom::{Tom, TomMapper};
pub use cymbals::Cymbals;
pub use room::Room;

// Re-export kit-level logic
pub use drum_kit::DrumKit;
