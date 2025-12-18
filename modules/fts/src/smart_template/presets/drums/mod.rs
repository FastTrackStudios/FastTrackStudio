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

/// Returns the default drum track list as a Vec<Track>
pub fn default_tracklist() -> Vec<Track> {
    let mut all_tracks = Vec::new();
    
    all_tracks.extend(Kick::new().default_tracklist());
    all_tracks.extend(Snare::new().default_tracklist());
    all_tracks.extend(Tom::new().default_tracklist());
    all_tracks.extend(Cymbals::new().default_tracklist());
    all_tracks.extend(Room::new().default_tracklist());
    
    all_tracks
}

/// Returns the default drum track list as a Template
pub fn default_tracks() -> Vec<Template> {
    vec![DrumKit::new().template()]
}
