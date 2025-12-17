//! Drum Kit group configurations
//!
//! Contains all drum-related groups: Kick, Snare, Tom, Cymbals, Rooms, and the parent Drums group.

pub mod kick;
pub mod snare;
pub mod tom;
pub mod cymbals;
pub mod rooms;
pub mod drums;

pub use kick::{create_kick_group, create_kick_track_structure};
pub use snare::{create_snare_group, create_snare_track_structure};
pub use tom::{create_tom_group, create_tom_track_structure};
pub use cymbals::{create_cymbals_group, create_cymbals_track_structure};
pub use rooms::{create_rooms_group, create_rooms_track_structure};
pub use drums::create_drums_group;

