//! Bass Preset
//!
//! Consolidates all bass-related logic: naming conventions, templates, and track lists.

pub mod naming;

use daw::tracks::Track;
use crate::smart_template::core::models::template::Template as TemplateStruct;
use crate::smart_template::core::traits::{Group, TemplateSource};

pub use naming::*;
pub use super::bass::Bass;

/// Returns the default bass track list as a Vec<Track>
pub fn default_tracklist() -> Vec<Track> {
    Bass::new().default_tracklist()
}

/// Returns the default bass track list as a Template
pub fn default_tracks() -> Vec<TemplateStruct> {
    vec![Bass::new().template()]
}
