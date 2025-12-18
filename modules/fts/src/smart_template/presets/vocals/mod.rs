//! Vocals Preset
//!
//! Consolidates all vocal-related logic: naming conventions, templates, and track lists.

pub mod naming;
pub mod bgvs;

use daw::tracks::Track;
use crate::smart_template::core::models::template::Template as TemplateStruct;
use crate::smart_template::core::traits::{Group, TemplateSource};

pub use naming::*;
pub use bgvs::*;
pub use super::vocals::Vocals;

/// Returns the default vocals track list as a Vec<Track>
pub fn default_tracklist() -> Vec<Track> {
    Vocals::new().default_tracklist()
}

/// Returns the default vocals track list as a Template
pub fn default_tracks() -> Vec<TemplateStruct> {
    vec![Vocals::new().template()]
}
