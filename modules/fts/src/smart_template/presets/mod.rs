//! Presets module
//!
//! Contains domain-specific implementations for drums, bass, vocals, etc.

pub mod drums;
pub mod bass;
pub mod guitar_electric;
pub mod guitar_acoustic;
pub mod keys;
pub mod synths;
pub mod vocals;

pub mod defaults;

pub use crate::smart_template::core::traits::Group;

use crate::smart_template::core::models::template::Template;

/// Merges all preset default tracks into a single list
pub fn get_all_default_tracks() -> Vec<Template> {
    let mut all_templates = Vec::new();
    
    all_templates.extend(drums::default_tracks());
    all_templates.extend(bass::default_tracks());
    all_templates.extend(guitar_electric::default_tracks());
    all_templates.extend(guitar_acoustic::default_tracks());
    all_templates.extend(keys::default_tracks());
    all_templates.extend(synths::default_tracks());
    all_templates.extend(vocals::default_tracks());
    
    all_templates
}
