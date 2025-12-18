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
