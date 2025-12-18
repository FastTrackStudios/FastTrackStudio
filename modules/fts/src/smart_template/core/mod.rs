//! Core module
//!
//! Contains core traits, data models, and engines.

pub mod traits;
pub mod models;
pub mod engines;
pub mod errors;

pub use traits::*;
pub use errors::*;
pub use models::template::Template;
pub use models::group_config::{GroupConfig, InsertMode, PatternCategory};
pub use models::template_config::{TemplateConfig, InheritanceMode};
pub use models::visibility_config::{VisibilityGroupConfig, ViewMode};
pub use models::group_mode::GroupMode;
pub use models::builder::TemplateBuilder;

pub use engines::template_engine::*;
pub use engines::group_engine::*;
pub use engines::visibility_engine::*;
