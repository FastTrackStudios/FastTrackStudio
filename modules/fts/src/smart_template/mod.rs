//! Smart Template Module
//!
//! A trait-based, configurable system for track template management, naming conventions,
//! and track organization.

pub mod core;
pub mod features;
pub mod presets;
pub mod drivers;
pub mod utils;

// Re-export core types for convenience
pub use core::traits::*;
pub use core::models::template::Template;
pub use core::models::group_config::{GroupConfig, InsertMode, PatternCategory};
pub use core::models::template_config::TemplateConfig;
pub use core::models::group_mode::GroupMode;
pub use core::engines::template_engine::TemplateEngine;
pub use core::engines::group_engine::GroupEngine;
pub use core::engines::visibility_engine::VisibilityEngine;

// Re-export common feature types
pub use features::naming::formatter::Formatter;
pub use features::naming::item_properties::ItemProperties;
pub use features::naming::item_properties_parser::{ItemPropertiesParser, parse_fts_item_properties};
pub use features::naming::item_properties_formatter::ItemPropertiesFormatter;
pub use features::naming::track_name::TrackNameLike;
pub use features::naming::track_ext::TrackItemPropertiesExt;

pub use features::matching::matcher::{MatchResult, MatchType};
pub use features::visibility::traits::{VisibilityManager, VisibilityState};

// Re-export utils
pub use utils::json_config_loader::JsonConfigLoader;
