//! Naming module
//!
//! Contains track name parsing and formatting functionality.

pub mod track_name;
pub mod formatter;
pub mod item_properties;
pub mod item_properties_parser;
pub mod item_properties_formatter;
pub mod track_ext;
pub mod components;
pub mod component_order;
pub mod default_groups;

pub use track_name::*;
pub use crate::smart_template::core::traits::Parser;
pub use formatter::*;
pub use item_properties::*;
pub use item_properties_parser::{ItemPropertiesParser, parse_fts_item_properties};
pub use item_properties_formatter::*;
pub use track_ext::*;
pub use components::*;
pub use component_order::*;
pub use default_groups::*;
