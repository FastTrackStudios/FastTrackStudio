//! Property definitions registry.
//!
//! Each domain entity carries a `properties: JsonValue` flat key→value map
//! matching Obsidian's YAML frontmatter shape. This module's
//! `PropertyDefinition` is the global registry of property names + their
//! canonical kinds (text/number/checkbox/date/datetime/list/tags).
//!
//! Obsidian-frontmatter compatibility constraint: a property name belongs
//! to exactly one canonical kind across the vault. The registry is the
//! source of truth that `PropertyService` consults when normalizing or
//! validating values written to per-entity `properties` blobs.

pub mod json_object;
pub mod model;

pub use json_object::JsonObject;
pub use model::*;
