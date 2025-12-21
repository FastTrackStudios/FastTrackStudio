//! Monarchy: A metadata-based hierarchical sorter
//!
//! Type-safe, config-compatible organization system.

pub mod config;
pub mod error;
pub mod field;
pub mod field_value;
pub mod group;
pub mod metadata;
pub mod organizer;
pub mod parser;
pub mod structure;
pub mod test_utils;

// Core exports
pub use config::{Config, ConfigBuilder, FallbackStrategy, ParserRules};
pub use error::{MonarchyError, Result};
pub use field::{IntoField, MetadataField};
pub use field_value::FieldValueDescriptor;
pub use group::{Group, GroupBuilder};
pub use metadata::Metadata;
pub use organizer::Organizer;
pub use parser::Parser;
pub use structure::Structure;
pub use test_utils::StructureAssertions;

// Re-export the derive macros
// Note: Metadata derive macro shares the same name as the Metadata trait - this is the standard Rust pattern
// (like serde::Serialize trait and serde::Serialize derive macro)
pub use monarchy_derive::{Metadata, MetadataBuilder};

// Re-export serde traits for derive usage
pub use serde::{Deserialize, Serialize};

use serde::{ser::SerializeStruct, Serializer};

/// Represents a parsed item with metadata
#[derive(Clone, Debug)]
pub struct Item<M: Metadata> {
    /// Unique identifier for this item
    pub id: String,

    /// Original input string
    pub original: String,

    /// Parsed metadata
    pub metadata: M,

    /// Full hierarchy of groups that matched, from top-level to most specific
    /// Stored as actual Group instances so we can access fields like prefix, transparent, etc.
    /// Example: [Drums Group, Drum_Kit Group, Kick Group]
    /// The last element is the most specific group that matched
    /// 
    /// Note: For serialization, this is serialized as group names (Vec<String>)
    pub matched_groups: Vec<Group<M>>,
}

/// Trait for converting various input types into sortable strings
pub trait IntoInputs {
    fn into_inputs(self) -> Vec<String>;
}

// Blanket implementation for any iterator that yields items convertible to String
impl<I, T> IntoInputs for I
where
    I: IntoIterator<Item = T>,
    T: Into<String>,
{
    fn into_inputs(self) -> Vec<String> {
        self.into_iter().map(|item| item.into()).collect()
    }
}

/// Main sorting function - now accepts anything that can be converted to inputs
pub fn monarchy_sort<M, I>(inputs: I, config: Config<M>) -> Result<Structure<M>>
where
    M: Metadata,
    I: IntoInputs,
{
    let input_strings = inputs.into_inputs();
    let parser = Parser::new(config.clone());
    let mut items = Vec::new();

    for input in input_strings {
        items.push(parser.parse(input)?);
    }

    let organizer = Organizer::new(config);
    Ok(organizer.organize(items))
}

/// Trait for containers that can hold sorted items
pub trait Target<M: Metadata> {
    fn existing_items(&self) -> Vec<Item<M>>;
}

/// Format metadata as a display name using group prefixes and field ordering from config
/// 
/// Format: "{prefixes} {group_name} {field_values}"
/// 
/// Example: "D Kick In" where:
/// - "D" is the prefix from "Drums" group
/// - "Kick" is the most specific group name
/// - "In" is the multi_mic value
/// 
/// Field values are included in the order they appear in the group's metadata_fields configuration.
pub fn to_display_name<M: Metadata>(item: &Item<M>, _config: &Config<M>) -> String {
    let mut parts = Vec::new();
    
    // Use matched_groups directly - now they're actual Group instances!
    let group_trail = &item.matched_groups;
    
    if group_trail.is_empty() {
        return item.original.clone();
    }
    
    // Collect prefixes from groups in trail (all except the last)
    let mut accumulated_prefixes: Vec<String> = Vec::new();
    
    for (i, group) in group_trail.iter().enumerate() {
        // For the last group, we use its name, not its prefix
        if i < group_trail.len() - 1 {
            // Apply inheritance rules
            if let Some(ref prefix) = group.prefix {
                // This group has its own prefix
                if group.inherit_prefix {
                    // Filter out blocked prefixes (only when adding our own prefix)
                    accumulated_prefixes.retain(|p: &String| !group.blocked_prefixes.contains(p));
                } else {
                    // Don't inherit, clear accumulated
                    accumulated_prefixes.clear();
                }
                accumulated_prefixes.push(prefix.clone());
            } else if !group.inherit_prefix {
                // No prefix but don't inherit, clear accumulated
                accumulated_prefixes.clear();
            }
            // If group has no prefix and inherit_prefix is true, we just continue
            // (blocked_prefixes only applies when the group adds its own prefix)
        }
    }
    
    // Add accumulated prefixes to display name
    if !accumulated_prefixes.is_empty() {
        parts.push(accumulated_prefixes.join(" "));
    }
    
    // Add the most specific group name (last in trail)
    if let Some(last_group) = group_trail.last() {
        parts.push(last_group.name.clone());
    }
    
    // Get field ordering from the most specific group
    if let Some(last_group) = group_trail.last() {
        // Add field values in the order they appear in metadata_fields
        for field in &last_group.metadata_fields {
            if let Some(value) = item.metadata.get(field) {
                let value_str = format_metadata_value::<M>(&value);
                if !value_str.is_empty() {
                    parts.push(value_str);
                }
            }
        }
    }
    
    parts.join(" ")
}

impl<M: Metadata> Serialize for Item<M>
where
    M: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Item", 4)?;
        state.serialize_field("id", &self.id)?;
        state.serialize_field("original", &self.original)?;
        state.serialize_field("metadata", &self.metadata)?;
        let group_names: Vec<String> = self.matched_groups.iter().map(|g| g.name.clone()).collect();
        state.serialize_field("matched_groups", &group_names)?;
        state.end()
    }
}

impl<'de, M: Metadata> Deserialize<'de> for Item<M>
where
    M: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{self, MapAccess, Visitor};
        use std::fmt;

        struct ItemVisitor<M: Metadata>(std::marker::PhantomData<M>);

        impl<'de, M: Metadata> Visitor<'de> for ItemVisitor<M>
        where
            M: Deserialize<'de>,
        {
            type Value = Item<M>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Item")
            }

            fn visit_map<V>(self, mut map: V) -> std::result::Result<Item<M>, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut id = None;
                let mut original = None;
                let mut metadata = None;
                let mut matched_group_names: Option<Vec<String>> = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        "id" => {
                            if id.is_some() {
                                return Err(de::Error::duplicate_field("id"));
                            }
                            id = Some(map.next_value()?);
                        }
                        "original" => {
                            if original.is_some() {
                                return Err(de::Error::duplicate_field("original"));
                            }
                            original = Some(map.next_value()?);
                        }
                        "metadata" => {
                            if metadata.is_some() {
                                return Err(de::Error::duplicate_field("metadata"));
                            }
                            metadata = Some(map.next_value()?);
                        }
                        "matched_groups" => {
                            if matched_group_names.is_some() {
                                return Err(de::Error::duplicate_field("matched_groups"));
                            }
                            matched_group_names = Some(map.next_value()?);
                        }
                        _ => {
                            let _ = map.next_value::<de::IgnoredAny>()?;
                        }
                    }
                }

                let id = id.ok_or_else(|| de::Error::missing_field("id"))?;
                let original = original.ok_or_else(|| de::Error::missing_field("original"))?;
                let metadata = metadata.ok_or_else(|| de::Error::missing_field("metadata"))?;
                let matched_group_names = matched_group_names.unwrap_or_default();

                // When deserializing, we can't reconstruct the full Group instances
                // So we create empty groups with just the names
                // This is a limitation - ideally we'd need the config to reconstruct properly
                let matched_groups = matched_group_names
                    .into_iter()
                    .map(|name| Group::builder(name).build())
                    .collect();

                Ok(Item {
                    id,
                    original,
                    metadata,
                    matched_groups,
                })
            }
        }

        deserializer.deserialize_struct("Item", &["id", "original", "metadata", "matched_groups"], ItemVisitor(std::marker::PhantomData))
    }
}

/// Helper to find a group in the config by name (recursively searches nested groups)
fn find_group_in_config<'a, M: Metadata>(config: &'a Config<M>, name: &str) -> Option<&'a Group<M>> {
    fn search_group<'a, M: Metadata>(group: &'a Group<M>, name: &str) -> Option<&'a Group<M>> {
        if group.name == name {
            return Some(group);
        }
        for nested in &group.groups {
            if let Some(found) = search_group(nested, name) {
                return Some(found);
            }
        }
        None
    }
    
    for group in &config.groups {
        if let Some(found) = search_group(group, name) {
            return Some(found);
        }
    }
    None
}

/// Format a metadata value as a string for display
fn format_metadata_value<M: Metadata>(value: &M::Value) -> String {
    let value_str = format!("{:?}", value);
    // Remove enum variant wrapper if present (e.g., "MultiMic([\"In\"])" -> "In")
    // This is a simple heuristic - we might need a better way
    if let Some(start) = value_str.find('(') {
        if let Some(end) = value_str.rfind(')') {
            let inner = &value_str[start + 1..end];
            // If it's a Vec format like ["In", "Out"], extract values
            if inner.starts_with('[') && inner.ends_with(']') {
                let content = &inner[1..inner.len() - 1];
                // Split by comma and clean up quotes
                let values: Vec<String> = content
                    .split(',')
                    .map(|s| s.trim().trim_matches('"').trim_matches('\'').to_string())
                    .filter(|s| !s.is_empty())
                    .collect();
                return values.join(" ");
            }
            // Otherwise, just return the inner content without quotes
            return inner.trim_matches('"').trim_matches('\'').to_string();
        }
    }
    value_str
}
