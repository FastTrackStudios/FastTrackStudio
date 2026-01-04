//! Display name generation utilities.
//!
//! This module provides functions for generating display names from items
//! and metadata. Display names combine:
//!
//! - Group prefixes (e.g., "D" for Drums, "GTR" for Guitars)
//! - The most specific group name
//! - Extracted metadata field values
//!
//! # Example
//!
//! For an item "Kick In.wav" matched to Drums > Kick with MultiMic: "In":
//!
//! ```text
//! Display name: "D Kick In"
//! - "D" = prefix from Drums group
//! - "Kick" = most specific group name
//! - "In" = MultiMic metadata value
//! ```
//!
//! # Usage
//!
//! ```ignore
//! use monarchy::display::to_display_name;
//!
//! let display = to_display_name(&item, &config);
//! println!("Display name: {}", display);
//! ```

use crate::{Config, Item, Metadata};

// region:    --- Display Name Generation

/// Format metadata as a display name using group prefixes and field ordering from config
///
/// Format: `"{prefixes} {group_name} {field_values}"`
///
/// # Example
/// `"D Kick In"` where:
/// - `"D"` is the prefix from "Drums" group
/// - `"Kick"` is the most specific group name
/// - `"In"` is the multi_mic value
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

/// Format a metadata value as a string for display
///
/// This handles enum variant wrappers and vector formats.
pub fn format_metadata_value<M: Metadata>(value: &M::Value) -> String {
    let value_str = format!("{:?}", value);

    // Remove enum variant wrapper if present (e.g., "MultiMic([\"In\"])" -> "In")
    // This is a simple heuristic - we might need a better way
    if let Some(start) = value_str.find('(')
        && let Some(end) = value_str.rfind(')')
    {
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
    value_str
}

// endregion: --- Display Name Generation

// region:    --- Support Functions

/// Helper to find a group in the config by name (recursively searches nested groups)
#[allow(dead_code)]
pub fn find_group_in_config<'a, M: Metadata>(
    config: &'a Config<M>,
    name: &str,
) -> Option<&'a crate::Group<M>> {
    fn search_group<'a, M: Metadata>(
        group: &'a crate::Group<M>,
        name: &str,
    ) -> Option<&'a crate::Group<M>> {
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

// endregion: --- Support Functions
