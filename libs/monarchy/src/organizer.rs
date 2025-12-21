use crate::{Config, Item, Metadata, Structure};

/// Organizes items into hierarchical structures based on groups
pub struct Organizer<M: Metadata> {
    config: Config<M>,
}

impl<M: Metadata> Organizer<M> {
    /// Create a new organizer with the given configuration
    pub fn new(config: Config<M>) -> Self {
        Self { config }
    }

    /// Organize items into a hierarchical structure
    pub fn organize(&self, items: Vec<Item<M>>) -> Structure<M> {
        let mut root = Structure::new("root");

        // Build structure from groups (skip metadata_only groups)
        for group in &self.config.groups {
            if !group.metadata_only {
                let group_structure = self.build_group_structure(group, &items, Vec::new());
                if !group_structure.is_empty() {
                    root.children.push(group_structure);
                }
            }
        }

        // Handle unmatched items based on fallback strategy
        let unmatched: Vec<Item<M>> = items
            .into_iter()
            .filter(|item| item.matched_groups.is_empty())
            .collect();

        if !unmatched.is_empty() {
            match self.config.fallback_strategy {
                crate::config::FallbackStrategy::CreateMisc => {
                    let mut misc = Structure::new("Misc");
                    misc.items = unmatched;
                    root.children.push(misc);
                }
                crate::config::FallbackStrategy::PlaceAtRoot => {
                    root.items = unmatched;
                }
                crate::config::FallbackStrategy::Reject => {
                    // Items were already rejected during parsing
                }
            }
        }

        // Collapse single-child nodes (only create folders when needed)
        root.collapse_single_children();

        root
    }

    /// Build a structure for a specific group with prefix inheritance
    fn build_group_structure(
        &self,
        group: &crate::Group<M>,
        all_items: &[Item<M>],
        parent_prefixes: Vec<String>,
    ) -> Structure<M> {
        // Calculate the accumulated prefixes for this group
        let mut current_prefixes = parent_prefixes.clone();

        // Add this group's prefix if it exists and should be inherited
        if let Some(ref prefix) = group.prefix {
            // Check if this prefix should be added based on inheritance rules
            if group.inherit_prefix {
                // Filter out blocked prefixes
                let filtered_prefixes: Vec<String> = current_prefixes
                    .into_iter()
                    .filter(|p| !group.blocked_prefixes.contains(p))
                    .collect();
                current_prefixes = filtered_prefixes;
            } else {
                // Don't inherit any prefixes if inherit_prefix is false
                current_prefixes.clear();
            }

            // Always add our own prefix
            current_prefixes.push(prefix.clone());
        } else if !group.inherit_prefix {
            // If we don't have a prefix but don't inherit, clear parent prefixes
            current_prefixes.clear();
        } else {
            // Filter out blocked prefixes even if we don't have our own prefix
            current_prefixes.retain(|p| !group.blocked_prefixes.contains(p));
        }

        // Create the display name with prefixes
        let display_name = if current_prefixes.is_empty() {
            group.name.clone()
        } else {
            format!("{} {}", current_prefixes.join(" "), group.name)
        };

        let mut structure = Structure::with_display_name(&group.name, display_name);

        // Find items that matched this group (check if last element in trail matches)
        let group_items: Vec<Item<M>> = all_items
            .iter()
            .filter(|item| item.matched_groups.last().map(|g| &g.name) == Some(&group.name))
            .cloned()
            .collect();
        

        // Handle tagged collections if specified (pattern-based grouping)
        if let Some(ref tagged_collection_group) = group.tagged_collection {
            // Check which items match the tagged collection patterns
            let mut matching_items = Vec::new();
            let mut non_matching_items = Vec::new();

            for item in group_items {
                if tagged_collection_group.matches(&item.original) {
                    matching_items.push(item);
                } else {
                    non_matching_items.push(item);
                }
            }

            // If we have both matching and non-matching items, create a subfolder for matches
            if !matching_items.is_empty() && !non_matching_items.is_empty() {
                // Create sub-structure for tagged collection
                let mut collection_structure = Structure::new(&tagged_collection_group.name);
                // Group matching items by metadata fields if needed
                let grouped_matching = self.group_items_by_metadata_fields(&matching_items, group);
                if grouped_matching.children.is_empty() && grouped_matching.items.is_empty() {
                    collection_structure.items = matching_items;
                } else {
                    collection_structure.children = grouped_matching.children;
                    collection_structure.items = grouped_matching.items;
                }
                structure.children.push(collection_structure);

                // Group non-matching items by metadata fields if needed
                let grouped_non_matching = self.group_items_by_metadata_fields(&non_matching_items, group);
                structure.children.extend(grouped_non_matching.children);
                structure.items = grouped_non_matching.items;
            } else if !matching_items.is_empty() {
                // All items match - group them by metadata fields if needed
                let grouped = self.group_items_by_metadata_fields(&matching_items, group);
                structure.children = grouped.children;
                structure.items = grouped.items;
            } else {
                // No items match - group non-matching items by metadata fields if needed
                let grouped = self.group_items_by_metadata_fields(&non_matching_items, group);
                structure.children = grouped.children;
                structure.items = grouped.items;
            }
        } else if let Some(ref variant_field) = group.variants {
            // Handle variants - keep items together but group them by variant value
            // Variants are kept in the same structure but sorted/grouped by variant value
            // so the client can handle them (e.g., different lanes on the same track)
            let mut variant_groups: std::collections::HashMap<String, Vec<Item<M>>> =
                std::collections::HashMap::new();

            for item in group_items {
                // Get the variant field value for this item
                let variant_key = if let Some(value) = item.metadata.get(variant_field) {
                    // Convert the value to a string for grouping
                    format!("{:?}", value)
                } else {
                    "default".to_string()
                };

                variant_groups
                    .entry(variant_key.clone())
                    .or_default()
                    .push(item);
            }

            // Sort items by variant value and add them to the structure
            // Items are kept together but grouped by variant for client-side handling
            let mut sorted_items = Vec::new();
            let mut variant_keys: Vec<String> = variant_groups.keys().cloned().collect();
            variant_keys.sort();
            
            for variant_key in variant_keys {
                if let Some(variant_items) = variant_groups.remove(&variant_key) {
                    sorted_items.extend(variant_items);
                }
            }
            
            structure.items = sorted_items;
        } else {
            // Check if we should group items by metadata field values
            // If multiple items have different values for metadata fields, create sub-structures
            if !group_items.is_empty() && !group.metadata_fields.is_empty() {
                // Group items by their metadata field values
                let mut field_groups: std::collections::HashMap<String, Vec<Item<M>>> =
                    std::collections::HashMap::new();
                let mut items_without_field_values = Vec::new();

                for item in group_items {
                    // Get the first metadata field value (we'll group by the first field that has a value)
                    let mut value_str_opt: Option<String> = None;
                    for field in &group.metadata_fields {
                        if let Some(value) = item.metadata.get(field) {
                            // Convert value to string for grouping
                            value_str_opt = Some(self.format_metadata_value_for_grouping(&value));
                            eprintln!("DEBUG: Item '{}' has field value: {:?} -> '{}'", 
                                item.original, field, value_str_opt.as_ref().unwrap());
                            break; // Use the first field that has a value
                        } else {
                            eprintln!("DEBUG: Item '{}' field {:?} has no value", item.original, field);
                        }
                    }

                    if let Some(value_str) = value_str_opt {
                        field_groups
                            .entry(value_str)
                            .or_default()
                            .push(item);
                    } else {
                        eprintln!("DEBUG: Item '{}' has no field values, adding to items_without_field_values", item.original);
                        items_without_field_values.push(item);
                    }
                }
                
                eprintln!("DEBUG: Created {} field_groups, {} items_without_field_values", 
                    field_groups.len(), items_without_field_values.len());

                // If we have multiple groups with different values, create sub-structures
                // Only create sub-structures if we have 2+ different field values
                if field_groups.len() > 1 {
                    // Create sub-structures for each unique field value
                    let mut field_keys: Vec<String> = field_groups.keys().cloned().collect();
                    field_keys.sort();

                    for field_key in field_keys {
                        if let Some(items) = field_groups.remove(&field_key) {
                            let mut sub_structure = Structure::new(&field_key);
                            sub_structure.items = items;
                            structure.children.push(sub_structure);
                        }
                    }

                    // Add items without field values to the current level
                    structure.items = items_without_field_values;
                } else if field_groups.len() == 1 {
                    // Only one unique value - keep items at current level (no need for sub-structure)
                    structure.items = field_groups.values().next().unwrap().clone();
                    structure.items.extend(items_without_field_values);
                } else {
                    // No field values found - keep items at current level
                    structure.items = items_without_field_values;
                }
            } else {
                // No metadata fields to group by - add items directly to this level
                structure.items = group_items;
            }
        }

        // Recursively build nested groups with accumulated prefixes
        for nested_group in &group.groups {
            let child =
                self.build_group_structure(nested_group, all_items, current_prefixes.clone());
            if !child.is_empty() {
                structure.children.push(child);
            }
        }

        structure
    }

    /// Group items by their metadata field values
    /// 
    /// If multiple items have different values for metadata fields, creates sub-structures.
    /// Returns a Structure with children (if grouping occurred) and items (if not grouped or items without values).
    fn group_items_by_metadata_fields(&self, items: &[Item<M>], group: &crate::Group<M>) -> Structure<M> {
        let mut result = Structure::new("temp");
        
        if items.is_empty() || group.metadata_fields.is_empty() {
            result.items = items.to_vec();
            return result;
        }

        // Group items by their metadata field values
        let mut field_groups: std::collections::HashMap<String, Vec<Item<M>>> =
            std::collections::HashMap::new();
        let mut items_without_field_values = Vec::new();

        for item in items {
            // Get the first metadata field value (we'll group by the first field that has a value)
            let mut value_str_opt: Option<String> = None;
            for field in &group.metadata_fields {
                if let Some(value) = item.metadata.get(field) {
                    // Convert value to string for grouping
                    value_str_opt = Some(self.format_metadata_value_for_grouping(&value));
                    break; // Use the first field that has a value
                }
            }

            if let Some(value_str) = value_str_opt {
                field_groups
                    .entry(value_str)
                    .or_default()
                    .push(item.clone());
            } else {
                items_without_field_values.push(item.clone());
            }
        }

        // If we have multiple groups with different values, create sub-structures
        // Only create sub-structures if we have 2+ different field values
        if field_groups.len() > 1 {
            // Create sub-structures for each unique field value
            let mut field_keys: Vec<String> = field_groups.keys().cloned().collect();
            field_keys.sort();

            for field_key in field_keys {
                if let Some(items) = field_groups.remove(&field_key) {
                    let mut sub_structure = Structure::new(&field_key);
                    sub_structure.items = items;
                    result.children.push(sub_structure);
                }
            }

            // Add items without field values to the current level
            result.items = items_without_field_values;
        } else if field_groups.len() == 1 {
            // Only one unique value - keep items at current level (no need for sub-structure)
            result.items = field_groups.values().next().unwrap().clone();
            result.items.extend(items_without_field_values);
        } else {
            // No field values found - keep items at current level
            result.items = items_without_field_values;
        }
        
        result
    }

    /// Format a metadata value for grouping purposes
    /// 
    /// This converts the metadata value to a string that can be used as a group key.
    /// Uses the format_metadata_value helper from lib.rs which handles Vec<String> properly.
    fn format_metadata_value_for_grouping(&self, value: &M::Value) -> String {
        crate::format_metadata_value::<M>(value)
    }
}
