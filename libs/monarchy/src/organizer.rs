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
                collection_structure.items = matching_items;
                structure.children.push(collection_structure);

                // Add non-matching items to current level
                structure.items = non_matching_items;
            } else if !matching_items.is_empty() {
                // All items match - keep them at current level (no need for subfolder)
                structure.items = matching_items;
            } else {
                // No items match - keep non-matching items at current level
                structure.items = non_matching_items;
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
            // No tagged collection or variant field - add items directly to this level
            structure.items = group_items;
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
}
