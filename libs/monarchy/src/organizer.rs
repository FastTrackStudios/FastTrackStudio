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

        // Build structure from groups
        for group in &self.config.groups {
            let group_structure = self.build_group_structure(group, &items, Vec::new());
            if !group_structure.is_empty() {
                root.children.push(group_structure);
            }
        }

        // Handle unmatched items based on fallback strategy
        let unmatched: Vec<Item<M>> = items
            .into_iter()
            .filter(|item| item.matched_group.is_none())
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
            current_prefixes = current_prefixes
                .into_iter()
                .filter(|p| !group.blocked_prefixes.contains(p))
                .collect();
        }

        // Create the display name with prefixes
        let display_name = if current_prefixes.is_empty() {
            group.name.clone()
        } else {
            format!("{} {}", current_prefixes.join(" "), group.name)
        };

        let mut structure = Structure::with_display_name(&group.name, display_name);

        // Find items that matched this group
        let group_items: Vec<Item<M>> = all_items
            .iter()
            .filter(|item| item.matched_group.as_ref() == Some(&group.name))
            .cloned()
            .collect();

        // Handle variant fields if specified
        if let Some(ref variant_field) = group.variant_field {
            // Group items by their variant field value
            let mut variants: std::collections::HashMap<String, Vec<Item<M>>> =
                std::collections::HashMap::new();

            for item in group_items {
                // Get the variant field value for this item
                let variant_key = if let Some(value) = item.metadata.get(variant_field) {
                    // Convert the value to a string for grouping
                    format!("{:?}", value)
                } else {
                    "default".to_string()
                };

                variants
                    .entry(variant_key.clone())
                    .or_insert_with(Vec::new)
                    .push(item);
            }

            // Create sub-structures for each variant
            for (variant_name, variant_items) in variants {
                if !variant_items.is_empty() {
                    let mut variant_structure = Structure::new(variant_name);
                    variant_structure.items = variant_items;
                    structure.children.push(variant_structure);
                }
            }
        } else {
            // No variant field - add items directly to this level
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
