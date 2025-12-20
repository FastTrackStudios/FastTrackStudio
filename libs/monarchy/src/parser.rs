use crate::{Config, Item, Metadata, MonarchyError, Result};

/// Parser that converts input strings into Items with metadata
pub struct Parser<M: Metadata> {
    config: Config<M>,
}

impl<M: Metadata> Parser<M> {
    /// Create a new parser with the given configuration
    pub fn new(config: Config<M>) -> Self {
        Self { config }
    }

    /// Parse an input string into an Item with metadata
    pub fn parse(&self, input: String) -> Result<Item<M>> {
        let mut metadata = M::default();

        // Sort groups by priority (metadata_only groups have low priority)
        let mut sorted_groups = self.config.groups.clone();
        sorted_groups.sort_by(|a, b| b.priority.cmp(&a.priority));

        // First, extract metadata from metadata_only groups (they always match or have low priority)
        for group in &sorted_groups {
            if group.metadata_only && group.matches(&input) {
                // Extract metadata from metadata-only groups
                for field in &group.metadata_fields {
                    if let Some(value) = self.extract_field_value(&input, field, group) {
                        metadata.set(field.clone(), value);
                    }
                }
            }
        }

        // Find the deepest matching non-metadata-only group and its hierarchy
        // We need to recursively check nested groups to find the most specific match
        let mut matched_groups: Vec<crate::Group<M>> = Vec::new();
        let mut matched_group: Option<&crate::Group<M>> = None;
        
        for group in &sorted_groups {
            if !group.metadata_only {
                // Recursively find the deepest matching group
                if let Some((deepest_group, path)) = self.find_deepest_matching_group(group, &input, Vec::new()) {
                    matched_group = Some(deepest_group);
                    // Clone the groups to store them
                    matched_groups = path.into_iter().map(|g| g.clone()).collect();
                    break;
                }
            }
        }
        
        // If we found a match, extract metadata
        if let Some(group) = matched_group {
            // Set the group trail in metadata (as names for serialization)
            let group_names: Vec<String> = matched_groups.iter().map(|g| g.name.clone()).collect();
            self.set_group_field(&mut metadata, &group_names);

            // Extract metadata based on the group's fields
            for field in &group.metadata_fields {
                if let Some(value) = self.extract_field_value(&input, field, group) {
                    metadata.set(field.clone(), value);
                }
            }
        }

        // Set original_name in metadata
        self.set_original_name_field(&mut metadata, &input);

        // Handle unmatched items based on fallback strategy
        if matched_groups.is_empty() {
            if let crate::config::FallbackStrategy::Reject = self.config.fallback_strategy {
                return Err(MonarchyError::NoMatch(input));
            }
        }

        Ok(Item {
            id: generate_id(),
            original: input,
            metadata,
            matched_groups,
        })
    }

    /// Recursively find the deepest matching group within a group hierarchy
    /// Returns the deepest matching group and its full path from root (as Group references)
    /// If a nested group matches, the parent is included in the path even if it doesn't match itself
    fn find_deepest_matching_group<'a>(
        &self,
        group: &'a crate::Group<M>,
        input: &str,
        mut current_path: Vec<&'a crate::Group<M>>,
    ) -> Option<(&'a crate::Group<M>, Vec<&'a crate::Group<M>>)> {
        // Only check non-metadata-only groups
        if group.metadata_only {
            return None;
        }
        
        // Add this group to the path (we'll include it if we find a nested match)
        current_path.push(group);
        
        // Check if this group matches
        let this_matches = group.matches(input);
        
        // Recursively check nested groups to find deeper matches
        let mut deepest_match: Option<(&'a crate::Group<M>, Vec<&'a crate::Group<M>>)> = None;
        let mut deepest_depth = 0;
        
        for nested_group in &group.groups {
            // Skip nested groups that are metadata field groups (used only for metadata extraction)
            // A metadata field group is one whose name matches a field in the parent's metadata_fields
            let is_metadata_field_group = group.metadata_fields.iter().any(|field| {
                let field_name = format!("{:?}", field);
                let group_name_lower = nested_group.name.to_lowercase().replace("_", "").replace(" ", "");
                let field_name_lower = field_name.to_lowercase().replace("_", "").replace(" ", "");
                group_name_lower == field_name_lower || nested_group.name.eq_ignore_ascii_case(&field_name)
            });
            
            // Only include non-metadata-field groups in the structural trail
            if !is_metadata_field_group {
                if let Some((nested_match, nested_path)) = self.find_deepest_matching_group(nested_group, input, current_path.clone()) {
                    let depth = nested_path.len();
                    if depth > deepest_depth {
                        deepest_depth = depth;
                        deepest_match = Some((nested_match, nested_path));
                    }
                }
            }
        }
        
        // If we found a deeper match, return it (this includes the current group in the path)
        if let Some((deepest_group, deepest_path)) = deepest_match {
            return Some((deepest_group, deepest_path));
        }
        
        // Otherwise, if this group matches, return it with the full path
        if this_matches {
            Some((group, current_path))
        } else {
            // This group doesn't match and no nested groups matched, so remove it from path
            None
        }
    }

    /// Find the hierarchy path to a group by recursively searching the config
    fn find_group_hierarchy(&self, group_name: &str) -> Vec<String> {
        // Recursively search through groups and their nested groups
        fn search_group<M: Metadata>(
            group: &crate::Group<M>,
            target_name: &str,
            current_path: &mut Vec<String>,
        ) -> bool {
            current_path.push(group.name.clone());
            
            // Check if this is the target group
            if group.name == target_name {
                return true;
            }
            
            // Recursively search nested groups
            for nested_group in &group.groups {
                if search_group(nested_group, target_name, current_path) {
                    return true;
                }
            }
            
            // Not found in this branch, backtrack
            current_path.pop();
            false
        }
        
        let mut path = Vec::new();
        for group in &self.config.groups {
            if search_group(group, group_name, &mut path) {
                return path;
            }
        }
        
        // If not found in hierarchy, return just the group name
        vec![group_name.to_string()]
    }

    /// Set the group trail field in metadata
    fn set_group_field(&self, metadata: &mut M, group_trail: &[String]) {
        for field in M::fields() {
            let field_str = format!("{:?}", field);
            // Match "Group" or "group" (case-insensitive)
            if field_str.eq_ignore_ascii_case("Group") {
                // Try Vec<String> first (for group trail)
                if let Some(value) = M::create_vec_string_value(&field, group_trail.to_vec()) {
                    metadata.set(field, value);
                    return;
                }
                // Fallback to String (for backward compatibility, use last element)
                if let Some(last) = group_trail.last() {
                    if let Some(value) = M::create_string_value(&field, last.clone()) {
                        metadata.set(field, value);
                    }
                }
                break;
            }
        }
    }

    /// Set the original_name field in metadata
    fn set_original_name_field(&self, metadata: &mut M, original_name: &str) {
        for field in M::fields() {
            let field_str = format!("{:?}", field);
            // Match "OriginalName" or "original_name" (case-insensitive, handle snake_case)
            let normalized = field_str.to_lowercase().replace("_", "");
            if normalized == "originalname" || normalized == "original_name" {
                if let Some(value) = M::create_string_value(&field, original_name.to_string()) {
                    metadata.set(field, value);
                }
                break;
            }
        }
    }

    /// Extract a value for a specific field from the input
    fn extract_field_value(
        &self,
        input: &str,
        field: &M::Field,
        group: &crate::Group<M>,
    ) -> Option<M::Value> {
        // First check if this field is in the group's metadata_fields
        if !group.metadata_fields.contains(field) {
            return None;
        }

        // Convert field enum to string to find matching nested group
        let field_name = format!("{:?}", field);
        
        // Find nested group that matches this field
        // The nested group name should match the field name (e.g., "MultiMic" group for MultiMic field)
        let nested_group = group.groups.iter().find(|g| {
            let group_name_lower = g.name.to_lowercase().replace("_", "").replace(" ", "");
            let field_name_lower = field_name.to_lowercase().replace("_", "").replace(" ", "");
            group_name_lower == field_name_lower || g.name.eq_ignore_ascii_case(&field_name)
        })?;

        // Extract matching patterns from nested group
        let matches = self.extract_patterns_from_group(input, nested_group);
        
        if matches.is_empty() {
            return None;
        }

        // Convert matches to Value based on field type
        // Try Vec<String> first (for fields like multi_mic), then String
        if let Some(value) = M::create_vec_string_value(field, matches.clone()) {
            return Some(value);
        }
        
        // For String fields, take the first match
        if let Some(first_match) = matches.first() {
            if let Some(value) = M::create_string_value(field, first_match.clone()) {
                return Some(value);
            }
        }

        None
    }

    /// Extract matching patterns from a group
    fn extract_patterns_from_group(&self, input: &str, group: &crate::Group<M>) -> Vec<String> {
        let input_lower = input.to_lowercase();
        let mut matches = Vec::new();

        // Check each pattern in the group
        for pattern in &group.patterns {
            let pattern_lower = pattern.to_lowercase();
            
            // Check if pattern appears in input (word boundary aware would be better, but simple contains works for now)
            if input_lower.contains(&pattern_lower) {
                matches.push(pattern.clone());
            }
        }

        matches
    }
}

/// Generate a unique ID for an item
fn generate_id() -> String {
    // Simple counter-based ID for now
    use std::sync::atomic::{AtomicUsize, Ordering};
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    format!("item_{}", COUNTER.fetch_add(1, Ordering::SeqCst))
}
