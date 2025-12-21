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
        // We need to check all groups and find the most specific (deepest) match
        // When depth is equal, prefer matches where more parent groups also match
        // (this helps groups with requires_parent_match win over regular groups)
        let mut matched_groups: Vec<crate::Group<M>> = Vec::new();
        let mut matched_group: Option<&crate::Group<M>> = None;
        let mut deepest_depth = 0;
        let mut best_match_count = 0; // Count of how many groups in the path actually match
        
        for group in &sorted_groups {
            if !group.metadata_only {
                // Recursively find the deepest matching group
                if let Some((deepest_group, path)) = self.find_deepest_matching_group(group, &input, Vec::new()) {
                    let depth = path.len();
                    // Count how many groups in the path actually match the input
                    // This gives preference to matches where more parent groups match
                    // This ensures "808 Kick" matches "Electronic Kit" -> "Kick" (2 matches)
                    // instead of "Drum Kit" -> "Kick" (1 match, since Drum Kit doesn't match "808")
                    let match_details: Vec<(String, bool)> = path.iter()
                        .map(|g| (g.name.clone(), g.matches(&input)))
                        .collect();
                    let match_count = match_details.iter().filter(|(_, matches)| *matches).count();
                    
                    // Check if any parent group in the path has only_match_nested_when_parent_matches set
                    // and doesn't match - if so, this path is invalid
                    let mut invalid_path = false;
                    for (i, group_in_path) in path.iter().enumerate() {
                        if group_in_path.only_match_nested_when_parent_matches {
                            // Check if this group matches
                            let group_matches = group_in_path.matches(&input);
                            if !group_matches {
                                // This group doesn't match, so nested groups shouldn't be searched
                                // But we're here, which means a nested group matched
                                // This is only valid if this is the last group in the path (the matched group itself)
                                if i < path.len() - 1 {
                                    // This is not the last group, so there are nested groups
                                    // Since this group doesn't match and has only_match_nested_when_parent_matches,
                                    // the nested groups shouldn't have been searched
                                    invalid_path = true;
                                    break;
                                }
                            }
                        }
                    }
                    
                    if invalid_path {
                        // Skip this path - it violates the only_match_nested_when_parent_matches rule
                        continue;
                    }
                    
                    // Keep the deepest match, or if depth is equal, prefer one with more matching groups
                    let is_better = depth > deepest_depth || 
                                   (depth == deepest_depth && match_count > best_match_count);
                    
                    if is_better {
                        deepest_depth = depth;
                        best_match_count = match_count;
                        matched_group = Some(deepest_group);
                        // Clone the groups to store them
                        matched_groups = path.into_iter().map(|g| g.clone()).collect();
                    }
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

            // Check if item matches any tagged collections in this group
            // An item can match multiple tagged collections, so we append to a vector
            if let Some(ref tagged_collection_group) = group.tagged_collection {
                if tagged_collection_group.matches(&input) {
                    self.add_tagged_collection_field(&mut metadata, &tagged_collection_group.name);
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
        
        // If this group requires parent match, check that the parent in the path matches
        if group.requires_parent_match && !current_path.is_empty() {
            // Get the parent (the group before this one in the path)
            let parent_index = current_path.len() - 2;
            if let Some(parent) = current_path.get(parent_index) {
                if !parent.matches(input) {
                    // Parent doesn't match, so this group can't match
                    return None;
                }
            }
        }
        
        // Recursively check nested groups to find deeper matches
        // Collect all matches at the deepest depth, then we'll pick the best one based on match count
        let mut deepest_matches: Vec<(&'a crate::Group<M>, Vec<&'a crate::Group<M>>)> = Vec::new();
        let mut deepest_depth = 0;
        
        // Check if we should only search nested groups when parent matches
        // This prevents items from matching nested groups when the parent doesn't match
        // (e.g., "FX Reverb" shouldn't match "Drums" -> "FX" -> "Reverb" if "Drums" doesn't match)
        // However, if the parent has no patterns (container group), we allow nested groups to be searched
        let should_only_match_nested_when_parent_matches = group.only_match_nested_when_parent_matches;
        let parent_has_patterns = !group.patterns.is_empty();
        
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
                // If this nested group requires parent match, check that parent matches first
                if nested_group.requires_parent_match && !this_matches {
                    // Skip this nested group if parent doesn't match
                    continue;
                }
                
                // If parent group has only_match_nested_when_parent_matches set, only search nested groups if parent matches
                // This prevents items from matching nested groups when the parent doesn't match
                // (e.g., "FX Reverb" shouldn't match "Drums" -> "FX" -> "Reverb" if "Drums" doesn't match)
                // For container groups with no patterns, we still enforce this rule
                if should_only_match_nested_when_parent_matches && !this_matches {
                    // Skip searching nested groups if parent doesn't match
                    continue;
                }
                
                if let Some((nested_match, nested_path)) = self.find_deepest_matching_group(nested_group, input, current_path.clone()) {
                    let depth = nested_path.len();
                    if depth > deepest_depth {
                        deepest_depth = depth;
                        deepest_matches.clear();
                        deepest_matches.push((nested_match, nested_path));
                    } else if depth == deepest_depth {
                        deepest_matches.push((nested_match, nested_path));
                    }
                }
            }
        }
        
        // If we found deeper matches, return the one with the most matching groups in the path
        if !deepest_matches.is_empty() {
            // Find the match with the most groups that actually match the input
            let best_match = deepest_matches.iter()
                .max_by_key(|(_, path)| {
                    path.iter().filter(|g| g.matches(input)).count()
                })
                .unwrap();
            // Clone the path since we can't return a reference to a local Vec
            return Some((best_match.0, best_match.1.clone()));
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

    /// Add a tagged collection to the tagged_collection field in metadata
    /// This appends to a vector since an item can match multiple tagged collections
    fn add_tagged_collection_field(&self, metadata: &mut M, collection_name: &str) {
        for field in M::fields() {
            let field_str = format!("{:?}", field);
            // Match "TaggedCollection" or "tagged_collection" (case-insensitive, handle snake_case)
            let normalized = field_str.to_lowercase().replace("_", "");
            if normalized == "taggedcollection" || normalized == "tagged_collection" {
                // Try to get existing value (should be Vec<String>)
                let existing = metadata.get(&field);
                let mut collections = if let Some(value) = existing {
                    // Try to extract Vec<String> from the value
                    // The value is an enum variant, so we need to format it and parse
                    let value_str = format!("{:?}", value);
                    // Extract from enum variant like "TaggedCollection([\"SUM\"])"
                    if let Some(start) = value_str.find('[') {
                        if let Some(end) = value_str.rfind(']') {
                            let inner = &value_str[start + 1..end];
                            // Parse the vector content
                            let mut vec: Vec<String> = inner
                                .split(',')
                                .map(|s| s.trim().trim_matches('"').trim_matches('\'').to_string())
                                .filter(|s| !s.is_empty())
                                .collect();
                            vec.push(collection_name.to_string());
                            vec
                        } else {
                            vec![collection_name.to_string()]
                        }
                    } else {
                        vec![collection_name.to_string()]
                    }
                } else {
                    vec![collection_name.to_string()]
                };
                
                // Remove duplicates and sort for consistency
                collections.sort();
                collections.dedup();
                
                // Set the updated vector
                if let Some(value) = M::create_vec_string_value(&field, collections) {
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

        // Convert field enum to string
        let field_name = format!("{:?}", field);
        
        // First, check if we have field_value_descriptors for this field
        // This takes precedence over nested groups
        if let Some(descriptors) = group.field_value_descriptors.get(&field_name) {
            // Use field value descriptors - each descriptor can match and return its value
            let mut matches = Vec::new();
            for descriptor in descriptors {
                if descriptor.matches(input) {
                    matches.push(descriptor.value.clone());
                }
            }
            
            if !matches.is_empty() {
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
            }
        }
        
        // Fall back to nested group approach (backward compatibility)
        // Find nested group that matches this field
        // The nested group name should match the field name (e.g., "MultiMic" group for MultiMic field)
        if let Some(nested_group) = group.groups.iter().find(|g| {
            let group_name_lower = g.name.to_lowercase().replace("_", "").replace(" ", "");
            let field_name_lower = field_name.to_lowercase().replace("_", "").replace(" ", "");
            group_name_lower == field_name_lower || g.name.eq_ignore_ascii_case(&field_name)
        }) {
            // Extract matching patterns from nested group
            let matches = self.extract_patterns_from_group(input, nested_group, field);
            
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
        }

        None
    }

    /// Extract matching patterns from a group
    /// If field_value_descriptors are available for this field, use them instead of simple patterns
    fn extract_patterns_from_group(&self, input: &str, group: &crate::Group<M>, field: &M::Field) -> Vec<String> {
        let field_name = format!("{:?}", field);
        
        // Check if we have field value descriptors for this field
        if let Some(descriptors) = group.field_value_descriptors.get(&field_name) {
            // Use field value descriptors - each descriptor can match and return its value
            let mut matches = Vec::new();
            for descriptor in descriptors {
                if descriptor.matches(input) {
                    matches.push(descriptor.value.clone());
                }
            }
            return matches;
        }
        
        // Fall back to simple pattern matching (backward compatibility)
        let input_lower = input.to_lowercase();
        let mut matches = Vec::new();

        // Check each pattern in the group
        for pattern in &group.patterns {
            let pattern_lower = pattern.to_lowercase();
            
            // Use word boundary matching (same as Group::matches)
            if crate::group::Group::<M>::contains_word(&input_lower, &pattern_lower) {
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
