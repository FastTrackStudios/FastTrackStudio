use crate::{Item, Metadata};
use serde::{Deserialize, Serialize};
use std::fmt;

/// The hierarchical structure that results from organizing items
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Structure<M: Metadata> {
    /// Name of this level in the hierarchy
    pub name: String,

    /// Display name with accumulated prefixes (e.g., "D Kick")
    pub display_name: String,

    /// Items at this level
    pub items: Vec<Item<M>>,

    /// Child structures
    pub children: Vec<Structure<M>>,
}

impl<M: Metadata> Structure<M> {
    /// Create a new structure with the given name
    pub fn new(name: impl Into<String>) -> Self {
        let name = name.into();
        Self {
            display_name: name.clone(),
            name,
            items: Vec::new(),
            children: Vec::new(),
        }
    }

    /// Create a new structure with a name and display name
    pub fn with_display_name(name: impl Into<String>, display_name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            display_name: display_name.into(),
            items: Vec::new(),
            children: Vec::new(),
        }
    }

    /// Check if this structure is empty (no items or children)
    pub fn is_empty(&self) -> bool {
        self.items.is_empty() && self.children.is_empty()
    }

    /// Get the total count of items (recursive)
    pub fn total_items(&self) -> usize {
        self.items.len() + self.children.iter().map(|c| c.total_items()).sum::<usize>()
    }

    /// Get the depth of the hierarchy from this point
    pub fn depth(&self) -> usize {
        if self.children.is_empty() {
            1
        } else {
            1 + self.children.iter().map(|c| c.depth()).max().unwrap_or(0)
        }
    }

    /// Find a child structure by name
    pub fn find_child(&self, name: &str) -> Option<&Structure<M>> {
        self.children.iter().find(|c| c.name == name)
    }

    /// Find a child structure by name (mutable)
    pub fn find_child_mut(&mut self, name: &str) -> Option<&mut Structure<M>> {
        self.children.iter_mut().find(|c| c.name == name)
    }

    /// Collapse nodes that have exactly one child and no items
    /// This implements the "only create folders when needed" philosophy
    pub fn collapse_single_children(&mut self) {
        // First, recursively collapse children
        for child in &mut self.children {
            child.collapse_single_children();
        }

        // If this node has exactly one child and no items, collapse it
        if self.children.len() == 1 && self.items.is_empty() {
            let only_child = self.children.remove(0);
            
            // Promote the child's properties to this node
            self.name = only_child.name;
            self.display_name = only_child.display_name;
            self.items = only_child.items;
            self.children = only_child.children;
            
            // Recursively collapse again in case we created another single-child situation
            self.collapse_single_children();
        }
    }

    /// Check if this structure or any of its descendants have items
    fn has_items_recursively(&self) -> bool {
        !self.items.is_empty() || self.children.iter().any(|c| c.has_items_recursively())
    }

    /// Enhanced collapse function that minimizes folder hierarchy while preserving detail
    /// 
    /// Algorithm:
    /// 1. Start from top track
    /// 2. Check if there's a parent -> if false, it's top level
    /// 3. Check if there's a single direct child
    /// 4. Keep checking the entire hierarchy down until we understand the whole structure
    /// 5. If a top-level track has one single direct child, unless it is the last group, it should be removed
    /// 
    /// `is_top_level` indicates if this is a top-level group (like "Drums", "Bass", etc.)
    /// `depth` is used for debug indentation
    pub fn collapse_hierarchy(&mut self, is_top_level: bool) {
        // Default implementation without config lookup (for backward compatibility)
        self.collapse_hierarchy_with_config(is_top_level, |_| None);
    }

    /// Enhanced collapse function with config lookup to identify deepest groups
    /// 
    /// `find_group` is a closure that can look up a group by name in the config
    /// to check if it has subgroups (deepest groups have no subgroups)
    pub fn collapse_hierarchy_with_config<'a, F>(&mut self, is_top_level: bool, find_group: F)
    where
        F: Fn(&str) -> Option<&'a crate::Group<M>>,
    {
        self.collapse_hierarchy_internal(is_top_level, 0, &find_group);
    }

    /// Internal implementation with depth tracking for debugging
    fn collapse_hierarchy_internal<'a, F>(&mut self, is_top_level: bool, depth: usize, find_group: &F)
    where
        F: Fn(&str) -> Option<&'a crate::Group<M>>,
    {
        let indent = "  ".repeat(depth);
        eprintln!("{}[COLLAPSE] Processing: '{}' (top_level: {}, children: {}, items: {})", 
                 indent, self.name, is_top_level, self.children.len(), self.items.len());

        // First, recursively process all children from top to bottom
        for child in &mut self.children {
            child.collapse_hierarchy_internal(false, depth + 1, find_group);
        }

        eprintln!("{}[COLLAPSE] After recursive processing: '{}' (children: {})", 
                 indent, self.name, self.children.len());

        // Now check each child - if it has only one direct child and no items, remove it
        // BUT keep it if it has siblings (other groups at the same level) - this preserves organizational structure
        let mut new_children = Vec::new();
        let total_siblings = self.children.len();
        for mut child in std::mem::take(&mut self.children) {
            let has_single_child = child.children.len() == 1;
            let has_no_items = child.items.is_empty();
            
            // Check if this is a "deepest group" - a group that has no organizational subgroups defined in the config
            // Deepest groups like "Kick" should never be removed
            // The `groups` vector contains organizational subgroups (like "Kick", "Snare" under "Drum Kit")
            // Metadata groups (like multi_mic, MultiMic) are stored in groups but are not organizational subgroups
            // They are used for metadata extraction, not for creating folder structure
            let is_deepest_group = if let Some(group) = find_group(&child.name) {
                // Known metadata field group names that should not count as organizational subgroups
                let metadata_group_names = ["MultiMic", "SUM", "Section", "Layers", "Effect"];
                
                // Filter out metadata groups - these are not organizational subgroups
                let org_subgroups: Vec<_> = group.groups.iter()
                    .filter(|g| !g.metadata_only && !metadata_group_names.contains(&g.name.as_str()))
                    .collect();
                let has_no_org_subgroups = org_subgroups.is_empty();
                
                if !has_no_org_subgroups {
                    eprintln!("{}[COLLAPSE] Group '{}' has {} organizational subgroups: {:?}", 
                             indent, child.name, org_subgroups.len(), 
                             org_subgroups.iter().map(|g| &g.name).collect::<Vec<_>>());
                }
                eprintln!("{}[COLLAPSE] Found group '{}' in config, has {} total subgroups, {} org subgroups (is_deepest: {})", 
                         indent, child.name, group.groups.len(), org_subgroups.len(), has_no_org_subgroups);
                has_no_org_subgroups
            } else {
                eprintln!("{}[COLLAPSE] Group '{}' NOT found in config", indent, child.name);
                false  // If we can't find it in config, assume it's not deepest
            };
            
            // A group is "last group" if it directly has items OR if all its children are leaf nodes (have items, no grandchildren)
            let is_last_group = !child.items.is_empty() || 
                               (child.children.iter().all(|c| !c.items.is_empty() && c.children.is_empty()));
            // Check if this group organizes items (has children that are all leaf nodes)
            let organizes_items = !child.children.is_empty() &&
                child.children.iter().all(|c| !c.items.is_empty() && c.children.is_empty());
            // Check if all children are "last groups" (organize items directly or via leaf children)
            // BUT exclude groups that organize items themselves (like "Kick" organizing "In" and "Out")
            let all_children_are_last_groups = !child.children.is_empty() &&
                child.children.iter().all(|c| {
                    !c.items.is_empty() ||  // Has items directly
                    (c.children.iter().all(|gc| !gc.items.is_empty() && gc.children.is_empty()))  // All children are leaf nodes
                }) && !organizes_items;  // Don't remove if this group organizes items
            
            eprintln!("{}[COLLAPSE] Checking child: '{}' (single_child: {}, no_items: {}, is_deepest: {}, is_last_group: {}, organizes_items: {}, all_children_last: {})", 
                     indent, child.name, has_single_child, has_no_items, is_deepest_group, is_last_group, organizes_items, all_children_are_last_groups);

            // Never remove deepest groups (like "Kick", "Snare") - they are the actual instrument groups
            if is_deepest_group {
                eprintln!("{}[COLLAPSE] KEEPING deepest group: '{}'", indent, child.name);
                new_children.push(child);
                continue;
            }

            // Check if the single child is a deepest group
            let single_child_is_deepest = has_single_child && 
                find_group(&child.children[0].name)
                    .map(|g| g.groups.iter().all(|sg| sg.metadata_only || 
                         ["MultiMic", "SUM", "Section", "Layers", "Effect"].contains(&sg.name.as_str())))
                    .unwrap_or(false);

            // Check if all children are deepest groups (have no organizational subgroups)
            let all_children_are_deepest = !child.children.is_empty() &&
                child.children.iter().all(|c| {
                    find_group(&c.name)
                        .map(|g| {
                            let metadata_group_names = ["MultiMic", "SUM", "Section", "Layers", "Effect"];
                            let org_subgroups: Vec<_> = g.groups.iter()
                                .filter(|sg| !sg.metadata_only && !metadata_group_names.contains(&sg.name.as_str()))
                                .collect();
                            org_subgroups.is_empty()
                        })
                        .unwrap_or(false)
                });
            
            // Remove if:
            // 1. It has a single direct child AND no items AND (it's not the last group OR the child is a deepest group), OR
            // 2. It has no items AND all its children are "last groups" (organize items) AND it doesn't organize items itself
            //    AND it has only one child (if it has multiple children, keep it as an organizational level), OR
            // 3. It has no items AND all its children are deepest groups (no organizational subgroups)
            //    This removes intermediate levels like "Drum Kit" when all children are deepest groups (Kick, Snare)
            // 4. It has no items AND no siblings (it's the only child of its parent) - remove it to minimize hierarchy
            //    This removes "Drum Kit" when it's the only child of "Drums" and there are no other groups like "Electronic Kit"
            // 5. It has items AND no siblings AND no children (or all children are leaf nodes) - promote items to parent
            //    This collapses metadata field levels (like "L" channel) when they're the only child and have items
            // BUT: Keep the group if it has siblings (other groups at the same level) - this preserves organizational structure
            // when multiple groups exist (e.g., "Drum Kit" and "Electronic Kit" both under "Drums")
            let has_siblings = total_siblings > 1;
            let has_items_but_no_children = !child.items.is_empty() && child.children.is_empty();
            let has_items_and_only_leaf_children = !child.items.is_empty() && 
                !child.children.is_empty() &&
                child.children.iter().all(|c| c.children.is_empty() && c.items.is_empty());
            
            if !has_siblings && ((has_single_child && has_no_items && (!is_last_group || single_child_is_deepest)) ||
               (has_no_items && all_children_are_last_groups && !organizes_items && has_single_child) ||
               (has_no_items && all_children_are_deepest && !organizes_items) ||
               (has_no_items && !has_siblings) ||
               (has_items_but_no_children && !has_siblings) ||
               (has_items_and_only_leaf_children && !has_siblings)) {
                eprintln!("{}[COLLAPSE] REMOVING intermediate: '{}' (promoting {} children, {} items, no siblings)", 
                         indent, child.name, child.children.len(), child.items.len());
                // Promote items to parent
                self.items.extend(std::mem::take(&mut child.items));
                // Promote all children, not just the first one
                new_children.extend(std::mem::take(&mut child.children));
            } else {
                if has_siblings {
                    eprintln!("{}[COLLAPSE] KEEPING: '{}' (has {} siblings)", indent, child.name, total_siblings - 1);
                } else {
                    eprintln!("{}[COLLAPSE] KEEPING: '{}'", indent, child.name);
                }
                new_children.push(child);
            }
        }
        self.children = new_children;

        eprintln!("{}[COLLAPSE] After removing intermediates: '{}' (children: {})", 
                 indent, self.name, self.children.len());

        // Special handling for top-level:
        // If top-level has only 1 child and no items, remove the top-level (promote the child)
        // ONLY if the child is a deepest group (has no organizational subgroups in config)
        // This minimizes hierarchy when the child is a leaf group, but keeps organizational levels
        if is_top_level && self.children.len() == 1 && self.items.is_empty() {
            let only_child = &self.children[0];
            let metadata_group_names = ["MultiMic", "SUM", "Section", "Layers", "Effect"];
            let is_deepest_group = if let Some(group) = find_group(&only_child.name) {
                let org_subgroups: Vec<_> = group.groups.iter()
                    .filter(|g| !g.metadata_only && !metadata_group_names.contains(&g.name.as_str()))
                    .collect();
                org_subgroups.is_empty()
            } else {
                false
            };
            
            eprintln!("{}[COLLAPSE] Top-level '{}' has 1 child '{}' (is_deepest: {})", 
                     indent, self.name, only_child.name, is_deepest_group);
            
            // Only remove top-level if child is a deepest group (leaf group with no organizational subgroups)
            // This keeps organizational levels like "Drums" -> "Drum Kit" -> "Kick"
            if is_deepest_group {
                eprintln!("{}[COLLAPSE] REMOVING top-level: '{}' (promoting deepest group '{}')", 
                         indent, self.name, only_child.name);
                let only_child = self.children.remove(0);
                self.name = only_child.name;
                self.display_name = only_child.display_name;
                self.items = only_child.items;
                self.children = only_child.children;
                // The promoted child is now at top level, recursively process it
                self.collapse_hierarchy_internal(true, depth, find_group);
            } else {
                eprintln!("{}[COLLAPSE] KEEPING top-level: '{}' (child is last group)", 
                         indent, self.name);
            }
        }
    }

    /// Print the structure as a tree to stdout
    pub fn print_tree(&self) {
        // Skip root level if it's just a container
        if self.name == "root" || self.name.is_empty() {
            for child in &self.children {
                child.print_tree_with_prefix("", true);
            }
            // Print root items if any
            if !self.items.is_empty() {
                print!("-Uncategorized: [");
                for (i, item) in self.items.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    print!("{}", item.original);
                }
                println!("]");
            }
        } else {
            self.print_tree_with_prefix("", true);
        }
    }

    /// Print the structure as a tree with the given prefix
    fn print_tree_with_prefix(&self, prefix: &str, _is_last: bool) {
        // Print current node
        let node_prefix = format!("{}-", prefix);
        if !self.items.is_empty() {
            // Node with items - show count if many items
            if self.items.len() > 3 {
                print!("{}{}: [", node_prefix, self.display_name);
                for i in 0..2 {
                    if i > 0 {
                        print!(", ");
                    }
                    print!("{}", self.items[i].original);
                }
                println!(", ... and {} more]", self.items.len() - 2);
            } else {
                print!("{}{}: [", node_prefix, self.display_name);
                for (i, item) in self.items.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    print!("{}", item.original);
                }
                println!("]");
            }
        } else if !self.children.is_empty() {
            // Node without items but with children
            println!("{}{}", node_prefix, self.display_name);
        } else {
            // Empty node (shouldn't happen normally but handle it)
            println!("{}{} (empty)", node_prefix, self.display_name);
        }

        // Print children
        for (i, child) in self.children.iter().enumerate() {
            let is_last_child = i == self.children.len() - 1;
            let child_prefix = format!("{}-", prefix);
            child.print_tree_with_prefix(&child_prefix, is_last_child);
        }
    }

    /// Convert the structure to a pretty-printed string tree
    pub fn to_tree_string(&self) -> String {
        let mut result = String::new();

        // Skip root level if it's just a container
        if self.name == "root" || self.name.is_empty() {
            for child in &self.children {
                result.push_str(&child.to_tree_string_with_prefix("", true));
            }
            // Add root items if any
            if !self.items.is_empty() {
                result.push_str("-Uncategorized: [");
                for (i, item) in self.items.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&item.original);
                }
                result.push_str("]\n");
            }
        } else {
            result.push_str(&self.to_tree_string_with_prefix("", true));
        }

        result
    }

    fn to_tree_string_with_prefix(&self, prefix: &str, _is_last: bool) -> String {
        let mut result = String::new();

        // Build current node string
        let node_prefix = format!("{}-", prefix);
        if !self.items.is_empty() {
            // Node with items - show count if many items
            if self.items.len() > 3 {
                result.push_str(&format!("{}{}: [", node_prefix, self.name));
                for i in 0..2 {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&self.items[i].original);
                }
                result.push_str(&format!(", ... and {} more]\n", self.items.len() - 2));
            } else {
                result.push_str(&format!("{}{}: [", node_prefix, self.name));
                for (i, item) in self.items.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&item.original);
                }
                result.push_str("]\n");
            }
        } else if !self.children.is_empty() {
            // Node without items but with children
            result.push_str(&format!("{}{}\n", node_prefix, self.name));
        } else {
            // Empty node
            result.push_str(&format!("{}{} (empty)\n", node_prefix, self.name));
        }

        // Add children
        for (i, child) in self.children.iter().enumerate() {
            let is_last_child = i == self.children.len() - 1;
            let child_prefix = format!("{}-", prefix);
            result.push_str(&child.to_tree_string_with_prefix(&child_prefix, is_last_child));
        }

        result
    }
}

impl<M: Metadata> fmt::Display for Structure<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_tree_string())
    }
}
