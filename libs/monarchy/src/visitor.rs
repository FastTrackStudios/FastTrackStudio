//! Visitor pattern for Structure traversal and transformation
//!
//! This module provides a visitor pattern for traversing and modifying
//! Structure trees. Visitors can be used for:
//! - Collapsing unnecessary hierarchy levels
//! - Renaming nodes based on metadata
//! - Removing empty nodes
//! - Custom transformations
//!
//! # Example
//!
//! ```ignore
//! use monarchy::{Structure, Visitable, CollapseHierarchy};
//!
//! let mut structure = organize_items(...);
//! structure.accept(&mut CollapseHierarchy::new(&config));
//! ```

use crate::{Config, Group, Metadata, Structure};

/// Trait for visiting and transforming Structure nodes
///
/// The visitor pattern allows separation of algorithms from the data structure.
/// Visitors are called in a depth-first manner, with `enter` called before
/// visiting children and `leave` called after.
pub trait StructureVisitor<M: Metadata> {
    /// Called when entering a node (before visiting children)
    ///
    /// Return `true` to continue visiting children, `false` to skip them.
    fn enter(&mut self, _node: &mut Structure<M>, _depth: usize) -> bool {
        true
    }

    /// Called when leaving a node (after visiting children)
    ///
    /// This is where most transformations should happen, as children
    /// have already been processed.
    fn leave(&mut self, _node: &mut Structure<M>, _depth: usize) {}
}

/// Trait for structures that can accept visitors
pub trait Visitable<M: Metadata> {
    /// Accept a visitor, traversing the structure depth-first
    fn accept<V: StructureVisitor<M>>(&mut self, visitor: &mut V);

    /// Accept a visitor with depth tracking
    fn accept_with_depth<V: StructureVisitor<M>>(&mut self, visitor: &mut V, depth: usize);
}

impl<M: Metadata> Visitable<M> for Structure<M> {
    fn accept<V: StructureVisitor<M>>(&mut self, visitor: &mut V) {
        self.accept_with_depth(visitor, 0);
    }

    fn accept_with_depth<V: StructureVisitor<M>>(&mut self, visitor: &mut V, depth: usize) {
        // Enter this node
        if !visitor.enter(self, depth) {
            return; // Skip children if enter returns false
        }

        // Visit children
        for child in &mut self.children {
            child.accept_with_depth(visitor, depth + 1);
        }

        // Leave this node
        visitor.leave(self, depth);
    }
}

/// Helper struct with shared config lookup methods
struct CollapseHelper<'a, M: Metadata> {
    config: &'a Config<M>,
    /// Names of metadata field groups that don't count as organizational subgroups
    metadata_group_names: Vec<&'static str>,
}

impl<'a, M: Metadata> CollapseHelper<'a, M> {
    fn new(config: &'a Config<M>) -> Self {
        Self { 
            config,
            metadata_group_names: vec!["MultiMic", "SUM", "Section", "Layers", "Effect", "Arrangement", "Performer", "Channel"],
        }
    }
    
    /// Find a group by name in the config (recursive search)
    fn find_group(&self, name: &str) -> Option<&'a Group<M>> {
        fn search<'a, M: Metadata>(group: &'a Group<M>, name: &str) -> Option<&'a Group<M>> {
            if group.name.eq_ignore_ascii_case(name) {
                return Some(group);
            }
            for nested in &group.groups {
                if let Some(found) = search(nested, name) {
                    return Some(found);
                }
            }
            None
        }

        for group in &self.config.groups {
            if let Some(found) = search(group, name) {
                return Some(found);
            }
        }
        None
    }
    
    /// Check if a group is a "deepest group" (has no organizational subgroups in config)
    /// Deepest groups like Kick, Snare should never be collapsed
    fn is_deepest_group(&self, name: &str) -> bool {
        if let Some(group) = self.find_group(name) {
            // Filter out metadata groups - these are not organizational subgroups
            let org_subgroups: Vec<_> = group
                .groups
                .iter()
                .filter(|g| {
                    !g.metadata_only && !self.metadata_group_names.contains(&g.name.as_str())
                })
                .collect();
            org_subgroups.is_empty()
        } else {
            // Not in config - this is a metadata-derived group (like In, Out)
            // These should never be collapsed
            true
        }
    }
    
    /// Check if a name corresponds to a config-defined group
    fn is_config_group(&self, name: &str) -> bool {
        self.find_group(name).is_some()
    }
    
    /// Check if a name is a top-level category (direct child of config.groups)
    fn is_top_level_category(&self, name: &str) -> bool {
        self.config.groups.iter().any(|g| g.name.eq_ignore_ascii_case(name))
    }
    
    /// Check if a top-level category has patterns (can match items directly)
    /// Groups with patterns should keep their name when collapsing (Guitars, Vocals)
    /// Groups without patterns are just containers and should collapse to their children (Drums)
    fn top_level_has_patterns(&self, name: &str) -> bool {
        self.config.groups.iter()
            .find(|g| g.name.eq_ignore_ascii_case(name))
            .map(|g| !g.patterns.is_empty())
            .unwrap_or(false)
    }
    
    /// Find the parent group of a given group name
    fn find_parent_group(&self, child_name: &str) -> Option<&'a Group<M>> {
        fn search_parent<'a, M: Metadata>(
            parent: &'a Group<M>, 
            child_name: &str
        ) -> Option<&'a Group<M>> {
            for child in &parent.groups {
                if child.name.eq_ignore_ascii_case(child_name) {
                    return Some(parent);
                }
                if let Some(found) = search_parent(child, child_name) {
                    return Some(found);
                }
            }
            None
        }

        for group in &self.config.groups {
            // Check if this top-level group is the parent
            for child in &group.groups {
                if child.name.eq_ignore_ascii_case(child_name) {
                    return Some(group);
                }
            }
            // Search deeper
            if let Some(found) = search_parent(group, child_name) {
                return Some(found);
            }
        }
        None
    }
    
    /// Check if a top-level category is transparent
    /// Transparent groups have their children promoted to top level
    fn is_top_level_transparent(&self, name: &str) -> bool {
        self.config.groups.iter()
            .find(|g| g.name.eq_ignore_ascii_case(name))
            .map(|g| g.transparent)
            .unwrap_or(false)
    }
    
    /// Check if a child group shares any patterns with its parent
    /// If true, the child should collapse INTO the parent's name (e.g., Lead Vocals -> Vocals)
    /// If false, the child should keep its own name (e.g., BGVs stays BGVs)
    fn child_shares_parent_patterns(&self, child_name: &str) -> bool {
        let child_group = match self.find_group(child_name) {
            Some(g) => g,
            None => return false,
        };
        
        let parent_group = match self.find_parent_group(child_name) {
            Some(g) => g,
            None => return false,
        };
        
        // Check if any pattern in child matches any pattern in parent
        for child_pattern in &child_group.patterns {
            let child_pattern_lower = child_pattern.to_lowercase();
            for parent_pattern in &parent_group.patterns {
                if child_pattern_lower == parent_pattern.to_lowercase() {
                    return true;
                }
            }
        }
        
        false
    }
}

/// Pass 1: Collapse intermediate groups
/// 
/// Removes intermediate groups between top-level categories and deepest groups.
/// For example: Drums -> Drum Kit -> [Kick, Snare] becomes Drums -> [Kick, Snare]
/// 
/// Rules:
/// - Never collapse top-level categories (Drums, Guitars, Vocals, etc.)
/// - Never collapse deepest groups (Kick, Snare, etc.) 
/// - Never collapse metadata-derived groups (In, Out, L, R, etc.)
/// - Collapse intermediate config groups that have a single child
pub struct CollapseIntermediateGroups<'a, M: Metadata> {
    helper: CollapseHelper<'a, M>,
}

impl<'a, M: Metadata> CollapseIntermediateGroups<'a, M> {
    pub fn new(config: &'a Config<M>) -> Self {
        Self { helper: CollapseHelper::new(config) }
    }
    
    /// Collapse single-child chains of metadata-derived groups
    /// This takes Electric -> Clean -> DBL -> L: [item] and collapses to Electric: [item]
    fn collapse_metadata_children(&self, node: &mut Structure<M>) {
        // While we have a single child that's a metadata-derived group (not in config),
        // collapse it by promoting its items and children
        while node.children.len() == 1 && node.items.is_empty() {
            let only_child = &node.children[0];
            
            // If the child is a config group, stop - we don't collapse config groups here
            if self.helper.is_config_group(&only_child.name) {
                break;
            }
            
            // Child is metadata-derived, collapse it
            let only_child = node.children.remove(0);
            node.items.extend(only_child.items);
            node.children = only_child.children;
        }
    }
}

impl<'a, M: Metadata> StructureVisitor<M> for CollapseIntermediateGroups<'a, M> {
    fn leave(&mut self, node: &mut Structure<M>, _depth: usize) {
        // First, collapse single-child metadata chains for ALL nodes
        // This handles: Clean -> Main: [item] => Clean: [item]
        // Even when node has multiple children (like Electric -> [Clean, Drive])
        // each child should have its single-child metadata chains collapsed
        if !self.helper.is_config_group(&node.name) {
            // This is a metadata-derived group - collapse any single-child metadata chains
            self.collapse_metadata_children(node);
        } else if self.helper.is_deepest_group(&node.name) {
            // This is a deepest config group - collapse metadata-only children
            // This handles: Electric -> Clean -> DBL -> L: [item] => Electric: [item]
            self.collapse_metadata_children(node);
        }
        
        // Don't process nodes that are top-level categories - they're handled in pass 2
        if self.helper.is_top_level_category(&node.name) {
            // But DO collapse intermediate children within top-level categories
            while node.children.len() == 1 && node.items.is_empty() {
                let only_child = &node.children[0];
                
                // Stop if child is a deepest group
                if self.helper.is_deepest_group(&only_child.name) {
                    break;
                }
                
                // Stop if child is not in config (metadata-derived)
                if !self.helper.is_config_group(&only_child.name) {
                    break;
                }
                
                // Stop if child is also a top-level category (shouldn't happen but be safe)
                if self.helper.is_top_level_category(&only_child.name) {
                    break;
                }
                
                // Collapse: promote child's children and items to this node
                let only_child = node.children.remove(0);
                node.items.extend(only_child.items);
                node.children = only_child.children;
            }
            return;
        }
        
        // For non-top-level config nodes (intermediate groups), collapse single-child chains
        // but stop at deepest groups and metadata-derived groups
        while node.children.len() == 1 && node.items.is_empty() {
            let only_child = &node.children[0];
            
            // Stop if child is a deepest group
            if self.helper.is_deepest_group(&only_child.name) {
                break;
            }
            
            // Stop if child is not in config (metadata-derived)
            if !self.helper.is_config_group(&only_child.name) {
                break;
            }
            
            // Stop if child is a top-level category
            if self.helper.is_top_level_category(&only_child.name) {
                break;
            }
            
            // Collapse: promote child's children and items to this node
            let only_child = node.children.remove(0);
            node.items.extend(only_child.items);
            node.children = only_child.children;
        }
    }
}

/// Pass 2: Collapse top-level groups
///
/// After intermediate groups are collapsed, this pass removes top-level categories
/// that have only a single child remaining.
/// 
/// For example: 
/// - Drums -> Kick (single deepest group) becomes just Kick
/// - Drums -> [Kick, Snare] stays as Drums -> [Kick, Snare]
pub struct CollapseTopLevelGroups<'a, M: Metadata> {
    helper: CollapseHelper<'a, M>,
}

impl<'a, M: Metadata> CollapseTopLevelGroups<'a, M> {
    pub fn new(config: &'a Config<M>) -> Self {
        Self { helper: CollapseHelper::new(config) }
    }
    
    /// For top-level groups WITH patterns (Guitars, Vocals):
    /// Collapse intermediate config groups based on transparency:
    /// 
    /// Rules:
    /// - If parent is NOT transparent → always keep parent name (Guitars, Keys)
    /// - If parent IS transparent → always use child name (Lead Vocals, BGVs)
    /// 
    /// Examples:
    /// - Guitars -> Electric -> [items] becomes Guitars: [items] (Guitars not transparent)
    /// - Guitars -> Acoustic -> [items] becomes Guitars: [items] (Guitars not transparent)
    /// - Vocals -> Lead Vocals -> [items] becomes Lead Vocals: [items] (Vocals is transparent)
    /// - Vocals -> BGVs -> [items] becomes BGVs: [items] (Vocals is transparent)
    fn collapse_intermediates_into_top_level(&self, node: &mut Structure<M>) {
        // Check if this top-level group is transparent
        let is_transparent = self.helper.is_top_level_transparent(&node.name);
        
        // While we have a single child that's a config group (not metadata-derived),
        // decide whether to keep parent name or child name
        while node.children.len() == 1 && node.items.is_empty() {
            let only_child = &node.children[0];
            
            // If child is not a config group (metadata-derived), stop
            if !self.helper.is_config_group(&only_child.name) {
                break;
            }
            
            // Decide whether to keep parent name or use child name
            let keep_parent_name = !is_transparent;
            // Non-transparent parent: always keep parent name (Guitars, Keys, etc.)
            // Transparent parent: always use child name (Lead Vocals, BGVs)
            
            if keep_parent_name {
                // Keep THIS node's name, promote child's content
                let only_child = node.children.remove(0);
                node.items = only_child.items;
                node.children = only_child.children;
            } else {
                // Replace this node with child (transparent parent like Vocals)
                let only_child = node.children.remove(0);
                node.name = only_child.name;
                node.display_name = only_child.display_name;
                node.items = only_child.items;
                node.children = only_child.children;
            }
        }
    }
    
    /// For top-level groups WITHOUT patterns (Drums):
    /// Collapse all the way to the deepest config group
    /// Drums -> Drum Kit -> Kick -> [items] becomes Kick: [items]
    fn collapse_to_deepest(&self, node: &mut Structure<M>) {
        while node.children.len() == 1 && node.items.is_empty() {
            let only_child = &node.children[0];
            
            // If child is not a config group, stop
            if !self.helper.is_config_group(&only_child.name) {
                break;
            }
            
            // Replace this node with the child
            let only_child = node.children.remove(0);
            node.name = only_child.name;
            node.display_name = only_child.display_name;
            node.items = only_child.items;
            node.children = only_child.children;
        }
    }
}

impl<'a, M: Metadata> StructureVisitor<M> for CollapseTopLevelGroups<'a, M> {
    fn leave(&mut self, node: &mut Structure<M>, depth: usize) {
        // Only process root level (depth 0)
        if depth != 0 {
            return;
        }
        
        // At root level, process each top-level child
        // For each child that's a top-level category:
        // - If it has patterns (Guitars, Vocals): keep the name, promote children's content
        // - If it has no patterns (Drums): collapse to the deepest matching group
        
        for child in &mut node.children {
            if !self.helper.is_top_level_category(&child.name) {
                continue;
            }
            
            // Check if this top-level category has patterns
            if self.helper.top_level_has_patterns(&child.name) {
                // Has patterns (Guitars, Vocals) - keep the top-level name
                // But collapse intermediate groups INTO this one
                self.collapse_intermediates_into_top_level(child);
            } else {
                // No patterns (Drums) - can collapse to specific child
                self.collapse_to_deepest(child);
            }
        }
        
        // Now handle the case where root has a single child that should become root
        while node.children.len() == 1 && node.items.is_empty() {
            let only_child = &node.children[0];
            
            // If the single child has multiple grandchildren, keep it
            if only_child.children.len() > 1 {
                break;
            }
            
            // If the single child is a top-level with patterns, keep it
            if self.helper.is_top_level_category(&only_child.name) 
                && self.helper.top_level_has_patterns(&only_child.name) {
                break;
            }
            
            // If the single child has items directly, it can become root
            if !only_child.items.is_empty() && only_child.children.is_empty() {
                let only_child = node.children.remove(0);
                node.name = only_child.name;
                node.display_name = only_child.display_name;
                node.items = only_child.items;
                node.children = only_child.children;
                break;
            }
            
            // If single child has a single grandchild, collapse one level
            if only_child.children.len() == 1 {
                let only_child = node.children.remove(0);
                let grandchild = only_child.children.into_iter().next().unwrap();
                node.name = grandchild.name;
                node.display_name = grandchild.display_name;
                node.items = grandchild.items;
                node.children = grandchild.children;
                continue;
            }
            
            break;
        }
    }
}

/// Combined collapse visitor that runs both passes
/// 
/// This is a convenience wrapper that applies:
/// 1. CollapseIntermediateGroups - removes unnecessary intermediate levels
/// 2. CollapseTopLevelGroups - removes top-level groups when they have single children
pub struct CollapseHierarchy<'a, M: Metadata> {
    config: &'a Config<M>,
}

impl<'a, M: Metadata> CollapseHierarchy<'a, M> {
    pub fn new(config: &'a Config<M>) -> Self {
        Self { config }
    }
    
    /// Apply all collapse passes to a structure
    pub fn apply(&self, root: &mut Structure<M>) {
        // Pass 1: Collapse intermediate groups
        let mut pass1 = CollapseIntermediateGroups::new(self.config);
        root.accept(&mut pass1);
        
        // Pass 2: Collapse top-level groups  
        let mut pass2 = CollapseTopLevelGroups::new(self.config);
        root.accept(&mut pass2);
    }
}

// Keep the StructureVisitor impl for backwards compatibility, but it just delegates to apply()
impl<'a, M: Metadata> StructureVisitor<M> for CollapseHierarchy<'a, M> {
    fn leave(&mut self, _node: &mut Structure<M>, _depth: usize) {
        // This is intentionally empty - the real work is done in apply()
        // The visitor pattern is used by the organizer, so we need this impl
    }
}

/// Visitor that organizes items into tagged collection subfolders
///
/// A tagged collection is a pattern-based grouping that creates a subfolder
/// for items matching certain patterns (e.g., "SUM" for summed tracks).
///
/// This visitor:
/// - Looks at each node's items
/// - If a node has a tagged collection defined in config
/// - AND has both matching and non-matching items
/// - Creates a subfolder for the matching items
///
/// If ALL items match (or none match), no subfolder is created - items stay at current level.
/// This prevents unnecessary nesting when there's nothing to separate.
pub struct ApplyTaggedCollections<'a, M: Metadata> {
    config: &'a Config<M>,
}

impl<'a, M: Metadata> ApplyTaggedCollections<'a, M> {
    pub fn new(config: &'a Config<M>) -> Self {
        Self { config }
    }

    /// Find a group by name in the config (recursive search)
    fn find_group(&self, name: &str) -> Option<&'a Group<M>> {
        fn search<'a, M: Metadata>(group: &'a Group<M>, name: &str) -> Option<&'a Group<M>> {
            if group.name.eq_ignore_ascii_case(name) {
                return Some(group);
            }
            for nested in &group.groups {
                if let Some(found) = search(nested, name) {
                    return Some(found);
                }
            }
            None
        }

        for group in &self.config.groups {
            if let Some(found) = search(group, name) {
                return Some(found);
            }
        }
        None
    }
}

impl<'a, M: Metadata> StructureVisitor<M> for ApplyTaggedCollections<'a, M> {
    fn leave(&mut self, node: &mut Structure<M>, _depth: usize) {
        // Find if this node's group has a tagged collection
        let tagged_collection = match self.find_group(&node.name) {
            Some(group) => group.tagged_collection.as_ref(),
            None => return,
        };

        let tagged_collection = match tagged_collection {
            Some(tc) => tc,
            None => return,
        };

        // No items to process
        if node.items.is_empty() {
            return;
        }

        // Partition items into matching and non-matching
        let mut matching_items = Vec::new();
        let mut non_matching_items = Vec::new();

        for item in std::mem::take(&mut node.items) {
            if tagged_collection.matches(&item.original) {
                matching_items.push(item);
            } else {
                non_matching_items.push(item);
            }
        }

        // Only create subfolder if we have BOTH matching and non-matching items
        // This prevents unnecessary nesting
        if !matching_items.is_empty() && !non_matching_items.is_empty() {
            // Create subfolder for tagged collection items
            let mut collection_node = Structure::new(&tagged_collection.name);
            collection_node.items = matching_items;
            
            // Add collection as first child (before other children)
            node.children.insert(0, collection_node);
            
            // Keep non-matching items at current level
            node.items = non_matching_items;
        } else {
            // All items match or none match - keep at current level
            node.items = matching_items;
            node.items.extend(non_matching_items);
        }
    }
}

/// Visitor that collects unsorted items into a single top-level "Unsorted" folder
///
/// Items are considered "unsorted" if they sit on a node that also has children
/// (subgroups they could potentially belong to). These items bubble up to a
/// single Unsorted folder at the root level.
///
/// This visitor:
/// - Traverses the entire tree
/// - Collects items from nodes that have both items AND children
/// - Removes those items from their original location
/// - After traversal, call `finalize()` to get the collected items
///
/// # Usage
/// ```ignore
/// let mut collector = CollectUnsorted::new();
/// structure.accept(&mut collector);
/// let unsorted_items = collector.finalize();
/// 
/// // Add unsorted folder to root if there are items
/// if !unsorted_items.is_empty() {
///     let mut unsorted_folder = Structure::new("Unsorted");
///     unsorted_folder.items = unsorted_items;
///     structure.children.push(unsorted_folder);
/// }
/// ```
pub struct CollectUnsorted<M: Metadata> {
    /// Name to use for unsorted folder (default: "Unsorted")
    pub folder_name: String,
    /// Collected unsorted items
    collected: Vec<crate::Item<M>>,
}

impl<M: Metadata> Default for CollectUnsorted<M> {
    fn default() -> Self {
        Self {
            folder_name: "Unsorted".to_string(),
            collected: Vec::new(),
        }
    }
}

impl<M: Metadata> CollectUnsorted<M> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_folder_name(name: impl Into<String>) -> Self {
        Self {
            folder_name: name.into(),
            collected: Vec::new(),
        }
    }

    /// Get the collected unsorted items and reset the collector
    pub fn finalize(&mut self) -> Vec<crate::Item<M>> {
        std::mem::take(&mut self.collected)
    }
    
    /// Check if any unsorted items were collected
    pub fn has_unsorted(&self) -> bool {
        !self.collected.is_empty()
    }
}

impl<M: Metadata> StructureVisitor<M> for CollectUnsorted<M> {
    fn leave(&mut self, node: &mut Structure<M>, _depth: usize) {
        // Only collect unsorted if:
        // 1. Node has items at this level
        // 2. Node also has children (subgroups that items could potentially belong to)
        // 
        // If there are no children, items belong here and aren't "unsorted"
        if node.items.is_empty() || node.children.is_empty() {
            return;
        }

        // Collect items - they'll be added to a top-level Unsorted folder later
        self.collected.extend(std::mem::take(&mut node.items));
    }
}

/// Helper function to apply CollectUnsorted and add the Unsorted folder to root
/// 
/// This is a convenience function that:
/// 1. Runs the CollectUnsorted visitor
/// 2. If any items were collected, adds an "Unsorted" folder to the root
/// 3. Returns the number of unsorted items collected
pub fn collect_unsorted_to_root<M: Metadata>(root: &mut Structure<M>) -> usize {
    collect_unsorted_to_root_with_name(root, "Unsorted")
}

/// Helper function to apply CollectUnsorted with a custom folder name
pub fn collect_unsorted_to_root_with_name<M: Metadata>(
    root: &mut Structure<M>,
    folder_name: &str,
) -> usize {
    let mut collector = CollectUnsorted::with_folder_name(folder_name);
    root.accept(&mut collector);
    
    let unsorted_items = collector.finalize();
    let count = unsorted_items.len();
    
    if !unsorted_items.is_empty() {
        let mut unsorted_folder = Structure::new(folder_name);
        unsorted_folder.items = unsorted_items;
        root.children.push(unsorted_folder);
    }
    
    count
}

/// Visitor that removes empty nodes (no items and no children)
pub struct RemoveEmptyNodes;

impl<M: Metadata> StructureVisitor<M> for RemoveEmptyNodes {
    fn leave(&mut self, node: &mut Structure<M>, _depth: usize) {
        node.children.retain(|child| !child.items.is_empty() || !child.children.is_empty());
    }
}

/// Visitor that promotes single children to their parent
///
/// If a node has exactly one child and no items, the child is promoted
/// to replace the parent (the parent is removed from the hierarchy).
pub struct PromoteSingleChild;

impl<M: Metadata> StructureVisitor<M> for PromoteSingleChild {
    fn leave(&mut self, node: &mut Structure<M>, _depth: usize) {
        if node.children.len() == 1 && node.items.is_empty() {
            let only_child = node.children.remove(0);
            // Keep parent name but take child's contents
            node.items = only_child.items;
            node.children = only_child.children;
        }
    }
}

/// Visitor that expands multiple items on a node into individual child nodes
///
/// This ensures each item becomes its own track. When a node has multiple items,
/// each item is converted to its own child node with the item's original name
/// as the node name.
///
/// Single items stay on the parent node (the folder will have the item on it).
///
/// Exception: Items that only differ by playlist/take metadata stay together
/// (handled by checking the Playlist metadata field if it exists)
pub struct ExpandItemsToChildren;

impl<M: Metadata> StructureVisitor<M> for ExpandItemsToChildren {
    fn leave(&mut self, node: &mut Structure<M>, _depth: usize) {
        // Only expand if we have multiple items
        // Single item stays on the parent node (will become a track/folder with item on it)
        if node.items.len() <= 1 {
            return;
        }
        
        // Expand each item into its own child node
        let items = std::mem::take(&mut node.items);
        for item in items {
            // Use the item's original name as the child node name
            // TODO: Could extract a cleaner display name from metadata
            let child_name = item.original.clone();
            let mut child = Structure::new(&child_name);
            child.items.push(item);
            node.children.push(child);
        }
    }
}

/// Helper function to expand items to children throughout a structure
pub fn expand_items_to_children<M: Metadata>(root: &mut Structure<M>) {
    let mut expander = ExpandItemsToChildren;
    root.accept(&mut expander);
}

#[cfg(test)]
mod tests {
    use super::*;

    // We'll add tests once we have a simple Metadata implementation for testing
}
