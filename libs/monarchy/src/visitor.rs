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

use crate::{Config, Group, Metadata, Structure, ToDisplayName};

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
            metadata_group_names: vec![
                "MultiMic",
                "SUM",
                "Section",
                "Layers",
                "Effect",
                "Arrangement",
                "Performer",
                "Channel",
            ],
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
        self.config
            .groups
            .iter()
            .any(|g| g.name.eq_ignore_ascii_case(name))
    }

    /// Check if a top-level category has patterns (can match items directly)
    /// Groups with patterns should keep their name when collapsing (Guitars, Vocals)
    /// Groups without patterns are just containers and should collapse to their children (Drums)
    fn top_level_has_patterns(&self, name: &str) -> bool {
        self.config
            .groups
            .iter()
            .find(|g| g.name.eq_ignore_ascii_case(name))
            .map(|g| !g.patterns.is_empty())
            .unwrap_or(false)
    }

    /// Find the parent group of a given group name
    fn find_parent_group(&self, child_name: &str) -> Option<&'a Group<M>> {
        fn search_parent<'a, M: Metadata>(
            parent: &'a Group<M>,
            child_name: &str,
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
        self.config
            .groups
            .iter()
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
        Self {
            helper: CollapseHelper::new(config),
        }
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
        Self {
            helper: CollapseHelper::new(config),
        }
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
                && self.helper.top_level_has_patterns(&only_child.name)
            {
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
        // Find if this node's group has a tagged collection (using let-else)
        let Some(group) = self.find_group(&node.name) else {
            return;
        };
        let Some(tagged_collection) = &group.tagged_collection else {
            return;
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
            folder_name: String::from("Unsorted"),
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
        node.children
            .retain(|child| !child.items.is_empty() || !child.children.is_empty());
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
/// each item is converted to its own child node. The node name is derived from
/// the item's metadata using `derive_display_name()` if the metadata implements
/// `ToDisplayName`, otherwise falls back to the original name.
///
/// Single items stay on the parent node (the folder will have the item on it).
///
/// Exception: Items that only differ by playlist/take metadata stay together
/// (handled by checking the Playlist metadata field if it exists)
pub struct ExpandItemsToChildren;

impl<M: Metadata + ToDisplayName> StructureVisitor<M> for ExpandItemsToChildren {
    fn leave(&mut self, node: &mut Structure<M>, _depth: usize) {
        // Expand items to children if:
        // 1. We have multiple items, OR
        // 2. We have items AND children (items need to become siblings of children)
        //
        // A single item with no children stays on the parent node
        // (will become a track/folder with item on it)
        let has_multiple_items = node.items.len() > 1;
        let has_items_and_children = !node.items.is_empty() && !node.children.is_empty();

        if !has_multiple_items && !has_items_and_children {
            return;
        }

        // Expand each item into its own child node
        let items = std::mem::take(&mut node.items);
        for item in items {
            // Use derive_display_name() to get a clean display name from metadata
            let child_name = item.derive_display_name();
            let mut child = Structure::new(&child_name);
            child.items.push(item);
            node.children.push(child);
        }
    }
}

/// Helper function to expand items to children throughout a structure
pub fn expand_items_to_children<M: Metadata + ToDisplayName>(root: &mut Structure<M>) {
    let mut expander = ExpandItemsToChildren;
    root.accept(&mut expander);
}

/// Visitor that cleans up display names by stripping redundant context
///
/// After `ExpandItemsToChildren` creates child nodes with full display names,
/// this visitor removes redundant parts based on the parent hierarchy.
///
/// For example:
/// - "Kick In" under "Kick" folder → "In"
/// - "Ed Crunch" under "Ed" folder → "Crunch"
/// - "D Kick" under "Drums" folder → "Kick"
///
/// Also handles duplicate detection and numbering:
/// - Two children named "Crunch" → "Crunch 1", "Crunch 2"
///
/// If stripping results in an empty string, falls back to the configured
/// fallback name (default: group name or "Main").
pub struct CleanupDisplayNames {
    /// Stack of ancestor names as we traverse
    context_stack: Vec<String>,
    /// Fallback name when stripping results in empty string
    fallback_name: String,
}

impl Default for CleanupDisplayNames {
    fn default() -> Self {
        Self {
            context_stack: Vec::new(),
            fallback_name: String::from("Main"),
        }
    }
}

impl CleanupDisplayNames {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set a custom fallback name for when stripping results in empty string
    pub fn with_fallback(mut self, fallback: impl Into<String>) -> Self {
        self.fallback_name = fallback.into();
        self
    }

    /// Clean up a filename to be suitable for display
    ///
    /// Performs the following cleanup:
    /// 1. Remove file extension (.wav, .aiff, .flac, etc.)
    /// 2. Remove leading track numbers (e.g., "49 " from "49 Organ Chords")
    /// 3. Remove trailing version/playlist suffixes (e.g., ".2_03", "_03", ".dup1")
    /// 4. Strip context words from ancestors
    fn cleanup_name(&self, name: &str) -> String {
        let mut cleaned = name.to_string();

        // Step 1: Remove file extension
        let extensions = [".wav", ".aiff", ".aif", ".flac", ".mp3", ".ogg", ".m4a"];
        for ext in extensions {
            if cleaned.to_lowercase().ends_with(ext) {
                cleaned = cleaned[..cleaned.len() - ext.len()].to_string();
                break;
            }
        }

        // Step 2: Remove trailing version/playlist suffixes
        // Patterns like "_03", ".2_03", ".dup1_03", ".01.R.05_03"
        // These are typically Pro Tools or DAW-specific suffixes
        let suffix_patterns = [
            // Full patterns with underscores
            r"[._]\d+[._]\d+$",           // .05_03, _05_03
            r"[._]dup\d*[._]?\d*$",       // .dup1_03, .dup1
            r"[._]\d+$",                  // _03, .2
            r"[._][RL][._]?\d*[._]?\d*$", // .R.05_03, .L
        ];

        // Simple suffix removal (not using regex for performance)
        // Remove patterns like "_03" at the end
        if let Some(idx) = cleaned.rfind('_') {
            let suffix = &cleaned[idx + 1..];
            if suffix.chars().all(|c| c.is_ascii_digit()) {
                cleaned = cleaned[..idx].to_string();
            }
        }

        // Remove patterns like ".2" at the end (but not things like "DX7")
        if let Some(idx) = cleaned.rfind('.') {
            let suffix = &cleaned[idx + 1..];
            // Only remove if it's a simple number or dup pattern
            if suffix.chars().all(|c| c.is_ascii_digit()) || suffix.starts_with("dup") {
                cleaned = cleaned[..idx].to_string();
            }
        }

        // Recursively clean more suffixes
        // Handle cases like "Snare.dup1.05_03" -> "Snare"
        loop {
            let before = cleaned.clone();

            // Remove trailing _NN
            if let Some(idx) = cleaned.rfind('_') {
                let suffix = &cleaned[idx + 1..];
                if suffix.chars().all(|c| c.is_ascii_digit()) {
                    cleaned = cleaned[..idx].to_string();
                    continue;
                }
            }

            // Remove trailing .NN or .dupN
            if let Some(idx) = cleaned.rfind('.') {
                let suffix = &cleaned[idx + 1..];
                if suffix.chars().all(|c| c.is_ascii_digit())
                    || suffix.starts_with("dup")
                    || suffix == "R"
                    || suffix == "L"
                {
                    cleaned = cleaned[..idx].to_string();
                    continue;
                }
            }

            if before == cleaned {
                break;
            }
        }

        // Step 3: Remove leading track numbers (e.g., "49 " from "49 Organ Chords")
        let parts: Vec<&str> = cleaned.split_whitespace().collect();
        if parts.len() > 1 {
            if let Some(first) = parts.first() {
                // Check if first word is just a number (track number)
                if first.chars().all(|c| c.is_ascii_digit()) {
                    cleaned = parts[1..].join(" ");
                }
            }
        }

        // Step 4: Strip context words from ancestors
        self.strip_context(&cleaned)
    }

    /// Strip context words from a name based on ancestor names
    ///
    /// Removes words from the name that appear in any ancestor's name.
    /// Case-insensitive comparison.
    ///
    /// If stripping would leave only numbers or short prefixes (e.g., "1", "V 2"),
    /// the last meaningful word from the original name is preserved to maintain
    /// meaningful context.
    fn strip_context(&self, name: &str) -> String {
        let words: Vec<&str> = name.split_whitespace().collect();

        // Build a set of all context words from ancestors (lowercase for comparison)
        let context_words: std::collections::HashSet<String> = self
            .context_stack
            .iter()
            .flat_map(|ancestor| ancestor.split_whitespace())
            .map(|w| w.to_lowercase())
            .collect();

        // Remove words that appear in context
        let mut filtered: Vec<&str> = words
            .iter()
            .filter(|word| !context_words.contains(&word.to_lowercase()))
            .copied()
            .collect();

        // Helper to check if a word is "meaningful" (not just a number or short prefix)
        // Short prefixes are typically 1-4 uppercase letters like "V", "D", "GTR", "BGV"
        let is_meaningful = |w: &str| {
            // Not all digits
            if w.chars().all(|c| c.is_ascii_digit()) {
                return false;
            }
            // Not a short all-caps prefix (1-4 chars)
            if w.len() <= 4 && w.chars().all(|c| c.is_uppercase() || c.is_ascii_digit()) {
                return false;
            }
            true
        };

        // Check if a word looks like a prefix (short all-caps)
        let is_prefix =
            |w: &str| w.len() <= 4 && w.chars().all(|c| c.is_uppercase() || c.is_ascii_digit());

        // Check if we're left with only non-meaningful words (numbers/prefixes)
        let has_meaningful_word = filtered.iter().any(|w| is_meaningful(w));

        if !has_meaningful_word && !filtered.is_empty() {
            // We only have numbers/prefixes - find the last meaningful word from original
            // to preserve some meaningful context
            if let Some(last_meaningful) = words.iter().rev().find(|w| is_meaningful(w)) {
                // Remove prefixes from filtered, keep only numbers
                filtered.retain(|w| !is_prefix(w));
                // Insert the meaningful word before the numbers
                filtered.insert(0, last_meaningful);
            }
        }

        filtered.join(" ")
    }

    /// Find a meaningful fallback name from the context stack
    ///
    /// When a cleaned name is empty, we need a fallback. Instead of using
    /// the immediate parent's name (which might be a collection like "SUM"),
    /// we look through the context stack to find the most appropriate name.
    ///
    /// Generic collection names are skipped in favor of actual group names.
    fn find_fallback_name(&self, parent_name: &str) -> String {
        // Names that are generic collections, not meaningful fallbacks
        const COLLECTION_NAMES: &[&str] = &["sum", "main", "sub", "aux", "bus", "group"];

        let parent_lower = parent_name.to_lowercase();

        // If parent name is not a generic collection, use it
        if !COLLECTION_NAMES.contains(&parent_lower.as_str()) {
            return parent_name.to_string();
        }

        // Search context stack from end to beginning for a meaningful name
        // Skip entries that are prefixed display names (contain spaces with prefix-like patterns)
        for name in self.context_stack.iter().rev() {
            let name_lower = name.to_lowercase();

            // Skip generic collection names
            if COLLECTION_NAMES.contains(&name_lower.as_str()) {
                continue;
            }

            // Skip prefixed display names (like "D Snare", "GTR E Electric")
            // These contain the prefix + group name, we want just the group name
            if name.contains(' ') {
                // Check if first word looks like a prefix (all caps or short)
                let first_word = name.split_whitespace().next().unwrap_or("");
                if first_word.len() <= 4
                    && first_word
                        .chars()
                        .all(|c| c.is_uppercase() || c.is_ascii_digit())
                {
                    continue;
                }
            }

            return name.clone();
        }

        // Last resort: use the fallback_name or parent_name
        if !self.fallback_name.is_empty() {
            self.fallback_name.clone()
        } else {
            parent_name.to_string()
        }
    }

    /// Convert a string to title case (capitalize first letter of each word)
    ///
    /// Examples:
    /// - "chords" -> "Chords"
    /// - "quad" -> "Quad"
    /// - "lead vox" -> "Lead Vox"
    /// - "DBL" -> "DBL" (preserves all-caps)
    fn title_case(s: &str) -> String {
        s.split_whitespace()
            .map(|word| {
                // If word is all uppercase (like "DBL", "OH"), preserve it
                if word.chars().all(|c| c.is_uppercase() || c.is_ascii_digit()) {
                    word.to_string()
                } else {
                    // Otherwise, capitalize first letter and lowercase the rest
                    let mut chars = word.chars();
                    match chars.next() {
                        None => String::new(),
                        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                    }
                }
            })
            .collect::<Vec<_>>()
            .join(" ")
    }

    /// Number duplicate names in a list of children
    ///
    /// If multiple children have the same name after stripping,
    /// append " 1", " 2", etc.
    fn number_duplicates(children: &mut [Structure<impl Metadata>]) {
        use std::collections::HashMap;

        // Count occurrences of each name
        let mut name_counts: HashMap<String, usize> = HashMap::new();
        for child in children.iter() {
            *name_counts.entry(child.name.clone()).or_insert(0) += 1;
        }

        // Track which names need numbering (count > 1)
        let duplicates: std::collections::HashSet<String> = name_counts
            .into_iter()
            .filter(|(_, count)| *count > 1)
            .map(|(name, _)| name)
            .collect();

        // Apply numbering to duplicates
        let mut name_counters: HashMap<String, usize> = HashMap::new();
        for child in children.iter_mut() {
            if duplicates.contains(&child.name) {
                let counter = name_counters.entry(child.name.clone()).or_insert(0);
                *counter += 1;
                child.name = format!("{} {}", child.name, counter);
            }
        }
    }

    /// Group sibling children that share a common prefix with numbers
    ///
    /// For example, if children are ["Lead", "Middle Bridge 1", "Middle Bridge 2", "Middle Bridge 3"],
    /// groups the numbered ones into a folder:
    /// ["Lead", "Middle Bridge" -> ["Middle Bridge 1", "Middle Bridge 2", "Middle Bridge 3"]]
    ///
    /// Only groups if:
    /// 1. There are at least 2 items with the same prefix
    /// 2. There are OTHER different siblings (otherwise grouping is unnecessary)
    /// 3. The prefix is NOT redundant with ancestor names (avoids Snare/SUM/Snare nesting)
    fn group_numbered_siblings(
        children: &mut Vec<Structure<impl Metadata + Clone>>,
        context_stack: &[String],
    ) {
        use std::collections::HashMap;

        if children.len() < 2 {
            return;
        }

        // Extract prefix from names like "Middle Bridge 1" -> "Middle Bridge"
        let extract_prefix = |name: &str| -> Option<(String, String)> {
            let name = name.trim();
            // Find the last word - if it's a number, the rest is the prefix
            let words: Vec<&str> = name.split_whitespace().collect();
            if words.len() >= 2 {
                let last = words.last().unwrap();
                if last.chars().all(|c| c.is_ascii_digit()) {
                    let prefix = words[..words.len() - 1].join(" ");
                    return Some((prefix, last.to_string()));
                }
            }
            None
        };

        // Check if a prefix would create redundant nesting
        // e.g., don't create "Snare/" folder when we're inside "Snare/SUM/"
        let is_redundant_with_ancestors = |prefix: &str| -> bool {
            context_stack
                .iter()
                .any(|ancestor| ancestor.eq_ignore_ascii_case(prefix))
        };

        // Count occurrences of each prefix
        let mut prefix_counts: HashMap<String, Vec<usize>> = HashMap::new();
        for (idx, child) in children.iter().enumerate() {
            if let Some((prefix, _)) = extract_prefix(&child.name) {
                // Skip prefixes that would create redundant nesting
                if !is_redundant_with_ancestors(&prefix) {
                    prefix_counts.entry(prefix).or_default().push(idx);
                }
            }
        }

        // Find prefixes that appear 2+ times
        let groupable_prefixes: Vec<(String, Vec<usize>)> = prefix_counts
            .into_iter()
            .filter(|(_, indices)| indices.len() >= 2)
            .collect();

        // Only group if there are OTHER children that won't be grouped
        // (i.e., the grouping provides organizational benefit)
        let total_groupable: usize = groupable_prefixes.iter().map(|(_, v)| v.len()).sum();
        if total_groupable == children.len() {
            // All children would be grouped - no benefit, skip
            return;
        }

        if groupable_prefixes.is_empty() {
            return;
        }

        // Create new children list with grouped items
        let mut new_children: Vec<Structure<_>> = Vec::new();
        let mut grouped_indices: std::collections::HashSet<usize> =
            std::collections::HashSet::new();

        // First, create folder structures for each prefix group
        for (prefix, indices) in &groupable_prefixes {
            let mut folder: Structure<_> = Structure::new(prefix);
            for &idx in indices {
                grouped_indices.insert(idx);
                let child = &children[idx];
                // Child keeps its full name (e.g., "Middle Bridge 1")
                // since it provides context within the folder
                folder.children.push(child.clone());
            }
            new_children.push(folder);
        }

        // Then add non-grouped children
        for (idx, child) in children.iter().enumerate() {
            if !grouped_indices.contains(&idx) {
                new_children.push(child.clone());
            }
        }

        *children = new_children;
    }
}

impl<M: Metadata> StructureVisitor<M> for CleanupDisplayNames {
    fn enter(&mut self, node: &mut Structure<M>, _depth: usize) -> bool {
        // Push both name and display_name onto the context stack before visiting children
        // The display_name includes prefixes (e.g., "D Kick", "GTR EG Electric")
        // which should also be stripped from child names
        self.context_stack.push(node.name.clone());
        if node.display_name != node.name {
            self.context_stack.push(node.display_name.clone());
        }
        true
    }

    fn leave(&mut self, node: &mut Structure<M>, _depth: usize) {
        // Helper to check if a name is ONLY numbers (no meaningful text at all)
        // This is more conservative - we only add parent context when there's
        // truly nothing meaningful, like "1", "2", "3"
        let is_only_numbers = |name: &str| {
            // All words must be purely numeric
            name.split_whitespace()
                .all(|word| word.chars().all(|c| c.is_ascii_digit()))
        };

        // Process children's names before popping context
        for child in &mut node.children {
            let cleaned = self.cleanup_name(&child.name);
            if cleaned.is_empty() {
                // Fallback: find a meaningful name from context stack
                // Skip generic collection names like "SUM" - look for the last group name
                // that represents an actual instrument/category
                child.name = self.find_fallback_name(&node.name);
            } else if cleaned.eq_ignore_ascii_case("Main") {
                // "Main" is a generic default - use parent name instead
                // E.g., under "Guiro" folder, "Main" becomes "Guiro"
                child.name = Self::title_case(&node.name);
            } else if is_only_numbers(&cleaned) {
                // Cleaned name is ONLY numbers (e.g., "1", "2 3")
                // Prefix with the parent's name for context
                // E.g., under "Outro" folder, "1" becomes "Outro 1"
                let parent_name = &node.name;
                // Don't use generic parent names
                const GENERIC_NAMES: &[&str] = &["SUM", "Main", "Sub", "Aux", "Bus", "Group"];
                if !GENERIC_NAMES
                    .iter()
                    .any(|g| g.eq_ignore_ascii_case(parent_name))
                {
                    child.name = format!("{} {}", Self::title_case(parent_name), cleaned);
                } else {
                    // Generic parent, try to find a better name from context
                    child.name = format!("{} {}", self.find_fallback_name(parent_name), cleaned);
                }
            } else {
                // Apply title case to cleaned name (e.g., "chords" -> "Chords")
                child.name = Self::title_case(&cleaned);
            }
        }

        // Number any duplicate names among children
        Self::number_duplicates(&mut node.children);

        // Group siblings that share the same prefix + number pattern
        // E.g., "Middle Bridge 1", "Middle Bridge 2" become folder "Middle Bridge" with children "1", "2"
        // But only if there are OTHER different children (otherwise no need to group)
        // Pass context stack to avoid redundant nesting (e.g., Snare/SUM/Snare)
        Self::group_numbered_siblings(&mut node.children, &self.context_stack);

        // Pop this node from context stack (pop display_name first if it was pushed)
        if node.display_name != node.name {
            self.context_stack.pop();
        }
        self.context_stack.pop();
    }
}

/// Helper function to clean up display names throughout a structure
///
/// This should be called after `expand_items_to_children()` to strip
/// redundant context from the generated display names.
pub fn cleanup_display_names<M: Metadata>(root: &mut Structure<M>) {
    let mut cleaner = CleanupDisplayNames::new();
    root.accept(&mut cleaner);
}

/// Helper function to clean up display names with a custom fallback
pub fn cleanup_display_names_with_fallback<M: Metadata>(root: &mut Structure<M>, fallback: &str) {
    let mut cleaner = CleanupDisplayNames::new().with_fallback(fallback);
    root.accept(&mut cleaner);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metadata::FieldName;
    use serde::{Deserialize, Serialize};

    // region:    --- Test Fixtures

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    #[derive(Clone, Debug, Default)]
    struct TestMetadata {
        group: Option<String>,
        multi_mic: Option<String>,
    }

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    enum TestMetadataField {
        Group,
        MultiMic,
    }

    impl FieldName for TestMetadataField {
        fn name(&self) -> &'static str {
            match self {
                TestMetadataField::Group => "Group",
                TestMetadataField::MultiMic => "MultiMic",
            }
        }
    }

    #[derive(Clone, Debug)]
    enum TestMetadataValue {
        Group(String),
        MultiMic(String),
    }

    impl crate::Metadata for TestMetadata {
        type Field = TestMetadataField;
        type Value = TestMetadataValue;

        fn get(&self, field: &Self::Field) -> Option<Self::Value> {
            match field {
                TestMetadataField::Group => self.group.clone().map(TestMetadataValue::Group),
                TestMetadataField::MultiMic => {
                    self.multi_mic.clone().map(TestMetadataValue::MultiMic)
                }
            }
        }

        fn set(&mut self, field: Self::Field, value: Self::Value) {
            match (field, value) {
                (TestMetadataField::Group, TestMetadataValue::Group(v)) => self.group = Some(v),
                (TestMetadataField::MultiMic, TestMetadataValue::MultiMic(v)) => {
                    self.multi_mic = Some(v)
                }
                _ => {}
            }
        }

        fn fields() -> Vec<Self::Field> {
            vec![TestMetadataField::Group, TestMetadataField::MultiMic]
        }
    }

    fn create_test_item(name: &str) -> crate::Item<TestMetadata> {
        crate::Item {
            id: name.to_string(),
            original: name.to_string(),
            metadata: TestMetadata::default(),
            matched_groups: vec![],
        }
    }

    // endregion: --- Test Fixtures

    // region:    --- RemoveEmptyNodes Tests

    mod remove_empty_nodes {
        use super::*;

        #[test]
        fn removes_empty_children() -> Result<()> {
            // -- Setup & Fixtures
            let mut root: Structure<TestMetadata> = Structure::new("Root");
            root.children.push(Structure::new("Empty"));
            let mut with_item = Structure::new("WithItem");
            with_item.items.push(create_test_item("item1"));
            root.children.push(with_item);

            // -- Exec
            root.accept(&mut RemoveEmptyNodes);

            // -- Check
            assert_eq!(root.children.len(), 1);
            assert_eq!(root.children[0].name, "WithItem");

            Ok(())
        }

        #[test]
        fn keeps_nodes_with_children() -> Result<()> {
            // -- Setup & Fixtures
            let mut root: Structure<TestMetadata> = Structure::new("Root");
            let mut parent = Structure::new("Parent");
            let mut child = Structure::new("Child");
            child.items.push(create_test_item("item1"));
            parent.children.push(child);
            root.children.push(parent);

            // -- Exec
            root.accept(&mut RemoveEmptyNodes);

            // -- Check
            assert_eq!(root.children.len(), 1);
            assert_eq!(root.children[0].name, "Parent");

            Ok(())
        }
    }

    // endregion: --- RemoveEmptyNodes Tests

    // region:    --- PromoteSingleChild Tests

    mod promote_single_child {
        use super::*;

        #[test]
        fn promotes_single_child_content() -> Result<()> {
            // -- Setup & Fixtures
            // Structure: Root -> Parent -> Child (with item1)
            // Both Root and Parent have single children with no items, so both should be promoted
            let mut root: Structure<TestMetadata> = Structure::new("Root");
            let mut parent: Structure<TestMetadata> = Structure::new("Parent");
            let mut child: Structure<TestMetadata> = Structure::new("Child");
            child.items.push(create_test_item("item1"));
            parent.children.push(child);
            root.children.push(parent);

            // -- Exec
            root.accept(&mut PromoteSingleChild);

            // -- Check
            // The visitor works bottom-up:
            // 1. leave(Child) - has item, no promotion
            // 2. leave(Parent) - has 1 child (Child) + 0 items -> promotes Child's content into Parent
            //    Parent now has: items=[item1], children=[]
            // 3. leave(Root) - has 1 child (Parent) + 0 items -> promotes Parent's content into Root
            //    Root now has: items=[item1], children=[]
            assert_eq!(root.children.len(), 0);
            assert_eq!(root.items.len(), 1);

            Ok(())
        }

        #[test]
        fn does_not_promote_when_parent_has_items() -> Result<()> {
            // -- Setup & Fixtures
            // If the parent has its own items, don't promote the child into parent
            // But root can still promote the parent into root
            let mut root: Structure<TestMetadata> = Structure::new("Root");
            let mut parent: Structure<TestMetadata> = Structure::new("Parent");
            parent.items.push(create_test_item("parent_item"));
            parent.children.push(Structure::new("Child"));
            root.children.push(parent);

            // -- Exec
            root.accept(&mut PromoteSingleChild);

            // -- Check
            // 1. leave(Child) - no items, no children -> no change
            // 2. leave(Parent) - has items, so its child (Child) is NOT promoted
            // 3. leave(Root) - has 1 child (Parent) + 0 items -> promotes Parent into Root
            //    Root now has: items=[parent_item], children=[Child]
            assert_eq!(root.items.len(), 1);
            assert_eq!(root.children.len(), 1);
            assert_eq!(root.children[0].name, "Child");

            Ok(())
        }

        #[test]
        fn does_not_promote_when_multiple_children() -> Result<()> {
            // -- Setup & Fixtures
            let mut root: Structure<TestMetadata> = Structure::new("Root");
            let mut parent: Structure<TestMetadata> = Structure::new("Parent");
            parent.children.push(Structure::new("Child1"));
            parent.children.push(Structure::new("Child2"));
            root.children.push(parent);

            // -- Exec
            root.accept(&mut PromoteSingleChild);

            // -- Check
            // Parent has 2 children, so it won't promote into Root (wait, Parent doesn't have items)
            // Actually Root has 1 child (Parent) + 0 items -> promotes Parent into Root
            // Root gets Parent's children: [Child1, Child2]
            assert_eq!(root.children.len(), 2);
            assert_eq!(root.children[0].name, "Child1");
            assert_eq!(root.children[1].name, "Child2");

            Ok(())
        }
    }

    // endregion: --- PromoteSingleChild Tests

    // region:    --- CollectUnsorted Tests

    mod collect_unsorted {
        use super::*;

        #[test]
        fn collects_items_when_node_has_children() -> Result<()> {
            // -- Setup & Fixtures
            let mut root: Structure<TestMetadata> = Structure::new("Root");
            root.items.push(create_test_item("unsorted1"));
            root.children.push(Structure::new("Child"));

            // -- Exec
            // Use the helper function that adds the Unsorted folder
            let count = collect_unsorted_to_root(&mut root);

            // -- Check
            assert!(root.items.is_empty());
            assert_eq!(count, 1);
            assert_eq!(root.children.len(), 2); // Child + Unsorted folder
            assert_eq!(root.children[1].name, "Unsorted");
            assert_eq!(root.children[1].items.len(), 1);

            Ok(())
        }

        #[test]
        fn does_not_collect_when_no_children() -> Result<()> {
            // -- Setup & Fixtures
            let mut root: Structure<TestMetadata> = Structure::new("Root");
            root.items.push(create_test_item("item1"));

            // -- Exec
            let mut collector = CollectUnsorted::new();
            root.accept(&mut collector);

            // -- Check
            // No children, so items stay on root
            assert_eq!(root.items.len(), 1);
            assert_eq!(root.children.len(), 0);

            Ok(())
        }
    }

    // endregion: --- CollectUnsorted Tests

    // region:    --- CleanupDisplayNames Tests

    mod cleanup_display_names {
        use super::*;

        #[test]
        fn removes_file_extension() -> Result<()> {
            // -- Setup & Fixtures
            let mut root: Structure<TestMetadata> = Structure::new("Root");
            root.children.push(Structure::new("Snare.wav"));

            // -- Exec
            let mut cleaner = CleanupDisplayNames::new();
            root.accept(&mut cleaner);

            // -- Check
            assert_eq!(root.children[0].name, "Snare");

            Ok(())
        }

        #[test]
        fn removes_track_number_prefix() -> Result<()> {
            // -- Setup & Fixtures
            let mut root: Structure<TestMetadata> = Structure::new("Root");
            root.children.push(Structure::new("49 Organ Chords"));

            // -- Exec
            let mut cleaner = CleanupDisplayNames::new();
            root.accept(&mut cleaner);

            // -- Check
            assert_eq!(root.children[0].name, "Organ Chords");

            Ok(())
        }

        #[test]
        fn strips_context_from_parent() -> Result<()> {
            // -- Setup & Fixtures
            let mut root: Structure<TestMetadata> = Structure::new("Drums");
            root.children.push(Structure::new("Drums Snare"));

            // -- Exec
            let mut cleaner = CleanupDisplayNames::new();
            root.accept(&mut cleaner);

            // -- Check
            assert_eq!(root.children[0].name, "Snare");

            Ok(())
        }

        #[test]
        fn numbers_duplicate_names() -> Result<()> {
            // -- Setup & Fixtures
            let mut root: Structure<TestMetadata> = Structure::new("Root");
            root.children.push(Structure::new("Snare"));
            root.children.push(Structure::new("Snare"));
            root.children.push(Structure::new("Snare"));

            // -- Exec
            let mut cleaner = CleanupDisplayNames::new();
            root.accept(&mut cleaner);

            // -- Check
            assert_eq!(root.children[0].name, "Snare 1");
            assert_eq!(root.children[1].name, "Snare 2");
            assert_eq!(root.children[2].name, "Snare 3");

            Ok(())
        }

        #[test]
        fn preserves_meaningful_word_when_only_numbers_remain() -> Result<()> {
            // -- Setup & Fixtures
            // Simulates: Outro folder containing "V Outro 1", "V Outro 2"
            // After stripping "V" and "Outro" from context, should keep "Outro 1" not just "1"
            let mut root: Structure<TestMetadata> = Structure::new("Root");
            let mut outro = Structure::new("Outro");
            outro.children.push(Structure::new("V Outro 1"));
            outro.children.push(Structure::new("V Outro 2"));
            outro.children.push(Structure::new("V Outro 3"));
            root.children.push(outro);

            // -- Exec
            let mut cleaner = CleanupDisplayNames::new();
            root.accept(&mut cleaner);

            // -- Check
            // Should preserve "Outro" as meaningful context, not just "1", "2", "3"
            let outro_folder = &root.children[0];
            assert_eq!(outro_folder.children[0].name, "Outro 1");
            assert_eq!(outro_folder.children[1].name, "Outro 2");
            assert_eq!(outro_folder.children[2].name, "Outro 3");

            Ok(())
        }

        #[test]
        fn prefixes_numbers_with_parent_name() -> Result<()> {
            // -- Setup & Fixtures
            // Simulates: Outro folder containing just "1", "2", "3"
            // (when display names don't include section because it's used for hierarchy)
            let mut root: Structure<TestMetadata> = Structure::new("Root");
            let mut outro = Structure::new("Outro");
            outro.children.push(Structure::new("1"));
            outro.children.push(Structure::new("2"));
            outro.children.push(Structure::new("3"));
            root.children.push(outro);

            // -- Exec
            let mut cleaner = CleanupDisplayNames::new();
            root.accept(&mut cleaner);

            // -- Check
            // Children that are just numbers should get prefixed with parent name
            let outro_folder = &root.children[0];
            assert_eq!(outro_folder.children[0].name, "Outro 1");
            assert_eq!(outro_folder.children[1].name, "Outro 2");
            assert_eq!(outro_folder.children[2].name, "Outro 3");

            Ok(())
        }
    }

    // endregion: --- CleanupDisplayNames Tests
}
