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

// ============================================================================
// Structure manipulation methods for scoped sorting workflow
// ============================================================================

impl<M: Metadata> Structure<M> {
    /// Remove and return a child structure by name
    /// 
    /// Returns `Some(Structure)` if found and removed, `None` otherwise.
    pub fn remove_child(&mut self, name: &str) -> Option<Structure<M>> {
        if let Some(pos) = self.children.iter().position(|c| c.name == name) {
            Some(self.children.remove(pos))
        } else {
            None
        }
    }
    
    /// Extract all items from this structure recursively
    /// 
    /// Returns all items from this node and all descendants, clearing them from the structure.
    pub fn extract_all_items(&mut self) -> Vec<Item<M>> {
        let mut items = std::mem::take(&mut self.items);
        for child in &mut self.children {
            items.extend(child.extract_all_items());
        }
        items
    }
    
    /// Extract items from a specific child by name
    /// 
    /// Removes the child and returns all its items (recursively).
    /// Returns empty Vec if child not found.
    pub fn extract_items_from_child(&mut self, name: &str) -> Vec<Item<M>> {
        if let Some(mut child) = self.remove_child(name) {
            child.extract_all_items()
        } else {
            Vec::new()
        }
    }
    
    /// Merge another structure into this one
    /// 
    /// The incoming structure's children are merged with existing children by name.
    /// If a child with the same name exists, items and grandchildren are merged recursively.
    /// If no matching child exists, the incoming child is added as a new child.
    pub fn merge(&mut self, other: Structure<M>) {
        // Merge items at this level
        self.items.extend(other.items);
        
        // Merge children
        for other_child in other.children {
            if let Some(existing_child) = self.find_child_mut(&other_child.name) {
                // Recursively merge into existing child
                existing_child.merge(other_child);
            } else {
                // Add as new child
                self.children.push(other_child);
            }
        }
    }
    
    /// Merge a structure into a specific child path
    /// 
    /// The path is a list of child names to traverse (e.g., ["Guitars", "Electric Guitar"]).
    /// Creates intermediate nodes if they don't exist.
    /// The structure is merged at the final path location.
    /// 
    /// # Example
    /// ```ignore
    /// // Merge into root.Guitars.Electric Guitar
    /// root.merge_at_path(&["Guitars", "Electric Guitar"], sorted_structure);
    /// ```
    pub fn merge_at_path(&mut self, path: &[&str], structure: Structure<M>) {
        if path.is_empty() {
            // Merge directly into this node
            self.merge(structure);
            return;
        }
        
        let target_name = path[0];
        let remaining_path = &path[1..];
        
        // Find or create the target child
        let target = if let Some(pos) = self.children.iter().position(|c| c.name == target_name) {
            &mut self.children[pos]
        } else {
            // Create new intermediate node
            self.children.push(Structure::new(target_name));
            self.children.last_mut().unwrap()
        };
        
        // Recursively merge at remaining path
        target.merge_at_path(remaining_path, structure);
    }
    
    /// Find a child structure by path (recursive)
    /// 
    /// Returns a reference to the structure at the given path, or None if not found.
    pub fn find_at_path(&self, path: &[&str]) -> Option<&Structure<M>> {
        if path.is_empty() {
            return Some(self);
        }
        
        let target_name = path[0];
        let remaining_path = &path[1..];
        
        self.find_child(target_name)
            .and_then(|child| child.find_at_path(remaining_path))
    }
    
    /// Find a child structure by path (mutable, recursive)
    pub fn find_at_path_mut(&mut self, path: &[&str]) -> Option<&mut Structure<M>> {
        if path.is_empty() {
            return Some(self);
        }
        
        let target_name = path[0];
        let remaining_path = &path[1..];
        
        self.find_child_mut(target_name)
            .and_then(|child| child.find_at_path_mut(remaining_path))
    }
    
    /// Get all items at a specific path
    /// 
    /// Returns references to items at the given path location.
    pub fn items_at_path(&self, path: &[&str]) -> Option<&[Item<M>]> {
        self.find_at_path(path).map(|s| s.items.as_slice())
    }
}
