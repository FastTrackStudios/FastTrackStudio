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
