//! Preset Registry - Global registry with fallback resolution
//!
//! Provides a centralized registry for all presets with:
//! - Category-based lookup with fallback chain resolution
//! - Tree structure generation for UI browsing
//! - Efficient indexing by category level

use std::collections::HashMap;

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use super::category::{BaseTone, Genre, PresetCategory};
use super::preset::Preset;

/// Tree node for the preset browser UI
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct CategoryNode {
    /// Node name (e.g., "Clean", "Blues", "John Mayer")
    pub name: String,
    /// Full path to this node
    pub path: Vec<String>,
    /// Hierarchy level (1-5)
    pub level: u8,
    /// Child category nodes
    pub children: Vec<CategoryNode>,
    /// Presets directly at this level
    pub presets: Vec<PresetSummary>,
    /// Total preset count (including children)
    pub total_count: usize,
}

/// Lightweight preset info for tree display
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct PresetSummary {
    pub id: Uuid,
    pub name: String,
    pub category: PresetCategory,
    pub snapshot_count: usize,
}

impl CategoryNode {
    /// Create an empty root node
    pub fn empty() -> Self {
        Self {
            name: "Presets".to_string(),
            path: Vec::new(),
            level: 0,
            children: Vec::new(),
            presets: Vec::new(),
            total_count: 0,
        }
    }

    /// Create a new category node
    pub fn new(name: impl Into<String>, path: Vec<String>, level: u8) -> Self {
        Self {
            name: name.into(),
            path,
            level,
            children: Vec::new(),
            presets: Vec::new(),
            total_count: 0,
        }
    }

    /// Find or create a child node by name
    fn get_or_create_child(&mut self, name: &str, level: u8) -> &mut CategoryNode {
        let idx = self.children.iter().position(|c| c.name == name);

        if let Some(idx) = idx {
            &mut self.children[idx]
        } else {
            let mut path = self.path.clone();
            path.push(name.to_string());
            self.children.push(CategoryNode::new(name, path, level));
            self.children.last_mut().unwrap()
        }
    }

    /// Update total counts recursively
    fn update_counts(&mut self) -> usize {
        let child_count: usize = self
            .children
            .iter_mut()
            .map(|c| c.update_counts())
            .sum();
        self.total_count = self.presets.len() + child_count;
        self.total_count
    }

    /// Sort children alphabetically
    fn sort_recursive(&mut self) {
        self.children.sort_by(|a, b| a.name.cmp(&b.name));
        for child in &mut self.children {
            child.sort_recursive();
        }
    }
}

impl Default for CategoryNode {
    fn default() -> Self {
        Self::empty()
    }
}

/// Global registry for all presets with fallback resolution
#[derive(Debug, Clone, Default)]
pub struct PresetRegistry {
    /// All presets by ID
    presets: HashMap<Uuid, Preset>,
    /// Index by exact category
    by_category: HashMap<PresetCategory, Vec<Uuid>>,
    /// Index by base tone (Level 1)
    by_base_tone: HashMap<BaseTone, Vec<Uuid>>,
    /// Index by genre (Level 2)
    by_genre: HashMap<(BaseTone, Genre), Vec<Uuid>>,
}

impl PresetRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a preset to the registry
    pub fn add(&mut self, preset: Preset) {
        let id = preset.id;
        let category = preset.category.clone();

        // Index by base tone
        self.by_base_tone
            .entry(category.base_tone)
            .or_default()
            .push(id);

        // Index by genre if present
        if let Some(genre) = &category.genre {
            self.by_genre
                .entry((category.base_tone, genre.clone()))
                .or_default()
                .push(id);
        }

        // Index by exact category
        self.by_category.entry(category).or_default().push(id);

        // Store the preset
        self.presets.insert(id, preset);
    }

    /// Remove a preset from the registry
    pub fn remove(&mut self, id: Uuid) -> Option<Preset> {
        let preset = self.presets.remove(&id)?;
        let category = &preset.category;

        // Remove from indices
        if let Some(ids) = self.by_base_tone.get_mut(&category.base_tone) {
            ids.retain(|&i| i != id);
        }

        if let Some(genre) = &category.genre {
            if let Some(ids) = self.by_genre.get_mut(&(category.base_tone, genre.clone())) {
                ids.retain(|&i| i != id);
            }
        }

        if let Some(ids) = self.by_category.get_mut(category) {
            ids.retain(|&i| i != id);
        }

        Some(preset)
    }

    /// Get a preset by ID
    pub fn get(&self, id: Uuid) -> Option<&Preset> {
        self.presets.get(&id)
    }

    /// Get a mutable preset by ID
    pub fn get_mut(&mut self, id: Uuid) -> Option<&mut Preset> {
        self.presets.get_mut(&id)
    }

    /// Get all presets
    pub fn all(&self) -> impl Iterator<Item = &Preset> {
        self.presets.values()
    }

    /// Get presets by exact category
    pub fn get_by_category(&self, category: &PresetCategory) -> Vec<&Preset> {
        self.by_category
            .get(category)
            .map(|ids| ids.iter().filter_map(|id| self.presets.get(id)).collect())
            .unwrap_or_default()
    }

    /// Get presets by base tone (Level 1)
    pub fn get_by_base_tone(&self, base_tone: BaseTone) -> Vec<&Preset> {
        self.by_base_tone
            .get(&base_tone)
            .map(|ids| ids.iter().filter_map(|id| self.presets.get(id)).collect())
            .unwrap_or_default()
    }

    /// Get presets by base tone and genre (Level 2)
    pub fn get_by_genre(&self, base_tone: BaseTone, genre: Genre) -> Vec<&Preset> {
        self.by_genre
            .get(&(base_tone, genre))
            .map(|ids| ids.iter().filter_map(|id| self.presets.get(id)).collect())
            .unwrap_or_default()
    }

    /// Resolve a preset by category with fallback chain
    ///
    /// Tries to find a preset matching the exact category first,
    /// then walks up the fallback chain until a match is found.
    ///
    /// # Example
    /// ```ignore
    /// // Request "Gravity Lead" (Level 5)
    /// // If not found, try "John Mayer Lead" (Level 4)
    /// // If not found, try "Blues Lead" (Level 2)
    /// // If not found, try "Lead" (Level 1)
    /// let preset = registry.resolve(&category);
    /// ```
    pub fn resolve(&self, category: &PresetCategory) -> Option<&Preset> {
        // Try exact match first
        if let Some(preset) = self.get_by_category(category).first() {
            return Some(*preset);
        }

        // Walk the fallback chain
        for fallback in category.fallback_chain().iter().skip(1) {
            if let Some(preset) = self.get_by_category(fallback).first() {
                return Some(*preset);
            }
        }

        None
    }

    /// Resolve a preset and return with the resolution info
    ///
    /// Returns the preset along with the category that was actually matched
    /// (useful for showing "using fallback" in UI).
    pub fn resolve_with_info(
        &self,
        category: &PresetCategory,
    ) -> Option<(&Preset, PresetCategory, bool)> {
        // Try exact match first
        if let Some(preset) = self.get_by_category(category).first() {
            return Some((*preset, category.clone(), false));
        }

        // Walk the fallback chain
        for fallback in category.fallback_chain().iter().skip(1) {
            if let Some(preset) = self.get_by_category(fallback).first() {
                return Some((*preset, fallback.clone(), true));
            }
        }

        None
    }

    /// Get the tree structure for UI browsing
    pub fn get_tree(&self) -> CategoryNode {
        let mut root = CategoryNode::empty();

        for preset in self.presets.values() {
            let path = preset.category.tree_path();
            let summary = PresetSummary {
                id: preset.id,
                name: preset.name.clone(),
                category: preset.category.clone(),
                snapshot_count: preset.snapshots.len(),
            };

            // Navigate/create path to the correct node
            let mut current = &mut root;

            for (depth, segment) in path.iter().enumerate() {
                let level = (depth + 1) as u8;
                current = current.get_or_create_child(segment, level);
            }

            // Add preset at the leaf node
            current.presets.push(summary);
        }

        // Update counts and sort
        root.update_counts();
        root.sort_recursive();

        root
    }

    /// Get presets that match a search query
    pub fn search(&self, query: &str) -> Vec<&Preset> {
        let query_lower = query.to_lowercase();

        self.presets
            .values()
            .filter(|p| {
                p.name.to_lowercase().contains(&query_lower)
                    || p.category.display_name().to_lowercase().contains(&query_lower)
            })
            .collect()
    }

    /// Get the total number of presets
    pub fn len(&self) -> usize {
        self.presets.len()
    }

    /// Check if the registry is empty
    pub fn is_empty(&self) -> bool {
        self.presets.is_empty()
    }

    /// Clear all presets
    pub fn clear(&mut self) {
        self.presets.clear();
        self.by_category.clear();
        self.by_base_tone.clear();
        self.by_genre.clear();
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::super::category::{Archetype, SongReference};
    use super::*;

    fn create_test_preset(name: &str, category: PresetCategory) -> Preset {
        Preset::new(name, category)
    }

    #[test]
    fn test_registry_add_and_get() {
        let mut registry = PresetRegistry::new();

        let preset = create_test_preset("Test Clean", PresetCategory::generic(BaseTone::Clean));
        let id = preset.id;

        registry.add(preset);

        assert!(registry.get(id).is_some());
        assert_eq!(registry.len(), 1);
    }

    #[test]
    fn test_fallback_resolution() {
        let mut registry = PresetRegistry::new();

        // Add only generic "Lead" preset
        let generic_lead = create_test_preset("Lead", PresetCategory::generic(BaseTone::Lead));
        registry.add(generic_lead);

        // Request "Gravity Lead" (Level 5) - should fall back to generic "Lead"
        let category = PresetCategory::song(
            BaseTone::Lead,
            Genre::Blues,
            Archetype::new("John Mayer"),
            SongReference::new("Gravity"),
        );

        let resolved = registry.resolve(&category);
        assert!(resolved.is_some());
        assert_eq!(resolved.unwrap().name, "Lead");
    }

    #[test]
    fn test_fallback_resolution_stops_at_closest() {
        let mut registry = PresetRegistry::new();

        // Add generic "Lead" and "Blues Lead"
        registry.add(create_test_preset(
            "Lead",
            PresetCategory::generic(BaseTone::Lead),
        ));
        registry.add(create_test_preset(
            "Blues Lead",
            PresetCategory::genre(BaseTone::Lead, Genre::Blues),
        ));

        // Request "John Mayer Lead" - should resolve to "Blues Lead" (closer match)
        let category = PresetCategory::archetype(
            BaseTone::Lead,
            Genre::Blues,
            Archetype::new("John Mayer"),
        );

        let resolved = registry.resolve(&category);
        assert!(resolved.is_some());
        assert_eq!(resolved.unwrap().name, "Blues Lead");
    }

    #[test]
    fn test_tree_structure() {
        let mut registry = PresetRegistry::new();

        registry.add(create_test_preset(
            "Clean",
            PresetCategory::generic(BaseTone::Clean),
        ));
        registry.add(create_test_preset(
            "Blues Clean",
            PresetCategory::genre(BaseTone::Clean, Genre::Blues),
        ));
        registry.add(create_test_preset(
            "John Mayer Clean",
            PresetCategory::archetype(
                BaseTone::Clean,
                Genre::Blues,
                Archetype::new("John Mayer"),
            ),
        ));

        let tree = registry.get_tree();

        // Root should have "Clean" as a child
        assert!(!tree.children.is_empty());

        let clean_node = tree.children.iter().find(|c| c.name == "Clean").unwrap();
        assert_eq!(clean_node.level, 1);

        // Clean should have Blues as a child
        let blues_node = clean_node.children.iter().find(|c| c.name == "Blues").unwrap();
        assert_eq!(blues_node.level, 2);

        // Blues should have John Mayer as a child
        let jm_node = blues_node
            .children
            .iter()
            .find(|c| c.name == "John Mayer")
            .unwrap();
        assert_eq!(jm_node.level, 3);
    }

    #[test]
    fn test_search() {
        let mut registry = PresetRegistry::new();

        registry.add(create_test_preset(
            "Blues Clean",
            PresetCategory::genre(BaseTone::Clean, Genre::Blues),
        ));
        registry.add(create_test_preset(
            "Rock Clean",
            PresetCategory::genre(BaseTone::Clean, Genre::Rock),
        ));
        registry.add(create_test_preset(
            "Blues Lead",
            PresetCategory::genre(BaseTone::Lead, Genre::Blues),
        ));

        let results = registry.search("blues");
        assert_eq!(results.len(), 2);

        let results = registry.search("clean");
        assert_eq!(results.len(), 2);
    }
}
