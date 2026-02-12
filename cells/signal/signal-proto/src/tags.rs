//! Tag System for organizing presets
//!
//! A flexible tagging system that enables:
//! - Hierarchical nested tags (Dynamics -> Compressor -> FET)
//! - Multiple tags per preset
//! - Inclusive/exclusive filtering (+blues -aggressive)
//! - Weighted fallback resolution
//! - Quick tag panel for fast organization
//!
//! Every preset must have at least one "base tone" tag to guarantee fallback.

use std::collections::{HashMap, HashSet};

use uuid::Uuid;

use crate::id::TagId;
use crate::normalized::Rating;

/// Namespace UUID for deterministic tag IDs (UUID v5).
/// This ensures the same tag name always produces the same UUID.
const TAG_NAMESPACE: Uuid = Uuid::from_bytes([
    0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
]);

/// Create a deterministic `TagId` for a tag name.
/// This ensures "Clean" always has the same ID across all instances.
fn deterministic_tag_id(name: &str) -> TagId {
    TagId::from_uuid(Uuid::new_v5(&TAG_NAMESPACE, name.as_bytes()))
}

// ============================================================================
// Tag
// ============================================================================

/// A tag for organizing presets.
///
/// Tags can be hierarchical (parent-child relationships) and have
/// priority weights for fallback resolution.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct Tag {
    /// Unique identifier.
    pub id: TagId,
    /// Tag name (e.g., "Blues", "Lead", "John Mayer").
    pub name: String,
    /// Tag category for grouping in UI.
    pub category: TagCategory,
    /// Parent tag ID for hierarchy (e.g., "FET" under "Compressor").
    pub parent_id: Option<TagId>,
    /// Priority weight for fallback resolution (higher = more important).
    pub priority: TagPriority,
    /// Color for UI display (hex color code).
    pub color: Option<String>,
    /// Whether this tag is hidden from the main tag list.
    pub hidden: bool,
    /// Optional description/notes.
    pub description: Option<String>,
}

impl Tag {
    /// Create a new tag with a deterministic ID based on the name.
    /// This ensures the same tag name always produces the same ID.
    pub fn new(name: impl Into<String>, category: TagCategory) -> Self {
        let name = name.into();
        Self {
            id: deterministic_tag_id(&name),
            name,
            category,
            parent_id: None,
            priority: category.default_priority(),
            color: category.default_color(),
            hidden: false,
            description: None,
        }
    }

    /// Create a tag with a specific ID (for testing/migration).
    pub fn with_id(id: TagId, name: impl Into<String>, category: TagCategory) -> Self {
        Self {
            id,
            name: name.into(),
            category,
            parent_id: None,
            priority: category.default_priority(),
            color: category.default_color(),
            hidden: false,
            description: None,
        }
    }

    /// Set the parent tag (for hierarchy).
    #[must_use]
    pub fn with_parent(mut self, parent_id: TagId) -> Self {
        self.parent_id = Some(parent_id);
        self
    }

    /// Set the priority.
    #[must_use]
    pub fn with_priority(mut self, priority: TagPriority) -> Self {
        self.priority = priority;
        self
    }

    /// Set the color.
    #[must_use]
    pub fn with_color(mut self, color: impl Into<String>) -> Self {
        self.color = Some(color.into());
        self
    }

    /// Set the description.
    #[must_use]
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Hide this tag from the main list.
    #[must_use]
    pub fn hidden(mut self) -> Self {
        self.hidden = true;
        self
    }

    // ── Common base tone tags ───────────────────────────────────────────

    /// Create a "Clean" base tone tag.
    pub fn clean() -> Self {
        Self::new("Clean", TagCategory::BaseTone)
    }

    /// Create a "Crunch" base tone tag.
    pub fn crunch() -> Self {
        Self::new("Crunch", TagCategory::BaseTone)
    }

    /// Create a "Drive" base tone tag.
    pub fn drive() -> Self {
        Self::new("Drive", TagCategory::BaseTone)
    }

    /// Create a "Lead" base tone tag.
    pub fn lead() -> Self {
        Self::new("Lead", TagCategory::BaseTone)
    }

    /// Create a "Solo" base tone tag.
    pub fn solo() -> Self {
        Self::new("Solo", TagCategory::BaseTone)
    }

    /// Create an "Ambient" base tone tag.
    pub fn ambient() -> Self {
        Self::new("Ambient", TagCategory::BaseTone)
    }

    // ── Common genre tags ───────────────────────────────────────────────

    /// Create a "Blues" genre tag.
    pub fn blues() -> Self {
        Self::new("Blues", TagCategory::Genre)
    }

    /// Create a "Rock" genre tag.
    pub fn rock() -> Self {
        Self::new("Rock", TagCategory::Genre)
    }

    /// Create a "Metal" genre tag.
    pub fn metal() -> Self {
        Self::new("Metal", TagCategory::Genre)
    }

    /// Create a "Jazz" genre tag.
    pub fn jazz() -> Self {
        Self::new("Jazz", TagCategory::Genre)
    }

    /// Create a "Worship" genre tag.
    pub fn worship() -> Self {
        Self::new("Worship", TagCategory::Genre)
    }

    /// Create a "Country" genre tag.
    pub fn country() -> Self {
        Self::new("Country", TagCategory::Genre)
    }

    // ── Common character tags ───────────────────────────────────────────

    /// Create a "Warm" character tag.
    pub fn warm() -> Self {
        Self::new("Warm", TagCategory::Character)
    }

    /// Create a "Bright" character tag.
    pub fn bright() -> Self {
        Self::new("Bright", TagCategory::Character)
    }

    /// Create an "Aggressive" character tag.
    pub fn aggressive() -> Self {
        Self::new("Aggressive", TagCategory::Character)
    }

    /// Create a "Smooth" character tag.
    pub fn smooth() -> Self {
        Self::new("Smooth", TagCategory::Character)
    }

    /// Create a "Vintage" character tag.
    pub fn vintage() -> Self {
        Self::new("Vintage", TagCategory::Character)
    }

    /// Create a "Modern" character tag.
    pub fn modern() -> Self {
        Self::new("Modern", TagCategory::Character)
    }

    // ── Common context tags ─────────────────────────────────────────────

    /// Create a "Verse" context tag.
    pub fn verse() -> Self {
        Self::new("Verse", TagCategory::Context)
    }

    /// Create a "Chorus" context tag.
    pub fn chorus() -> Self {
        Self::new("Chorus", TagCategory::Context)
    }

    /// Create a "Bridge" context tag.
    pub fn bridge() -> Self {
        Self::new("Bridge", TagCategory::Context)
    }

    /// Create a "Rhythm" context tag.
    pub fn rhythm() -> Self {
        Self::new("Rhythm", TagCategory::Context)
    }
}

impl PartialEq for Tag {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Tag {}

impl std::hash::Hash for Tag {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

// ============================================================================
// TagCategory
// ============================================================================

/// Tag categories for organization and display.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::facet::Facet, Default)]
#[repr(u8)]
pub enum TagCategory {
    /// Base tone type - REQUIRED for fallback (Clean, Lead, Drive, etc.)
    #[default]
    BaseTone,
    /// Musical genre (Blues, Rock, Metal, Jazz, Worship).
    Genre,
    /// Sub-genre for more specific styles.
    SubGenre,
    /// Artist/archetype reference (John Mayer, SRV, EVH).
    Archetype,
    /// Specific song.
    Song,
    /// Tonal character (Warm, Bright, Aggressive, Smooth).
    Character,
    /// Usage context (Verse, Chorus, Solo, Rhythm).
    Context,
    /// Gear emulation (Fender, Marshall, Klon, 1176).
    Gear,
    /// Custom user-defined category.
    Custom,
}

impl TagCategory {
    /// Get the default priority for this category.
    pub const fn default_priority(&self) -> TagPriority {
        match self {
            Self::BaseTone => TagPriority::Required,
            Self::Genre => TagPriority::High,
            Self::SubGenre | Self::Archetype => TagPriority::Medium,
            Self::Song | Self::Character | Self::Context => TagPriority::Low,
            Self::Gear | Self::Custom => TagPriority::Optional,
        }
    }

    /// Get the default color for this category.
    pub fn default_color(&self) -> Option<String> {
        match self {
            Self::BaseTone => Some("#3B82F6".to_string()),  // Blue
            Self::Genre => Some("#22C55E".to_string()),     // Green
            Self::SubGenre => Some("#EAB308".to_string()),  // Yellow
            Self::Archetype => Some("#A855F7".to_string()), // Purple
            Self::Song => Some("#EC4899".to_string()),      // Pink
            Self::Character => Some("#F97316".to_string()), // Orange
            Self::Context => Some("#06B6D4".to_string()),   // Cyan
            Self::Gear => Some("#6B7280".to_string()),      // Gray
            Self::Custom => Some("#78716C".to_string()),    // Stone
        }
    }

    /// Get the display name for this category.
    pub const fn display_name(&self) -> &'static str {
        match self {
            Self::BaseTone => "Tone",
            Self::Genre => "Genre",
            Self::SubGenre => "Sub-Genre",
            Self::Archetype => "Artist",
            Self::Song => "Song",
            Self::Character => "Character",
            Self::Context => "Context",
            Self::Gear => "Gear",
            Self::Custom => "Custom",
        }
    }

    /// Get all categories in display order.
    pub const fn all() -> &'static [TagCategory] {
        &[
            Self::BaseTone,
            Self::Genre,
            Self::SubGenre,
            Self::Archetype,
            Self::Song,
            Self::Character,
            Self::Context,
            Self::Gear,
            Self::Custom,
        ]
    }
}

// ============================================================================
// TagPriority
// ============================================================================

/// Priority levels for tag-based fallback resolution.
///
/// Higher priority tags are matched first during fallback.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ::facet::Facet, Default)]
#[repr(u8)]
pub enum TagPriority {
    /// Required - must match (base tone tags).
    Required = 100,
    /// High priority (genre).
    High = 75,
    /// Medium priority (archetype, sub-genre).
    #[default]
    Medium = 50,
    /// Low priority (song, character, context).
    Low = 25,
    /// Optional - nice to have but not important for matching.
    Optional = 10,
}

impl TagPriority {
    /// Get the numeric weight for this priority.
    pub const fn weight(&self) -> u32 {
        *self as u32
    }
}

// ============================================================================
// Tags
// ============================================================================

/// A set of tags applied to any taggable item.
///
/// Tracks both manually-applied tags and auto-derived tags (from name analysis).
/// Auto-derived tags can be hidden in UI since they're redundant with the name.
#[derive(Debug, Clone, Default, ::facet::Facet, PartialEq, Eq)]
pub struct Tags {
    /// Manually applied tag IDs.
    pub manual: HashSet<TagId>,
    /// Auto-derived tag IDs (from name analysis).
    pub auto_derived: HashSet<TagId>,
}

impl Tags {
    /// Create empty tag set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create from a list of manual tag IDs.
    pub fn from_ids(ids: impl IntoIterator<Item = TagId>) -> Self {
        Self {
            manual: ids.into_iter().collect(),
            auto_derived: HashSet::new(),
        }
    }

    /// Add a manual tag.
    pub fn add(&mut self, tag_id: TagId) {
        self.manual.insert(tag_id);
    }

    /// Add an auto-derived tag.
    pub fn add_auto(&mut self, tag_id: TagId) {
        self.auto_derived.insert(tag_id);
    }

    /// Remove a tag from both manual and auto-derived.
    pub fn remove(&mut self, tag_id: TagId) {
        self.manual.remove(&tag_id);
        self.auto_derived.remove(&tag_id);
    }

    /// Check if a tag is present (manual or auto-derived).
    pub fn has(&self, tag_id: TagId) -> bool {
        self.manual.contains(&tag_id) || self.auto_derived.contains(&tag_id)
    }

    /// Check if a tag is auto-derived.
    pub fn is_auto_derived(&self, tag_id: TagId) -> bool {
        self.auto_derived.contains(&tag_id)
    }

    /// Get all tags (both manual and auto-derived).
    pub fn all(&self) -> HashSet<TagId> {
        self.manual.union(&self.auto_derived).copied().collect()
    }

    /// Get only display tags (manual tags, excluding auto-derived).
    pub fn display_tags(&self) -> impl Iterator<Item = &TagId> {
        self.manual.iter()
    }

    /// Check if any of the given tags are present.
    pub fn has_any(&self, tag_ids: &HashSet<TagId>) -> bool {
        let all = self.all();
        all.intersection(tag_ids).next().is_some()
    }

    /// Check if all of the given tags are present.
    pub fn has_all(&self, tag_ids: &HashSet<TagId>) -> bool {
        let all = self.all();
        tag_ids.is_subset(&all)
    }

    /// Get the number of tags (manual + auto-derived, deduplicated).
    pub fn len(&self) -> usize {
        self.all().len()
    }

    /// Check if empty (no tags at all).
    pub fn is_empty(&self) -> bool {
        self.manual.is_empty() && self.auto_derived.is_empty()
    }

    /// Iterate over all tag IDs.
    pub fn iter(&self) -> impl Iterator<Item = TagId> + '_ {
        self.manual.iter().chain(self.auto_derived.iter()).copied()
    }
}

// ============================================================================
// Taggable trait
// ============================================================================

/// Trait for items that can be tagged.
pub trait Taggable {
    /// Get the tags for this item.
    fn tags(&self) -> &Tags;
    /// Get mutable tags for this item.
    fn tags_mut(&mut self) -> &mut Tags;
    /// Get the name (used for auto-tagging).
    fn name(&self) -> &str;

    /// Add a manual tag.
    fn add_tag(&mut self, tag_id: TagId) {
        self.tags_mut().add(tag_id);
    }

    /// Remove a tag (from both manual and auto-derived).
    fn remove_tag(&mut self, tag_id: TagId) {
        self.tags_mut().remove(tag_id);
    }

    /// Check if a tag is present (manual or auto-derived).
    fn has_tag(&self, tag_id: TagId) -> bool {
        self.tags().has(tag_id)
    }

    /// Get all tag IDs (both manual and auto-derived).
    fn all_tag_ids(&self) -> HashSet<TagId> {
        self.tags().all()
    }
}

// ============================================================================
// TagFilter
// ============================================================================

/// Filter for searching presets by tags.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TagFilter {
    /// Tags that MUST be present (inclusive).
    pub include: HashSet<TagId>,
    /// Tags that MUST NOT be present (exclusive).
    pub exclude: HashSet<TagId>,
    /// Minimum star rating (0 = any).
    pub min_rating: Rating,
    /// Text search query.
    pub search_query: String,
}

impl TagFilter {
    /// Create a new empty filter.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add an inclusive tag filter.
    #[must_use]
    pub fn include(mut self, tag_id: TagId) -> Self {
        self.include.insert(tag_id);
        self
    }

    /// Add an exclusive tag filter.
    #[must_use]
    pub fn exclude(mut self, tag_id: TagId) -> Self {
        self.exclude.insert(tag_id);
        self
    }

    /// Set minimum rating.
    #[must_use]
    pub fn min_rating(mut self, rating: Rating) -> Self {
        self.min_rating = rating;
        self
    }

    /// Set search query.
    #[must_use]
    pub fn search(mut self, query: impl Into<String>) -> Self {
        self.search_query = query.into();
        self
    }

    /// Check if a preset's tags match this filter.
    pub fn matches(&self, tags: &Tags) -> bool {
        // Must have all inclusive tags
        if !tags.has_all(&self.include) {
            return false;
        }

        // Must not have any exclusive tags
        if tags.has_any(&self.exclude) {
            return false;
        }

        true
    }

    /// Check if the filter is empty (no constraints).
    pub fn is_empty(&self) -> bool {
        self.include.is_empty()
            && self.exclude.is_empty()
            && !self.min_rating.is_rated()
            && self.search_query.is_empty()
    }
}

// ============================================================================
// TagRegistry
// ============================================================================

/// Registry for all available tags.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TagRegistry {
    /// All tags by ID.
    tags: HashMap<TagId, Tag>,
    /// Tags by category for quick lookup.
    by_category: HashMap<TagCategory, Vec<TagId>>,
    /// Child tags by parent ID.
    children: HashMap<TagId, Vec<TagId>>,
    /// Root tags (no parent) by category.
    roots: HashMap<TagCategory, Vec<TagId>>,
}

impl TagRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a registry with default tags for guitar presets.
    pub fn with_defaults() -> Self {
        let mut registry = Self::new();

        // Base tones (required)
        registry.add(Tag::clean());
        registry.add(Tag::crunch());
        registry.add(Tag::drive());
        registry.add(Tag::lead());
        registry.add(Tag::solo());
        registry.add(Tag::ambient());
        registry.add(Tag::new("DI", TagCategory::BaseTone));
        registry.add(Tag::new("Dry", TagCategory::BaseTone));

        // Genres
        registry.add(Tag::blues());
        registry.add(Tag::rock());
        registry.add(Tag::metal());
        registry.add(Tag::jazz());
        registry.add(Tag::worship());
        registry.add(Tag::country());
        registry.add(Tag::new("Funk", TagCategory::Genre));
        registry.add(Tag::new("Pop", TagCategory::Genre));
        registry.add(Tag::new("R&B", TagCategory::Genre));
        registry.add(Tag::new("Indie", TagCategory::Genre));

        // Character
        registry.add(Tag::warm());
        registry.add(Tag::bright());
        registry.add(Tag::aggressive());
        registry.add(Tag::smooth());
        registry.add(Tag::vintage());
        registry.add(Tag::modern());
        registry.add(Tag::new("Glassy", TagCategory::Character));
        registry.add(Tag::new("Thick", TagCategory::Character));
        registry.add(Tag::new("Compressed", TagCategory::Character));
        registry.add(Tag::new("Dynamic", TagCategory::Character));

        // Context
        registry.add(Tag::verse());
        registry.add(Tag::chorus());
        registry.add(Tag::bridge());
        registry.add(Tag::rhythm());
        registry.add(Tag::new("Intro", TagCategory::Context));
        registry.add(Tag::new("Outro", TagCategory::Context));
        registry.add(Tag::new("Fingerpicking", TagCategory::Context));
        registry.add(Tag::new("Strumming", TagCategory::Context));

        registry
    }

    /// Add a tag to the registry.
    pub fn add(&mut self, tag: Tag) {
        let id = tag.id;
        let category = tag.category;
        let parent_id = tag.parent_id;

        // Index by category
        self.by_category.entry(category).or_default().push(id);

        // Index by parent
        if let Some(parent) = parent_id {
            self.children.entry(parent).or_default().push(id);
        } else {
            // It's a root tag
            self.roots.entry(category).or_default().push(id);
        }

        // Store the tag
        self.tags.insert(id, tag);
    }

    /// Remove a tag from the registry.
    pub fn remove(&mut self, id: TagId) -> Option<Tag> {
        let tag = self.tags.remove(&id)?;

        // Remove from category index
        if let Some(ids) = self.by_category.get_mut(&tag.category) {
            ids.retain(|&i| i != id);
        }

        // Remove from parent's children or roots
        if let Some(parent) = tag.parent_id {
            if let Some(children) = self.children.get_mut(&parent) {
                children.retain(|&i| i != id);
            }
        } else if let Some(roots) = self.roots.get_mut(&tag.category) {
            roots.retain(|&i| i != id);
        }

        // Remove this tag's children entry
        self.children.remove(&id);

        Some(tag)
    }

    /// Get a tag by ID.
    pub fn get(&self, id: TagId) -> Option<&Tag> {
        self.tags.get(&id)
    }

    /// Get all tags.
    pub fn all(&self) -> impl Iterator<Item = &Tag> {
        self.tags.values()
    }

    /// Get tags by category.
    pub fn by_category(&self, category: TagCategory) -> Vec<&Tag> {
        self.by_category
            .get(&category)
            .map(|ids| ids.iter().filter_map(|id| self.tags.get(id)).collect())
            .unwrap_or_default()
    }

    /// Get root tags (no parent) for a category.
    pub fn roots(&self, category: TagCategory) -> Vec<&Tag> {
        self.roots
            .get(&category)
            .map(|ids| ids.iter().filter_map(|id| self.tags.get(id)).collect())
            .unwrap_or_default()
    }

    /// Get children of a tag.
    pub fn children(&self, parent_id: TagId) -> Vec<&Tag> {
        self.children
            .get(&parent_id)
            .map(|ids| ids.iter().filter_map(|id| self.tags.get(id)).collect())
            .unwrap_or_default()
    }

    /// Get visible tags (not hidden).
    pub fn visible(&self) -> impl Iterator<Item = &Tag> {
        self.tags.values().filter(|t| !t.hidden)
    }

    /// Find a tag by name (case-insensitive).
    pub fn find_by_name(&self, name: &str) -> Option<&Tag> {
        let name_lower = name.to_lowercase();
        self.tags
            .values()
            .find(|t| t.name.to_lowercase() == name_lower)
    }

    /// Find tags matching a search query (simple substring match).
    pub fn search(&self, query: &str) -> Vec<&Tag> {
        let query_lower = query.to_lowercase();
        self.tags
            .values()
            .filter(|t| {
                t.name.to_lowercase().contains(&query_lower)
                    || t.description
                        .as_ref()
                        .is_some_and(|d| d.to_lowercase().contains(&query_lower))
            })
            .collect()
    }

    /// Get the total number of tags.
    pub fn len(&self) -> usize {
        self.tags.len()
    }

    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.tags.is_empty()
    }

    /// Get tag ancestry (tag + all parents up to root).
    pub fn ancestry(&self, tag_id: TagId) -> Vec<TagId> {
        let mut result = vec![tag_id];
        let mut current = tag_id;

        while let Some(tag) = self.get(current) {
            if let Some(parent) = tag.parent_id {
                result.push(parent);
                current = parent;
            } else {
                break;
            }
        }

        result
    }

    /// Get all descendants of a tag.
    pub fn descendants(&self, tag_id: TagId) -> Vec<TagId> {
        let mut result = Vec::new();
        let mut to_visit = vec![tag_id];

        while let Some(current) = to_visit.pop() {
            if let Some(children) = self.children.get(&current) {
                for &child in children {
                    result.push(child);
                    to_visit.push(child);
                }
            }
        }

        result
    }

    /// Resolve tags for fallback - given requested tags, find the best match
    /// by progressively removing low-priority tags until a match is found.
    pub fn fallback_chain(&self, requested: &HashSet<TagId>) -> Vec<HashSet<TagId>> {
        if requested.is_empty() {
            return vec![];
        }

        // Group tags by priority
        let mut priority_map: HashMap<TagPriority, Vec<TagId>> = HashMap::new();

        for &tag_id in requested {
            let priority = self.get(tag_id).map_or(TagPriority::Medium, |t| t.priority);
            priority_map.entry(priority).or_default().push(tag_id);
        }

        // Sort by priority (highest first)
        let mut priorities: Vec<_> = priority_map.keys().copied().collect();
        priorities.sort_by(|a, b| b.cmp(a));

        let mut by_priority: Vec<(TagPriority, Vec<TagId>)> = Vec::new();
        for priority in priorities {
            if let Some(tags) = priority_map.remove(&priority) {
                by_priority.push((priority, tags));
            }
        }

        // Generate fallback chain by progressively removing lowest priority tags
        let mut chain = vec![requested.clone()];
        let mut current = requested.clone();

        // Start from lowest priority and remove
        for (priority, tags) in by_priority.iter().rev() {
            // Don't remove required tags
            if *priority == TagPriority::Required {
                continue;
            }

            for tag in tags {
                current.remove(tag);
            }

            if !current.is_empty() && current != *chain.last().unwrap() {
                chain.push(current.clone());
            }
        }

        chain
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tag_creation() {
        let tag = Tag::new("Blues", TagCategory::Genre);
        assert_eq!(tag.name, "Blues");
        assert_eq!(tag.category, TagCategory::Genre);
        assert_eq!(tag.priority, TagPriority::High);
    }

    #[test]
    fn tag_deterministic_id() {
        let a = Tag::new("Clean", TagCategory::BaseTone);
        let b = Tag::new("Clean", TagCategory::BaseTone);
        assert_eq!(a.id, b.id);
    }

    #[test]
    fn tag_different_names_different_ids() {
        let a = Tag::new("Clean", TagCategory::BaseTone);
        let b = Tag::new("Crunch", TagCategory::BaseTone);
        assert_ne!(a.id, b.id);
    }

    #[test]
    fn tag_with_builder_methods() {
        let parent = Tag::rock();
        let tag = Tag::new("Classic Rock", TagCategory::SubGenre)
            .with_parent(parent.id)
            .with_priority(TagPriority::High)
            .with_color("#FF0000")
            .with_description("70s and 80s rock")
            .hidden();

        assert_eq!(tag.parent_id, Some(parent.id));
        assert_eq!(tag.priority, TagPriority::High);
        assert_eq!(tag.color.as_deref(), Some("#FF0000"));
        assert_eq!(tag.description.as_deref(), Some("70s and 80s rock"));
        assert!(tag.hidden);
    }

    #[test]
    fn tag_equality_by_id() {
        let a = Tag::new("Blues", TagCategory::Genre);
        let mut b = Tag::new("Blues", TagCategory::Genre);
        b.description = Some("different desc".into());
        // Same name -> same id -> equal despite different fields
        assert_eq!(a, b);
    }

    #[test]
    fn tag_with_id_constructor() {
        let id = TagId::new();
        let tag = Tag::with_id(id, "Test", TagCategory::Custom);
        assert_eq!(tag.id, id);
        assert_eq!(tag.name, "Test");
    }

    // ── TagCategory ─────────────────────────────────────────────────────

    #[test]
    fn tag_category_default_is_base_tone() {
        assert_eq!(TagCategory::default(), TagCategory::BaseTone);
    }

    #[test]
    fn tag_category_default_priorities() {
        assert_eq!(
            TagCategory::BaseTone.default_priority(),
            TagPriority::Required
        );
        assert_eq!(TagCategory::Genre.default_priority(), TagPriority::High);
        assert_eq!(
            TagCategory::SubGenre.default_priority(),
            TagPriority::Medium
        );
        assert_eq!(
            TagCategory::Archetype.default_priority(),
            TagPriority::Medium
        );
        assert_eq!(TagCategory::Song.default_priority(), TagPriority::Low);
        assert_eq!(TagCategory::Character.default_priority(), TagPriority::Low);
        assert_eq!(TagCategory::Context.default_priority(), TagPriority::Low);
        assert_eq!(TagCategory::Gear.default_priority(), TagPriority::Optional);
        assert_eq!(
            TagCategory::Custom.default_priority(),
            TagPriority::Optional
        );
    }

    #[test]
    fn tag_category_default_colors() {
        for cat in TagCategory::all() {
            assert!(cat.default_color().is_some());
        }
    }

    #[test]
    fn tag_category_display_names() {
        assert_eq!(TagCategory::BaseTone.display_name(), "Tone");
        assert_eq!(TagCategory::Genre.display_name(), "Genre");
        assert_eq!(TagCategory::SubGenre.display_name(), "Sub-Genre");
        assert_eq!(TagCategory::Archetype.display_name(), "Artist");
        assert_eq!(TagCategory::Song.display_name(), "Song");
        assert_eq!(TagCategory::Character.display_name(), "Character");
        assert_eq!(TagCategory::Context.display_name(), "Context");
        assert_eq!(TagCategory::Gear.display_name(), "Gear");
        assert_eq!(TagCategory::Custom.display_name(), "Custom");
    }

    #[test]
    fn tag_category_all_returns_all_variants() {
        assert_eq!(TagCategory::all().len(), 9);
    }

    // ── TagPriority ─────────────────────────────────────────────────────

    #[test]
    fn tag_priority_default_is_medium() {
        assert_eq!(TagPriority::default(), TagPriority::Medium);
    }

    #[test]
    fn tag_priority_weights() {
        assert_eq!(TagPriority::Required.weight(), 100);
        assert_eq!(TagPriority::High.weight(), 75);
        assert_eq!(TagPriority::Medium.weight(), 50);
        assert_eq!(TagPriority::Low.weight(), 25);
        assert_eq!(TagPriority::Optional.weight(), 10);
    }

    #[test]
    fn tag_priority_ordering() {
        assert!(TagPriority::Required > TagPriority::High);
        assert!(TagPriority::High > TagPriority::Medium);
        assert!(TagPriority::Medium > TagPriority::Low);
        assert!(TagPriority::Low > TagPriority::Optional);
    }

    // ── Tags ────────────────────────────────────────────────────────────

    #[test]
    fn tags_new_is_empty() {
        let tags = Tags::new();
        assert!(tags.is_empty());
        assert_eq!(tags.len(), 0);
    }

    #[test]
    fn tags_from_ids() {
        let id1 = TagId::new();
        let id2 = TagId::new();
        let tags = Tags::from_ids([id1, id2]);
        assert!(tags.has(id1));
        assert!(tags.has(id2));
        assert_eq!(tags.len(), 2);
    }

    #[test]
    fn tags_add_and_remove() {
        let mut tags = Tags::new();
        let id1 = TagId::new();
        let id2 = TagId::new();

        tags.add(id1);
        tags.add(id2);

        assert!(tags.has(id1));
        assert!(tags.has(id2));
        assert_eq!(tags.len(), 2);

        tags.remove(id1);
        assert!(!tags.has(id1));
        assert_eq!(tags.len(), 1);
    }

    #[test]
    fn tags_auto_derived() {
        let mut tags = Tags::new();
        let manual_id = TagId::new();
        let auto_id = TagId::new();

        tags.add(manual_id);
        tags.add_auto(auto_id);

        // Both should be present
        assert!(tags.has(manual_id));
        assert!(tags.has(auto_id));
        assert_eq!(tags.len(), 2);

        // Auto-derived check
        assert!(!tags.is_auto_derived(manual_id));
        assert!(tags.is_auto_derived(auto_id));
    }

    #[test]
    fn tags_display_excludes_auto_derived() {
        let mut tags = Tags::new();
        let manual_id = TagId::new();
        let auto_id = TagId::new();

        tags.add(manual_id);
        tags.add_auto(auto_id);

        let display: Vec<_> = tags.display_tags().copied().collect();
        assert_eq!(display.len(), 1);
        assert!(display.contains(&manual_id));
        assert!(!display.contains(&auto_id));
    }

    #[test]
    fn tags_has_any() {
        let mut tags = Tags::new();
        let id1 = TagId::new();
        let id2 = TagId::new();
        let id3 = TagId::new();

        tags.add(id1);

        let search: HashSet<_> = [id1, id2].into_iter().collect();
        assert!(tags.has_any(&search));

        let search: HashSet<_> = [id2, id3].into_iter().collect();
        assert!(!tags.has_any(&search));
    }

    #[test]
    fn tags_has_all() {
        let mut tags = Tags::new();
        let id1 = TagId::new();
        let id2 = TagId::new();
        let id3 = TagId::new();

        tags.add(id1);
        tags.add(id2);

        let search: HashSet<_> = [id1, id2].into_iter().collect();
        assert!(tags.has_all(&search));

        let search: HashSet<_> = [id1, id2, id3].into_iter().collect();
        assert!(!tags.has_all(&search));
    }

    #[test]
    fn tags_all_combines_manual_and_auto() {
        let mut tags = Tags::new();
        let id1 = TagId::new();
        let id2 = TagId::new();

        tags.add(id1);
        tags.add_auto(id2);

        let all = tags.all();
        assert_eq!(all.len(), 2);
        assert!(all.contains(&id1));
        assert!(all.contains(&id2));
    }

    #[test]
    fn tags_iter() {
        let mut tags = Tags::new();
        let id1 = TagId::new();
        let id2 = TagId::new();

        tags.add(id1);
        tags.add_auto(id2);

        let collected: HashSet<_> = tags.iter().collect();
        assert_eq!(collected.len(), 2);
        assert!(collected.contains(&id1));
        assert!(collected.contains(&id2));
    }

    // ── Taggable trait ──────────────────────────────────────────────────

    #[test]
    fn taggable_default_methods() {
        struct TestItem {
            tags: Tags,
            name: String,
        }

        impl Taggable for TestItem {
            fn tags(&self) -> &Tags {
                &self.tags
            }
            fn tags_mut(&mut self) -> &mut Tags {
                &mut self.tags
            }
            fn name(&self) -> &str {
                &self.name
            }
        }

        let mut item = TestItem {
            tags: Tags::new(),
            name: "Test".to_string(),
        };

        let id = TagId::new();
        item.add_tag(id);
        assert!(item.has_tag(id));

        let all = item.all_tag_ids();
        assert!(all.contains(&id));

        item.remove_tag(id);
        assert!(!item.has_tag(id));
    }

    // ── TagFilter ───────────────────────────────────────────────────────

    #[test]
    fn tag_filter_new_is_empty() {
        let filter = TagFilter::new();
        assert!(filter.is_empty());
    }

    #[test]
    fn tag_filter_builder_methods() {
        let id1 = TagId::new();
        let id2 = TagId::new();

        let filter = TagFilter::new()
            .include(id1)
            .exclude(id2)
            .min_rating(Rating::new(3))
            .search("blues");

        assert!(filter.include.contains(&id1));
        assert!(filter.exclude.contains(&id2));
        assert_eq!(filter.min_rating.get(), 3);
        assert_eq!(filter.search_query, "blues");
        assert!(!filter.is_empty());
    }

    #[test]
    fn tag_filter_matches_inclusive() {
        let id1 = TagId::new();
        let id2 = TagId::new();

        let mut tags = Tags::new();
        tags.add(id1);
        tags.add(id2);

        let filter = TagFilter::new().include(id1).include(id2);
        assert!(filter.matches(&tags));
    }

    #[test]
    fn tag_filter_rejects_missing_inclusive() {
        let id1 = TagId::new();
        let id2 = TagId::new();

        let mut tags = Tags::new();
        tags.add(id1);

        let filter = TagFilter::new().include(id1).include(id2);
        assert!(!filter.matches(&tags));
    }

    #[test]
    fn tag_filter_rejects_excluded() {
        let id1 = TagId::new();
        let id2 = TagId::new();

        let mut tags = Tags::new();
        tags.add(id1);
        tags.add(id2);

        let filter = TagFilter::new().include(id1).exclude(id2);
        assert!(!filter.matches(&tags));
    }

    #[test]
    fn tag_filter_accepts_when_excluded_absent() {
        let id1 = TagId::new();
        let id2 = TagId::new();

        let mut tags = Tags::new();
        tags.add(id1);

        let filter = TagFilter::new().include(id1).exclude(id2);
        assert!(filter.matches(&tags));
    }

    // ── TagRegistry ─────────────────────────────────────────────────────

    #[test]
    fn registry_new_is_empty() {
        let registry = TagRegistry::new();
        assert!(registry.is_empty());
        assert_eq!(registry.len(), 0);
    }

    #[test]
    fn registry_add_and_get() {
        let mut registry = TagRegistry::new();
        let blues = Tag::blues();
        let blues_id = blues.id;
        registry.add(blues);

        assert_eq!(registry.len(), 1);
        assert!(registry.get(blues_id).is_some());
        assert_eq!(registry.get(blues_id).unwrap().name, "Blues");
    }

    #[test]
    fn registry_remove() {
        let mut registry = TagRegistry::new();
        let blues = Tag::blues();
        let blues_id = blues.id;
        registry.add(blues);

        let removed = registry.remove(blues_id);
        assert!(removed.is_some());
        assert_eq!(removed.unwrap().name, "Blues");
        assert!(registry.is_empty());
    }

    #[test]
    fn registry_by_category() {
        let mut registry = TagRegistry::new();
        registry.add(Tag::blues());
        registry.add(Tag::rock());
        registry.add(Tag::clean());

        let genres = registry.by_category(TagCategory::Genre);
        assert_eq!(genres.len(), 2);

        let tones = registry.by_category(TagCategory::BaseTone);
        assert_eq!(tones.len(), 1);
    }

    #[test]
    fn registry_roots() {
        let mut registry = TagRegistry::new();
        let rock = Tag::rock();
        let rock_id = rock.id;
        registry.add(rock);

        let sub = Tag::new("Classic Rock", TagCategory::SubGenre).with_parent(rock_id);
        registry.add(sub);

        let genre_roots = registry.roots(TagCategory::Genre);
        assert_eq!(genre_roots.len(), 1);
        assert_eq!(genre_roots[0].name, "Rock");
    }

    #[test]
    fn registry_children() {
        let mut registry = TagRegistry::new();
        let rock = Tag::rock();
        let rock_id = rock.id;
        registry.add(rock);

        registry.add(Tag::new("Classic Rock", TagCategory::SubGenre).with_parent(rock_id));
        registry.add(Tag::new("Punk Rock", TagCategory::SubGenre).with_parent(rock_id));

        let children = registry.children(rock_id);
        assert_eq!(children.len(), 2);
    }

    #[test]
    fn registry_visible() {
        let mut registry = TagRegistry::new();
        registry.add(Tag::blues());
        registry.add(Tag::new("Hidden Tag", TagCategory::Custom).hidden());

        let visible: Vec<_> = registry.visible().collect();
        assert_eq!(visible.len(), 1);
        assert_eq!(visible[0].name, "Blues");
    }

    #[test]
    fn registry_find_by_name() {
        let mut registry = TagRegistry::new();
        registry.add(Tag::blues());

        assert!(registry.find_by_name("Blues").is_some());
        assert!(registry.find_by_name("blues").is_some()); // case-insensitive
        assert!(registry.find_by_name("BLUES").is_some()); // case-insensitive
        assert!(registry.find_by_name("Nonexistent").is_none());
    }

    #[test]
    fn registry_search() {
        let mut registry = TagRegistry::new();
        registry.add(Tag::blues());
        registry.add(Tag::rock());
        registry.add(
            Tag::new("Blues Rock", TagCategory::SubGenre)
                .with_description("A mix of blues and rock"),
        );

        let results = registry.search("blues");
        assert_eq!(results.len(), 2); // "Blues" and "Blues Rock"

        let results = registry.search("mix");
        assert_eq!(results.len(), 1); // matches description of "Blues Rock"
    }

    #[test]
    fn registry_with_defaults_is_populated() {
        let registry = TagRegistry::with_defaults();
        assert!(!registry.is_empty());

        // Should have base tones
        let tones = registry.by_category(TagCategory::BaseTone);
        assert!(tones.len() >= 6);

        // Should have genres
        let genres = registry.by_category(TagCategory::Genre);
        assert!(genres.len() >= 6);

        // Should have character tags
        let characters = registry.by_category(TagCategory::Character);
        assert!(characters.len() >= 6);

        // Should have context tags
        let contexts = registry.by_category(TagCategory::Context);
        assert!(contexts.len() >= 4);
    }

    #[test]
    fn registry_ancestry() {
        let mut registry = TagRegistry::new();

        let rock = Tag::rock();
        let rock_id = rock.id;
        registry.add(rock);

        let classic = Tag::new("Classic Rock", TagCategory::SubGenre).with_parent(rock_id);
        let classic_id = classic.id;
        registry.add(classic);

        let sixties = Tag::new("60s Rock", TagCategory::SubGenre).with_parent(classic_id);
        let sixties_id = sixties.id;
        registry.add(sixties);

        let ancestry = registry.ancestry(sixties_id);
        assert_eq!(ancestry.len(), 3);
        assert_eq!(ancestry[0], sixties_id);
        assert_eq!(ancestry[1], classic_id);
        assert_eq!(ancestry[2], rock_id);
    }

    #[test]
    fn registry_descendants() {
        let mut registry = TagRegistry::new();

        let rock = Tag::rock();
        let rock_id = rock.id;
        registry.add(rock);

        let classic = Tag::new("Classic Rock", TagCategory::SubGenre).with_parent(rock_id);
        let classic_id = classic.id;
        registry.add(classic);

        let punk = Tag::new("Punk Rock", TagCategory::SubGenre).with_parent(rock_id);
        let punk_id = punk.id;
        registry.add(punk);

        let descendants = registry.descendants(rock_id);
        assert_eq!(descendants.len(), 2);
        assert!(descendants.contains(&classic_id));
        assert!(descendants.contains(&punk_id));
    }

    #[test]
    fn registry_fallback_chain() {
        let mut registry = TagRegistry::new();

        let lead = Tag::lead();
        let lead_id = lead.id;
        registry.add(lead);

        let blues = Tag::blues();
        let blues_id = blues.id;
        registry.add(blues);

        let jm = Tag::new("John Mayer", TagCategory::Archetype);
        let jm_id = jm.id;
        registry.add(jm);

        let gravity = Tag::new("Gravity", TagCategory::Song);
        let gravity_id = gravity.id;
        registry.add(gravity);

        // Request: Gravity + John Mayer + Blues + Lead
        let requested: HashSet<TagId> =
            [gravity_id, jm_id, blues_id, lead_id].into_iter().collect();

        let chain = registry.fallback_chain(&requested);

        // Should have progressively fewer tags
        assert!(chain.len() >= 2);
        assert_eq!(chain[0].len(), 4); // All tags

        // Last entry should have just required tags
        let last = chain.last().unwrap();
        assert!(last.contains(&lead_id)); // Required tag stays
    }

    #[test]
    fn registry_fallback_chain_empty_input() {
        let registry = TagRegistry::new();
        let chain = registry.fallback_chain(&HashSet::new());
        assert!(chain.is_empty());
    }

    // ── Convenience constructors ────────────────────────────────────────

    #[test]
    fn convenience_base_tone_tags() {
        let tags = [
            Tag::clean(),
            Tag::crunch(),
            Tag::drive(),
            Tag::lead(),
            Tag::solo(),
            Tag::ambient(),
        ];
        for tag in &tags {
            assert_eq!(tag.category, TagCategory::BaseTone);
            assert_eq!(tag.priority, TagPriority::Required);
        }
    }

    #[test]
    fn convenience_genre_tags() {
        let tags = [
            Tag::blues(),
            Tag::rock(),
            Tag::metal(),
            Tag::jazz(),
            Tag::worship(),
            Tag::country(),
        ];
        for tag in &tags {
            assert_eq!(tag.category, TagCategory::Genre);
            assert_eq!(tag.priority, TagPriority::High);
        }
    }

    #[test]
    fn convenience_character_tags() {
        let tags = [
            Tag::warm(),
            Tag::bright(),
            Tag::aggressive(),
            Tag::smooth(),
            Tag::vintage(),
            Tag::modern(),
        ];
        for tag in &tags {
            assert_eq!(tag.category, TagCategory::Character);
            assert_eq!(tag.priority, TagPriority::Low);
        }
    }

    #[test]
    fn convenience_context_tags() {
        let tags = [Tag::verse(), Tag::chorus(), Tag::bridge(), Tag::rhythm()];
        for tag in &tags {
            assert_eq!(tag.category, TagCategory::Context);
            assert_eq!(tag.priority, TagPriority::Low);
        }
    }
}
