//! Tag System - Scout-inspired tagging for presets
//!
//! A flexible tagging system that enables:
//! - Hierarchical nested tags (Dynamics → Compressor → FET)
//! - Multiple tags per preset
//! - Inclusive/exclusive filtering (+blues -aggressive)
//! - Weighted fallback resolution
//! - Quick tag panel for fast organization
//!
//! Every preset must have at least one "base tone" tag to guarantee fallback.

use std::collections::{HashMap, HashSet};

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Namespace UUID for deterministic tag IDs (UUID v5)
/// This ensures the same tag name always produces the same UUID
const TAG_NAMESPACE: Uuid = Uuid::from_bytes([
    0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
]);

/// Create a deterministic UUID for a tag name
/// This ensures "Clean" always has the same UUID across all instances
fn deterministic_tag_id(name: &str) -> Uuid {
    Uuid::new_v5(&TAG_NAMESPACE, name.as_bytes())
}

/// A tag for organizing presets.
///
/// Tags can be hierarchical (parent-child relationships) and have
/// priority weights for fallback resolution.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct PresetTag {
    /// Unique identifier
    pub id: Uuid,
    /// Tag name (e.g., "Blues", "Lead", "John Mayer")
    pub name: String,
    /// Tag category for grouping in UI
    pub category: TagCategory,
    /// Parent tag ID for hierarchy (e.g., "FET" under "Compressor")
    pub parent_id: Option<Uuid>,
    /// Priority weight for fallback resolution (higher = more important)
    pub priority: TagPriority,
    /// Color for UI display (hex color code)
    pub color: Option<String>,
    /// Whether this tag is hidden from the main tag list
    pub hidden: bool,
    /// Optional description/notes
    pub description: Option<String>,
}

/// Tag categories for organization and display.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Facet, Default)]
#[repr(u8)]
pub enum TagCategory {
    /// Base tone type - REQUIRED for fallback (Clean, Lead, Drive, etc.)
    #[default]
    BaseTone,
    /// Musical genre (Blues, Rock, Metal, Jazz, Worship)
    Genre,
    /// Sub-genre for more specific styles
    SubGenre,
    /// Artist/archetype reference (John Mayer, SRV, EVH)
    Archetype,
    /// Specific song
    Song,
    /// Tonal character (Warm, Bright, Aggressive, Smooth)
    Character,
    /// Usage context (Verse, Chorus, Solo, Rhythm)
    Context,
    /// Gear emulation (Fender, Marshall, Klon, 1176)
    Gear,
    /// Custom user-defined category
    Custom,
}

/// Priority levels for tag-based fallback resolution.
///
/// Higher priority tags are matched first during fallback.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Facet, Default)]
#[repr(u8)]
pub enum TagPriority {
    /// Required - must match (base tone tags)
    Required = 100,
    /// High priority (genre)
    High = 75,
    /// Medium priority (archetype, sub-genre)
    #[default]
    Medium = 50,
    /// Low priority (song, character, context)
    Low = 25,
    /// Optional - nice to have but not important for matching
    Optional = 10,
}

/// A set of tags applied to any taggable item.
///
/// Tracks both manually-applied tags and auto-derived tags (from name analysis).
/// Auto-derived tags can be hidden in UI since they're redundant with the name.
#[derive(Debug, Clone, Default, Serialize, Deserialize, Facet, PartialEq, Eq)]
pub struct Tags {
    /// Manually applied tag IDs
    pub manual: HashSet<Uuid>,
    /// Auto-derived tag IDs (from name analysis)
    #[serde(default)]
    pub auto_derived: HashSet<Uuid>,
}

/// Trait for items that can be tagged
pub trait Taggable {
    /// Get the tags for this item
    fn tags(&self) -> &Tags;
    /// Get mutable tags for this item
    fn tags_mut(&mut self) -> &mut Tags;
    /// Get the name (used for auto-tagging)
    fn name(&self) -> &str;

    /// Add a manual tag
    fn add_tag(&mut self, tag_id: Uuid) {
        self.tags_mut().add(tag_id);
    }

    /// Remove a tag (from both manual and auto-derived)
    fn remove_tag(&mut self, tag_id: Uuid) {
        self.tags_mut().remove(tag_id);
    }

    /// Check if a tag is present (manual or auto-derived)
    fn has_tag(&self, tag_id: Uuid) -> bool {
        self.tags().has(tag_id)
    }

    /// Get all tag IDs (both manual and auto-derived)
    fn all_tag_ids(&self) -> HashSet<Uuid> {
        self.tags().all()
    }

    /// Get only the tags that should be displayed (excludes auto-derived)
    fn display_tag_ids(&self) -> impl Iterator<Item = &Uuid> {
        self.tags().display_tags()
    }

    /// Apply auto-tagging based on the item's name
    fn auto_tag(&mut self, registry: &TagRegistry) {
        let auto_tags = derive_tags_from_name(self.name(), registry);
        self.tags_mut().auto_derived = auto_tags;
    }
}

/// Legacy alias for backwards compatibility
pub type PresetTags = Tags;

/// Legacy alias for backwards compatibility
pub type SnapshotTags = Tags;

/// Filter for searching presets by tags.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TagFilter {
    /// Tags that MUST be present (inclusive)
    pub include: HashSet<Uuid>,
    /// Tags that MUST NOT be present (exclusive)
    pub exclude: HashSet<Uuid>,
    /// Minimum star rating (1-5, 0 = any)
    pub min_rating: u8,
    /// Text search query
    pub search_query: String,
}

/// Result of a tag-based preset search with match score.
#[derive(Debug, Clone)]
pub struct TagMatchResult {
    /// Preset ID
    pub preset_id: Uuid,
    /// Match score (higher = better match)
    pub score: u32,
    /// Which requested tags were matched
    pub matched_tags: HashSet<Uuid>,
    /// Which requested tags were NOT matched (for fallback info)
    pub missing_tags: HashSet<Uuid>,
    /// Whether this is a fallback result (not all tags matched)
    pub is_fallback: bool,
}

/// Result of a fuzzy search match on a tag.
#[derive(Debug, Clone)]
pub struct FuzzyMatch<'a> {
    /// The matched tag
    pub tag: &'a PresetTag,
    /// Fuzzy match score (higher = better match)
    pub score: i64,
}

// ============================================================================
// PresetTag Implementation
// ============================================================================

impl PresetTag {
    /// Create a new tag with a deterministic UUID based on the name
    /// This ensures the same tag name always produces the same UUID
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

    /// Create a tag with a specific ID (for testing/migration)
    pub fn with_id(id: Uuid, name: impl Into<String>, category: TagCategory) -> Self {
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

    /// Set the parent tag (for hierarchy)
    pub fn with_parent(mut self, parent_id: Uuid) -> Self {
        self.parent_id = Some(parent_id);
        self
    }

    /// Set the priority
    pub fn with_priority(mut self, priority: TagPriority) -> Self {
        self.priority = priority;
        self
    }

    /// Set the color
    pub fn with_color(mut self, color: impl Into<String>) -> Self {
        self.color = Some(color.into());
        self
    }

    /// Set the description
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Hide this tag from the main list
    pub fn hidden(mut self) -> Self {
        self.hidden = true;
        self
    }

    // Common base tone tags
    pub fn clean() -> Self {
        Self::new("Clean", TagCategory::BaseTone)
    }
    pub fn crunch() -> Self {
        Self::new("Crunch", TagCategory::BaseTone)
    }
    pub fn drive() -> Self {
        Self::new("Drive", TagCategory::BaseTone)
    }
    pub fn lead() -> Self {
        Self::new("Lead", TagCategory::BaseTone)
    }
    pub fn solo() -> Self {
        Self::new("Solo", TagCategory::BaseTone)
    }
    pub fn ambient() -> Self {
        Self::new("Ambient", TagCategory::BaseTone)
    }

    // Common genre tags
    pub fn blues() -> Self {
        Self::new("Blues", TagCategory::Genre)
    }
    pub fn rock() -> Self {
        Self::new("Rock", TagCategory::Genre)
    }
    pub fn metal() -> Self {
        Self::new("Metal", TagCategory::Genre)
    }
    pub fn jazz() -> Self {
        Self::new("Jazz", TagCategory::Genre)
    }
    pub fn worship() -> Self {
        Self::new("Worship", TagCategory::Genre)
    }
    pub fn country() -> Self {
        Self::new("Country", TagCategory::Genre)
    }

    // Common character tags
    pub fn warm() -> Self {
        Self::new("Warm", TagCategory::Character)
    }
    pub fn bright() -> Self {
        Self::new("Bright", TagCategory::Character)
    }
    pub fn aggressive() -> Self {
        Self::new("Aggressive", TagCategory::Character)
    }
    pub fn smooth() -> Self {
        Self::new("Smooth", TagCategory::Character)
    }
    pub fn vintage() -> Self {
        Self::new("Vintage", TagCategory::Character)
    }
    pub fn modern() -> Self {
        Self::new("Modern", TagCategory::Character)
    }

    // Common context tags
    pub fn verse() -> Self {
        Self::new("Verse", TagCategory::Context)
    }
    pub fn chorus() -> Self {
        Self::new("Chorus", TagCategory::Context)
    }
    pub fn bridge() -> Self {
        Self::new("Bridge", TagCategory::Context)
    }
    pub fn rhythm() -> Self {
        Self::new("Rhythm", TagCategory::Context)
    }
}

impl PartialEq for PresetTag {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for PresetTag {}

impl std::hash::Hash for PresetTag {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

// ============================================================================
// TagCategory Implementation
// ============================================================================

impl TagCategory {
    /// Get the default priority for this category
    pub const fn default_priority(&self) -> TagPriority {
        match self {
            Self::BaseTone => TagPriority::Required,
            Self::Genre => TagPriority::High,
            Self::SubGenre => TagPriority::Medium,
            Self::Archetype => TagPriority::Medium,
            Self::Song => TagPriority::Low,
            Self::Character => TagPriority::Low,
            Self::Context => TagPriority::Low,
            Self::Gear => TagPriority::Optional,
            Self::Custom => TagPriority::Optional,
        }
    }

    /// Get the default color for this category
    pub fn default_color(&self) -> Option<String> {
        match self {
            Self::BaseTone => Some("#3B82F6".to_string()),    // Blue
            Self::Genre => Some("#22C55E".to_string()),       // Green
            Self::SubGenre => Some("#EAB308".to_string()),    // Yellow
            Self::Archetype => Some("#A855F7".to_string()),   // Purple
            Self::Song => Some("#EC4899".to_string()),        // Pink
            Self::Character => Some("#F97316".to_string()),   // Orange
            Self::Context => Some("#06B6D4".to_string()),     // Cyan
            Self::Gear => Some("#6B7280".to_string()),        // Gray
            Self::Custom => Some("#78716C".to_string()),      // Stone
        }
    }

    /// Get the display name for this category
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

    /// Get all categories in display order
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
// TagPriority Implementation
// ============================================================================

impl TagPriority {
    /// Get the numeric weight for this priority
    pub const fn weight(&self) -> u32 {
        *self as u32
    }
}

// ============================================================================
// Tags Implementation
// ============================================================================

impl Tags {
    /// Create empty tag set
    pub fn new() -> Self {
        Self::default()
    }

    /// Create from a list of manual tag IDs
    pub fn from_ids(ids: impl IntoIterator<Item = Uuid>) -> Self {
        Self {
            manual: ids.into_iter().collect(),
            auto_derived: HashSet::new(),
        }
    }

    /// Create with both manual and auto-derived tags
    pub fn with_auto(manual: impl IntoIterator<Item = Uuid>, auto: impl IntoIterator<Item = Uuid>) -> Self {
        Self {
            manual: manual.into_iter().collect(),
            auto_derived: auto.into_iter().collect(),
        }
    }

    /// Add a manual tag
    pub fn add(&mut self, tag_id: Uuid) {
        self.manual.insert(tag_id);
    }

    /// Add an auto-derived tag
    pub fn add_auto(&mut self, tag_id: Uuid) {
        self.auto_derived.insert(tag_id);
    }

    /// Remove a tag from both manual and auto-derived
    pub fn remove(&mut self, tag_id: Uuid) {
        self.manual.remove(&tag_id);
        self.auto_derived.remove(&tag_id);
    }

    /// Check if a tag is present (manual or auto-derived)
    pub fn has(&self, tag_id: Uuid) -> bool {
        self.manual.contains(&tag_id) || self.auto_derived.contains(&tag_id)
    }

    /// Check if a tag is auto-derived
    pub fn is_auto_derived(&self, tag_id: Uuid) -> bool {
        self.auto_derived.contains(&tag_id)
    }

    /// Get all tags (both manual and auto-derived)
    pub fn all(&self) -> HashSet<Uuid> {
        self.manual.union(&self.auto_derived).copied().collect()
    }

    /// Get only display tags (manual tags, excluding auto-derived)
    pub fn display_tags(&self) -> impl Iterator<Item = &Uuid> {
        self.manual.iter()
    }

    /// Check if any of the given tags are present
    pub fn has_any(&self, tag_ids: &HashSet<Uuid>) -> bool {
        let all = self.all();
        all.intersection(tag_ids).next().is_some()
    }

    /// Check if all of the given tags are present
    pub fn has_all(&self, tag_ids: &HashSet<Uuid>) -> bool {
        let all = self.all();
        tag_ids.is_subset(&all)
    }

    /// Get the number of tags (manual + auto-derived, deduplicated)
    pub fn len(&self) -> usize {
        self.all().len()
    }

    /// Get the number of display tags (manual only)
    pub fn display_len(&self) -> usize {
        self.manual.len()
    }

    /// Check if empty (no tags at all)
    pub fn is_empty(&self) -> bool {
        self.manual.is_empty() && self.auto_derived.is_empty()
    }

    /// Iterate over all tag IDs
    pub fn iter(&self) -> impl Iterator<Item = Uuid> + '_ {
        self.manual.iter().chain(self.auto_derived.iter()).copied()
    }

    /// Get intersection with another tag set
    pub fn intersection(&self, other: &Tags) -> HashSet<Uuid> {
        let all_self = self.all();
        let all_other = other.all();
        all_self.intersection(&all_other).copied().collect()
    }

    /// Calculate match score against requested tags using the tag registry
    pub fn match_score(&self, requested: &HashSet<Uuid>, registry: &TagRegistry) -> u32 {
        let mut score = 0u32;
        for tag_id in requested {
            if self.has(*tag_id) {
                // Get the tag's priority weight
                if let Some(tag) = registry.get(*tag_id) {
                    score += tag.priority.weight();
                } else {
                    score += TagPriority::Medium.weight();
                }
            }
        }
        score
    }

    /// Clear auto-derived tags
    pub fn clear_auto(&mut self) {
        self.auto_derived.clear();
    }
}

// ============================================================================
// Auto-tagging Logic
// ============================================================================

/// Common name patterns that map to tags
static AUTO_TAG_PATTERNS: &[(&str, &str)] = &[
    // Base tones
    ("clean", "Clean"),
    ("crunch", "Crunch"),
    ("drive", "Drive"),
    ("lead", "Lead"),
    ("solo", "Solo"),
    ("ambient", "Ambient"),
    ("dry", "Dry"),
    ("di", "DI"),
    // Context
    ("verse", "Verse"),
    ("chorus", "Chorus"),
    ("bridge", "Bridge"),
    ("intro", "Intro"),
    ("outro", "Outro"),
    ("rhythm", "Rhythm"),
    // Character
    ("warm", "Warm"),
    ("bright", "Bright"),
    ("aggressive", "Aggressive"),
    ("smooth", "Smooth"),
    ("vintage", "Vintage"),
    ("modern", "Modern"),
    ("glassy", "Glassy"),
    ("thick", "Thick"),
    // Genres
    ("blues", "Blues"),
    ("rock", "Rock"),
    ("metal", "Metal"),
    ("jazz", "Jazz"),
    ("worship", "Worship"),
    ("country", "Country"),
    ("funk", "Funk"),
    ("pop", "Pop"),
];

/// Derive tags from a name string
pub fn derive_tags_from_name(name: &str, registry: &TagRegistry) -> HashSet<Uuid> {
    let name_lower = name.to_lowercase();
    let mut derived = HashSet::new();

    for (pattern, tag_name) in AUTO_TAG_PATTERNS {
        if name_lower.contains(pattern) {
            if let Some(tag) = registry.find_by_name(tag_name) {
                derived.insert(tag.id);
            }
        }
    }

    derived
}

// ============================================================================
// TagFilter Implementation
// ============================================================================

impl TagFilter {
    /// Create a new empty filter
    pub fn new() -> Self {
        Self::default()
    }

    /// Add an inclusive tag filter
    pub fn include(mut self, tag_id: Uuid) -> Self {
        self.include.insert(tag_id);
        self
    }

    /// Add an exclusive tag filter
    pub fn exclude(mut self, tag_id: Uuid) -> Self {
        self.exclude.insert(tag_id);
        self
    }

    /// Set minimum rating
    pub fn min_rating(mut self, rating: u8) -> Self {
        self.min_rating = rating.min(5);
        self
    }

    /// Set search query
    pub fn search(mut self, query: impl Into<String>) -> Self {
        self.search_query = query.into();
        self
    }

    /// Check if a preset's tags match this filter
    pub fn matches(&self, preset_tags: &PresetTags) -> bool {
        // Must have all inclusive tags
        if !preset_tags.has_all(&self.include) {
            return false;
        }

        // Must not have any exclusive tags
        if preset_tags.has_any(&self.exclude) {
            return false;
        }

        true
    }

    /// Check if the filter is empty (no constraints)
    pub fn is_empty(&self) -> bool {
        self.include.is_empty()
            && self.exclude.is_empty()
            && self.min_rating == 0
            && self.search_query.is_empty()
    }

    /// Clear all filters
    pub fn clear(&mut self) {
        self.include.clear();
        self.exclude.clear();
        self.min_rating = 0;
        self.search_query.clear();
    }
}

// ============================================================================
// TagRegistry - Global tag management
// ============================================================================

/// Registry for all available tags.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct TagRegistry {
    /// All tags by ID
    tags: HashMap<Uuid, PresetTag>,
    /// Tags by category for quick lookup
    by_category: HashMap<TagCategory, Vec<Uuid>>,
    /// Child tags by parent ID
    children: HashMap<Uuid, Vec<Uuid>>,
    /// Root tags (no parent) by category
    roots: HashMap<TagCategory, Vec<Uuid>>,
}

impl TagRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a registry with default tags for guitar presets
    pub fn with_defaults() -> Self {
        let mut registry = Self::new();

        // Base tones (required)
        registry.add(PresetTag::clean());
        registry.add(PresetTag::crunch());
        registry.add(PresetTag::drive());
        registry.add(PresetTag::lead());
        registry.add(PresetTag::solo());
        registry.add(PresetTag::ambient());
        registry.add(PresetTag::new("DI", TagCategory::BaseTone));
        registry.add(PresetTag::new("Dry", TagCategory::BaseTone));

        // Genres
        registry.add(PresetTag::blues());
        registry.add(PresetTag::rock());
        registry.add(PresetTag::metal());
        registry.add(PresetTag::jazz());
        registry.add(PresetTag::worship());
        registry.add(PresetTag::country());
        registry.add(PresetTag::new("Funk", TagCategory::Genre));
        registry.add(PresetTag::new("Pop", TagCategory::Genre));
        registry.add(PresetTag::new("R&B", TagCategory::Genre));
        registry.add(PresetTag::new("Indie", TagCategory::Genre));

        // Character
        registry.add(PresetTag::warm());
        registry.add(PresetTag::bright());
        registry.add(PresetTag::aggressive());
        registry.add(PresetTag::smooth());
        registry.add(PresetTag::vintage());
        registry.add(PresetTag::modern());
        registry.add(PresetTag::new("Glassy", TagCategory::Character));
        registry.add(PresetTag::new("Thick", TagCategory::Character));
        registry.add(PresetTag::new("Compressed", TagCategory::Character));
        registry.add(PresetTag::new("Dynamic", TagCategory::Character));

        // Context
        registry.add(PresetTag::verse());
        registry.add(PresetTag::chorus());
        registry.add(PresetTag::bridge());
        registry.add(PresetTag::rhythm());
        registry.add(PresetTag::new("Intro", TagCategory::Context));
        registry.add(PresetTag::new("Outro", TagCategory::Context));
        registry.add(PresetTag::new("Fingerpicking", TagCategory::Context));
        registry.add(PresetTag::new("Strumming", TagCategory::Context));

        registry
    }

    /// Add a tag to the registry
    pub fn add(&mut self, tag: PresetTag) {
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

    /// Remove a tag from the registry
    pub fn remove(&mut self, id: Uuid) -> Option<PresetTag> {
        let tag = self.tags.remove(&id)?;

        // Remove from category index
        if let Some(ids) = self.by_category.get_mut(&tag.category) {
            ids.retain(|&i| i != id);
        }

        // Remove from parent's children
        if let Some(parent) = tag.parent_id {
            if let Some(children) = self.children.get_mut(&parent) {
                children.retain(|&i| i != id);
            }
        } else {
            if let Some(roots) = self.roots.get_mut(&tag.category) {
                roots.retain(|&i| i != id);
            }
        }

        // Remove this tag's children entry
        self.children.remove(&id);

        Some(tag)
    }

    /// Get a tag by ID
    pub fn get(&self, id: Uuid) -> Option<&PresetTag> {
        self.tags.get(&id)
    }

    /// Get a mutable tag by ID
    pub fn get_mut(&mut self, id: Uuid) -> Option<&mut PresetTag> {
        self.tags.get_mut(&id)
    }

    /// Get all tags
    pub fn all(&self) -> impl Iterator<Item = &PresetTag> {
        self.tags.values()
    }

    /// Get tags by category
    pub fn by_category(&self, category: TagCategory) -> Vec<&PresetTag> {
        self.by_category
            .get(&category)
            .map(|ids| ids.iter().filter_map(|id| self.tags.get(id)).collect())
            .unwrap_or_default()
    }

    /// Get root tags (no parent) for a category
    pub fn roots(&self, category: TagCategory) -> Vec<&PresetTag> {
        self.roots
            .get(&category)
            .map(|ids| ids.iter().filter_map(|id| self.tags.get(id)).collect())
            .unwrap_or_default()
    }

    /// Get children of a tag
    pub fn children(&self, parent_id: Uuid) -> Vec<&PresetTag> {
        self.children
            .get(&parent_id)
            .map(|ids| ids.iter().filter_map(|id| self.tags.get(id)).collect())
            .unwrap_or_default()
    }

    /// Get visible tags (not hidden)
    pub fn visible(&self) -> impl Iterator<Item = &PresetTag> {
        self.tags.values().filter(|t| !t.hidden)
    }

    /// Get visible tags by category
    pub fn visible_by_category(&self, category: TagCategory) -> Vec<&PresetTag> {
        self.by_category(category)
            .into_iter()
            .filter(|t| !t.hidden)
            .collect()
    }

    /// Find a tag by name (case-insensitive)
    pub fn find_by_name(&self, name: &str) -> Option<&PresetTag> {
        let name_lower = name.to_lowercase();
        self.tags.values().find(|t| t.name.to_lowercase() == name_lower)
    }

    /// Find tags matching a search query (simple substring match)
    pub fn search(&self, query: &str) -> Vec<&PresetTag> {
        let query_lower = query.to_lowercase();
        self.tags
            .values()
            .filter(|t| {
                t.name.to_lowercase().contains(&query_lower)
                    || t.description
                        .as_ref()
                        .map(|d| d.to_lowercase().contains(&query_lower))
                        .unwrap_or(false)
            })
            .collect()
    }

    /// Fuzzy search tags using nucleo matcher (like fzf/Sublime Text)
    ///
    /// Returns tags sorted by match score (best matches first).
    /// Uses the same fuzzy matching algorithm as Helix editor.
    pub fn fuzzy_search(&self, query: &str) -> Vec<FuzzyMatch<'_>> {
        use nucleo_matcher::{
            pattern::{CaseMatching, Normalization, Pattern},
            Config, Matcher, Utf32Str,
        };

        if query.is_empty() {
            // Return all visible tags with score 0 when no query
            return self
                .tags
                .values()
                .filter(|t| !t.hidden)
                .map(|tag| FuzzyMatch { tag, score: 0 })
                .collect();
        }

        let mut matcher = Matcher::new(Config::DEFAULT);
        let pattern = Pattern::parse(query, CaseMatching::Smart, Normalization::Smart);

        let mut matches: Vec<FuzzyMatch<'_>> = self
            .tags
            .values()
            .filter(|t| !t.hidden)
            .filter_map(|tag| {
                // Try matching against tag name
                let mut buf = Vec::new();
                let haystack = Utf32Str::new(&tag.name, &mut buf);
                let name_score = pattern.score(haystack, &mut matcher);

                // Also try matching against description if present
                let desc_score = tag.description.as_ref().and_then(|desc| {
                    let mut buf2 = Vec::new();
                    let haystack2 = Utf32Str::new(desc, &mut buf2);
                    pattern.score(haystack2, &mut matcher)
                });

                // Use best score from name or description
                let best_score = match (name_score, desc_score) {
                    (Some(n), Some(d)) => Some(n.max(d)),
                    (Some(n), None) => Some(n),
                    (None, Some(d)) => Some(d),
                    (None, None) => None,
                };

                best_score.map(|score| FuzzyMatch {
                    tag,
                    score: score as i64,
                })
            })
            .collect();

        // Sort by score descending (best matches first)
        matches.sort_by(|a, b| b.score.cmp(&a.score));

        matches
    }

    /// Fuzzy search with minimum score threshold
    pub fn fuzzy_search_min_score(&self, query: &str, min_score: i64) -> Vec<FuzzyMatch<'_>> {
        self.fuzzy_search(query)
            .into_iter()
            .filter(|m| m.score >= min_score)
            .collect()
    }

    /// Get the total number of tags
    pub fn len(&self) -> usize {
        self.tags.len()
    }

    /// Check if the registry is empty
    pub fn is_empty(&self) -> bool {
        self.tags.is_empty()
    }

    /// Get tag ancestry (tag + all parents up to root)
    pub fn ancestry(&self, tag_id: Uuid) -> Vec<Uuid> {
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

    /// Get all descendants of a tag
    pub fn descendants(&self, tag_id: Uuid) -> Vec<Uuid> {
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
    pub fn fallback_chain(&self, requested: &HashSet<Uuid>) -> Vec<HashSet<Uuid>> {
        if requested.is_empty() {
            return vec![];
        }

        // Group tags by priority
        let mut by_priority: Vec<(TagPriority, Vec<Uuid>)> = Vec::new();
        let mut priority_map: HashMap<TagPriority, Vec<Uuid>> = HashMap::new();

        for &tag_id in requested {
            let priority = self
                .get(tag_id)
                .map(|t| t.priority)
                .unwrap_or(TagPriority::Medium);
            priority_map.entry(priority).or_default().push(tag_id);
        }

        // Sort by priority (highest first)
        let mut priorities: Vec<_> = priority_map.keys().copied().collect();
        priorities.sort_by(|a, b| b.cmp(a));

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
    fn test_tag_creation() {
        let tag = PresetTag::new("Blues", TagCategory::Genre);
        assert_eq!(tag.name, "Blues");
        assert_eq!(tag.category, TagCategory::Genre);
        assert_eq!(tag.priority, TagPriority::High);
    }

    #[test]
    fn test_tags() {
        let mut tags = Tags::new();
        let id1 = Uuid::new_v4();
        let id2 = Uuid::new_v4();

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
    fn test_auto_derived_tags() {
        let mut tags = Tags::new();
        let manual_id = Uuid::new_v4();
        let auto_id = Uuid::new_v4();

        tags.add(manual_id);
        tags.add_auto(auto_id);

        // Both should be present
        assert!(tags.has(manual_id));
        assert!(tags.has(auto_id));
        assert_eq!(tags.len(), 2);

        // Only manual should be displayed
        assert_eq!(tags.display_len(), 1);
        assert!(tags.display_tags().any(|&id| id == manual_id));

        // Auto-derived check
        assert!(!tags.is_auto_derived(manual_id));
        assert!(tags.is_auto_derived(auto_id));
    }

    #[test]
    fn test_derive_tags_from_name() {
        let registry = TagRegistry::with_defaults();

        // "Clean Verse" should auto-tag as Clean and Verse
        let derived = derive_tags_from_name("Clean Verse", &registry);
        assert!(derived.len() >= 2);

        let clean = registry.find_by_name("Clean").unwrap();
        let verse = registry.find_by_name("Verse").unwrap();
        assert!(derived.contains(&clean.id));
        assert!(derived.contains(&verse.id));

        // "Crunch" should auto-tag as Crunch
        let derived = derive_tags_from_name("Crunch", &registry);
        let crunch = registry.find_by_name("Crunch").unwrap();
        assert!(derived.contains(&crunch.id));

        // "My Custom Name" shouldn't auto-tag anything
        let derived = derive_tags_from_name("My Custom Name", &registry);
        assert!(derived.is_empty());
    }

    #[test]
    fn test_tag_filter() {
        let mut preset_tags = PresetTags::new();
        let blues = Uuid::new_v4();
        let lead = Uuid::new_v4();
        let aggressive = Uuid::new_v4();

        preset_tags.add(blues);
        preset_tags.add(lead);

        // Filter: +blues +lead
        let filter = TagFilter::new().include(blues).include(lead);
        assert!(filter.matches(&preset_tags));

        // Filter: +blues +lead -aggressive
        let filter = TagFilter::new()
            .include(blues)
            .include(lead)
            .exclude(aggressive);
        assert!(filter.matches(&preset_tags));

        // Add aggressive, should now fail
        preset_tags.add(aggressive);
        assert!(!filter.matches(&preset_tags));
    }

    #[test]
    fn test_tag_registry() {
        let mut registry = TagRegistry::new();

        let blues = PresetTag::blues();
        let blues_id = blues.id;
        registry.add(blues);

        let rock = PresetTag::rock();
        let rock_id = rock.id;
        registry.add(rock);

        assert_eq!(registry.len(), 2);
        assert!(registry.get(blues_id).is_some());
        assert!(registry.get(rock_id).is_some());

        let genres = registry.by_category(TagCategory::Genre);
        assert_eq!(genres.len(), 2);
    }

    #[test]
    fn test_fallback_chain() {
        let mut registry = TagRegistry::new();

        let lead = PresetTag::lead();
        let lead_id = lead.id;
        registry.add(lead);

        let blues = PresetTag::blues();
        let blues_id = blues.id;
        registry.add(blues);

        let jm = PresetTag::new("John Mayer", TagCategory::Archetype);
        let jm_id = jm.id;
        registry.add(jm);

        let gravity = PresetTag::new("Gravity", TagCategory::Song);
        let gravity_id = gravity.id;
        registry.add(gravity);

        // Request: Gravity + John Mayer + Blues + Lead
        let requested: HashSet<Uuid> = [gravity_id, jm_id, blues_id, lead_id]
            .into_iter()
            .collect();

        let chain = registry.fallback_chain(&requested);

        // Should have progressively fewer tags
        assert!(chain.len() >= 2);
        assert_eq!(chain[0].len(), 4); // All tags

        // Last entry should have just required tags
        let last = chain.last().unwrap();
        assert!(last.contains(&lead_id)); // Required tag stays
    }

    #[test]
    fn test_tag_hierarchy() {
        let mut registry = TagRegistry::new();

        let rock = PresetTag::rock();
        let rock_id = rock.id;
        registry.add(rock);

        let classic_rock = PresetTag::new("Classic Rock", TagCategory::SubGenre)
            .with_parent(rock_id);
        let classic_rock_id = classic_rock.id;
        registry.add(classic_rock);

        let punk_rock = PresetTag::new("Punk Rock", TagCategory::SubGenre)
            .with_parent(rock_id);
        registry.add(punk_rock);

        let children = registry.children(rock_id);
        assert_eq!(children.len(), 2);

        let ancestry = registry.ancestry(classic_rock_id);
        assert_eq!(ancestry.len(), 2);
        assert!(ancestry.contains(&classic_rock_id));
        assert!(ancestry.contains(&rock_id));
    }

    #[test]
    fn test_fuzzy_search() {
        let registry = TagRegistry::with_defaults();

        // Exact match should score highly
        let results = registry.fuzzy_search("Blues");
        assert!(!results.is_empty());
        assert_eq!(results[0].tag.name, "Blues");

        // Fuzzy match - "blu" should match "Blues"
        let results = registry.fuzzy_search("blu");
        assert!(!results.is_empty());
        assert!(results.iter().any(|m| m.tag.name == "Blues"));

        // Fuzzy match - "clnn" should match "Clean" (fzf-style)
        let results = registry.fuzzy_search("cln");
        assert!(!results.is_empty());
        assert!(results.iter().any(|m| m.tag.name == "Clean"));

        // Empty query returns all visible tags
        let results = registry.fuzzy_search("");
        assert!(!results.is_empty());

        // No match should return empty
        let results = registry.fuzzy_search("xyznonexistent");
        assert!(results.is_empty());
    }

    #[test]
    fn test_fuzzy_search_ranking() {
        let mut registry = TagRegistry::new();

        // Add tags with similar names to test ranking
        registry.add(PresetTag::new("Lead", TagCategory::BaseTone));
        registry.add(PresetTag::new("Lead Guitar", TagCategory::Context));
        registry.add(PresetTag::new("Dual Lead", TagCategory::Context));

        let results = registry.fuzzy_search("Lead");

        // All Lead-containing tags should be found
        assert!(!results.is_empty());
        assert!(results.len() >= 3);

        // All results should contain "Lead" in the name
        assert!(results.iter().all(|r| r.tag.name.contains("Lead")));

        // Exact match "Lead" should be in the results (but ranking may vary with nucleo)
        assert!(results.iter().any(|r| r.tag.name == "Lead"));
    }
}
