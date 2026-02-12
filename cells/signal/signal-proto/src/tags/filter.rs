//! Enhanced filtering — OR-group clauses, sort specifications, and full matching.
//!
//! Extends [`TagFilter`] with:
//! - [`TagClause`] OR-groups: `(Blues|Jazz) AND (Clean|Crunch)`
//! - [`SortField`] + [`SortDirection`] for result ordering
//! - [`matches_full()`](TagFilter::matches_full) that checks tags, rating, and search query

use std::collections::HashSet;

use crate::id::TagId;
use crate::normalized::Rating;
use crate::tags::{TagFilter, Tags};

// ─── TagClause ───────────────────────────────────────────────────

/// An OR-group of tag alternatives.
///
/// Matches if *any* of the `alternatives` are present in the item's tags.
/// Multiple clauses on a [`TagFilter`] are ANDed together:
///
/// ```text
/// clause1: (Blues | Jazz)
/// clause2: (Clean | Crunch)
/// → matches if (Blues OR Jazz) AND (Clean OR Crunch)
/// ```
#[derive(Debug, Clone, Default, PartialEq, Eq, ::facet::Facet)]
pub struct TagClause {
    /// Tag IDs — item must have at least one.
    pub alternatives: HashSet<TagId>,
}

impl TagClause {
    /// Create a new empty clause.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a clause from tag IDs.
    pub fn from_ids(ids: impl IntoIterator<Item = TagId>) -> Self {
        Self {
            alternatives: ids.into_iter().collect(),
        }
    }

    /// Create a clause with a single tag (degenerates to simple AND).
    pub fn single(id: TagId) -> Self {
        let mut alternatives = HashSet::new();
        alternatives.insert(id);
        Self { alternatives }
    }

    /// Create a clause with two alternatives.
    pub fn either(a: TagId, b: TagId) -> Self {
        let mut alternatives = HashSet::new();
        alternatives.insert(a);
        alternatives.insert(b);
        Self { alternatives }
    }

    /// Add an alternative to this clause.
    pub fn or(mut self, id: TagId) -> Self {
        self.alternatives.insert(id);
        self
    }

    /// Check if a tag set satisfies this clause (has at least one alternative).
    pub fn matches(&self, tags: &Tags) -> bool {
        if self.alternatives.is_empty() {
            return true; // Empty clause matches everything
        }
        tags.has_any(&self.alternatives)
    }

    /// Check if the clause is empty (no alternatives — always matches).
    pub fn is_empty(&self) -> bool {
        self.alternatives.is_empty()
    }
}

// ─── SortField ───────────────────────────────────────────────────

/// Sort criteria for preset lists.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, ::facet::Facet)]
#[repr(u8)]
pub enum SortField {
    /// Sort by tag relevance score (requires scoring module).
    #[default]
    Relevance,
    /// Sort alphabetically by name.
    Name,
    /// Sort by star rating.
    Rating,
    /// Sort by creation timestamp.
    DateCreated,
    /// Sort by last modification timestamp.
    DateUpdated,
    /// Sort by preset category level.
    Category,
}

// ─── SortDirection ───────────────────────────────────────────────

/// Sort direction for preset lists.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, ::facet::Facet)]
#[repr(u8)]
pub enum SortDirection {
    /// Highest first (most relevant, highest rated, newest).
    #[default]
    Descending,
    /// Lowest first (A-Z, oldest, least rated).
    Ascending,
}

// ─── TagFilter extensions ────────────────────────────────────────

impl TagFilter {
    /// Check if tags, name, and rating all satisfy this filter.
    ///
    /// This is the complete matching function that checks:
    /// 1. All `include` tags are present (AND logic)
    /// 2. All `include_clauses` are satisfied (each has at least one match)
    /// 3. No `exclude` tags are present
    /// 4. Rating meets `min_rating` threshold (if set)
    /// 5. Name contains `search_query` (if non-empty, case-insensitive)
    pub fn matches_full(&self, tags: &Tags, name: &str, rating: Rating) -> bool {
        // 1. Must have all inclusive tags (existing AND logic)
        if !self.include.is_empty() && !tags.has_all(&self.include) {
            return false;
        }

        // 2. Must satisfy all OR-group clauses
        for clause in &self.include_clauses {
            if !clause.matches(tags) {
                return false;
            }
        }

        // 3. Must not have any exclusive tags
        if !self.exclude.is_empty() && tags.has_any(&self.exclude) {
            return false;
        }

        // 4. Rating threshold
        if self.min_rating.is_rated() && (!rating.is_rated() || rating < self.min_rating) {
            return false;
        }

        // 5. Text search
        if !self.search_query.is_empty()
            && !name
                .to_lowercase()
                .contains(&self.search_query.to_lowercase())
        {
            return false;
        }

        true
    }

    /// Add an OR-group clause to the filter.
    #[must_use]
    pub fn with_clause(mut self, clause: TagClause) -> Self {
        self.include_clauses.push(clause);
        self
    }

    /// Set the sort field.
    #[must_use]
    pub fn sort(mut self, field: SortField) -> Self {
        self.sort_field = field;
        self
    }

    /// Set the sort direction.
    #[must_use]
    pub fn direction(mut self, dir: SortDirection) -> Self {
        self.sort_direction = dir;
        self
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tags(ids: &[TagId]) -> Tags {
        Tags::from_ids(ids.iter().copied())
    }

    // ── TagClause ────────────────────────────────────────────────

    #[test]
    fn empty_clause_matches_everything() {
        let clause = TagClause::new();
        assert!(clause.matches(&Tags::new()));
        assert!(clause.matches(&make_tags(&[TagId::new()])));
    }

    #[test]
    fn single_clause_matches() {
        let id = TagId::new();
        let clause = TagClause::single(id);
        assert!(clause.matches(&make_tags(&[id])));
        assert!(!clause.matches(&Tags::new()));
    }

    #[test]
    fn either_clause_matches_any() {
        let a = TagId::new();
        let b = TagId::new();
        let c = TagId::new();
        let clause = TagClause::either(a, b);

        assert!(clause.matches(&make_tags(&[a])));
        assert!(clause.matches(&make_tags(&[b])));
        assert!(clause.matches(&make_tags(&[a, b])));
        assert!(!clause.matches(&make_tags(&[c])));
    }

    #[test]
    fn clause_or_builder() {
        let a = TagId::new();
        let b = TagId::new();
        let c = TagId::new();
        let clause = TagClause::single(a).or(b).or(c);
        assert_eq!(clause.alternatives.len(), 3);
    }

    #[test]
    fn clause_from_ids() {
        let ids = vec![TagId::new(), TagId::new()];
        let clause = TagClause::from_ids(ids.clone());
        assert_eq!(clause.alternatives.len(), 2);
        assert!(clause.matches(&make_tags(&ids)));
    }

    // ── matches_full: OR-groups ──────────────────────────────────

    #[test]
    fn or_groups_anded_together() {
        let blues = TagId::new();
        let jazz = TagId::new();
        let clean = TagId::new();
        let crunch = TagId::new();

        let filter = TagFilter::new()
            .with_clause(TagClause::either(blues, jazz))
            .with_clause(TagClause::either(clean, crunch));

        // Blues + Clean → matches
        assert!(filter.matches_full(&make_tags(&[blues, clean]), "", Rating::default()));
        // Jazz + Crunch → matches
        assert!(filter.matches_full(&make_tags(&[jazz, crunch]), "", Rating::default()));
        // Blues + Crunch → matches
        assert!(filter.matches_full(&make_tags(&[blues, crunch]), "", Rating::default()));
        // Blues only → fails (missing tone clause)
        assert!(!filter.matches_full(&make_tags(&[blues]), "", Rating::default()));
        // Clean only → fails (missing genre clause)
        assert!(!filter.matches_full(&make_tags(&[clean]), "", Rating::default()));
    }

    #[test]
    fn or_groups_with_include_tags() {
        let blues = TagId::new();
        let warm = TagId::new();
        let clean = TagId::new();
        let crunch = TagId::new();

        let filter = TagFilter::new()
            .include(warm) // Must have warm (AND)
            .with_clause(TagClause::either(clean, crunch)); // Must have clean OR crunch

        assert!(filter.matches_full(&make_tags(&[warm, clean]), "", Rating::default()));
        assert!(filter.matches_full(&make_tags(&[warm, crunch]), "", Rating::default()));
        assert!(!filter.matches_full(&make_tags(&[warm, blues]), "", Rating::default()));
        assert!(!filter.matches_full(&make_tags(&[clean]), "", Rating::default()));
    }

    // ── matches_full: rating ─────────────────────────────────────

    #[test]
    fn rating_threshold() {
        let filter = TagFilter::new().min_rating(Rating::new(3));

        assert!(filter.matches_full(&Tags::new(), "", Rating::new(5)));
        assert!(filter.matches_full(&Tags::new(), "", Rating::new(3)));
        assert!(!filter.matches_full(&Tags::new(), "", Rating::new(2)));
        assert!(!filter.matches_full(&Tags::new(), "", Rating::new(0)));
    }

    #[test]
    fn unrated_filter_matches_any_rating() {
        let filter = TagFilter::new(); // min_rating defaults to 0 (unrated)
        assert!(filter.matches_full(&Tags::new(), "", Rating::new(0)));
        assert!(filter.matches_full(&Tags::new(), "", Rating::new(5)));
    }

    // ── matches_full: search query ───────────────────────────────

    #[test]
    fn search_query_case_insensitive() {
        let filter = TagFilter::new().search("blues");
        assert!(filter.matches_full(&Tags::new(), "My Blues Tone", Rating::default()));
        assert!(filter.matches_full(&Tags::new(), "BLUES LEAD", Rating::default()));
        assert!(!filter.matches_full(&Tags::new(), "Rock Lead", Rating::default()));
    }

    #[test]
    fn empty_search_matches_all() {
        let filter = TagFilter::new();
        assert!(filter.matches_full(&Tags::new(), "Anything", Rating::default()));
        assert!(filter.matches_full(&Tags::new(), "", Rating::default()));
    }

    #[test]
    fn search_query_substring() {
        let filter = TagFilter::new().search("mayer");
        assert!(filter.matches_full(&Tags::new(), "John Mayer Blues", Rating::default()));
        assert!(!filter.matches_full(&Tags::new(), "SRV Blues", Rating::default()));
    }

    // ── matches_full: combined ───────────────────────────────────

    #[test]
    fn combined_tags_rating_search() {
        let blues = TagId::new();
        let metal = TagId::new();

        let filter = TagFilter::new()
            .include(blues)
            .exclude(metal)
            .min_rating(Rating::new(3))
            .search("lead");

        // All criteria met
        assert!(filter.matches_full(&make_tags(&[blues]), "Blues Lead", Rating::new(4),));

        // Missing tag
        assert!(!filter.matches_full(&Tags::new(), "Blues Lead", Rating::new(4),));

        // Has excluded tag
        assert!(!filter.matches_full(&make_tags(&[blues, metal]), "Blues Lead", Rating::new(4),));

        // Low rating
        assert!(!filter.matches_full(&make_tags(&[blues]), "Blues Lead", Rating::new(2),));

        // Name doesn't match search
        assert!(!filter.matches_full(&make_tags(&[blues]), "Blues Rhythm", Rating::new(4),));
    }

    // ── Backward compatibility ───────────────────────────────────

    #[test]
    fn old_matches_still_works() {
        let id = TagId::new();
        let excl = TagId::new();

        let filter = TagFilter::new().include(id).exclude(excl);
        let mut tags = Tags::new();
        tags.add(id);

        // Old matches() only checks include/exclude tags
        assert!(filter.matches(&tags));

        tags.add(excl);
        assert!(!filter.matches(&tags));
    }

    // ── Sort builder ─────────────────────────────────────────────

    #[test]
    fn sort_builder() {
        let filter = TagFilter::new()
            .sort(SortField::Rating)
            .direction(SortDirection::Ascending);

        assert_eq!(filter.sort_field, SortField::Rating);
        assert_eq!(filter.sort_direction, SortDirection::Ascending);
    }

    #[test]
    fn default_sort_is_relevance_descending() {
        let filter = TagFilter::new();
        assert_eq!(filter.sort_field, SortField::Relevance);
        assert_eq!(filter.sort_direction, SortDirection::Descending);
    }
}
