//! Relevance scoring — ranked matching for preset search results.
//!
//! [`MatchScore`] is a normalized relevance value in `[0.0, 1.0]`.
//! [`ScoredMatch`] pairs an item with its score for sorted result lists.

use crate::normalized::Rating;
use crate::tags::{TagFilter, TagPriority, TagRegistry, Tags};

// ─── MatchScore ──────────────────────────────────────────────────

/// A relevance score in `[0.0, 1.0]`.
///
/// `0.0` means no match at all. `1.0` means perfect match.
/// Values between indicate partial relevance.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct MatchScore(f64);

impl MatchScore {
    /// No match.
    pub const ZERO: Self = Self(0.0);
    /// Perfect match.
    pub const PERFECT: Self = Self(1.0);

    /// Create a score, clamped to `[0.0, 1.0]`.
    pub fn new(value: f64) -> Self {
        Self(value.clamp(0.0, 1.0))
    }

    /// Get the raw score value.
    pub fn get(self) -> f64 {
        self.0
    }

    /// Whether this represents any match at all (score > 0.0).
    pub fn is_match(self) -> bool {
        self.0 > 0.0
    }
}

impl Default for MatchScore {
    fn default() -> Self {
        Self::ZERO
    }
}

// ─── ScoredMatch ─────────────────────────────────────────────────

/// An item paired with its relevance score.
#[derive(Debug, Clone)]
pub struct ScoredMatch<T> {
    pub item: T,
    pub score: MatchScore,
}

impl<T> ScoredMatch<T> {
    pub fn new(item: T, score: MatchScore) -> Self {
        Self { item, score }
    }
}

// ─── Scoring methods on TagFilter ────────────────────────────────

/// Points awarded for a name search match (before normalization).
const NAME_MATCH_BONUS: f64 = 20.0;

impl TagFilter {
    /// Score a tag set against this filter's include/exclude constraints.
    ///
    /// Each matching include tag contributes points based on its priority weight.
    /// If any excluded tag is present, returns `MatchScore::ZERO`.
    /// Hierarchy bonus: if a tag's ancestor matches an include tag, adds 50% credit.
    ///
    /// The result is normalized to `[0.0, 1.0]`.
    pub fn score(&self, tags: &Tags, registry: &TagRegistry) -> MatchScore {
        if self.include.is_empty() && self.include_clauses.is_empty() {
            // No include criteria → everything is a perfect match
            return MatchScore::PERFECT;
        }

        // Exclude veto
        if !self.exclude.is_empty() && tags.has_any(&self.exclude) {
            return MatchScore::ZERO;
        }

        // Check clauses — if any clause fails, score is 0
        for clause in &self.include_clauses {
            if !clause.matches(tags) {
                return MatchScore::ZERO;
            }
        }

        if self.include.is_empty() {
            // Only clauses, all passed → perfect
            return MatchScore::PERFECT;
        }

        let all_tags = tags.all();
        let mut earned = 0.0_f64;
        let mut max_possible = 0.0_f64;

        for &include_id in &self.include {
            let weight = registry
                .get(include_id)
                .map(|t| t.priority.weight() as f64)
                .unwrap_or(TagPriority::Medium.weight() as f64);

            max_possible += weight;

            if all_tags.contains(&include_id) {
                // Direct match — full weight
                earned += weight;
            } else {
                // Check hierarchy: does the item have a descendant of the include tag?
                let descendants = registry.descendants(include_id);
                if descendants.iter().any(|d| all_tags.contains(d)) {
                    earned += weight * 0.5; // Hierarchy bonus: 50% credit
                }
            }
        }

        if max_possible == 0.0 {
            return MatchScore::PERFECT;
        }

        MatchScore::new(earned / max_possible)
    }

    /// Score with full context: tags, name, and rating.
    ///
    /// - Tag score: weighted match against include tags (see [`score()`](Self::score))
    /// - Name bonus: +20 normalized points if `search_query` is found in name
    /// - Rating gate: returns 0.0 if rating is below `min_rating`
    pub fn score_full(
        &self,
        tags: &Tags,
        name: &str,
        rating: Rating,
        registry: &TagRegistry,
    ) -> MatchScore {
        // Rating gate
        if self.min_rating.is_rated() && (!rating.is_rated() || rating < self.min_rating) {
            return MatchScore::ZERO;
        }

        let tag_score = self.score(tags, registry);
        if !tag_score.is_match() && (!self.include.is_empty() || !self.include_clauses.is_empty()) {
            return MatchScore::ZERO;
        }

        // Calculate max possible including name bonus
        let max_tag_points: f64 = if self.include.is_empty() {
            100.0 // Default when no includes
        } else {
            self.include
                .iter()
                .map(|&id| {
                    registry
                        .get(id)
                        .map(|t| t.priority.weight() as f64)
                        .unwrap_or(TagPriority::Medium.weight() as f64)
                })
                .sum()
        };

        let max_possible = max_tag_points + NAME_MATCH_BONUS;
        let tag_earned = tag_score.get() * max_tag_points;

        // Name match bonus
        let name_bonus = if !self.search_query.is_empty()
            && name
                .to_lowercase()
                .contains(&self.search_query.to_lowercase())
        {
            NAME_MATCH_BONUS
        } else {
            0.0
        };

        MatchScore::new((tag_earned + name_bonus) / max_possible)
    }

    /// Score and collect matching items, sorted by relevance (highest first).
    pub fn score_and_sort<'a, T>(
        &self,
        items: impl Iterator<Item = &'a T>,
        registry: &TagRegistry,
        get_tags: impl Fn(&T) -> &Tags,
        get_name: impl Fn(&T) -> &str,
        get_rating: impl Fn(&T) -> Rating,
    ) -> Vec<ScoredMatch<&'a T>>
    where
        T: 'a,
    {
        let mut results: Vec<ScoredMatch<&'a T>> = items
            .map(|item| {
                let score =
                    self.score_full(get_tags(item), get_name(item), get_rating(item), registry);
                ScoredMatch::new(item, score)
            })
            .filter(|m| m.score.is_match())
            .collect();

        results.sort_by(|a, b| {
            b.score
                .get()
                .partial_cmp(&a.score.get())
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        results
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id::TagId;
    use crate::tags::{Tag, TagCategory};

    fn setup() -> (TagRegistry, TagId, TagId, TagId, TagId) {
        let mut registry = TagRegistry::new();

        let clean = Tag::clean();
        let clean_id = clean.id;
        registry.add(clean);

        let blues = Tag::blues();
        let blues_id = blues.id;
        registry.add(blues);

        let warm = Tag::warm();
        let warm_id = warm.id;
        registry.add(warm);

        let verse = Tag::verse();
        let verse_id = verse.id;
        registry.add(verse);

        (registry, clean_id, blues_id, warm_id, verse_id)
    }

    fn make_tags(ids: &[TagId]) -> Tags {
        Tags::from_ids(ids.iter().copied())
    }

    // ── MatchScore ───────────────────────────────────────────────

    #[test]
    fn match_score_clamps() {
        assert_eq!(MatchScore::new(-1.0).get(), 0.0);
        assert_eq!(MatchScore::new(2.0).get(), 1.0);
        assert_eq!(MatchScore::new(0.5).get(), 0.5);
    }

    #[test]
    fn match_score_is_match() {
        assert!(!MatchScore::ZERO.is_match());
        assert!(MatchScore::PERFECT.is_match());
        assert!(MatchScore::new(0.01).is_match());
    }

    // ── Tag scoring ──────────────────────────────────────────────

    #[test]
    fn perfect_match_all_tags_present() {
        let (registry, clean_id, blues_id, _, _) = setup();
        let filter = TagFilter::new().include(clean_id).include(blues_id);
        let tags = make_tags(&[clean_id, blues_id]);

        let score = filter.score(&tags, &registry);
        assert!((score.get() - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn partial_match_some_tags() {
        let (registry, clean_id, blues_id, _, _) = setup();
        let filter = TagFilter::new().include(clean_id).include(blues_id);
        let tags = make_tags(&[clean_id]); // Only has clean, missing blues

        let score = filter.score(&tags, &registry);
        assert!(score.is_match());
        assert!(score.get() < 1.0);
        assert!(score.get() > 0.0);
    }

    #[test]
    fn exclude_veto_returns_zero() {
        let (registry, clean_id, blues_id, _, _) = setup();
        let filter = TagFilter::new().include(clean_id).exclude(blues_id);
        let tags = make_tags(&[clean_id, blues_id]);

        let score = filter.score(&tags, &registry);
        assert!(!score.is_match());
    }

    #[test]
    fn no_include_criteria_scores_perfect() {
        let (registry, _, _, _, _) = setup();
        let filter = TagFilter::new();
        let tags = make_tags(&[TagId::new()]);

        let score = filter.score(&tags, &registry);
        assert!((score.get() - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn priority_weighting() {
        let (registry, clean_id, _, warm_id, _) = setup();

        // Clean is BaseTone (Required = 100), Warm is Character (Low = 25)
        let filter_high = TagFilter::new().include(clean_id);
        let filter_low = TagFilter::new().include(warm_id);

        let tags_clean = make_tags(&[clean_id]);
        let tags_warm = make_tags(&[warm_id]);

        // Both should score 1.0 (perfect match on their single include)
        assert!((filter_high.score(&tags_clean, &registry).get() - 1.0).abs() < f64::EPSILON);
        assert!((filter_low.score(&tags_warm, &registry).get() - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn hierarchy_bonus() {
        let mut registry = TagRegistry::new();
        let rock = Tag::rock();
        let rock_id = rock.id;
        registry.add(rock);

        let classic_rock = Tag::new("Classic Rock", TagCategory::SubGenre).with_parent(rock_id);
        let classic_id = classic_rock.id;
        registry.add(classic_rock);

        // Filter asks for "Rock", item has "Classic Rock" (child)
        let filter = TagFilter::new().include(rock_id);
        let tags = make_tags(&[classic_id]);

        let score = filter.score(&tags, &registry);
        assert!(score.is_match());
        assert!(score.get() > 0.0);
        assert!(score.get() < 1.0); // Partial credit, not full
    }

    // ── Full scoring ─────────────────────────────────────────────

    #[test]
    fn rating_gate_blocks_low_rating() {
        let (registry, clean_id, _, _, _) = setup();
        let filter = TagFilter::new()
            .include(clean_id)
            .min_rating(Rating::new(3));
        let tags = make_tags(&[clean_id]);

        assert!(!filter
            .score_full(&tags, "Clean", Rating::new(2), &registry)
            .is_match());
        assert!(filter
            .score_full(&tags, "Clean", Rating::new(3), &registry)
            .is_match());
    }

    #[test]
    fn name_search_bonus() {
        let (registry, clean_id, _, _, _) = setup();
        let filter_with = TagFilter::new().include(clean_id).search("blues");
        let filter_without = TagFilter::new().include(clean_id);
        let tags = make_tags(&[clean_id]);

        let score_with = filter_with.score_full(&tags, "Blues Clean", Rating::default(), &registry);
        let score_without =
            filter_without.score_full(&tags, "Blues Clean", Rating::default(), &registry);

        // Score with matching name search should be >= score without
        assert!(score_with.get() >= score_without.get());
    }

    // ── score_and_sort ───────────────────────────────────────────

    #[test]
    fn score_and_sort_orders_by_relevance() {
        let (registry, clean_id, blues_id, _, _) = setup();
        let filter = TagFilter::new().include(clean_id).include(blues_id);

        struct Item {
            name: String,
            tags: Tags,
            rating: Rating,
        }

        let items = vec![
            Item {
                name: "Only Clean".into(),
                tags: make_tags(&[clean_id]),
                rating: Rating::default(),
            },
            Item {
                name: "Clean Blues".into(),
                tags: make_tags(&[clean_id, blues_id]),
                rating: Rating::default(),
            },
        ];

        let results = filter.score_and_sort(
            items.iter(),
            &registry,
            |i| &i.tags,
            |i| &i.name,
            |i| i.rating,
        );

        assert_eq!(results.len(), 2);
        // Perfect match should be first
        assert!(results[0].score.get() > results[1].score.get());
        assert_eq!(results[0].item.name, "Clean Blues");
    }
}
