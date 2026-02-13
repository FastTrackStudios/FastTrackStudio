//! Tag suggestions — co-occurrence based tag recommendations.
//!
//! [`TagCooccurrence`] tracks which tags appear together across items,
//! then uses conditional probability to suggest complementary tags.

use std::collections::HashMap;

use crate::id::TagId;
use crate::tags::Tags;

// ─── TagCooccurrence ─────────────────────────────────────────────

/// Tracks which tags appear together across a collection of items.
///
/// Pair keys are stored in canonical order `(min, max)` to avoid duplicates.
/// Use [`suggest()`](Self::suggest) to get ranked recommendations based on
/// conditional probability `P(suggested | current_tags)`.
#[derive(Debug, Clone, Default)]
pub struct TagCooccurrence {
    /// Count of times each pair appears together. Key: `(min(a,b), max(a,b))`.
    pair_counts: HashMap<(TagId, TagId), u32>,
    /// Count of times each individual tag appears.
    tag_counts: HashMap<TagId, u32>,
    /// Total number of items recorded.
    total_items: u32,
}

impl TagCooccurrence {
    /// Create an empty co-occurrence tracker.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record one item's tags, incrementing all pair and individual counts.
    pub fn record(&mut self, tags: &Tags) {
        let all: Vec<TagId> = tags.iter().collect();
        if all.is_empty() {
            self.total_items += 1;
            return;
        }

        self.total_items += 1;

        // Increment individual counts
        for &tag in &all {
            *self.tag_counts.entry(tag).or_insert(0) += 1;
        }

        // Increment pair counts (canonical order)
        for i in 0..all.len() {
            for j in (i + 1)..all.len() {
                let pair = canonical_pair(all[i], all[j]);
                *self.pair_counts.entry(pair).or_insert(0) += 1;
            }
        }
    }

    /// Build from an iterator of tag sets.
    pub fn build_from<'a>(items: impl Iterator<Item = &'a Tags>) -> Self {
        let mut co = Self::new();
        for tags in items {
            co.record(tags);
        }
        co
    }

    /// Suggest tags that commonly appear alongside the current tags.
    ///
    /// Returns up to `limit` suggestions ranked by conditional probability
    /// `P(suggested | current)`, excluding tags already in `current`.
    pub fn suggest(
        &self,
        current: &std::collections::HashSet<TagId>,
        limit: usize,
    ) -> Vec<(TagId, f64)> {
        if current.is_empty() || self.total_items == 0 {
            // Convert (TagId, u32) counts to (TagId, f64) probabilities
            return self
                .most_common(limit)
                .into_iter()
                .map(|(id, count)| (id, count as f64 / self.total_items.max(1) as f64))
                .collect();
        }

        // For each candidate tag not in current, compute average conditional probability
        let mut scores: HashMap<TagId, f64> = HashMap::new();

        for &current_tag in current {
            let current_count = self.tag_counts.get(&current_tag).copied().unwrap_or(0);
            if current_count == 0 {
                continue;
            }

            // Find all tags that co-occur with current_tag
            for (&pair, &count) in &self.pair_counts {
                let other = if pair.0 == current_tag {
                    pair.1
                } else if pair.1 == current_tag {
                    pair.0
                } else {
                    continue;
                };

                if current.contains(&other) {
                    continue; // Skip already-present tags
                }

                // P(other | current_tag) = co-occurrence / current_tag count
                let prob = count as f64 / current_count as f64;
                *scores.entry(other).or_insert(0.0) += prob;
            }
        }

        // Average across current tags
        let current_count = current.len() as f64;
        let mut results: Vec<(TagId, f64)> = scores
            .into_iter()
            .map(|(id, total_prob)| (id, total_prob / current_count))
            .collect();

        // Sort by probability descending
        results.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        results.truncate(limit);
        results
    }

    /// Get the most commonly used tags globally.
    pub fn most_common(&self, limit: usize) -> Vec<(TagId, u32)> {
        let mut counts: Vec<(TagId, u32)> = self.tag_counts.iter().map(|(&k, &v)| (k, v)).collect();
        counts.sort_by(|a, b| b.1.cmp(&a.1));
        counts.truncate(limit);
        // Convert to f64 for consistent API
        counts
    }

    /// Get tags most associated with a single tag.
    ///
    /// Returns up to `limit` tags ranked by co-occurrence frequency
    /// relative to the given tag's total count.
    pub fn related(&self, tag_id: TagId, limit: usize) -> Vec<(TagId, f64)> {
        let tag_count = self.tag_counts.get(&tag_id).copied().unwrap_or(0);
        if tag_count == 0 {
            return Vec::new();
        }

        let mut related: Vec<(TagId, f64)> = self
            .pair_counts
            .iter()
            .filter_map(|(&pair, &count)| {
                let other = if pair.0 == tag_id {
                    Some(pair.1)
                } else if pair.1 == tag_id {
                    Some(pair.0)
                } else {
                    None
                };
                other.map(|o| (o, count as f64 / tag_count as f64))
            })
            .collect();

        related.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        related.truncate(limit);
        related
    }

    /// Total number of items recorded.
    pub fn total_items(&self) -> u32 {
        self.total_items
    }

    /// Number of unique tags seen.
    pub fn unique_tags(&self) -> usize {
        self.tag_counts.len()
    }
}

/// Create a canonical pair key (deterministic ordering by UUID bytes).
fn canonical_pair(a: TagId, b: TagId) -> (TagId, TagId) {
    if a.as_uuid().as_bytes() <= b.as_uuid().as_bytes() {
        (a, b)
    } else {
        (b, a)
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    fn make_tags(ids: &[TagId]) -> Tags {
        Tags::from_ids(ids.iter().copied())
    }

    #[test]
    fn empty_cooccurrence() {
        let co = TagCooccurrence::new();
        assert_eq!(co.total_items(), 0);
        assert_eq!(co.unique_tags(), 0);
        assert!(co.suggest(&HashSet::new(), 5).is_empty());
    }

    #[test]
    fn record_single_item() {
        let a = TagId::new();
        let b = TagId::new();
        let mut co = TagCooccurrence::new();
        co.record(&make_tags(&[a, b]));

        assert_eq!(co.total_items(), 1);
        assert_eq!(co.unique_tags(), 2);
    }

    #[test]
    fn suggest_based_on_cooccurrence() {
        let blues = TagId::new();
        let clean = TagId::new();
        let warm = TagId::new();
        let metal = TagId::new();

        let mut co = TagCooccurrence::new();

        // Blues + Clean appear together 10 times
        for _ in 0..10 {
            co.record(&make_tags(&[blues, clean]));
        }

        // Blues + Warm appear together 5 times
        for _ in 0..5 {
            co.record(&make_tags(&[blues, warm]));
        }

        // Metal + Clean appear together 2 times
        for _ in 0..2 {
            co.record(&make_tags(&[metal, clean]));
        }

        // Given blues, suggest should rank clean > warm
        let current: HashSet<TagId> = [blues].into_iter().collect();
        let suggestions = co.suggest(&current, 10);

        assert!(suggestions.len() >= 2);
        // Clean should be first (higher co-occurrence with blues)
        assert_eq!(suggestions[0].0, clean);
        assert_eq!(suggestions[1].0, warm);
    }

    #[test]
    fn suggest_excludes_already_present() {
        let a = TagId::new();
        let b = TagId::new();
        let c = TagId::new();

        let mut co = TagCooccurrence::new();
        for _ in 0..10 {
            co.record(&make_tags(&[a, b, c]));
        }

        let current: HashSet<TagId> = [a, b].into_iter().collect();
        let suggestions = co.suggest(&current, 10);

        // Should only suggest c (a and b are already present)
        assert_eq!(suggestions.len(), 1);
        assert_eq!(suggestions[0].0, c);
    }

    #[test]
    fn most_common_ordering() {
        let a = TagId::new();
        let b = TagId::new();

        let mut co = TagCooccurrence::new();
        for _ in 0..10 {
            co.record(&make_tags(&[a]));
        }
        for _ in 0..5 {
            co.record(&make_tags(&[b]));
        }

        let common = co.most_common(10);
        assert_eq!(common.len(), 2);
        assert_eq!(common[0].0, a);
        assert_eq!(common[0].1, 10);
        assert_eq!(common[1].0, b);
        assert_eq!(common[1].1, 5);
    }

    #[test]
    fn related_tags() {
        let blues = TagId::new();
        let clean = TagId::new();
        let warm = TagId::new();

        let mut co = TagCooccurrence::new();
        for _ in 0..10 {
            co.record(&make_tags(&[blues, clean]));
        }
        for _ in 0..3 {
            co.record(&make_tags(&[blues, warm]));
        }

        let related = co.related(blues, 10);
        assert_eq!(related.len(), 2);
        assert_eq!(related[0].0, clean); // Higher co-occurrence
        assert_eq!(related[1].0, warm);
    }

    #[test]
    fn related_unknown_tag() {
        let co = TagCooccurrence::new();
        let unknown = TagId::new();
        assert!(co.related(unknown, 10).is_empty());
    }

    #[test]
    fn build_from_iterator() {
        let a = TagId::new();
        let b = TagId::new();
        let items = vec![make_tags(&[a, b]), make_tags(&[a]), make_tags(&[b])];

        let co = TagCooccurrence::build_from(items.iter());
        assert_eq!(co.total_items(), 3);
        assert_eq!(co.unique_tags(), 2);
    }

    #[test]
    fn empty_tags_recorded_but_not_counted() {
        let mut co = TagCooccurrence::new();
        co.record(&Tags::new());
        assert_eq!(co.total_items(), 1);
        assert_eq!(co.unique_tags(), 0);
    }

    #[test]
    fn suggest_with_empty_current_returns_most_common() {
        let a = TagId::new();
        let mut co = TagCooccurrence::new();
        for _ in 0..5 {
            co.record(&make_tags(&[a]));
        }

        let suggestions = co.suggest(&HashSet::new(), 5);
        // Should fall back to most_common
        assert!(!suggestions.is_empty());
    }

    #[test]
    fn limit_respects_cap() {
        let tags: Vec<TagId> = (0..10).map(|_| TagId::new()).collect();
        let mut co = TagCooccurrence::new();
        co.record(&make_tags(&tags));

        let common = co.most_common(3);
        assert_eq!(common.len(), 3);
    }
}
