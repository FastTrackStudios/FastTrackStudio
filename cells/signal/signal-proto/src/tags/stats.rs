//! Tag statistics — usage analytics for understanding tag health.
//!
//! [`TagStats`] aggregates tag usage across a collection, tracking frequency,
//! coverage, and distribution.

use std::collections::HashMap;

use crate::id::TagId;
use crate::tags::{TagCategory, TagRegistry, Tags};

// ─── TagStats ────────────────────────────────────────────────────

/// Aggregate tag statistics across a collection of items.
#[derive(Debug, Clone, Default)]
pub struct TagStats {
    /// Usage count per tag.
    usage: HashMap<TagId, u32>,
    /// Total number of items recorded.
    total_items: u32,
    /// Number of items with zero tags.
    untagged_items: u32,
    /// Distribution: how many items have exactly N tags.
    tag_count_distribution: HashMap<u32, u32>,
}

impl TagStats {
    /// Create empty stats.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record one item's tags into the statistics.
    pub fn record(&mut self, tags: &Tags) {
        self.total_items += 1;

        let all: Vec<TagId> = tags.iter().collect();
        let count = all.len() as u32;

        if count == 0 {
            self.untagged_items += 1;
        }

        *self.tag_count_distribution.entry(count).or_insert(0) += 1;

        for tag_id in all {
            *self.usage.entry(tag_id).or_insert(0) += 1;
        }
    }

    /// Build statistics from an iterator of tag sets.
    pub fn build_from<'a>(items: impl Iterator<Item = &'a Tags>) -> Self {
        let mut stats = Self::new();
        for tags in items {
            stats.record(tags);
        }
        stats
    }

    /// Get the most-used tags, sorted by frequency descending.
    pub fn most_used(&self, limit: usize) -> Vec<(TagId, u32)> {
        let mut counts: Vec<(TagId, u32)> = self.usage.iter().map(|(&k, &v)| (k, v)).collect();
        counts.sort_by(|a, b| b.1.cmp(&a.1));
        counts.truncate(limit);
        counts
    }

    /// Get the least-used tags (with at least 1 usage), sorted ascending.
    pub fn least_used(&self, limit: usize) -> Vec<(TagId, u32)> {
        let mut counts: Vec<(TagId, u32)> = self.usage.iter().map(|(&k, &v)| (k, v)).collect();
        counts.sort_by(|a, b| a.1.cmp(&b.1));
        counts.truncate(limit);
        counts
    }

    /// Get the usage count for a specific tag.
    pub fn usage_count(&self, tag_id: TagId) -> u32 {
        self.usage.get(&tag_id).copied().unwrap_or(0)
    }

    /// Percentage of items that have at least one tag.
    pub fn coverage(&self) -> f64 {
        if self.total_items == 0 {
            return 0.0;
        }
        (self.total_items - self.untagged_items) as f64 / self.total_items as f64
    }

    /// Average number of tags per item.
    pub fn average_tags_per_item(&self) -> f64 {
        if self.total_items == 0 {
            return 0.0;
        }
        let total_tag_assignments: u64 = self.usage.values().map(|&v| v as u64).sum();
        total_tag_assignments as f64 / self.total_items as f64
    }

    /// Tag usage grouped by category (requires registry to resolve tag IDs).
    pub fn by_category(&self, registry: &TagRegistry) -> HashMap<TagCategory, u32> {
        let mut by_cat: HashMap<TagCategory, u32> = HashMap::new();
        for (&tag_id, &count) in &self.usage {
            if let Some(tag) = registry.get(tag_id) {
                *by_cat.entry(tag.category).or_insert(0) += count;
            }
        }
        by_cat
    }

    /// Find tags in the registry that have zero usage.
    pub fn unused_tags(&self, registry: &TagRegistry) -> Vec<TagId> {
        registry
            .all()
            .filter(|tag| !self.usage.contains_key(&tag.id))
            .map(|tag| tag.id)
            .collect()
    }

    /// Total items recorded.
    pub fn total_items(&self) -> u32 {
        self.total_items
    }

    /// Items with no tags.
    pub fn untagged_items(&self) -> u32 {
        self.untagged_items
    }

    /// Number of unique tags seen.
    pub fn unique_tags(&self) -> usize {
        self.usage.len()
    }

    /// Get the tag count distribution (N tags → item count).
    pub fn distribution(&self) -> &HashMap<u32, u32> {
        &self.tag_count_distribution
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tags(ids: &[TagId]) -> Tags {
        Tags::from_ids(ids.iter().copied())
    }

    #[test]
    fn empty_stats() {
        let stats = TagStats::new();
        assert_eq!(stats.total_items(), 0);
        assert_eq!(stats.untagged_items(), 0);
        assert_eq!(stats.coverage(), 0.0);
        assert_eq!(stats.average_tags_per_item(), 0.0);
    }

    #[test]
    fn single_tagged_item() {
        let a = TagId::new();
        let b = TagId::new();
        let mut stats = TagStats::new();
        stats.record(&make_tags(&[a, b]));

        assert_eq!(stats.total_items(), 1);
        assert_eq!(stats.untagged_items(), 0);
        assert_eq!(stats.coverage(), 1.0);
        assert_eq!(stats.average_tags_per_item(), 2.0);
        assert_eq!(stats.usage_count(a), 1);
        assert_eq!(stats.usage_count(b), 1);
    }

    #[test]
    fn untagged_items() {
        let mut stats = TagStats::new();
        stats.record(&Tags::new());
        stats.record(&Tags::new());
        stats.record(&make_tags(&[TagId::new()]));

        assert_eq!(stats.total_items(), 3);
        assert_eq!(stats.untagged_items(), 2);
        assert!((stats.coverage() - 1.0 / 3.0).abs() < f64::EPSILON);
    }

    #[test]
    fn most_used_ordering() {
        let a = TagId::new();
        let b = TagId::new();
        let mut stats = TagStats::new();

        for _ in 0..10 {
            stats.record(&make_tags(&[a]));
        }
        for _ in 0..5 {
            stats.record(&make_tags(&[b]));
        }

        let most = stats.most_used(10);
        assert_eq!(most[0], (a, 10));
        assert_eq!(most[1], (b, 5));
    }

    #[test]
    fn least_used_ordering() {
        let a = TagId::new();
        let b = TagId::new();
        let mut stats = TagStats::new();

        for _ in 0..10 {
            stats.record(&make_tags(&[a]));
        }
        for _ in 0..2 {
            stats.record(&make_tags(&[b]));
        }

        let least = stats.least_used(10);
        assert_eq!(least[0], (b, 2));
        assert_eq!(least[1], (a, 10));
    }

    #[test]
    fn distribution() {
        let a = TagId::new();
        let b = TagId::new();
        let mut stats = TagStats::new();

        stats.record(&make_tags(&[a, b])); // 2 tags
        stats.record(&make_tags(&[a])); // 1 tag
        stats.record(&make_tags(&[a])); // 1 tag
        stats.record(&Tags::new()); // 0 tags

        let dist = stats.distribution();
        assert_eq!(dist.get(&0), Some(&1));
        assert_eq!(dist.get(&1), Some(&2));
        assert_eq!(dist.get(&2), Some(&1));
    }

    #[test]
    fn unused_tags() {
        let mut registry = TagRegistry::new();
        let used_tag = crate::tags::Tag::blues();
        let used_id = used_tag.id;
        registry.add(used_tag);

        let unused_tag = crate::tags::Tag::metal();
        let unused_id = unused_tag.id;
        registry.add(unused_tag);

        let mut stats = TagStats::new();
        stats.record(&make_tags(&[used_id]));

        let unused = stats.unused_tags(&registry);
        assert!(unused.contains(&unused_id));
        assert!(!unused.contains(&used_id));
    }

    #[test]
    fn by_category() {
        let mut registry = TagRegistry::new();
        let blues = crate::tags::Tag::blues();
        let blues_id = blues.id;
        registry.add(blues);

        let clean = crate::tags::Tag::clean();
        let clean_id = clean.id;
        registry.add(clean);

        let mut stats = TagStats::new();
        stats.record(&make_tags(&[blues_id, clean_id]));
        stats.record(&make_tags(&[blues_id]));

        let by_cat = stats.by_category(&registry);
        assert_eq!(by_cat.get(&TagCategory::Genre), Some(&2));
        assert_eq!(by_cat.get(&TagCategory::BaseTone), Some(&1));
    }

    #[test]
    fn build_from_iterator() {
        let a = TagId::new();
        let items = vec![make_tags(&[a]), make_tags(&[a]), Tags::new()];

        let stats = TagStats::build_from(items.iter());
        assert_eq!(stats.total_items(), 3);
        assert_eq!(stats.usage_count(a), 2);
        assert_eq!(stats.untagged_items(), 1);
    }

    #[test]
    fn average_tags_across_items() {
        let a = TagId::new();
        let b = TagId::new();
        let c = TagId::new();
        let mut stats = TagStats::new();

        stats.record(&make_tags(&[a, b, c])); // 3 tags
        stats.record(&make_tags(&[a])); // 1 tag

        // Total assignments = 4, items = 2, average = 2.0
        assert!((stats.average_tags_per_item() - 2.0).abs() < f64::EPSILON);
    }
}
