//! Auto-tagging engine — infers tags from preset names.
//!
//! The [`AutoTagger`] applies [`TagRule`]s to extract tags from preset names.
//! Rules are matched longest-pattern-first to prevent partial matches
//! (e.g., "Texas Blues" matches before "Blues").
//!
//! # Example
//!
//! ```
//! use signal_proto::tags::auto_tagger::AutoTagger;
//! use signal_proto::tags::{TagRegistry, Tags};
//!
//! let registry = TagRegistry::with_defaults();
//! let tagger = AutoTagger::with_defaults(&registry);
//!
//! let mut tags = Tags::new();
//! tagger.apply_to_tags("Clean Blues Lead Tone", &mut tags);
//!
//! assert!(!tags.is_empty());
//! ```

use std::collections::HashSet;

use crate::id::TagId;
use crate::tags::{deterministic_tag_id, TagCategory, TagRegistry, Taggable, Tags};

// ─── MatchMode ───────────────────────────────────────────────────

/// How a rule matches against preset name tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, ::facet::Facet)]
#[repr(u8)]
pub enum MatchMode {
    /// Whole-word boundary match. Prevents "Cleaning" from matching "Clean".
    #[default]
    Word,
    /// Substring match on the full name. For multi-word patterns like "Texas Blues".
    Contains,
    /// Matches the start of any word token.
    Prefix,
}

// ─── TagRule ─────────────────────────────────────────────────────

/// A single name-to-tag mapping rule.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct TagRule {
    /// Lowercased pattern to match against.
    pub pattern: String,
    /// The tag ID to apply when this rule matches.
    pub tag_id: TagId,
    /// The tag's category (for documentation/debugging).
    pub category: TagCategory,
    /// How to match the pattern.
    pub match_mode: MatchMode,
}

impl TagRule {
    /// Create a new rule.
    pub fn new(
        pattern: impl Into<String>,
        tag_id: TagId,
        category: TagCategory,
        match_mode: MatchMode,
    ) -> Self {
        Self {
            pattern: pattern.into().to_lowercase(),
            tag_id,
            category,
            match_mode,
        }
    }

    /// Create a Word-mode rule.
    pub fn word(pattern: impl Into<String>, tag_id: TagId, category: TagCategory) -> Self {
        Self::new(pattern, tag_id, category, MatchMode::Word)
    }

    /// Create a Contains-mode rule.
    pub fn contains(pattern: impl Into<String>, tag_id: TagId, category: TagCategory) -> Self {
        Self::new(pattern, tag_id, category, MatchMode::Contains)
    }

    /// Create a Prefix-mode rule.
    pub fn prefix(pattern: impl Into<String>, tag_id: TagId, category: TagCategory) -> Self {
        Self::new(pattern, tag_id, category, MatchMode::Prefix)
    }
}

// ─── AutoTagger ──────────────────────────────────────────────────

/// Name-to-tag inference engine.
///
/// Holds a set of [`TagRule`]s sorted by pattern length (longest first) so that
/// multi-word patterns like "Texas Blues" are matched before "Blues".
#[derive(Debug, Clone)]
pub struct AutoTagger {
    rules: Vec<TagRule>,
}

impl AutoTagger {
    /// Create an empty auto-tagger with no rules.
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    /// Create an auto-tagger pre-loaded with rules derived from every tag
    /// in the registry, plus common guitar-domain synonyms.
    pub fn with_defaults(registry: &TagRegistry) -> Self {
        let mut tagger = Self::new();

        // Create a Word rule for every tag in the registry
        for tag in registry.all() {
            tagger
                .rules
                .push(TagRule::word(&tag.name, tag.id, tag.category));
        }

        // ── Synonym table: abbreviations and alternate names ─────────

        // Drive / Overdrive synonyms
        let drive_id = deterministic_tag_id("Drive");
        tagger.add_synonym("od", drive_id, TagCategory::BaseTone);
        tagger.add_synonym("overdrive", drive_id, TagCategory::BaseTone);
        tagger.add_synonym("dist", drive_id, TagCategory::BaseTone);
        tagger.add_synonym("distortion", drive_id, TagCategory::BaseTone);

        // Crunch synonyms
        let crunch_id = deterministic_tag_id("Crunch");
        tagger.add_synonym("grit", crunch_id, TagCategory::BaseTone);
        tagger.add_synonym("gritty", crunch_id, TagCategory::BaseTone);
        tagger.add_synonym("breakup", crunch_id, TagCategory::BaseTone);

        // Clean synonyms
        let clean_id = deterministic_tag_id("Clean");
        tagger.add_synonym("crystal", clean_id, TagCategory::BaseTone);
        tagger.add_synonym("pristine", clean_id, TagCategory::BaseTone);

        // Ambient synonyms
        let ambient_id = deterministic_tag_id("Ambient");
        tagger.add_synonym("atmospheric", ambient_id, TagCategory::BaseTone);
        tagger.add_synonym("pad", ambient_id, TagCategory::BaseTone);
        tagger.add_synonym("soundscape", ambient_id, TagCategory::BaseTone);

        // Character synonyms
        let warm_id = deterministic_tag_id("Warm");
        tagger.add_synonym("fat", warm_id, TagCategory::Character);
        tagger.add_synonym("round", warm_id, TagCategory::Character);

        let bright_id = deterministic_tag_id("Bright");
        tagger.add_synonym("sparkle", bright_id, TagCategory::Character);
        tagger.add_synonym("sparkly", bright_id, TagCategory::Character);
        tagger.add_synonym("chimey", bright_id, TagCategory::Character);
        tagger.add_synonym("chime", bright_id, TagCategory::Character);

        let aggressive_id = deterministic_tag_id("Aggressive");
        tagger.add_synonym("heavy", aggressive_id, TagCategory::Character);
        tagger.add_synonym("brutal", aggressive_id, TagCategory::Character);

        let vintage_id = deterministic_tag_id("Vintage");
        tagger.add_synonym("retro", vintage_id, TagCategory::Character);
        tagger.add_synonym("classic", vintage_id, TagCategory::Character);
        tagger.add_synonym("old school", vintage_id, TagCategory::Character);

        let modern_id = deterministic_tag_id("Modern");
        tagger.add_synonym("contemporary", modern_id, TagCategory::Character);
        tagger.add_synonym("hi-fi", modern_id, TagCategory::Character);

        // Gear synonyms (common pedal/amp names)
        let gear_cat = TagCategory::Gear;
        tagger.add_synonym("plexi", deterministic_tag_id("Plexi"), gear_cat);
        tagger.add_synonym("marshall", deterministic_tag_id("Marshall"), gear_cat);
        tagger.add_synonym("fender", deterministic_tag_id("Fender"), gear_cat);
        tagger.add_synonym("vox", deterministic_tag_id("Vox"), gear_cat);
        tagger.add_synonym("dumble", deterministic_tag_id("Dumble"), gear_cat);
        tagger.add_synonym("mesa", deterministic_tag_id("Mesa"), gear_cat);
        tagger.add_synonym("bogner", deterministic_tag_id("Bogner"), gear_cat);
        tagger.add_synonym("soldano", deterministic_tag_id("Soldano"), gear_cat);
        tagger.add_synonym("klon", deterministic_tag_id("Klon"), gear_cat);
        tagger.add_synonym(
            "tube screamer",
            deterministic_tag_id("Tube Screamer"),
            gear_cat,
        );
        tagger.add_synonym("ts808", deterministic_tag_id("Tube Screamer"), gear_cat);
        tagger.add_synonym("ts-808", deterministic_tag_id("Tube Screamer"), gear_cat);
        tagger.add_synonym("1176", deterministic_tag_id("1176"), gear_cat);
        tagger.add_synonym("la2a", deterministic_tag_id("LA-2A"), gear_cat);
        tagger.add_synonym("la-2a", deterministic_tag_id("LA-2A"), gear_cat);

        // Archetype synonyms (common artist abbreviations)
        let arch_cat = TagCategory::Archetype;
        tagger.add_synonym("srv", deterministic_tag_id("SRV"), arch_cat);
        tagger.add_synonym("stevie ray", deterministic_tag_id("SRV"), arch_cat);
        tagger.add_synonym("jm", deterministic_tag_id("John Mayer"), arch_cat);
        tagger.add_synonym("john mayer", deterministic_tag_id("John Mayer"), arch_cat);
        tagger.add_synonym("mayer", deterministic_tag_id("John Mayer"), arch_cat);
        tagger.add_synonym("evh", deterministic_tag_id("EVH"), arch_cat);
        tagger.add_synonym("van halen", deterministic_tag_id("EVH"), arch_cat);
        tagger.add_synonym("hendrix", deterministic_tag_id("Hendrix"), arch_cat);
        tagger.add_synonym("jimi", deterministic_tag_id("Hendrix"), arch_cat);
        tagger.add_synonym("gilmour", deterministic_tag_id("Gilmour"), arch_cat);
        tagger.add_synonym("clapton", deterministic_tag_id("Clapton"), arch_cat);
        tagger.add_synonym("santana", deterministic_tag_id("Santana"), arch_cat);
        tagger.add_synonym("knopfler", deterministic_tag_id("Knopfler"), arch_cat);
        tagger.add_synonym("bb king", deterministic_tag_id("BB King"), arch_cat);

        tagger.sort_rules();
        tagger
    }

    /// Add a custom rule.
    pub fn add_rule(&mut self, rule: TagRule) {
        self.rules.push(rule);
        self.sort_rules();
    }

    /// Add a synonym (convenience for a Contains-mode rule).
    pub fn add_synonym(
        &mut self,
        synonym: impl Into<String>,
        tag_id: TagId,
        category: TagCategory,
    ) {
        self.rules
            .push(TagRule::contains(synonym, tag_id, category));
    }

    /// Sort rules longest-pattern-first so multi-word patterns match before
    /// their substrings.
    fn sort_rules(&mut self) {
        self.rules
            .sort_by(|a, b| b.pattern.len().cmp(&a.pattern.len()));
    }

    /// Extract tag IDs from a preset name by applying all rules.
    ///
    /// Returns a deduplicated list of matched tag IDs.
    pub fn tag_name(&self, name: &str) -> Vec<TagId> {
        let lower = name.to_lowercase();
        let tokens: Vec<&str> = lower
            .split(|c: char| c.is_whitespace() || c == '-' || c == '_' || c == '/' || c == '|')
            .filter(|t| !t.is_empty())
            .collect();

        let mut matched = HashSet::new();

        for rule in &self.rules {
            if matched.contains(&rule.tag_id) {
                continue; // Already matched this tag via a longer pattern
            }

            let is_match = match rule.match_mode {
                MatchMode::Word => tokens.iter().any(|t| *t == rule.pattern),
                MatchMode::Contains => lower.contains(&rule.pattern),
                MatchMode::Prefix => tokens.iter().any(|t| t.starts_with(&rule.pattern)),
            };

            if is_match {
                matched.insert(rule.tag_id);
            }
        }

        matched.into_iter().collect()
    }

    /// Apply auto-tagging to a `Taggable` item.
    ///
    /// Extracted tags are added as auto-derived (not manual).
    pub fn apply<T: Taggable>(&self, item: &mut T) {
        let tag_ids = self.tag_name(item.name());
        for id in tag_ids {
            item.tags_mut().add_auto(id);
        }
    }

    /// Apply auto-tagging to a `Tags` collection using a name string.
    pub fn apply_to_tags(&self, name: &str, tags: &mut Tags) {
        let tag_ids = self.tag_name(name);
        for id in tag_ids {
            tags.add_auto(id);
        }
    }

    /// Get the number of rules.
    pub fn rule_count(&self) -> usize {
        self.rules.len()
    }
}

impl Default for AutoTagger {
    fn default() -> Self {
        Self::new()
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn registry() -> TagRegistry {
        TagRegistry::with_defaults()
    }

    fn tagger() -> AutoTagger {
        AutoTagger::with_defaults(&registry())
    }

    // ── Basic extraction ─────────────────────────────────────────

    #[test]
    fn extracts_base_tone_from_name() {
        let tagger = tagger();
        let tags = tagger.tag_name("Clean Tone");
        let clean_id = deterministic_tag_id("Clean");
        assert!(tags.contains(&clean_id));
    }

    #[test]
    fn extracts_genre_from_name() {
        let tagger = tagger();
        let tags = tagger.tag_name("Blues Lead");
        let blues_id = deterministic_tag_id("Blues");
        assert!(tags.contains(&blues_id));
    }

    #[test]
    fn extracts_multiple_tags() {
        let tagger = tagger();
        let tags = tagger.tag_name("Clean Blues Warm");
        assert!(tags.contains(&deterministic_tag_id("Clean")));
        assert!(tags.contains(&deterministic_tag_id("Blues")));
        assert!(tags.contains(&deterministic_tag_id("Warm")));
    }

    #[test]
    fn extracts_character_tags() {
        let tagger = tagger();
        let tags = tagger.tag_name("Vintage Smooth Lead");
        assert!(tags.contains(&deterministic_tag_id("Vintage")));
        assert!(tags.contains(&deterministic_tag_id("Smooth")));
        assert!(tags.contains(&deterministic_tag_id("Lead")));
    }

    #[test]
    fn extracts_context_tags() {
        let tagger = tagger();
        let tags = tagger.tag_name("Verse Clean Rhythm");
        assert!(tags.contains(&deterministic_tag_id("Verse")));
        assert!(tags.contains(&deterministic_tag_id("Clean")));
        assert!(tags.contains(&deterministic_tag_id("Rhythm")));
    }

    // ── Synonyms / abbreviations ─────────────────────────────────

    #[test]
    fn synonym_od_maps_to_drive() {
        let tagger = tagger();
        let tags = tagger.tag_name("Blues OD");
        let drive_id = deterministic_tag_id("Drive");
        assert!(tags.contains(&drive_id));
    }

    #[test]
    fn synonym_dist_maps_to_drive() {
        let tagger = tagger();
        let tags = tagger.tag_name("Heavy Dist Lead");
        assert!(tags.contains(&deterministic_tag_id("Drive")));
    }

    #[test]
    fn synonym_crystal_maps_to_clean() {
        let tagger = tagger();
        let tags = tagger.tag_name("Crystal Clear");
        assert!(tags.contains(&deterministic_tag_id("Clean")));
    }

    #[test]
    fn gear_synonyms() {
        let tagger = tagger();

        let tags = tagger.tag_name("Plexi Style Lead");
        assert!(tags.contains(&deterministic_tag_id("Plexi")));

        let tags = tagger.tag_name("Klon Into Marshall");
        assert!(tags.contains(&deterministic_tag_id("Klon")));
        assert!(tags.contains(&deterministic_tag_id("Marshall")));
    }

    #[test]
    fn archetype_synonyms() {
        let tagger = tagger();

        let tags = tagger.tag_name("SRV Style Blues");
        assert!(tags.contains(&deterministic_tag_id("SRV")));
        assert!(tags.contains(&deterministic_tag_id("Blues")));

        let tags = tagger.tag_name("John Mayer Clean");
        assert!(tags.contains(&deterministic_tag_id("John Mayer")));
        assert!(tags.contains(&deterministic_tag_id("Clean")));
    }

    #[test]
    fn multi_word_synonym_tube_screamer() {
        let tagger = tagger();
        let tags = tagger.tag_name("Tube Screamer Blues Drive");
        assert!(tags.contains(&deterministic_tag_id("Tube Screamer")));
    }

    #[test]
    fn ts808_synonym() {
        let tagger = tagger();
        let tags = tagger.tag_name("TS808 Crunch");
        assert!(tags.contains(&deterministic_tag_id("Tube Screamer")));
        assert!(tags.contains(&deterministic_tag_id("Crunch")));
    }

    // ── Word boundary safety ─────────────────────────────────────

    #[test]
    fn word_mode_prevents_partial_match() {
        let tagger = tagger();
        // "Cleaning" should NOT match "Clean" in Word mode
        let tags = tagger.tag_name("Cleaning Supplies");
        let clean_id = deterministic_tag_id("Clean");
        assert!(!tags.contains(&clean_id));
    }

    #[test]
    fn word_mode_handles_hyphenated_names() {
        let tagger = tagger();
        // "hi-fi" split on hyphens should still work for contains-mode synonyms
        let tags = tagger.tag_name("Hi-Fi Modern Tone");
        assert!(tags.contains(&deterministic_tag_id("Modern")));
    }

    // ── No double-counting ───────────────────────────────────────

    #[test]
    fn deduplicates_same_tag() {
        let tagger = tagger();
        // "Drive" and "OD" both map to Drive tag — should only appear once
        let tags = tagger.tag_name("Drive OD Blues");
        let drive_id = deterministic_tag_id("Drive");
        assert_eq!(tags.iter().filter(|&&id| id == drive_id).count(), 1);
    }

    // ── Empty / edge cases ───────────────────────────────────────

    #[test]
    fn empty_name_returns_no_tags() {
        let tagger = tagger();
        assert!(tagger.tag_name("").is_empty());
    }

    #[test]
    fn garbage_name_returns_no_tags() {
        let tagger = tagger();
        assert!(tagger.tag_name("123 !!! ???").is_empty());
    }

    #[test]
    fn single_unrecognized_word() {
        let tagger = tagger();
        assert!(tagger.tag_name("Xylophone").is_empty());
    }

    // ── apply / apply_to_tags ────────────────────────────────────

    #[test]
    fn apply_to_tags_adds_auto_derived() {
        let tagger = tagger();
        let mut tags = Tags::new();
        tagger.apply_to_tags("Blues Clean Warm", &mut tags);

        assert!(!tags.is_empty());
        // All extracted tags should be auto-derived
        assert!(tags.is_auto_derived(deterministic_tag_id("Blues")));
        assert!(tags.is_auto_derived(deterministic_tag_id("Clean")));
        assert!(tags.is_auto_derived(deterministic_tag_id("Warm")));
    }

    #[test]
    fn apply_preserves_manual_tags() {
        let tagger = tagger();
        let mut tags = Tags::new();
        let manual_id = TagId::new();
        tags.add(manual_id);

        tagger.apply_to_tags("Blues", &mut tags);

        // Manual tag still there
        assert!(tags.has(manual_id));
        assert!(!tags.is_auto_derived(manual_id));
        // Auto tag added
        assert!(tags.has(deterministic_tag_id("Blues")));
        assert!(tags.is_auto_derived(deterministic_tag_id("Blues")));
    }

    // ── Custom rules ─────────────────────────────────────────────

    #[test]
    fn custom_rule_fires() {
        let mut tagger = AutoTagger::new();
        let custom_id = TagId::new();
        tagger.add_rule(TagRule::word(
            "sparkleshine",
            custom_id,
            TagCategory::Custom,
        ));

        let tags = tagger.tag_name("My SparkleShine Preset");
        assert!(tags.contains(&custom_id));
    }

    #[test]
    fn prefix_mode_matches_word_start() {
        let mut tagger = AutoTagger::new();
        let id = TagId::new();
        tagger.add_rule(TagRule::prefix("comp", id, TagCategory::Gear));

        let tags = tagger.tag_name("Compressor Heavy");
        assert!(tags.contains(&id));

        // Should also match "compressed"
        let tags = tagger.tag_name("Compressed Drive");
        assert!(tags.contains(&id));
    }

    #[test]
    fn with_defaults_has_rules() {
        let registry = registry();
        let tagger = AutoTagger::with_defaults(&registry);
        assert!(tagger.rule_count() > 30);
    }

    // ── Real-world preset names ──────────────────────────────────

    #[test]
    fn real_preset_john_mayer_clean_blues() {
        let tagger = tagger();
        let tags = tagger.tag_name("John Mayer Clean Blues Tone");
        assert!(tags.contains(&deterministic_tag_id("John Mayer")));
        assert!(tags.contains(&deterministic_tag_id("Clean")));
        assert!(tags.contains(&deterministic_tag_id("Blues")));
    }

    #[test]
    fn real_preset_srv_texas_blues() {
        let tagger = tagger();
        let tags = tagger.tag_name("SRV Style Blues Drive");
        assert!(tags.contains(&deterministic_tag_id("SRV")));
        assert!(tags.contains(&deterministic_tag_id("Blues")));
        assert!(tags.contains(&deterministic_tag_id("Drive")));
    }

    #[test]
    fn real_preset_dumble_lead() {
        let tagger = tagger();
        let tags = tagger.tag_name("Dumble OD Lead");
        assert!(tags.contains(&deterministic_tag_id("Dumble")));
        assert!(tags.contains(&deterministic_tag_id("Drive")));
        assert!(tags.contains(&deterministic_tag_id("Lead")));
    }

    #[test]
    fn real_preset_worship_ambient() {
        let tagger = tagger();
        let tags = tagger.tag_name("Worship Ambient Clean");
        assert!(tags.contains(&deterministic_tag_id("Worship")));
        assert!(tags.contains(&deterministic_tag_id("Ambient")));
        assert!(tags.contains(&deterministic_tag_id("Clean")));
    }

    #[test]
    fn real_preset_vintage_crunch() {
        let tagger = tagger();
        let tags = tagger.tag_name("Vintage Crunch Rhythm");
        assert!(tags.contains(&deterministic_tag_id("Vintage")));
        assert!(tags.contains(&deterministic_tag_id("Crunch")));
        assert!(tags.contains(&deterministic_tag_id("Rhythm")));
    }
}
