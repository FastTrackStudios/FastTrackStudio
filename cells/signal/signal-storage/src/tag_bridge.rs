//! Tag bridge — converts between storage strings and domain tag types.
//!
//! Storage persists tags as `Vec<String>` in JSON. The domain layer uses
//! [`TagId`] UUIDs with a [`TagRegistry`] for metadata. This module bridges
//! the two representations at the storage boundary.

use signal_proto::id::TagId;
use signal_proto::tags::{Tag, TagCategory, TagRegistry, Tags};

/// Convert storage tag strings to a domain [`Tags`] struct.
///
/// Each string is resolved to a [`TagId`] via the registry (case-insensitive).
/// Unknown tags get a deterministic fallback ID from [`Tag::new`].
/// All tags are placed in the `manual` set (storage doesn't track auto-derived).
pub fn strings_to_tags(strings: &[String], registry: &TagRegistry) -> Tags {
    let mut tags = Tags::new();
    for s in strings {
        let id = resolve_tag_id(s, registry);
        tags.add(id);
    }
    tags
}

/// Convert a domain [`Tags`] struct back to storage strings.
///
/// Includes both manual and auto-derived tags. Each [`TagId`] is resolved
/// to its display name via the registry. Unknown IDs are silently skipped.
pub fn tags_to_strings(tags: &Tags, registry: &TagRegistry) -> Vec<String> {
    let mut result: Vec<String> = tags
        .iter()
        .filter_map(|id| resolve_tag_name(id, registry))
        .collect();
    result.sort();
    result.dedup();
    result
}

/// Resolve a tag name string to a [`TagId`].
///
/// Checks the registry first (case-insensitive). Falls back to a deterministic
/// UUID v5 for user-created tags not in the registry.
pub fn resolve_tag_id(name: &str, registry: &TagRegistry) -> TagId {
    // Registry lookup (case-insensitive)
    if let Some(tag) = registry.find_by_name(name) {
        return tag.id;
    }
    // Deterministic fallback — same UUID v5 that Tag::new() uses
    Tag::new(name, TagCategory::Custom).id
}

/// Resolve a [`TagId`] back to its display name.
///
/// Returns `None` if the ID is not in the registry.
pub fn resolve_tag_name(id: TagId, registry: &TagRegistry) -> Option<String> {
    registry.get(id).map(|tag| tag.name.clone())
}

/// Resolve a [`TagId`] to a name, with a fallback for unknown IDs.
///
/// If the ID is in the registry, returns the registered name.
/// Otherwise returns the `fallback` string (typically the original storage string).
pub fn resolve_tag_name_or(id: TagId, registry: &TagRegistry, fallback: &str) -> String {
    registry
        .get(id)
        .map(|tag| tag.name.clone())
        .unwrap_or_else(|| fallback.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn registry() -> TagRegistry {
        TagRegistry::with_defaults()
    }

    #[test]
    fn round_trip_registered_tags() {
        let reg = registry();
        let strings = vec!["Blues".to_string(), "Clean".to_string(), "Warm".to_string()];

        let tags = strings_to_tags(&strings, &reg);
        assert_eq!(tags.len(), 3);

        let back = tags_to_strings(&tags, &reg);
        assert_eq!(back, vec!["Blues", "Clean", "Warm"]);
    }

    #[test]
    fn resolve_known_tag_case_insensitive() {
        let reg = registry();
        let id_lower = resolve_tag_id("blues", &reg);
        let id_upper = resolve_tag_id("Blues", &reg);
        assert_eq!(id_lower, id_upper);
    }

    #[test]
    fn resolve_unknown_tag_gets_deterministic_id() {
        let reg = registry();
        let id1 = resolve_tag_id("My Custom Tag", &reg);
        let id2 = resolve_tag_id("My Custom Tag", &reg);
        assert_eq!(id1, id2, "same name should produce same ID");
    }

    #[test]
    fn unknown_tag_id_not_resolved_back() {
        let reg = registry();
        let id = resolve_tag_id("Totally Unknown", &reg);
        assert!(resolve_tag_name(id, &reg).is_none());
    }

    #[test]
    fn resolve_tag_name_or_uses_fallback() {
        let reg = registry();
        let id = resolve_tag_id("Custom Thing", &reg);
        let name = resolve_tag_name_or(id, &reg, "Custom Thing");
        assert_eq!(name, "Custom Thing");
    }

    #[test]
    fn resolve_tag_name_or_uses_registry_name() {
        let reg = registry();
        let id = resolve_tag_id("Blues", &reg);
        let name = resolve_tag_name_or(id, &reg, "blues");
        assert_eq!(name, "Blues"); // Registry canonical name, not fallback
    }

    #[test]
    fn empty_strings_produce_empty_tags() {
        let reg = registry();
        let tags = strings_to_tags(&[], &reg);
        assert!(tags.is_empty());
    }

    #[test]
    fn empty_tags_produce_empty_strings() {
        let reg = registry();
        let strings = tags_to_strings(&Tags::new(), &reg);
        assert!(strings.is_empty());
    }

    #[test]
    fn auto_derived_tags_included_in_output() {
        let reg = registry();
        let blues_id = resolve_tag_id("Blues", &reg);
        let clean_id = resolve_tag_id("Clean", &reg);

        let mut tags = Tags::new();
        tags.add(blues_id); // manual
        tags.add_auto(clean_id); // auto-derived

        let strings = tags_to_strings(&tags, &reg);
        assert!(strings.contains(&"Blues".to_string()));
        assert!(strings.contains(&"Clean".to_string()));
    }

    #[test]
    fn deduplicates_output() {
        let reg = registry();
        let blues_id = resolve_tag_id("Blues", &reg);

        let mut tags = Tags::new();
        tags.add(blues_id);
        tags.add_auto(blues_id); // Same tag in both manual and auto

        let strings = tags_to_strings(&tags, &reg);
        assert_eq!(strings.len(), 1);
        assert_eq!(strings[0], "Blues");
    }
}
