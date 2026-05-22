//! System + user prompt templates ported from
//! `nashsu/llm_wiki`. Each template is a const string with
//! `{name}`-style placeholders the caller fills in via
//! [`render`].
//!
//! Templates are kept verbatim with their `llm_wiki` origin
//! cited so future drift between the two projects is
//! reviewable. Where `llm_wiki` uses a "language directive"
//! injection, Task replaces it with the curator's configured
//! locale (default `en`).
//!
//! ## Catalog
//!
//! | Const                          | llm_wiki source                                       |
//! |--------------------------------|-------------------------------------------------------|
//! | [`INGEST_ANALYZE_SYSTEM`]      | `src/lib/ingest.ts:978-1024`                          |
//! | [`INGEST_GENERATE_SYSTEM`]     | `src/lib/ingest.ts:1029-1169`                         |
//! | [`QUERY_SYNTHESIS_SYSTEM`]     | `src/lib/deep-research.ts:114-133`                    |
//! | [`LINT_SEMANTIC_SYSTEM`]       | `src/lib/lint.ts:215-242`                             |
//! | [`SWEEP_REVIEWS_SYSTEM`]       | `src/lib/sweep-reviews.ts:215-232`                    |
//! | [`OPTIMIZE_RESEARCH_SYSTEM`]   | `src/lib/optimize-research-topic.ts:22-47`            |
//! | [`DEDUP_DETECT_SYSTEM`]        | `src/lib/dedup.ts:171-198`                            |
//! | [`DEDUP_MERGE_SYSTEM`]         | `src/lib/dedup.ts:321-331`                            |
//! | [`VISION_CAPTION_PINNED`]      | `src/lib/vision-caption.ts:69-70`                     |
//! | [`VISION_CAPTION_CONTEXTUAL`]  | `src/lib/vision-caption.ts:83-103`                    |
//! | [`LANGUAGE_DIRECTIVE`]         | `src/lib/output-language.ts:22-33`                    |

use std::collections::HashMap;

/// Substitute `{key}` placeholders in `template` with values
/// from `vars`. Keys not present in `vars` are left as
/// literal `{key}` in the output (intentional — missing
/// context is visible in the prompt rather than silently
/// dropped).
#[must_use]
pub fn render<S: std::hash::BuildHasher>(template: &str, vars: &HashMap<&str, &str, S>) -> String {
    let mut out = String::with_capacity(template.len());
    let mut rest = template;
    while let Some(start) = rest.find('{') {
        out.push_str(&rest[..start]);
        let after = &rest[start + 1..];
        if let Some(end) = after.find('}') {
            let key = &after[..end];
            if let Some(val) = vars.get(key) {
                out.push_str(val);
            } else {
                out.push('{');
                out.push_str(key);
                out.push('}');
            }
            rest = &after[end + 1..];
        } else {
            out.push_str(&rest[start..]);
            break;
        }
    }
    out.push_str(rest);
    out
}

pub const INGEST_ANALYZE_SYSTEM: &str = include_str!("templates/ingest_analyze_system.txt");
pub const INGEST_GENERATE_SYSTEM: &str = include_str!("templates/ingest_generate_system.txt");
pub const QUERY_SYNTHESIS_SYSTEM: &str = include_str!("templates/query_synthesis_system.txt");
pub const OPTIMIZE_RESEARCH_SYSTEM: &str = include_str!("templates/optimize_research_system.txt");
pub const LINT_SEMANTIC_SYSTEM: &str = include_str!("templates/lint_semantic_system.txt");
pub const SWEEP_REVIEWS_SYSTEM: &str = include_str!("templates/sweep_reviews_system.txt");
pub const DEDUP_DETECT_SYSTEM: &str = include_str!("templates/dedup_detect_system.txt");
pub const DEDUP_MERGE_SYSTEM: &str = include_str!("templates/dedup_merge_system.txt");
pub const VISION_CAPTION_PINNED: &str = include_str!("templates/vision_caption_pinned.txt");
pub const VISION_CAPTION_CONTEXTUAL: &str = include_str!("templates/vision_caption_contextual.txt");
pub const LANGUAGE_DIRECTIVE: &str = include_str!("templates/language_directive.txt");

/// Helper: build the language directive block.
#[must_use]
pub fn language_directive(lang: &str) -> String {
    let mut vars = HashMap::new();
    vars.insert("language", lang);
    render(LANGUAGE_DIRECTIVE, &vars)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn render_substitutes_known_keys() {
        let mut vars = HashMap::new();
        vars.insert("name", "Alice");
        assert_eq!(render("Hello, {name}!", &vars), "Hello, Alice!");
    }

    #[test]
    fn render_leaves_unknown_keys_literal() {
        let vars = HashMap::new();
        assert_eq!(render("Hi {nobody}", &vars), "Hi {nobody}");
    }

    #[test]
    fn language_directive_substitutes() {
        let out = language_directive("English");
        assert!(out.contains("MANDATORY OUTPUT LANGUAGE: English"));
        assert!(!out.contains("{language}"));
    }
}
