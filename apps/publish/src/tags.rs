//! Tag extraction + reverse index. Walks every block looking
//! for `#nested/tag` patterns in the content and rolls them up
//! into `tag → [pages]` for tag-aggregation pages.
//!
//! Source-of-truth note: blocks already carry a `refs_json`
//! field that includes parsed tags from the live editor's
//! reindexer. We re-parse here from `content` to avoid the
//! cross-crate JSON contract — keeps this binary self-contained
//! and makes the publisher work on any vault snapshot regardless
//! of whether the reindex pass ran. Cost is small (regex over
//! markdown) and only runs at build time.

use crate::site::slugify;
use knowledge_proto::{Block, Page};
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;
use uuid::Uuid;

/// `tag → [PageEntry]` (sorted by page basename).
#[derive(Clone, Default, PartialEq)]
pub struct TagIndex(pub Arc<BTreeMap<String, Vec<TagPageEntry>>>);

#[derive(Clone, Debug, PartialEq, serde::Serialize)]
pub struct TagPageEntry {
    pub slug: String,
    pub label: String,
}

/// Build the index. Tag matching mirrors `knowledge_proto`'s
/// regex (`#[A-Za-z0-9/_-]+`), bounded by whitespace or string
/// edges.
pub fn build(pages: &[Page], blocks: &[Block]) -> TagIndex {
    let mut id_to_meta: HashMap<Uuid, (String, String)> = HashMap::new();
    for p in pages {
        id_to_meta.insert(p.id, (slugify(&p.basename), p.basename.clone()));
    }

    // tag → set of (slug, label) — BTreeSet by slug to dedupe.
    let mut by_tag: BTreeMap<String, BTreeMap<String, String>> = BTreeMap::new();
    for b in blocks {
        let Some((slug, label)) = id_to_meta.get(&b.page_id) else {
            continue;
        };
        for tag in extract_tags(&b.content) {
            by_tag
                .entry(tag)
                .or_default()
                .insert(slug.clone(), label.clone());
        }
    }

    let map: BTreeMap<String, Vec<TagPageEntry>> = by_tag
        .into_iter()
        .map(|(tag, pages)| {
            (
                tag,
                pages
                    .into_iter()
                    .map(|(slug, label)| TagPageEntry { slug, label })
                    .collect(),
            )
        })
        .collect();
    TagIndex(Arc::new(map))
}

/// Extract `#tag` occurrences from a block's content.
/// Bounded by start-of-string or whitespace on the left;
/// terminated by whitespace, punctuation, or end on the right.
/// Allowed chars in a tag: letters, digits, `/`, `_`, `-`.
fn extract_tags(s: &str) -> Vec<String> {
    let bytes = s.as_bytes();
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < s.len() {
        if bytes[i] == b'#' {
            // Left boundary: start-of-string or whitespace.
            let left_ok = i == 0
                || s[..i]
                    .chars()
                    .last()
                    .map(|c| c.is_whitespace())
                    .unwrap_or(true);
            if !left_ok {
                i += 1;
                continue;
            }
            let start = i + 1;
            let mut end = start;
            while end < s.len() {
                let c = s[end..].chars().next().unwrap();
                if c.is_alphanumeric() || c == '/' || c == '_' || c == '-' {
                    end += c.len_utf8();
                } else {
                    break;
                }
            }
            if end > start {
                out.push(s[start..end].to_string());
                i = end;
                continue;
            }
        }
        let c = s[i..].chars().next().unwrap();
        i += c.len_utf8();
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn page(name: &str) -> Page {
        Page {
            id: Uuid::new_v4(),
            vault_id: Uuid::nil(),
            folder_id: None,
            path: format!("{}.md", name),
            basename: name.into(),
            ext: "md".into(),
            aliases: Vec::new(),
            frontmatter_json: "{}".into(),
            stat_ctime: chrono::Utc::now(),
            stat_mtime: chrono::Utc::now(),
            stat_size: 0,
            is_journal: false,
            journal_day: None,
            shadow_for_kind: None,
            shadow_for_id: None,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
        }
    }
    fn block(page_id: Uuid, content: &str) -> Block {
        Block {
            id: Uuid::new_v4(),
            vault_id: Uuid::nil(),
            page_id,
            parent_block_id: None,
            sort_key: "a".into(),
            content: content.into(),
            kind: "paragraph".into(),
            heading_level: None,
            list_ordered: false,
            list_task: None,
            code_lang: None,
            callout_kind: None,
            callout_foldable: false,
            properties_json: "{}".into(),
            obsidian_block_id: None,
            refs_json: "[]".into(),
            canvas_node_json: None,
            collapsed: false,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
        }
    }

    #[test]
    fn extracts_simple_tags() {
        assert_eq!(
            extract_tags("see #foo and #bar/baz end"),
            vec!["foo", "bar/baz"]
        );
    }

    #[test]
    fn skips_pound_in_word() {
        // `a#b` is not a tag (no left boundary).
        assert_eq!(extract_tags("a#b #real"), vec!["real"]);
    }

    #[test]
    fn dedupes_per_page() {
        let p = page("Notes");
        let blocks = vec![
            block(p.id, "see #foo and #foo again"),
            block(p.id, "third #foo"),
        ];
        let idx = build(&[p], &blocks);
        let entries = idx.0.get("foo").unwrap();
        assert_eq!(entries.len(), 1);
    }
}
