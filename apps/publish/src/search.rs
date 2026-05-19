//! Precomputed search index. Emits `assets/search.json` —
//! a flat array of `{slug, title, snippet}` per page. The
//! browser-side `search.js` does case-insensitive substring
//! filtering against this. Tiny, no Lucene/MiniSearch needed
//! for vaults at typical sizes.

use crate::site::slugify;
use knowledge_proto::{Block, Page};
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Clone, Debug, PartialEq, serde::Serialize)]
pub struct SearchEntry {
    pub slug: String,
    pub title: String,
    pub snippet: String,
    /// Optional `#block-<uuid>` anchor for block-level deep links.
    /// `None` means the entry is the page itself (search.js
    /// builds `/slug/`); `Some(uuid_simple)` builds
    /// `/slug/#block-<uuid_simple>`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub anchor: Option<String>,
}

const SNIPPET_MAX: usize = 240;
const BLOCK_SNIPPET_MAX: usize = 160;

pub fn build(pages: &[Page], blocks_by_page: &HashMap<Uuid, Vec<Block>>) -> Vec<SearchEntry> {
    let mut out: Vec<SearchEntry> = Vec::new();
    for p in pages {
        let slug = slugify(&p.basename);
        let title = p.basename.clone();

        // One page-level entry — snippet joins the first few blocks.
        let mut joined = String::new();
        let blocks: Vec<Block> = blocks_by_page.get(&p.id).cloned().unwrap_or_default();
        let mut sorted = blocks.clone();
        sorted.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
        for b in &sorted {
            if !joined.is_empty() {
                joined.push(' ');
            }
            joined.push_str(&b.content);
            if joined.len() > SNIPPET_MAX * 2 {
                break;
            }
        }
        out.push(SearchEntry {
            slug: slug.clone(),
            title: title.clone(),
            snippet: truncate_snippet(&joined, SNIPPET_MAX),
            anchor: None,
        });

        // One per non-empty block — anchor lets the result deep-
        // link to the block, mirroring Logseq's block-grain search.
        for b in &sorted {
            let snippet = truncate_snippet(&b.content, BLOCK_SNIPPET_MAX);
            if snippet.is_empty() {
                continue;
            }
            out.push(SearchEntry {
                slug: slug.clone(),
                title: title.clone(),
                snippet,
                anchor: Some(b.id.simple().to_string()),
            });
        }
    }
    // Page entries first within each page (anchor=None < Some),
    // alphabetic by title across pages — keeps page-hits at the
    // top of result lists for typical title-search behavior.
    out.sort_by(|a, b| {
        a.title
            .to_lowercase()
            .cmp(&b.title.to_lowercase())
            .then_with(|| a.anchor.is_some().cmp(&b.anchor.is_some()))
    });
    out
}

fn truncate_snippet(s: &str, max: usize) -> String {
    let collapsed: String = s.split_whitespace().collect::<Vec<_>>().join(" ");
    if collapsed.chars().count() > max {
        let mut out: String = collapsed.chars().take(max).collect();
        out.push('…');
        out
    } else {
        collapsed
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use std::collections::HashMap;
    use uuid::Uuid;

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
            stat_ctime: Utc::now(),
            stat_mtime: Utc::now(),
            stat_size: 0,
            is_journal: false,
            journal_day: None,
            shadow_for_kind: None,
            shadow_for_id: None,
            created_at: Utc::now(),
            updated_at: Utc::now(),
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
            created_at: Utc::now(),
            updated_at: Utc::now(),
        }
    }

    #[test]
    fn builds_page_plus_block_entries() {
        let p = page("Notes");
        let b1 = block(p.id, "first block");
        let b2 = block(p.id, "second block");
        let mut by_page = HashMap::new();
        by_page.insert(p.id, vec![b1.clone(), b2.clone()]);
        let out = build(&[p.clone()], &by_page);
        assert_eq!(out.len(), 3, "1 page + 2 block entries");
        // Page entry (anchor=None) sorts before block entries.
        assert!(out[0].anchor.is_none());
        assert!(out[1].anchor.is_some() && out[2].anchor.is_some());
    }

    #[test]
    fn empty_blocks_skipped() {
        let p = page("Notes");
        let mut empty = block(p.id, "   ");
        empty.content = "".into();
        let mut by_page = HashMap::new();
        by_page.insert(p.id, vec![empty]);
        let out = build(&[p], &by_page);
        // Only the page entry — no block entry for empty content.
        assert_eq!(out.len(), 1);
        assert!(out[0].anchor.is_none());
    }
}
