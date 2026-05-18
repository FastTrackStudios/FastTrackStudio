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
}

const SNIPPET_MAX: usize = 240;

pub fn build(pages: &[Page], blocks_by_page: &HashMap<Uuid, Vec<Block>>) -> Vec<SearchEntry> {
    let mut out: Vec<SearchEntry> = pages
        .iter()
        .map(|p| {
            let mut joined = String::new();
            if let Some(blocks) = blocks_by_page.get(&p.id) {
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
            }
            // Trim to a clean snippet; collapse whitespace.
            let snippet: String = joined.split_whitespace().collect::<Vec<_>>().join(" ");
            let snippet = if snippet.chars().count() > SNIPPET_MAX {
                let mut s: String = snippet.chars().take(SNIPPET_MAX).collect();
                s.push('…');
                s
            } else {
                snippet
            };
            SearchEntry {
                slug: slugify(&p.basename),
                title: p.basename.clone(),
                snippet,
            }
        })
        .collect();
    out.sort_by(|a, b| a.title.to_lowercase().cmp(&b.title.to_lowercase()));
    out
}
