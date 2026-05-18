//! Site builder. Reads a Loro snapshot, walks all pages and
//! their blocks, drives Dioxus SSR per page.

use crdt::CrdtDoc;
use eyre::{Context, Result};
use knowledge_crdt::{BlockRepoLoro, PageRepoLoro};
use knowledge_proto::{Block, BlockRepo, PageRepo};
use project_proto::architect::Page as PageWindow;
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;
use uuid::Uuid;

use crate::components::WikiResolver;
use crate::{feeds, graph, render, search, tags};

const STYLE_CSS: &str = include_str!("style.css");
const GRAPH_JS: &str = include_str!("graph.js");
const SEARCH_JS: &str = include_str!("search.js");
const KATEX_LOADER_JS: &str = include_str!("katex_loader.js");

pub async fn build(
    snapshot: &[u8],
    out: &Path,
    site_title: &str,
    base_url: Option<&str>,
) -> Result<()> {
    let doc = CrdtDoc::ephemeral();
    doc.loro()
        .import(snapshot)
        .map_err(|e| eyre::eyre!("import snapshot: {e}"))?;

    let page_repo = PageRepoLoro::new(&doc);
    let block_repo = BlockRepoLoro::new(&doc);
    let big = PageWindow {
        index: 0,
        size: 100_000,
    };
    let page_listing = page_repo
        .list(big.clone(), None, None)
        .await
        .map_err(|e| eyre::eyre!("page list: {e}"))?;
    let block_listing = block_repo
        .list(big, None, None)
        .await
        .map_err(|e| eyre::eyre!("block list: {e}"))?;
    let pages = page_listing.items;
    let blocks = block_listing.items;

    tracing::info!(
        page_count = pages.len(),
        block_count = blocks.len(),
        "loaded vault"
    );

    let mut blocks_by_page: HashMap<Uuid, Vec<Block>> = HashMap::new();
    for b in blocks {
        blocks_by_page.entry(b.page_id).or_default().push(b);
    }

    let mut basename_to_slug: HashMap<String, String> = HashMap::new();
    for p in &pages {
        basename_to_slug.insert(p.basename.to_lowercase(), slugify(&p.basename));
    }
    let resolver = WikiResolver(Arc::new(basename_to_slug));

    if out.exists() {
        tokio::fs::remove_dir_all(out)
            .await
            .with_context(|| format!("clean {}", out.display()))?;
    }
    tokio::fs::create_dir_all(out).await?;
    tokio::fs::create_dir_all(out.join("assets")).await?;
    tokio::fs::write(out.join("assets/style.css"), STYLE_CSS).await?;
    tokio::fs::write(out.join("assets/graph.js"), GRAPH_JS).await?;
    tokio::fs::write(out.join("assets/search.js"), SEARCH_JS).await?;
    tokio::fs::write(out.join("assets/katex_loader.js"), KATEX_LOADER_JS).await?;

    // All blocks, flattened — used by graph + search + tags.
    let all_blocks: Vec<Block> = blocks_by_page.values().flatten().cloned().collect();

    // ── Graph ────────────────────────────────────────────
    let graph_data = graph::compute(&pages, &all_blocks);
    let graph_json =
        serde_json::to_string(&graph_data).map_err(|e| eyre::eyre!("graph json: {e}"))?;
    tokio::fs::write(out.join("assets/graph.json"), graph_json).await?;
    let graph_page_html = render::render_graph_page(site_title, &pages);
    tokio::fs::create_dir_all(out.join("graph")).await?;
    tokio::fs::write(out.join("graph/index.html"), graph_page_html).await?;

    // ── Backlinks index — used per-page below the article ──
    let backlinks = graph::build_backlinks(&pages, &all_blocks);

    // ── Tags index — extract `#tag` from block content ────
    let tag_index = tags::build(&pages, &all_blocks);

    // ── Per-page HTML ────────────────────────────────────
    for page in &pages {
        let slug = slugify(&page.basename);
        let blocks = blocks_by_page.get(&page.id).cloned().unwrap_or_default();
        let bl = backlinks.0.get(&slug).cloned().unwrap_or_default();
        let html = render::render_page(site_title, page, &blocks, &resolver, &pages, &bl);
        let dir = out.join(&slug);
        tokio::fs::create_dir_all(&dir).await?;
        tokio::fs::write(dir.join("index.html"), html).await?;
    }

    // ── Tag aggregation pages ────────────────────────────
    tokio::fs::create_dir_all(out.join("tags")).await?;
    let tag_map: &std::collections::BTreeMap<_, _> = tag_index.0.as_ref();
    for (tag, entries) in tag_map.iter() {
        let dir = out.join("tags").join(tag);
        tokio::fs::create_dir_all(&dir).await?;
        let html = render::render_tag_page(site_title, tag, entries, &pages);
        tokio::fs::write(dir.join("index.html"), html).await?;
    }
    let all_tags: Vec<(String, usize)> =
        tag_map.iter().map(|(k, v)| (k.clone(), v.len())).collect();
    let tags_index_html = render::render_tags_index(site_title, &all_tags, &pages);
    tokio::fs::write(out.join("tags/index.html"), tags_index_html).await?;

    // ── Search index (precomputed) ───────────────────────
    let search_payload = search::build(&pages, &blocks_by_page);
    tokio::fs::write(
        out.join("assets/search.json"),
        serde_json::to_string(&search_payload).map_err(|e| eyre::eyre!("search json: {e}"))?,
    )
    .await?;

    // ── Sitemap + RSS (only when caller supplied base_url) ─
    if let Some(base) = base_url {
        let sitemap_xml = feeds::sitemap(base, &pages);
        tokio::fs::write(out.join("sitemap.xml"), sitemap_xml).await?;
        let rss_xml = feeds::rss(base, site_title, &pages, &blocks_by_page, 50);
        tokio::fs::write(out.join("feed.xml"), rss_xml).await?;
    }

    let mut sorted_pages = pages.clone();
    sorted_pages.sort_by(|a, b| a.basename.to_lowercase().cmp(&b.basename.to_lowercase()));
    let index = render::render_index(site_title, &sorted_pages);
    tokio::fs::write(out.join("index.html"), index).await?;

    Ok(())
}

/// URL-safe slug from a page basename. Lowercases, replaces
/// runs of non-alphanumeric with `-`, trims dashes.
pub(crate) fn slugify(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut last_dash = true;
    for c in s.chars() {
        if c.is_ascii_alphanumeric() {
            for ch in c.to_lowercase() {
                out.push(ch);
            }
            last_dash = false;
        } else if !last_dash {
            out.push('-');
            last_dash = true;
        }
    }
    let trimmed = out.trim_matches('-');
    if trimmed.is_empty() {
        return "untitled".into();
    }
    trimmed.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slugify_basic() {
        assert_eq!(slugify("Hello World"), "hello-world");
        assert_eq!(slugify("My Notes! V2"), "my-notes-v2");
        assert_eq!(slugify("---trim---"), "trim");
        assert_eq!(slugify("UPPER/lower"), "upper-lower");
    }
}
