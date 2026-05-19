//! Wikilink graph extraction + Dioxus component for the
//! published site.
//!
//! Two pieces:
//!
//! 1. [`compute`] walks every block, parses wikilinks, builds
//!    `{nodes, edges}` JSON (the format the embedded JS reader
//!    expects).
//! 2. [`GraphView`] is the Dioxus component that renders an
//!    empty `<canvas>` plus a `<script>` that reads `graph.json`
//!    via fetch and runs a tiny force simulation. Heavy enough
//!    that we ship the simulation as JS rather than wasm — the
//!    static site stays small + cacheable.
//!
//! Inspired by Quartz's `Graph.tsx` (D3-force + Pixi) and
//! Logseq's graph view, but trimmed to a no-dependency
//! implementation: vanilla canvas + ~120 lines of JS for the
//! simulation. Good enough up to ~500 nodes; bigger vaults can
//! upgrade to Pixi/D3 in a later phase.

use crate::site::slugify;
use dioxus::prelude::*;
use knowledge_proto::{Block, Page};
use publish_core::parser;
use publish_core::{
    BlockRefResolver, BlockRefTarget, NamespaceResolver, PageEmbedResolver, QueryResolver,
    WikiResolver,
};
use std::collections::HashMap;
use std::sync::Arc;
use uuid::Uuid;

/// Node in the page-link graph. `slug` is the URL of the page
/// (`/<slug>/`); `degree` is the count of incoming + outgoing
/// edges, used by the renderer to size the dot.
#[derive(Clone, Debug, PartialEq, serde::Serialize)]
pub struct GraphNode {
    pub id: String,    // slug — also the URL fragment
    pub label: String, // page basename
    pub degree: u32,
}

#[derive(Clone, Debug, PartialEq, serde::Serialize)]
pub struct GraphEdge {
    pub source: String, // slug
    pub target: String, // slug
}

#[derive(Clone, Debug, PartialEq, serde::Serialize)]
pub struct GraphData {
    pub nodes: Vec<GraphNode>,
    pub edges: Vec<GraphEdge>,
}

/// Compute the graph from a vault's pages + blocks. Walks each
/// block's content via the inline parser to find wikilinks, then
/// rolls them up into per-page edges (de-duped — multiple links
/// in one page to the same target count as one edge).
pub fn compute(pages: &[Page], blocks: &[Block]) -> GraphData {
    // Index pages by id so we can map block.page_id → slug fast.
    let mut id_to_slug: HashMap<Uuid, String> = HashMap::new();
    let mut basename_to_slug: HashMap<String, String> = HashMap::new();
    for p in pages {
        let slug = slugify(&p.basename);
        id_to_slug.insert(p.id, slug.clone());
        basename_to_slug.insert(p.basename.to_lowercase(), slug);
    }
    let resolver = WikiResolver(Arc::new(basename_to_slug));
    let block_refs = build_block_ref_resolver(blocks, &id_to_slug);

    // Collect per-page outgoing slugs (deduped via a HashSet).
    // Both `[[Page]]` wikilinks and `((uuid))` block refs become
    // page→page edges — a block ref to a block on page B is an
    // outgoing link from the source page to B.
    let mut outgoing: HashMap<String, std::collections::HashSet<String>> = HashMap::new();
    for b in blocks {
        let Some(src) = id_to_slug.get(&b.page_id) else {
            continue;
        };
        let nodes = publish_core::parser::parse(
            &b.content,
            &resolver,
            &block_refs,
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &publish_core::PagePropertyResolver::default(),
        );
        for n in walk(&nodes) {
            match n {
                publish_core::parser::Node::Wikilink {
                    slug,
                    broken: false,
                    ..
                } => {
                    if &slug != src {
                        outgoing
                            .entry(src.clone())
                            .or_default()
                            .insert(slug.clone());
                    }
                }
                publish_core::parser::Node::BlockRef {
                    page_slug,
                    broken: false,
                    ..
                } => {
                    if &page_slug != src {
                        outgoing
                            .entry(src.clone())
                            .or_default()
                            .insert(page_slug.clone());
                    }
                }
                _ => {}
            }
        }
    }

    let mut edges: Vec<GraphEdge> = Vec::new();
    let mut degree: HashMap<String, u32> = HashMap::new();
    for (source, targets) in &outgoing {
        for target in targets {
            edges.push(GraphEdge {
                source: source.clone(),
                target: target.clone(),
            });
            *degree.entry(source.clone()).or_default() += 1;
            *degree.entry(target.clone()).or_default() += 1;
        }
    }

    let mut nodes: Vec<GraphNode> = pages
        .iter()
        .map(|p| {
            let slug = slugify(&p.basename);
            let d = degree.get(&slug).copied().unwrap_or(0);
            GraphNode {
                id: slug,
                label: p.basename.clone(),
                degree: d,
            }
        })
        .collect();
    nodes.sort_by(|a, b| a.id.cmp(&b.id));
    edges.sort_by(|a, b| {
        (a.source.clone(), a.target.clone()).cmp(&(b.source.clone(), b.target.clone()))
    });

    GraphData { nodes, edges }
}

/// Build a UUID → (page_slug, snippet) lookup so `((uuid))`
/// refs resolve at parse time. Snippet = block content trimmed +
/// truncated to 80 chars — same fallback the live app's
/// `BlockRefChip` uses.
pub fn build_block_ref_resolver(
    blocks: &[Block],
    id_to_slug: &HashMap<Uuid, String>,
) -> BlockRefResolver {
    let mut map: HashMap<Uuid, BlockRefTarget> = HashMap::new();
    for b in blocks {
        let Some(slug) = id_to_slug.get(&b.page_id) else {
            continue;
        };
        map.insert(
            b.id,
            BlockRefTarget {
                page_slug: slug.clone(),
                snippet: block_snippet(&b.content),
                content: b.content.clone(),
            },
        );
    }
    BlockRefResolver(Arc::new(map))
}

fn block_snippet(content: &str) -> String {
    // Collapse whitespace, trim, cap at 80 chars (utf-8 safe).
    let mut s: String = content.split_whitespace().collect::<Vec<_>>().join(" ");
    if s.chars().count() > 80 {
        s = s.chars().take(80).collect::<String>() + "…";
    }
    if s.is_empty() {
        s.push('…');
    }
    s
}

/// Flatten an Inline tree to a borrow-iterator of leaf nodes
/// (Wikilinks live at any depth — inside Bold, Italic, etc.).
pub(crate) fn walk(nodes: &[publish_core::parser::Node]) -> Vec<publish_core::parser::Node> {
    let mut out = Vec::new();
    fn rec(nodes: &[publish_core::parser::Node], out: &mut Vec<publish_core::parser::Node>) {
        for n in nodes {
            match n {
                publish_core::parser::Node::Bold(c)
                | publish_core::parser::Node::Italic(c)
                | publish_core::parser::Node::Strikethrough(c)
                | publish_core::parser::Node::Highlight(c) => rec(c, out),
                _ => out.push(n.clone()),
            }
        }
    }
    rec(nodes, &mut out);
    out
}

/// Reverse-index built once per build: for each page slug, the
/// list of pages (slug + title) that link to it. Used by the
/// `Backlinks` component below the article.
#[derive(Clone, Default, PartialEq)]
pub struct BacklinkIndex(pub std::sync::Arc<HashMap<String, Vec<BacklinkEntry>>>);

// `BacklinkEntry` itself lives in `publish_core` so the shared
// renderer can consume it; re-exported here so existing
// `crate::graph::BacklinkEntry` imports keep working.
pub use publish_core::BacklinkEntry;

/// Build the backlinks index from the same data the graph uses.
/// Symmetric edges are deduped by (target ← source-page, source-block) pair
/// so the same page can show up as a backlink once per referring block.
pub fn build_backlinks(pages: &[Page], blocks: &[Block]) -> BacklinkIndex {
    let mut id_to_meta: HashMap<Uuid, (String, String)> = HashMap::new();
    let mut basename_to_slug: HashMap<String, String> = HashMap::new();
    let mut id_to_slug: HashMap<Uuid, String> = HashMap::new();
    for p in pages {
        let slug = slugify(&p.basename);
        id_to_meta.insert(p.id, (slug.clone(), p.basename.clone()));
        basename_to_slug.insert(p.basename.to_lowercase(), slug.clone());
        id_to_slug.insert(p.id, slug);
    }
    let resolver = WikiResolver(Arc::new(basename_to_slug));
    let block_refs = build_block_ref_resolver(blocks, &id_to_slug);

    // target_slug → list of BacklinkEntry. We dedupe per
    // (source-page-slug + block-id) so the same page can contribute
    // multiple snippets when multiple of its blocks reference the
    // target — matching Logseq's "Linked references" panel.
    let mut by_target: HashMap<String, Vec<BacklinkEntry>> = HashMap::new();
    let mut seen: HashMap<String, std::collections::HashSet<(String, Uuid)>> = HashMap::new();
    for b in blocks {
        let Some((src_slug, src_label)) = id_to_meta.get(&b.page_id) else {
            continue;
        };
        let parsed = publish_core::parser::parse(
            &b.content,
            &resolver,
            &block_refs,
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &publish_core::PagePropertyResolver::default(),
        );
        let snippet = block_snippet(&b.content);
        for n in walk(&parsed) {
            let target = match n {
                publish_core::parser::Node::Wikilink {
                    slug,
                    broken: false,
                    ..
                } => slug,
                publish_core::parser::Node::BlockRef {
                    page_slug,
                    broken: false,
                    ..
                } => page_slug,
                _ => continue,
            };
            if &target == src_slug {
                continue;
            }
            let dedupe_key = (src_slug.clone(), b.id);
            let already = seen.entry(target.clone()).or_default().insert(dedupe_key);
            if !already {
                continue;
            }
            by_target.entry(target).or_default().push(BacklinkEntry {
                slug: src_slug.clone(),
                label: src_label.clone(),
                snippet: snippet.clone(),
            });
        }
    }
    // Sort entries deterministically by (page label, snippet).
    for entries in by_target.values_mut() {
        entries.sort_by(|a, b| {
            a.label
                .to_lowercase()
                .cmp(&b.label.to_lowercase())
                .then_with(|| a.snippet.cmp(&b.snippet))
        });
    }
    BacklinkIndex(Arc::new(by_target))
}

/// Dioxus component that renders an empty `<canvas>` plus a
/// `<script>` that fetches `/assets/graph.json` and runs the
/// force simulation. Self-contained — no external CDN.
#[component]
pub fn GraphView(height_px: u32) -> Element {
    rsx! {
        div { class: "graph-wrap",
            canvas {
                id: "wikigraph",
                width: "800",
                height: "{height_px}",
                style: "width: 100%; height: {height_px}px; display: block; background: var(--bg-alt); border: 1px solid var(--border); border-radius: 0.5em;",
            }
            script { src: "/assets/graph.js", defer: "true" }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn computes_basic_edges() {
        let alpha = page("Alpha");
        let beta = page("Beta");
        let blocks = vec![
            block(alpha.id, "see [[Beta]]"),
            block(beta.id, "back to [[Alpha]]"),
        ];
        let g = compute(&[alpha.clone(), beta.clone()], &blocks);
        assert_eq!(g.nodes.len(), 2);
        assert_eq!(g.edges.len(), 2);
        // Each node has degree 2 (one in, one out).
        assert!(g.nodes.iter().all(|n| n.degree == 2));
    }

    #[test]
    fn dedupes_repeat_links_in_one_page() {
        let alpha = page("Alpha");
        let beta = page("Beta");
        let blocks = vec![
            block(alpha.id, "see [[Beta]]"),
            block(alpha.id, "again [[Beta]] and yet [[Beta]]"),
        ];
        let g = compute(&[alpha, beta], &blocks);
        // Despite three occurrences, edge alpha→beta is one edge.
        assert_eq!(g.edges.len(), 1);
    }

    #[test]
    fn skips_self_links() {
        let alpha = page("Alpha");
        let blocks = vec![block(alpha.id, "loop [[Alpha]]")];
        let g = compute(&[alpha], &blocks);
        assert_eq!(g.edges.len(), 0);
    }

    #[test]
    fn skips_broken_wikilinks() {
        let alpha = page("Alpha");
        let blocks = vec![block(alpha.id, "see [[NotARealPage]]")];
        let g = compute(&[alpha], &blocks);
        assert_eq!(g.edges.len(), 0);
    }

    #[test]
    fn block_ref_contributes_page_edge() {
        let alpha = page("Alpha");
        let beta = page("Beta");
        let target = block(beta.id, "target paragraph");
        let target_id = target.id;
        let mut src = block(alpha.id, "");
        src.content = format!("see (({target_id}))");
        let g = compute(&[alpha, beta], &vec![target, src]);
        assert_eq!(g.edges.len(), 1, "block ref should add alpha→beta edge");
        assert_eq!(g.edges[0].source, "alpha");
        assert_eq!(g.edges[0].target, "beta");
    }

    #[test]
    fn block_ref_backlinks_appear() {
        let alpha = page("Alpha");
        let beta = page("Beta");
        let target = block(beta.id, "target paragraph");
        let target_id = target.id;
        let mut src = block(alpha.id, "");
        src.content = format!("see (({target_id}))");
        let bl = build_backlinks(&[alpha, beta], &vec![target, src]);
        let entries = bl.0.get("beta").cloned().unwrap_or_default();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].slug, "alpha");
    }
}
