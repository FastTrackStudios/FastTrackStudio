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

use crate::components::WikiResolver;
use crate::inline;
use crate::site::slugify;
use dioxus::prelude::*;
use knowledge_proto::{Block, Page};
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

    // Collect per-page outgoing slugs (deduped via a HashSet).
    let mut outgoing: HashMap<String, std::collections::HashSet<String>> = HashMap::new();
    for b in blocks {
        let Some(src) = id_to_slug.get(&b.page_id) else {
            continue;
        };
        let nodes = inline::parse(&b.content, &resolver);
        for n in walk(&nodes) {
            if let inline::Node::Wikilink {
                slug,
                broken: false,
                ..
            } = n
            {
                if &slug != src {
                    outgoing
                        .entry(src.clone())
                        .or_default()
                        .insert(slug.clone());
                }
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

/// Flatten an Inline tree to a borrow-iterator of leaf nodes
/// (Wikilinks live at any depth — inside Bold, Italic, etc.).
pub(crate) fn walk(nodes: &[inline::Node]) -> Vec<inline::Node> {
    let mut out = Vec::new();
    fn rec(nodes: &[inline::Node], out: &mut Vec<inline::Node>) {
        for n in nodes {
            match n {
                inline::Node::Bold(c)
                | inline::Node::Italic(c)
                | inline::Node::Strikethrough(c)
                | inline::Node::Highlight(c) => rec(c, out),
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

#[derive(Clone, Debug, PartialEq)]
pub struct BacklinkEntry {
    pub slug: String,
    pub label: String,
}

/// Build the backlinks index from the same data the graph uses.
/// Symmetric edges are deduped by (target ← source) pair.
pub fn build_backlinks(pages: &[Page], blocks: &[Block]) -> BacklinkIndex {
    let mut id_to_meta: HashMap<Uuid, (String, String)> = HashMap::new();
    let mut basename_to_slug: HashMap<String, String> = HashMap::new();
    for p in pages {
        let slug = slugify(&p.basename);
        id_to_meta.insert(p.id, (slug.clone(), p.basename.clone()));
        basename_to_slug.insert(p.basename.to_lowercase(), slug);
    }
    let resolver = WikiResolver(Arc::new(basename_to_slug));

    // target_slug → set of (source_slug, source_label)
    let mut by_target: HashMap<String, std::collections::BTreeMap<String, String>> = HashMap::new();
    for b in blocks {
        let Some((src_slug, src_label)) = id_to_meta.get(&b.page_id) else {
            continue;
        };
        let parsed = inline::parse(&b.content, &resolver);
        for n in walk(&parsed) {
            if let inline::Node::Wikilink {
                slug: target,
                broken: false,
                ..
            } = n
            {
                if &target == src_slug {
                    continue;
                }
                by_target
                    .entry(target.clone())
                    .or_default()
                    .insert(src_slug.clone(), src_label.clone());
            }
        }
    }
    let map: HashMap<String, Vec<BacklinkEntry>> = by_target
        .into_iter()
        .map(|(target, srcs)| {
            (
                target,
                srcs.into_iter()
                    .map(|(slug, label)| BacklinkEntry { slug, label })
                    .collect(),
            )
        })
        .collect();
    BacklinkIndex(Arc::new(map))
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
}
