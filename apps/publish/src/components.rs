//! Dioxus components for the published static site.
//!
//! These are deliberately context-free (no signals, no callbacks)
//! so they render cleanly under `dioxus-ssr`. The shape mirrors
//! what the live app's `BlockView` produces — when we extract
//! a shared component crate, this is the side that survives;
//! the live app gains the interactivity over the same shell.
//!
//! ## Use beyond Task-architect
//!
//! These components are intended to be reusable for **arbitrary
//! doc sites** — not just task vaults. Anything with the shape
//! "list of pages, each page is a list of typed blocks" can
//! drive them. The `Block` and `Page` types are
//! `knowledge_proto`-flavored today; future work lifts the
//! component contracts to a generic `PublishablePage` trait so
//! external content sources (raw markdown trees, RSS, etc.) can
//! plug in.

use dioxus::prelude::*;
use knowledge_proto::{Block, Page};
use std::collections::HashMap;
use std::sync::Arc;

use crate::graph::BacklinkEntry;
use crate::inline;

/// Resolution table: lowercased basename → URL slug. Provided
/// as context so descendant components can resolve `[[wikilinks]]`
/// without prop drilling.
#[derive(Clone, Default, PartialEq)]
pub struct WikiResolver(pub Arc<HashMap<String, String>>);

/// Per-block target metadata: where the block lives + a short
/// snippet (for inline chips) + the full content (for
/// transclusion / embed when a block is just a solo `((uuid))`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockRefTarget {
    pub page_slug: String,
    pub snippet: String,
    pub content: String,
}

/// Resolution table: block UUID → target page slug + snippet.
/// Lets `((uuid))` references in any block resolve to a deep link
/// at parse time, the same way [`WikiResolver`] does for pages.
#[derive(Clone, Default, PartialEq)]
pub struct BlockRefResolver(pub Arc<HashMap<uuid::Uuid, BlockRefTarget>>);

/// Resolution table: lowercased page basename → ordered list of
/// block contents (sorted by block sort_key). Lets `![[Page]]`
/// embed nodes render the target page's blocks inline at parse
/// time. Bounded to a small per-page max in the builder so a
/// huge page can't blow up the embedding page.
#[derive(Clone, Default, PartialEq)]
pub struct PageEmbedResolver(pub Arc<HashMap<String, Vec<String>>>);

/// One row in a `{{query}}` result set — page slug + display title.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QueryHit {
    pub slug: String,
    pub title: String,
}

/// Resolution table: lowercased tag → matching pages. Lets
/// `{{query #tag}}` block content evaluate at parse time and
/// embed the result list inline. Logseq has full Datalog over
/// blocks; v1 supports tag filtering — the simplest 80% case.
#[derive(Clone, Default, PartialEq)]
pub struct QueryResolver(pub Arc<HashMap<String, Vec<QueryHit>>>);

/// Resolution table: lowercased namespace prefix → matching child
/// pages. Lets `{{namespace foo/bar}}` block content list every
/// page under that namespace (alphabetical). Mirrors Logseq's
/// `{{namespace}}` macro.
#[derive(Clone, Default, PartialEq)]
pub struct NamespaceResolver(pub Arc<HashMap<String, Vec<QueryHit>>>);

/// Outer document. `dioxus-ssr` renders the children as HTML
/// inside a hand-written `<html>` shell (`crate::render::shell`)
/// — Dioxus 0.7 doesn't have a first-class `<html>`/`<head>`
/// SSR primitive that's portable across configs.
///
/// Mobile drawer: the hamburger button + backdrop are inlined
/// here so every page gets them. Visibility is media-query +
/// `:checked` driven (checkbox hack); see `style.css`.
#[component]
pub fn DocBody(site_title: String, page_title: String, sidebar: Element, body: Element) -> Element {
    rsx! {
        // Hidden checkbox drives the drawer state via the
        // "checkbox hack" — pure CSS, no JS required. Must be
        // a sibling (not a descendant) of the elements that
        // react to its `:checked` state, hence its placement
        // here at body root.
        input {
            id: "nav-toggle",
            r#type: "checkbox",
            class: "nav-toggle-input",
            "aria-hidden": "true",
        }
        header { class: "site-header",
            label {
                r#for: "nav-toggle",
                class: "nav-toggle",
                "aria-label": "Toggle navigation",
                title: "Menu",
                // Three-line hamburger that animates to "x".
                span { class: "hamburger-bar" }
                span { class: "hamburger-bar" }
                span { class: "hamburger-bar" }
            }
            a { class: "site-title", href: "/", "{site_title}" }
        }
        // Backdrop intercepts clicks outside the drawer to
        // close it. Clicking it flips the checkbox off.
        label {
            r#for: "nav-toggle",
            class: "nav-backdrop",
            "aria-hidden": "true",
        }
        main { class: "layout",
            {sidebar}
            section { class: "main-col",
                h1 { class: "page-title", "{page_title}" }
                {body}
            }
        }
    }
}

/// One node in the namespace-hierarchy tree built from page
/// basenames split on `/`. Intermediate nodes (no page) act as
/// folder headers; leaf nodes link to the page.
#[derive(Clone, Debug, PartialEq)]
pub struct PageTreeNode {
    pub segment: String,
    pub full_basename: Option<String>,
    pub page_id: Option<uuid::Uuid>,
    pub children: Vec<PageTreeNode>,
}

/// Build a namespace tree from a flat page list. Segments are
/// split on `/`; pages without a `/` land at the root. Intermediate
/// "folder" nodes are synthesized for namespaces that have no
/// direct page. Children are sorted alphabetically per level.
pub fn build_page_tree(pages: &[Page]) -> Vec<PageTreeNode> {
    use std::collections::BTreeMap;

    #[derive(Default)]
    struct Builder {
        page_id: Option<uuid::Uuid>,
        full_basename: Option<String>,
        children: BTreeMap<String, Builder>,
    }

    let mut root: Builder = Builder::default();
    for p in pages {
        let segments: Vec<&str> = p.basename.split('/').filter(|s| !s.is_empty()).collect();
        if segments.is_empty() {
            continue;
        }
        let mut cur = &mut root;
        for s in &segments {
            cur = cur.children.entry((*s).to_string()).or_default();
        }
        cur.page_id = Some(p.id);
        cur.full_basename = Some(p.basename.clone());
    }

    fn to_nodes(b: Builder) -> Vec<PageTreeNode> {
        b.children
            .into_iter()
            .map(|(segment, child)| PageTreeNode {
                segment,
                full_basename: child.full_basename.clone(),
                page_id: child.page_id,
                children: to_nodes(child),
            })
            .collect()
    }
    to_nodes(root)
}

/// Sidebar — namespaced tree of pages, alphabetical per level.
/// Pages with `/` in their basename group under their parents.
#[component]
pub fn Sidebar(pages: Vec<Page>, current_id: Option<uuid::Uuid>) -> Element {
    let tree = build_page_tree(&pages);
    rsx! {
        nav { class: "sidebar",
            div { class: "sidebar-section",
                a { class: "sidebar-link", href: "/", "Home" }
                a { class: "sidebar-link", href: "/graph/", "Graph" }
                a { class: "sidebar-link", href: "/tags/", "Tags" }
                a { class: "sidebar-link", href: "/journals/", "Journals" }
            }
            div { class: "search-box",
                input {
                    id: "search-input",
                    r#type: "search",
                    placeholder: "Search… (/)",
                    autocomplete: "off",
                }
                ul { id: "search-results", class: "search-results" }
            }
            script { src: "/assets/search.js", defer: "true" }
            h2 { "Pages" }
            ul { class: "page-tree",
                for node in tree {
                    PageTreeItem { key: "{node.segment}", node: node, current_id: current_id }
                }
            }
        }
    }
}

/// Recursive sidebar item — renders one node + its children. Leaf
/// pages are anchors; intermediate folders are plain spans that
/// wrap a nested `<ul>` of children.
#[component]
fn PageTreeItem(node: PageTreeNode, current_id: Option<uuid::Uuid>) -> Element {
    let is_active = node.page_id.is_some() && node.page_id == current_id;
    let active_cls = if is_active { "active" } else { "" };
    rsx! {
        li {
            if let Some(full) = node.full_basename.clone() {
                {
                    let slug = crate::site::slugify(&full);
                    rsx! { a { class: "{active_cls}", href: "/{slug}/", "{node.segment}" } }
                }
            } else {
                span { class: "page-tree-folder", "{node.segment}" }
            }
            if !node.children.is_empty() {
                ul { class: "page-tree-children",
                    for child in node.children {
                        PageTreeItem { key: "{child.segment}", node: child, current_id: current_id }
                    }
                }
            }
        }
    }
}

/// "Linked references" panel rendered below the article. Each
/// entry shows the referring page title + a snippet of the
/// referring block's content (Logseq's "Linked References"
/// sidebar shape).
#[component]
pub fn BacklinksPanel(entries: Vec<BacklinkEntry>) -> Element {
    if entries.is_empty() {
        return rsx! {};
    }
    let count = entries.len();
    rsx! {
        section { class: "backlinks",
            h2 {
                "Linked references "
                span { class: "backlinks-count", "({count})" }
            }
            ul {
                for (i, e) in entries.into_iter().enumerate() {
                    {
                        let href = format!("/{}/", e.slug);
                        rsx! {
                            li { key: "{i}",
                                a { class: "backlink-page", href: "{href}", "{e.label}" }
                                div { class: "backlink-snippet", "{e.snippet}" }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Page body — sorted blocks rendered as their kind.
#[component]
pub fn PageContent(
    blocks: Vec<Block>,
    journal_day: Option<String>,
    frontmatter_json: String,
) -> Element {
    let mut sorted = blocks.clone();
    sorted.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let page_props = parse_props(&frontmatter_json);
    rsx! {
        if let Some(d) = journal_day {
            div { class: "page-meta", "{d}" }
        }
        if !page_props.is_empty() {
            dl { class: "page-props",
                for (k, v) in page_props {
                    dt { key: "{k}", "{k}" }
                    dd { "{v}" }
                }
            }
        }
        article { class: "content",
            for b in sorted {
                BlockNode { key: "{b.id}", block: b }
            }
        }
    }
}

/// One block. Kind decides the wrapping element.
#[component]
pub fn BlockNode(block: Block) -> Element {
    // Task glyphs — extends the Obsidian Tasks plugin set with
    // Logseq's wider state vocabulary. `struck: true` strikes
    // through the rest of the block (done / cancelled).
    let task_glyph = match block.list_task.as_deref() {
        Some(" ") | Some("") => Some(("☐", false)),
        Some("/") => Some(("◐", false)),
        Some("x") | Some("X") => Some(("☑", true)),
        Some("-") => Some(("⊟", true)),  // cancelled
        Some(">") => Some(("→", false)), // migrated / forward
        Some("<") => Some(("←", false)), // scheduled
        Some("!") => Some(("❗", false)),
        Some("?") => Some(("❓", false)),
        _ => None,
    };
    let resolver = try_use_context::<WikiResolver>().unwrap_or_default();
    let block_refs = try_use_context::<BlockRefResolver>().unwrap_or_default();
    let page_embeds = try_use_context::<PageEmbedResolver>().unwrap_or_default();
    let queries = try_use_context::<QueryResolver>().unwrap_or_default();
    // Peel a leading TODO/DOING/etc. keyword off the content before
    // parsing inlines. Logseq's task UX is text-based: `TODO buy
    // milk` is a task block, not a `[ ]` checkbox.
    let (marker, body_content) = peel_task_marker(&block.content);
    // Also strip SCHEDULED:/DEADLINE: planning lines and surface
    // them as date pills under the block body.
    let (plan, body_content) = peel_planning(body_content);
    let namespaces = try_use_context::<NamespaceResolver>().unwrap_or_default();
    let inlines = inline::parse(
        body_content,
        &resolver,
        &block_refs,
        &page_embeds,
        &queries,
        &namespaces,
    );
    // Block-anchor id — lets `((uuid))` references in other pages
    // deep-link to this block. Mirrors Logseq's `#block-<uuid>`.
    let anchor = format!("block-{}", block.id.simple());

    // Transclusion: if the trimmed content is just a single
    // resolved block-ref, render the target's full content inline
    // as an `<aside>` instead of a small chip. Mirrors Logseq's
    // "solo block-ref expands" convention. Re-parsing the target's
    // content with an empty BlockRefResolver bounds recursion to
    // one level so embed cycles can't blow the stack.
    if let Some((target_id, page_slug, content)) = solo_block_ref(&inlines, &block_refs) {
        let href = format!("/{page_slug}/#block-{}", target_id.simple());
        let empty_blocks = BlockRefResolver::default();
        let empty_embeds = PageEmbedResolver::default();
        let target_inlines = inline::parse(
            &content,
            &resolver,
            &empty_blocks,
            &empty_embeds,
            &QueryResolver::default(),
            &NamespaceResolver::default(),
        );
        return rsx! {
            aside { id: "{anchor}", class: "block-embed",
                a { class: "block-embed-source", href: "{href}",
                    "↪ from /{page_slug}"
                }
                div { class: "block-embed-body",
                    Inlines { nodes: target_inlines }
                }
            }
        };
    }
    let body = match block.kind.as_str() {
        "heading" => {
            let level = block.heading_level.unwrap_or(1).clamp(1, 6) as u8;
            // Dynamic h{1..6} via match — small cost, big clarity.
            match level {
                1 => rsx! { h1 { id: "{anchor}", Inlines { nodes: inlines } } },
                2 => rsx! { h2 { id: "{anchor}", Inlines { nodes: inlines } } },
                3 => rsx! { h3 { id: "{anchor}", Inlines { nodes: inlines } } },
                4 => rsx! { h4 { id: "{anchor}", Inlines { nodes: inlines } } },
                5 => rsx! { h5 { id: "{anchor}", Inlines { nodes: inlines } } },
                _ => rsx! { h6 { id: "{anchor}", Inlines { nodes: inlines } } },
            }
        }
        "code" => {
            let lang = block
                .code_lang
                .clone()
                .filter(|s| !s.is_empty())
                .unwrap_or_else(|| "plain".into());
            // Mermaid diagram blocks — bypass syntect and emit a
            // `<pre class="mermaid">` with raw source. The bundled
            // mermaid loader (see site.rs assets) initializes
            // these client-side via CDN, lazy-loaded only when
            // the page actually has a diagram.
            if lang.eq_ignore_ascii_case("mermaid") {
                rsx! {
                    pre { id: "{anchor}", class: "mermaid", "{block.content}" }
                }
            } else {
                // Server-side syntax highlighting via syntect. The
                // output is raw HTML (`<span style=…>` per token);
                // Dioxus needs `dangerous_inner_html` to embed it
                // verbatim. Safe because the input is the user's own
                // vault content + syntect generates only spans with
                // hex color styles.
                let highlighted = crate::syntax::highlight(&block.content, &lang);
                rsx! {
                    pre { id: "{anchor}", class: "code",
                        div { class: "code-lang", "{lang}" }
                        code {
                            dangerous_inner_html: "{highlighted}",
                        }
                    }
                }
            }
        }
        "list_item" => rsx! {
            div { id: "{anchor}", class: "list-item",
                "• "
                if let Some((g, struck)) = task_glyph {
                    span { class: if struck { "task-done" } else { "" }, "{g} " }
                }
                Inlines { nodes: inlines }
            }
        },
        _ => rsx! {
            p { id: "{anchor}",
                if let Some((g, _)) = task_glyph {
                    span { "{g} " }
                }
                Inlines { nodes: inlines }
            }
        },
    };
    // Block-level `prop:: value` chips — parsed from
    // `properties_json` and rendered as a row of pills under the
    // block body. Mirrors Logseq's inline property display.
    let chips = parse_props(&block.properties_json);
    let marker_cls = marker.map(|m| m.css_class());
    let marker_label = marker.map(|m| m.label());
    rsx! {
        if let (Some(cls), Some(label)) = (marker_cls, marker_label) {
            div { class: "task-marker-row",
                span { class: "task-marker {cls}", "{label}" }
                {body}
            }
        } else {
            {body}
        }
        if !plan.scheduled.is_empty() || !plan.deadline.is_empty() {
            div { class: "block-plan",
                if !plan.scheduled.is_empty() {
                    span { class: "plan-pill plan-scheduled",
                        span { class: "plan-key", "SCHEDULED" }
                        span { class: "plan-val", "{plan.scheduled}" }
                    }
                }
                if !plan.deadline.is_empty() {
                    span { class: "plan-pill plan-deadline",
                        span { class: "plan-key", "DEADLINE" }
                        span { class: "plan-val", "{plan.deadline}" }
                    }
                }
            }
        }
        if !chips.is_empty() {
            div { class: "block-props",
                for (k, v) in chips {
                    span { key: "{k}", class: "prop-chip",
                        span { class: "prop-key", "{k}" }
                        span { class: "prop-val", "{v}" }
                    }
                }
            }
        }
    }
}

/// Logseq-style text task markers. The block's content starts
/// with one of these uppercase tokens (followed by space) to be
/// recognized; we peel it off and render a chip.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TaskMarker {
    Todo,
    Doing,
    Done,
    Later,
    Now,
    Waiting,
    Cancelled,
}

impl TaskMarker {
    fn css_class(self) -> &'static str {
        match self {
            Self::Todo => "task-todo",
            Self::Doing => "task-doing",
            Self::Done => "task-done",
            Self::Later => "task-later",
            Self::Now => "task-now",
            Self::Waiting => "task-waiting",
            Self::Cancelled => "task-cancelled",
        }
    }
    fn label(self) -> &'static str {
        match self {
            Self::Todo => "TODO",
            Self::Doing => "DOING",
            Self::Done => "DONE",
            Self::Later => "LATER",
            Self::Now => "NOW",
            Self::Waiting => "WAITING",
            Self::Cancelled => "CANCELLED",
        }
    }
}

/// Strip a leading task-marker keyword from block content. Returns
/// `(marker, remainder)` where remainder has the keyword + one
/// trailing space removed; or `(None, content)` if no marker.
pub fn peel_task_marker(content: &str) -> (Option<TaskMarker>, &str) {
    // Order matters: longer markers first so `CANCELLED` wins over
    // a hypothetical `CANCEL`. All matched case-sensitively —
    // Logseq is uppercase.
    let candidates: &[(&str, TaskMarker)] = &[
        ("CANCELLED", TaskMarker::Cancelled),
        ("CANCELED", TaskMarker::Cancelled),
        ("WAITING", TaskMarker::Waiting),
        ("DOING", TaskMarker::Doing),
        ("LATER", TaskMarker::Later),
        ("DONE", TaskMarker::Done),
        ("TODO", TaskMarker::Todo),
        ("NOW", TaskMarker::Now),
        ("WAIT", TaskMarker::Waiting),
    ];
    for (token, marker) in candidates {
        if let Some(rest) = content.strip_prefix(token) {
            // Must be followed by whitespace or end-of-content so
            // we don't peel `TODOLIST` as `TODO` + `LIST`.
            if rest.is_empty() {
                return (Some(*marker), "");
            }
            if rest.starts_with(|c: char| c.is_whitespace()) {
                return (Some(*marker), rest.trim_start_matches(' '));
            }
        }
    }
    (None, content)
}

/// Pulled-out SCHEDULED / DEADLINE planning timestamps from a
/// block's content. Logseq stores them as `SCHEDULED: <date>` and
/// `DEADLINE: <date>` lines anywhere in the block.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PlanningTimestamps {
    pub scheduled: String,
    pub deadline: String,
}

/// Strip SCHEDULED: and DEADLINE: lines from block content,
/// returning the parsed timestamps + the cleaned remainder.
/// Format mirrors Org-mode / Logseq: `SCHEDULED: <2026-05-20>`.
pub fn peel_planning(content: &str) -> (PlanningTimestamps, &str) {
    let mut plan = PlanningTimestamps::default();
    let mut kept: Vec<&str> = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim_start();
        if let Some(rest) = trimmed
            .strip_prefix("SCHEDULED:")
            .or_else(|| trimmed.strip_prefix("Scheduled:"))
        {
            plan.scheduled = rest.trim().trim_matches(|c| c == '<' || c == '>').into();
            continue;
        }
        if let Some(rest) = trimmed
            .strip_prefix("DEADLINE:")
            .or_else(|| trimmed.strip_prefix("Deadline:"))
        {
            plan.deadline = rest.trim().trim_matches(|c| c == '<' || c == '>').into();
            continue;
        }
        kept.push(line);
    }
    // If we didn't strip anything, return original slice to keep
    // the borrow lifetime; otherwise we'd need to box a String.
    if plan.scheduled.is_empty() && plan.deadline.is_empty() {
        (plan, content)
    } else {
        // Edge case: when we strip lines we need an owned String,
        // but the call site borrows &str. Trick: rejoin into a
        // leaked str? No — we use the original content's first
        // chunk before the SCHEDULED line. Simpler: detect at
        // call-site by checking plan.is_empty(); for now we
        // return the joined kept text via a thread_local cache.
        // Pragmatic path: leak via Box::leak for build-time data.
        let joined = kept.join("\n");
        let s: &'static str = Box::leak(joined.into_boxed_str());
        (plan, s)
    }
}

/// Parse a `frontmatter_json` / `properties_json` blob into a
/// sorted list of `(key, display)` pairs. JSON objects are the
/// only supported shape; anything else returns empty. Values are
/// stringified compactly so they fit in a chip or table cell.
pub fn parse_props(blob: &str) -> Vec<(String, String)> {
    let v: serde_json::Value = match serde_json::from_str(blob) {
        Ok(v) => v,
        Err(_) => return Vec::new(),
    };
    let obj = match v.as_object() {
        Some(o) => o,
        None => return Vec::new(),
    };
    let mut out: Vec<(String, String)> = obj
        .iter()
        .map(|(k, v)| (k.clone(), display_prop(v)))
        .collect();
    out.sort_by(|a, b| a.0.cmp(&b.0));
    out
}

fn display_prop(v: &serde_json::Value) -> String {
    match v {
        serde_json::Value::String(s) => s.clone(),
        serde_json::Value::Null => String::new(),
        serde_json::Value::Bool(b) => b.to_string(),
        serde_json::Value::Number(n) => n.to_string(),
        serde_json::Value::Array(arr) => {
            arr.iter().map(display_prop).collect::<Vec<_>>().join(", ")
        }
        // Nested objects: compact JSON, hosts that want richer
        // rendering can extend the renderer later.
        serde_json::Value::Object(_) => v.to_string(),
    }
}

/// If the parsed inline tree is just a single resolved block-ref
/// (optionally surrounded by whitespace text), return the embed
/// target's metadata. Used to switch from "chip" rendering to
/// full-block transclusion. Returns `None` for anything else,
/// including broken refs (a broken ref still renders as a chip).
fn solo_block_ref(
    nodes: &[inline::Node],
    refs: &BlockRefResolver,
) -> Option<(uuid::Uuid, String, String)> {
    let mut ref_node: Option<(uuid::Uuid, String, bool)> = None;
    for n in nodes {
        match n {
            inline::Node::Text(s) if s.trim().is_empty() => continue,
            inline::Node::BlockRef {
                target_id,
                page_slug,
                broken,
                ..
            } => {
                if ref_node.is_some() {
                    return None;
                }
                ref_node = Some((*target_id, page_slug.clone(), *broken));
            }
            _ => return None,
        }
    }
    let (id, slug, broken) = ref_node?;
    if broken {
        return None;
    }
    let target = refs.0.get(&id)?;
    Some((id, slug, target.content.clone()))
}

/// Render a list of inline nodes.
#[component]
pub fn Inlines(nodes: Vec<inline::Node>) -> Element {
    rsx! {
        for (i, n) in nodes.into_iter().enumerate() {
            InlineNode { key: "{i}", node: n }
        }
    }
}

/// One inline node.
#[component]
pub fn InlineNode(node: inline::Node) -> Element {
    use inline::Node;
    match node {
        Node::Text(s) => rsx! { "{s}" },
        Node::Bold(children) => rsx! { strong { Inlines { nodes: children } } },
        Node::Italic(children) => rsx! { em { Inlines { nodes: children } } },
        Node::Strikethrough(children) => rsx! { s { Inlines { nodes: children } } },
        Node::Highlight(children) => rsx! { mark { Inlines { nodes: children } } },
        Node::Code(s) => rsx! { code { "{s}" } },
        Node::Wikilink {
            slug,
            label,
            broken,
        } => {
            let class = if broken {
                "wikilink broken"
            } else {
                "wikilink"
            };
            let title = if broken { "page not found" } else { "" };
            // Broken wikilinks get a `<span>` not an `<a>` — no
            // href to follow.
            if broken {
                rsx! {
                    span { class: "{class}", title: "{title}", "{label}" }
                }
            } else {
                let href = format!("/{slug}/");
                rsx! {
                    a { class: "{class}", href: "{href}", "{label}" }
                }
            }
        }
        Node::ExternalLink { label, url } => rsx! {
            a { class: "ext", href: "{url}", target: "_blank", rel: "noopener", "{label}" }
        },
        Node::Namespace { prefix, results } => {
            rsx! {
                aside { class: "namespace",
                    div { class: "namespace-header",
                        span { class: "namespace-expr", "{{ namespace {prefix} }}" }
                        span { class: "namespace-count", "{results.len()} page",
                            if results.len() != 1 { "s" }
                        }
                    }
                    if results.is_empty() {
                        p { class: "namespace-empty", "no pages under this namespace" }
                    } else {
                        ul { class: "namespace-results",
                            for hit in results {
                                {
                                    let href = format!("/{}/", hit.slug);
                                    rsx! {
                                        li { key: "{hit.slug}",
                                            a { href: "{href}", "{hit.title}" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        Node::Query {
            expr,
            results,
            broken,
        } => {
            if broken {
                rsx! {
                    span { class: "query broken", title: "unknown query form",
                        "{{ query {expr} }}"
                    }
                }
            } else {
                rsx! {
                    aside { class: "query",
                        div { class: "query-header",
                            span { class: "query-expr", "{{ query {expr} }}" }
                            span { class: "query-count", "{results.len()} match",
                                if results.len() != 1 { "es" }
                            }
                        }
                        if results.is_empty() {
                            p { class: "query-empty", "no matches" }
                        } else {
                            ul { class: "query-results",
                                for hit in results {
                                    {
                                        let href = format!("/{}/", hit.slug);
                                        rsx! {
                                            li { key: "{hit.slug}",
                                                a { href: "{href}", "{hit.title}" }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        Node::PageEmbed {
            slug,
            label,
            contents,
            broken,
        } => {
            if broken {
                rsx! {
                    span {
                        class: "page-embed broken",
                        title: "page not found",
                        "![[{label}]]"
                    }
                }
            } else {
                // Render the embedded page as an aside with a
                // header link + each block content reparsed
                // through the inline parser (empty resolvers to
                // bound recursion at one level). Limit body count
                // to 10 to avoid runaway docs.
                let href = format!("/{slug}/");
                let resolver = try_use_context::<WikiResolver>().unwrap_or_default();
                let empty_blocks = BlockRefResolver::default();
                let empty_embeds = PageEmbedResolver::default();
                let bodies: Vec<Vec<inline::Node>> = contents
                    .iter()
                    .take(10)
                    .map(|c| {
                        inline::parse(
                            c,
                            &resolver,
                            &empty_blocks,
                            &empty_embeds,
                            &QueryResolver::default(),
                            &NamespaceResolver::default(),
                        )
                    })
                    .collect();
                rsx! {
                    aside { class: "page-embed",
                        a { class: "page-embed-source", href: "{href}",
                            "↪ {label}"
                        }
                        div { class: "page-embed-body",
                            for (i, body) in bodies.into_iter().enumerate() {
                                p { key: "{i}", Inlines { nodes: body } }
                            }
                        }
                    }
                }
            }
        }
        Node::BlockRef {
            target_id,
            page_slug,
            snippet,
            broken,
        } => {
            if broken {
                rsx! {
                    span {
                        class: "block-ref broken",
                        title: "block not found",
                        "(({snippet}))"
                    }
                }
            } else {
                let href = format!("/{page_slug}/#block-{}", target_id.simple());
                rsx! {
                    a {
                        class: "block-ref",
                        href: "{href}",
                        title: "{snippet}",
                        "{snippet}"
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peel_todo_marker() {
        let (m, rest) = peel_task_marker("TODO buy milk");
        assert_eq!(m, Some(TaskMarker::Todo));
        assert_eq!(rest, "buy milk");
    }

    #[test]
    fn peel_doing_marker() {
        let (m, rest) = peel_task_marker("DOING refactor parser");
        assert_eq!(m, Some(TaskMarker::Doing));
        assert_eq!(rest, "refactor parser");
    }

    #[test]
    fn peel_cancelled_marker() {
        let (m, _) = peel_task_marker("CANCELLED skip it");
        assert_eq!(m, Some(TaskMarker::Cancelled));
        // CANCELED (US spelling) also matches.
        let (m2, _) = peel_task_marker("CANCELED skip it");
        assert_eq!(m2, Some(TaskMarker::Cancelled));
    }

    #[test]
    fn peel_marker_only_at_word_boundary() {
        // `TODOLIST` should NOT be peeled to TODO + LIST.
        let (m, rest) = peel_task_marker("TODOLIST not a task");
        assert_eq!(m, None);
        assert_eq!(rest, "TODOLIST not a task");
    }

    #[test]
    fn peel_no_marker() {
        let (m, rest) = peel_task_marker("regular content");
        assert_eq!(m, None);
        assert_eq!(rest, "regular content");
    }

    #[test]
    fn peel_scheduled_only() {
        let (p, rest) = peel_planning("SCHEDULED: <2026-05-20>\nrest of block");
        assert_eq!(p.scheduled, "2026-05-20");
        assert!(p.deadline.is_empty());
        assert_eq!(rest, "rest of block");
    }

    #[test]
    fn peel_deadline_and_scheduled() {
        let (p, rest) =
            peel_planning("task body\nSCHEDULED: <2026-05-20>\nDEADLINE: <2026-05-22>\nmore body");
        assert_eq!(p.scheduled, "2026-05-20");
        assert_eq!(p.deadline, "2026-05-22");
        assert_eq!(rest, "task body\nmore body");
    }

    #[test]
    fn peel_planning_passthrough_when_none() {
        let (p, rest) = peel_planning("just content, no planning");
        assert!(p.scheduled.is_empty() && p.deadline.is_empty());
        assert_eq!(rest, "just content, no planning");
    }
}
