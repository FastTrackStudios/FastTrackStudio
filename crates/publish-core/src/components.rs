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

/// One row in the "Linked references" panel.
#[derive(Clone, Debug, PartialEq)]
pub struct BacklinkEntry {
    pub slug: String,
    pub label: String,
    pub snippet: String,
}

/// Extract a YouTube video id from a payload that may already be
/// a bare id, a `youtube.com/watch?v=…` URL, or a `youtu.be/…`
/// short link. Returns the input unchanged when nothing matches
/// (lets the iframe surface its own error).
pub fn extract_youtube_id(s: &str) -> String {
    let s = s.trim();
    if let Some(idx) = s.find("v=") {
        let after = &s[idx + 2..];
        let id: String = after
            .chars()
            .take_while(|c| *c != '&' && *c != '#')
            .collect();
        if !id.is_empty() {
            return id;
        }
    }
    if let Some(idx) = s.rfind("youtu.be/") {
        let after = &s[idx + 9..];
        let id: String = after
            .chars()
            .take_while(|c| *c != '?' && *c != '&')
            .collect();
        if !id.is_empty() {
            return id;
        }
    }
    if let Some(idx) = s.find("/embed/") {
        let after = &s[idx + 7..];
        let id: String = after
            .chars()
            .take_while(|c| *c != '?' && *c != '&')
            .collect();
        if !id.is_empty() {
            return id;
        }
    }
    s.to_string()
}

/// URL-safe slug. Lowercases, replaces runs of non-alphanumeric
/// with `-`, trims dashes.
pub fn slugify(s: &str) -> String {
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
use crate::parser as inline;

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
/// blocks; we support tag filtering + page-property filtering
/// via the s-expression DSL (see `parser::eval_query`).
#[derive(Clone, Default, PartialEq)]
pub struct QueryResolver(pub Arc<HashMap<String, Vec<QueryHit>>>);

/// Resolution table: template name (lowercased) → ordered list
/// of block contents that form the template body. Lets
/// `{{template foo}}` macros expand at render time into the
/// template's blocks. Templates are conventionally tagged with
/// a `template:: <name>` block-property in Logseq vaults.
#[derive(Clone, Default, PartialEq)]
pub struct TemplateResolver(pub Arc<HashMap<String, Vec<String>>>);

/// Resolution table for `(property <key> <value>)` queries.
/// Inner map: `key` → `value` → matching pages. Keys + values
/// are lowercased on the way in to make matching forgiving.
#[derive(Clone, Default, PartialEq)]
pub struct PagePropertyResolver(pub Arc<HashMap<String, HashMap<String, Vec<QueryHit>>>>);

/// Vault asset root, used to rewrite relative image URLs
/// (`../assets/foo.png`, `assets/foo.png`, `./assets/foo.png`) to
/// absolute `file://` URLs the renderer can actually load. Empty
/// means "don't rewrite" — static-site builds leave their URLs as
/// relative paths because the publisher copies the assets next to
/// the output HTML.
#[derive(Clone, Default, PartialEq)]
pub struct AssetBaseResolver(pub Arc<Option<std::path::PathBuf>>);

impl AssetBaseResolver {
    pub fn from_root(root: std::path::PathBuf) -> Self {
        Self(Arc::new(Some(root)))
    }
    /// Apply the resolver to a raw `src` URL. Absolute URLs
    /// (`http://`, `https://`, `file://`, `data:`) pass through
    /// unchanged.
    pub fn resolve(&self, url: &str) -> String {
        if url.starts_with("http://")
            || url.starts_with("https://")
            || url.starts_with("file://")
            || url.starts_with("data:")
        {
            return url.to_string();
        }
        let Some(root) = self.0.as_ref().as_ref() else {
            return url.to_string();
        };
        // Strip leading `./`, `../`, and `/`. We treat all relative
        // forms as vault-root-relative since pages can live in
        // `pages/` or `journals/` and Logseq's `../assets/` form
        // resolves the same way.
        let cleaned = url.trim_start_matches("./");
        let cleaned = cleaned.trim_start_matches("../");
        let cleaned = cleaned.trim_start_matches('/');
        let path = root.join(cleaned);
        format!("file://{}", path.display())
    }
}

/// Resolution table: lowercased namespace prefix → matching child
/// pages. Lets `{{namespace foo/bar}}` block content list every
/// page under that namespace (alphabetical). Mirrors Logseq's
/// `{{namespace}}` macro.
#[derive(Clone, Default, PartialEq)]
pub struct NamespaceResolver(pub Arc<HashMap<String, Vec<QueryHit>>>);

/// Optional in-app navigator. Hosts that want clicks on
/// `[[Page]]` rendered links to stay inside the app (rather
/// than the browser/webview attempting to navigate to `/slug/`)
/// provide a callback. Static-site renderers leave it unset
/// and the renderer falls back to `<a href>` anchors.
#[derive(Clone, Default)]
pub struct WikiNavigator(pub Option<Callback<String>>);

impl PartialEq for WikiNavigator {
    fn eq(&self, other: &Self) -> bool {
        self.0.is_some() == other.0.is_some()
    }
}

/// Resolution table: page slug → preview snippet (first ~140
/// chars of the page's first block). Drives the hover-preview
/// tooltip on `[[wikilinks]]` in the live shell. Empty resolver
/// means previews are disabled.
#[derive(Clone, Default, PartialEq)]
pub struct WikiPreviewResolver(pub Arc<HashMap<String, String>>);

/// Optional in-app navigator for `#tag` clicks. The host receives
/// the lowercased tag name and is expected to surface a tag view
/// (every block referencing the tag). Static-site builds leave
/// this unset; the renderer falls back to a plain `<a href>`.
#[derive(Clone, Default)]
pub struct TagNavigator(pub Option<Callback<String>>);

impl PartialEq for TagNavigator {
    fn eq(&self, other: &Self) -> bool {
        self.0.is_some() == other.0.is_some()
    }
}

/// Same shape but for `((uuid))` block-ref clicks. Hosts
/// receive the target block's UUID.
#[derive(Clone, Default)]
pub struct BlockRefNavigator(pub Option<Callback<uuid::Uuid>>);

impl PartialEq for BlockRefNavigator {
    fn eq(&self, other: &Self) -> bool {
        self.0.is_some() == other.0.is_some()
    }
}

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
                    let slug = slugify(&full);
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
    let properties = try_use_context::<PagePropertyResolver>().unwrap_or_default();
    let templates = try_use_context::<TemplateResolver>().unwrap_or_default();
    let inlines = inline::parse(
        body_content,
        &resolver,
        &block_refs,
        &page_embeds,
        &queries,
        &namespaces,
        &properties,
        &templates,
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
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
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
    pub fn css_class(self) -> &'static str {
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
    pub fn label(self) -> &'static str {
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
/// One drawer section parsed out of block content.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Drawer {
    /// Drawer name (the token between `:`s on the opening line),
    /// uppercase. Common names: `LOGBOOK`, `PROPERTIES`.
    pub name: String,
    /// Lines between the opening `:NAME:` and the closing `:END:`,
    /// joined by newlines. Empty when the drawer was empty.
    pub body: String,
}

/// Strip `:NAME: … :END:` drawer sections from block content.
/// Returns the extracted drawers (in source order) plus the
/// content with those sections removed.
///
/// Logseq uses drawers for things like LOGBOOK (time tracking
/// entries) and PROPERTIES (Org-mode-style legacy block props).
/// We surface them as structured data so the renderer can show
/// each drawer as a collapsible section.
pub fn peel_drawers(content: &str) -> (Vec<Drawer>, String) {
    let mut drawers: Vec<Drawer> = Vec::new();
    let mut kept: Vec<&str> = Vec::new();
    let mut iter = content.lines();
    while let Some(line) = iter.next() {
        if let Some(name) = drawer_open(line) {
            // Consume lines until `:END:` (case-insensitive).
            let mut body: Vec<&str> = Vec::new();
            let mut closed = false;
            for inner in iter.by_ref() {
                if drawer_close(inner) {
                    closed = true;
                    break;
                }
                body.push(inner);
            }
            if closed {
                drawers.push(Drawer {
                    name,
                    body: body.join("\n"),
                });
                continue;
            } else {
                // Unclosed drawer — preserve the opening line as
                // literal content so the user doesn't lose data.
                kept.push(line);
                kept.extend(body);
            }
        } else {
            kept.push(line);
        }
    }
    (drawers, kept.join("\n"))
}

/// Match an opening drawer line `:NAME:` (allow surrounding
/// whitespace). Returns the uppercased name on match.
fn drawer_open(line: &str) -> Option<String> {
    let t = line.trim();
    if t.starts_with(':') && t.ends_with(':') && t.len() >= 3 {
        let inner = &t[1..t.len() - 1];
        if inner.eq_ignore_ascii_case("END") {
            return None;
        }
        // Drawer names: word characters, dash, underscore.
        if !inner.is_empty()
            && inner
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
        {
            return Some(inner.to_ascii_uppercase());
        }
    }
    None
}

fn drawer_close(line: &str) -> bool {
    let t = line.trim();
    t.eq_ignore_ascii_case(":END:")
}

/// Strip Logseq-style `key:: value` property lines from block
/// content. Returns the parsed properties as a sorted JSON
/// object string (the same shape `properties_json` carries on
/// the wire) plus the remaining content with those lines
/// removed.
///
/// Recognized lines: any `^[a-z][a-z0-9_-]*:: <value>` at the
/// start of a line. Keys are normalized to lowercase. The
/// special key `id` is treated as the block's UUID and is
/// surfaced separately by callers (it stays in the returned
/// props so callers can read it once).
///
/// Lines are matched per-line; values run to end-of-line and
/// are trimmed.
pub fn peel_block_properties(content: &str) -> (String, String) {
    let mut map = serde_json::Map::new();
    let mut kept: Vec<&str> = Vec::new();
    for line in content.lines() {
        if let Some((key, value)) = parse_property_line(line) {
            // First write wins — Logseq treats later duplicates
            // as overrides, but for round-trip we conservatively
            // keep the first occurrence.
            map.entry(key).or_insert(serde_json::Value::String(value));
            continue;
        }
        kept.push(line);
    }
    if map.is_empty() {
        return ("{}".to_string(), content.to_string());
    }
    let json = serde_json::Value::Object(map).to_string();
    (json, kept.join("\n"))
}

/// Match a single line against the `key:: value` property
/// grammar. Returns `Some((key_lower, value))` on match, `None`
/// otherwise.
pub fn parse_property_line(line: &str) -> Option<(String, String)> {
    let trimmed = line.trim_start();
    let bytes = trimmed.as_bytes();
    if bytes.is_empty() || !bytes[0].is_ascii_lowercase() {
        return None;
    }
    // Walk the key: lowercase + digits + dash + underscore.
    let mut i = 0;
    while i < bytes.len()
        && (bytes[i].is_ascii_lowercase()
            || bytes[i].is_ascii_digit()
            || bytes[i] == b'-'
            || bytes[i] == b'_')
    {
        i += 1;
    }
    // Need at least one key char + the `:: ` separator.
    if i == 0 || i + 1 >= bytes.len() {
        return None;
    }
    if bytes[i] != b':' || bytes[i + 1] != b':' {
        return None;
    }
    let key = trimmed[..i].to_string();
    let rest = &trimmed[i + 2..];
    // Require a space (or end) after `::`.
    let value = rest.trim_start();
    Some((key, value.to_string()))
}

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
                // In-app: if a host provided a `WikiNavigator`,
                // render as a `<span>` with onclick instead of an
                // anchor so the webview doesn't try to navigate
                // outside the app. Static-site builds (no
                // navigator in context) keep the plain `<a href>`.
                let nav = try_use_context::<WikiNavigator>().and_then(|n| n.0);
                let previews = try_use_context::<WikiPreviewResolver>().unwrap_or_default();
                let preview = previews.0.get(&slug).cloned();
                let link = if let Some(cb) = nav {
                    let slug_for_click = slug.clone();
                    rsx! {
                        span {
                            class: "{class}",
                            style: "cursor: pointer;",
                            onclick: move |e: Event<MouseData>| {
                                e.prevent_default();
                                e.stop_propagation();
                                cb.call(slug_for_click.clone());
                            },
                            "{label}"
                        }
                    }
                } else {
                    let href = format!("/{slug}/");
                    rsx! {
                        a { class: "{class}", href: "{href}", "{label}" }
                    }
                };
                if let Some(snippet) = preview {
                    rsx! {
                        span { class: "wikilink-wrap",
                            {link}
                            span { class: "wikilink-preview", "{snippet}" }
                        }
                    }
                } else {
                    link
                }
            }
        }
        Node::ExternalLink { label, url } => rsx! {
            a { class: "ext", href: "{url}", target: "_blank", rel: "noopener", "{label}" }
        },
        Node::Image { alt, url } => {
            let assets = try_use_context::<AssetBaseResolver>().unwrap_or_default();
            let resolved = assets.resolve(url.as_str());
            rsx! {
                img { class: "inline-image", src: "{resolved}", alt: "{alt}", loading: "lazy" }
            }
        }
        Node::MathInline(src) => rsx! {
            span { class: "math math-inline", "{src}" }
        },
        Node::MathBlock(src) => rsx! {
            div { class: "math math-block", "{src}" }
        },
        Node::FootnoteRef(id) => {
            let href = format!("#fn-{id}");
            rsx! {
                sup { class: "footnote-ref",
                    a { href: "{href}", "[{id}]" }
                }
            }
        }
        Node::Media { kind, target } => {
            match kind {
                crate::parser::MediaKind::Video => rsx! {
                    video {
                        class: "media-video",
                        src: "{target}",
                        controls: true,
                        style: "max-width: 100%;",
                    }
                },
                crate::parser::MediaKind::Youtube => {
                    // Accept either a bare id or a full URL; extract
                    // the id from common URL shapes.
                    let id = extract_youtube_id(&target);
                    let src = format!("https://www.youtube.com/embed/{id}");
                    rsx! {
                        iframe {
                            class: "media-youtube",
                            src: "{src}",
                            width: "560",
                            height: "315",
                            "frameborder": "0",
                            allowfullscreen: true,
                        }
                    }
                }
                crate::parser::MediaKind::Tweet => {
                    // Render as a link card; full Twitter embed
                    // requires their JS which we don't want to
                    // ship by default.
                    let url = if target.starts_with("http") {
                        target.clone()
                    } else {
                        format!("https://twitter.com/i/web/status/{target}")
                    };
                    rsx! {
                        a {
                            class: "media-tweet",
                            href: "{url}",
                            target: "_blank",
                            rel: "noopener",
                            "Tweet ↗"
                        }
                    }
                }
            }
        }
        Node::VideoTimestamp(secs) => {
            let label = crate::parser::format_timestamp(secs);
            rsx! {
                button { class: "video-timestamp",
                    "data-ts-seconds": "{secs}",
                    title: "Seek to {label}",
                    "▶ {label}"
                }
            }
        }
        Node::PdfMacro(url) => {
            // Strip vault-relative `../` so the chip label is the
            // bare filename; the click delegate handles the URL.
            let label = url.rsplit('/').next().unwrap_or(&url).to_string();
            rsx! {
                button { class: "pdf-macro",
                    "data-pdf-url": "{url}",
                    title: "Open {url}",
                    "📄 {label}"
                }
            }
        }
        Node::Hashtag(tag) => {
            let nav = try_use_context::<TagNavigator>().unwrap_or_default();
            let tag_lower = tag.to_lowercase();
            if let Some(cb) = nav.0 {
                let t = tag_lower.clone();
                rsx! {
                    button { class: "tag",
                        onclick: move |_| cb.call(t.clone()),
                        "#{tag}"
                    }
                }
            } else {
                let href = format!("/tags/{tag_lower}/");
                rsx! {
                    a { class: "tag", href: "{href}", "#{tag}" }
                }
            }
        }
        Node::Template {
            name,
            contents,
            broken,
        } => {
            if broken {
                rsx! {
                    span { class: "template-broken",
                        title: "no template named '{name}'",
                        "{{{{template {name}}}}}"
                    }
                }
            } else {
                rsx! {
                    aside { class: "template-expansion",
                        for (i, body) in contents.iter().enumerate() {
                            div { key: "{i}", class: "template-line", "{body}" }
                        }
                    }
                }
            }
        }
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
                            &PagePropertyResolver::default(),
                            &TemplateResolver::default(),
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
                let nav = try_use_context::<BlockRefNavigator>().and_then(|n| n.0);
                if let Some(cb) = nav {
                    let target_for_click = target_id;
                    rsx! {
                        span {
                            class: "block-ref",
                            style: "cursor: pointer;",
                            title: "{snippet}",
                            onclick: move |e: Event<MouseData>| {
                                e.prevent_default();
                                e.stop_propagation();
                                cb.call(target_for_click);
                            },
                            "{snippet}"
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn asset_base_passes_absolute_urls() {
        let r = AssetBaseResolver::from_root("/tmp/vault".into());
        assert_eq!(
            r.resolve("https://example.com/x.png"),
            "https://example.com/x.png"
        );
        assert_eq!(
            r.resolve("data:image/png;base64,iVBOR"),
            "data:image/png;base64,iVBOR"
        );
    }

    #[test]
    fn asset_base_rewrites_relative_urls() {
        let r = AssetBaseResolver::from_root("/tmp/vault".into());
        assert_eq!(
            r.resolve("../assets/foo.png"),
            "file:///tmp/vault/assets/foo.png"
        );
        assert_eq!(
            r.resolve("./assets/foo.png"),
            "file:///tmp/vault/assets/foo.png"
        );
        assert_eq!(
            r.resolve("assets/foo.png"),
            "file:///tmp/vault/assets/foo.png"
        );
    }

    #[test]
    fn asset_base_empty_passes_through() {
        let r = AssetBaseResolver::default();
        assert_eq!(r.resolve("../assets/foo.png"), "../assets/foo.png");
    }

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

    #[test]
    fn parse_property_line_basic() {
        assert_eq!(
            parse_property_line("priority:: high"),
            Some(("priority".to_string(), "high".to_string()))
        );
        assert_eq!(
            parse_property_line("  id:: deadbeef"),
            Some(("id".to_string(), "deadbeef".to_string()))
        );
        // Underscore + dash + digits in key.
        assert_eq!(
            parse_property_line("foo-bar_2:: baz"),
            Some(("foo-bar_2".to_string(), "baz".to_string()))
        );
    }

    #[test]
    fn parse_property_line_rejects_bad_shapes() {
        // Uppercase key.
        assert_eq!(parse_property_line("Priority:: high"), None);
        // Single colon.
        assert_eq!(parse_property_line("priority: high"), None);
        // Empty key.
        assert_eq!(parse_property_line(":: high"), None);
        // Not a property line.
        assert_eq!(parse_property_line("hello world"), None);
        // Empty.
        assert_eq!(parse_property_line(""), None);
    }

    #[test]
    fn peel_block_properties_strips_lines() {
        let (json, rest) =
            peel_block_properties("first line\nstatus:: active\nsecond line\npriority:: high");
        // Both properties survive.
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(v["status"], serde_json::json!("active"));
        assert_eq!(v["priority"], serde_json::json!("high"));
        // Non-property lines preserved in order.
        assert_eq!(rest, "first line\nsecond line");
    }

    #[test]
    fn peel_block_properties_keeps_first_on_dupe() {
        let (json, _rest) = peel_block_properties("priority:: high\npriority:: low");
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(v["priority"], serde_json::json!("high"));
    }

    #[test]
    fn peel_block_properties_passthrough() {
        let (json, rest) = peel_block_properties("just text\nmore text");
        assert_eq!(json, "{}");
        assert_eq!(rest, "just text\nmore text");
    }

    #[test]
    fn peel_drawers_logbook() {
        let content = "task body\n:LOGBOOK:\nCLOCK: 2026-05-19 09:00\nCLOCK: 2026-05-19 11:30\n:END:\nmore body";
        let (drawers, rest) = peel_drawers(content);
        assert_eq!(drawers.len(), 1);
        assert_eq!(drawers[0].name, "LOGBOOK");
        assert!(drawers[0].body.contains("CLOCK: 2026-05-19 09:00"));
        assert_eq!(rest, "task body\nmore body");
    }

    #[test]
    fn peel_drawers_case_insensitive_end() {
        // `:end:` lowercase should still close.
        let (drawers, rest) = peel_drawers(":NOTES:\nhello\n:end:");
        assert_eq!(drawers.len(), 1);
        assert_eq!(drawers[0].name, "NOTES");
        assert_eq!(drawers[0].body, "hello");
        assert!(rest.is_empty());
    }

    #[test]
    fn peel_drawers_unclosed_preserved_as_content() {
        // No `:END:` → entire chunk stays as literal content so
        // the user doesn't lose data.
        let (drawers, rest) = peel_drawers(":LOGBOOK:\nclock entry\nmore text");
        assert!(drawers.is_empty());
        assert_eq!(rest, ":LOGBOOK:\nclock entry\nmore text");
    }

    #[test]
    fn peel_drawers_multiple_sections() {
        let content = ":PROPERTIES:\nid:: abc\n:END:\nbody\n:LOGBOOK:\nclock\n:END:\ntail";
        let (drawers, rest) = peel_drawers(content);
        assert_eq!(drawers.len(), 2);
        assert_eq!(drawers[0].name, "PROPERTIES");
        assert_eq!(drawers[1].name, "LOGBOOK");
        assert_eq!(rest, "body\ntail");
    }
}
