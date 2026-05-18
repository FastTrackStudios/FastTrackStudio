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

/// Sidebar — flat alphabetical page list, current page marked.
#[component]
pub fn Sidebar(pages: Vec<Page>, current_id: Option<uuid::Uuid>) -> Element {
    let mut sorted = pages.clone();
    sorted.sort_by(|a, b| a.basename.to_lowercase().cmp(&b.basename.to_lowercase()));
    rsx! {
        nav { class: "sidebar",
            div { class: "sidebar-section",
                a { class: "sidebar-link", href: "/", "Home" }
                a { class: "sidebar-link", href: "/graph/", "Graph" }
                a { class: "sidebar-link", href: "/tags/", "Tags" }
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
            ul {
                for p in sorted {
                    {
                        let slug = crate::site::slugify(&p.basename);
                        let active = current_id == Some(p.id);
                        let cls = if active { "active" } else { "" };
                        rsx! {
                            li { key: "{p.id}",
                                a { class: "{cls}", href: "/{slug}/", "{p.basename}" }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// "Linked by" panel rendered below the article. Uses the
/// `BacklinkEntry` list passed in (precomputed from the
/// `BacklinkIndex` in `graph.rs`).
#[component]
pub fn BacklinksPanel(entries: Vec<BacklinkEntry>) -> Element {
    if entries.is_empty() {
        return rsx! {};
    }
    rsx! {
        section { class: "backlinks",
            h2 { "Linked by" }
            ul {
                for e in entries {
                    {
                        let href = format!("/{}/", e.slug);
                        rsx! {
                            li { key: "{e.slug}",
                                a { href: "{href}", "{e.label}" }
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
pub fn PageContent(blocks: Vec<Block>, journal_day: Option<String>) -> Element {
    let mut sorted = blocks.clone();
    sorted.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    rsx! {
        if let Some(d) = journal_day {
            div { class: "page-meta", "{d}" }
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
    let task_glyph = match block.list_task.as_deref() {
        Some(" ") | Some("") => Some(("☐", false)),
        Some("/") => Some(("◐", false)),
        Some("x") | Some("X") => Some(("☑", true)),
        _ => None,
    };
    let resolver = try_use_context::<WikiResolver>().unwrap_or_default();
    let inlines = inline::parse(&block.content, &resolver);
    match block.kind.as_str() {
        "heading" => {
            let level = block.heading_level.unwrap_or(1).clamp(1, 6) as u8;
            // Dynamic h{1..6} via match — small cost, big clarity.
            match level {
                1 => rsx! { h1 { Inlines { nodes: inlines } } },
                2 => rsx! { h2 { Inlines { nodes: inlines } } },
                3 => rsx! { h3 { Inlines { nodes: inlines } } },
                4 => rsx! { h4 { Inlines { nodes: inlines } } },
                5 => rsx! { h5 { Inlines { nodes: inlines } } },
                _ => rsx! { h6 { Inlines { nodes: inlines } } },
            }
        }
        "code" => {
            let lang = block
                .code_lang
                .clone()
                .filter(|s| !s.is_empty())
                .unwrap_or_else(|| "plain".into());
            // Server-side syntax highlighting via syntect. The
            // output is raw HTML (`<span style=…>` per token);
            // Dioxus needs `dangerous_inner_html` to embed it
            // verbatim. Safe because the input is the user's own
            // vault content + syntect generates only spans with
            // hex color styles.
            let highlighted = crate::syntax::highlight(&block.content, &lang);
            rsx! {
                pre { class: "code",
                    div { class: "code-lang", "{lang}" }
                    code {
                        dangerous_inner_html: "{highlighted}",
                    }
                }
            }
        }
        "list_item" => rsx! {
            div { class: "list-item",
                "• "
                if let Some((g, struck)) = task_glyph {
                    span { class: if struck { "task-done" } else { "" }, "{g} " }
                }
                Inlines { nodes: inlines }
            }
        },
        _ => rsx! {
            p {
                if let Some((g, _)) = task_glyph {
                    span { "{g} " }
                }
                Inlines { nodes: inlines }
            }
        },
    }
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
    }
}
