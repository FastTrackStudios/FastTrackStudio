//! Interactive Lookbook-style shell.
//!
//! Renderer-agnostic Dioxus component. Drop into any Dioxus 0.7 app
//! (web/desktop/native/mobile) and it renders:
//!
//! - **Sidebar** — every story registered via
//!   [`STORIES`](fts_story_runtime::STORIES), grouped by `category`.
//! - **Preview pane** — the selected story rendered with the current
//!   knob values.
//! - **Header** — story name, category breadcrumb, source location.
//!
//! Knob editor + state-matrix toggle land in Phase 2 once the `#[story]`
//! macro is emitting `KnobSpec` defaults. For now the shell renders each
//! story with [`NoKnobs`](fts_story_runtime::NoKnobs).

use dioxus::prelude::*;
use fts_story_runtime::{render_fn, NoKnobs, Story, STORIES};

#[component]
pub fn Lookbook() -> Element {
    // Snapshot the global registry once. STORIES is a `linkme` slice
    // populated at link time; iterating it on every render is cheap, but
    // we pre-collect into a Vec so we can sort by category/name.
    let stories: Vec<&'static Story> = {
        let mut v: Vec<&'static Story> = STORIES.iter().copied().collect();
        v.sort_by_key(|s| (s.category.unwrap_or("zzz"), s.name));
        v
    };

    let initial = stories.first().map(|s| s.name).unwrap_or("");
    let selected = use_signal(|| initial.to_string());

    let current = stories
        .iter()
        .find(|s| s.name == selected())
        .copied()
        .or_else(|| stories.first().copied());

    rsx! {
        div { class: "fts-story-shell",
            style { {SHELL_CSS} }
            div { class: "fts-story-shell__layout",
                aside { class: "fts-story-shell__sidebar",
                    h1 { class: "fts-story-shell__brand", "fts-story" }
                    p { class: "fts-story-shell__count", "{stories.len()} stories" }
                    SidebarTree {
                        stories: stories.clone(),
                        selected,
                    }
                }
                main { class: "fts-story-shell__main",
                    if let Some(story) = current {
                        StoryHeader { story }
                        div { class: "fts-story-shell__preview",
                            StoryPreview { story }
                        }
                    } else {
                        EmptyState {}
                    }
                }
            }
        }
    }
}

#[component]
fn SidebarTree(stories: Vec<&'static Story>, selected: Signal<String>) -> Element {
    // Group by category preserving the sorted order.
    let mut groups: Vec<(&'static str, Vec<&'static Story>)> = Vec::new();
    for story in &stories {
        let cat = story.category.unwrap_or("Uncategorised");
        match groups.last_mut() {
            Some((g, list)) if *g == cat => list.push(*story),
            _ => groups.push((cat, vec![*story])),
        }
    }

    rsx! {
        nav { class: "fts-story-shell__nav",
            for (category, items) in groups {
                section { class: "fts-story-shell__group",
                    h2 { class: "fts-story-shell__group-name", "{category}" }
                    ul { class: "fts-story-shell__group-list",
                        for story in items {
                            {
                                let name = story.name;
                                let active = selected() == name;
                                let class = if active {
                                    "fts-story-shell__nav-item fts-story-shell__nav-item--active"
                                } else {
                                    "fts-story-shell__nav-item"
                                };
                                rsx! {
                                    li {
                                        button {
                                            class,
                                            onclick: move |_| selected.set(name.to_string()),
                                            "{name}"
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
}

#[component]
fn StoryHeader(story: &'static Story) -> Element {
    rsx! {
        header { class: "fts-story-shell__header",
            div { class: "fts-story-shell__crumbs",
                if let Some(category) = story.category {
                    span { class: "fts-story-shell__crumb", "{category}" }
                    span { class: "fts-story-shell__crumb-sep", " / " }
                }
                span { class: "fts-story-shell__crumb fts-story-shell__crumb--leaf",
                    "{story.name}"
                }
            }
            if !story.description.is_empty() {
                p { class: "fts-story-shell__description", "{story.description}" }
            }
            if !story.source.is_empty() {
                p { class: "fts-story-shell__source", "{story.source}" }
            }
        }
    }
}

#[component]
fn StoryPreview(story: &'static Story) -> Element {
    // SAFETY: `story.render` is a `RenderFn` produced via the const_story
    // builder or the (forthcoming) `#[story]` macro.
    let render = unsafe { render_fn(story) };
    render(&NoKnobs)
}

#[component]
fn EmptyState() -> Element {
    rsx! {
        div { class: "fts-story-shell__empty",
            h2 { "No stories registered" }
            p {
                "Add "
                code { "#[story]" }
                " to a Dioxus component, or hand-roll a "
                code { "Story" }
                " value and register it via "
                code { "#[linkme::distributed_slice(STORIES)]" }
                "."
            }
        }
    }
}

const SHELL_CSS: &str = r#"
.fts-story-shell {
    --fg: #e5e7eb;
    --fg-muted: #9ca3af;
    --bg: #0b0f17;
    --bg-elev: #111827;
    --border: #1f2937;
    --accent: #60a5fa;

    color: var(--fg);
    background: var(--bg);
    font-family: ui-sans-serif, system-ui, sans-serif;
    min-height: 100vh;
}
.fts-story-shell__layout {
    display: grid;
    grid-template-columns: 240px 1fr;
    min-height: 100vh;
}
.fts-story-shell__sidebar {
    border-right: 1px solid var(--border);
    background: var(--bg-elev);
    padding: 12px 0;
    overflow-y: auto;
    position: sticky;
    top: 0;
    max-height: 100vh;
}
.fts-story-shell__brand {
    font-size: 13px;
    font-weight: 700;
    letter-spacing: 0.04em;
    text-transform: uppercase;
    color: var(--fg-muted);
    margin: 0 16px 4px;
}
.fts-story-shell__count {
    font-size: 11px;
    color: var(--fg-muted);
    margin: 0 16px 12px;
}
.fts-story-shell__group {
    padding: 8px 0;
    border-top: 1px solid var(--border);
}
.fts-story-shell__group-name {
    font-size: 11px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: var(--fg-muted);
    margin: 0 16px 6px;
}
.fts-story-shell__group-list {
    list-style: none;
    margin: 0;
    padding: 0;
}
.fts-story-shell__nav-item {
    display: block;
    width: 100%;
    text-align: left;
    background: transparent;
    border: 0;
    color: var(--fg);
    font: inherit;
    padding: 6px 16px;
    cursor: pointer;
    border-left: 2px solid transparent;
}
.fts-story-shell__nav-item:hover {
    background: rgba(96, 165, 250, 0.08);
}
.fts-story-shell__nav-item--active {
    background: rgba(96, 165, 250, 0.16);
    border-left-color: var(--accent);
    font-weight: 600;
}
.fts-story-shell__main {
    padding: 24px 32px;
    overflow-y: auto;
}
.fts-story-shell__header {
    margin-bottom: 24px;
    padding-bottom: 16px;
    border-bottom: 1px solid var(--border);
}
.fts-story-shell__crumbs {
    font-size: 13px;
    color: var(--fg-muted);
    margin-bottom: 6px;
}
.fts-story-shell__crumb--leaf {
    color: var(--fg);
    font-weight: 600;
}
.fts-story-shell__description {
    font-size: 14px;
    color: var(--fg);
    margin: 4px 0 0;
}
.fts-story-shell__source {
    font-size: 11px;
    font-family: ui-monospace, monospace;
    color: var(--fg-muted);
    margin: 4px 0 0;
}
.fts-story-shell__preview {
    background: var(--bg-elev);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 24px;
    min-height: 240px;
}
.fts-story-shell__empty {
    color: var(--fg-muted);
    max-width: 480px;
}
.fts-story-shell__empty code {
    background: var(--bg-elev);
    padding: 2px 6px;
    border-radius: 4px;
    font-size: 12px;
}
"#;
