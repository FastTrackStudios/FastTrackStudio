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

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_story_runtime::{render_fn, KnobKind, KnobSource, KnobValue, Story, STORIES};

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
                        StoryView { story }
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

/// `StoryView` ties the header, knob editor, and preview together.
/// Each story gets its own keyed instance so the per-story knob state
/// resets cleanly when the user navigates between stories.
#[component]
fn StoryView(story: &'static Story) -> Element {
    let initial: HashMap<&'static str, KnobValue> = story
        .knobs
        .iter()
        .filter_map(|spec| spec.default.as_ref().map(|v| (spec.name, clone_knob(v))))
        .collect();
    let knob_state = use_signal(|| initial);

    rsx! {
        div { key: "{story.name}",
            StoryHeader { story }
            if !story.knobs.is_empty() {
                KnobEditor { story, knob_state }
            }
            div { class: "fts-story-shell__preview",
                StoryPreview { story, knob_state }
            }
        }
    }
}

#[component]
fn StoryPreview(story: &'static Story, knob_state: Signal<HashMap<&'static str, KnobValue>>) -> Element {
    // SAFETY: `story.render` is a `RenderFn` produced via the
    // `const_story` builder or the `#[story]` proc-macro.
    let render = unsafe { render_fn(story) };
    let snapshot = knob_state.read().clone();
    render(&MapKnobs(&snapshot))
}

#[component]
fn KnobEditor(
    story: &'static Story,
    knob_state: Signal<HashMap<&'static str, KnobValue>>,
) -> Element {
    rsx! {
        section { class: "fts-story-shell__knobs",
            for spec in story.knobs.iter() {
                {
                    let name = spec.name;
                    let doc = spec.doc;
                    rsx! {
                        div { class: "fts-story-shell__knob",
                            label { class: "fts-story-shell__knob-name", "{name}" }
                            if !doc.is_empty() {
                                p { class: "fts-story-shell__knob-doc", "{doc}" }
                            }
                            KnobControl { spec, knob_state }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn KnobControl(
    spec: &'static fts_story_runtime::KnobSpec,
    knob_state: Signal<HashMap<&'static str, KnobValue>>,
) -> Element {
    let name = spec.name;
    let current = knob_state
        .read()
        .get(name)
        .or(spec.default.as_ref())
        .map(clone_knob);

    match &spec.kind {
        KnobKind::Bool => {
            let checked = matches!(current, Some(KnobValue::Bool(true)));
            rsx! {
                input {
                    r#type: "checkbox",
                    checked,
                    onchange: move |e| {
                        let v = e.value() == "true";
                        knob_state.write().insert(name, KnobValue::Bool(v));
                    },
                }
            }
        }
        KnobKind::Number { .. } => {
            let value_str = match &current {
                Some(KnobValue::Int(v)) => v.to_string(),
                Some(KnobValue::Float(v)) => v.to_string(),
                _ => String::new(),
            };
            rsx! {
                input {
                    r#type: "number",
                    class: "fts-story-shell__knob-input",
                    value: "{value_str}",
                    oninput: move |e| {
                        let raw = e.value();
                        let value = if raw.contains('.') {
                            raw.parse::<f64>().ok().map(KnobValue::Float)
                        } else {
                            raw.parse::<i64>().ok().map(KnobValue::Int)
                        };
                        if let Some(v) = value {
                            knob_state.write().insert(name, v);
                        }
                    },
                }
            }
        }
        KnobKind::String { multiline } => {
            let value_str = match &current {
                Some(KnobValue::Str(s)) => s.to_string(),
                _ => String::new(),
            };
            // KnobValue::Str holds &'static str — to support free-form
            // editing without leaking we store the edited string into
            // a side signal and only commit it on blur. For the MVP
            // we leak via Box::leak so the changes flow through to the
            // render thunk. This is fine for an interactive dev tool;
            // the snapshot harness uses defaults verbatim.
            let on_change = move |e: FormEvent| {
                let leaked: &'static str = Box::leak(e.value().into_boxed_str());
                knob_state.write().insert(name, KnobValue::Str(leaked));
            };
            if *multiline {
                rsx! {
                    textarea {
                        class: "fts-story-shell__knob-input fts-story-shell__knob-textarea",
                        value: "{value_str}",
                        oninput: on_change,
                    }
                }
            } else {
                rsx! {
                    input {
                        r#type: "text",
                        class: "fts-story-shell__knob-input",
                        value: "{value_str}",
                        oninput: on_change,
                    }
                }
            }
        }
        KnobKind::Enum { variants } => {
            let selected = match &current {
                Some(KnobValue::EnumVariant(v)) => v.to_string(),
                _ => variants.first().map(|s| s.to_string()).unwrap_or_default(),
            };
            rsx! {
                select {
                    class: "fts-story-shell__knob-input",
                    onchange: move |e| {
                        // Enum variant names are part of the static
                        // KnobSpec, so we leak (cheap, bounded) to
                        // produce the &'static str the KnobValue wants.
                        let leaked: &'static str = Box::leak(e.value().into_boxed_str());
                        knob_state.write().insert(name, KnobValue::EnumVariant(leaked));
                    },
                    for v in variants.iter() {
                        option { value: "{v}", selected: selected == *v, "{v}" }
                    }
                }
            }
        }
        KnobKind::Color | KnobKind::Opaque => rsx! {
            span { class: "fts-story-shell__knob-readonly",
                "(no editor — use defaults)"
            }
        },
    }
}

fn clone_knob(v: &KnobValue) -> KnobValue {
    match *v {
        KnobValue::Bool(b) => KnobValue::Bool(b),
        KnobValue::Int(i) => KnobValue::Int(i),
        KnobValue::Float(f) => KnobValue::Float(f),
        KnobValue::Str(s) => KnobValue::Str(s),
        KnobValue::EnumVariant(s) => KnobValue::EnumVariant(s),
    }
}

/// `KnobSource` over a borrowed map. Cheap to construct per-render so
/// `StoryPreview` doesn't have to plumb signals through the trait.
struct MapKnobs<'a>(&'a HashMap<&'static str, KnobValue>);

impl KnobSource for MapKnobs<'_> {
    fn get(&self, name: &'static str) -> Option<&KnobValue> {
        self.0.get(name)
    }
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
.fts-story-shell__knobs {
    background: var(--bg-elev);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 16px 20px;
    margin-bottom: 16px;
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
    gap: 12px 20px;
}
.fts-story-shell__knob {
    display: flex;
    flex-direction: column;
    gap: 4px;
}
.fts-story-shell__knob-name {
    font-size: 11px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.04em;
    color: var(--fg-muted);
}
.fts-story-shell__knob-doc {
    font-size: 11px;
    color: var(--fg-muted);
    margin: 0;
}
.fts-story-shell__knob-input {
    background: var(--bg);
    color: var(--fg);
    border: 1px solid var(--border);
    border-radius: 4px;
    padding: 4px 8px;
    font: inherit;
    font-size: 13px;
}
.fts-story-shell__knob-textarea {
    min-height: 60px;
    resize: vertical;
    font-family: ui-monospace, monospace;
}
.fts-story-shell__knob-readonly {
    font-size: 11px;
    color: var(--fg-muted);
    font-style: italic;
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
