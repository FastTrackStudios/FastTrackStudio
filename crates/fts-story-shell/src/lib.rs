//! Interactive Lookbook-style shell.
//!
//! Renderer-agnostic Dioxus component. Drop into any Dioxus 0.7 app
//! (web/desktop/native/mobile) and it renders:
//!
//! - **Top bar** — branding plus a `header_extras` slot the host fills
//!   with theme controls, density toggle, etc.
//! - **Sidebar** — every story registered via
//!   [`STORIES`](fts_story_runtime::STORIES), grouped by `category`.
//! - **Preview pane** — the selected story rendered with the current
//!   knob values (and inheriting whatever theme the host provides).
//! - **Knob editor** — typed inputs above the preview, one per knob.
//!
//! ## Styling
//!
//! The shell does **not** ship its own colour palette. Every surface
//! uses the standard shadcn-style Tailwind tokens — `bg-background`,
//! `bg-card`, `text-foreground`, `text-muted-foreground`, `border`,
//! `border-border`, `bg-accent`, etc. — so when the host wraps the
//! Lookbook in a `ThemeProvider`, switching the preset or mode in the
//! top bar reskins the shell *and* every preview at once.
//!
//! Consumers must therefore have a Tailwind config / generated CSS that
//! defines those tokens. fts-ui ships that out of the box; any other
//! consumer (frame-ui, etc.) just needs the same `@theme` block in
//! their tailwind input file.

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_story_runtime::{render_fn, KnobKind, KnobSource, KnobValue, Story, STORIES};

#[derive(Props, Clone, PartialEq)]
pub struct LookbookProps {
    /// Optional content rendered into the sticky top bar — typically a
    /// theme picker, density toggle, or other globally-scoped control.
    /// Anything dispatched from here should propagate through Dioxus
    /// context so the previews below pick it up.
    #[props(default)]
    pub header_extras: Option<Element>,
    /// Optional starting story by name. Useful for snapshot harnesses
    /// that boot the binary into a specific preview.
    #[props(default)]
    pub initial_story: Option<String>,
    /// When `false`, render only the preview (no sidebar / top bar /
    /// header / knob editor). Used by the parity harness so the
    /// captured pixels are just the component, not the chrome.
    #[props(default = true)]
    pub chrome: bool,
}

#[component]
pub fn Lookbook(props: LookbookProps) -> Element {
    let stories: Vec<&'static Story> = {
        let mut v: Vec<&'static Story> = STORIES.iter().copied().collect();
        v.sort_by_key(|s| (s.category.unwrap_or("zzz"), s.name));
        v
    };

    let initial = props
        .initial_story
        .clone()
        .unwrap_or_else(|| stories.first().map(|s| s.name.to_string()).unwrap_or_default());
    let selected = use_signal(|| initial);

    let current = stories
        .iter()
        .find(|s| s.name == selected())
        .copied()
        .or_else(|| stories.first().copied());

    if !props.chrome {
        // Headless / snapshot mode: render the preview only. No
        // sidebar, no top bar, no story header. The host app sets a
        // fixed window size so the captured pixels are deterministic.
        return rsx! {
            div { class: "min-h-screen bg-background text-foreground p-6",
                "data-fts-story-name": current.map(|s| s.name).unwrap_or_default(),
                if let Some(story) = current {
                    ChromelessPreview { story }
                }
            }
        };
    }

    rsx! {
        div { class: "min-h-screen bg-background text-foreground",
            header { class: "sticky top-0 z-20 flex items-center gap-4 h-12 px-4 border-b border-border bg-card text-card-foreground",
                div { class: "flex items-baseline gap-3",
                    span { class: "text-xs font-semibold uppercase tracking-wider", "fts-story" }
                    span { class: "text-[11px] text-muted-foreground", "{stories.len()} stories" }
                }
                if let Some(extras) = props.header_extras {
                    div { class: "ml-auto flex items-center gap-2",
                        {extras}
                    }
                }
            }
            div { class: "grid grid-cols-[240px_1fr] min-h-[calc(100vh-3rem)]",
                aside { class: "sticky top-12 self-start max-h-[calc(100vh-3rem)] w-60 overflow-y-auto border-r border-border bg-card text-card-foreground",
                    SidebarTree { stories: stories.clone(), selected }
                }
                main { class: "p-6 overflow-y-auto",
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
    let mut groups: Vec<(&'static str, Vec<&'static Story>)> = Vec::new();
    for story in &stories {
        let cat = story.category.unwrap_or("Uncategorised");
        match groups.last_mut() {
            Some((g, list)) if *g == cat => list.push(*story),
            _ => groups.push((cat, vec![*story])),
        }
    }

    rsx! {
        nav { class: "flex flex-col py-2",
            for (category, items) in groups {
                section { class: "py-2",
                    h2 { class: "px-4 mb-1 text-[10px] font-semibold uppercase tracking-wider text-muted-foreground",
                        "{category}"
                    }
                    ul { class: "flex flex-col",
                        for story in items {
                            {
                                let name = story.name;
                                let active = selected() == name;
                                let class = if active {
                                    "block w-full text-left px-4 py-1.5 text-sm border-l-2 border-primary bg-accent/40 text-foreground font-medium"
                                } else {
                                    "block w-full text-left px-4 py-1.5 text-sm border-l-2 border-transparent hover:bg-accent/20 text-muted-foreground hover:text-foreground"
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

/// Chromeless preview used by the parity / snapshot harness — renders
/// the story with its declared default knobs, no editor or header.
#[component]
fn ChromelessPreview(story: &'static Story) -> Element {
    let initial: HashMap<&'static str, KnobValue> = story
        .knobs
        .iter()
        .filter_map(|spec| spec.default.as_ref().map(|v| (spec.name, clone_knob(v))))
        .collect();
    let knob_state = use_signal(|| initial);
    rsx! {
        StoryPreview { story, knob_state }
    }
}

#[component]
fn StoryView(story: &'static Story) -> Element {
    let initial: HashMap<&'static str, KnobValue> = story
        .knobs
        .iter()
        .filter_map(|spec| spec.default.as_ref().map(|v| (spec.name, clone_knob(v))))
        .collect();
    let knob_state = use_signal(|| initial);

    rsx! {
        div { key: "{story.name}", class: "flex flex-col gap-4 max-w-5xl",
            StoryHeader { story }
            if !story.knobs.is_empty() {
                KnobEditor { story, knob_state }
            }
            div { class: "rounded-lg border border-border bg-card text-card-foreground p-6 min-h-[14rem]",
                StoryPreview { story, knob_state }
            }
        }
    }
}

#[component]
fn StoryHeader(story: &'static Story) -> Element {
    rsx! {
        header { class: "pb-3 border-b border-border",
            div { class: "text-sm text-muted-foreground",
                if let Some(category) = story.category {
                    span { "{category}" }
                    span { class: "mx-1", "/" }
                }
                span { class: "text-foreground font-semibold", "{story.name}" }
            }
            if !story.description.is_empty() {
                p { class: "mt-1 text-sm", "{story.description}" }
            }
            if !story.source.is_empty() {
                p { class: "mt-1 text-[11px] font-mono text-muted-foreground",
                    "{story.source}"
                }
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
        section { class: "rounded-lg border border-border bg-card text-card-foreground p-4 grid gap-3 grid-cols-[repeat(auto-fill,minmax(13rem,1fr))]",
            for spec in story.knobs.iter() {
                {
                    let name = spec.name;
                    let doc = spec.doc;
                    rsx! {
                        div { class: "flex flex-col gap-1",
                            label { class: "text-[11px] font-semibold uppercase tracking-wider text-muted-foreground",
                                "{name}"
                            }
                            if !doc.is_empty() {
                                p { class: "text-[11px] text-muted-foreground", "{doc}" }
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

    let input_class = "h-8 rounded border border-input bg-background px-2 text-sm text-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring";

    match &spec.kind {
        KnobKind::Bool => {
            let checked = matches!(current, Some(KnobValue::Bool(true)));
            rsx! {
                input {
                    r#type: "checkbox",
                    class: "size-4 accent-primary",
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
                    class: input_class,
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
            // Free-form text edits leak via Box::leak so the resulting
            // &'static str can flow into KnobValue::Str. This is bounded
            // and fine for an interactive dev tool; the snapshot
            // harness uses defaults verbatim.
            let on_change = move |e: FormEvent| {
                let leaked: &'static str = Box::leak(e.value().into_boxed_str());
                knob_state.write().insert(name, KnobValue::Str(leaked));
            };
            if *multiline {
                rsx! {
                    textarea {
                        class: "{input_class} h-20 font-mono",
                        value: "{value_str}",
                        oninput: on_change,
                    }
                }
            } else {
                rsx! {
                    input {
                        r#type: "text",
                        class: input_class,
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
                    class: input_class,
                    onchange: move |e| {
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
            span { class: "text-[11px] text-muted-foreground italic",
                "(no editor — uses default)"
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

#[component]
fn EmptyState() -> Element {
    rsx! {
        div { class: "max-w-md text-muted-foreground text-sm",
            h2 { class: "text-foreground text-base font-semibold mb-2", "No stories registered" }
            p {
                "Add "
                code { class: "rounded bg-accent/40 px-1 py-0.5 text-[12px]", "#[story]" }
                " to a Dioxus component, or hand-roll a "
                code { class: "rounded bg-accent/40 px-1 py-0.5 text-[12px]", "Story" }
                " value and register it via "
                code { class: "rounded bg-accent/40 px-1 py-0.5 text-[12px]", "#[linkme::distributed_slice(STORIES)]" }
                "."
            }
        }
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
