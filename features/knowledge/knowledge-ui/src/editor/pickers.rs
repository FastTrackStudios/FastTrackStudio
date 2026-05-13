//! `[[`, `((`, `/`, `#` autocomplete popovers.
//!
//! Each picker is a dumb component: receives candidates + a query
//! signal, emits `on_pick`/`on_close`. The outliner owns the visibility
//! state and the candidate-sourcing wiring (which is feature-scoped:
//! page list for wikilink, block list for blockref, tag table for tag,
//! static command list for slash).

pub use crate::common::{BlockBrief, PageBrief};
use dioxus::prelude::*;
use nucleo_matcher::{
    Matcher,
    pattern::{CaseMatching, Normalization, Pattern},
};

/// One slash-command entry.
#[derive(Clone, Debug, PartialEq)]
pub struct SlashCommand {
    pub id: &'static str,
    pub label: &'static str,
    pub description: &'static str,
}

/// Default v1 slash command palette.
pub fn default_slash_commands() -> Vec<SlashCommand> {
    vec![
        SlashCommand {
            id: "h1",
            label: "Heading 1",
            description: "Large section heading",
        },
        SlashCommand {
            id: "h2",
            label: "Heading 2",
            description: "Subsection heading",
        },
        SlashCommand {
            id: "h3",
            label: "Heading 3",
            description: "Smaller subsection",
        },
        SlashCommand {
            id: "code",
            label: "Code Block",
            description: "Fenced code with language",
        },
        SlashCommand {
            id: "quote",
            label: "Quote",
            description: "Blockquote",
        },
        SlashCommand {
            id: "callout",
            label: "Callout",
            description: "> [!note] callout",
        },
        SlashCommand {
            id: "ol",
            label: "Numbered List",
            description: "Ordered list",
        },
        SlashCommand {
            id: "ul",
            label: "Bullet List",
            description: "Unordered list",
        },
        SlashCommand {
            id: "divider",
            label: "Divider",
            description: "--- thematic break",
        },
        SlashCommand {
            id: "date",
            label: "Date (today)",
            description: "Insert today's date",
        },
        SlashCommand {
            id: "tag",
            label: "Tag",
            description: "Pick a tag",
        },
        SlashCommand {
            id: "page",
            label: "Page Link",
            description: "Insert [[…]] wikilink",
        },
    ]
}

fn fuzzy_filter<T, F>(items: &[T], query: &str, key: F) -> Vec<T>
where
    T: Clone,
    F: Fn(&T) -> String,
{
    if query.is_empty() {
        return items.to_vec();
    }
    let mut matcher = Matcher::new(nucleo_matcher::Config::DEFAULT);
    let pat = Pattern::parse(query, CaseMatching::Smart, Normalization::Smart);
    let mut scored: Vec<(u32, T)> = items
        .iter()
        .filter_map(|item| {
            let keys = key(item);
            let mut buf = Vec::new();
            let haystack = nucleo_matcher::Utf32Str::new(&keys, &mut buf);
            pat.score(haystack, &mut matcher).map(|s| (s, item.clone()))
        })
        .collect();
    scored.sort_by(|a, b| b.0.cmp(&a.0));
    scored.into_iter().map(|(_, t)| t).collect()
}

#[component]
pub fn WikilinkPicker(
    query: Signal<String>,
    candidates: Vec<PageBrief>,
    on_pick: EventHandler<PageBrief>,
    on_close: EventHandler<()>,
) -> Element {
    let q = query();
    let filtered = fuzzy_filter(&candidates, &q, |p| format!("{} {}", p.basename, p.path));
    rsx! {
        div { class: "absolute z-50 max-h-72 w-80 overflow-y-auto rounded-md border bg-popover p-1 shadow-md",
            onkeydown: move |ev| if ev.key().to_string() == "Escape" { on_close.call(()); },
            if filtered.is_empty() {
                div { class: "px-3 py-2 text-sm text-muted-foreground", "No pages match" }
            }
            for p in filtered {
                {
                    let pp = p.clone();
                    rsx! {
                        button {
                            key: "{p.id}",
                            class: "flex w-full flex-col items-start rounded px-2 py-1 text-left hover:bg-accent",
                            onclick: move |_| on_pick.call(pp.clone()),
                            div { class: "text-sm", "{p.basename}" }
                            div { class: "text-xs text-muted-foreground", "{p.path}" }
                        }
                    }
                }
            }
        }
    }
}

#[component]
pub fn BlockRefPicker(
    query: Signal<String>,
    candidates: Vec<BlockBrief>,
    on_pick: EventHandler<BlockBrief>,
    on_close: EventHandler<()>,
) -> Element {
    let q = query();
    let filtered = fuzzy_filter(&candidates, &q, |b| {
        format!("{} {}", b.preview, b.page_basename)
    });
    rsx! {
        div { class: "absolute z-50 max-h-72 w-96 overflow-y-auto rounded-md border bg-popover p-1 shadow-md",
            onkeydown: move |ev| if ev.key().to_string() == "Escape" { on_close.call(()); },
            if filtered.is_empty() {
                div { class: "px-3 py-2 text-sm text-muted-foreground", "No blocks match" }
            }
            for b in filtered {
                {
                    let bb = b.clone();
                    rsx! {
                        button {
                            key: "{b.id}",
                            class: "flex w-full flex-col items-start rounded px-2 py-1 text-left hover:bg-accent",
                            onclick: move |_| on_pick.call(bb.clone()),
                            div { class: "text-sm truncate w-full", "{b.preview}" }
                            div { class: "text-xs text-muted-foreground", "in {b.page_basename}" }
                        }
                    }
                }
            }
        }
    }
}

#[component]
pub fn SlashCommandMenu(
    query: Signal<String>,
    commands: Vec<SlashCommand>,
    on_run: EventHandler<SlashCommand>,
) -> Element {
    let q = query();
    let filtered = fuzzy_filter(&commands, &q, |c| format!("{} {}", c.label, c.description));
    rsx! {
        div { class: "absolute z-50 max-h-72 w-80 overflow-y-auto rounded-md border bg-popover p-1 shadow-md",
            if filtered.is_empty() {
                div { class: "px-3 py-2 text-sm text-muted-foreground", "No commands" }
            }
            for cmd in filtered {
                {
                    let cc = cmd.clone();
                    rsx! {
                        button {
                            key: "{cmd.id}",
                            class: "flex w-full flex-col items-start rounded px-2 py-1 text-left hover:bg-accent",
                            onclick: move |_| on_run.call(cc.clone()),
                            div { class: "text-sm font-medium", "{cmd.label}" }
                            div { class: "text-xs text-muted-foreground", "{cmd.description}" }
                        }
                    }
                }
            }
        }
    }
}
