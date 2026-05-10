//! Combobox — shadcn v4 maia style searchable select.

use dioxus::prelude::*;
use fts_story_runtime::story;
use nucleo_matcher::{
    Matcher, Utf32Str,
    pattern::{CaseMatching, Normalization, Pattern},
};

/// Fuzzy-match `query` against any of `candidates`. Returns `Some(score)`
/// for the best-matching candidate, or `None` if nothing matches.
///
/// Uses `nucleo-matcher` (the matcher extracted from Helix's Nucleo) so
/// the ranking matches what users expect from fzf / cmdk: subsequence
/// match with bonuses for prefix, word-boundary, and consecutive
/// characters. Non-ASCII strings (e.g. emoji keywords) round-trip
/// through a UTF-32 buffer.
pub(super) fn fuzzy_best_score(query: &str, candidates: &[&str]) -> Option<u32> {
    if query.is_empty() {
        return Some(0);
    }
    let mut matcher = Matcher::new(nucleo_matcher::Config::DEFAULT);
    let pattern = Pattern::parse(query, CaseMatching::Ignore, Normalization::Smart);
    let mut buf = Vec::new();
    candidates
        .iter()
        .filter_map(|c| {
            let haystack = Utf32Str::new(c, &mut buf);
            pattern.score(haystack, &mut matcher)
        })
        .max()
}

// ── Context ──────────────────────────────────────────────────────────────────

#[derive(Clone, Copy)]
struct ComboboxContext {
    value: Signal<String>,
    on_change: Option<Callback<String>>,
    open: Signal<bool>,
    search_query: Signal<String>,
}

// ── Combobox ────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct ComboboxProps {
    /// The currently selected value (two-way bound).
    pub value: Signal<String>,

    /// Called when the user picks a new value.
    #[props(default)]
    pub on_change: Option<Callback<String>>,

    /// Text shown when no value is selected.
    #[props(default = "Select...".to_string())]
    pub placeholder: String,

    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: combobox root
#[component]
pub fn Combobox(props: ComboboxProps) -> Element {
    let mut open = use_signal(|| false);
    let mut search_query = use_signal(String::new);

    use_context_provider(|| ComboboxContext {
        value: props.value,
        on_change: props.on_change,
        open,
        search_query,
    });

    rsx! {
        div {
            class: crate::cn::merge_slice(&["relative inline-block w-full", props.class.as_str()]),

            // Click-outside overlay when open
            if *open.read() {
                div {
                    class: "fixed inset-0 z-40",
                    onclick: move |_| {
                        open.set(false);
                        search_query.set(String::new());
                    },
                }
            }

            {props.children}
        }
    }
}

// ── ComboboxTrigger ─────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct ComboboxTriggerProps {
    /// Placeholder text when no value selected.
    #[props(default = "Select...".to_string())]
    pub placeholder: String,

    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: combobox trigger button
#[component]
pub fn ComboboxTrigger(props: ComboboxTriggerProps) -> Element {
    let mut ctx: ComboboxContext = use_context();
    let current = ctx.value.read().clone();
    let display = if current.is_empty() {
        props.placeholder.clone()
    } else {
        current
    };
    let is_placeholder = ctx.value.read().is_empty();

    rsx! {
        button {
            r#type: "button",
            class: crate::cn::merge(format!(
                "inline-flex items-center justify-between gap-1.5 h-9 w-full px-3 text-sm rounded-lg border border-input bg-input/30 hover:bg-input/50 transition-colors cursor-pointer select-none {} {}",
                if is_placeholder { "text-muted-foreground" } else { "" },
                props.class
            )),
            onclick: move |_| {
                let was_open = *ctx.open.read();
                ctx.open.set(!was_open);
                if was_open {
                    ctx.search_query.set(String::new());
                }
            },
            span { class: "truncate", "{display}" }
            // Chevron-down icon
            svg {
                class: "size-4 text-muted-foreground shrink-0",
                xmlns: "http://www.w3.org/2000/svg",
                width: "24",
                height: "24",
                view_box: "0 0 24 24",
                fill: "none",
                stroke: "currentColor",
                stroke_width: "2",
                stroke_linecap: "round",
                stroke_linejoin: "round",
                path { d: "m6 9 6 6 6-6" }
            }
        }
    }
}

// ── ComboboxContent ─────────────────────────────────────────────────────────

/// Data record for a single combobox option. Used with the
/// `ComboboxContent { items: ... }` data-driven API to enable
/// fuzzy-score sorting and automatic empty-state handling that the
/// children-based API can't provide (children are opaque to the parent).
#[derive(Clone, PartialEq)]
pub struct ComboboxItemData {
    /// Stable selection value — also fuzzy-matched against the search
    /// query. Use the same string a `value` Signal in `Combobox` will
    /// hold once selected.
    pub value: String,
    /// Visible label. Defaults to `value` if left empty.
    pub label: String,
    /// Extra search terms — synonyms, abbreviations, emoji.
    pub keywords: Vec<String>,
    /// When true, the item renders muted and ignores clicks.
    pub disabled: bool,
}

impl ComboboxItemData {
    pub fn new(value: impl Into<String>, label: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            label: label.into(),
            keywords: Vec::new(),
            disabled: false,
        }
    }

    pub fn keywords<I, S>(mut self, kw: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.keywords = kw.into_iter().map(Into::into).collect();
        self
    }

    pub fn disabled(mut self, disabled: bool) -> Self {
        self.disabled = disabled;
        self
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct ComboboxContentProps {
    /// Placeholder for the search input.
    #[props(default = "Search...".to_string())]
    pub search_placeholder: String,

    /// Data-driven items. When `Some`, items are fuzzy-filtered AND
    /// sorted by score, and `empty` is rendered automatically when no
    /// item matches. Pass `None` to fall back to the children-based
    /// composition (no sort / no auto-empty).
    #[props(default)]
    pub items: Option<Vec<ComboboxItemData>>,

    /// Element shown when `items` is `Some` and no item matches the
    /// query. Ignored in the children-based path.
    #[props(default)]
    pub empty: Option<Element>,

    #[props(default)]
    pub class: String,

    #[props(default)]
    pub children: Element,
}

/// shadcn v4 maia: combobox dropdown panel with search input
#[component]
pub fn ComboboxContent(props: ComboboxContentProps) -> Element {
    let mut ctx: ComboboxContext = use_context();

    if !*ctx.open.read() {
        return rsx! {};
    }

    rsx! {
        div {
            class: crate::cn::merge(format!(
                "absolute z-50 mt-1 w-full max-h-72 overflow-hidden rounded-lg bg-popover text-popover-foreground border border-border shadow-md {}",
                props.class
            )),
            onclick: move |e| e.stop_propagation(),
            // Search input
            div {
                class: "flex items-center border-b border-border px-3",
                svg {
                    class: "size-4 opacity-50 shrink-0",
                    xmlns: "http://www.w3.org/2000/svg",
                    width: "24",
                    height: "24",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    stroke_linecap: "round",
                    stroke_linejoin: "round",
                    path { d: "m21 21-4.3-4.3" }
                    circle { cx: "11", cy: "11", r: "8" }
                }
                input {
                    class: "flex-1 h-9 bg-transparent text-sm placeholder:text-muted-foreground focus:outline-none ml-2",
                    r#type: "text",
                    placeholder: "{props.search_placeholder}",
                    value: "{ctx.search_query.read()}",
                    oninput: move |e| ctx.search_query.set(e.value()),
                    autofocus: true,
                    onmounted: move |elem| async move {
                        let _ = elem.set_focus(true).await;
                    },
                }
            }
            // Items container
            div {
                class: "max-h-56 overflow-y-auto p-1",
                if let Some(items) = props.items.as_ref() {
                    {render_items_data(items, ctx, props.empty.clone())}
                } else {
                    {props.children}
                }
            }
        }
    }
}

/// Render the `items: Vec<ComboboxItemData>` path: fuzzy-filter against
/// the live query, sort survivors by descending score, and emit
/// clickable rows. When nothing matches, render `empty` (or a default
/// "No results." line if the caller didn't provide one).
fn render_items_data(
    items: &[ComboboxItemData],
    mut ctx: ComboboxContext,
    empty: Option<Element>,
) -> Element {
    let query = ctx.search_query.read().clone();

    // Score each item; drop non-matches.
    let mut scored: Vec<(u32, &ComboboxItemData)> = items
        .iter()
        .filter_map(|item| {
            let mut candidates: Vec<&str> = vec![item.value.as_str()];
            candidates.extend(item.keywords.iter().map(|s| s.as_str()));
            // When the label differs from the value, include it too.
            if !item.label.is_empty() && item.label != item.value {
                candidates.push(item.label.as_str());
            }
            fuzzy_best_score(&query, &candidates).map(|s| (s, item))
        })
        .collect();
    // Higher score first. Stable sort preserves author order on ties
    // (matches what users expect for the empty-query case where every
    // item gets score 0).
    scored.sort_by(|a, b| b.0.cmp(&a.0));

    if scored.is_empty() {
        return rsx! {
            if let Some(e) = empty {
                {e}
            } else {
                div {
                    class: "py-6 text-center text-sm text-muted-foreground",
                    "No results."
                }
            }
        };
    }

    rsx! {
        for (_, item) in scored {
            {
                let value = item.value.clone();
                let label = if item.label.is_empty() { item.value.clone() } else { item.label.clone() };
                let is_selected = *ctx.value.read() == value;
                let disabled = item.disabled;
                let click_value = value.clone();
                rsx! {
                    div {
                        key: "{value}",
                        class: crate::cn::merge(format!(
                            "relative flex cursor-pointer select-none items-center rounded-xl px-3 py-2 text-sm hover:bg-accent hover:text-accent-foreground transition-colors gap-2.5 {}",
                            if disabled { "opacity-50 pointer-events-none" } else { "" }
                        )),
                        onclick: move |_| {
                            if disabled { return; }
                            let val = click_value.clone();
                            ctx.value.set(val.clone());
                            ctx.open.set(false);
                            ctx.search_query.set(String::new());
                            if let Some(cb) = &ctx.on_change {
                                cb.call(val);
                            }
                        },
                        span { class: "flex-1", "{label}" }
                        if is_selected {
                            svg {
                                class: "size-4 text-current shrink-0",
                                xmlns: "http://www.w3.org/2000/svg",
                                width: "24", height: "24", view_box: "0 0 24 24",
                                fill: "none", stroke: "currentColor",
                                stroke_width: "2", stroke_linecap: "round", stroke_linejoin: "round",
                                path { d: "M20 6 9 17l-5-5" }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ── ComboboxItem ────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct ComboboxItemProps {
    /// The value this item represents. Matched against the search
    /// query (case-insensitive substring).
    pub value: String,

    /// Extra search terms matched alongside `value`. Mirrors shadcn
    /// cmdk's `keywords` prop — useful when the visible label differs
    /// from the value, or when synonyms/abbreviations should match.
    /// Example: `keywords: vec!["fruit".into(), "🍎".into()]`.
    #[props(default)]
    pub keywords: Vec<String>,

    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: combobox item
#[component]
pub fn ComboboxItem(props: ComboboxItemProps) -> Element {
    let mut ctx: ComboboxContext = use_context();
    let is_selected = *ctx.value.read() == props.value;
    let item_value = props.value.clone();

    // Filter against the search query. shadcn/cmdk fuzzy-matches the
    // query against `value` plus every `keyword`; we delegate to
    // `nucleo-matcher` so the ranking matches fzf-style expectations
    // (subsequence match with prefix / word-boundary / consecutive-
    // character bonuses).
    let query = ctx.search_query.read().clone();
    let mut candidates: Vec<&str> = vec![props.value.as_str()];
    candidates.extend(props.keywords.iter().map(|s| s.as_str()));
    if fuzzy_best_score(&query, &candidates).is_none() {
        return rsx! {};
    }

    rsx! {
        div {
            class: crate::cn::merge(format!(
                "relative flex cursor-pointer select-none items-center rounded-xl px-3 py-2 text-sm hover:bg-accent hover:text-accent-foreground transition-colors gap-2.5 {}",
                props.class
            )),
            onclick: move |_| {
                let val = item_value.clone();
                ctx.value.set(val.clone());
                ctx.open.set(false);
                ctx.search_query.set(String::new());
                if let Some(cb) = &ctx.on_change {
                    cb.call(val);
                }
            },
            span { class: "flex-1", {props.children} }
            // Check icon when selected
            if is_selected {
                svg {
                    class: "size-4 text-current shrink-0",
                    xmlns: "http://www.w3.org/2000/svg",
                    width: "24",
                    height: "24",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    stroke_linecap: "round",
                    stroke_linejoin: "round",
                    path { d: "M20 6 9 17l-5-5" }
                }
            }
        }
    }
}

// ── ComboboxEmpty ───────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct ComboboxEmptyProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: shown when no combobox items match
#[component]
pub fn ComboboxEmpty(props: ComboboxEmptyProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["py-6 text-center text-sm text-muted-foreground", props.class.as_str()]),
            {props.children}
        }
    }
}

/// Combobox using the data-driven `items` API: fuzzy-filtered AND
/// sorted by score, with an automatic empty state when no item matches.
#[story(category = "Combobox", name = "combobox default")]
pub fn combobox_default() -> Element {
    let value = use_signal(|| "apple".to_string());
    let items = vec![
        ComboboxItemData::new("apple", "Apple").keywords(["fruit", "red", "🍎"]),
        ComboboxItemData::new("banana", "Banana").keywords(["fruit", "yellow", "🍌"]),
        ComboboxItemData::new("cherry", "Cherry").keywords(["fruit", "red", "🍒"]),
        ComboboxItemData::new("durian", "Durian").keywords(["fruit", "stinky"]),
    ];
    rsx! {
        div { class: "p-6 bg-background text-foreground max-w-xs",
            Combobox { value,
                ComboboxTrigger { placeholder: "Select fruit...".to_string() }
                ComboboxContent {
                    items,
                    empty: rsx! {
                        div {
                            class: "py-6 text-center text-sm text-muted-foreground",
                            "No fruit matches that search."
                        }
                    }
                }
            }
        }
    }
}
