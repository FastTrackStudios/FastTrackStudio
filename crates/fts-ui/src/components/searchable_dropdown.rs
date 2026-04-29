//! Searchable dropdown — floating popover with backdrop, search input, and
//! scrollable children.
//!
//! Positioned with `position: fixed` at explicit `(x, y)` coordinates so it
//! escapes CSS `transform` stacking contexts (important for Wry/WebKit).

use std::sync::atomic::{AtomicU64, Ordering};

use dioxus::prelude::*;

static DROPDOWN_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

// ---------------------------------------------------------------------------
// SearchableDropdown
// ---------------------------------------------------------------------------

/// Floating searchable dropdown panel with a click-away backdrop.
#[derive(Props, Clone, PartialEq)]
pub struct SearchableDropdownProps {
    /// Whether the dropdown is visible.
    pub open: bool,

    /// Called when the backdrop is clicked or Escape is pressed.
    pub on_close: Callback<()>,

    /// Two-way bound search text (consumer owns this for filtering).
    pub value: Signal<String>,

    /// Placeholder text for the search input.
    #[props(default = "Search...".to_string())]
    pub placeholder: String,

    /// Text shown when there are no results.
    #[props(default = "No results".to_string())]
    pub empty_label: String,

    /// Whether the children contain any results. Controls empty-state display.
    #[props(default = true)]
    pub has_results: bool,

    /// Fixed-position X coordinate (pixels).
    #[props(default = 0.0)]
    pub x: f64,

    /// Fixed-position Y coordinate (pixels).
    #[props(default = 0.0)]
    pub y: f64,

    /// Width class (e.g. `"w-60"`).
    #[props(default = "w-60".to_string())]
    pub width: String,

    /// Max-height class (e.g. `"max-h-80"`).
    #[props(default = "max-h-80".to_string())]
    pub max_height: String,

    /// Optional header slot rendered above the search input (for tabs/pills).
    #[props(default)]
    pub header: Option<Element>,

    /// Extra CSS classes on the panel.
    #[props(default)]
    pub class: String,

    /// The filtered results to display.
    pub children: Element,
}

#[component]
pub fn SearchableDropdown(props: SearchableDropdownProps) -> Element {
    if !props.open {
        return rsx! {};
    }

    let mut value = props.value;
    let input_id = use_signal(|| {
        let id = DROPDOWN_ID_COUNTER.fetch_add(1, Ordering::Relaxed);
        format!("searchable-dropdown-{id}")
    });
    let iid = input_id();

    let panel_style = format!(
        "position: fixed; left: {}px; top: {}px; z-index: 9999;",
        props.x, props.y
    );

    // Auto-focus the search input on mount.
    let focus_js = format!(
        r#"(function(){{ var el = document.getElementById('{iid}'); if(el) el.focus(); }})()"#
    );

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 bg-black/80",
            style: "z-index: 9998;",
            onmousedown: move |evt| {
                evt.stop_propagation();
                props.on_close.call(());
            },
        }

        // Panel
        div {
            class: format!(
                "{} {} bg-background border border-border rounded-xl shadow-md shadow-black/50 \
                 flex flex-col overflow-hidden {}",
                props.width,
                props.max_height,
                props.class
            ),
            style: "{panel_style}",
            onclick: move |evt| evt.stop_propagation(),
            onkeydown: move |evt| {
                if evt.key() == Key::Escape {
                    props.on_close.call(());
                }
                evt.stop_propagation();
            },

            // Optional header (tabs, pills, etc.)
            if let Some(header) = &props.header {
                {header}
            }

            // Search input
            div { class: "px-3 py-1.5 border-b border-border",
                input {
                    id: "{iid}",
                    class: "w-full bg-input/30 border border-border rounded-md px-2.5 py-1.5 \
                            text-[11px] text-foreground outline-none focus:border-ring \
                            placeholder:text-muted-foreground transition-all",
                    r#type: "text",
                    placeholder: "{props.placeholder}",
                    value: "{value}",
                    oninput: move |evt| value.set(evt.value().clone()),
                }
            }
            script { "{focus_js}" }

            // Scrollable results
            div { class: "flex-1 overflow-y-auto min-h-0 px-1.5 py-1.5",
                if !props.has_results {
                    div { class: "flex items-center justify-center py-4",
                        p { class: "text-[10px] text-muted-foreground", "{props.empty_label}" }
                    }
                } else {
                    {props.children}
                }
            }
        }
    }
}
