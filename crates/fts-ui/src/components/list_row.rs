//! List row — horizontal card with leading indicator, text stack, and trailing action.
//!
//! Replaces the repeated `flex items-center justify-between p-3 rounded-lg border`
//! pattern found throughout the dashboard and manage views.

use dioxus::prelude::*;

// ---------------------------------------------------------------------------
// ListRow
// ---------------------------------------------------------------------------

/// A horizontal card row with optional leading element, text stack, and trailing action.
///
/// ```rust,ignore
/// ListRow {
///     label: "Signal REAPER",
///     detail: "Running — PID 1234",
///     leading: rsx! { StatusDot { color: StatusDotColor::Success } },
///     trailing: rsx! { Button { variant: ButtonVariant::Secondary, "Launch" } },
/// }
/// ```
#[derive(Props, Clone, PartialEq)]
pub struct ListRowProps {
    /// Primary label text.
    pub label: String,

    /// Optional secondary detail text below the label.
    #[props(default)]
    pub detail: String,

    /// Optional small tag rendered next to the label (e.g. "AUTO").
    #[props(default)]
    pub tag: String,

    /// Optional leading element (status dot, icon, avatar).
    #[props(default)]
    pub leading: Option<Element>,

    /// Optional trailing element (action button, chevron).
    #[props(default)]
    pub trailing: Option<Element>,

    /// Click handler for the entire row.
    #[props(default)]
    pub on_click: Option<Callback<()>>,

    /// Extra CSS classes on the outer container.
    #[props(default)]
    pub class: String,
}

#[component]
pub fn ListRow(props: ListRowProps) -> Element {
    let base_class = format!(
        "flex items-center justify-between p-3 rounded-lg border border-border bg-card/50 {}",
        props.class
    );

    let clickable_class = if props.on_click.is_some() {
        "cursor-pointer hover:bg-accent/30 transition-colors"
    } else {
        ""
    };

    rsx! {
        div {
            class: "{base_class} {clickable_class}",
            onclick: move |_| {
                if let Some(cb) = &props.on_click {
                    cb.call(());
                }
            },

            // Left section: leading + text
            div { class: "flex items-center gap-3 min-w-0",
                if let Some(leading) = &props.leading {
                    {leading}
                }

                div { class: "min-w-0",
                    div { class: "flex items-center gap-2",
                        p { class: "text-sm font-medium text-foreground truncate",
                            "{props.label}"
                        }
                        if !props.tag.is_empty() {
                            span {
                                class: "px-1.5 py-0.5 text-[10px] font-medium rounded bg-secondary text-muted-foreground flex-shrink-0",
                                "{props.tag}"
                            }
                        }
                    }
                    if !props.detail.is_empty() {
                        p { class: "text-xs text-muted-foreground truncate",
                            "{props.detail}"
                        }
                    }
                }
            }

            // Right section: trailing
            if let Some(trailing) = &props.trailing {
                div { class: "flex-shrink-0 ml-3",
                    {trailing}
                }
            }
        }
    }
}
