//! Shared loading / error / empty phase components for data pages.
//!
//! Before this, each page hand-rolled its own loading/error/empty
//! markup — and several rendered *nothing* on a failed fetch (the
//! ledger swallowed errors and showed a blank book; tasks had a
//! bespoke one-off banner), with no way to retry short of a full
//! reload. These give one consistent look across pages plus an
//! optional retry affordance. (codeberg #27, item 1.)

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Inbox, RotateCw, TriangleAlert};
use fts_ui::prelude::*;

/// Skeleton placeholder shown while a page's primary data loads.
/// `rows` controls how many shimmer cards render.
#[component]
pub fn LoadingState(#[props(default = 4)] rows: usize) -> Element {
    rsx! {
        div { class: "flex flex-col gap-3",
            for _ in 0..rows {
                div { class: "flex flex-col gap-2 rounded-xl border border-border/70 bg-card p-4",
                    Skeleton { class: "h-5 w-40" }
                    Skeleton { class: "h-4 w-full" }
                }
            }
        }
    }
}

/// Error card that keeps the failure visible instead of leaving a
/// blank page, with an optional **Try again** button. Pass `on_retry`
/// when the caller can re-run the fetch (e.g. a Dioxus `Resource`'s
/// `restart()`); omit it for read models that have no cheap refetch.
#[component]
pub fn ErrorState(
    /// The error detail to show under the title.
    message: String,
    /// Headline above the message. Defaults to a generic line.
    #[props(default = "Something went wrong".to_string())]
    title: String,
    /// Optional retry handler — renders the **Try again** button.
    #[props(default)]
    on_retry: Option<EventHandler<()>>,
) -> Element {
    rsx! {
        div { class: "flex flex-col items-center gap-3 rounded-2xl border border-destructive/40 bg-destructive/10 px-6 py-10 text-center",
            div { class: "flex size-11 items-center justify-center rounded-xl bg-destructive/15 text-destructive",
                TriangleAlert { size: 22 }
            }
            div { class: "flex flex-col gap-1",
                Heading { level: HeadingLevel::H3, "{title}" }
                Text { variant: TextVariant::Muted, class: "max-w-md break-words", "{message}" }
            }
            if let Some(retry) = on_retry {
                Button {
                    variant: ButtonVariant::Secondary,
                    size: ButtonSize::Small,
                    on_click: move |_| retry.call(()),
                    RotateCw { size: 14 }
                    "Try again"
                }
            }
        }
    }
}

/// Empty-state placeholder for a page with no rows yet: an icon, a
/// title, and an optional hint line.
#[component]
pub fn EmptyState(
    /// Headline (e.g. "No transactions yet").
    title: String,
    /// Optional secondary line explaining how to populate the view.
    #[props(default)]
    hint: Option<String>,
) -> Element {
    rsx! {
        div { class: "flex flex-col items-center gap-3 rounded-2xl border border-dashed border-border/70 bg-card/40 px-6 py-16 text-center",
            div { class: "flex size-12 items-center justify-center rounded-2xl bg-muted text-muted-foreground",
                Inbox { size: 24 }
            }
            Heading { level: HeadingLevel::H3, "{title}" }
            if let Some(h) = hint {
                Text { variant: TextVariant::Muted, "{h}" }
            }
        }
    }
}
