//! `TimerIdleDialog` — Solidtime-style "you've been idle for N minutes"
//! prompt. Three choices: keep the idle time, discard it, or split the
//! entry so the away-from-keyboard window is excluded but a new entry
//! resumes when the user returns.
//!
//! Web-only caveat: we can only detect *tab-level* idleness (via
//! `visibilitychange`), not OS-level idleness. The dialog component
//! itself is platform-neutral.

use chrono::{DateTime, Utc};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use timer_proto::TimeEntry;

/// Detected idle window during a running timer.
#[derive(Clone, Debug, PartialEq)]
pub struct IdleWindow {
    /// When the idle period began (e.g. when the tab was hidden).
    pub started_at: DateTime<Utc>,
    /// When the user came back (visibility flipped to "visible").
    pub ended_at: DateTime<Utc>,
}

impl IdleWindow {
    pub fn duration_seconds(&self) -> i64 {
        (self.ended_at - self.started_at).num_seconds().max(0)
    }
}

#[component]
pub fn TimerIdleDialog(
    open: bool,
    running: Option<TimeEntry>,
    idle: Option<IdleWindow>,
    on_keep: EventHandler<()>,
    on_discard: EventHandler<()>,
    on_split: EventHandler<()>,
    on_close: EventHandler<()>,
) -> Element {
    if !open {
        return rsx! {};
    }

    let idle_for_body = idle.clone();
    let running_desc = running
        .as_ref()
        .and_then(|e| e.description.clone())
        .unwrap_or_else(|| "(no description)".into());

    let body_text = if let Some(w) = idle_for_body.as_ref() {
        let secs = w.duration_seconds();
        let mins = secs / 60;
        let dur = if mins >= 60 {
            format!("{}h {}m", mins / 60, mins % 60)
        } else {
            format!("{}m", mins.max(1))
        };
        format!(
            "While you were away for {dur} ({} → {}), your timer kept running on '{}'. What do you want to do with that time?",
            w.started_at.format("%H:%M"),
            w.ended_at.format("%H:%M"),
            running_desc,
        )
    } else {
        "Your timer was running while you were idle.".to_string()
    };

    let on_overlay = on_close;
    rsx! {
        Dialog {
            open: true,
            is_modal: true,
            on_open_change: move |o: bool| {
                if !o {
                    on_overlay.call(());
                }
            },

            DialogHeader {
                DialogTitle { "You've been idle" }
                DialogDescription { "{body_text}" }
            }

            div {
                class: "flex flex-col gap-2",
                onkeydown: move |e: KeyboardEvent| {
                    if e.key() == Key::Escape {
                        on_close.call(());
                    }
                },

                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| {
                        on_keep.call(());
                        on_close.call(());
                    },
                    "Keep idle time"
                }
                Button {
                    variant: ButtonVariant::Outline,
                    on_click: move |_| {
                        on_discard.call(());
                        on_close.call(());
                    },
                    "Discard idle time"
                }
                Button {
                    variant: ButtonVariant::Outline,
                    on_click: move |_| {
                        on_split.call(());
                        on_close.call(());
                    },
                    "Split entry"
                }
            }
        }
    }
}
