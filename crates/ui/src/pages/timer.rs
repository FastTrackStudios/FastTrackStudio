//! `/timer` — billable time tracking over the org's `TimerService`.
//!
//! Shows the running session with a live elapsed clock + a Stop
//! button, a Start form when nothing is running, and a list of recent
//! sessions with a today total. Org-scoped: it reads/writes the
//! selected org's timer service.
//!
//! Identity: `org_id` comes from the org's manifest (surfaced via the
//! well-known endpoint). `user_id` is a stable per-org "local owner"
//! id derived from `org_id` — a single-user stand-in until auth wires
//! a real signed-in user through to the page.

use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use timer_proto::{StartTimerRequest, WorkSession};

use crate::chrome::{fmt_hms, owner_id, resolve_org, use_second_tick};
use crate::orgs::{OrgMeta, OrgSelection};

#[component]
pub fn TimerView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    let target = use_memo(move || resolve_org(&selection.read(), &org_list.read()));

    // Bumped after start/stop so the resources refetch.
    let mut reload = use_signal(|| 0u32);

    let active = use_resource(move || async move {
        let _ = reload();
        match target() {
            Some((slug, org_id)) => crate::feeds::fetch_active_timer(&slug, owner_id(org_id)).await,
            None => Ok(None),
        }
    });
    let sessions = use_resource(move || async move {
        let _ = reload();
        match target() {
            Some((slug, org_id)) => {
                crate::feeds::fetch_recent_sessions(&slug, owner_id(org_id)).await
            }
            None => Ok(Vec::new()),
        }
    });

    // Live elapsed clock — re-render once a second while mounted.
    let tick = use_signal(|| 0u64);
    use_second_tick(tick);
    let _ = tick(); // subscribe so the running card's clock ticks.

    let mut description = use_signal(String::new);

    let start = move |_| {
        let Some((slug, org_id)) = target() else {
            return;
        };
        let desc = description.peek().trim().to_string();
        spawn(async move {
            let req = StartTimerRequest {
                user_id: owner_id(org_id),
                org_id,
                project_id: None,
                project_path: String::new(),
                task_note_path: String::new(),
                description: desc,
            };
            if crate::feeds::start_timer(&slug, req).await.is_ok() {
                description.set(String::new());
                reload += 1;
            }
        });
    };

    let stop = move |_| {
        let Some((slug, org_id)) = target() else {
            return;
        };
        spawn(async move {
            if crate::feeds::stop_timer(&slug, owner_id(org_id))
                .await
                .is_ok()
            {
                reload += 1;
            }
        });
    };

    let body = if target().is_none() {
        rsx! {
            Text { variant: TextVariant::Muted, "Select an org to track time." }
        }
    } else {
        rsx! {
            // Running session, or the start form.
            match &*active.read_unchecked() {
                Some(Ok(Some(s))) => running_card(s, stop),
                Some(Ok(None)) => start_form(description, start),
                Some(Err(e)) => rsx! {
                    div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
                        "Couldn't reach the timer service: {e}"
                    }
                },
                None => rsx! { Text { variant: TextVariant::Muted, "Loading timer…" } },
            }
            // Recent sessions.
            match &*sessions.read_unchecked() {
                Some(Ok(rows)) if !rows.is_empty() => session_list(rows),
                _ => rsx! {},
            }
        }
    };

    rsx! {
        div { class: "mx-auto flex w-full max-w-2xl flex-col gap-5 p-4 sm:p-6 lg:p-8",
            header { class: "flex flex-col gap-1",
                span { class: "text-[0.7rem] font-semibold uppercase tracking-[0.18em] text-muted-foreground",
                    "Time tracking"
                }
                Heading { level: HeadingLevel::H1, class: "tracking-tight", "Timer" }
            }
            {body}
        }
    }
}

/// The running-session card: description, live elapsed clock, Stop.
fn running_card(s: &WorkSession, on_stop: impl FnMut(MouseEvent) + 'static) -> Element {
    let elapsed = (Utc::now() - s.start_time).num_seconds();
    let title = if s.description.trim().is_empty() {
        "(no description)".to_string()
    } else {
        s.description.clone()
    };
    rsx! {
        div { class: "flex flex-col gap-3 rounded-2xl border border-emerald-500/40 bg-emerald-500/5 p-5",
            div { class: "flex items-center gap-2",
                span { class: "relative flex size-2.5",
                    span { class: "absolute inline-flex size-full animate-ping rounded-full bg-emerald-400/70" }
                    span { class: "relative inline-flex size-2.5 rounded-full bg-emerald-400" }
                }
                Text { variant: TextVariant::Muted, "Tracking" }
            }
            div { class: "font-mono text-4xl font-semibold tabular-nums tracking-tight", "{fmt_hms(elapsed)}" }
            div { class: "text-sm text-foreground", "{title}" }
            div { class: "flex",
                Button {
                    variant: ButtonVariant::Destructive,
                    on_click: on_stop,
                    "Stop"
                }
            }
        }
    }
}

/// The start form: a description input + Start button.
fn start_form(
    mut description: Signal<String>,
    on_start: impl FnMut(MouseEvent) + 'static,
) -> Element {
    rsx! {
        div { class: "flex flex-col gap-3 rounded-2xl border border-border/70 bg-card/60 p-5",
            Text { variant: TextVariant::Muted, "Nothing tracking right now." }
            div { class: "flex flex-col gap-2 sm:flex-row",
                input {
                    class: "flex-1 rounded-md border border-border bg-background px-3 py-2 text-sm outline-none focus:ring-2 focus:ring-primary/40",
                    r#type: "text",
                    placeholder: "What are you working on?",
                    value: "{description}",
                    oninput: move |e| description.set(e.value()),
                }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: on_start,
                    "Start"
                }
            }
        }
    }
}

/// Recent sessions, newest first, with a today total.
fn session_list(rows: &[WorkSession]) -> Element {
    let today = Utc::now().date_naive();
    let today_secs: i64 = rows
        .iter()
        .filter(|s| s.start_time.date_naive() == today)
        .filter_map(|s| s.end_time.map(|e| (e - s.start_time).num_seconds()))
        .sum();

    rsx! {
        div { class: "flex flex-col gap-2",
            div { class: "flex items-center justify-between",
                Heading { level: HeadingLevel::H3, "Recent" }
                Text { variant: TextVariant::Muted, "Today: {fmt_hms(today_secs)}" }
            }
            div { class: "flex flex-col divide-y divide-border/50 rounded-xl border border-border/60 bg-card/40",
                for s in rows.iter().take(20) {
                    {
                        let running = s.end_time.is_none();
                        let dur = s.end_time.map_or_else(
                            || (Utc::now() - s.start_time).num_seconds(),
                            |e| (e - s.start_time).num_seconds(),
                        );
                        let title = if s.description.trim().is_empty() {
                            "(no description)".to_string()
                        } else {
                            s.description.clone()
                        };
                        let when = s.start_time.format("%a %b %-d, %-I:%M %p").to_string();
                        rsx! {
                            div { key: "{s.id}", class: "flex items-center justify-between gap-3 px-3 py-2.5",
                                div { class: "flex min-w-0 flex-col",
                                    span { class: "truncate text-sm text-foreground", "{title}" }
                                    span { class: "text-xs text-muted-foreground", "{when}" }
                                }
                                span {
                                    class: if running {
                                        "shrink-0 font-mono text-sm tabular-nums text-emerald-400"
                                    } else {
                                        "shrink-0 font-mono text-sm tabular-nums text-muted-foreground"
                                    },
                                    "{fmt_hms(dur)}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// Shared chrome helpers (`resolve_org`, `owner_id`, `fmt_hms`,
// `use_second_tick`) live in `crate::chrome` so the top-bar timer
// widget and this page agree on the same org / owner identity.
