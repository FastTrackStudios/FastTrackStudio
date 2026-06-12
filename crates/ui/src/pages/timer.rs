//! `/timer` — billable time tracking over the org's `TimerService`.
//!
//! Shows the running session with a live elapsed clock + a Stop
//! button, a Start form when nothing is running, and a list of recent
//! sessions with a today total. Org-scoped: it reads/writes the
//! selected org's timer service.
//!
//! State is the shared optimistic session store ([`crate::stores`]):
//! one slug-tagged list across the selected orgs, with the running
//! session *derived* from it (the open row for the active org +
//! owner) — no separate `active_timer` round-trip and no
//! refresh-counter refetch after start/stop/edit/delete. Mutations
//! patch the store instantly and reconcile against the server
//! (rollback + tray notification on failure).
//!
//! Identity: `org_id` comes from the org's manifest (surfaced via the
//! well-known endpoint). `user_id` is a stable per-org "local owner"
//! id derived from `org_id` — a single-user stand-in until auth wires
//! a real signed-in user through to the page.

use architect::Id;
use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use timer_proto::{StartTimerRequest, WorkSession};
use uuid::Uuid;

use crate::chrome::{fmt_hms, owner_id, resolve_org, use_second_tick};
use crate::orgs::{OrgMeta, OrgSelection};
use crate::stores;

#[component]
pub fn TimerView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    let target = use_memo(move || resolve_org(&selection.read(), &org_list.read()));

    // The shared session store: every selected org, newest first.
    let result = stores::use_session_list();
    let muts = stores::use_timer_mutations();

    // Live elapsed clock — re-render once a second while mounted.
    let tick = use_signal(|| 0u64);
    use_second_tick(tick);
    let _ = tick(); // subscribe so the running card's clock ticks.

    let mut description = use_signal(String::new);

    let rows: Vec<(Id<Uuid>, stores::OrgSession)> = result.value().cloned().unwrap_or_default();
    let load_err = result.error().cloned();
    let first_load = result.is_waiting() && result.value().is_none();

    // The running session, derived from the one list: the open row for
    // the active org + owner (the backend enforces at most one).
    let active: Option<stores::OrgSession> = target().and_then(|(slug, org_id)| {
        let owner = owner_id(org_id);
        rows.iter()
            .map(|(_, r)| r)
            .find(|r| r.slug == slug && r.session.user_id == owner && r.session.end_time.is_none())
            .cloned()
    });

    let start = move |_| {
        let Some((slug, org_id)) = target() else {
            return;
        };
        let desc = description.peek().trim().to_string();
        description.set(String::new());
        muts.start(
            slug,
            StartTimerRequest {
                user_id: owner_id(org_id),
                org_id,
                project_id: None,
                project_path: String::new(),
                task_note_path: String::new(),
                description: desc,
            },
        );
    };

    let active_for_stop = active.clone();
    let stop = move |_| {
        let Some((slug, org_id)) = target() else {
            return;
        };
        let Some(open) = active_for_stop.as_ref() else {
            return;
        };
        muts.stop(slug, owner_id(org_id), open.session.id);
    };

    let body = if target().is_none() {
        rsx! {
            Text { variant: TextVariant::Muted, "Select an org to track time." }
        }
    } else {
        rsx! {
            // Running session, or the start form.
            if let Some(open) = active.as_ref() {
                {running_card(&open.session, stop)}
            } else if first_load {
                Text { variant: TextVariant::Muted, "Loading timer…" }
            } else if let Some(e) = load_err.as_ref() {
                div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
                    "Couldn't reach the timer service: {e}"
                }
            } else {
                {start_form(description, start)}
            }
            // Recent sessions (all selected orgs).
            if !rows.is_empty() {
                {session_list(&rows)}
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

/// Recent sessions, newest first, with a today total. Each row (tagged
/// with its org) edits / deletes in place through the shared store.
fn session_list(rows: &[(Id<Uuid>, stores::OrgSession)]) -> Element {
    let today = Utc::now().date_naive();
    let today_secs: i64 = rows
        .iter()
        .filter(|(_, r)| r.session.start_time.date_naive() == today)
        .filter_map(|(_, r)| {
            r.session
                .end_time
                .map(|e| (e - r.session.start_time).num_seconds())
        })
        .sum();

    rsx! {
        div { class: "flex flex-col gap-2",
            div { class: "flex items-center justify-between",
                Heading { level: HeadingLevel::H3, "Recent" }
                Text { variant: TextVariant::Muted, "Today: {fmt_hms(today_secs)}" }
            }
            div { class: "flex flex-col divide-y divide-border/50 rounded-xl border border-border/60 bg-card/40",
                for (id , row) in rows.iter().take(50) {
                    SessionRow {
                        key: "{id}",
                        pending: id.is_temp(),
                        session: row.session.clone(),
                        slug: row.slug.clone(),
                    }
                }
            }
        }
    }
}

/// One recent-session row — its org badge + an Edit affordance that
/// swaps to an inline editor (description + billable + Save / Cancel /
/// Delete). Editing re-snapshots the rate server-side; the store
/// reconciles against the returned row.
#[component]
fn SessionRow(session: WorkSession, slug: String, pending: bool) -> Element {
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let muts = stores::use_timer_mutations();
    let mut editing = use_signal(|| false);
    // Edit drafts — (re)seeded from the store row when Edit is opened,
    // so a reconciled server value isn't shadowed by a stale draft.
    let mut desc = use_signal(String::new);
    let mut billable = use_signal(|| false);

    let running = session.end_time.is_none();
    let dur = session.end_time.map_or_else(
        || (Utc::now() - session.start_time).num_seconds(),
        |e| (e - session.start_time).num_seconds(),
    );
    let title = if session.description.trim().is_empty() {
        "(no description)".to_string()
    } else {
        session.description.clone()
    };
    let when = session
        .start_time
        .format("%a %b %-d, %-I:%M %p")
        .to_string();
    let id = session.id;
    let stored_desc = session.description.clone();
    let stored_billable = session.billable;

    let org_name = org_list
        .read()
        .iter()
        .find(|o| o.slug == slug)
        .map_or_else(|| slug.clone(), |o| o.name.clone());

    let save = {
        let slug = slug.clone();
        move |_| {
            let req = timer_proto::service::UpdateSessionRequest {
                id,
                description: Some(desc.peek().clone()),
                billable: Some(billable()),
                ..Default::default()
            };
            muts.update(slug.clone(), req);
            editing.set(false);
        }
    };
    let delete = {
        let slug = slug.clone();
        move |_| {
            muts.delete(slug.clone(), id);
        }
    };

    let row_cls = if pending {
        "opacity-60"
    } else {
        ""
    };

    if editing() {
        rsx! {
            div { class: "flex flex-col gap-2 px-3 py-2.5 {row_cls}",
                input {
                    class: "rounded-md border border-border bg-background px-3 py-2 text-sm outline-none focus:ring-2 focus:ring-primary/40",
                    value: "{desc}",
                    oninput: move |e| desc.set(e.value()),
                }
                div { class: "flex items-center gap-2",
                    Button {
                        variant: if billable() { ButtonVariant::Secondary } else { ButtonVariant::Ghost },
                        size: ButtonSize::Small,
                        on_click: move |_| billable.toggle(),
                        if billable() { "Billable ✓" } else { "Non-billable" }
                    }
                    div { class: "flex-1" }
                    Button { variant: ButtonVariant::Ghost, size: ButtonSize::Small, on_click: move |_| editing.set(false), "Cancel" }
                    Button { variant: ButtonVariant::Primary, size: ButtonSize::Small, on_click: save, "Save" }
                    Button { variant: ButtonVariant::Destructive, size: ButtonSize::Small, on_click: delete, "Delete" }
                }
            }
        }
    } else {
        rsx! {
            div { class: "group flex items-center justify-between gap-3 px-3 py-2.5 {row_cls}",
                div { class: "flex min-w-0 flex-col",
                    span { class: "truncate text-sm text-foreground", "{title}" }
                    span { class: "flex items-center gap-1.5 text-xs text-muted-foreground",
                        span { class: "rounded bg-muted px-1.5 py-px text-[10px]", "{org_name}" }
                        span { "{when}" }
                    }
                }
                span {
                    class: if running {
                        "shrink-0 font-mono text-sm tabular-nums text-emerald-400"
                    } else {
                        "shrink-0 font-mono text-sm tabular-nums text-muted-foreground"
                    },
                    "{fmt_hms(dur)}"
                }
                button {
                    class: "shrink-0 rounded px-1.5 py-0.5 text-xs text-muted-foreground opacity-0 transition-opacity hover:text-foreground group-hover:opacity-100",
                    onclick: move |_| {
                        desc.set(stored_desc.clone());
                        billable.set(stored_billable);
                        editing.set(true);
                    },
                    "Edit"
                }
            }
        }
    }
}

// Shared chrome helpers (`resolve_org`, `owner_id`, `fmt_hms`,
// `use_second_tick`) live in `crate::chrome` so the top-bar timer
// widget and this page agree on the same org / owner identity.
