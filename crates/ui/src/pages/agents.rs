//! `/agents` — agent sessions across the selected orgs.
//!
//! Lists the conversations hosted by each org's agent backend (the
//! in-process Codex backend on `task-server`), fetched over the
//! architect-generated `SessionsClient` via
//! [`crate::feeds::fetch_agent_sessions`]. Each row shows the session
//! title, backend, status badge, token usage, and last-activity time;
//! in multi-org "All" mode the owning org slug is shown too.
//!
//! Read-only at this slice — dispatch (`TurnDispatchClient`) is mounted
//! server-side but the composer UI is a follow-up. Everything is
//! theme-token only so it tracks light/dark (dark default).

use agent_proto::session::{Session, SessionStatus};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Bot;
use fts_ui::prelude::*;

#[component]
pub fn AgentsView() -> Element {
    let selection = use_context::<Signal<crate::orgs::OrgSelection>>();
    let org_list = use_context::<Signal<Vec<crate::orgs::OrgMeta>>>();

    let sessions = use_resource(move || async move {
        let slugs = crate::orgs::selected_slugs(&selection.read(), &org_list.read());
        if slugs.is_empty() {
            return Ok(Vec::new());
        }
        crate::feeds::fetch_agent_sessions(&slugs).await
    });

    // Show the org column only when more than one org is in view.
    let multi_org = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read()).len() > 1
    });

    let body = match &*sessions.read_unchecked() {
        Some(Ok(rows)) if rows.is_empty() => rsx! {
            empty_state {}
        },
        Some(Ok(rows)) => {
            let mut rows = rows.clone();
            // Newest activity first; fall back to creation time.
            rows.sort_by(|(_, a), (_, b)| {
                let ka = a.last_message_at.unwrap_or(a.created_at);
                let kb = b.last_message_at.unwrap_or(b.created_at);
                kb.cmp(&ka)
            });
            rsx! {
                session_list { rows, multi_org: multi_org() }
            }
        }
        Some(Err(e)) => rsx! {
            div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
                "Couldn't reach the agent service: {e}"
            }
        },
        None => rsx! {
            Text { variant: TextVariant::Muted, "Loading agent sessions…" }
        },
    };

    rsx! {
        div { class: "mx-auto flex w-full max-w-4xl flex-col gap-5 p-4 sm:p-6 lg:p-8",
            header { class: "flex flex-col gap-1",
                span { class: "text-[0.7rem] font-semibold uppercase tracking-[0.18em] text-muted-foreground",
                    "Agents"
                }
                div { class: "flex items-center gap-2",
                    Bot { size: 22 }
                    Heading { level: HeadingLevel::H1, class: "tracking-tight", "Sessions" }
                }
            }
            {body}
        }
    }
}

/// The list of sessions, newest activity first.
#[component]
fn session_list(rows: Vec<(String, Session)>, multi_org: bool) -> Element {
    rsx! {
        div { class: "flex flex-col divide-y divide-border/50 rounded-xl border border-border/60 bg-card/40",
            for (slug , s) in rows.iter() {
                {
                    let title = if s.title.trim().is_empty() {
                        "(untitled)".to_string()
                    } else {
                        s.title.clone()
                    };
                    let when = s
                        .last_message_at
                        .unwrap_or(s.created_at)
                        .format("%a %b %-d, %-I:%M %p")
                        .to_string();
                    let tokens = s.usage.input_tokens + s.usage.output_tokens;
                    rsx! {
                        div {
                            key: "{slug}/{s.id}",
                            class: "flex items-center justify-between gap-3 px-3 py-2.5",
                            div { class: "flex min-w-0 flex-col gap-0.5",
                                div { class: "flex items-center gap-2",
                                    span { class: "truncate text-sm font-medium text-foreground", "{title}" }
                                    if s.pinned {
                                        span { class: "text-xs text-muted-foreground", "📌" }
                                    }
                                }
                                div { class: "flex flex-wrap items-center gap-x-2 gap-y-0.5 text-xs text-muted-foreground",
                                    span { "{s.backend_id}" }
                                    span { "·" }
                                    span { "{when}" }
                                    if tokens > 0 {
                                        span { "·" }
                                        span { "{tokens} tok" }
                                    }
                                    if multi_org {
                                        span { "·" }
                                        span { class: "font-mono", "{slug}" }
                                    }
                                }
                            }
                            StatusBadge {
                                variant: status_variant(s.status),
                                label: status_label(s.status).to_string(),
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Shown when no org hosts any agent session yet.
#[component]
fn empty_state() -> Element {
    rsx! {
        div { class: "flex flex-col items-center gap-2 rounded-xl border border-dashed border-border/60 bg-card/30 px-6 py-12 text-center",
            Bot { size: 28 }
            Text { variant: TextVariant::Muted, "No agent sessions yet." }
            Text {
                variant: TextVariant::Muted,
                "Dispatch one from the CLI (`task agent …`) to see it here.",
            }
        }
    }
}

fn status_label(status: SessionStatus) -> &'static str {
    match status {
        SessionStatus::Idle => "Idle",
        SessionStatus::Running => "Running",
        SessionStatus::AwaitingUser => "Awaiting user",
        SessionStatus::Cancelled => "Cancelled",
        SessionStatus::Errored => "Errored",
    }
}

fn status_variant(status: SessionStatus) -> StatusBadgeVariant {
    match status {
        SessionStatus::Running => StatusBadgeVariant::Success,
        SessionStatus::AwaitingUser => StatusBadgeVariant::Warning,
        SessionStatus::Errored => StatusBadgeVariant::Danger,
        SessionStatus::Idle | SessionStatus::Cancelled => StatusBadgeVariant::Neutral,
    }
}
