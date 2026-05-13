//! `AgentRunBoard` — Kanban over `AgentRun`s across the org.
//!
//! Columns are status-bucketed (Queued | Running | Blocked |
//! Succeeded | Failed) with per-column count chips. Cards are
//! display-only — there's no quick-create here since agents aren't
//! authored from this surface.

use std::collections::HashMap;

use agent_proto::AgentRun;
use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Bot, X as XIcon};
use fts_ui::prelude::*;
use uuid::Uuid;

use super::common::{format_cost_cents, format_relative_time, run_status_variant};

const COLUMNS: &[(&str, &str)] = &[
    ("queued", "Queued"),
    ("running", "Running"),
    ("blocked", "Blocked"),
    ("succeeded", "Succeeded"),
    ("failed", "Failed"),
];

fn bucket(status: &str) -> Option<&'static str> {
    match status {
        "queued" => Some("queued"),
        "running" => Some("running"),
        "blocked" => Some("blocked"),
        "succeeded" | "completed" | "success" => Some("succeeded"),
        "failed" | "error" => Some("failed"),
        _ => None, // cancelled / archived / unknown — out of board view
    }
}

#[component]
pub fn AgentRunBoard(
    runs: Vec<AgentRun>,
    task_titles_by_id: HashMap<Uuid, String>,
    on_open: EventHandler<Uuid>,
    on_cancel: EventHandler<Uuid>,
) -> Element {
    // Bucket runs.
    let mut by_col: HashMap<&'static str, Vec<AgentRun>> = HashMap::new();
    for r in &runs {
        if let Some(b) = bucket(&r.status) {
            by_col.entry(b).or_default().push(r.clone());
        }
    }
    for items in by_col.values_mut() {
        items.sort_by_key(|r| std::cmp::Reverse(r.started_at.unwrap_or(r.created_at)));
    }

    let now = Utc::now();

    rsx! {
        ScrollArea { class: "w-full",
            div { class: "flex items-start gap-3 px-1 py-2 min-h-[24rem]",
                for (key, label) in COLUMNS.iter() {
                    {
                        let items = by_col.remove(key).unwrap_or_default();
                        let count = items.len();
                        let dot_color = match run_status_variant(key) {
                            StatusBadgeVariant::Success => StatusDotColor::Success,
                            StatusBadgeVariant::Warning => StatusDotColor::Warning,
                            StatusBadgeVariant::Danger => StatusDotColor::Danger,
                            StatusBadgeVariant::Neutral => StatusDotColor::Neutral,
                        };
                        rsx! {
                            div { key: "col-{key}",
                                class: "flex shrink-0 flex-col gap-2 w-72 rounded-xl bg-muted/30 p-2",
                                HStack { class: "items-center justify-between px-1",
                                    HStack { class: "items-center gap-2",
                                        StatusDot { color: dot_color, size: StatusDotSize::Small }
                                        span { class: "text-xs font-semibold uppercase tracking-widest text-foreground", "{label}" }
                                        Badge { variant: BadgeVariant::Secondary, "{count}" }
                                    }
                                }
                                div { class: "flex flex-col gap-2",
                                    for run in items.into_iter() {
                                        {
                                            let run_id = run.id;
                                            let name = run.name.clone();
                                            let kind = run.kind.clone();
                                            let integration = run.integration.clone().unwrap_or_else(|| "local".into());
                                            let cost = run.cost_cents.map(format_cost_cents).unwrap_or_default();
                                            let started = run.started_at.unwrap_or(run.created_at);
                                            let started_label = format_relative_time(started, now);
                                            let task_title = run
                                                .task_id
                                                .and_then(|t| task_titles_by_id.get(&t).cloned());
                                            let can_cancel = matches!(run.status.as_str(), "queued" | "running" | "blocked");
                                            rsx! {
                                                div { key: "{run_id}",
                                                    class: "group rounded-lg border border-border bg-card p-2 text-sm shadow-sm hover:border-primary/40 hover:shadow-md transition-all cursor-pointer flex flex-col gap-1.5",
                                                    onclick: move |_| on_open.call(run_id),
                                                    HStack { class: "items-center gap-2",
                                                        Bot { size: 12 }
                                                        span { class: "font-medium truncate text-foreground", "{name}" }
                                                    }
                                                    HStack { class: "items-center gap-2 flex-wrap",
                                                        Badge { variant: BadgeVariant::Outline, "{kind}" }
                                                        Badge { variant: BadgeVariant::Secondary, "{integration}" }
                                                    }
                                                    if let Some(tt) = task_title {
                                                        span { class: "text-[10px] text-muted-foreground truncate",
                                                            "from Task: {tt}"
                                                        }
                                                    }
                                                    HStack { class: "items-center justify-between",
                                                        span { class: "text-[10px] text-muted-foreground", "{started_label}" }
                                                        HStack { class: "items-center gap-2",
                                                            if !cost.is_empty() {
                                                                span { class: "text-[10px] text-muted-foreground", "{cost}" }
                                                            }
                                                            if can_cancel {
                                                                button {
                                                                    r#type: "button",
                                                                    class: "rounded p-1 text-muted-foreground hover:text-rose-500",
                                                                    onclick: move |e: MouseEvent| {
                                                                        e.stop_propagation();
                                                                        on_cancel.call(run_id);
                                                                    },
                                                                    XIcon { size: 10 }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    if count == 0 {
                                        div { class: "rounded-lg border border-dashed border-border/60 px-3 py-4 text-center text-[11px] text-muted-foreground",
                                            "Empty"
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
