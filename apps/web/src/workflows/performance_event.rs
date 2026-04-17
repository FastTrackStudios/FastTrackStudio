//! Performance Event workflow — concerts, festivals, sessions, rehearsals.
//!
//! Panels: personnel, schedule

use dioxus::prelude::*;
use super::{PanelDef, WorkflowContext, WorkflowExt};
use crate::UserAvatar;

pub struct PerformanceEventExt;

impl WorkflowExt for PerformanceEventExt {
    fn label(&self) -> &'static str {
        "Performance Event"
    }

    fn stats_panel(&self, ctx: &WorkflowContext) -> Element {
        let booking = ctx.tasks.iter().filter(|t| has_tag(t, &["booking", "logistics"])).count();
        let performance = ctx.tasks.iter().filter(|t| has_tag(t, &["performance", "setlist"])).count();
        let legal = ctx.tasks.iter().filter(|t| has_tag(t, &["legal", "contracts"])).count();
        let marketing = ctx.tasks.iter().filter(|t| has_tag(t, &["marketing", "social"])).count();
        let merch = ctx.tasks.iter().filter(|t| has_tag(t, &["merch"])).count();
        let team = ctx.project.team.len();

        rsx! {
            div { class: "grid grid-cols-3 sm:grid-cols-6 gap-3",
                StatCard { label: "Booking", value: booking }
                StatCard { label: "Performance", value: performance }
                StatCard { label: "Contracts", value: legal }
                StatCard { label: "Marketing", value: marketing }
                StatCard { label: "Merch", value: merch }
                StatCard { label: "Team", value: team }
            }
        }
    }

    fn panels(&self) -> Vec<PanelDef> {
        vec![
            PanelDef {
                id: "personnel",
                label: "Team & Personnel",
                collapsed: false,
                render: render_personnel,
            },
            PanelDef {
                id: "schedule",
                label: "Schedule",
                collapsed: false,
                render: render_schedule,
            },
        ]
    }
}

fn has_tag(task: &crate::ApiTask, tags: &[&str]) -> bool {
    task.tags.iter().any(|t| tags.contains(&t.as_str()))
}

fn render_personnel(ctx: &WorkflowContext) -> Element {
    let team = &ctx.project.team;
    if team.is_empty() {
        return rsx! {};
    }

    rsx! {
        div { class: "divide-y divide-border",
            for member in team.iter() {
                {
                    let member_tasks: Vec<_> = ctx.tasks.iter()
                        .filter(|t| t.assignee.as_deref() == Some(member.as_str()))
                        .filter(|t| t.status != "Done")
                        .collect();
                    rsx! {
                        div { class: "flex items-center gap-3 px-4 py-2.5",
                            UserAvatar { name: member.clone(), size: "size-7".to_string() }
                            div { class: "flex-1 min-w-0",
                                span { class: "text-sm font-medium", "{member}" }
                                if !member_tasks.is_empty() {
                                    span { class: "text-xs text-muted-foreground ml-2",
                                        { format!("{} active", member_tasks.len()) }
                                    }
                                }
                            }
                            if !member_tasks.is_empty() {
                                div { class: "text-xs text-muted-foreground truncate max-w-48",
                                    { member_tasks.first().map(|t| t.title.as_str()).unwrap_or("") }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn render_schedule(ctx: &WorkflowContext) -> Element {
    let mut scheduled: Vec<_> = ctx.tasks.iter()
        .filter(|t| t.due.is_some() && t.status != "Done")
        .collect();
    scheduled.sort_by_key(|t| t.due.clone());

    if scheduled.is_empty() {
        return rsx! {
            div { class: "px-4 py-3 text-xs text-muted-foreground italic", "No scheduled items" }
        };
    }

    rsx! {
        div { class: "divide-y divide-border",
            for task in scheduled.iter() {
                div { class: "flex items-center gap-3 px-4 py-2",
                    span { class: "text-xs text-muted-foreground tabular-nums shrink-0 w-20",
                        { task.due.as_deref().unwrap_or("") }
                    }
                    span { class: "text-sm font-medium flex-1 truncate", "{task.title}" }
                    for tag in task.tags.iter().take(2) {
                        span { class: "inline-flex items-center h-4 rounded-full bg-secondary text-secondary-foreground px-1.5 text-[10px] font-medium",
                            "{tag}"
                        }
                    }
                    if let Some(ref assignee) = task.assignee {
                        UserAvatar { name: assignee.clone(), size: "size-5".to_string() }
                    }
                }
            }
        }
    }
}

#[component]
fn StatCard(label: &'static str, value: usize) -> Element {
    rsx! {
        div { class: "rounded-lg border border-border bg-card px-3 py-2 text-center",
            div { class: "text-lg font-semibold tabular-nums", "{value}" }
            div { class: "text-[10px] text-muted-foreground", "{label}" }
        }
    }
}
