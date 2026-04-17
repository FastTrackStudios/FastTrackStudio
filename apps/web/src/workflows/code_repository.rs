//! Code Repository workflow — software projects, websites, tools.
//!
//! Panels: issues-by-label, milestones

use dioxus::prelude::*;
use super::{PanelDef, WorkflowContext, WorkflowExt};
use crate::{UserAvatar, next_subtask};

pub struct CodeRepositoryExt;

impl WorkflowExt for CodeRepositoryExt {
    fn label(&self) -> &'static str {
        "Code Repository"
    }

    fn stats_panel(&self, ctx: &WorkflowContext) -> Element {
        let engineering = ctx.tasks.iter().filter(|t| has_tag(t, &["engineering"])).count();
        let frontend = ctx.tasks.iter().filter(|t| has_tag(t, &["frontend"])).count();
        let devops = ctx.tasks.iter().filter(|t| has_tag(t, &["devops", "infra", "ci"])).count();
        let bugs = ctx.tasks.iter().filter(|t| has_tag(t, &["bug", "fix"])).count();
        let docs = ctx.tasks.iter().filter(|t| has_tag(t, &["docs", "documentation"])).count();
        let done = ctx.tasks.iter().filter(|t| t.status == "Done").count();

        rsx! {
            div { class: "grid grid-cols-3 sm:grid-cols-6 gap-3",
                StatCard { label: "Engineering", value: engineering }
                StatCard { label: "Frontend", value: frontend }
                StatCard { label: "DevOps", value: devops }
                StatCard { label: "Bugs", value: bugs }
                StatCard { label: "Docs", value: docs }
                StatCard { label: "Closed", value: done }
            }
            if let Some(ref repo) = ctx.project.repo {
                div { class: "flex items-center gap-2 text-xs text-muted-foreground mt-1",
                    svg {
                        class: "size-3.5",
                        xmlns: "http://www.w3.org/2000/svg",
                        view_box: "0 0 24 24",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "2",
                        path { d: "M15 22v-4a4.8 4.8 0 0 0-1-3.5c3 0 6-2 6-5.5.08-1.25-.27-2.48-1-3.5.28-1.15.28-2.35 0-3.5 0 0-1 0-3 1.5-2.64-.5-5.36-.5-8 0C6 2 5 2 5 2c-.3 1.15-.3 2.35 0 3.5A5.403 5.403 0 0 0 4 9c0 3.5 3 5.5 6 5.5-.39.49-.68 1.05-.85 1.65S8.93 17.38 9 18v4" }
                        path { d: "M9 18c-4.51 2-5-2-7-2" }
                    }
                    span { "{repo}" }
                }
            }
        }
    }

    fn panels(&self) -> Vec<PanelDef> {
        vec![
            PanelDef {
                id: "issues",
                label: "Open Issues",
                collapsed: false,
                render: render_issues,
            },
            PanelDef {
                id: "backlog",
                label: "Backlog",
                collapsed: true,
                render: render_backlog,
            },
        ]
    }
}

fn has_tag(task: &crate::ApiTask, tags: &[&str]) -> bool {
    task.tags.iter().any(|t| tags.contains(&t.as_str()))
}

fn render_issues(ctx: &WorkflowContext) -> Element {
    // Open + InProgress tasks, sorted by priority
    let mut issues: Vec<_> = ctx.tasks.iter()
        .filter(|t| t.status == "Open" || t.status == "InProgress")
        .collect();
    issues.sort_by_key(|t| match t.priority.as_str() {
        "Urgent" => 0, "High" => 1, "Normal" => 2, "Low" => 3, _ => 4,
    });

    if issues.is_empty() {
        return rsx! {
            div { class: "px-4 py-3 text-xs text-muted-foreground italic", "No open issues" }
        };
    }

    rsx! {
        div { class: "divide-y divide-border",
            for task in issues.iter() {
                div { class: "px-4 py-2.5",
                    div { class: "flex items-center justify-between",
                        div { class: "flex items-center gap-2 min-w-0",
                            span { class: "size-2 rounded-full shrink-0",
                                class: if task.status == "InProgress" { "bg-chart-2" } else { "bg-chart-1" },
                            }
                            span { class: "text-sm font-medium truncate", "{task.title}" }
                        }
                        div { class: "flex items-center gap-2 shrink-0",
                            for tag in task.tags.iter().take(2) {
                                span { class: "inline-flex items-center h-4 rounded-full bg-secondary text-secondary-foreground px-1.5 text-[10px] font-medium",
                                    "{tag}"
                                }
                            }
                            span { class: "text-[10px] text-muted-foreground", "{task.priority}" }
                            if let Some(ref assignee) = task.assignee {
                                UserAvatar { name: assignee.clone(), size: "size-5".to_string() }
                            }
                        }
                    }
                    if let Some(ref body) = task.body {
                        if let Some(st) = next_subtask(body) {
                            p { class: "text-xs text-muted-foreground mt-0.5 ml-4", "→ {st}" }
                        }
                    }
                }
            }
        }
    }
}

fn render_backlog(ctx: &WorkflowContext) -> Element {
    let backlog: Vec<_> = ctx.tasks.iter()
        .filter(|t| t.status == "Planned" || t.status == "OnHold")
        .collect();

    if backlog.is_empty() {
        return rsx! {
            div { class: "px-4 py-3 text-xs text-muted-foreground italic", "Backlog empty" }
        };
    }

    rsx! {
        div { class: "divide-y divide-border",
            for task in backlog.iter() {
                div { class: "flex items-center gap-3 px-4 py-2 text-muted-foreground",
                    span { class: "size-2 rounded-full bg-muted-foreground/30 shrink-0" }
                    span { class: "text-sm flex-1 truncate", "{task.title}" }
                    span { class: "text-[10px]", "{task.status}" }
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
