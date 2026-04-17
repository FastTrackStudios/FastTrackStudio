//! Video Production workflow — music videos, promos, livestreams, docs.
//!
//! Panels: shot-list, footage-log, deliverables

use dioxus::prelude::*;
use super::{PanelDef, WorkflowContext, WorkflowExt};
use crate::UserAvatar;

pub struct VideoProductionExt;

impl WorkflowExt for VideoProductionExt {
    fn label(&self) -> &'static str {
        "Video Production"
    }

    fn stats_panel(&self, ctx: &WorkflowContext) -> Element {
        let preproduction = ctx.tasks.iter().filter(|t| has_tag(t, &["pre-production", "scripting", "storyboard"])).count();
        let shooting = ctx.tasks.iter().filter(|t| has_tag(t, &["shooting", "filming", "camera"])).count();
        let editing = ctx.tasks.iter().filter(|t| has_tag(t, &["editing", "cut", "assembly"])).count();
        let color = ctx.tasks.iter().filter(|t| has_tag(t, &["color", "grade", "grading"])).count();
        let vfx = ctx.tasks.iter().filter(|t| has_tag(t, &["vfx", "motion", "graphics"])).count();
        let delivery = ctx.tasks.iter().filter(|t| has_tag(t, &["delivery", "export", "render"])).count();

        rsx! {
            div { class: "grid grid-cols-3 sm:grid-cols-6 gap-3",
                StatCard { label: "Pre-Prod", value: preproduction }
                StatCard { label: "Shooting", value: shooting }
                StatCard { label: "Editing", value: editing }
                StatCard { label: "Color", value: color }
                StatCard { label: "VFX", value: vfx }
                StatCard { label: "Delivery", value: delivery }
            }
        }
    }

    fn panels(&self) -> Vec<PanelDef> {
        vec![
            PanelDef {
                id: "shot-list",
                label: "Shot List",
                collapsed: false,
                render: render_shot_list,
            },
            PanelDef {
                id: "sessions",
                label: "Shoot Log",
                collapsed: false,
                render: render_shoot_log,
            },
            PanelDef {
                id: "deliverables",
                label: "Deliverables",
                collapsed: false,
                render: render_deliverables,
            },
        ]
    }

    fn session_row(&self, session: &crate::ApiSession) -> Option<Element> {
        let type_color = match session.session_type.as_str() {
            "Writing" | "PreProduction" => "bg-chart-4",
            "Recording" => "bg-chart-1",  // "shooting"
            "Editing" => "bg-chart-2",
            "Review" => "bg-chart-3",
            _ => "bg-secondary",
        };

        Some(rsx! {
            div { class: "flex items-center gap-3 px-4 py-2.5",
                span { class: "size-2 rounded-full {type_color}" }
                div { class: "flex-1 min-w-0",
                    div { class: "flex items-center gap-2",
                        span { class: "inline-flex items-center h-4 rounded-full bg-secondary text-secondary-foreground px-1.5 text-[10px] font-medium",
                            "{session.session_type}"
                        }
                        span { class: "text-sm font-medium truncate", "{session.title}" }
                    }
                    if let Some(ref loc) = session.location {
                        span { class: "text-xs text-muted-foreground", "{loc}" }
                    }
                }
                if !session.attendees.is_empty() {
                    div { class: "flex -space-x-1 shrink-0",
                        for a in session.attendees.iter() {
                            UserAvatar { name: a.clone(), size: "size-4".to_string() }
                        }
                    }
                }
                if let Some(ref date) = session.date {
                    span { class: "text-xs text-muted-foreground tabular-nums shrink-0", "{date}" }
                }
            }
        })
    }
}

fn has_tag(task: &crate::ApiTask, tags: &[&str]) -> bool {
    task.tags.iter().any(|t| tags.contains(&t.as_str()))
}

fn render_shot_list(ctx: &WorkflowContext) -> Element {
    // Use tracks as "shots/scenes" for video projects
    let tracks = &ctx.workflow.tracks;
    if tracks.is_empty() {
        return rsx! {
            div { class: "px-4 py-3 text-xs text-muted-foreground italic", "No shots defined" }
        };
    }

    rsx! {
        div { class: "divide-y divide-border",
            div { class: "grid grid-cols-[2rem_1fr_4rem_4rem] gap-2 px-4 py-1 text-[10px] text-muted-foreground",
                span { class: "text-right", "#" }
                span { "Scene / Shot" }
                span { class: "text-right", "Dur" }
                span { class: "text-right", "Takes" }
            }
            for (i, track) in tracks.iter().enumerate() {
                div { class: "grid grid-cols-[2rem_1fr_4rem_4rem] gap-2 px-4 py-1.5 items-center text-xs hover:bg-accent/30 transition-colors",
                    span { class: "text-muted-foreground tabular-nums text-right", { format!("{}", i + 1) } }
                    span { class: "font-medium", "{track.title}" }
                    span { class: "text-muted-foreground tabular-nums text-right",
                        { track.duration_seconds.map(|s| format!("{}:{:02}", s / 60, s % 60)).unwrap_or_default() }
                    }
                    span { class: "text-muted-foreground tabular-nums text-right",
                        { track.take_count.map(|t| format!("{t}")).unwrap_or_default() }
                    }
                }
            }
        }
    }
}

fn render_shoot_log(ctx: &WorkflowContext) -> Element {
    let sessions = &ctx.workflow.sessions;
    if sessions.is_empty() {
        return rsx! {
            div { class: "px-4 py-3 text-xs text-muted-foreground italic", "No shoots logged" }
        };
    }

    let ext = VideoProductionExt;
    rsx! {
        div { class: "divide-y divide-border",
            for session in sessions.iter() {
                { ext.session_row(session).unwrap_or(rsx! {
                    div { class: "px-4 py-2 text-sm", "{session.title}" }
                }) }
            }
        }
    }
}

fn render_deliverables(ctx: &WorkflowContext) -> Element {
    // Show tasks tagged with delivery-related tags
    let deliverables: Vec<_> = ctx.tasks.iter()
        .filter(|t| has_tag(t, &["delivery", "export", "render", "master"]))
        .collect();

    if deliverables.is_empty() {
        return rsx! {
            div { class: "px-4 py-3 text-xs text-muted-foreground italic", "No deliverables defined" }
        };
    }

    rsx! {
        div { class: "divide-y divide-border",
            for task in deliverables.iter() {
                div { class: "flex items-center gap-3 px-4 py-2",
                    span { class: "size-2 rounded-full",
                        class: if task.status == "Done" { "bg-primary" } else { "bg-muted-foreground/30" },
                    }
                    span { class: "text-sm flex-1",
                        class: if task.status == "Done" { "line-through text-muted-foreground" } else { "font-medium" },
                        "{task.title}"
                    }
                    span { class: "text-xs text-muted-foreground", "{task.status}" }
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
