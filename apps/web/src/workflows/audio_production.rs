//! Audio Production workflow extension.
//!
//! Panels: track-list, sessions, input-list
//! Session types: Writing, Recording, Overdub, Editing, Mixing, Mastering, Review

use dioxus::prelude::*;
use super::{PanelDef, WorkflowContext, WorkflowExt};
use crate::{ApiSession, UserAvatar};

pub struct AudioProductionExt;

impl WorkflowExt for AudioProductionExt {
    fn label(&self) -> &'static str {
        "Audio Production"
    }

    fn stats_panel(&self, ctx: &WorkflowContext) -> Element {
        let recording = ctx.tasks.iter().filter(|t| t.tags.iter().any(|tag| tag == "recording")).count();
        let mixing = ctx.tasks.iter().filter(|t| t.tags.iter().any(|tag| tag == "mixing")).count();
        let mastering = ctx.tasks.iter().filter(|t| t.tags.iter().any(|tag| tag == "mastering")).count();
        let done = ctx.tasks.iter().filter(|t| t.status == "Done").count();
        let sessions = ctx.workflow.sessions.len();
        let tracks = ctx.workflow.tracks.len();

        rsx! {
            div { class: "grid grid-cols-3 sm:grid-cols-6 gap-3",
                StatCard { label: "Tracks", value: tracks }
                StatCard { label: "Sessions", value: sessions }
                StatCard { label: "Recording", value: recording }
                StatCard { label: "Mixing", value: mixing }
                StatCard { label: "Mastering", value: mastering }
                StatCard { label: "Complete", value: done }
            }
        }
    }

    fn panels(&self) -> Vec<PanelDef> {
        vec![
            PanelDef {
                id: "track-list",
                label: "Track List",
                collapsed: false,
                render: render_track_list,
            },
            PanelDef {
                id: "sessions",
                label: "Sessions",
                collapsed: false,
                render: render_sessions,
            },
            PanelDef {
                id: "input-list",
                label: "Input List",
                collapsed: true,
                render: render_input_list,
            },
        ]
    }

    fn session_row(&self, session: &ApiSession) -> Option<Element> {
        let type_color = match session.session_type.as_str() {
            "Writing" | "PreProduction" => "bg-chart-4",
            "Recording" | "Overdub" => "bg-chart-1",
            "Editing" => "bg-chart-3",
            "Mixing" => "bg-chart-2",
            "Mastering" => "bg-chart-5",
            "Review" => "bg-muted-foreground",
            _ => "bg-secondary",
        };

        Some(rsx! {
            div { class: "px-4 py-2.5",
                div { class: "flex items-center justify-between",
                    div { class: "flex items-center gap-2",
                        span { class: "size-2 rounded-full {type_color}" }
                        span { class: "inline-flex items-center h-4 rounded-full bg-secondary text-secondary-foreground px-1.5 text-[10px] font-medium",
                            "{session.session_type}"
                        }
                        span { class: "text-sm font-medium", "{session.title}" }
                    }
                    div { class: "flex items-center gap-2",
                        if let Some(ref loc) = session.location {
                            span { class: "text-[10px] text-muted-foreground", "{loc}" }
                        }
                        if let Some(ref date) = session.date {
                            span { class: "text-xs text-muted-foreground tabular-nums", "{date}" }
                        }
                    }
                }
                if !session.attendees.is_empty() {
                    div { class: "flex items-center gap-1.5 mt-1",
                        div { class: "flex -space-x-1",
                            for a in session.attendees.iter() {
                                UserAvatar { name: a.clone(), size: "size-4".to_string() }
                            }
                        }
                        span { class: "text-[10px] text-muted-foreground",
                            { session.attendees.join(", ") }
                        }
                    }
                }
            }
        })
    }
}

// ── Panel render functions ──────────────────────────────────────────────────

fn render_track_list(ctx: &WorkflowContext) -> Element {
    let tracks = &ctx.workflow.tracks;
    let total_minutes = ctx.workflow.total_duration_minutes;

    if tracks.is_empty() {
        return rsx! {};
    }

    rsx! {
        div { class: "divide-y divide-border",
            // Column headers
            div { class: "grid grid-cols-[2rem_1fr_3rem_3rem_4rem_4rem] gap-2 px-4 py-1 text-[10px] text-muted-foreground",
                span { class: "text-right", "#" }
                span { "Title" }
                span { class: "text-center", "Key" }
                span { class: "text-center", "BPM" }
                span { class: "text-right", "Dur" }
                span { class: "text-right", "Takes" }
            }
            for (i, track) in tracks.iter().enumerate() {
                div { class: "grid grid-cols-[2rem_1fr_3rem_3rem_4rem_4rem] gap-2 px-4 py-1.5 items-center text-xs hover:bg-accent/30 transition-colors",
                    span { class: "text-muted-foreground tabular-nums text-right", { format!("{}", i + 1) } }
                    span { class: "font-medium", "{track.title}" }
                    span { class: "text-muted-foreground text-center",
                        { track.key.as_deref().unwrap_or("") }
                    }
                    span { class: "text-muted-foreground tabular-nums text-center",
                        { track.tempo.map(|t| format!("{t}")).unwrap_or_default() }
                    }
                    span { class: "text-muted-foreground tabular-nums text-right",
                        { track.duration_seconds.map(|s| format!("{}:{:02}", s / 60, s % 60)).unwrap_or_default() }
                    }
                    span { class: "text-muted-foreground tabular-nums text-right",
                        if let Some(takes) = track.take_count {
                            if let Some(best) = track.best_take {
                                { format!("{takes} (#{best})") }
                            } else {
                                { format!("{takes}") }
                            }
                        }
                    }
                }
            }
        }
        if let Some(dur) = total_minutes {
            div { class: "px-4 py-1.5 text-xs text-muted-foreground border-t border-border",
                { format!("{} tracks · {} min", tracks.len(), dur) }
            }
        }
    }
}

fn render_sessions(ctx: &WorkflowContext) -> Element {
    let sessions = &ctx.workflow.sessions;
    let ext = AudioProductionExt;

    if sessions.is_empty() {
        return rsx! {};
    }

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

fn render_input_list(ctx: &WorkflowContext) -> Element {
    let Some(ref il) = ctx.workflow.input_list else {
        return rsx! {};
    };

    rsx! {
        div { class: "divide-y divide-border",
            div { class: "grid grid-cols-[2.5rem_1fr_1fr_2rem_2rem] gap-2 px-4 py-1.5 text-[10px] font-medium text-muted-foreground",
                span { "Ch" }
                span { "Source" }
                span { "Mic" }
                span { "DI" }
                span { "48V" }
            }
            for ch in il.channels.iter() {
                div { class: "grid grid-cols-[2.5rem_1fr_1fr_2rem_2rem] gap-2 px-4 py-1 items-center text-xs hover:bg-accent/30 transition-colors",
                    span { class: "text-muted-foreground tabular-nums font-medium", { format!("{}", ch.channel) } }
                    span { "{ch.source}" }
                    span { class: "text-muted-foreground",
                        { ch.mic.as_deref().unwrap_or("—") }
                    }
                    span { class: "text-center", if ch.di { "✓" } else { "" } }
                    span { class: "text-center", if ch.phantom { "✓" } else { "" } }
                }
            }
        }
        div { class: "px-4 py-1.5 text-xs text-muted-foreground border-t border-border",
            { format!("{} channels · {}", il.channels.len(), il.monitor_type.as_deref().unwrap_or("")) }
        }
    }
}

// ── Shared components ───────────────────────────────────────────────────────

#[component]
fn StatCard(label: &'static str, value: usize) -> Element {
    rsx! {
        div { class: "rounded-lg border border-border bg-card px-3 py-2 text-center",
            div { class: "text-lg font-semibold tabular-nums", "{value}" }
            div { class: "text-[10px] text-muted-foreground", "{label}" }
        }
    }
}
