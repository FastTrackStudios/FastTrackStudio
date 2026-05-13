//! `RunningTimerBar` — Solidtime's sticky top bar.
//!
//! Caller wraps in their own sticky container; this component just
//! renders the row. When `running` is `Some`, shows the live elapsed
//! clock + a destructive stop button. When `None`, shows a primary
//! play button and editable fields that bundle into a
//! `TimeEntryStartInput` on click.

use std::time::Duration as StdDuration;

use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::components::ComboboxItemData;
use fts_ui::lucide_dioxus::{DollarSign, Play, Square};
use fts_ui::prelude::*;
use project_proto::Project;
use timer_proto::{TimeEntry, TimeEntryUpdate};
use uuid::Uuid;

use super::common::format_duration_hms;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct TimeEntryStartInput {
    pub description: Option<String>,
    pub project_id: Option<Uuid>,
    pub client_id: Option<Uuid>,
    pub billable: bool,
    pub tags: Vec<String>,
}

#[derive(Props, Clone, PartialEq)]
pub struct RunningTimerBarProps {
    pub running: Option<TimeEntry>,
    pub projects: Vec<Project>,
    pub on_start: EventHandler<TimeEntryStartInput>,
    pub on_stop: EventHandler<Uuid>,
    pub on_update: EventHandler<(Uuid, TimeEntryUpdate)>,
}

#[component]
pub fn RunningTimerBar(props: RunningTimerBarProps) -> Element {
    let running = props.running.clone();
    let is_running = running.is_some();

    // Draft state for the "not yet running" path.
    let mut draft_desc = use_signal(String::new);
    let mut draft_project: Signal<Option<Uuid>> = use_signal(|| None);
    let mut draft_billable = use_signal(|| false);

    // Live elapsed seconds — recomputed each tick while running.
    let mut elapsed = use_signal(|| 0i64);

    // Echo the currently-running entry's description / project /
    // billable into the editable inputs so the bar always reflects
    // truth even after a parent re-render.
    let running_id = running.as_ref().map(|e| e.id);
    let running_desc = running
        .as_ref()
        .and_then(|e| e.description.clone())
        .unwrap_or_default();
    let running_proj = running.as_ref().and_then(|e| e.project_id);
    let running_billable = running.as_ref().map(|e| e.billable).unwrap_or(false);
    let running_start = running.as_ref().map(|e| e.start_time);

    // Sync drafts with the running entry on transitions in/out.
    use_effect(use_reactive(
        &(
            running_id,
            running_desc.clone(),
            running_proj,
            running_billable,
        ),
        move |(_id, desc, proj, billable)| {
            draft_desc.set(desc);
            draft_project.set(proj);
            draft_billable.set(billable);
        },
    ));

    // Initial elapsed snapshot.
    use_effect(use_reactive(&running_start, move |start| {
        if let Some(s) = start {
            elapsed.set((Utc::now() - s).num_seconds().max(0));
        } else {
            elapsed.set(0);
        }
    }));

    // Live ticker — sleeps 1s repeatedly while running.
    let _ = use_future(move || async move {
        loop {
            gloo_timers::future::TimeoutFuture::new(1000).await;
            if let Some(start) = running_start {
                elapsed.set((Utc::now() - start).num_seconds().max(0));
            } else {
                // Idle path still polls but does no work; cheap and
                // avoids race conditions when starting a new timer.
                let _ = StdDuration::from_millis(0);
            }
        }
    });

    // Project options (id-as-string for Combobox value compatibility).
    let project_items: Vec<ComboboxItemData> = props
        .projects
        .iter()
        .map(|p| ComboboxItemData::new(p.id.to_string(), p.name.clone()))
        .collect();
    let project_value = use_signal(|| draft_project().map(|id| id.to_string()).unwrap_or_default());
    // Keep the combobox signal in sync with draft on transitions.
    let mut project_value_for_effect = project_value;
    use_effect(use_reactive(&draft_project(), move |proj| {
        project_value_for_effect.set(proj.map(|id| id.to_string()).unwrap_or_default());
    }));

    let projects_map = props.projects.clone();
    let project_color_for_current = {
        let pv = project_value();
        projects_map
            .iter()
            .find(|p| p.id.to_string() == pv)
            .and_then(|p| p.color.clone())
    };
    let project_name_for_current = {
        let pv = project_value();
        projects_map
            .iter()
            .find(|p| p.id.to_string() == pv)
            .map(|p| p.name.clone())
    };

    // Persist edits while running. Description on blur, project +
    // billable immediately.
    let dispatch_desc = {
        let on_update = props.on_update;
        move |v: String| {
            if let Some(id) = running_id {
                let new_desc = if v.trim().is_empty() {
                    None
                } else {
                    Some(v.clone())
                };
                on_update.call((
                    id,
                    TimeEntryUpdate {
                        description: Some(new_desc),
                        ..Default::default()
                    },
                ));
            }
        }
    };

    let dispatch_project = {
        let on_update = props.on_update;
        let projects = props.projects.clone();
        move |v: String| {
            let id = running_id;
            let new_proj: Option<Uuid> = if v.is_empty() {
                None
            } else {
                Uuid::parse_str(&v).ok()
            };
            let new_client = new_proj
                .and_then(|pid| projects.iter().find(|p| p.id == pid))
                .and_then(|_| None::<Uuid>);
            draft_project.set(new_proj);
            if let Some(id) = id {
                on_update.call((
                    id,
                    TimeEntryUpdate {
                        project_id: Some(new_proj),
                        client_id: Some(new_client),
                        ..Default::default()
                    },
                ));
            }
        }
    };

    let dispatch_billable = {
        let on_update = props.on_update;
        move |v: bool| {
            draft_billable.set(v);
            if let Some(id) = running_id {
                on_update.call((
                    id,
                    TimeEntryUpdate {
                        billable: Some(v),
                        ..Default::default()
                    },
                ));
            }
        }
    };

    let on_play = {
        let on_start = props.on_start;
        let on_stop = props.on_stop;
        move |_| {
            if let Some(id) = running_id {
                on_stop.call(id);
            } else {
                on_start.call(TimeEntryStartInput {
                    description: if draft_desc.read().trim().is_empty() {
                        None
                    } else {
                        Some(draft_desc.read().clone())
                    },
                    project_id: draft_project(),
                    client_id: None,
                    billable: draft_billable(),
                    tags: Vec::new(),
                });
            }
        }
    };

    let clock_text = format_duration_hms(elapsed());

    rsx! {
        div {
            class: "flex items-center gap-3 rounded-lg border bg-card px-3 py-2 shadow-sm",

            // Play / Stop button
            Button {
                variant: if is_running { ButtonVariant::Destructive } else { ButtonVariant::Primary },
                size: ButtonSize::Medium,
                on_click: on_play,
                if is_running {
                    Square { size: 16 }
                } else {
                    Play { size: 16 }
                }
            }

            // Description
            div { class: "flex-1 min-w-40",
                Input {
                    value: draft_desc,
                    placeholder: "What are you working on?",
                    on_change: move |e: FormEvent| {
                        let v = e.value();
                        if is_running {
                            dispatch_desc(v);
                        }
                    },
                }
            }

            // Project Combobox (placeholder shows current name; color
            // dot rendered to the side since ComboboxTrigger has no
            // children slot).
            div { class: "flex items-center gap-2 w-64",
                if project_color_for_current.is_some() {
                    span {
                        class: "inline-block size-2.5 rounded-full shrink-0",
                        style: format!("background: {}", project_color_for_current.clone().unwrap_or_else(|| "#888".into())),
                    }
                }
                div { class: "flex-1",
                    Combobox {
                        value: project_value,
                        on_change: dispatch_project,
                        ComboboxTrigger {
                            placeholder: project_name_for_current.clone().unwrap_or_else(|| "Project".to_string()),
                        }
                        ComboboxContent {
                            items: project_items,
                            empty: rsx! {
                                div { class: "py-4 text-center text-xs text-muted-foreground",
                                    "No projects."
                                }
                            }
                        }
                    }
                }
            }

            // Billable switch
            HStack { class: "items-center gap-1",
                DollarSign { size: 14 }
                Switch {
                    checked: draft_billable,
                    on_change: dispatch_billable,
                }
            }

            // Elapsed clock
            div {
                class: if is_running {
                    "font-mono text-lg tabular-nums text-foreground"
                } else {
                    "font-mono text-lg tabular-nums text-muted-foreground"
                },
                "{clock_text}"
            }
        }
    }
}
