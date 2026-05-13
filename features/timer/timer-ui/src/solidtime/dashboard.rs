//! `SolidtimeDashboard` — composes the running bar, stats, filters,
//! week report, table, and manual-entry dialog into a single mounted
//! component the route renders.

use std::collections::HashMap;

use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Plus;
use fts_ui::prelude::*;
use invoice_proto::Client;
use project_proto::Project;
use timer_proto::{TimeEntry, TimeEntryCreate, TimeEntryUpdate};
use uuid::Uuid;

use super::bar::{RunningTimerBar, TimeEntryStartInput};
use super::common::week_start;
use super::filter::{TimerFilterBar, TimerFilterState, apply_filter};
use super::idle::{IdleWindow, TimerIdleDialog};
use super::idle_detector::use_idle_detector;
use super::manual::ManualTimeEntryDialog;
use super::report::TimerWeekReport;
use super::stats::TimerSummaryStats;
use super::table::TimeEntryTable;

#[derive(Props, Clone, PartialEq)]
pub struct SolidtimeDashboardProps {
    pub entries: Vec<TimeEntry>,
    pub projects: Vec<Project>,
    pub clients: Vec<Client>,
    pub on_start: EventHandler<TimeEntryStartInput>,
    pub on_stop: EventHandler<Uuid>,
    pub on_create: EventHandler<TimeEntryCreate>,
    pub on_edit: EventHandler<(Uuid, TimeEntryUpdate)>,
    pub on_delete: EventHandler<Uuid>,
    pub on_duplicate: EventHandler<Uuid>,
}

#[component]
pub fn SolidtimeDashboard(props: SolidtimeDashboardProps) -> Element {
    let today = Utc::now().date_naive();
    let mut filter = use_signal(TimerFilterState::default);
    let mut week = use_signal(|| week_start(today));
    let mut manual_open = use_signal(|| false);

    // Tab-idle detector. Fires Some(IdleWindow) when the tab has been
    // hidden ≥ 5 minutes and the user returns. v1 threshold is fixed.
    let idle_signal = use_idle_detector(5 * 60);
    let mut idle_open = use_signal(|| false);
    let mut idle_window: Signal<Option<IdleWindow>> = use_signal(|| None);

    let projects_by_id: HashMap<Uuid, Project> =
        props.projects.iter().map(|p| (p.id, p.clone())).collect();
    let clients_by_id: HashMap<Uuid, Client> =
        props.clients.iter().map(|c| (c.id, c.clone())).collect();

    let all_tags: Vec<String> = {
        let mut s: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
        for e in props.entries.iter() {
            for t in e.tags.iter() {
                s.insert(t.clone());
            }
        }
        s.into_iter().collect()
    };

    let running = props.entries.iter().find(|e| e.end_time.is_none()).cloned();

    // When the detector signal fires and a timer is running, surface
    // the dialog. We mirror the window into a local signal so the
    // dialog continues to display the captured range after the
    // detector signal is cleared.
    {
        let running_present = running.is_some();
        use_effect(use_reactive(
            &(idle_signal(), running_present),
            move |(w, present)| {
                if let Some(w) = w {
                    if present {
                        idle_window.set(Some(w));
                        idle_open.set(true);
                    }
                }
            },
        ));
    }

    let filtered = apply_filter(&props.entries, &filter.read());

    rsx! {
        VStack { class: "gap-4",

            // Sticky running bar
            div { class: "sticky top-0 z-30 -mx-3 px-3 py-2 bg-background/95 backdrop-blur border-b",
                RunningTimerBar {
                    running: running.clone(),
                    projects: props.projects.clone(),
                    on_start: props.on_start,
                    on_stop: props.on_stop,
                    on_update: props.on_edit,
                }
            }

            // Stats row
            TimerSummaryStats {
                entries: props.entries.clone(),
                today: today,
            }

            // Filter + manual entry
            HStack { class: "items-center justify-between gap-2 flex-wrap",
                TimerFilterBar {
                    state: filter.read().clone(),
                    projects: props.projects.clone(),
                    clients: props.clients.clone(),
                    all_tags: all_tags,
                    on_change: move |s: TimerFilterState| filter.set(s),
                }
                Button {
                    variant: ButtonVariant::Primary,
                    size: ButtonSize::Small,
                    on_click: move |_| manual_open.set(true),
                    Plus { size: 14 }
                    " Manual entry"
                }
            }

            // Side-by-side report + table on wide screens.
            div { class: "grid grid-cols-1 lg:grid-cols-3 gap-4",
                div { class: "lg:col-span-1",
                    TimerWeekReport {
                        week_start: week(),
                        entries: props.entries.clone(),
                        projects_by_id: projects_by_id.clone(),
                        on_week_change: move |d| week.set(d),
                    }
                }
                div { class: "lg:col-span-2",
                    TimeEntryTable {
                        entries: filtered,
                        projects_by_id: projects_by_id,
                        clients_by_id: clients_by_id,
                        on_edit: props.on_edit,
                        on_delete: props.on_delete,
                        on_duplicate: props.on_duplicate,
                    }
                }
            }

            // Idle dialog — user picks Keep / Discard / Split. The
            // dashboard translates each into an `on_edit` patch
            // against the running entry (or, for Split, a stop +
            // create pair).
            TimerIdleDialog {
                open: idle_open(),
                running: running.clone(),
                idle: idle_window(),
                on_keep: move |_| {
                    // Keep: no-op — the running entry's clock kept
                    // ticking, so the time is already accounted for.
                },
                on_discard: {
                    let running_for_discard = running.clone();
                    move |_| {
                        if let (Some(entry), Some(win)) = (running_for_discard.clone(), idle_window()) {
                            // Stop the entry at the moment we went
                            // idle. The repo treats `end_time = Some`
                            // as a stop.
                            props.on_edit.call((
                                entry.id,
                                TimeEntryUpdate {
                                    end_time: Some(Some(win.started_at)),
                                    ..Default::default()
                                },
                            ));
                        }
                    }
                },
                on_split: {
                    let running_for_split = running.clone();
                    move |_| {
                        if let (Some(entry), Some(win)) = (running_for_split.clone(), idle_window()) {
                            // Stop original at the idle boundary…
                            props.on_edit.call((
                                entry.id,
                                TimeEntryUpdate {
                                    end_time: Some(Some(win.started_at)),
                                    ..Default::default()
                                },
                            ));
                            // …then resume by starting a fresh entry
                            // at idle.ended_at carrying the same
                            // metadata. The route owns the actual
                            // create call; we re-use TimeEntryCreate.
                            props.on_create.call(TimeEntryCreate {
                                task_id: entry.task_id,
                                project_id: entry.project_id,
                                client_id: entry.client_id,
                                user: entry.user.clone(),
                                start_time: win.ended_at,
                                end_time: None,
                                description: entry.description.clone(),
                                billable: entry.billable,
                                manual: false,
                                billable_rate_cents: entry.billable_rate_cents,
                                tags: entry.tags.clone(),
                                invoiced_at: None,
                            });
                        }
                    }
                },
                on_close: move |_| {
                    idle_open.set(false);
                    idle_window.set(None);
                },
            }

            ManualTimeEntryDialog {
                open: manual_open(),
                projects: props.projects.clone(),
                clients: props.clients.clone(),
                initial: None,
                on_submit: move |p: TimeEntryCreate| {
                    props.on_create.call(p);
                    manual_open.set(false);
                },
                on_close: move |_| manual_open.set(false),
            }
        }
    }
}
