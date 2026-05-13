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

    let filtered = apply_filter(&props.entries, &filter.read());

    rsx! {
        VStack { class: "gap-4",

            // Sticky running bar
            div { class: "sticky top-0 z-30 -mx-3 px-3 py-2 bg-background/95 backdrop-blur border-b",
                RunningTimerBar {
                    running: running,
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
