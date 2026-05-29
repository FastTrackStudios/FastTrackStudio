//! Sidebar + mobile-tab definitions.
//!
//! `NavTab` is the shared shape; `nav_tabs()` is the desktop set
//! and `primary_mobile_tabs()` is the smaller bottom-bar set.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    BookOpen, CalendarDays, ChartGantt, CircleCheck, FolderKanban, House, Inbox as InboxIcon,
    Notebook, Settings as SettingsIcon, Target,
};

use crate::routes::Route;

#[derive(Clone, PartialEq)]
#[allow(unpredictable_function_pointer_comparisons)]
pub struct NavTab {
    pub label: &'static str,
    pub icon: fn() -> Element,
    pub route: Route,
}

fn icon_house() -> Element {
    rsx! { House { size: 16 } }
}
fn icon_inbox() -> Element {
    rsx! { InboxIcon { size: 16 } }
}
fn icon_projects() -> Element {
    rsx! { FolderKanban { size: 16 } }
}
fn icon_tasks() -> Element {
    rsx! { CircleCheck { size: 16 } }
}
fn icon_vault() -> Element {
    rsx! { Notebook { size: 16 } }
}
fn icon_schedule() -> Element {
    rsx! { CalendarDays { size: 16 } }
}
fn icon_gantt() -> Element {
    rsx! { ChartGantt { size: 16 } }
}
fn icon_wiki() -> Element {
    rsx! { BookOpen { size: 16 } }
}
fn icon_goals() -> Element {
    rsx! { Target { size: 18 } }
}
fn icon_settings() -> Element {
    rsx! { SettingsIcon { size: 16 } }
}

pub fn nav_tabs() -> Vec<NavTab> {
    vec![
        NavTab {
            label: "Home",
            icon: icon_house,
            route: Route::HomeRoute {},
        },
        NavTab {
            label: "Inbox",
            icon: icon_inbox,
            route: Route::InboxRoute {},
        },
        NavTab {
            label: "Projects",
            icon: icon_projects,
            route: Route::ProjectsRoute {},
        },
        NavTab {
            label: "Goals",
            icon: icon_goals,
            route: Route::GoalsRoute {},
        },
        NavTab {
            label: "Tasks",
            icon: icon_tasks,
            route: Route::TasksRoute {},
        },
        NavTab {
            label: "Vault",
            icon: icon_vault,
            route: Route::VaultRoute {},
        },
        NavTab {
            label: "Schedule",
            icon: icon_schedule,
            route: Route::ScheduleRoute {},
        },
        NavTab {
            label: "Gantt",
            icon: icon_gantt,
            route: Route::GanttRoute {},
        },
        NavTab {
            label: "Wiki",
            icon: icon_wiki,
            route: Route::WikiRoute {},
        },
        NavTab {
            label: "Settings",
            icon: icon_settings,
            route: Route::SettingsRoute {},
        },
    ]
}

pub fn primary_mobile_tabs() -> Vec<NavTab> {
    vec![
        NavTab {
            label: "Home",
            icon: icon_house,
            route: Route::HomeRoute {},
        },
        NavTab {
            label: "Inbox",
            icon: icon_inbox,
            route: Route::InboxRoute {},
        },
        NavTab {
            label: "Projects",
            icon: icon_projects,
            route: Route::ProjectsRoute {},
        },
        NavTab {
            label: "Vault",
            icon: icon_vault,
            route: Route::VaultRoute {},
        },
        NavTab {
            label: "Settings",
            icon: icon_settings,
            route: Route::SettingsRoute {},
        },
    ]
}

pub fn tabs_match(current: &Route, tab: &NavTab) -> bool {
    std::mem::discriminant(current) == std::mem::discriminant(&tab.route)
}

pub fn route_title(route: &Route) -> &'static str {
    match route {
        Route::HomeRoute {} => "Home",
        Route::InboxRoute {} => "Inbox",
        Route::ProjectsRoute {} => "Projects",
        Route::GoalsRoute {} => "Goals",
        Route::TasksRoute {} => "Tasks",
        Route::VaultRoute {} => "Vault",
        Route::ScheduleRoute {} => "Schedule",
        Route::GanttRoute {} => "Gantt",
        Route::WikiRoute {} => "Wiki",
        Route::SettingsRoute {} => "Settings",
    }
}
