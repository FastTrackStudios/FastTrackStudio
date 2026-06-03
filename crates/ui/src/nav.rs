//! Sidebar + mobile-tab definitions.
//!
//! `NavTab` is the shared shape; `nav_tabs()` is the desktop set
//! and `primary_mobile_tabs()` is the smaller bottom-bar set.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    BookOpen, CalendarClock, CalendarDays, ChartGantt, CircleCheck, Dumbbell, Flag, FolderKanban,
    House, Inbox as InboxIcon, MapPin, Notebook, Package, ReceiptText, Settings as SettingsIcon,
    Target, Timer, Utensils, Wallet,
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
fn icon_locations() -> Element {
    rsx! { MapPin { size: 16 } }
}
fn icon_inventory() -> Element {
    rsx! { Package { size: 16 } }
}
fn icon_milestones() -> Element {
    rsx! { Flag { size: 16 } }
}
fn icon_fitness() -> Element {
    rsx! { Dumbbell { size: 16 } }
}
fn icon_mealplan() -> Element {
    rsx! { Utensils { size: 16 } }
}
fn icon_schedule() -> Element {
    rsx! { CalendarDays { size: 16 } }
}
fn icon_bookings() -> Element {
    rsx! { CalendarClock { size: 16 } }
}
fn icon_gantt() -> Element {
    rsx! { ChartGantt { size: 16 } }
}
fn icon_timer() -> Element {
    rsx! { Timer { size: 16 } }
}
fn icon_finances() -> Element {
    rsx! { Wallet { size: 16 } }
}
fn icon_invoices() -> Element {
    rsx! { ReceiptText { size: 16 } }
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
            label: "Locations",
            icon: icon_locations,
            route: Route::LocationsRoute {},
        },
        NavTab {
            label: "Inventory",
            icon: icon_inventory,
            route: Route::InventoryRoute {},
        },
        NavTab {
            label: "Milestones",
            icon: icon_milestones,
            route: Route::MilestonesRoute {},
        },
        NavTab {
            label: "Fitness",
            icon: icon_fitness,
            route: Route::FitnessRoute {},
        },
        NavTab {
            label: "Mealplan",
            icon: icon_mealplan,
            route: Route::MealplanRoute {},
        },
        NavTab {
            label: "Schedule",
            icon: icon_schedule,
            route: Route::ScheduleRoute {},
        },
        NavTab {
            label: "Bookings",
            icon: icon_bookings,
            route: Route::BookingsRoute {},
        },
        NavTab {
            label: "Gantt",
            icon: icon_gantt,
            route: Route::GanttRoute {},
        },
        NavTab {
            label: "Timer",
            icon: icon_timer,
            route: Route::TimerRoute {},
        },
        NavTab {
            label: "Finances",
            icon: icon_finances,
            route: Route::FinancesRoute {},
        },
        NavTab {
            label: "Invoices",
            icon: icon_invoices,
            route: Route::InvoicesRoute {},
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
        Route::ProjectDetailRoute { .. } => "Project",
        Route::GoalsRoute {} => "Goals",
        Route::TasksRoute {} => "Tasks",
        Route::VaultRoute {} => "Vault",
        Route::LocationsRoute {} => "Locations",
        Route::InventoryRoute {} => "Inventory",
        Route::MilestonesRoute {} => "Milestones",
        Route::FitnessRoute {} => "Fitness",
        Route::MealplanRoute {} => "Mealplan",
        Route::ScheduleRoute {} => "Schedule",
        Route::BookingsRoute {} => "Bookings",
        Route::GanttRoute {} => "Gantt",
        Route::TimerRoute {} => "Timer",
        Route::FinancesRoute {} => "Finances",
        Route::InvoicesRoute {} => "Invoices",
        Route::WikiRoute {} => "Wiki",
        Route::SettingsRoute {} => "Settings",
    }
}
