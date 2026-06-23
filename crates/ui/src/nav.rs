//! Sidebar + mobile-tab definitions.
//!
//! `NavTab` is the shared shape; `nav_tabs()` is the desktop set
//! and `primary_mobile_tabs()` is the smaller bottom-bar set.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    BookOpen, Bot, CalendarClock, CalendarDays, ChartGantt, CircleCheck, Dumbbell, Flag,
    FolderKanban, GitBranch, House, Inbox as InboxIcon, Mail, MapPin, Notebook, Package,
    ReceiptText, Scale, Settings as SettingsIcon, Target, Timer, Utensils, Wallet, Waypoints,
    Youtube,
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
fn icon_email() -> Element {
    rsx! { Mail { size: 16 } }
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
fn icon_scripture() -> Element {
    rsx! { BookOpen { size: 16 } }
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
fn icon_ledger() -> Element {
    rsx! { Scale { size: 16 } }
}
fn icon_agents() -> Element {
    rsx! { Bot { size: 16 } }
}
fn icon_repos() -> Element {
    rsx! { GitBranch { size: 16 } }
}
fn icon_wiki() -> Element {
    rsx! { BookOpen { size: 16 } }
}
fn icon_connections() -> Element {
    rsx! { Waypoints { size: 16 } }
}
fn icon_watch() -> Element {
    rsx! { Youtube { size: 16 } }
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
            label: "Email",
            icon: icon_email,
            route: Route::EmailRoute {},
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
            route: Route::VaultRoute {
                path: String::new(),
            },
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
            label: "Scripture",
            icon: icon_scripture,
            route: Route::ScriptureRoute {},
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
            label: "Ledger",
            icon: icon_ledger,
            route: Route::LedgerRoute {},
        },
        NavTab {
            label: "Wiki",
            icon: icon_wiki,
            route: Route::WikiRoute {},
        },
        NavTab {
            label: "Connections",
            icon: icon_connections,
            route: Route::ConnectionsRoute {},
        },
        // Bases now open inside the vault (selecting a `.base` file
        // renders its tables), so no dedicated tab — Obsidian-style.
        NavTab {
            label: "Watch",
            icon: icon_watch,
            route: Route::WatchRoute {
                v: String::new(),
                node: String::new(),
            },
        },
        NavTab {
            label: "Agents",
            icon: icon_agents,
            route: Route::AgentsRoute {},
        },
        NavTab {
            label: "Repos",
            icon: icon_repos,
            route: Route::ReposRoute {},
        },
        NavTab {
            label: "Settings",
            icon: icon_settings,
            route: Route::SettingsRoute {},
        },
    ]
}

/// The bottom-tab-bar set: four primary destinations. Everything else
/// lives behind the "More" tab the bar appends itself.
pub fn primary_mobile_tabs() -> Vec<NavTab> {
    vec![
        NavTab {
            label: "Home",
            icon: icon_house,
            route: Route::HomeRoute {},
        },
        NavTab {
            label: "Tasks",
            icon: icon_tasks,
            route: Route::TasksRoute {},
        },
        NavTab {
            label: "Projects",
            icon: icon_projects,
            route: Route::ProjectsRoute {},
        },
        NavTab {
            label: "Schedule",
            icon: icon_schedule,
            route: Route::ScheduleRoute {},
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
        Route::EmailRoute {} => "Email",
        Route::ProjectsRoute {} => "Projects",
        Route::ProjectDetailRoute { .. } => "Project",
        Route::GoalsRoute {} => "Goals",
        Route::TasksRoute {} => "Tasks",
        Route::TaskDetailRoute { .. } => "Task",
        Route::VaultRoute { .. } => "Vault",
        Route::LocationsRoute {} => "Locations",
        Route::InventoryRoute {} => "Inventory",
        Route::ScriptureRoute {} => "Scripture",
        Route::MilestonesRoute {} => "Milestones",
        Route::FitnessRoute {} => "Fitness",
        Route::MealplanRoute {} => "Mealplan",
        Route::RecipeCookRoute { .. } => "Cook",
        Route::RecipeEditRoute { .. } => "Edit recipe",
        Route::ScheduleRoute {} => "Schedule",
        Route::BookingsRoute {} => "Bookings",
        Route::GanttRoute {} => "Gantt",
        Route::TimerRoute {} => "Timer",
        Route::FinancesRoute {} => "Finances",
        Route::InvoicesRoute {} => "Invoices",
        Route::LedgerRoute {} => "Ledger",
        Route::WikiRoute {} => "Wiki",
        Route::ConnectionsRoute {} => "Connections",
        Route::BasesRoute {} => "Bases",
        Route::WatchRoute { .. } => "Watch",
        Route::WikiSourcesRoute {} => "Archived sources",
        Route::WikiSourceRoute { .. } => "Source",
        Route::AgentsRoute {} => "Agents",
        Route::ReposRoute {} => "Repos",
        Route::SettingsRoute {} => "Settings",
    }
}
