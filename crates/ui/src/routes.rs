//! Source of truth for the app's top-level routes.
//!
//! Every variant maps to a single page component in
//! [`crate::pages`]. Adding a new sidebar item is a four-step
//! change: add a `Route` variant here, add a page module, list
//! it in [`crate::nav::nav_tabs`], and add a title match arm in
//! [`crate::nav::route_title`].

use dioxus::prelude::*;

use crate::pages;
use crate::shell::app_shell::AppShell;

#[derive(Clone, Debug, PartialEq, Routable)]
#[rustfmt::skip]
pub enum Route {
    #[layout(AppShell)]
        #[route("/")]
        HomeRoute {},

        #[route("/inbox")]
        InboxRoute {},

        #[route("/email")]
        EmailRoute {},

        #[route("/projects")]
        ProjectsRoute {},

        #[route("/projects/:id")]
        ProjectDetailRoute { id: String },

        #[route("/goals")]
        GoalsRoute {},

        #[route("/tasks")]
        TasksRoute {},

        #[route("/tasks/:id")]
        TaskDetailRoute { id: uuid::Uuid },

        // `path` deep-links straight to a note (vault-relative path,
        // e.g. from a knowledge-graph node click); empty opens the
        // tree with nothing selected.
        #[route("/vault?:path")]
        VaultRoute { path: String },

        #[route("/locations")]
        LocationsRoute {},

        #[route("/inventory")]
        InventoryRoute {},

        #[route("/scripture")]
        ScriptureRoute {},

        #[route("/milestones")]
        MilestonesRoute {},

        #[route("/fitness")]
        FitnessRoute {},

        #[route("/mealplan")]
        MealplanRoute {},

        // Deep-link straight into cook mode for one recipe (vault-relative
        // `.cook` path as a query value, like `VaultRoute`).
        #[route("/mealplan/recipe?:path")]
        RecipeCookRoute { path: String },

        // Edit a recipe's cooklang source.
        #[route("/mealplan/recipe/edit?:path")]
        RecipeEditRoute { path: String },

        #[route("/schedule")]
        ScheduleRoute {},

        #[route("/bookings")]
        BookingsRoute {},

        #[route("/gantt")]
        GanttRoute {},

        #[route("/timer")]
        TimerRoute {},

        #[route("/finances")]
        FinancesRoute {},

        #[route("/invoices")]
        InvoicesRoute {},

        #[route("/ledger")]
        LedgerRoute {},

        #[route("/repos")]
        ReposRoute {},

        #[route("/wiki")]
        WikiRoute {},

        #[route("/wiki/sources")]
        WikiSourcesRoute {},

        #[route("/wiki/source/:name")]
        WikiSourceRoute { name: String },

        #[route("/agents")]
        AgentsRoute {},

        #[route("/settings")]
        SettingsRoute {},
}

#[component]
fn HomeRoute() -> Element {
    rsx! { pages::home::HomeView {} }
}

#[component]
fn InboxRoute() -> Element {
    rsx! { pages::inbox::InboxView {} }
}

#[component]
fn EmailRoute() -> Element {
    rsx! { pages::email::EmailView {} }
}

#[component]
fn ProjectsRoute() -> Element {
    rsx! { pages::projects::ProjectsView {} }
}

#[component]
fn ProjectDetailRoute(id: String) -> Element {
    rsx! { pages::project_detail::ProjectDetailView { id } }
}

#[component]
fn GoalsRoute() -> Element {
    rsx! { pages::goals::GoalsView {} }
}

#[component]
fn TasksRoute() -> Element {
    rsx! { pages::tasks::TasksView {} }
}

#[component]
fn TaskDetailRoute(id: uuid::Uuid) -> Element {
    rsx! { pages::task_detail::TaskDetailPage { id } }
}

#[component]
fn VaultRoute(path: String) -> Element {
    rsx! { pages::vault::VaultView { initial_path: path } }
}

#[component]
fn LocationsRoute() -> Element {
    rsx! { pages::locations::LocationsView {} }
}

#[component]
fn InventoryRoute() -> Element {
    rsx! { pages::inventory::InventoryView {} }
}

#[component]
fn ScriptureRoute() -> Element {
    rsx! { pages::scripture::ScriptureView {} }
}

#[component]
fn MilestonesRoute() -> Element {
    rsx! { pages::milestones::MilestonesView {} }
}

#[component]
fn FitnessRoute() -> Element {
    rsx! { pages::fitness::FitnessView {} }
}

#[component]
fn MealplanRoute() -> Element {
    rsx! { pages::mealplan::MealplanView {} }
}

#[component]
fn RecipeCookRoute(path: String) -> Element {
    rsx! { pages::cook_mode::RecipeCookView { path } }
}

#[component]
fn RecipeEditRoute(path: String) -> Element {
    rsx! { pages::recipe_edit::EditRecipeView { path } }
}

#[component]
fn ScheduleRoute() -> Element {
    rsx! { pages::schedule::ScheduleView {} }
}

#[component]
fn BookingsRoute() -> Element {
    rsx! { pages::bookings::BookingsView {} }
}

#[component]
fn GanttRoute() -> Element {
    rsx! { pages::gantt::GanttView {} }
}

#[component]
fn TimerRoute() -> Element {
    rsx! { pages::timer::TimerView {} }
}

#[component]
fn FinancesRoute() -> Element {
    rsx! { pages::finances::FinancesView {} }
}

#[component]
fn InvoicesRoute() -> Element {
    rsx! { pages::invoices::InvoicesView {} }
}

#[component]
fn LedgerRoute() -> Element {
    rsx! { pages::ledger::LedgerView {} }
}

#[component]
fn ReposRoute() -> Element {
    rsx! { pages::repos::ReposView {} }
}

#[component]
fn WikiRoute() -> Element {
    rsx! { pages::wiki::WikiView {} }
}

#[component]
fn WikiSourcesRoute() -> Element {
    rsx! { pages::wiki_source::WikiSourcesView {} }
}

#[component]
fn WikiSourceRoute(name: String) -> Element {
    rsx! { pages::wiki_source::WikiSourceView { name } }
}

#[component]
fn AgentsRoute() -> Element {
    rsx! { pages::agents::AgentsView {} }
}

#[component]
fn SettingsRoute() -> Element {
    rsx! { pages::settings::SettingsView {} }
}
