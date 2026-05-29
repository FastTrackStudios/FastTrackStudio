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

        #[route("/projects")]
        ProjectsRoute {},

        #[route("/projects/:id")]
        ProjectDetailRoute { id: String },

        #[route("/goals")]
        GoalsRoute {},

        #[route("/tasks")]
        TasksRoute {},

        #[route("/vault")]
        VaultRoute {},

        #[route("/schedule")]
        ScheduleRoute {},

        #[route("/gantt")]
        GanttRoute {},

        #[route("/wiki")]
        WikiRoute {},

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
fn VaultRoute() -> Element {
    rsx! { pages::vault::VaultView {} }
}

#[component]
fn ScheduleRoute() -> Element {
    rsx! { pages::schedule::ScheduleView {} }
}

#[component]
fn GanttRoute() -> Element {
    rsx! { pages::gantt::GanttView {} }
}

#[component]
fn WikiRoute() -> Element {
    rsx! { pages::wiki::WikiView {} }
}

#[component]
fn SettingsRoute() -> Element {
    rsx! { pages::settings::SettingsView {} }
}
