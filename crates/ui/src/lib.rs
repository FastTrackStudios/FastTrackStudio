//! Complete application UI shell.
//!
//! Hosts the router, sidebar, mobile chrome, and every top-level
//! page. Feature crates (under `features/*/x-ui`) provide
//! reusable components; this crate composes them into the
//! product surface.

pub mod app;
pub mod chrome;
pub mod data;
pub mod feeds;
pub mod forge_views;
pub mod gantt_adapt;
pub mod nav;
pub mod optimistic;
pub mod orgs;
pub mod pages;
pub mod routes;
pub mod server_registry;
pub mod shell;
pub mod task_wiring;
pub mod theming;
pub mod vox_clients;
pub mod vox_session;

pub use app::App;
pub use routes::Route;
