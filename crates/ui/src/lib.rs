//! Complete application UI shell.
//!
//! Hosts the router, sidebar, mobile chrome, and every top-level
//! page. Feature crates (under `features/*/x-ui`) provide
//! reusable components; this crate composes them into the
//! product surface.

pub mod app;
pub mod auth;
pub mod chrome;
pub mod collab;
pub mod document_session;
pub mod feeds;
pub mod forge_views;
pub mod gantt_adapt;
pub mod nav;
pub mod orgs;
pub mod pages;
pub mod prefs;
pub mod presence;
pub mod routes;
pub mod server_registry;
pub mod shell;
pub mod states;
pub mod stores;
pub mod tag_icon;
pub mod task_sort;
pub mod theming;
pub mod vault_lookup;
pub mod vox_clients;
pub mod vox_session;

pub use app::App;
pub use routes::Route;
