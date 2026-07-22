//! Complete application UI shell.
//!
//! Hosts the router, sidebar, mobile chrome, and every top-level
//! page. Feature crates (under `features/*/x-ui`) provide
//! reusable components; this crate composes them into the
//! product surface.

pub mod actions;
pub mod app;
pub mod app_views;
pub mod auth;
pub mod chrome;
pub mod collab;
pub mod document_session;
pub mod feeds;
pub mod forge_views;
pub mod fuzzy;
pub mod gantt_adapt;
pub mod nav;
pub mod orgs;
pub mod pages;
pub mod palette;
pub mod prefs;
pub mod presence;
pub mod routes;
pub mod server_registry;
// Stage 4a in-browser session engine (wasm-only): builds a headless
// daw-standalone setlist player in the tab via architect's in-process
// LocalServer. Parked at boot; the UI bridge is dormant (STAGE_4B).
#[cfg(target_arch = "wasm32")]
pub mod session_engine;
pub mod shell;
pub mod shortcuts;
pub mod states;
pub mod stores;
pub mod tabs;
pub mod tag_icon;
pub mod task_sort;
pub mod theming;
pub mod vault_lookup;
pub mod vox_clients;
pub mod vox_session;

pub use app::App;
pub use routes::Route;
