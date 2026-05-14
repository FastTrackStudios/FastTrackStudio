//! Shared UI for Task — used by every platform app (web/desktop/mobile).
//!
//! Components are built on top of `fts-ui`. Platform crates only wire up
//! the renderer and asset pipeline; everything else lives here so views
//! stay identical across targets.

pub mod app;
pub mod data;
pub mod feature_routes;
pub mod server_registry;
pub mod theming;
pub mod vox_session;

pub use app::App;
