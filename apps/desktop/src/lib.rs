//! FastTrackStudio desktop/web-server shared library.
//!
//! The in-process WebSocket gateway, the REAPER connection/service wiring, and
//! the DAW-status + Tools UI components live here so they can be shared between
//! two binaries:
//!
//! - `fasttrackstudio-desktop` (`src/main.rs`) — the Dioxus desktop app.
//! - `fasttrackstudio-web-server` (`src/bin/web-server.rs`) — a headless server
//!   that connects to fts-extensions and serves the WASM webapp + its WS
//!   backend, so the web version runs without the desktop app.

pub mod daw_status;
pub mod gateway;
pub mod services;
pub mod tools;
