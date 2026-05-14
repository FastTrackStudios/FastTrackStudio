//! Dock host — platform-portable dock/window management.
//!
//! Abstracts over the OS-level container hosting UI panels (REAPER
//! docker, standalone native window, browser DOM, mock for tests).

mod service;
mod types;

pub use service::{
    DockHostService, DockHostServiceClient, DockHostServiceDispatcher,
    dock_host_service_service_descriptor,
};
pub use types::{DockEvent, DockHandle, DockKind, PanelPixels, UiEventDto};
