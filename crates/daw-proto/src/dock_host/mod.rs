//! Dock host — platform-portable dock/window management.

mod service;
mod types;

pub use service::{DockHosting, DockHostingRpc};
pub use types::{DockEvent, DockHandle, DockKind, PanelPixels, UiEventDto};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, DockHostingClient, descriptor, layer, serve};
