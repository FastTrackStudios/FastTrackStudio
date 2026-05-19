//! WindowManager — named workspace layouts (toolbars + docking).

mod service;
mod types;

pub use service::{WindowManager, WindowManagerRpc};
pub use types::{
    LayoutPlacement, LayoutToolbar, ModeDockerLayout, MonitorRect, WindowLayout,
    WindowLayoutOptions, WindowLayoutResult, WindowLayoutSummary,
};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, Service, WindowManagerClient, descriptor, layer, serve};
