//! Toolbar — types + service traits.

mod service;
mod types;

pub use service::{
    Toolbar, ToolbarService, ToolbarServiceClient, ToolbarServiceDispatcher,
    toolbar_service_service_descriptor,
};
pub use types::{
    ToolbarButton, ToolbarIcon, ToolbarIconKind, ToolbarItemInfo, ToolbarPlacement, ToolbarResult,
    ToolbarSnapshot, ToolbarSnapshotSource, ToolbarTarget, TrackedButton,
};
