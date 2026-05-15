//! Toolbar — types + service trait.

mod service;
mod types;

pub use service::{Toolbar, ToolbarRpc};
pub use types::{
    ToolbarButton, ToolbarIcon, ToolbarIconKind, ToolbarItemInfo, ToolbarPlacement, ToolbarResult,
    ToolbarSnapshot, ToolbarSnapshotSource, ToolbarTarget, TrackedButton,
};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, ToolbarClient, descriptor, serve};
