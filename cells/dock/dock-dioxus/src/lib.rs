//! Dock Dioxus — modular docking layout UI components.
//!
//! Provides reactive components for rendering and manipulating dock layouts
//! defined by [`dock_proto`].

pub mod components;
pub mod context;
pub mod hooks;
pub mod prelude;
pub mod signals;

// Re-export public API
pub use components::{DockRoot, DockTabBar, PanelHeader, PresetBar, SplitPane, TilePane};
pub use context::{DockContext, DockProvider, PanelRenderer};
pub use hooks::{init_dock_presets, use_dock_actions, DockActions};
pub use signals::*;
