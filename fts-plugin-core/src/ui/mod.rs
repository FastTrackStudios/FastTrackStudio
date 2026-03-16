//! UI module for FTS plugins — Blitz-compatible components and theme.
//!
//! All components use inline styles for reliable rendering in the
//! nih_plug_dioxus Blitz renderer. Import everything via the prelude:
//!
//! ```ignore
//! use fts_plugin_core::ui::prelude::*;
//! ```

pub mod components;
pub mod theme;

/// UI prelude — import this for all FTS plugin UI building blocks.
pub mod prelude {
    pub use super::components::*;
    pub use super::theme;
}
