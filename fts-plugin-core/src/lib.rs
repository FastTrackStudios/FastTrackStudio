//! FTS Plugin Core - Abstraction layer for FastTrackStudio audio plugins.
//!
//! Provides a unified interface for building FTS audio plugins with
//! consistent UI, state management, and plugin configuration.
//!
//! # Usage
//!
//! ```ignore
//! use fts_plugin_core::prelude::*;  // nih_plug + nih_plug_dioxus re-exports
//! use fts_plugin_core::ui::prelude::*;  // FTS UI components (Toggle, Section, etc.)
//! ```

// Re-export core dependencies
pub use nih_plug;
pub use nih_plug::prelude::*;

#[cfg(feature = "gui")]
pub use nih_plug_dioxus;
#[cfg(feature = "gui")]
pub use nih_plug_dioxus::prelude::*;

#[cfg(feature = "gui")]
pub mod ui;

/// Prelude for convenient imports.
pub mod prelude {
    pub use nih_plug::prelude::*;

    #[cfg(feature = "gui")]
    pub use nih_plug_dioxus::prelude::*;
}

/// Standard window size for FTS plugins (16:9 aspect ratio).
pub const DEFAULT_WINDOW_SIZE: (u32, u32) = (640, 360);

/// Create a standard editor state with FTS defaults.
#[cfg(feature = "gui")]
#[must_use]
pub fn default_editor_state() -> std::sync::Arc<DioxusState> {
    DioxusState::new(|| DEFAULT_WINDOW_SIZE)
}
