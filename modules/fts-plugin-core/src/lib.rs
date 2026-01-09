//! FTS Plugin Core - Abstraction layer for FastTrackStudio audio plugins.
//!
//! This crate provides a unified interface for building FTS audio plugins
//! with consistent UI, state management, and cross-plugin communication.
//!
//! # Features
//!
//! - **Unified Plugin Trait**: Common interface for all FTS plugins
//! - **Dioxus UI Integration**: Pre-configured UI components and styling
//! - **State Management**: Shared state between windowed and embedded editors
//! - **Hub Integration**: Optional cross-plugin communication via `plugin-hub`
//!
//! # Example
//!
//! ```ignore
//! use fts_plugin_core::prelude::*;
//!
//! #[derive(Params)]
//! struct MyParams {
//!     #[persist = "editor-state"]
//!     editor_state: Arc<DioxusState>,
//!
//!     #[id = "gain"]
//!     gain: FloatParam,
//! }
//!
//! struct MyPlugin {
//!     params: Arc<MyParams>,
//! }
//!
//! impl FtsPlugin for MyPlugin {
//!     type Params = MyParams;
//!     
//!     fn params(&self) -> Arc<Self::Params> {
//!         self.params.clone()
//!     }
//!     
//!     fn process_sample(&mut self, sample: f32) -> f32 {
//!         sample * self.params.gain.value()
//!     }
//! }
//! ```

// Re-export core dependencies
pub use nih_plug;
pub use nih_plug::prelude::*;

#[cfg(feature = "gui")]
pub use nih_plug_dioxus;
#[cfg(feature = "gui")]
pub use nih_plug_dioxus::prelude::*;

#[cfg(feature = "hub")]
pub use plugin_hub;

pub mod dsp;
pub mod meters;

#[cfg(feature = "gui")]
pub mod dropdown;
#[cfg(feature = "gui")]
pub mod shell;
#[cfg(feature = "gui")]
pub mod sizing;

/// Prelude for convenient imports.
pub mod prelude {
    pub use nih_plug::prelude::*;

    #[cfg(feature = "gui")]
    pub use nih_plug_dioxus::prelude::*;

    #[cfg(feature = "gui")]
    pub use crate::shell::{PluginShell, TOP_BAR_HEIGHT};

    #[cfg(feature = "gui")]
    pub use crate::sizing::{AspectRatio, SizeTier, calculate_size, default_window_size};

    #[cfg(feature = "hub")]
    pub use plugin_hub::{MeterData, ParamInfo, PluginInfo, PluginType};

    pub use crate::dsp::*;
    pub use crate::meters::*;
}

/// Standard window size for FTS plugins (16:9 aspect ratio).
#[cfg(feature = "gui")]
pub const DEFAULT_WINDOW_SIZE: (u32, u32) = sizing::AspectRatio::Widescreen.base_dimensions();
#[cfg(not(feature = "gui"))]
pub const DEFAULT_WINDOW_SIZE: (u32, u32) = (640, 360);

/// Minimum window size for FTS plugins.
#[cfg(feature = "gui")]
pub const MIN_WINDOW_SIZE: (u32, u32) = sizing::MIN_WINDOW_SIZE;
#[cfg(not(feature = "gui"))]
pub const MIN_WINDOW_SIZE: (u32, u32) = (200, 100);

/// Maximum logical window size (to stay within GPU texture limits).
#[cfg(feature = "gui")]
pub const MAX_WINDOW_SIZE: (u32, u32) = sizing::MAX_WINDOW_SIZE;
#[cfg(not(feature = "gui"))]
pub const MAX_WINDOW_SIZE: (u32, u32) = (1920, 1080);

/// Create a standard editor state with FTS defaults.
#[cfg(feature = "gui")]
#[must_use]
pub fn default_editor_state() -> std::sync::Arc<DioxusState> {
    DioxusState::new(sizing::default_window_size)
}
