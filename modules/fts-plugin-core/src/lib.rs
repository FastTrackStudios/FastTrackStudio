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

/// Prelude for convenient imports.
pub mod prelude {
    pub use nih_plug::prelude::*;

    #[cfg(feature = "gui")]
    pub use nih_plug_dioxus::prelude::*;

    #[cfg(feature = "gui")]
    pub use crate::shell::{PluginShell, SIZE_PRESET_NAMES, SIZE_PRESETS, TOP_BAR_HEIGHT};

    #[cfg(feature = "hub")]
    pub use plugin_hub::{MeterData, ParamInfo, PluginInfo, PluginType};

    pub use crate::dsp::*;
    pub use crate::meters::*;
}

/// Standard window size for FTS plugins (16:9 aspect ratio).
pub const DEFAULT_WINDOW_SIZE: (u32, u32) = (640, 360);

/// Minimum window size for FTS plugins.
pub const MIN_WINDOW_SIZE: (u32, u32) = (320, 180);

/// Maximum logical window size (to stay within GPU texture limits).
pub const MAX_WINDOW_SIZE: (u32, u32) = (1920, 1080);

/// Create a standard editor state with FTS defaults.
#[cfg(feature = "gui")]
#[must_use]
pub fn default_editor_state() -> std::sync::Arc<DioxusState> {
    DioxusState::new(|| DEFAULT_WINDOW_SIZE)
}
