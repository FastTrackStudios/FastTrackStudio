//! REAPER UI Embedding Module
//!
//! This module provides infrastructure for embedding Dioxus views and Vello scenes
//! into REAPER windows using BaseView for cross-platform windowing.
//!
//! # Window Modes
//!
//! The module supports three window modes:
//!
//! - **Floating**: Standard child window parented to REAPER's main window
//! - **Docked**: Window integrated into REAPER's 16-dock system
//! - **Overlay**: Transparent window that draws over existing REAPER windows
//!
//! # Example
//!
//! ```ignore
//! use reaper_embed::{ReaperWindow, WindowConfig, WindowMode, VelloEmbedSource};
//!
//! let source = VelloEmbedSource::new(|scene, w, h| {
//!     // Draw content using Vello
//! });
//!
//! let window = ReaperWindow::open(
//!     reaper.get_main_hwnd(),
//!     source,
//!     WindowConfig {
//!         title: "Chart Editor".into(),
//!         size: (800, 600),
//!         mode: WindowMode::Floating,
//!         ..Default::default()
//!     },
//! );
//! ```

mod embed_source;
mod gpu;
mod window_handler;

pub mod platform;
pub mod reaper;
pub mod sources;
pub mod window;

// Re-export main types
pub use embed_source::EmbedSource;
pub use gpu::{GpuError, GpuState};
pub use reaper::docker::{DockController, ScreensetAction, ScreensetCallback};
pub use reaper::hwnd::{extract_hwnd, to_reaper_hwnd, RawHwnd};
pub use sources::vello::{VelloEmbedSource, VelloEmbedSourceWithEvents};
pub use platform::{OverlayConfig, WindowRect};
pub use window::{DockedWindow, OverlayTarget, OverlayWindow, ReaperWindow, TransparentWindow, WindowConfig, WindowMode};
pub use window_handler::ReaperWindowHandler;

// Re-export baseview event types for event handling
pub use baseview::{Event, EventStatus, MouseButton, MouseEvent, ScrollDelta};

#[cfg(feature = "dioxus")]
pub use sources::dioxus::DioxusEmbedSource;
