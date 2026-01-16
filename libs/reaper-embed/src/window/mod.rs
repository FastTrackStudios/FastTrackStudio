//! Window management for REAPER embedding
//!
//! Provides the high-level API for creating and managing embedded windows.

mod docked;
mod floating;
mod overlay;
mod transparent;

pub use docked::DockedWindow;
pub use floating::ReaperWindow;
pub use overlay::OverlayWindow;
pub use transparent::TransparentWindow;

/// Window mode for REAPER embedding.
#[derive(Debug, Clone)]
pub enum WindowMode {
    /// Floating window parented to REAPER main window.
    Floating,

    /// Docked window in REAPER's dock system.
    Docked {
        /// Unique identifier for screenset persistence.
        dock_id: &'static str,
        /// Initial dock position (0-15, or -1 for floating initially).
        initial_dock: i32,
    },

    /// Transparent overlay drawn over a REAPER window.
    Overlay {
        /// Target window to overlay (e.g., arrange view).
        target: OverlayTarget,
        /// Whether clicks pass through to the underlying window.
        pass_through: bool,
    },
}

impl Default for WindowMode {
    fn default() -> Self {
        Self::Floating
    }
}

/// Target windows for overlay mode.
#[derive(Debug, Clone, Copy)]
pub enum OverlayTarget {
    /// Main REAPER window.
    MainWindow,
    /// Arrange/track view (child ID 0x3E8 = 1000).
    ArrangeView,
    /// Mixer window.
    Mixer,
    /// Custom child window by ID.
    ChildById(u32),
}

/// Configuration for creating a REAPER window.
#[derive(Debug, Clone)]
pub struct WindowConfig {
    /// Window title.
    pub title: String,
    /// Initial window size in logical pixels.
    pub size: (u32, u32),
    /// Window mode (floating, docked, or overlay).
    pub mode: WindowMode,
    /// Whether the window can be resized.
    pub resizable: bool,
}

impl Default for WindowConfig {
    fn default() -> Self {
        Self {
            title: "REAPER Window".into(),
            size: (800, 600),
            mode: WindowMode::Floating,
            resizable: true,
        }
    }
}
