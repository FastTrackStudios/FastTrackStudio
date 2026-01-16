//! Floating window implementation
//!
//! Standard child window parented to REAPER's main window.

use baseview::{Size, Window, WindowHandle, WindowOpenOptions};
use raw_window_handle::HasWindowHandle;

use crate::embed_source::EmbedSource;
use crate::window::WindowConfig;
use crate::window_handler::ReaperWindowHandler;

/// A window embedded in REAPER.
///
/// This is the main API for creating embedded windows. Currently supports
/// floating windows, with docked and overlay modes planned.
///
/// # Example
///
/// ```ignore
/// use reaper_embed::{ReaperWindow, WindowConfig, VelloEmbedSource};
///
/// let source = VelloEmbedSource::new(|scene, w, h| {
///     // Draw content
/// });
///
/// let window = ReaperWindow::open(
///     reaper.get_main_hwnd(),
///     source,
///     WindowConfig {
///         title: "My Window".into(),
///         size: (800, 600),
///         ..Default::default()
///     },
/// );
/// ```
pub struct ReaperWindow {
    handle: WindowHandle,
}

impl ReaperWindow {
    /// Open a new window as a child of a REAPER window.
    ///
    /// # Arguments
    ///
    /// * `parent` - The parent window (typically REAPER's main window)
    /// * `source` - The content source to render
    /// * `config` - Window configuration
    ///
    /// # Returns
    ///
    /// A `ReaperWindow` handle that can be used to control the window.
    pub fn open<P, S>(parent: &P, source: S, config: WindowConfig) -> Self
    where
        P: HasWindowHandle,
        S: EmbedSource,
    {
        let options = WindowOpenOptions {
            title: config.title,
            size: Size::new(f64::from(config.size.0), f64::from(config.size.1)),
            scale: baseview::WindowScalePolicy::SystemScaleFactor,
        };

        let width = config.size.0;
        let height = config.size.1;

        let handle = Window::open_parented(parent, options, move |_window| {
            ReaperWindowHandler::new(source, width, height)
        });

        Self { handle }
    }

    /// Close the window.
    pub fn close(&mut self) {
        self.handle.close();
    }

    /// Check if the window is still open.
    pub fn is_open(&self) -> bool {
        self.handle.is_open()
    }
}
