//! Docked window implementation
//!
//! Window integrated into REAPER's 16-dock system with screenset persistence.

use baseview::{Size, Window, WindowHandle, WindowOpenOptions};
use raw_window_handle::HasWindowHandle;
use reaper_low::Reaper;

use crate::embed_source::EmbedSource;
use crate::reaper::docker::{simple_screenset_callback, DockController};
use crate::reaper::hwnd::{extract_hwnd, to_reaper_hwnd};
use crate::window_handler::ReaperWindowHandler;

/// A docked window embedded in REAPER.
///
/// This window integrates with REAPER's dock system, allowing it to be
/// docked alongside other REAPER windows (e.g., mixer, FX browser).
///
/// # Example
///
/// ```ignore
/// use reaper_embed::{DockedWindow, VelloEmbedSource};
///
/// let source = VelloEmbedSource::new(|scene, w, h| {
///     // Draw content
/// });
///
/// let window = DockedWindow::open(
///     &reaper,
///     reaper.get_main_hwnd(),
///     source,
///     "my_dock_id",
///     "My Window",
///     (300, 400),
///     0, // Initial dock position (left)
/// );
/// ```
pub struct DockedWindow {
    handle: WindowHandle,
    dock_controller: Option<DockController>,
}

impl DockedWindow {
    /// Open a new docked window.
    ///
    /// # Arguments
    ///
    /// * `reaper` - Reference to the REAPER low-level API
    /// * `parent` - The parent window (typically REAPER's main window)
    /// * `source` - The content source to render
    /// * `dock_id` - Unique identifier for screenset persistence
    /// * `name` - Window name displayed in dock tabs
    /// * `size` - Initial window size
    /// * `initial_dock` - Initial dock position (0-15, or -1 for floating initially)
    ///
    /// # Returns
    ///
    /// A `DockedWindow` handle that can be used to control the window.
    pub fn open<P, S>(
        reaper: &Reaper,
        parent: &P,
        source: S,
        dock_id: &str,
        name: &str,
        size: (u32, u32),
        initial_dock: i32,
    ) -> Self
    where
        P: HasWindowHandle,
        S: EmbedSource,
    {
        let options = WindowOpenOptions {
            title: name.to_string(),
            size: Size::new(f64::from(size.0), f64::from(size.1)),
            scale: baseview::WindowScalePolicy::SystemScaleFactor,
        };

        let (width, height) = size;

        let handle = Window::open_parented(parent, options, move |_window| {
            ReaperWindowHandler::new(source, width, height)
        });

        // Extract the HWND from the WindowHandle (which implements HasWindowHandle)
        let dock_controller = if let Some(hwnd) = extract_hwnd(&handle) {
            log::debug!(
                "Extracted HWND for docked window '{}' with dock_id '{}'",
                name,
                dock_id
            );

            let mut controller = DockController::new(hwnd, dock_id, name);

            // Register for screenset persistence
            let hwnd_ptr = to_reaper_hwnd(hwnd);
            controller.register_screenset(reaper, simple_screenset_callback, hwnd_ptr);

            // Dock the window if requested
            if initial_dock >= 0 {
                controller.dock(reaper, initial_dock);
            }

            Some(controller)
        } else {
            log::warn!(
                "Failed to extract HWND for docked window '{}'. Docking will not work.",
                name
            );
            None
        };

        Self {
            handle,
            dock_controller,
        }
    }

    /// Set up docking with a known HWND.
    ///
    /// This is useful if automatic HWND extraction failed, or if you need
    /// to set up docking with a different HWND.
    ///
    /// # Arguments
    ///
    /// * `reaper` - Reference to the REAPER low-level API
    /// * `hwnd` - The native window handle
    /// * `dock_id` - Unique identifier for screenset persistence
    /// * `name` - Window name displayed in dock tabs
    /// * `dock_position` - Dock index (0-15) or -1 for floating
    pub fn setup_docking(
        &mut self,
        reaper: &Reaper,
        hwnd: crate::RawHwnd,
        dock_id: &str,
        name: &str,
        dock_position: i32,
    ) {
        let mut controller = DockController::new(hwnd, dock_id, name);

        // Register for screenset persistence
        let hwnd_ptr = to_reaper_hwnd(hwnd);
        controller.register_screenset(reaper, simple_screenset_callback, hwnd_ptr);

        // Dock the window
        if dock_position >= 0 {
            controller.dock(reaper, dock_position);
        }

        self.dock_controller = Some(controller);
    }

    /// Dock the window to a specific dock position.
    ///
    /// # Arguments
    ///
    /// * `reaper` - Reference to the REAPER low-level API
    /// * `dock_position` - Dock index (0-15)
    pub fn dock(&mut self, reaper: &Reaper, dock_position: i32) {
        if let Some(ref mut controller) = self.dock_controller {
            controller.dock(reaper, dock_position);
        }
    }

    /// Undock the window (make floating).
    ///
    /// # Arguments
    ///
    /// * `reaper` - Reference to the REAPER low-level API
    pub fn undock(&mut self, reaper: &Reaper) {
        if let Some(ref mut controller) = self.dock_controller {
            controller.undock(reaper);
        }
    }

    /// Toggle docked/floating state.
    ///
    /// # Arguments
    ///
    /// * `reaper` - Reference to the REAPER low-level API
    pub fn toggle_dock(&mut self, reaper: &Reaper) {
        if let Some(ref mut controller) = self.dock_controller {
            controller.toggle(reaper);
        }
    }

    /// Check if the window is currently docked.
    ///
    /// # Arguments
    ///
    /// * `reaper` - Reference to the REAPER low-level API
    pub fn is_docked(&self, reaper: &Reaper) -> bool {
        self.dock_controller
            .as_ref()
            .map(|c| c.is_docked(reaper))
            .unwrap_or(false)
    }

    /// Activate the docked window (bring to front).
    ///
    /// # Arguments
    ///
    /// * `reaper` - Reference to the REAPER low-level API
    pub fn activate(&self, reaper: &Reaper) {
        if let Some(ref controller) = self.dock_controller {
            controller.activate(reaper);
        }
    }

    /// Close the window.
    pub fn close(&mut self, reaper: &Reaper) {
        // Unregister from screenset before closing
        if let Some(ref controller) = self.dock_controller {
            controller.unregister_screenset(reaper);
        }

        self.handle.close();
        self.dock_controller = None;
    }

    /// Check if the window is still open.
    pub fn is_open(&self) -> bool {
        self.handle.is_open()
    }

    /// Get the current dock position.
    ///
    /// Returns -1 if floating, 0-15 for dock index, or None if docking
    /// hasn't been set up.
    pub fn current_dock(&self) -> Option<i32> {
        self.dock_controller.as_ref().map(|c| c.current_dock())
    }
}
