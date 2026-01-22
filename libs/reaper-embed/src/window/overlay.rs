//! Overlay window implementation
//!
//! Transparent window that draws over existing REAPER windows without capturing input.
//! Inspired by the TK_Trackname_in_Arrange Lua script pattern.

#![allow(unused_imports)]

use vello::Scene;

use crate::embed_source::EmbedSource;
use crate::gpu::{GpuError, GpuState};
use crate::platform::{self, WindowRect};
use crate::reaper::hwnd::RawHwnd;
use crate::window::{OverlayTarget, WindowMode};

/// A transparent overlay window that draws over REAPER windows.
///
/// This window is positioned over a target REAPER window (e.g., the arrange view)
/// and renders content using Vello. Mouse events pass through to the underlying
/// REAPER window.
///
/// # Example
///
/// ```ignore
/// use reaper_embed::{OverlayWindow, VelloEmbedSource, OverlayTarget};
///
/// // Create an overlay over the arrange view
/// let overlay = OverlayWindow::open(
///     main_hwnd,
///     OverlayTarget::ArrangeView,
///     VelloEmbedSource::new(|scene, w, h| {
///         // Draw track names or other overlays
///     }),
/// );
///
/// // Call this periodically to keep the overlay in sync
/// overlay.sync_bounds();
/// ```
pub struct OverlayWindow<S: EmbedSource> {
    /// The content source for rendering
    source: S,
    /// Target window to overlay
    target: OverlayTarget,
    /// The native overlay window handle
    hwnd: Option<RawHwnd>,
    /// The target window handle (for syncing bounds)
    target_hwnd: Option<RawHwnd>,
    /// GPU state for Vello rendering
    gpu: Option<GpuState>,
    /// Current window bounds
    bounds: WindowRect,
    /// Whether the overlay is visible
    visible: bool,
}

impl<S: EmbedSource> OverlayWindow<S> {
    /// Open a new overlay window over a REAPER window.
    ///
    /// # Arguments
    ///
    /// * `main_hwnd` - REAPER's main window handle
    /// * `target` - The target window to overlay
    /// * `source` - The content source to render
    ///
    /// # Returns
    ///
    /// An `OverlayWindow` handle, or an error if creation failed.
    pub fn open(main_hwnd: RawHwnd, target: OverlayTarget, source: S) -> Result<Self, GpuError> {
        // Find the target window
        let target_hwnd = Self::find_target_window(main_hwnd, &target);

        // Get initial bounds from target
        let bounds = target_hwnd
            .and_then(platform::get_window_rect)
            .unwrap_or(WindowRect::new(100, 100, 800, 600));

        // Create the overlay window
        let hwnd = platform::create_overlay_window(bounds.x, bounds.y, bounds.width, bounds.height);

        let mut overlay = Self {
            source,
            target,
            hwnd,
            target_hwnd,
            gpu: None,
            bounds,
            visible: false,
        };

        // Initialize GPU if window was created
        if let Some(hwnd) = overlay.hwnd {
            overlay.init_gpu(hwnd)?;
        }

        Ok(overlay)
    }

    /// Find the target window based on the overlay target.
    fn find_target_window(main_hwnd: RawHwnd, target: &OverlayTarget) -> Option<RawHwnd> {
        match target {
            OverlayTarget::MainWindow => Some(main_hwnd),
            OverlayTarget::ArrangeView => platform::find_child_by_id(
                main_hwnd,
                crate::reaper::windows::child_ids::ARRANGE_VIEW,
            ),
            OverlayTarget::Mixer => {
                // Mixer window ID would need to be discovered
                log::warn!("Mixer overlay target not yet implemented");
                None
            }
            OverlayTarget::ChildById(id) => platform::find_child_by_id(main_hwnd, *id),
        }
    }

    /// Initialize GPU rendering for the overlay.
    fn init_gpu(&mut self, _hwnd: RawHwnd) -> Result<(), GpuError> {
        // For now, we'll skip GPU init since we need a proper window handle
        // that implements HasWindowHandle. Native windows need special handling.
        //
        // TODO: Implement proper GPU surface creation for native windows.
        // This requires either:
        // 1. Wrapping the native window in a HasWindowHandle impl
        // 2. Using wgpu's raw window handle APIs directly
        log::warn!("GPU rendering for overlays not yet implemented");
        Ok(())
    }

    /// Show the overlay window.
    pub fn show(&mut self) {
        if let Some(hwnd) = self.hwnd {
            platform::show_window(hwnd, true);
            self.visible = true;
        }
    }

    /// Hide the overlay window.
    pub fn hide(&mut self) {
        if let Some(hwnd) = self.hwnd {
            platform::show_window(hwnd, false);
            self.visible = false;
        }
    }

    /// Check if the overlay is visible.
    pub fn is_visible(&self) -> bool {
        self.visible
    }

    /// Toggle overlay visibility.
    pub fn toggle(&mut self) {
        if self.visible {
            self.hide();
        } else {
            self.show();
        }
    }

    /// Sync the overlay position and size with the target window.
    ///
    /// Call this periodically (e.g., on REAPER's refresh timer) to keep
    /// the overlay positioned correctly when the target window moves or resizes.
    pub fn sync_bounds(&mut self) {
        let Some(target_hwnd) = self.target_hwnd else {
            return;
        };

        let Some(new_bounds) = platform::get_window_rect(target_hwnd) else {
            return;
        };

        // Only update if bounds changed
        if new_bounds.x != self.bounds.x
            || new_bounds.y != self.bounds.y
            || new_bounds.width != self.bounds.width
            || new_bounds.height != self.bounds.height
        {
            self.bounds = new_bounds;

            if let Some(hwnd) = self.hwnd {
                platform::set_window_frame(
                    hwnd,
                    new_bounds.x,
                    new_bounds.y,
                    new_bounds.width,
                    new_bounds.height,
                );

                // TODO: Resize GPU surface
            }
        }
    }

    /// Render the overlay content.
    ///
    /// Call this to redraw the overlay. Should be called after sync_bounds()
    /// or when content needs updating.
    pub fn render(&mut self) {
        if !self.visible {
            return;
        }

        // Build the scene
        let mut scene = Scene::new();
        self.source
            .render(&mut scene, self.bounds.width, self.bounds.height);

        // TODO: Render with GPU when implemented
        // if let Some(ref mut gpu) = self.gpu {
        //     if let Err(e) = gpu.render(&scene) {
        //         log::error!("Overlay render failed: {}", e);
        //     }
        // }
    }

    /// Get the current bounds of the overlay.
    pub fn bounds(&self) -> &WindowRect {
        &self.bounds
    }

    /// Get the target window handle.
    pub fn target_hwnd(&self) -> Option<RawHwnd> {
        self.target_hwnd
    }

    /// Close the overlay window.
    pub fn close(&mut self) {
        if let Some(hwnd) = self.hwnd.take() {
            platform::close_window(hwnd);
        }
        self.gpu = None;
        self.visible = false;
    }
}

impl<S: EmbedSource> Drop for OverlayWindow<S> {
    fn drop(&mut self) {
        self.close();
    }
}

/// Known REAPER child window IDs for overlay targets.
pub mod child_ids {
    /// Arrange/track view (main editing area) - ID 0x3E8 (1000)
    pub const ARRANGE_VIEW: u32 = 0x3E8;

    /// Transport bar (estimated, needs verification)
    pub const TRANSPORT: u32 = 0x3ED;

    /// Mixer panel (estimated, needs verification)
    pub const MIXER: u32 = 0x3EC;
}
