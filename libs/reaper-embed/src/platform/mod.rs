//! Platform-specific window utilities
//!
//! Provides platform-specific functionality for overlay windows,
//! window finding, and rect queries.

#[cfg(target_os = "macos")]
pub mod macos;

#[cfg(target_os = "windows")]
pub mod windows;

#[cfg(target_os = "linux")]
pub mod linux;

use crate::reaper::hwnd::RawHwnd;

/// Configure a window for overlay mode (transparent, click-through).
///
/// # Arguments
///
/// * `hwnd` - The native window handle (NSView* on macOS, HWND on Windows)
/// * `config` - Overlay configuration options
///
/// # Safety
///
/// The hwnd must be a valid window handle.
pub fn configure_overlay(hwnd: RawHwnd, config: &OverlayConfig) {
    #[cfg(target_os = "macos")]
    macos::configure_overlay(hwnd, config);

    #[cfg(target_os = "windows")]
    windows::configure_overlay(hwnd, config);

    #[cfg(target_os = "linux")]
    linux::configure_overlay(hwnd, config);
}

/// Find a child window/view by ID.
///
/// On macOS, this searches for an NSView with a matching `tag`.
/// On Windows, this uses `GetDlgItem`.
///
/// # Arguments
///
/// * `parent` - The parent window handle
/// * `child_id` - The child window/view ID to find
///
/// # Returns
///
/// The child window handle if found, or `None`.
pub fn find_child_by_id(parent: RawHwnd, child_id: u32) -> Option<RawHwnd> {
    #[cfg(target_os = "macos")]
    return macos::find_child_by_id(parent, child_id);

    #[cfg(target_os = "windows")]
    return windows::find_child_by_id(parent, child_id);

    #[cfg(target_os = "linux")]
    return linux::find_child_by_id(parent, child_id);

    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "linux")))]
    {
        let _ = (parent, child_id);
        None
    }
}

/// Get the screen-coordinate bounds of a window.
///
/// # Arguments
///
/// * `hwnd` - The window handle
///
/// # Returns
///
/// A tuple of (x, y, width, height) in screen coordinates, or `None` if failed.
pub fn get_window_rect(hwnd: RawHwnd) -> Option<WindowRect> {
    #[cfg(target_os = "macos")]
    return macos::get_window_rect(hwnd);

    #[cfg(target_os = "windows")]
    return windows::get_window_rect(hwnd);

    #[cfg(target_os = "linux")]
    return linux::get_window_rect(hwnd);

    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "linux")))]
    {
        let _ = hwnd;
        None
    }
}

/// Create a new overlay window.
///
/// Creates a borderless, transparent window with click-through enabled.
///
/// # Arguments
///
/// * `x` - X position in screen coordinates
/// * `y` - Y position in screen coordinates (top-left origin)
/// * `width` - Window width
/// * `height` - Window height
///
/// # Returns
///
/// The window handle, or `None` if creation failed.
pub fn create_overlay_window(x: i32, y: i32, width: u32, height: u32) -> Option<RawHwnd> {
    #[cfg(target_os = "macos")]
    return macos::create_overlay_window(x, y, width, height);

    #[cfg(target_os = "windows")]
    return windows::create_overlay_window(x, y, width, height);

    #[cfg(target_os = "linux")]
    return linux::create_overlay_window(x, y, width, height);

    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "linux")))]
    {
        let _ = (x, y, width, height);
        None
    }
}

/// Show or hide a window.
pub fn show_window(hwnd: RawHwnd, show: bool) {
    #[cfg(target_os = "macos")]
    macos::show_window(hwnd, show);

    #[cfg(target_os = "windows")]
    windows::show_window(hwnd, show);

    #[cfg(target_os = "linux")]
    linux::show_window(hwnd, show);

    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "linux")))]
    let _ = (hwnd, show);
}

/// Set the position and size of a window.
pub fn set_window_frame(hwnd: RawHwnd, x: i32, y: i32, width: u32, height: u32) {
    #[cfg(target_os = "macos")]
    macos::set_window_frame(hwnd, x, y, width, height);

    #[cfg(target_os = "windows")]
    windows::set_window_frame(hwnd, x, y, width, height);

    #[cfg(target_os = "linux")]
    linux::set_window_frame(hwnd, x, y, width, height);

    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "linux")))]
    let _ = (hwnd, x, y, width, height);
}

/// Close and release a window.
pub fn close_window(hwnd: RawHwnd) {
    #[cfg(target_os = "macos")]
    macos::close_window(hwnd);

    #[cfg(target_os = "windows")]
    windows::close_window(hwnd);

    #[cfg(target_os = "linux")]
    linux::close_window(hwnd);

    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "linux")))]
    let _ = hwnd;
}

/// Configuration options for overlay windows.
#[derive(Debug, Clone, Default)]
pub struct OverlayConfig {
    /// Whether the window background should be transparent.
    pub transparent: bool,
    /// Whether mouse events should pass through to windows below.
    pub click_through: bool,
    /// Whether to remove window decorations (title bar, borders).
    pub no_decorations: bool,
    /// Whether to remove the window shadow.
    pub no_shadow: bool,
    /// Whether the window should stay on top.
    pub topmost: bool,
}

impl OverlayConfig {
    /// Create a configuration for a fully transparent click-through overlay.
    pub fn transparent_overlay() -> Self {
        Self {
            transparent: true,
            click_through: true,
            no_decorations: true,
            no_shadow: true,
            topmost: false, // Usually we want it below REAPER's windows
        }
    }
}

/// Window rectangle in screen coordinates.
#[derive(Debug, Clone, Copy, Default)]
pub struct WindowRect {
    pub x: i32,
    pub y: i32,
    pub width: u32,
    pub height: u32,
}

impl WindowRect {
    pub fn new(x: i32, y: i32, width: u32, height: u32) -> Self {
        Self {
            x,
            y,
            width,
            height,
        }
    }
}
