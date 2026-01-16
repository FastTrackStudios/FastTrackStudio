//! Windows-specific window utilities
//!
//! Uses Win32 APIs for overlay windows, window finding, and rect queries.

use crate::reaper::hwnd::RawHwnd;
use super::{OverlayConfig, WindowRect};

/// Configure a window for overlay mode.
pub fn configure_overlay(_hwnd: RawHwnd, _config: &OverlayConfig) {
    // TODO: Implement using Win32 APIs
    // - SetWindowLongPtr with WS_EX_LAYERED | WS_EX_TRANSPARENT for click-through
    // - SetLayeredWindowAttributes for transparency
    // - SetWindowPos for z-order
    log::warn!("Windows overlay configuration not yet implemented");
}

/// Find a child window by ID using GetDlgItem.
pub fn find_child_by_id(_parent: RawHwnd, _child_id: u32) -> Option<RawHwnd> {
    // TODO: Implement using GetDlgItem or EnumChildWindows
    log::warn!("Windows find_child_by_id not yet implemented");
    None
}

/// Get the screen-coordinate bounds of a window.
pub fn get_window_rect(_hwnd: RawHwnd) -> Option<WindowRect> {
    // TODO: Implement using GetWindowRect
    log::warn!("Windows get_window_rect not yet implemented");
    None
}

/// Create a new overlay window.
pub fn create_overlay_window(_x: i32, _y: i32, _width: u32, _height: u32) -> Option<RawHwnd> {
    // TODO: Implement using CreateWindowEx with WS_EX_LAYERED | WS_EX_TRANSPARENT
    log::warn!("Windows create_overlay_window not yet implemented");
    None
}

/// Show or hide a window.
pub fn show_window(_hwnd: RawHwnd, _show: bool) {
    // TODO: Implement using ShowWindow
    log::warn!("Windows show_window not yet implemented");
}

/// Set the position and size of a window.
pub fn set_window_frame(_hwnd: RawHwnd, _x: i32, _y: i32, _width: u32, _height: u32) {
    // TODO: Implement using SetWindowPos
    log::warn!("Windows set_window_frame not yet implemented");
}

/// Close a window.
pub fn close_window(_hwnd: RawHwnd) {
    // TODO: Implement using DestroyWindow
    log::warn!("Windows close_window not yet implemented");
}
