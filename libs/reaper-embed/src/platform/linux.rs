//! Linux-specific window utilities
//!
//! Uses X11 APIs for overlay windows, window finding, and rect queries.

use crate::reaper::hwnd::RawHwnd;
use super::{OverlayConfig, WindowRect};

/// Configure a window for overlay mode.
pub fn configure_overlay(_hwnd: RawHwnd, _config: &OverlayConfig) {
    // TODO: Implement using X11 APIs
    // Note: Click-through on X11 is complex and compositor-dependent
    // May need XShape extension or compositor-specific hints
    log::warn!("Linux overlay configuration not yet implemented");
}

/// Find a child window by ID.
pub fn find_child_by_id(_parent: RawHwnd, _child_id: u32) -> Option<RawHwnd> {
    // TODO: Implement using XQueryTree
    log::warn!("Linux find_child_by_id not yet implemented");
    None
}

/// Get the screen-coordinate bounds of a window.
pub fn get_window_rect(_hwnd: RawHwnd) -> Option<WindowRect> {
    // TODO: Implement using XGetWindowAttributes + XTranslateCoordinates
    log::warn!("Linux get_window_rect not yet implemented");
    None
}

/// Create a new overlay window.
pub fn create_overlay_window(_x: i32, _y: i32, _width: u32, _height: u32) -> Option<RawHwnd> {
    // TODO: Implement using XCreateWindow
    log::warn!("Linux create_overlay_window not yet implemented");
    None
}

/// Show or hide a window.
pub fn show_window(_hwnd: RawHwnd, _show: bool) {
    // TODO: Implement using XMapWindow/XUnmapWindow
    log::warn!("Linux show_window not yet implemented");
}

/// Set the position and size of a window.
pub fn set_window_frame(_hwnd: RawHwnd, _x: i32, _y: i32, _width: u32, _height: u32) {
    // TODO: Implement using XMoveResizeWindow
    log::warn!("Linux set_window_frame not yet implemented");
}

/// Close a window.
pub fn close_window(_hwnd: RawHwnd) {
    // TODO: Implement using XDestroyWindow
    log::warn!("Linux close_window not yet implemented");
}
