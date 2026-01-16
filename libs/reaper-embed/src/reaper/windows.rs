//! REAPER window utilities
//!
//! Provides utilities for finding and interacting with REAPER's windows,
//! such as the arrange view, mixer, and other child windows.

// TODO: Phase 3 - Implement window finding for overlay mode
//
// To find REAPER child windows by ID (like JS_Window_FindChildByID):
//
// On Windows:
//   - Use GetDlgItem(parent_hwnd, child_id) for direct children
//   - Or EnumChildWindows for recursive search
//
// On macOS:
//   - Traverse NSView hierarchy using subviews
//   - Match by tag or identifier
//
// On Linux:
//   - Use XQueryTree to enumerate child windows
//   - Match by window properties

use crate::reaper::hwnd::RawHwnd;

/// Known REAPER child window IDs.
///
/// These IDs are used to find specific REAPER windows for overlay positioning.
pub mod child_ids {
    /// Arrange/track view (main editing area).
    ///
    /// This is the main area where tracks and items are displayed.
    pub const ARRANGE_VIEW: u32 = 0x3E8; // 1000

    // TODO: Add more known window IDs as they are discovered
    // pub const TRANSPORT: u32 = ...;
    // pub const MIXER: u32 = ...;
}

/// Find a REAPER child window by ID.
///
/// Equivalent to `JS_Window_FindChildByID` from the js_ReaScriptAPI.
///
/// # Arguments
///
/// * `parent` - Parent window handle (typically REAPER's main window)
/// * `child_id` - The child window ID to find
///
/// # Returns
///
/// The child window handle if found, or `None` if not found.
pub fn find_child_by_id(_parent: RawHwnd, _child_id: u32) -> Option<RawHwnd> {
    // TODO: Implement platform-specific child window search
    //
    // #[cfg(target_os = "windows")]
    // {
    //     use windows::Win32::UI::WindowsAndMessaging::GetDlgItem;
    //     let child = unsafe { GetDlgItem(parent, child_id as i32) };
    //     (!child.is_invalid()).then_some(child)
    // }
    //
    // #[cfg(target_os = "macos")]
    // {
    //     // Traverse NSView hierarchy
    //     None
    // }
    //
    // #[cfg(target_os = "linux")]
    // {
    //     // Use XQueryTree
    //     None
    // }
    None
}

/// Get the position and size of a window.
///
/// # Arguments
///
/// * `hwnd` - Window handle
///
/// # Returns
///
/// A tuple of (x, y, width, height) in screen coordinates.
pub fn get_window_rect(_hwnd: RawHwnd) -> Option<(i32, i32, u32, u32)> {
    // TODO: Implement platform-specific window rect query
    //
    // #[cfg(target_os = "windows")]
    // {
    //     use windows::Win32::UI::WindowsAndMessaging::GetWindowRect;
    //     let mut rect = RECT::default();
    //     unsafe { GetWindowRect(hwnd, &mut rect).ok()? };
    //     Some((rect.left, rect.top, (rect.right - rect.left) as u32, (rect.bottom - rect.top) as u32))
    // }
    None
}
