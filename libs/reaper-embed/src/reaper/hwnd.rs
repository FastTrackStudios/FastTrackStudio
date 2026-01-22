//! HWND extraction from BaseView windows
//!
//! Provides utilities for extracting platform window handles from BaseView's
//! `WindowHandle` for use with REAPER's window APIs.

use raw_window_handle::{HasWindowHandle, RawWindowHandle};

/// Platform-specific window handle type.
///
/// On Windows, this is an HWND (pointer-sized handle).
/// On macOS, this is an NSView pointer.
/// On Linux, this is an X11 window ID (u32).
#[cfg(target_os = "windows")]
pub type RawHwnd = isize; // HWND is a pointer-sized handle

#[cfg(target_os = "macos")]
pub type RawHwnd = *mut std::ffi::c_void; // NSView pointer

#[cfg(target_os = "linux")]
pub type RawHwnd = u32; // X11 window ID

/// Extract the platform HWND from a window handle.
///
/// # Arguments
///
/// * `window` - A window that implements `HasWindowHandle`
///
/// # Returns
///
/// The platform-specific window handle, or `None` if extraction fails.
pub fn extract_hwnd<W: HasWindowHandle>(window: &W) -> Option<RawHwnd> {
    let handle = window.window_handle().ok()?;

    match handle.as_raw() {
        #[cfg(target_os = "windows")]
        RawWindowHandle::Win32(win32) => Some(win32.hwnd.get() as RawHwnd),

        #[cfg(target_os = "macos")]
        RawWindowHandle::AppKit(appkit) => Some(appkit.ns_view.as_ptr()),

        #[cfg(target_os = "linux")]
        RawWindowHandle::Xcb(xcb) => Some(xcb.window.get()),

        #[cfg(target_os = "linux")]
        RawWindowHandle::Xlib(xlib) => Some(xlib.window as u32),

        _ => None,
    }
}

/// Convert a RawHwnd to the type expected by REAPER's low-level API.
///
/// REAPER uses `*mut std::ffi::c_void` (HWND__) for window handles.
#[cfg(target_os = "windows")]
pub fn to_reaper_hwnd(hwnd: RawHwnd) -> *mut std::ffi::c_void {
    hwnd as *mut std::ffi::c_void
}

#[cfg(target_os = "macos")]
pub fn to_reaper_hwnd(hwnd: RawHwnd) -> *mut std::ffi::c_void {
    hwnd
}

#[cfg(target_os = "linux")]
pub fn to_reaper_hwnd(hwnd: RawHwnd) -> *mut std::ffi::c_void {
    hwnd as *mut std::ffi::c_void
}
