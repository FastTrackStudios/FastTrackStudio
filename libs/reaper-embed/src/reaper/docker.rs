//! REAPER docking integration
//!
//! Manages docking BaseView windows into REAPER's dock system.
//!
//! REAPER provides 16 docks that can hold multiple windows.
//! This module provides the `DockController` for managing window docking
//! and screenset persistence.

use std::ffi::{CString, c_char, c_int, c_void};

use reaper_low::Reaper;
use reaper_low::raw::HWND;

use super::hwnd::RawHwnd;

/// Screenset callback action codes (from reaper_plugin.h)
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScreensetAction {
    /// Return HWND to be moved on screenset switch (or 0)
    GetHwnd = 0,
    /// Returns 1 if docked
    IsDocked = 1,
    /// Dock if undocked and vice-versa
    SwitchDock = 4,
    /// Load state from actionParm (of actionParmSize). If both are NULL, hide.
    LoadState = 0x100,
    /// Save state to actionParm, return length used
    SaveState = 0x101,
    /// Returns desired size for SaveState
    WantStateSize = 0x102,
}

/// Manages docking a BaseView window into REAPER's dock system.
///
/// # Usage
///
/// ```ignore
/// let controller = DockController::new(hwnd, "my_dock_id", "My Window");
/// controller.dock(&reaper, 0); // Dock to left dock
/// controller.register_screenset(&reaper);
/// ```
pub struct DockController {
    /// The native window handle
    hwnd: RawHwnd,
    /// Unique identifier for screenset persistence (must be static for REAPER)
    dock_id: CString,
    /// Window name for display in dock tabs
    name: CString,
    /// Current dock position (-1 = floating, 0-15 = dock index)
    current_dock: i32,
}

impl DockController {
    /// Create a new dock controller.
    ///
    /// # Arguments
    ///
    /// * `hwnd` - The native window handle
    /// * `dock_id` - Unique identifier for screenset persistence
    /// * `name` - Window name displayed in dock tabs
    pub fn new(hwnd: RawHwnd, dock_id: &str, name: &str) -> Self {
        Self {
            hwnd,
            dock_id: CString::new(dock_id).unwrap_or_default(),
            name: CString::new(name).unwrap_or_default(),
            current_dock: -1,
        }
    }

    /// Convert our RawHwnd to REAPER's HWND type.
    fn to_reaper_hwnd(&self) -> HWND {
        super::hwnd::to_reaper_hwnd(self.hwnd) as HWND
    }

    /// Register window with REAPER's dock system.
    ///
    /// # Arguments
    ///
    /// * `reaper` - Reference to the REAPER low-level API
    /// * `dock_position` - Dock index (0-15) or -1 for floating
    ///
    /// # Safety
    ///
    /// This function calls unsafe REAPER APIs.
    pub fn dock(&mut self, reaper: &Reaper, dock_position: i32) {
        self.current_dock = dock_position;

        // First update the dock ID to set the target dock position
        unsafe {
            reaper.Dock_UpdateDockID(self.dock_id.as_ptr(), dock_position);
        }

        // Then add the window to the dock
        unsafe {
            reaper.DockWindowAddEx(
                self.to_reaper_hwnd(),
                self.name.as_ptr(),
                self.dock_id.as_ptr(),
                true, // allowShow
            );
        }

        log::debug!(
            "Docked window '{}' to dock {} with id '{}'",
            self.name.to_string_lossy(),
            dock_position,
            self.dock_id.to_string_lossy()
        );
    }

    /// Undock window (make floating).
    ///
    /// # Safety
    ///
    /// This function calls unsafe REAPER APIs.
    pub fn undock(&mut self, reaper: &Reaper) {
        unsafe {
            reaper.DockWindowRemove(self.to_reaper_hwnd());
        }
        self.current_dock = -1;

        log::debug!("Undocked window '{}'", self.name.to_string_lossy());
    }

    /// Check if the window is currently docked.
    ///
    /// # Safety
    ///
    /// This function calls unsafe REAPER APIs.
    pub fn is_docked(&self, reaper: &Reaper) -> bool {
        let mut is_floating = false;
        let dock_index =
            unsafe { reaper.DockIsChildOfDock(self.to_reaper_hwnd(), &mut is_floating) };
        dock_index >= 0
    }

    /// Toggle docked/floating state.
    ///
    /// # Safety
    ///
    /// This function calls unsafe REAPER APIs.
    pub fn toggle(&mut self, reaper: &Reaper) {
        if self.is_docked(reaper) {
            self.undock(reaper);
        } else {
            // Dock to the last known dock position, or default to 0
            let dock = if self.current_dock >= 0 {
                self.current_dock
            } else {
                0
            };
            self.dock(reaper, dock);
        }
    }

    /// Activate the docked window (bring to front).
    ///
    /// # Safety
    ///
    /// This function calls unsafe REAPER APIs.
    pub fn activate(&self, reaper: &Reaper) {
        unsafe {
            reaper.DockWindowActivate(self.to_reaper_hwnd());
        }
    }

    /// Register for screenset persistence.
    ///
    /// This allows REAPER to save and restore the window's dock state
    /// when switching between screensets.
    ///
    /// # Arguments
    ///
    /// * `reaper` - Reference to the REAPER low-level API
    /// * `callback` - The screenset callback function
    /// * `user_data` - User data pointer passed to the callback
    ///
    /// # Safety
    ///
    /// This function calls unsafe REAPER APIs. The callback and user_data
    /// must remain valid for the lifetime of the registration.
    pub fn register_screenset(
        &self,
        reaper: &Reaper,
        callback: ScreensetCallback,
        user_data: *mut c_void,
    ) {
        // Note: screenset_registerNew takes a mutable pointer to the id string,
        // but only reads from it. We need to cast away const.
        let id_ptr = self.dock_id.as_ptr() as *mut c_char;

        unsafe {
            reaper.screenset_registerNew(id_ptr, Some(callback), user_data);
        }

        log::debug!(
            "Registered screenset for '{}'",
            self.dock_id.to_string_lossy()
        );
    }

    /// Unregister from screenset persistence.
    ///
    /// # Safety
    ///
    /// This function calls unsafe REAPER APIs.
    pub fn unregister_screenset(&self, reaper: &Reaper) {
        let id_ptr = self.dock_id.as_ptr() as *mut c_char;

        unsafe {
            reaper.screenset_unregister(id_ptr);
        }

        log::debug!(
            "Unregistered screenset for '{}'",
            self.dock_id.to_string_lossy()
        );
    }

    /// Get the current dock position.
    ///
    /// Returns -1 if floating, 0-15 for dock index.
    pub fn current_dock(&self) -> i32 {
        self.current_dock
    }

    /// Get the dock ID.
    pub fn dock_id(&self) -> &str {
        self.dock_id.to_str().unwrap_or("")
    }

    /// Get the window name.
    pub fn name(&self) -> &str {
        self.name.to_str().unwrap_or("")
    }
}

/// Screenset callback function type.
///
/// This matches REAPER's `screensetNewCallbackFunc` signature.
pub type ScreensetCallback = unsafe extern "C" fn(
    action: c_int,
    id: *const c_char,
    param: *mut c_void,
    action_parm: *mut c_void,
    action_parm_size: c_int,
) -> isize;

/// A simple screenset callback that only handles GETHWND.
///
/// This is the minimal callback needed for basic dock persistence.
/// It returns the HWND when asked, allowing REAPER to track the window.
///
/// # Safety
///
/// The `user` parameter must be a valid `RawHwnd` cast to `*mut c_void`.
pub unsafe extern "C" fn simple_screenset_callback(
    action: c_int,
    _id: *const c_char,
    user: *mut c_void,
    _action_parm: *mut c_void,
    _action_parm_size: c_int,
) -> isize {
    match action {
        0 => {
            // SCREENSET_ACTION_GETHWND - return the HWND
            user as isize
        }
        _ => 0,
    }
}
