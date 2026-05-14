//! Host-window geometry — read/move/resize the focused or main window.
//!
//! Operations key off `WindowTarget` (Focused / Main) instead of raw HWND
//! handles, which matches REAPER + SWELL semantics across platforms.

use crate::{DawResult, ScreensetRect, WindowTarget};

pub trait WindowGeometry {
    /// Outer rect of the targeted window in screen coordinates.
    fn rect(&self, target: WindowTarget) -> Option<ScreensetRect>;

    /// Translate the window by `(dx, dy)` pixels.
    fn nudge(&self, target: WindowTarget, dx: i32, dy: i32) -> DawResult<()>;

    /// Resize the window by `(dw, dh)` pixels (clamped to a sane minimum).
    fn grow(&self, target: WindowTarget, dw: i32, dh: i32) -> DawResult<()>;

    /// Replace the window's rect.
    fn set_rect(&self, target: WindowTarget, rect: ScreensetRect) -> DawResult<()>;
}
