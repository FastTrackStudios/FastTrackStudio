//! Window geometry helpers for nudge / resize / snap actions.
//!
//! Thin layer over `swell-ui`'s `Window` (the typed FTS wrapper around
//! SWELL HWNDs). All transforms preserve Z-order and don't steal focus,
//! which matters because these helpers are typically driven by REAPER
//! action invocations — the user pressed a key, they don't want their
//! click target to disappear.

use daw_proto::ScreensetRect;
use daw_proto::window_geometry::{
    WindowGeometryResult, WindowGeometryService, WindowTarget,
};
use reaper_low::raw;
use swell_ui::Window;

use crate::main_thread;

/// Minimum allowed width/height after a shrink. Some window managers
/// refuse zero-size windows or snap them to a corner; clamp here so a
/// runaway `grow(-N)` chain doesn't paint the user into one.
const MIN_DIM: i32 = 80;

/// Resolve a `WindowTarget` to a SWELL [`Window`].
///
/// Returns `None` when SWELL isn't loaded (older REAPER builds, or
/// daw-bridge.so loaded before `Swell::make_available_globally`), or the
/// requested target isn't currently usable (no focused window, no main
/// HWND).
pub fn resolve_target(target: WindowTarget) -> Option<Window> {
    if !reaper_low::Swell::is_available_globally() {
        return None;
    }
    match target {
        WindowTarget::Focused => Window::focused(),
        WindowTarget::Main => {
            let hwnd = reaper_high::Reaper::get().medium_reaper().get_main_hwnd();
            Window::new(hwnd.as_ptr())
        }
    }
}

/// Read the outer-rect of `window` in screen coordinates.
pub fn read_rect(window: Window) -> ScreensetRect {
    let r = window.window_rect();
    rect_to_proto(r)
}

/// Move `window` by `(dx, dy)` pixels without changing its size.
pub fn nudge(window: Window, dx: i32, dy: i32) {
    let r = window.window_rect();
    move_to_pixels(window, r.left as i32 + dx, r.top as i32 + dy);
}

/// Resize `window` by `(dw, dh)` pixels without moving its origin.
pub fn grow(window: Window, dw: i32, dh: i32) {
    let r = window.window_rect();
    let new_w = ((r.right - r.left) as i32 + dw).max(MIN_DIM);
    let new_h = ((r.bottom - r.top) as i32 + dh).max(MIN_DIM);
    set_size_pixels(window, new_w, new_h);
}

/// Replace the window's rect outright.
pub fn set_rect(window: Window, rect: ScreensetRect) {
    move_to_pixels(window, rect.x, rect.y);
    set_size_pixels(window, rect.width as i32, rect.height as i32);
}

/// Convert a SWELL `RECT` to the wire type used in screensets and the
/// geometry service.
fn rect_to_proto(r: raw::RECT) -> ScreensetRect {
    ScreensetRect {
        x: r.left as i32,
        y: r.top as i32,
        width: (r.right - r.left).max(0) as u32,
        height: (r.bottom - r.top).max(0) as u32,
    }
}

fn move_to_pixels(window: Window, x: i32, y: i32) {
    use swell_ui::{Pixels, Point};
    // SWELL coordinates are unsigned; clamp negatives to 0 so a window
    // can never be moved off-screen entirely on platforms that interpret
    // negative coords as "leftmost monitor edge minus N".
    window.move_to_pixels(Point::new(
        Pixels(x.max(0) as u32),
        Pixels(y.max(0) as u32),
    ));
}

fn set_size_pixels(window: Window, w: i32, h: i32) {
    use swell_ui::{Dimensions, Pixels};
    window.resize(Dimensions::new(
        Pixels(w.max(0) as u32),
        Pixels(h.max(0) as u32),
    ));
}

fn unresolved() -> WindowGeometryResult {
    WindowGeometryResult {
        applied: false,
        rect: None,
        error: "no resolvable target window (SWELL unavailable or nothing focused)".to_string(),
    }
}

/// REAPER-backed [`WindowGeometryService`] used by daw-bridge.
///
/// All operations queue on the REAPER main thread because SWELL window
/// APIs are not safe to call from arbitrary threads.
#[derive(Clone, Default)]
pub struct ReaperWindowGeometry;

impl ReaperWindowGeometry {
    pub fn new() -> Self {
        Self
    }
}

impl WindowGeometryService for ReaperWindowGeometry {
    async fn get_rect(&self, target: WindowTarget) -> WindowGeometryResult {
        main_thread::query(move || match resolve_target(target) {
            Some(window) => WindowGeometryResult {
                applied: true,
                rect: Some(read_rect(window)),
                error: String::new(),
            },
            None => unresolved(),
        })
        .await
        .unwrap_or_else(unresolved)
    }

    async fn nudge(&self, target: WindowTarget, dx: i32, dy: i32) -> WindowGeometryResult {
        main_thread::query(move || match resolve_target(target) {
            Some(window) => {
                nudge(window, dx, dy);
                WindowGeometryResult {
                    applied: true,
                    rect: Some(read_rect(window)),
                    error: String::new(),
                }
            }
            None => unresolved(),
        })
        .await
        .unwrap_or_else(unresolved)
    }

    async fn grow(&self, target: WindowTarget, dw: i32, dh: i32) -> WindowGeometryResult {
        main_thread::query(move || match resolve_target(target) {
            Some(window) => {
                grow(window, dw, dh);
                WindowGeometryResult {
                    applied: true,
                    rect: Some(read_rect(window)),
                    error: String::new(),
                }
            }
            None => unresolved(),
        })
        .await
        .unwrap_or_else(unresolved)
    }

    async fn set_rect(
        &self,
        target: WindowTarget,
        rect: ScreensetRect,
    ) -> WindowGeometryResult {
        main_thread::query(move || match resolve_target(target) {
            Some(window) => {
                set_rect(window, rect);
                WindowGeometryResult {
                    applied: true,
                    rect: Some(read_rect(window)),
                    error: String::new(),
                }
            }
            None => unresolved(),
        })
        .await
        .unwrap_or_else(unresolved)
    }
}
