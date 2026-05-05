//! Window geometry helpers for nudge / resize / snap actions.
//!
//! Two pluggable backends sit behind the same `nudge`/`grow`/`rect`/
//! `set_rect` surface:
//!
//! - `swell` — the default. Talks to SWELL through `swell-ui::Window`,
//!   which is what works on Win32, macOS, X11, and *floating* Wayland
//!   compositors that honor xdg-shell explicit geometry.
//! - `niri` — picked when `$NIRI_SOCKET` is set. niri is a tiling
//!   compositor that ignores explicit-geometry requests by design;
//!   instead we shell out to `niri msg action ...` so the same FTS
//!   keybinds mean something useful (move-column-left, set-column-width
//!   "+50", etc.) on niri.
//!
//! The selection happens once per process at first call to
//! [`backend()`]. Same key pressed on a niri seat as on a Win32 seat
//! does The Right Thing for the local layout model — the user doesn't
//! re-bind anything when they switch seats.

use daw_proto::ScreensetRect;
use daw_proto::window_geometry::{WindowGeometryResult, WindowGeometryService, WindowTarget};
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
fn swell_nudge(window: Window, dx: i32, dy: i32) {
    let r = window.window_rect();
    move_to_pixels(window, r.left as i32 + dx, r.top as i32 + dy);
}

/// Resize `window` by `(dw, dh)` pixels without moving its origin.
fn swell_grow(window: Window, dw: i32, dh: i32) {
    let r = window.window_rect();
    let new_w = ((r.right - r.left) as i32 + dw).max(MIN_DIM);
    let new_h = ((r.bottom - r.top) as i32 + dh).max(MIN_DIM);
    set_size_pixels(window, new_w, new_h);
}

/// Replace the window's rect outright.
fn swell_set_rect(window: Window, rect: ScreensetRect) {
    move_to_pixels(window, rect.x, rect.y);
    set_size_pixels(window, rect.width as i32, rect.height as i32);
}

// ─── public action surface — backend-aware ───────────────────────────────

/// Backend-aware nudge. On SWELL backends moves the window by `(dx, dy)`
/// pixels; on niri translates to column / row moves (sign of dx/dy
/// chooses direction; magnitude is ignored — niri's actions are discrete).
pub fn nudge(window: Window, dx: i32, dy: i32) {
    match backend() {
        Backend::Swell => swell_nudge(window, dx, dy),
        Backend::Niri => niri_nudge(dx, dy),
    }
}

/// Backend-aware resize. On SWELL changes `(width, height)` by
/// `(dw, dh)` pixels (clamped to MIN_DIM). On niri sends
/// `set-column-width` / `set-window-height` with a `+|dw|` / `-|dw|`
/// pixel-delta string.
pub fn grow(_window: Window, dw: i32, dh: i32) {
    match backend() {
        Backend::Swell => swell_grow(_window, dw, dh),
        Backend::Niri => niri_grow(dw, dh),
    }
}

/// Backend-aware absolute placement. niri ignores explicit geometry by
/// design, so this is a no-op on niri — callers should fall back to
/// `nudge` / `grow` actions when they need to drive layout there.
pub fn set_rect(window: Window, rect: ScreensetRect) {
    match backend() {
        Backend::Swell => swell_set_rect(window, rect),
        // niri tiles by design — explicit geometry isn't honoured.
        // Returning silently keeps the screenset apply path tolerant
        // of cross-seat saves: a layout captured on a floating WM and
        // restored on niri restores everything *except* main-window
        // bounds, which is fine because niri picks them anyway.
        Backend::Niri => {}
    }
}

/// Which window-management backend the helpers route through. Decided
/// once at first use — see [`backend()`].
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Backend {
    Swell,
    Niri,
}

/// Pick the backend to use for the current process. Cached for the
/// process lifetime — switching window managers mid-session would
/// require a REAPER restart anyway.
fn backend() -> Backend {
    use std::sync::OnceLock;
    static CHOSEN: OnceLock<Backend> = OnceLock::new();
    *CHOSEN.get_or_init(|| {
        // niri exports `$NIRI_SOCKET` to clients running under it. We
        // also gate on `niri-msg` being PATH-resolvable to avoid taking
        // the niri code path when a stale env var leaks into a non-niri
        // session. The `which`-style check is a single existence test,
        // not a spawn.
        let socket = std::env::var("NIRI_SOCKET").unwrap_or_default();
        if !socket.is_empty() && std::path::Path::new(&socket).exists() && which_in_path("niri") {
            tracing::info!("window-geometry backend: niri (socket={socket})");
            Backend::Niri
        } else {
            tracing::debug!("window-geometry backend: swell");
            Backend::Swell
        }
    })
}

/// Look for `bin` in the user's `PATH`. Avoids depending on the `which`
/// crate for a one-off check.
fn which_in_path(bin: &str) -> bool {
    let Ok(path) = std::env::var("PATH") else {
        return false;
    };
    for dir in path.split(':') {
        let candidate = std::path::Path::new(dir).join(bin);
        if candidate.is_file() {
            return true;
        }
    }
    false
}

/// Run a `niri msg action <name> [args...]` command. Fire-and-forget —
/// niri's actions return success synchronously and we don't surface
/// errors back to the user since these are bound to keypresses.
fn niri_action(args: &[&str]) {
    let mut cmd = std::process::Command::new("niri");
    cmd.arg("msg").arg("action").args(args);
    match cmd.status() {
        Ok(status) if status.success() => {}
        Ok(status) => tracing::warn!("niri msg action {args:?} exited with {status}"),
        Err(err) => tracing::warn!("niri msg action {args:?} failed to spawn: {err}"),
    }
}

/// niri-specific nudge mapping. Direction-only; magnitude is ignored
/// because niri's column/row moves are discrete.
fn niri_nudge(dx: i32, dy: i32) {
    if dx < 0 {
        niri_action(&["move-column-left"]);
    } else if dx > 0 {
        niri_action(&["move-column-right"]);
    }
    if dy < 0 {
        niri_action(&["move-window-up"]);
    } else if dy > 0 {
        niri_action(&["move-window-down"]);
    }
}

/// niri-specific resize mapping. Translates pixel deltas into niri's
/// `set-column-width` / `set-window-height` arguments — niri accepts
/// `+N` / `-N` strings for pixel deltas.
fn niri_grow(dw: i32, dh: i32) {
    if dw != 0 {
        let arg = format!("{:+}", dw);
        niri_action(&["set-column-width", &arg]);
    }
    if dh != 0 {
        let arg = format!("{:+}", dh);
        niri_action(&["set-window-height", &arg]);
    }
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
    window.move_to_pixels(Point::new(Pixels(x.max(0) as u32), Pixels(y.max(0) as u32)));
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

    async fn set_rect(&self, target: WindowTarget, rect: ScreensetRect) -> WindowGeometryResult {
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
