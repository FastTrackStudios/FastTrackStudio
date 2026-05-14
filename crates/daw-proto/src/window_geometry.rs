//! Window geometry service — drives nudge / resize / position actions
//! against REAPER windows. Built on the same SWELL primitives the
//! screenset service uses, but exposed as a discrete service so CLI / UI
//! / agents can drive layout changes without going through a screenset
//! capture-apply cycle.

use facet::Facet;
use vox::service;

use crate::ScreensetRect;

/// Which window the geometry op targets.
///
/// - `Focused` — whatever window currently has keyboard focus. Useful for
///   hotkey-driven nudges so the user picks the target by clicking before
///   pressing the binding.
/// - `Main` — REAPER's main window.
#[repr(u8)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Facet)]
#[facet(rename_all = "snake_case")]
pub enum WindowTarget {
    #[default]
    Focused,
    Main,
}

/// Result of a geometry op. `applied` is `false` when the target wasn't
/// resolvable (e.g. nothing focused, or SWELL unavailable). The `rect`
/// reflects the window state *after* the op when `applied`, or `None`
/// when not.
#[derive(Debug, Clone, Default, Facet)]
pub struct WindowGeometryResult {
    pub applied: bool,
    pub rect: Option<ScreensetRect>,
    /// Empty on success; populated when the op couldn't run.
    pub error: String,
}

#[service]
pub trait WindowGeometryService {
    /// Read the current outer-rect of `target`.
    async fn get_rect(&self, target: WindowTarget) -> WindowGeometryResult;

    /// Nudge `target` by `dx`/`dy` pixels.
    async fn nudge(&self, target: WindowTarget, dx: i32, dy: i32) -> WindowGeometryResult;

    /// Grow / shrink `target` by `dw`/`dh` pixels (negative shrinks).
    async fn grow(&self, target: WindowTarget, dw: i32, dh: i32) -> WindowGeometryResult;

    /// Move + resize to an explicit rect.
    async fn set_rect(&self, target: WindowTarget, rect: ScreensetRect) -> WindowGeometryResult;
}

// =============================================================================
// Sync trait — relocated from `crate::sync::window_geometry`.
// =============================================================================
//
// Operations key off `WindowTarget` (Focused / Main) instead of raw HWND
// handles, which matches REAPER + SWELL semantics across platforms.

use crate::DawResult;

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
