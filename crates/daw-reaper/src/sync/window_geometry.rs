//! Sync `WindowGeometry` impl — delegates to SWELL/niri backends already
//! used by the async `WindowGeometryService`.

use daw_proto::{DawError, DawResult, ScreensetRect, WindowTarget, sync::WindowGeometry};

use crate::window_geometry as wg;

use super::ReaperMainThread;

pub struct ReaperWindowGeometry<'a> {
    _mt: &'a ReaperMainThread,
}

impl<'a> ReaperWindowGeometry<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread) -> Self {
        Self { _mt: mt }
    }
}

fn no_target() -> DawError {
    DawError::not_found(
        "Window",
        "no resolvable target (SWELL unavailable or nothing focused)",
    )
}

impl<'a> WindowGeometry for ReaperWindowGeometry<'a> {
    fn rect(&self, target: WindowTarget) -> Option<ScreensetRect> {
        wg::resolve_target(target).map(wg::read_rect)
    }

    fn nudge(&self, target: WindowTarget, dx: i32, dy: i32) -> DawResult<()> {
        let window = wg::resolve_target(target).ok_or_else(no_target)?;
        wg::nudge(window, dx, dy);
        Ok(())
    }

    fn grow(&self, target: WindowTarget, dw: i32, dh: i32) -> DawResult<()> {
        let window = wg::resolve_target(target).ok_or_else(no_target)?;
        wg::grow(window, dw, dh);
        Ok(())
    }

    fn set_rect(&self, target: WindowTarget, rect: ScreensetRect) -> DawResult<()> {
        let window = wg::resolve_target(target).ok_or_else(no_target)?;
        wg::set_rect(window, rect);
        Ok(())
    }
}
