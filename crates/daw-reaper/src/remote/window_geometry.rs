//! Remote window geometry handle: [`RemoteWindowGeometry`].

use daw_proto::{Daw as _, WindowGeometry as WindowGeometryTrait};
use daw_proto::{DawResult, ScreensetRect, WindowTarget};

use super::{ReaperRemote, dispatch};

pub struct RemoteWindowGeometry<'a> {
    remote: &'a ReaperRemote,
}

impl<'a> RemoteWindowGeometry<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote) -> Self {
        Self { remote }
    }
}

impl<'a> WindowGeometryTrait for RemoteWindowGeometry<'a> {
    fn rect(&self, target: WindowTarget) -> Option<ScreensetRect> {
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                mt.window_geometry().rect(target)
            })
            .await
            .flatten()
        })
    }

    fn nudge(&self, target: WindowTarget, dx: i32, dy: i32) -> DawResult<()> {
        dispatch(self.remote, move || {
            let mt = super::main_thread()?;
            mt.window_geometry().nudge(target, dx, dy)
        })
    }

    fn grow(&self, target: WindowTarget, dw: i32, dh: i32) -> DawResult<()> {
        dispatch(self.remote, move || {
            let mt = super::main_thread()?;
            mt.window_geometry().grow(target, dw, dh)
        })
    }

    fn set_rect(&self, target: WindowTarget, rect: ScreensetRect) -> DawResult<()> {
        dispatch(self.remote, move || {
            let mt = super::main_thread()?;
            mt.window_geometry().set_rect(target, rect)
        })
    }
}
