//! Window geometry service traits.

use super::{WindowGeometryResult, WindowTarget};
use crate::{DawResult, ScreensetRect};
use vox::service;

#[service]
pub trait WindowGeometryService {
    async fn get_rect(&self, target: WindowTarget) -> WindowGeometryResult;
    async fn nudge(&self, target: WindowTarget, dx: i32, dy: i32) -> WindowGeometryResult;
    async fn grow(&self, target: WindowTarget, dw: i32, dh: i32) -> WindowGeometryResult;
    async fn set_rect(&self, target: WindowTarget, rect: ScreensetRect) -> WindowGeometryResult;
}

/// Sync handle counterpart — used by in-process callers.
pub trait WindowGeometry {
    fn rect(&self, target: WindowTarget) -> Option<ScreensetRect>;
    fn nudge(&self, target: WindowTarget, dx: i32, dy: i32) -> DawResult<()>;
    fn grow(&self, target: WindowTarget, dw: i32, dh: i32) -> DawResult<()>;
    fn set_rect(&self, target: WindowTarget, rect: ScreensetRect) -> DawResult<()>;
}
