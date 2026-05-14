//! `Reaper` — the REAPER backend singleton.
//!
//! After the `#[architect::rpc]` port, backends are stateless: they
//! don't hold a project guid, they don't hold a TaskSupport handle,
//! they don't allocate at construction. The full mount surface is:
//!
//! ```ignore
//! use daw_proto::marker;
//! router.mount(marker::serve(Reaper));
//! ```
//!
//! `Reaper` carries:
//!
//! - The dispatcher (via [`architect::HasDispatcher`]) — every sync
//!   method goes through [`ReaperMainThreadDispatcher`] to land on
//!   the REAPER main thread before touching the C API.
//! - Per-service trait impls (`Markers`, eventually `Items`, `Tracks`,
//!   …). Each method takes `ProjectContext` so one `Reaper` instance
//!   serves every project the binary touches.
//!
//! Tests / alternate-runtime scenarios that want a different dispatcher
//! (e.g. `CurrentThreadDispatcher` for unit tests without REAPER
//! running) use a newtype wrapper with its own `HasDispatcher` impl.

use std::any::Any;
use std::ffi::CString;
use std::future::Future;
use std::pin::Pin;

use architect::HasDispatcher;
use architect::dispatch::{BoxedAny, DispatchError, Dispatcher};
use daw_proto::sync::Markers;
use daw_proto::{DawError, DawResult, Marker, Position, ProjectContext, TimePosition};
use reaper_high::Reaper as ReaperHigh;
use reaper_medium::{MarkerOrRegionPosition, PositionInSeconds};

use crate::main_thread;
use crate::project_context::resolve_project_context;
use crate::safe_wrappers::markers as sw;
use crate::safe_wrappers::ruler_lanes;

// ── Dispatcher ─────────────────────────────────────────────────────────

/// Marshals sync closures onto REAPER's main thread via
/// [`main_thread::query`]. Use this with any architect-rpc host whose
/// backend touches REAPER state.
#[derive(Debug, Default, Clone, Copy)]
pub struct ReaperMainThreadDispatcher;

impl Dispatcher for ReaperMainThreadDispatcher {
    fn dispatch(
        &self,
        f: Box<dyn FnOnce() -> BoxedAny + Send + 'static>,
    ) -> Pin<Box<dyn Future<Output = Result<BoxedAny, DispatchError>> + Send + 'static>> {
        Box::pin(async move {
            // `main_thread::query` returns `None` when TaskSupport
            // isn't installed (extension not yet bootstrapped or
            // already torn down). Map that to `ShutDown` so callers
            // see a clean error instead of a silent panic.
            match main_thread::query(f).await {
                Some(any) => Ok(any),
                None => Err(DispatchError::ShutDown),
            }
        })
    }
}

// ── Backend singleton ──────────────────────────────────────────────────

/// REAPER backend identity. Stateless — project context flows through
/// each method call. Mount with `marker::serve(Reaper)` (and the
/// per-service `serve` of every other trait `Reaper` impls).
#[derive(Debug, Default, Clone, Copy)]
pub struct Reaper;

impl HasDispatcher for Reaper {
    type Dispatcher = ReaperMainThreadDispatcher;

    fn dispatcher(&self) -> Self::Dispatcher {
        ReaperMainThreadDispatcher
    }
}

// ── Markers impl ───────────────────────────────────────────────────────

impl Markers for Reaper {
    fn all(&self, project: ProjectContext) -> Vec<Marker> {
        let reaper = ReaperHigh::get();
        let medium = reaper.medium_reaper();
        let low = medium.low();
        let ctx = resolve_project_context(&project);
        let mut markers = Vec::new();

        let total_count = medium.count_project_markers(ctx).total_count;
        for idx in 0..total_count {
            medium.enum_project_markers_3(ctx, idx, |result| {
                if let Some(info) = result
                    && info.region_end_position.is_none()
                {
                    let id = info.id.get();
                    let lane = ruler_lanes::assigned_lane(low, ctx, false, id)
                        .or_else(|| ruler_lanes::get_marker_lane(low, ctx, idx));
                    markers.push(Marker {
                        id: Some(id),
                        position: Position::from_time(TimePosition::from_seconds(
                            info.position.get(),
                        )),
                        name: info.name.to_string(),
                        color: {
                            let c = info.color.to_raw();
                            if c != 0 { Some(c as u32) } else { None }
                        },
                        guid: None,
                        lane,
                    });
                }
            });
        }

        markers.sort_by(|a, b| {
            a.position_seconds()
                .partial_cmp(&b.position_seconds())
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        markers
    }

    fn get(&self, project: ProjectContext, id: u32) -> Option<Marker> {
        // Fully-qualified — `Reaper` impls multiple service traits
        // (`Markers`, `Tracks`, …) and `self.all(...)` would be
        // ambiguous between them.
        <Self as Markers>::all(self, project)
            .into_iter()
            .find(|m| m.id == Some(id))
    }

    fn count(&self, project: ProjectContext) -> u32 {
        let medium = ReaperHigh::get().medium_reaper();
        let ctx = resolve_project_context(&project);
        let total = medium.count_project_markers(ctx).total_count;
        let mut n = 0u32;
        for idx in 0..total {
            medium.enum_project_markers_3(ctx, idx, |result| {
                if let Some(info) = result
                    && info.region_end_position.is_none()
                {
                    n += 1;
                }
            });
        }
        n
    }

    fn add(&self, project: ProjectContext, position: f64, name: &str) -> DawResult<u32> {
        let ctx = resolve_project_context(&project);
        let medium = ReaperHigh::get().medium_reaper();
        let pos = PositionInSeconds::new(position)
            .map_err(|e| DawError::operation_failed(format!("invalid position: {e:?}")))?;
        let id = medium
            .add_project_marker_2(ctx, MarkerOrRegionPosition::Marker(pos), name, None, None)
            .map_err(|e| DawError::operation_failed(format!("add marker failed: {e:?}")))?;
        Ok(id)
    }

    fn remove(&self, project: ProjectContext, id: u32) -> DawResult<()> {
        let ctx = resolve_project_context(&project);
        let low = ReaperHigh::get().medium_reaper().low();
        sw::delete_project_marker(low, ctx, id as i32, false);
        Ok(())
    }

    fn set_position(&self, _project: ProjectContext, id: u32, position: f64) -> DawResult<()> {
        let low = ReaperHigh::get().medium_reaper().low();
        sw::set_project_marker(low, id as i32, false, position, 0.0, None);
        Ok(())
    }

    fn rename(&self, _project: ProjectContext, id: u32, name: &str) -> DawResult<()> {
        let low = ReaperHigh::get().medium_reaper().low();
        let cname = CString::new(name)
            .map_err(|e| DawError::operation_failed(format!("invalid name: {e}")))?;
        sw::set_project_marker(low, id as i32, false, -1.0, 0.0, Some(&cname));
        Ok(())
    }

    fn set_color(&self, project: ProjectContext, id: u32, color: u32) -> DawResult<()> {
        let ctx = resolve_project_context(&project);
        let medium = ReaperHigh::get().medium_reaper();
        let low = medium.low();
        let total_count = medium.count_project_markers(ctx).total_count;
        let reaper_color = (color | 0x01000000) as i32;

        let mut found = false;
        for idx in 0..total_count {
            medium.enum_project_markers_3(ctx, idx, |result| {
                if let Some(info) = result
                    && info.region_end_position.is_none()
                    && info.id.get() == id
                    && let Ok(name) = CString::new(info.name.to_string())
                {
                    sw::set_project_marker_by_index2(
                        low,
                        ctx,
                        idx as i32,
                        false,
                        info.position.get(),
                        0.0,
                        id as i32,
                        Some(&name),
                        reaper_color,
                        0,
                    );
                    found = true;
                }
            });
            if found {
                break;
            }
        }
        if !found {
            return Err(DawError::not_found("Marker", &id.to_string()));
        }
        Ok(())
    }
}

// ── Compile-time tripwires ─────────────────────────────────────────────
//
// Fail the build (here, with an obvious diagnostic) if the wiring
// contract slips — these are cheaper to maintain than catching the
// same problem at every `marker::serve(Reaper)` callsite.

#[allow(dead_code)]
fn _assert_reaper_is_arc_safe() {
    fn assert_send_sync_static<T: Send + Sync + 'static>() {}
    assert_send_sync_static::<Reaper>();
}

#[allow(dead_code)]
fn _assert_dispatcher_impls_trait() {
    fn assert_dispatcher<T: Dispatcher>() {}
    assert_dispatcher::<ReaperMainThreadDispatcher>();
}

#[allow(dead_code)]
fn _assert_reaper_has_dispatcher() {
    fn assert_has_dispatcher<T: HasDispatcher>() {}
    assert_has_dispatcher::<Reaper>();
}

#[allow(dead_code)]
fn _erase_unused() {
    // `Any` is referenced by `BoxedAny` indirectly; quiet the lint
    // without a top-of-file `#[allow]`.
    let _: Option<&dyn Any> = None;
}
