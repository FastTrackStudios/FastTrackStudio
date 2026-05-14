//! Glue between daw-reaper's main-thread dispatcher primitive and
//! `architect::dispatch::Dispatcher`, plus owning backend types that
//! `architect::rpc`-emitted hosts can wrap.
//!
//! The existing borrowed-view backends (`ReaperMarkers<'a>` etc.) work
//! for inline call sites that already hold a `ReaperMainThread`
//! marker. For hosting via `<T>Host` the backend must be `'static`,
//! so this module provides owning equivalents that read REAPER state
//! through the same `safe_wrappers` paths the borrowed versions use.
//!
//! Pilot scope (see `architect/DESIGN.md`): only `Markers` is wired
//! here. Other services get the same pattern when their port lands.

use std::any::Any;
use std::future::Future;
use std::pin::Pin;

use architect::dispatch::{BoxedAny, DispatchError, Dispatcher};
use daw_proto::sync::{Daw as _, Markers as MarkersTrait, Project as _};
use daw_proto::{DawError, DawResult, Marker};

use crate::main_thread;

// ── Dispatcher: marshal sync closures onto REAPER's main thread ────────

/// Implements [`architect::dispatch::Dispatcher`] by routing every
/// closure through [`main_thread::query`], which forwards to REAPER's
/// `TaskSupport`. Use this with any `#[architect::rpc]` host whose
/// backend touches REAPER state.
///
/// Construction is free — the actual TaskSupport handle is read from
/// the process-global slot at dispatch time, so this struct can be
/// constructed before the extension has finished initializing.
#[derive(Debug, Default, Clone, Copy)]
pub struct ReaperMainThreadDispatcher;

impl Dispatcher for ReaperMainThreadDispatcher {
    fn dispatch(
        &self,
        f: Box<dyn FnOnce() -> BoxedAny + Send + 'static>,
    ) -> Pin<Box<dyn Future<Output = Result<BoxedAny, DispatchError>> + Send + 'static>> {
        Box::pin(async move {
            // `main_thread::query` returns `Option<R>` — `None` means
            // TaskSupport isn't installed (extension not yet
            // bootstrapped or already torn down). Map that to
            // `DispatchError::ShutDown` so callers see a clean error
            // instead of a silent panic.
            match main_thread::query(f).await {
                Some(any) => Ok(any),
                None => Err(DispatchError::ShutDown),
            }
        })
    }
}

// ── Owning Markers backend ─────────────────────────────────────────────

/// `'static`-owning version of [`crate::sync::ReaperMarkers`].
///
/// Holds the project guid as an owned `String` so the value is
/// `Send + Sync + 'static` — that's the bound `MarkersHost<S, D>`
/// needs to wrap it. All methods delegate to the borrowed-view backend
/// after constructing an inline `ReaperMainThread` marker, so the
/// REAPER C API is only touched after the dispatcher has marshaled
/// the call onto the main thread.
#[derive(Debug, Clone)]
pub struct ReaperMarkersBackend {
    guid: String,
}

impl ReaperMarkersBackend {
    pub fn new(project_guid: impl Into<String>) -> Self {
        Self {
            guid: project_guid.into(),
        }
    }
}

impl MarkersTrait for ReaperMarkersBackend {
    fn all(&self) -> Vec<Marker> {
        match crate::sync::ReaperMainThread::try_new() {
            Some(mt) => match mt.project(&self.guid) {
                Ok(project) => project.markers().all(),
                Err(_) => Vec::new(),
            },
            None => Vec::new(),
        }
    }

    fn get(&self, id: u32) -> Option<Marker> {
        let mt = crate::sync::ReaperMainThread::try_new()?;
        let project = mt.project(&self.guid).ok()?;
        project.markers().get(id)
    }

    fn count(&self) -> u32 {
        match crate::sync::ReaperMainThread::try_new() {
            Some(mt) => mt
                .project(&self.guid)
                .map(|p| p.markers().count())
                .unwrap_or(0),
            None => 0,
        }
    }

    fn add(&self, position: f64, name: &str) -> DawResult<u32> {
        let mt = main_thread_or_err()?;
        mt.project(&self.guid)?.markers().add(position, name)
    }

    fn remove(&self, id: u32) -> DawResult<()> {
        let mt = main_thread_or_err()?;
        mt.project(&self.guid)?.markers().remove(id)
    }

    fn set_position(&self, id: u32, position: f64) -> DawResult<()> {
        let mt = main_thread_or_err()?;
        mt.project(&self.guid)?.markers().set_position(id, position)
    }

    fn rename(&self, id: u32, name: &str) -> DawResult<()> {
        let mt = main_thread_or_err()?;
        mt.project(&self.guid)?.markers().rename(id, name)
    }

    fn set_color(&self, id: u32, color: u32) -> DawResult<()> {
        let mt = main_thread_or_err()?;
        mt.project(&self.guid)?.markers().set_color(id, color)
    }
}

fn main_thread_or_err() -> DawResult<crate::sync::ReaperMainThread> {
    crate::sync::ReaperMainThread::try_new()
        .ok_or_else(|| DawError::Internal("not running on REAPER main thread".into()))
}

// ── Compile-time type assertions ───────────────────────────────────────
//
// These don't run at runtime — they're a tripwire that fires if the
// pilot's wiring contract slips. If `ReaperMarkersBackend` ever loses
// `Send + Sync + 'static` (or `ReaperMainThreadDispatcher` loses
// `Dispatcher`), the build breaks here instead of at the
// `MarkersHost::new` call site, which makes the diagnostic obvious.

#[allow(dead_code)]
fn _assert_backend_is_arc_safe() {
    fn assert_send_sync_static<T: Send + Sync + 'static>() {}
    assert_send_sync_static::<ReaperMarkersBackend>();
}

#[allow(dead_code)]
fn _assert_dispatcher_impls_trait() {
    fn assert_dispatcher<T: Dispatcher>() {}
    assert_dispatcher::<ReaperMainThreadDispatcher>();
}

#[allow(dead_code)]
fn _erase_unused() {
    // `Any` is referenced by `BoxedAny` indirectly; this keeps the
    // unused-import lint quiet without a top-of-file `#[allow]`.
    let _: Option<&dyn Any> = None;
}
