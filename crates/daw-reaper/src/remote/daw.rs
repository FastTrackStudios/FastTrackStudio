//! Root remote handle: [`ReaperRemote`].

use std::sync::Arc;

use daw_proto::Daw;
use daw_proto::{DawResult, LastTouchedFx, ProjectInfo};

use super::{RemoteProject, dispatch, dispatch_read, main_thread};

/// `Send + Sync` root handle for REAPER, dispatching every call onto the
/// main thread via `main_thread::query` and blocking on a tokio runtime.
///
/// This is the cross-thread counterpart to [`crate::sync::ReaperMainThread`].
/// Use it from audio callbacks, vox handlers, plugin GUIs, or any other
/// thread that cannot itself touch REAPER's API.
#[derive(Clone)]
pub struct ReaperRemote {
    pub(crate) runtime: Arc<tokio::runtime::Runtime>,
}

// Compile-time assertion: ReaperRemote is Send + Sync — that is the entire
// reason this type exists alongside the `!Send` `ReaperMainThread`.
const _: fn() = || {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<ReaperRemote>();
};

impl ReaperRemote {
    /// Construct a new remote handle backed by the supplied tokio runtime.
    ///
    /// The runtime is shared (`Arc`) so callers can keep using it for other
    /// async work; it must outlive every dispatched call.
    pub fn new(runtime: Arc<tokio::runtime::Runtime>) -> Self {
        Self { runtime }
    }
}

impl Daw for ReaperRemote {
    type Project<'a>
        = RemoteProject<'a>
    where
        Self: 'a;

    fn current_project(&self) -> DawResult<Self::Project<'_>> {
        let guid = dispatch(self, move || {
            let mt = main_thread()?;
            let project = mt.current_project()?;
            Ok(project.guid.clone())
        })?;
        Ok(RemoteProject::new(self, guid))
    }

    fn project(&self, guid: &str) -> DawResult<Self::Project<'_>> {
        let guid_owned = guid.to_string();
        let validated = dispatch(self, move || {
            let mt = main_thread()?;
            let project = mt.project(&guid_owned)?;
            Ok(project.guid.clone())
        })?;
        Ok(RemoteProject::new(self, validated))
    }

    fn projects(&self) -> Vec<ProjectInfo> {
        dispatch_read(self, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            mt.projects()
        })
    }

    fn show_console_msg(&self, msg: &str) {
        let owned = msg.to_string();
        let _ = dispatch(self, move || {
            let mt = main_thread()?;
            mt.show_console_msg(&owned);
            Ok(())
        });
    }

    fn last_touched_fx(&self) -> Option<LastTouchedFx> {
        dispatch_read(self, move || {
            let mt = crate::sync::ReaperMainThread::try_new()?;
            mt.last_touched_fx()
        })
    }
}
