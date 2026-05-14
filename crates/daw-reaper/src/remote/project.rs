//! Per-project remote handle: [`RemoteProject`].

use daw_proto::sync::{Daw as _, Project as ProjectTrait};
use daw_proto::{DawResult, ProjectInfo};

use super::{ReaperRemote, RemoteItems, RemoteRouting, dispatch, main_thread};

/// A `Send + Sync` handle scoped to a single REAPER project tab.
pub struct RemoteProject<'a> {
    pub(crate) remote: &'a ReaperRemote,
    pub(crate) guid: String,
}

impl<'a> RemoteProject<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote, guid: String) -> Self {
        Self { remote, guid }
    }
}

impl<'a> ProjectTrait for RemoteProject<'a> {
    type Items<'b>
        = RemoteItems<'b>
    where
        Self: 'b;
    type Routing<'b>
        = RemoteRouting<'b>
    where
        Self: 'b;

    fn guid(&self) -> &str {
        &self.guid
    }

    fn info(&self) -> DawResult<ProjectInfo> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.info()
        })
    }

    fn items(&self) -> Self::Items<'_> {
        RemoteItems::new(self.remote, &self.guid)
    }

    fn routing(&self) -> Self::Routing<'_> {
        RemoteRouting::new(self.remote, &self.guid)
    }
}
