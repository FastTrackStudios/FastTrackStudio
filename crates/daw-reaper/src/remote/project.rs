//! Per-project remote handle: [`RemoteProject`].

use daw_proto::sync::{Daw as _, Project as ProjectTrait};
use daw_proto::{DawResult, ProjectInfo};

use super::{
    ReaperRemote, RemoteExtState, RemoteFxChains, RemoteFxParams, RemoteItems, RemoteRegions,
    RemoteRouting, RemoteTakes, RemoteTempoMap, RemoteTracks, RemoteTransport, dispatch,
    main_thread,
};

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
    type Transport<'b>
        = RemoteTransport<'b>
    where
        Self: 'b;
    type Regions<'b>
        = RemoteRegions<'b>
    where
        Self: 'b;
    type TempoMap<'b>
        = RemoteTempoMap<'b>
    where
        Self: 'b;
    type Tracks<'b>
        = RemoteTracks<'b>
    where
        Self: 'b;
    type ExtState<'b>
        = RemoteExtState<'b>
    where
        Self: 'b;
    type FxChains<'b>
        = RemoteFxChains<'b>
    where
        Self: 'b;
    type FxParams<'b>
        = RemoteFxParams<'b>
    where
        Self: 'b;
    type Items<'b>
        = RemoteItems<'b>
    where
        Self: 'b;
    type Takes<'b>
        = RemoteTakes<'b>
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

    fn transport(&self) -> Self::Transport<'_> {
        RemoteTransport::new(self.remote, &self.guid)
    }

    fn regions(&self) -> Self::Regions<'_> {
        RemoteRegions::new(self.remote, &self.guid)
    }

    fn tempo_map(&self) -> Self::TempoMap<'_> {
        RemoteTempoMap::new(self.remote, &self.guid)
    }

    fn tracks(&self) -> Self::Tracks<'_> {
        RemoteTracks::new(self.remote, &self.guid)
    }

    fn ext_state(&self) -> Self::ExtState<'_> {
        RemoteExtState::new(self.remote)
    }

    fn fx_chains(&self) -> Self::FxChains<'_> {
        RemoteFxChains::new(self.remote, &self.guid)
    }

    fn fx_params(&self) -> Self::FxParams<'_> {
        RemoteFxParams::new(self.remote, &self.guid)
    }

    fn items(&self) -> Self::Items<'_> {
        RemoteItems::new(self.remote, &self.guid)
    }

    fn takes(&self) -> Self::Takes<'_> {
        RemoteTakes::new(self.remote, &self.guid)
    }

    fn routing(&self) -> Self::Routing<'_> {
        RemoteRouting::new(self.remote, &self.guid)
    }
}
