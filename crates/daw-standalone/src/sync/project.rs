//! `StandaloneProject` — per-project sync handle.

use daw_proto::{DawResult, ProjectInfo, Projects};

use super::daw::Standalone;
// `ExtState` ported to architect::rpc — see `crate::ext_state`.
// `FxChains` ported to architect::rpc — see `crate::fx_chains`.
// `FxParams` ported to architect::rpc — see `crate::fx_params`.
// `Items` ported to architect::rpc — see `crate::item`.
// `Routing` ported to architect::rpc — `impl Routing for Standalone`
// lives at `crate::routing_sync`.
// `Takes` ported to architect::rpc — `impl Takes for Standalone`
// lives at `crate::take`. Borrowed view retired with the port.
// `Tracks` ported to architect::rpc — `impl Tracks for Standalone`
// lives at `crate::track`. The borrowed `StandaloneTracks<'a>` view
// retired with the port.
// `Transport` ported to architect::rpc — impl on `Standalone` at
// `crate::transport`.

/// Per-project sync handle. Owns sub-domain accessors.
pub struct StandaloneProject<'a> {
    daw: &'a Standalone,
    guid: String,
}

impl<'a> StandaloneProject<'a> {
    pub(crate) fn new(daw: &'a Standalone, guid: String) -> Self {
        Self { daw, guid }
    }
}

impl<'a> Projects for StandaloneProject<'a> {
    fn guid(&self) -> &str {
        &self.guid
    }

    fn info(&self) -> DawResult<ProjectInfo> {
        self.daw.with_project(&self.guid, |p| p.info.clone())
    }
}
