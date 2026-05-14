//! `StandaloneProject` — per-project sync handle.

use daw_proto::{DawResult, ProjectInfo, sync::Project};

use super::daw::Standalone;
// `ExtState` ported to architect::rpc — see `crate::ext_state`.
use super::fx_chains::StandaloneFxChains;
use super::fx_params::StandaloneFxParams;
use super::items::StandaloneItems;
use super::routing::StandaloneRouting;
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

impl<'a> Project for StandaloneProject<'a> {
    type FxChains<'b>
        = StandaloneFxChains<'b>
    where
        Self: 'b;
    type FxParams<'b>
        = StandaloneFxParams<'b>
    where
        Self: 'b;
    type Items<'b>
        = StandaloneItems<'b>
    where
        Self: 'b;
    type Routing<'b>
        = StandaloneRouting<'b>
    where
        Self: 'b;

    fn guid(&self) -> &str {
        &self.guid
    }

    fn info(&self) -> DawResult<ProjectInfo> {
        self.daw.with_project(&self.guid, |p| p.info.clone())
    }

    fn fx_chains(&self) -> Self::FxChains<'_> {
        StandaloneFxChains::new(self.daw, self.guid.clone())
    }

    fn fx_params(&self) -> Self::FxParams<'_> {
        StandaloneFxParams::new(self.daw, self.guid.clone())
    }

    fn items(&self) -> Self::Items<'_> {
        StandaloneItems::new(self.daw, self.guid.clone())
    }

    fn routing(&self) -> Self::Routing<'_> {
        StandaloneRouting::new(self.daw, self.guid.clone())
    }
}
