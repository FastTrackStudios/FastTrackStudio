//! `StandaloneProject` — per-project sync handle.

use daw_proto::{DawResult, ProjectInfo, sync::Project};

use super::daw::Standalone;
use super::ext_state::StandaloneExtState;
use super::fx_chains::StandaloneFxChains;
use super::fx_params::StandaloneFxParams;
use super::items::StandaloneItems;
use super::regions::StandaloneRegions;
use super::routing::StandaloneRouting;
use super::takes::StandaloneTakes;
use super::tempo_map::StandaloneTempoMap;
// `Tracks` ported to architect::rpc — `impl Tracks for Standalone`
// lives at `crate::track`. The borrowed `StandaloneTracks<'a>` view
// retired with the port.
use super::transport::StandaloneTransport;

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
    type Transport<'b>
        = StandaloneTransport<'b>
    where
        Self: 'b;
    type Regions<'b>
        = StandaloneRegions<'b>
    where
        Self: 'b;
    type TempoMap<'b>
        = StandaloneTempoMap<'b>
    where
        Self: 'b;
    type ExtState<'b>
        = StandaloneExtState<'b>
    where
        Self: 'b;
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
    type Takes<'b>
        = StandaloneTakes<'b>
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

    fn transport(&self) -> Self::Transport<'_> {
        StandaloneTransport::new(self.daw, self.guid.clone())
    }

    fn regions(&self) -> Self::Regions<'_> {
        StandaloneRegions::new(self.daw, self.guid.clone())
    }

    fn tempo_map(&self) -> Self::TempoMap<'_> {
        StandaloneTempoMap::new(self.daw, self.guid.clone())
    }

    fn ext_state(&self) -> Self::ExtState<'_> {
        StandaloneExtState::new(self.daw, self.guid.clone())
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

    fn takes(&self) -> Self::Takes<'_> {
        StandaloneTakes::new(self.daw, self.guid.clone())
    }

    fn routing(&self) -> Self::Routing<'_> {
        StandaloneRouting::new(self.daw, self.guid.clone())
    }
}
