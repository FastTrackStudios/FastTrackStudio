use crate::DawResult;

use super::{ExtState, FxChains, FxParams, Items, Routing, Takes, TempoMap};

/// Per-project sync handle. Owns sub-domain accessors.
///
/// `Markers`, `Regions`, `Tracks`, and `Transport` have all been lifted
/// out of this trait by their architect::rpc ports. Other accessors
/// follow as their services are ported.
pub trait Project {
    type TempoMap<'a>: TempoMap + 'a
    where
        Self: 'a;
    type ExtState<'a>: ExtState + 'a
    where
        Self: 'a;
    type FxChains<'a>: FxChains + 'a
    where
        Self: 'a;
    type FxParams<'a>: FxParams + 'a
    where
        Self: 'a;
    type Items<'a>: Items + 'a
    where
        Self: 'a;
    type Takes<'a>: Takes + 'a
    where
        Self: 'a;
    type Routing<'a>: Routing + 'a
    where
        Self: 'a;

    fn guid(&self) -> &str;
    fn info(&self) -> DawResult<crate::ProjectInfo>;

    // `transport()`, `regions()`, `markers()` accessors retired
    // alongside their architect::rpc ports — mount the singletons via
    // `{transport,region,marker}::serve(Reaper)` or call the
    // architect-emitted clients directly with a ProjectContext.
    fn tempo_map(&self) -> Self::TempoMap<'_>;
    // `tracks()` accessor retired alongside the architect::rpc port —
    // mount the singleton via `track::serve(Reaper)` or call
    // `TracksClient` directly with a ProjectContext.
    fn ext_state(&self) -> Self::ExtState<'_>;
    fn fx_chains(&self) -> Self::FxChains<'_>;
    fn fx_params(&self) -> Self::FxParams<'_>;
    fn items(&self) -> Self::Items<'_>;
    fn takes(&self) -> Self::Takes<'_>;
    fn routing(&self) -> Self::Routing<'_>;
}
