use crate::DawResult;

use super::{ExtState, FxChains, FxParams, Items, Routing};

/// Per-project sync handle. Owns sub-domain accessors.
///
/// `Markers`, `Regions`, `Tracks`, `Transport`, `TempoMap` have all
/// been lifted out by their architect::rpc ports. Other accessors
/// follow as their services are ported.
pub trait Project {
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
    type Routing<'a>: Routing + 'a
    where
        Self: 'a;

    fn guid(&self) -> &str;
    fn info(&self) -> DawResult<crate::ProjectInfo>;

    // `transport()`, `regions()`, `markers()`, `tracks()`, `tempo_map()`
    // accessors retired by their architect::rpc ports — mount the
    // singletons via `<feature>::serve(Reaper)` or use the architect-
    // emitted clients directly with a ProjectContext.
    fn ext_state(&self) -> Self::ExtState<'_>;
    fn fx_chains(&self) -> Self::FxChains<'_>;
    fn fx_params(&self) -> Self::FxParams<'_>;
    fn items(&self) -> Self::Items<'_>;
    fn routing(&self) -> Self::Routing<'_>;
}
