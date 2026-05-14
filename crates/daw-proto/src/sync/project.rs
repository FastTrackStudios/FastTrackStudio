use crate::DawResult;

use super::{ExtState, FxChains, FxParams, Items, Regions, Routing, Takes, TempoMap, Transport};

/// Per-project sync handle. Owns sub-domain accessors.
///
/// `Markers` was lifted out of this trait when it got ported to
/// `#[architect::rpc]`: backends now hold no per-project state, so
/// `marker::serve(Reaper)` mounts once per binary and serves every
/// project. Other accessors will follow as their services are ported.
pub trait Project {
    type Transport<'a>: Transport + 'a
    where
        Self: 'a;
    type Regions<'a>: Regions + 'a
    where
        Self: 'a;
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

    fn transport(&self) -> Self::Transport<'_>;
    fn regions(&self) -> Self::Regions<'_>;
    // `markers()` accessor retired — use `marker::serve(Reaper)` to
    // mount the backend, or call directly via the architect-emitted
    // `MarkersClient` for the project guid.
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
