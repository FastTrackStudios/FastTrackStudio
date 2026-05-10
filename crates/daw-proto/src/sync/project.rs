use crate::DawResult;

use super::{
    ExtState, FxChains, FxParams, Items, Markers, Regions, Routing, Takes, TempoMap, Tracks,
    Transport,
};

/// Per-project sync handle. Owns sub-domain accessors.
pub trait Project {
    type Transport<'a>: Transport + 'a
    where
        Self: 'a;
    type Regions<'a>: Regions + 'a
    where
        Self: 'a;
    type Markers<'a>: Markers + 'a
    where
        Self: 'a;
    type TempoMap<'a>: TempoMap + 'a
    where
        Self: 'a;
    type Tracks<'a>: Tracks + 'a
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
    fn markers(&self) -> Self::Markers<'_>;
    fn tempo_map(&self) -> Self::TempoMap<'_>;
    fn tracks(&self) -> Self::Tracks<'_>;
    fn ext_state(&self) -> Self::ExtState<'_>;
    fn fx_chains(&self) -> Self::FxChains<'_>;
    fn fx_params(&self) -> Self::FxParams<'_>;
    fn items(&self) -> Self::Items<'_>;
    fn takes(&self) -> Self::Takes<'_>;
    fn routing(&self) -> Self::Routing<'_>;
}
