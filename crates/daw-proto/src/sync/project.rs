use crate::DawResult;

/// Per-project sync handle. Owns sub-domain accessors.
///
/// `Markers`, `Regions`, `Tracks`, `Transport`, `TempoMap` have all
/// been lifted out by their architect::rpc ports. Other accessors
/// follow as their services are ported.
pub trait Project {
    fn guid(&self) -> &str;
    fn info(&self) -> DawResult<crate::ProjectInfo>;

    // `transport()`, `regions()`, `markers()`, `tracks()`, `tempo_map()`
    // accessors retired by their architect::rpc ports — mount the
    // singletons via `<feature>::serve(Reaper)` or use the architect-
    // emitted clients directly with a ProjectContext.
}
