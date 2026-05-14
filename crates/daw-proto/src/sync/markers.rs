use crate::{DawResult, Marker};

/// Markers service surface.
///
/// `#[architect::rpc]` (re-exported here from `architect_rpc_derive`)
/// derives an async vox client + server-side host alongside this
/// trait. Backends (REAPER live, standalone, dawfile editors)
/// implement `Markers` directly; in-process callers use it as a plain
/// sync API, and remote callers reach `MarkersClient` over vox. See
/// `architect/DESIGN.md` for the full pattern.
#[architect_rpc_derive::rpc]
pub trait Markers {
    fn all(&self) -> Vec<Marker>;
    fn get(&self, id: u32) -> Option<Marker>;
    fn count(&self) -> u32;

    fn add(&self, position: f64, name: &str) -> DawResult<u32>;
    fn remove(&self, id: u32) -> DawResult<()>;
    fn set_position(&self, id: u32, position: f64) -> DawResult<()>;
    fn rename(&self, id: u32, name: &str) -> DawResult<()>;
    fn set_color(&self, id: u32, color: u32) -> DawResult<()>;
}
