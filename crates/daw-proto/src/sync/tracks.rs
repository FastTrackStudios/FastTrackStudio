use crate::{DawResult, Track};

pub trait Tracks {
    fn all(&self) -> Vec<Track>;
    fn count(&self) -> u32;
    fn by_guid(&self, guid: &str) -> Option<Track>;
    fn selected(&self) -> Vec<Track>;
    fn master(&self) -> DawResult<Track>;

    /// Insert a new track. `at_index` of `None` appends at the end.
    /// Returns the new track's GUID.
    fn add(&self, name: &str, at_index: Option<u32>) -> DawResult<String>;
    fn remove(&self, guid: &str) -> DawResult<()>;
    fn remove_all(&self) -> DawResult<()>;

    fn set_muted(&self, guid: &str, muted: bool) -> DawResult<()>;
    fn set_soloed(&self, guid: &str, soloed: bool) -> DawResult<()>;
    fn set_volume(&self, guid: &str, volume: f64) -> DawResult<()>;
    fn set_pan(&self, guid: &str, pan: f64) -> DawResult<()>;
    fn rename(&self, guid: &str, name: &str) -> DawResult<()>;
    fn set_color(&self, guid: &str, color: u32) -> DawResult<()>;

    /// Per-track ext state (`P_EXT:section:key`).
    fn get_ext_state(&self, guid: &str, section: &str, key: &str) -> Option<String>;
    fn set_ext_state(&self, guid: &str, section: &str, key: &str, value: &str) -> DawResult<()>;
}
