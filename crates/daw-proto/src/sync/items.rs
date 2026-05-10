//! Media items on tracks.
//!
//! Items are addressed by their own GUID after creation; collection ops scope
//! to a track guid.

use crate::{DawResult, Item};

pub trait Items {
    fn list(&self, track_guid: &str) -> Vec<Item>;
    fn count(&self, track_guid: &str) -> u32;
    fn get(&self, item_guid: &str) -> Option<Item>;

    fn add(&self, track_guid: &str, position: f64, length: f64) -> DawResult<String>;
    fn remove(&self, item_guid: &str) -> DawResult<()>;
    fn duplicate(&self, item_guid: &str) -> DawResult<String>;

    fn set_position(&self, item_guid: &str, position: f64) -> DawResult<()>;
    fn set_length(&self, item_guid: &str, length: f64) -> DawResult<()>;
    fn set_muted(&self, item_guid: &str, muted: bool) -> DawResult<()>;
    fn set_color(&self, item_guid: &str, color: u32) -> DawResult<()>;
}
