//! Takes within a media item.

use crate::{DawResult, Take};

pub trait Takes {
    fn list(&self, item_guid: &str) -> Vec<Take>;
    fn count(&self, item_guid: &str) -> u32;
    fn active(&self, item_guid: &str) -> Option<Take>;
    fn get(&self, take_guid: &str) -> Option<Take>;

    fn set_active(&self, item_guid: &str, take_idx: u32) -> DawResult<()>;
    fn rename(&self, take_guid: &str, name: &str) -> DawResult<()>;
}
