//! Sync takes handle: [`ReaperTakes`].

use std::ffi::CString;

use daw_proto::sync::Takes as TakesTrait;
use daw_proto::{DawError, DawResult, ItemRef, Take, TakeRef};
use reaper_high::Reaper;
use reaper_medium::{MediaItem, MediaItemTake, ProjectContext as ReaperProjectContext};

use crate::item::{ReaperItem, ReaperTake};
use crate::safe_wrappers::item as item_sw;

use super::ReaperMainThread;

pub struct ReaperTakes<'a> {
    _mt: &'a ReaperMainThread,
    guid: &'a str,
}

impl<'a> ReaperTakes<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread, guid: &'a str) -> Self {
        Self { _mt: mt, guid }
    }

    fn ctx(&self) -> Option<ReaperProjectContext> {
        super::resolve_reaper_ctx(self.guid).ok()
    }

    fn resolve_item_by_guid(&self, item_guid: &str) -> Option<MediaItem> {
        let ctx = self.ctx()?;
        ReaperItem::resolve_item(&ItemRef::Guid(item_guid.to_string()), ctx)
    }

    /// Walk every item in the resolved project tab looking for a take whose
    /// GUID matches. Returns `(item, take, index)` on hit.
    fn find_take_by_guid(&self, take_guid: &str) -> Option<(MediaItem, MediaItemTake, u32)> {
        let ctx = self.ctx()?;
        let medium = Reaper::get().medium_reaper();
        let low = medium.low();
        let total = medium.count_media_items(ctx);
        for i in 0..total {
            if let Some(item) = medium.get_media_item(ctx, i) {
                let count = item_sw::count_takes(low, item);
                for t in 0..count {
                    if let Some(take) = item_sw::get_take(low, item, t) {
                        let g = item_sw::get_take_guid_string(low, take);
                        if g == take_guid {
                            return Some((item, take, t as u32));
                        }
                    }
                }
            }
        }
        None
    }
}

impl<'a> TakesTrait for ReaperTakes<'a> {
    fn list(&self, item_guid: &str) -> Vec<Take> {
        let Some(item) = self.resolve_item_by_guid(item_guid) else {
            return Vec::new();
        };
        let low = Reaper::get().medium_reaper().low();
        let count = item_sw::count_takes(low, item);
        let mut out = Vec::with_capacity(count.max(0) as usize);
        for i in 0..count {
            if let Some(take) = item_sw::get_take(low, item, i)
                && let Some(t) = ReaperTake::media_take_to_take(item, take, i as u32)
            {
                out.push(t);
            }
        }
        out
    }

    fn count(&self, item_guid: &str) -> u32 {
        let Some(item) = self.resolve_item_by_guid(item_guid) else {
            return 0;
        };
        let low = Reaper::get().medium_reaper().low();
        item_sw::count_takes(low, item).max(0) as u32
    }

    fn active(&self, item_guid: &str) -> Option<Take> {
        let item = self.resolve_item_by_guid(item_guid)?;
        let medium = Reaper::get().medium_reaper();
        let low = medium.low();
        let take = item_sw::get_active_take(medium, item)?;
        // Find index by pointer comparison
        let mut idx = 0u32;
        let count = item_sw::count_takes(low, item);
        for i in 0..count {
            if item_sw::get_take(low, item, i) == Some(take) {
                idx = i as u32;
                break;
            }
        }
        ReaperTake::media_take_to_take(item, take, idx)
    }

    fn get(&self, take_guid: &str) -> Option<Take> {
        let (item, take, idx) = self.find_take_by_guid(take_guid)?;
        ReaperTake::media_take_to_take(item, take, idx)
    }

    fn set_active(&self, item_guid: &str, take_idx: u32) -> DawResult<()> {
        let item = self
            .resolve_item_by_guid(item_guid)
            .ok_or_else(|| DawError::not_found("Item", item_guid))?;
        let low = Reaper::get().medium_reaper().low();
        let take = ReaperTake::resolve_take(item, &TakeRef::Index(take_idx)).ok_or_else(|| {
            DawError::out_of_range(
                take_idx,
                item_sw::count_takes(low, item).max(0) as u32,
                "takes.take_idx",
            )
        })?;
        item_sw::set_active_take(low, take);
        Ok(())
    }

    fn rename(&self, take_guid: &str, name: &str) -> DawResult<()> {
        let (_item, take, _idx) = self
            .find_take_by_guid(take_guid)
            .ok_or_else(|| DawError::not_found("Take", take_guid))?;
        let low = Reaper::get().medium_reaper().low();
        let cname = CString::new(name)
            .map_err(|e| DawError::operation_failed(format!("invalid name: {e}")))?;
        item_sw::set_take_name(low, take, &cname);
        Ok(())
    }
}
