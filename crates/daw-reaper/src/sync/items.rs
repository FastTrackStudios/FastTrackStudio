//! Sync items handle: [`ReaperItems`].

use daw_proto::sync::Items as ItemsTrait;
use daw_proto::{DawError, DawResult, Item, ItemRef};
use reaper_high::Reaper;
use reaper_medium::{
    DurationInSeconds, ItemAttributeKey, MediaItem, PositionInSeconds,
    ProjectContext as ReaperProjectContext, UiRefreshBehavior,
};

use crate::item::ReaperItem;
use crate::safe_wrappers::item as item_sw;

use super::ReaperMainThread;

pub struct ReaperItems<'a> {
    _mt: &'a ReaperMainThread,
    guid: &'a str,
}

impl<'a> ReaperItems<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread, guid: &'a str) -> Self {
        Self { _mt: mt, guid }
    }

    /// Get the medium-level project context for this handle.
    fn ctx(&self) -> Option<ReaperProjectContext> {
        super::resolve_reaper_ctx(self.guid).ok()
    }

    /// Resolve a media item by guid using the helper baked into `ReaperItem`.
    fn resolve_by_guid(&self, item_guid: &str) -> Option<MediaItem> {
        let ctx = self.ctx()?;
        ReaperItem::resolve_item(&ItemRef::Guid(item_guid.to_string()), ctx)
    }
}

impl<'a> ItemsTrait for ReaperItems<'a> {
    fn list(&self, track_guid: &str) -> Vec<Item> {
        let Ok(project) = super::resolve_project(self.guid) else {
            return Vec::new();
        };
        let Some(track) = super::find_track_by_guid(&project, track_guid) else {
            return Vec::new();
        };
        let medium = Reaper::get().medium_reaper();
        let Ok(raw_track) = track.raw() else {
            return Vec::new();
        };
        let count = item_sw::count_track_media_items(medium, raw_track);
        let mut out = Vec::with_capacity(count as usize);
        for i in 0..count {
            if let Some(item) = item_sw::get_track_media_item(medium, raw_track, i)
                && let Some(mut info) = ReaperItem::media_item_to_item(item)
            {
                info.index = i;
                out.push(info);
            }
        }
        out
    }

    fn count(&self, track_guid: &str) -> u32 {
        let Ok(project) = super::resolve_project(self.guid) else {
            return 0;
        };
        let Some(track) = super::find_track_by_guid(&project, track_guid) else {
            return 0;
        };
        track.item_count()
    }

    fn get(&self, item_guid: &str) -> Option<Item> {
        let item = self.resolve_by_guid(item_guid)?;
        ReaperItem::media_item_to_item(item)
    }

    fn add(&self, track_guid: &str, position: f64, length: f64) -> DawResult<String> {
        let project = super::resolve_project(self.guid)?;
        let track = super::find_track_by_guid(&project, track_guid)
            .ok_or_else(|| DawError::not_found("Track", track_guid))?;
        let raw_track = track
            .raw()
            .map_err(|e| DawError::invalid_object("Track", &format!("{e:?}")))?;
        let low = Reaper::get().medium_reaper().low();
        let start = position;
        let end = position + length;
        let item = crate::safe_wrappers::midi::create_new_midi_item(low, raw_track, start, end)
            .ok_or_else(|| DawError::operation_failed("create_new_midi_item failed"))?;
        Reaper::get().medium_reaper().update_timeline();
        Ok(crate::item::item_guid_string(
            Reaper::get().medium_reaper(),
            item,
        ))
    }

    fn remove(&self, item_guid: &str) -> DawResult<()> {
        let item = self
            .resolve_by_guid(item_guid)
            .ok_or_else(|| DawError::not_found("Item", item_guid))?;
        let medium = Reaper::get().medium_reaper();
        let track = item_sw::get_media_item_track(medium, item)
            .ok_or_else(|| DawError::operation_failed("get_media_item_track returned None"))?;
        item_sw::delete_track_media_item(medium, track, item);
        Ok(())
    }

    fn duplicate(&self, item_guid: &str) -> DawResult<String> {
        // REAPER has no direct "duplicate item" API; the existing async impl
        // uses action 41295 with selection juggling and only returns the new
        // item's pointer (no stable GUID). Skip for the sync surface.
        Err(DawError::not_supported(format!(
            "Items::duplicate (item={item_guid}) is not directly supported by reaper-rs; \
             use the async ItemService or a chunk-copy fallback"
        )))
    }

    fn set_position(&self, item_guid: &str, position: f64) -> DawResult<()> {
        let item = self
            .resolve_by_guid(item_guid)
            .ok_or_else(|| DawError::not_found("Item", item_guid))?;
        let medium = Reaper::get().medium_reaper();
        let pos = PositionInSeconds::new(position)
            .map_err(|e| DawError::operation_failed(format!("invalid position: {e:?}")))?;
        item_sw::set_media_item_position(medium, item, pos, UiRefreshBehavior::Refresh);
        Ok(())
    }

    fn set_length(&self, item_guid: &str, length: f64) -> DawResult<()> {
        let item = self
            .resolve_by_guid(item_guid)
            .ok_or_else(|| DawError::not_found("Item", item_guid))?;
        let medium = Reaper::get().medium_reaper();
        let len = DurationInSeconds::new(length)
            .map_err(|e| DawError::operation_failed(format!("invalid length: {e:?}")))?;
        item_sw::set_media_item_length(medium, item, len, UiRefreshBehavior::Refresh);
        Ok(())
    }

    fn set_muted(&self, item_guid: &str, muted: bool) -> DawResult<()> {
        let item = self
            .resolve_by_guid(item_guid)
            .ok_or_else(|| DawError::not_found("Item", item_guid))?;
        let medium = Reaper::get().medium_reaper();
        item_sw::set_item_info_value(
            medium,
            item,
            ItemAttributeKey::Mute,
            if muted { 1.0 } else { 0.0 },
        );
        Ok(())
    }

    fn set_color(&self, item_guid: &str, color: u32) -> DawResult<()> {
        let item = self
            .resolve_by_guid(item_guid)
            .ok_or_else(|| DawError::not_found("Item", item_guid))?;
        let medium = Reaper::get().medium_reaper();
        // Bit 24 (0x01000000) tells REAPER to use the custom color.
        let value = if color == 0 {
            0i32
        } else {
            (color as i32) | 0x01000000
        };
        item_sw::set_item_info_value(medium, item, ItemAttributeKey::CustomColor, value as f64);
        Ok(())
    }
}
