//! `StandaloneItems` — sync media-item sub-handle.

use daw_proto::{DawError, DawResult, Duration, Item, PositionInSeconds, sync::Items};

use super::daw::{ItemEntry, Standalone};

pub struct StandaloneItems<'a> {
    daw: &'a Standalone,
    project_guid: String,
}

impl<'a> StandaloneItems<'a> {
    pub(crate) fn new(daw: &'a Standalone, project_guid: String) -> Self {
        Self { daw, project_guid }
    }
}

impl<'a> Items for StandaloneItems<'a> {
    fn list(&self, track_guid: &str) -> Vec<Item> {
        self.daw
            .with_project(&self.project_guid, |p| {
                p.items_by_track
                    .get(track_guid)
                    .map(|guids| {
                        guids
                            .iter()
                            .filter_map(|g| p.items.get(g).map(|e| e.item.clone()))
                            .collect()
                    })
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }

    fn count(&self, track_guid: &str) -> u32 {
        self.daw
            .with_project(&self.project_guid, |p| {
                p.items_by_track
                    .get(track_guid)
                    .map(|v| v.len() as u32)
                    .unwrap_or(0)
            })
            .unwrap_or(0)
    }

    fn get(&self, item_guid: &str) -> Option<Item> {
        self.daw
            .with_project(&self.project_guid, |p| {
                p.items.get(item_guid).map(|e| e.item.clone())
            })
            .ok()
            .flatten()
    }

    fn add(&self, track_guid: &str, position: f64, length: f64) -> DawResult<String> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            if !p.tracks.iter().any(|t| t.guid == track_guid) {
                return Err(DawError::not_found("Track", track_guid));
            }
            let counter = p.next_item_counter;
            p.next_item_counter += 1;
            let guid = format!("standalone-item-{:016x}", counter);

            let order = p.items_by_track.entry(track_guid.to_string()).or_default();
            let index = order.len() as u32;
            order.push(guid.clone());

            let mut item = Item::default();
            item.guid = guid.clone();
            item.track_guid = track_guid.to_string();
            item.index = index;
            item.position = PositionInSeconds::from_seconds(position);
            item.length = Duration::from_seconds(length);

            p.items.insert(guid.clone(), ItemEntry { item });
            Ok(guid)
        })?
    }

    fn remove(&self, item_guid: &str) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let entry = p
                .items
                .remove(item_guid)
                .ok_or_else(|| DawError::not_found("Item", item_guid))?;
            let track_guid = entry.item.track_guid.clone();
            if let Some(order) = p.items_by_track.get_mut(&track_guid) {
                order.retain(|g| g != item_guid);
                for (i, g) in order.iter().enumerate() {
                    if let Some(e) = p.items.get_mut(g) {
                        e.item.index = i as u32;
                    }
                }
            }
            p.takes.remove(item_guid);
            Ok::<(), DawError>(())
        })?
    }

    fn duplicate(&self, item_guid: &str) -> DawResult<String> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let src = p
                .items
                .get(item_guid)
                .ok_or_else(|| DawError::not_found("Item", item_guid))?
                .clone();
            let counter = p.next_item_counter;
            p.next_item_counter += 1;
            let new_guid = format!("standalone-item-{:016x}", counter);

            let order = p
                .items_by_track
                .entry(src.item.track_guid.clone())
                .or_default();
            let new_index = order.len() as u32;
            order.push(new_guid.clone());

            let mut new_item = src.item.clone();
            new_item.guid = new_guid.clone();
            new_item.index = new_index;

            p.items
                .insert(new_guid.clone(), ItemEntry { item: new_item });
            Ok(new_guid)
        })?
    }

    fn set_position(&self, item_guid: &str, position: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let e = p
                .items
                .get_mut(item_guid)
                .ok_or_else(|| DawError::not_found("Item", item_guid))?;
            e.item.position = PositionInSeconds::from_seconds(position);
            Ok::<(), DawError>(())
        })?
    }

    fn set_length(&self, item_guid: &str, length: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let e = p
                .items
                .get_mut(item_guid)
                .ok_or_else(|| DawError::not_found("Item", item_guid))?;
            e.item.length = Duration::from_seconds(length);
            Ok::<(), DawError>(())
        })?
    }

    fn set_muted(&self, item_guid: &str, muted: bool) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let e = p
                .items
                .get_mut(item_guid)
                .ok_or_else(|| DawError::not_found("Item", item_guid))?;
            e.item.muted = muted;
            Ok::<(), DawError>(())
        })?
    }

    fn set_color(&self, item_guid: &str, color: u32) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let e = p
                .items
                .get_mut(item_guid)
                .ok_or_else(|| DawError::not_found("Item", item_guid))?;
            e.item.color = Some(color);
            Ok::<(), DawError>(())
        })?
    }
}
