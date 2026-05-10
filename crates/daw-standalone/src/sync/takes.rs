//! `StandaloneTakes` — sync takes sub-handle.

use daw_proto::{DawError, DawResult, Take, sync::Takes};

use super::daw::Standalone;

pub struct StandaloneTakes<'a> {
    daw: &'a Standalone,
    project_guid: String,
}

impl<'a> StandaloneTakes<'a> {
    pub(crate) fn new(daw: &'a Standalone, project_guid: String) -> Self {
        Self { daw, project_guid }
    }
}

impl<'a> Takes for StandaloneTakes<'a> {
    fn list(&self, item_guid: &str) -> Vec<Take> {
        self.daw
            .with_project(&self.project_guid, |p| {
                p.takes
                    .get(item_guid)
                    .map(|tl| tl.takes.clone())
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }

    fn count(&self, item_guid: &str) -> u32 {
        self.daw
            .with_project(&self.project_guid, |p| {
                p.takes
                    .get(item_guid)
                    .map(|tl| tl.takes.len() as u32)
                    .unwrap_or(0)
            })
            .unwrap_or(0)
    }

    fn active(&self, item_guid: &str) -> Option<Take> {
        self.daw
            .with_project(&self.project_guid, |p| {
                p.takes
                    .get(item_guid)
                    .and_then(|tl| tl.takes.get(tl.active_idx as usize).cloned())
            })
            .ok()
            .flatten()
    }

    fn get(&self, take_guid: &str) -> Option<Take> {
        self.daw
            .with_project(&self.project_guid, |p| {
                for tl in p.takes.values() {
                    if let Some(t) = tl.takes.iter().find(|t| t.guid == take_guid) {
                        return Some(t.clone());
                    }
                }
                None
            })
            .ok()
            .flatten()
    }

    fn set_active(&self, item_guid: &str, take_idx: u32) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let tl = p
                .takes
                .get_mut(item_guid)
                .ok_or_else(|| DawError::not_found("Item", item_guid))?;
            if (take_idx as usize) >= tl.takes.len() {
                return Err(DawError::not_found("Take", &take_idx.to_string()));
            }
            tl.active_idx = take_idx;
            for (i, t) in tl.takes.iter_mut().enumerate() {
                t.is_active = i as u32 == take_idx;
            }
            // Mirror onto the item if present.
            if let Some(item_entry) = p.items.get_mut(item_guid) {
                item_entry.item.active_take_index = take_idx;
            }
            Ok::<(), DawError>(())
        })?
    }

    fn rename(&self, take_guid: &str, name: &str) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            for tl in p.takes.values_mut() {
                if let Some(t) = tl.takes.iter_mut().find(|t| t.guid == take_guid) {
                    t.name = name.to_string();
                    return Ok(());
                }
            }
            Err(DawError::not_found("Take", take_guid))
        })?
    }
}
