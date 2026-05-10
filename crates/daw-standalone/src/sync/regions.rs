//! `StandaloneRegions` — sync regions sub-handle.

use daw_proto::{DawError, DawResult, Region, TimeRange, sync::Regions};

use super::daw::Standalone;

pub struct StandaloneRegions<'a> {
    daw: &'a Standalone,
    guid: String,
}

impl<'a> StandaloneRegions<'a> {
    pub(crate) fn new(daw: &'a Standalone, guid: String) -> Self {
        Self { daw, guid }
    }
}

impl<'a> Regions for StandaloneRegions<'a> {
    fn all(&self) -> Vec<Region> {
        self.daw
            .with_project(&self.guid, |p| p.regions.values().cloned().collect())
            .unwrap_or_default()
    }

    fn get(&self, id: u32) -> Option<Region> {
        self.daw
            .with_project(&self.guid, |p| p.regions.get(&id).cloned())
            .ok()
            .flatten()
    }

    fn count(&self) -> u32 {
        self.daw
            .with_project(&self.guid, |p| p.regions.len() as u32)
            .unwrap_or(0)
    }

    fn add(&self, start: f64, end: f64, name: &str) -> DawResult<u32> {
        self.daw.with_project_mut(&self.guid, |p| {
            let id = p.next_region_id;
            p.next_region_id += 1;
            let mut region = Region::from_seconds(start, end, name.to_string());
            region.id = Some(id);
            p.regions.insert(id, region);
            id
        })
    }

    fn remove(&self, id: u32) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            p.regions
                .remove(&id)
                .map(|_| ())
                .ok_or_else(|| DawError::not_found("Region", &id.to_string()))
        })?
    }

    fn set_bounds(&self, id: u32, start: f64, end: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            let r = p
                .regions
                .get_mut(&id)
                .ok_or_else(|| DawError::not_found("Region", &id.to_string()))?;
            r.time_range = TimeRange::from_seconds(start, end);
            Ok::<(), DawError>(())
        })?
    }

    fn rename(&self, id: u32, name: &str) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            let r = p
                .regions
                .get_mut(&id)
                .ok_or_else(|| DawError::not_found("Region", &id.to_string()))?;
            r.name = name.to_string();
            Ok::<(), DawError>(())
        })?
    }

    fn set_color(&self, id: u32, color: u32) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            let r = p
                .regions
                .get_mut(&id)
                .ok_or_else(|| DawError::not_found("Region", &id.to_string()))?;
            r.color = Some(color);
            Ok::<(), DawError>(())
        })?
    }
}
