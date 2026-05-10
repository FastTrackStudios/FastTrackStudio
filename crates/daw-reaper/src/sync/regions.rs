//! Sync regions handle: [`ReaperRegions`].

use std::ffi::CString;

use daw_proto::sync::Regions as RegionsTrait;
use daw_proto::{DawError, DawResult, Region, TimeRange};
use reaper_high::Reaper;
use reaper_medium::{
    MarkerOrRegionPosition, PositionInSeconds, ProjectContext as ReaperProjectContext,
};

use crate::safe_wrappers::markers as sw;
use crate::safe_wrappers::ruler_lanes;

use super::ReaperMainThread;

pub struct ReaperRegions<'a> {
    _mt: &'a ReaperMainThread,
    guid: &'a str,
}

impl<'a> ReaperRegions<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread, guid: &'a str) -> Self {
        Self { _mt: mt, guid }
    }

    fn ctx(&self) -> DawResult<ReaperProjectContext> {
        super::resolve_reaper_ctx(self.guid)
    }

    fn ctx_or_current(&self) -> ReaperProjectContext {
        self.ctx().unwrap_or(ReaperProjectContext::CurrentProject)
    }
}

impl<'a> RegionsTrait for ReaperRegions<'a> {
    fn all(&self) -> Vec<Region> {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();
        let low = medium.low();
        let ctx = self.ctx_or_current();
        let mut regions = Vec::new();

        let total_count = medium.count_project_markers(ctx).total_count;
        for idx in 0..total_count {
            medium.enum_project_markers_3(ctx, idx, |result| {
                if let Some(info) = result
                    && let Some(end_pos) = info.region_end_position
                {
                    let id = info.id.get();
                    let lane = ruler_lanes::assigned_lane(low, ctx, true, id)
                        .or_else(|| ruler_lanes::get_marker_lane(low, ctx, idx));
                    regions.push(Region {
                        id: Some(id),
                        time_range: TimeRange::from_seconds(info.position.get(), end_pos.get()),
                        name: info.name.to_string(),
                        color: {
                            let c = info.color.to_raw();
                            if c != 0 { Some(c as u32) } else { None }
                        },
                        guid: None,
                        lane,
                    });
                }
            });
        }

        regions.sort_by(|a, b| {
            a.start_seconds()
                .partial_cmp(&b.start_seconds())
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        regions
    }

    fn get(&self, id: u32) -> Option<Region> {
        self.all().into_iter().find(|r| r.id == Some(id))
    }

    fn count(&self) -> u32 {
        let medium = Reaper::get().medium_reaper();
        let ctx = self.ctx_or_current();
        let total = medium.count_project_markers(ctx).total_count;
        let mut n = 0u32;
        for idx in 0..total {
            medium.enum_project_markers_3(ctx, idx, |result| {
                if let Some(info) = result
                    && info.region_end_position.is_some()
                {
                    n += 1;
                }
            });
        }
        n
    }

    fn add(&self, start: f64, end: f64, name: &str) -> DawResult<u32> {
        let ctx = self.ctx()?;
        let medium = Reaper::get().medium_reaper();
        let start_pos = PositionInSeconds::new(start)
            .map_err(|e| DawError::operation_failed(format!("invalid start: {e:?}")))?;
        let end_pos = PositionInSeconds::new(end)
            .map_err(|e| DawError::operation_failed(format!("invalid end: {e:?}")))?;
        let id = medium
            .add_project_marker_2(
                ctx,
                MarkerOrRegionPosition::Region(start_pos, end_pos),
                name,
                None,
                None,
            )
            .map_err(|e| DawError::operation_failed(format!("add region failed: {e:?}")))?;
        Ok(id)
    }

    fn remove(&self, id: u32) -> DawResult<()> {
        let ctx = self.ctx()?;
        let low = Reaper::get().medium_reaper().low();
        sw::delete_project_marker(low, ctx, id as i32, true);
        Ok(())
    }

    fn set_bounds(&self, id: u32, start: f64, end: f64) -> DawResult<()> {
        // Validate that the project exists.
        let _ = self.ctx()?;
        let low = Reaper::get().medium_reaper().low();
        sw::set_project_marker(low, id as i32, true, start, end, None);
        Ok(())
    }

    fn rename(&self, id: u32, name: &str) -> DawResult<()> {
        let ctx = self.ctx()?;
        let medium = Reaper::get().medium_reaper();
        let low = medium.low();
        let total_count = medium.count_project_markers(ctx).total_count;
        let cname = CString::new(name)
            .map_err(|e| DawError::operation_failed(format!("invalid name: {e}")))?;

        let mut found = false;
        for idx in 0..total_count {
            medium.enum_project_markers_3(ctx, idx, |result| {
                if let Some(info) = result
                    && let Some(end_pos) = info.region_end_position
                    && info.id.get() == id
                {
                    sw::set_project_marker(
                        low,
                        id as i32,
                        true,
                        info.position.get(),
                        end_pos.get(),
                        Some(&cname),
                    );
                    found = true;
                }
            });
            if found {
                break;
            }
        }
        if !found {
            return Err(DawError::not_found("Region", &id.to_string()));
        }
        Ok(())
    }

    fn set_color(&self, id: u32, color: u32) -> DawResult<()> {
        let ctx = self.ctx()?;
        let medium = Reaper::get().medium_reaper();
        let low = medium.low();
        let total_count = medium.count_project_markers(ctx).total_count;
        let reaper_color = (color | 0x01000000) as i32;

        let mut found = false;
        for idx in 0..total_count {
            medium.enum_project_markers_3(ctx, idx, |result| {
                if let Some(info) = result
                    && let Some(end_pos) = info.region_end_position
                    && info.id.get() == id
                    && let Ok(name) = CString::new(info.name.to_string())
                {
                    sw::set_project_marker_by_index2(
                        low,
                        ctx,
                        idx as i32,
                        true,
                        info.position.get(),
                        end_pos.get(),
                        id as i32,
                        Some(&name),
                        reaper_color,
                        0,
                    );
                    found = true;
                }
            });
            if found {
                break;
            }
        }
        if !found {
            return Err(DawError::not_found("Region", &id.to_string()));
        }
        Ok(())
    }
}
