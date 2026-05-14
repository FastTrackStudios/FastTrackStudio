//! `impl Regions for Reaper` — sync trait + REAPER C API.
//!
//! Mounting goes through `daw_proto::region::serve(Reaper)`. The
//! dispatcher (REAPER's main thread queue) is pulled off the backend
//! via `HasDispatcher` on `Reaper`. Each method assumes it's running
//! on the main thread — the bridge enforces that contract.
//!
//! Helper `get_regions_on_main_thread` stays public for callers that
//! already hold a main-thread proof (no need to go through the
//! singleton trait).

use std::ffi::CString;

use daw_proto::sync::Regions;
use daw_proto::{DawError, DawResult, ProjectContext, Region, TimeRange};
use reaper_high::Reaper as ReaperHigh;
use reaper_medium::{
    MarkerOrRegionPosition, PositionInSeconds, ProjectContext as ReaperProjectContext,
};

use crate::project_context::resolve_project_context;
use crate::safe_wrappers::markers as sw;
use crate::safe_wrappers::ruler_lanes;

// ── Public sync helper ────────────────────────────────────────────────

/// Read all regions from the current project, sorted by start position.
/// Must be called from the main thread.
pub fn get_regions_on_main_thread() -> Vec<Region> {
    read_regions(ReaperProjectContext::CurrentProject)
}

fn read_regions(ctx: ReaperProjectContext) -> Vec<Region> {
    let reaper = ReaperHigh::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();
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

// ── Tracks impl ───────────────────────────────────────────────────────

fn not_found_region() -> DawError {
    DawError::not_found("Region", "")
}

impl Regions for crate::Reaper {
    fn all(&self, project: ProjectContext) -> Vec<Region> {
        read_regions(resolve_project_context(&project))
    }

    fn get(&self, project: ProjectContext, id: u32) -> Option<Region> {
        read_regions(resolve_project_context(&project))
            .into_iter()
            .find(|r| r.id == Some(id))
    }

    fn count(&self, project: ProjectContext) -> u32 {
        let medium = ReaperHigh::get().medium_reaper();
        let ctx = resolve_project_context(&project);
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

    fn add(&self, project: ProjectContext, start: f64, end: f64, name: &str) -> DawResult<u32> {
        let ctx = resolve_project_context(&project);
        let medium = ReaperHigh::get().medium_reaper();
        let start_pos = PositionInSeconds::new(start)
            .map_err(|e| DawError::operation_failed(format!("invalid start position: {e:?}")))?;
        let end_pos = PositionInSeconds::new(end)
            .map_err(|e| DawError::operation_failed(format!("invalid end position: {e:?}")))?;
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

    fn remove(&self, project: ProjectContext, id: u32) -> DawResult<()> {
        let ctx = resolve_project_context(&project);
        let low = ReaperHigh::get().medium_reaper().low();
        sw::delete_project_marker(low, ctx, id as i32, true);
        Ok(())
    }

    fn set_bounds(&self, _project: ProjectContext, id: u32, start: f64, end: f64) -> DawResult<()> {
        let low = ReaperHigh::get().medium_reaper().low();
        sw::set_project_marker(low, id as i32, true, start, end, None);
        Ok(())
    }

    fn rename(&self, project: ProjectContext, id: u32, name: &str) -> DawResult<()> {
        let ctx = resolve_project_context(&project);
        let reaper = ReaperHigh::get();
        let medium = reaper.medium_reaper();
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
            return Err(not_found_region());
        }
        Ok(())
    }

    fn set_color(&self, project: ProjectContext, id: u32, color: u32) -> DawResult<()> {
        let ctx = resolve_project_context(&project);
        let medium = ReaperHigh::get().medium_reaper();
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
            return Err(not_found_region());
        }
        Ok(())
    }
}
