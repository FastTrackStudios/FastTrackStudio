//! Sync markers handle: [`ReaperMarkers`].

use std::ffi::CString;

use daw_proto::sync::Markers as MarkersTrait;
use daw_proto::{DawError, DawResult, Marker, Position, TimePosition};
use reaper_high::Reaper;
use reaper_medium::{
    MarkerOrRegionPosition, PositionInSeconds, ProjectContext as ReaperProjectContext,
};

use crate::safe_wrappers::markers as sw;
use crate::safe_wrappers::ruler_lanes;

use super::ReaperMainThread;

pub struct ReaperMarkers<'a> {
    _mt: &'a ReaperMainThread,
    guid: &'a str,
}

impl<'a> ReaperMarkers<'a> {
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

impl<'a> MarkersTrait for ReaperMarkers<'a> {
    fn all(&self) -> Vec<Marker> {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();
        let low = medium.low();
        let ctx = self.ctx_or_current();
        let mut markers = Vec::new();

        let total_count = medium.count_project_markers(ctx).total_count;
        for idx in 0..total_count {
            medium.enum_project_markers_3(ctx, idx, |result| {
                if let Some(info) = result
                    && info.region_end_position.is_none()
                {
                    let id = info.id.get();
                    let lane = ruler_lanes::assigned_lane(low, ctx, false, id)
                        .or_else(|| ruler_lanes::get_marker_lane(low, ctx, idx));
                    markers.push(Marker {
                        id: Some(id),
                        position: Position::from_time(TimePosition::from_seconds(
                            info.position.get(),
                        )),
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

        markers.sort_by(|a, b| {
            a.position_seconds()
                .partial_cmp(&b.position_seconds())
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        markers
    }

    fn get(&self, id: u32) -> Option<Marker> {
        self.all().into_iter().find(|m| m.id == Some(id))
    }

    fn count(&self) -> u32 {
        let medium = Reaper::get().medium_reaper();
        let ctx = self.ctx_or_current();
        let total = medium.count_project_markers(ctx).total_count;
        let mut n = 0u32;
        for idx in 0..total {
            medium.enum_project_markers_3(ctx, idx, |result| {
                if let Some(info) = result
                    && info.region_end_position.is_none()
                {
                    n += 1;
                }
            });
        }
        n
    }

    fn add(&self, position: f64, name: &str) -> DawResult<u32> {
        let ctx = self.ctx()?;
        let medium = Reaper::get().medium_reaper();
        let pos = PositionInSeconds::new(position)
            .map_err(|e| DawError::operation_failed(format!("invalid position: {e:?}")))?;
        let id = medium
            .add_project_marker_2(ctx, MarkerOrRegionPosition::Marker(pos), name, None, None)
            .map_err(|e| DawError::operation_failed(format!("add marker failed: {e:?}")))?;
        Ok(id)
    }

    fn remove(&self, id: u32) -> DawResult<()> {
        let ctx = self.ctx()?;
        let low = Reaper::get().medium_reaper().low();
        sw::delete_project_marker(low, ctx, id as i32, false);
        Ok(())
    }

    fn set_position(&self, id: u32, position: f64) -> DawResult<()> {
        let _ = self.ctx()?;
        let low = Reaper::get().medium_reaper().low();
        sw::set_project_marker(low, id as i32, false, position, 0.0, None);
        Ok(())
    }

    fn rename(&self, id: u32, name: &str) -> DawResult<()> {
        let _ = self.ctx()?;
        let low = Reaper::get().medium_reaper().low();
        let cname = CString::new(name)
            .map_err(|e| DawError::operation_failed(format!("invalid name: {e}")))?;
        sw::set_project_marker(low, id as i32, false, -1.0, 0.0, Some(&cname));
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
                    && info.region_end_position.is_none()
                    && info.id.get() == id
                    && let Ok(name) = CString::new(info.name.to_string())
                {
                    sw::set_project_marker_by_index2(
                        low,
                        ctx,
                        idx as i32,
                        false,
                        info.position.get(),
                        0.0,
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
            return Err(DawError::not_found("Marker", &id.to_string()));
        }
        Ok(())
    }
}
