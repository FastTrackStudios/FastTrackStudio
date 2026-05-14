//! Sync tracks handle: [`ReaperTracks`].

use std::ffi::CString;

use daw_proto::sync::Tracks as TracksTrait;
use daw_proto::{DawError, DawResult, Track};
use reaper_high::{GroupingBehavior, Reaper};
use reaper_medium::GangBehavior;

use crate::track::build_track_info;

use super::ReaperMainThread;

pub struct ReaperTracks<'a> {
    _mt: &'a ReaperMainThread,
    guid: &'a str,
}

impl<'a> ReaperTracks<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread, guid: &'a str) -> Self {
        Self { _mt: mt, guid }
    }
}

/// Walk the flat track list and assign `parent_guid` using folder depth changes.
fn assign_parent_guids(tracks: &mut [Track]) {
    let mut folder_stack: Vec<String> = Vec::new();
    for track in tracks.iter_mut() {
        track.parent_guid = folder_stack.last().cloned();
        let depth = track.folder_depth;
        if depth > 0 {
            folder_stack.push(track.guid.clone());
        } else if depth < 0 {
            for _ in 0..depth.unsigned_abs() {
                folder_stack.pop();
            }
        }
    }
}

impl<'a> TracksTrait for ReaperTracks<'a> {
    fn all(&self) -> Vec<Track> {
        let Ok(project) = super::resolve_project(self.guid) else {
            return Vec::new();
        };
        let mut tracks: Vec<Track> = project.tracks().map(|t| build_track_info(&t)).collect();
        assign_parent_guids(&mut tracks);
        tracks
    }

    fn count(&self) -> u32 {
        super::resolve_project(self.guid)
            .map(|p| p.track_count())
            .unwrap_or(0)
    }

    fn by_guid(&self, guid: &str) -> Option<Track> {
        let project = super::resolve_project(self.guid).ok()?;
        let track = super::find_track_by_guid(&project, guid)?;
        Some(build_track_info(&track))
    }

    fn selected(&self) -> Vec<Track> {
        let Ok(project) = super::resolve_project(self.guid) else {
            return Vec::new();
        };
        let mut tracks: Vec<Track> = project
            .tracks()
            .filter(|t| t.is_selected())
            .map(|t| build_track_info(&t))
            .collect();
        // parent_guid only makes sense relative to the full track list; selected
        // tracks aren't necessarily contiguous, so we leave parent_guid as-is.
        // For consistency with `all()`, populate from the full list.
        let mut all: Vec<Track> = project.tracks().map(|t| build_track_info(&t)).collect();
        assign_parent_guids(&mut all);
        for sel in tracks.iter_mut() {
            if let Some(full) = all.iter().find(|t| t.guid == sel.guid) {
                sel.parent_guid = full.parent_guid.clone();
            }
        }
        tracks
    }

    fn add(&self, name: &str, at_index: Option<u32>) -> DawResult<String> {
        let project = super::resolve_project(self.guid)?;
        let idx = at_index.unwrap_or_else(|| project.track_count());
        let track = project
            .insert_track_at(idx)
            .map_err(|e| DawError::operation_failed(format!("insert_track_at: {e:?}")))?;
        if !name.is_empty() {
            track.set_name(name);
        }
        Ok(track.guid().to_string_without_braces())
    }

    fn remove(&self, guid: &str) -> DawResult<()> {
        let project = super::resolve_project(self.guid)?;
        let track = super::find_track_by_guid(&project, guid)
            .ok_or_else(|| DawError::not_found("Track", guid))?;
        project.remove_track(&track);
        Ok(())
    }

    fn remove_all(&self) -> DawResult<()> {
        let project = super::resolve_project(self.guid)?;
        while project.track_count() > 0 {
            if let Some(track) = project.track_by_index(project.track_count() - 1) {
                project.remove_track(&track);
            } else {
                break;
            }
        }
        Ok(())
    }

    fn master(&self) -> DawResult<Track> {
        let project = super::resolve_project(self.guid)?;
        let master = project
            .master_track()
            .map_err(|e| DawError::operation_failed(format!("master_track failed: {e:?}")))?;
        Ok(build_track_info(&master))
    }

    fn set_muted(&self, guid: &str, muted: bool) -> DawResult<()> {
        let project = super::resolve_project(self.guid)?;
        let track = super::find_track_by_guid(&project, guid)
            .ok_or_else(|| DawError::not_found("Track", guid))?;
        if muted {
            track.mute(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
        } else {
            track.unmute(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
        }
        Ok(())
    }

    fn set_soloed(&self, guid: &str, soloed: bool) -> DawResult<()> {
        let project = super::resolve_project(self.guid)?;
        let track = super::find_track_by_guid(&project, guid)
            .ok_or_else(|| DawError::not_found("Track", guid))?;
        if soloed {
            track.solo(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
        } else {
            track.unsolo(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
        }
        Ok(())
    }

    fn set_volume(&self, guid: &str, volume: f64) -> DawResult<()> {
        let project = super::resolve_project(self.guid)?;
        let track = super::find_track_by_guid(&project, guid)
            .ok_or_else(|| DawError::not_found("Track", guid))?;
        let val = reaper_medium::ReaperVolumeValue::new(volume)
            .map_err(|e| DawError::operation_failed(format!("invalid volume: {e:?}")))?;
        let _ = track.set_volume_smart(val, Default::default());
        Ok(())
    }

    fn set_pan(&self, guid: &str, pan: f64) -> DawResult<()> {
        let project = super::resolve_project(self.guid)?;
        let track = super::find_track_by_guid(&project, guid)
            .ok_or_else(|| DawError::not_found("Track", guid))?;
        let val = reaper_medium::ReaperPanValue::new_panic(pan.clamp(-1.0, 1.0));
        let _ = track.set_pan_smart(val, Default::default());
        Ok(())
    }

    fn rename(&self, guid: &str, name: &str) -> DawResult<()> {
        let project = super::resolve_project(self.guid)?;
        let track = super::find_track_by_guid(&project, guid)
            .ok_or_else(|| DawError::not_found("Track", guid))?;
        track.set_name(name);
        Ok(())
    }

    fn set_color(&self, guid: &str, color: u32) -> DawResult<()> {
        let project = super::resolve_project(self.guid)?;
        let track = super::find_track_by_guid(&project, guid)
            .ok_or_else(|| DawError::not_found("Track", guid))?;
        if color == 0 {
            track.set_custom_color(None);
        } else {
            let r = ((color >> 16) & 0xFF) as u8;
            let g = ((color >> 8) & 0xFF) as u8;
            let b = (color & 0xFF) as u8;
            track.set_custom_color(Some(reaper_medium::RgbColor::rgb(r, g, b)));
        }
        Ok(())
    }

    fn get_ext_state(&self, guid: &str, section: &str, key: &str) -> Option<String> {
        let project = super::resolve_project(self.guid).ok()?;
        let track = super::find_track_by_guid(&project, guid)?;
        let raw = track.raw().ok()?;
        let low = Reaper::get().medium_reaper().low();
        let attr = CString::new(format!("P_EXT:{section}:{key}")).ok()?;
        let mut buf = vec![0u8; 65536];
        let ok = unsafe {
            low.GetSetMediaTrackInfo_String(
                raw.as_ptr(),
                attr.as_ptr(),
                buf.as_mut_ptr() as *mut i8,
                false,
            )
        };
        if !ok {
            return None;
        }
        let val = crate::safe_wrappers::buffer::string_from_buffer(&buf);
        if val.is_empty() { None } else { Some(val) }
    }

    fn set_ext_state(&self, guid: &str, section: &str, key: &str, value: &str) -> DawResult<()> {
        let project = super::resolve_project(self.guid)?;
        let track = super::find_track_by_guid(&project, guid)
            .ok_or_else(|| DawError::not_found("Track", guid))?;
        let raw = track
            .raw()
            .map_err(|e| DawError::invalid_object("Track", &format!("{e:?}")))?;
        let low = Reaper::get().medium_reaper().low();
        let attr = CString::new(format!("P_EXT:{section}:{key}"))
            .map_err(|e| DawError::operation_failed(format!("invalid section/key: {e}")))?;
        let val = CString::new(value)
            .map_err(|e| DawError::operation_failed(format!("invalid value: {e}")))?;
        unsafe {
            low.GetSetMediaTrackInfo_String(
                raw.as_ptr(),
                attr.as_ptr(),
                val.as_ptr() as *mut i8,
                true,
            );
        }
        Ok(())
    }
}
