//! Sync routing handle: [`ReaperRouting`].

use daw_proto::sync::Routing as RoutingTrait;
use daw_proto::{DawError, DawResult, RouteType, TrackRoute};
use reaper_high::{Project, Reaper, SendPartnerType, Track};
use reaper_medium::{EditMode, ReaperVolumeValue, TrackSendCategory};

use crate::routing::convert_track_route;
use crate::safe_wrappers::routing as routing_sw;

use super::ReaperMainThread;

pub struct ReaperRouting<'a> {
    _mt: &'a ReaperMainThread,
    guid: &'a str,
}

impl<'a> ReaperRouting<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread, guid: &'a str) -> Self {
        Self { _mt: mt, guid }
    }

    fn project(&self) -> DawResult<Project> {
        super::resolve_project(self.guid)
    }

    fn project_opt(&self) -> Option<Project> {
        super::resolve_project(self.guid).ok()
    }

    fn track(&self, project: &Project, track_guid: &str) -> Option<Track> {
        super::find_track_by_guid(project, track_guid)
    }
}

impl<'a> RoutingTrait for ReaperRouting<'a> {
    fn sends(&self, source_track_guid: &str) -> Vec<TrackRoute> {
        let Some(project) = self.project_opt() else {
            return Vec::new();
        };
        let Some(track) = self.track(&project, source_track_guid) else {
            return Vec::new();
        };
        track
            .typed_sends(SendPartnerType::Track)
            .enumerate()
            .map(|(i, route)| convert_track_route(&route, RouteType::Send, i as u32))
            .collect()
    }

    fn receives(&self, dest_track_guid: &str) -> Vec<TrackRoute> {
        let Some(project) = self.project_opt() else {
            return Vec::new();
        };
        let Some(track) = self.track(&project, dest_track_guid) else {
            return Vec::new();
        };
        track
            .receives()
            .enumerate()
            .map(|(i, route)| convert_track_route(&route, RouteType::Receive, i as u32))
            .collect()
    }

    fn hardware_outputs(&self, track_guid: &str) -> Vec<TrackRoute> {
        let Some(project) = self.project_opt() else {
            return Vec::new();
        };
        let Some(track) = self.track(&project, track_guid) else {
            return Vec::new();
        };
        track
            .typed_sends(SendPartnerType::HardwareOutput)
            .enumerate()
            .map(|(i, route)| convert_track_route(&route, RouteType::HardwareOutput, i as u32))
            .collect()
    }

    fn send_count(&self, track_guid: &str) -> u32 {
        let Some(project) = self.project_opt() else {
            return 0;
        };
        let Some(track) = self.track(&project, track_guid) else {
            return 0;
        };
        track.typed_send_count(SendPartnerType::Track)
    }

    fn receive_count(&self, track_guid: &str) -> u32 {
        let Some(project) = self.project_opt() else {
            return 0;
        };
        let Some(track) = self.track(&project, track_guid) else {
            return 0;
        };
        track.receive_count()
    }

    fn add_send(&self, source_track_guid: &str, dest_track_guid: &str) -> DawResult<u32> {
        let project = self.project()?;
        let source = self
            .track(&project, source_track_guid)
            .ok_or_else(|| DawError::not_found("Track", source_track_guid))?;
        let dest = self
            .track(&project, dest_track_guid)
            .ok_or_else(|| DawError::not_found("Track", dest_track_guid))?;
        let route = source.add_send_to(&dest);
        route
            .track_route_index()
            .ok_or_else(|| DawError::operation_failed("add_send: no track_route_index"))
    }

    fn remove_send(&self, source_track_guid: &str, send_idx: u32) -> DawResult<()> {
        let project = self.project()?;
        let track = self
            .track(&project, source_track_guid)
            .ok_or_else(|| DawError::not_found("Track", source_track_guid))?;
        let raw_track = track
            .raw()
            .map_err(|e| DawError::invalid_object("Track", &format!("{e:?}")))?;
        routing_sw::remove_track_send(
            Reaper::get().medium_reaper(),
            raw_track,
            TrackSendCategory::Send,
            send_idx,
        );
        Ok(())
    }

    fn set_send_volume(&self, track_guid: &str, send_idx: u32, volume: f64) -> DawResult<()> {
        let project = self.project()?;
        let track = self
            .track(&project, track_guid)
            .ok_or_else(|| DawError::not_found("Track", track_guid))?;
        let hw_count = track.typed_send_count(SendPartnerType::HardwareOutput);
        let route = track.send_by_index(hw_count + send_idx).ok_or_else(|| {
            DawError::out_of_range(
                send_idx,
                track.typed_send_count(SendPartnerType::Track),
                "routing.send_idx",
            )
        })?;
        let val = ReaperVolumeValue::new(volume)
            .map_err(|e| DawError::operation_failed(format!("invalid volume: {e:?}")))?;
        route
            .set_volume(val, EditMode::NormalTweak)
            .map_err(|e| DawError::operation_failed(format!("set_volume: {e:?}")))?;
        Ok(())
    }

    fn set_send_pan(&self, track_guid: &str, send_idx: u32, pan: f64) -> DawResult<()> {
        let project = self.project()?;
        let track = self
            .track(&project, track_guid)
            .ok_or_else(|| DawError::not_found("Track", track_guid))?;
        let hw_count = track.typed_send_count(SendPartnerType::HardwareOutput);
        let route = track.send_by_index(hw_count + send_idx).ok_or_else(|| {
            DawError::out_of_range(
                send_idx,
                track.typed_send_count(SendPartnerType::Track),
                "routing.send_idx",
            )
        })?;
        let pan_obj = reaper_high::Pan::from_normalized_value(pan.clamp(-1.0, 1.0));
        route
            .set_pan(pan_obj, EditMode::NormalTweak)
            .map_err(|e| DawError::operation_failed(format!("set_pan: {e:?}")))?;
        Ok(())
    }

    fn set_send_muted(&self, track_guid: &str, send_idx: u32, muted: bool) -> DawResult<()> {
        let project = self.project()?;
        let track = self
            .track(&project, track_guid)
            .ok_or_else(|| DawError::not_found("Track", track_guid))?;
        let hw_count = track.typed_send_count(SendPartnerType::HardwareOutput);
        let route = track.send_by_index(hw_count + send_idx).ok_or_else(|| {
            DawError::out_of_range(
                send_idx,
                track.typed_send_count(SendPartnerType::Track),
                "routing.send_idx",
            )
        })?;
        if muted {
            route
                .mute()
                .map_err(|e| DawError::operation_failed(format!("mute: {e:?}")))?;
        } else {
            route
                .unmute()
                .map_err(|e| DawError::operation_failed(format!("unmute: {e:?}")))?;
        }
        Ok(())
    }

    fn is_send_muted(&self, track_guid: &str, send_idx: u32) -> bool {
        let Some(project) = self.project_opt() else {
            return false;
        };
        let Some(track) = self.track(&project, track_guid) else {
            return false;
        };
        let hw_count = track.typed_send_count(SendPartnerType::HardwareOutput);
        let Some(route) = track.send_by_index(hw_count + send_idx) else {
            return false;
        };
        route.is_muted().unwrap_or(false)
    }
}
