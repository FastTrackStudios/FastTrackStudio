//! `StandaloneRouting` — sync routing sub-handle.

use daw_proto::{DawError, DawResult, RouteType, TrackRoute, sync::Routing};

use super::daw::Standalone;

pub struct StandaloneRouting<'a> {
    daw: &'a Standalone,
    project_guid: String,
}

impl<'a> StandaloneRouting<'a> {
    pub(crate) fn new(daw: &'a Standalone, project_guid: String) -> Self {
        Self { daw, project_guid }
    }
}

fn reindex(routes: &mut [TrackRoute]) {
    for (i, r) in routes.iter_mut().enumerate() {
        r.index = i as u32;
    }
}

impl<'a> Routing for StandaloneRouting<'a> {
    fn sends(&self, source_track_guid: &str) -> Vec<TrackRoute> {
        self.daw
            .with_project(&self.project_guid, |p| {
                p.sends.get(source_track_guid).cloned().unwrap_or_default()
            })
            .unwrap_or_default()
    }

    fn receives(&self, dest_track_guid: &str) -> Vec<TrackRoute> {
        self.daw
            .with_project(&self.project_guid, |p| {
                p.receives.get(dest_track_guid).cloned().unwrap_or_default()
            })
            .unwrap_or_default()
    }

    fn hardware_outputs(&self, track_guid: &str) -> Vec<TrackRoute> {
        self.daw
            .with_project(&self.project_guid, |p| {
                p.hw_outputs.get(track_guid).cloned().unwrap_or_default()
            })
            .unwrap_or_default()
    }

    fn send_count(&self, track_guid: &str) -> u32 {
        self.daw
            .with_project(&self.project_guid, |p| {
                p.sends.get(track_guid).map(|v| v.len() as u32).unwrap_or(0)
            })
            .unwrap_or(0)
    }

    fn receive_count(&self, track_guid: &str) -> u32 {
        self.daw
            .with_project(&self.project_guid, |p| {
                p.receives
                    .get(track_guid)
                    .map(|v| v.len() as u32)
                    .unwrap_or(0)
            })
            .unwrap_or(0)
    }

    fn add_send(&self, source_track_guid: &str, dest_track_guid: &str) -> DawResult<u32> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            // Validate both tracks exist.
            let dest_name = p
                .tracks
                .iter()
                .find(|t| t.guid == source_track_guid)
                .ok_or_else(|| DawError::not_found("Track", source_track_guid))
                .and_then(|_| {
                    p.tracks
                        .iter()
                        .find(|t| t.guid == dest_track_guid)
                        .ok_or_else(|| DawError::not_found("Track", dest_track_guid))
                        .map(|t| t.name.clone())
                })?;

            let sends = p.sends.entry(source_track_guid.to_string()).or_default();
            let send_idx = sends.len() as u32;
            let mut send = TrackRoute::default();
            send.index = send_idx;
            send.route_type = RouteType::Send;
            send.source_track_guid = source_track_guid.to_string();
            send.dest_track_guid = Some(dest_track_guid.to_string());
            send.dest_track_name = Some(dest_name);
            sends.push(send);

            let receives = p.receives.entry(dest_track_guid.to_string()).or_default();
            let mut recv = TrackRoute::default();
            recv.index = receives.len() as u32;
            recv.route_type = RouteType::Receive;
            recv.source_track_guid = source_track_guid.to_string();
            recv.dest_track_guid = Some(dest_track_guid.to_string());
            receives.push(recv);

            Ok(send_idx)
        })?
    }

    fn remove_send(&self, source_track_guid: &str, send_idx: u32) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let sends = p
                .sends
                .get_mut(source_track_guid)
                .ok_or_else(|| DawError::not_found("Track", source_track_guid))?;
            if (send_idx as usize) >= sends.len() {
                return Err(DawError::not_found("Send", &send_idx.to_string()));
            }
            let removed = sends.remove(send_idx as usize);
            reindex(sends);

            // Remove the matching receive on the dest track.
            if let Some(dest) = removed.dest_track_guid.as_deref() {
                if let Some(recvs) = p.receives.get_mut(dest) {
                    if let Some(pos) = recvs
                        .iter()
                        .position(|r| r.source_track_guid == source_track_guid)
                    {
                        recvs.remove(pos);
                        reindex(recvs);
                    }
                }
            }
            Ok::<(), DawError>(())
        })?
    }

    fn set_send_volume(&self, track_guid: &str, send_idx: u32, volume: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let sends = p
                .sends
                .get_mut(track_guid)
                .ok_or_else(|| DawError::not_found("Track", track_guid))?;
            let send = sends
                .get_mut(send_idx as usize)
                .ok_or_else(|| DawError::not_found("Send", &send_idx.to_string()))?;
            send.volume = volume;
            Ok::<(), DawError>(())
        })?
    }

    fn set_send_pan(&self, track_guid: &str, send_idx: u32, pan: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let sends = p
                .sends
                .get_mut(track_guid)
                .ok_or_else(|| DawError::not_found("Track", track_guid))?;
            let send = sends
                .get_mut(send_idx as usize)
                .ok_or_else(|| DawError::not_found("Send", &send_idx.to_string()))?;
            send.pan = pan;
            Ok::<(), DawError>(())
        })?
    }

    fn set_send_muted(&self, track_guid: &str, send_idx: u32, muted: bool) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let sends = p
                .sends
                .get_mut(track_guid)
                .ok_or_else(|| DawError::not_found("Track", track_guid))?;
            let send = sends
                .get_mut(send_idx as usize)
                .ok_or_else(|| DawError::not_found("Send", &send_idx.to_string()))?;
            send.muted = muted;
            Ok::<(), DawError>(())
        })?
    }

    fn is_send_muted(&self, track_guid: &str, send_idx: u32) -> bool {
        self.daw
            .with_project(&self.project_guid, |p| {
                p.sends
                    .get(track_guid)
                    .and_then(|sends| sends.get(send_idx as usize))
                    .map(|s| s.muted)
                    .unwrap_or(false)
            })
            .unwrap_or(false)
    }
}
