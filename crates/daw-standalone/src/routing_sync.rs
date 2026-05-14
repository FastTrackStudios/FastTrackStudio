//! `impl Routing for Standalone` — post-architect::rpc port.
//!
//! Backed by `ProjectState::sends`/`receives`/`hw_outputs` —
//! `HashMap<track_guid, Vec<TrackRoute>>`. Resolves the current
//! project automatically.

use daw_proto::sync::Routing;
use daw_proto::{DawError, DawResult, RouteType, TrackRoute};

use crate::sync::Standalone;

fn resolve_project(daw: &Standalone) -> Option<String> {
    let state = daw.state.lock().ok()?;
    state.current_project_guid.clone()
}

fn no_project() -> DawError {
    DawError::not_found("Project", "current")
}

fn reindex(routes: &mut [TrackRoute]) {
    for (i, r) in routes.iter_mut().enumerate() {
        r.index = i as u32;
    }
}

impl Routing for Standalone {
    fn sends(&self, source_track_guid: &str) -> Vec<TrackRoute> {
        let Some(guid) = resolve_project(self) else {
            return Vec::new();
        };
        self.with_project(&guid, |p| {
            p.sends.get(source_track_guid).cloned().unwrap_or_default()
        })
        .unwrap_or_default()
    }

    fn receives(&self, dest_track_guid: &str) -> Vec<TrackRoute> {
        let Some(guid) = resolve_project(self) else {
            return Vec::new();
        };
        self.with_project(&guid, |p| {
            p.receives.get(dest_track_guid).cloned().unwrap_or_default()
        })
        .unwrap_or_default()
    }

    fn hardware_outputs(&self, track_guid: &str) -> Vec<TrackRoute> {
        let Some(guid) = resolve_project(self) else {
            return Vec::new();
        };
        self.with_project(&guid, |p| {
            p.hw_outputs.get(track_guid).cloned().unwrap_or_default()
        })
        .unwrap_or_default()
    }

    fn send_count(&self, track_guid: &str) -> u32 {
        let Some(guid) = resolve_project(self) else {
            return 0;
        };
        self.with_project(&guid, |p| {
            p.sends.get(track_guid).map(|v| v.len() as u32).unwrap_or(0)
        })
        .unwrap_or(0)
    }

    fn receive_count(&self, track_guid: &str) -> u32 {
        let Some(guid) = resolve_project(self) else {
            return 0;
        };
        self.with_project(&guid, |p| {
            p.receives
                .get(track_guid)
                .map(|v| v.len() as u32)
                .unwrap_or(0)
        })
        .unwrap_or(0)
    }

    fn add_send(&self, source_track_guid: &str, dest_track_guid: &str) -> DawResult<u32> {
        let guid = resolve_project(self).ok_or_else(no_project)?;
        self.with_project_mut(&guid, |p| {
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

            Ok::<u32, DawError>(send_idx)
        })?
    }

    fn remove_send(&self, source_track_guid: &str, send_idx: u32) -> DawResult<()> {
        let guid = resolve_project(self).ok_or_else(no_project)?;
        self.with_project_mut(&guid, |p| {
            let sends = p
                .sends
                .get_mut(source_track_guid)
                .ok_or_else(|| DawError::not_found("Track", source_track_guid))?;
            if (send_idx as usize) >= sends.len() {
                return Err(DawError::not_found("Send", &send_idx.to_string()));
            }
            let removed = sends.remove(send_idx as usize);
            reindex(sends);

            if let Some(dest) = removed.dest_track_guid.as_deref()
                && let Some(recvs) = p.receives.get_mut(dest)
                && let Some(pos) = recvs
                    .iter()
                    .position(|r| r.source_track_guid == source_track_guid)
            {
                recvs.remove(pos);
                reindex(recvs);
            }
            Ok::<(), DawError>(())
        })?
    }

    fn set_send_volume(&self, track_guid: &str, send_idx: u32, volume: f64) -> DawResult<()> {
        let guid = resolve_project(self).ok_or_else(no_project)?;
        self.with_project_mut(&guid, |p| {
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
        let guid = resolve_project(self).ok_or_else(no_project)?;
        self.with_project_mut(&guid, |p| {
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
        let guid = resolve_project(self).ok_or_else(no_project)?;
        self.with_project_mut(&guid, |p| {
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
        let Some(guid) = resolve_project(self) else {
            return false;
        };
        self.with_project(&guid, |p| {
            p.sends
                .get(track_guid)
                .and_then(|sends| sends.get(send_idx as usize))
                .map(|s| s.muted)
                .unwrap_or(false)
        })
        .unwrap_or(false)
    }
}
