//! `StandaloneTracks` — sync tracks sub-handle.

use daw_proto::{DawError, DawResult, Track, sync::Tracks};

use super::daw::Standalone;

pub struct StandaloneTracks<'a> {
    daw: &'a Standalone,
    guid: String,
}

impl<'a> StandaloneTracks<'a> {
    pub(crate) fn new(daw: &'a Standalone, guid: String) -> Self {
        Self { daw, guid }
    }
}

fn find_track_mut<'b>(tracks: &'b mut [Track], guid: &str) -> DawResult<&'b mut Track> {
    tracks
        .iter_mut()
        .find(|t| t.guid == guid)
        .ok_or_else(|| DawError::not_found("Track", guid))
}

impl<'a> Tracks for StandaloneTracks<'a> {
    fn all(&self) -> Vec<Track> {
        self.daw
            .with_project(&self.guid, |p| p.tracks.clone())
            .unwrap_or_default()
    }

    fn count(&self) -> u32 {
        self.daw
            .with_project(&self.guid, |p| p.tracks.len() as u32)
            .unwrap_or(0)
    }

    fn by_guid(&self, guid: &str) -> Option<Track> {
        self.daw
            .with_project(&self.guid, |p| {
                p.tracks.iter().find(|t| t.guid == guid).cloned()
            })
            .ok()
            .flatten()
    }

    fn selected(&self) -> Vec<Track> {
        self.daw
            .with_project(&self.guid, |p| {
                p.tracks.iter().filter(|t| t.selected).cloned().collect()
            })
            .unwrap_or_default()
    }

    fn master(&self) -> DawResult<Track> {
        self.daw.with_project(&self.guid, |p| {
            p.tracks
                .iter()
                .find(|t| t.is_master())
                .cloned()
                .ok_or_else(|| DawError::not_found("Track", "master"))
        })?
    }

    fn set_muted(&self, guid: &str, muted: bool) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            find_track_mut(&mut p.tracks, guid)?.muted = muted;
            Ok::<(), DawError>(())
        })?
    }

    fn set_soloed(&self, guid: &str, soloed: bool) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            find_track_mut(&mut p.tracks, guid)?.soloed = soloed;
            Ok::<(), DawError>(())
        })?
    }

    fn set_volume(&self, guid: &str, volume: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            find_track_mut(&mut p.tracks, guid)?.volume = volume;
            Ok::<(), DawError>(())
        })?
    }

    fn set_pan(&self, guid: &str, pan: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            find_track_mut(&mut p.tracks, guid)?.pan = pan;
            Ok::<(), DawError>(())
        })?
    }

    fn rename(&self, guid: &str, name: &str) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            find_track_mut(&mut p.tracks, guid)?.name = name.to_string();
            Ok::<(), DawError>(())
        })?
    }

    fn set_color(&self, guid: &str, color: u32) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            find_track_mut(&mut p.tracks, guid)?.color = Some(color);
            Ok::<(), DawError>(())
        })?
    }

    fn get_ext_state(&self, guid: &str, section: &str, key: &str) -> Option<String> {
        self.daw
            .with_project(&self.guid, |p| {
                p.track_ext_state
                    .get(&(guid.to_string(), section.to_string(), key.to_string()))
                    .cloned()
            })
            .ok()
            .flatten()
    }

    fn set_ext_state(&self, guid: &str, section: &str, key: &str, value: &str) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            // Validate the track exists, mirroring REAPER semantics.
            if !p.tracks.iter().any(|t| t.guid == guid) {
                return Err(DawError::not_found("Track", guid));
            }
            p.track_ext_state.insert(
                (guid.to_string(), section.to_string(), key.to_string()),
                value.to_string(),
            );
            Ok(())
        })?
    }
}
