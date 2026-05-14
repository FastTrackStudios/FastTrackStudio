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

    fn add(&self, name: &str, at_index: Option<u32>) -> DawResult<String> {
        self.daw.with_project_mut(&self.guid, |p| {
            let guid = format!("standalone-track-{:016x}", p.tracks.len() as u64 + 1);
            let idx = at_index
                .map(|i| i as usize)
                .unwrap_or(p.tracks.len())
                .min(p.tracks.len());
            let track = Track {
                guid: guid.clone(),
                index: idx as u32,
                name: name.to_string(),
                color: None,
                muted: false,
                soloed: false,
                armed: false,
                selected: false,
                volume: 1.0,
                pan: 0.0,
                parent_guid: None,
                folder_depth: 0,
                is_folder: false,
                visible_in_tcp: true,
                visible_in_mixer: true,
                fx_count: 0,
                input_fx_count: 0,
            };
            p.tracks.insert(idx, track);
            for (i, t) in p.tracks.iter_mut().enumerate() {
                t.index = i as u32;
            }
            guid
        })
    }

    fn remove(&self, guid: &str) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            let pos = p.tracks.iter().position(|t| t.guid == guid);
            if let Some(i) = pos {
                p.tracks.remove(i);
                for (j, t) in p.tracks.iter_mut().enumerate() {
                    t.index = j as u32;
                }
                Ok(())
            } else {
                Err(DawError::not_found("Track", guid))
            }
        })?
    }

    fn remove_all(&self) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| p.tracks.clear())
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
