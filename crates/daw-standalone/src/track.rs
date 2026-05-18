//! `impl Tracks for Standalone` — post-architect::rpc port.
//!
//! Backed by `ProjectState::tracks: Vec<Track>` in the existing
//! in-memory state. Master track is synthesized on demand. The old
//! 400+-line async `StandaloneTrack` impl with parallel `TrackState`
//! storage was retired in favor of operating directly on the
//! canonical state — fewer places for state to drift.

use daw_proto::Tracks;
use daw_proto::{DawError, DawResult, ProjectContext, RecordInput, Track, TrackRef};
use uuid::Uuid;

use crate::sync::Standalone;

fn resolve_project(daw: &Standalone, ctx: &ProjectContext) -> Option<String> {
    match ctx {
        ProjectContext::Project(guid) => Some(guid.clone()),
        ProjectContext::Current => {
            let state = daw.state.lock().ok()?;
            state.current_project_guid.clone()
        }
    }
}

fn find_track_index<'a>(tracks: &'a [Track], r: &TrackRef) -> Option<usize> {
    match r {
        TrackRef::Guid(guid) => tracks.iter().position(|t| t.guid == *guid),
        TrackRef::Index(idx) => {
            let i = *idx as usize;
            if i < tracks.len() { Some(i) } else { None }
        }
        TrackRef::Master => None,
    }
}

fn not_found_proj() -> DawError {
    DawError::not_found("Project", "context")
}

fn not_found_track() -> DawError {
    DawError::not_found("Track", "")
}

impl Tracks for Standalone {
    fn all(&self, project: ProjectContext) -> Vec<Track> {
        let Some(guid) = resolve_project(self, &project) else {
            return Vec::new();
        };
        self.with_project(&guid, |p| p.tracks.clone())
            .unwrap_or_default()
    }

    fn get(&self, project: ProjectContext, track: TrackRef) -> Option<Track> {
        let guid = resolve_project(self, &project)?;
        self.with_project(&guid, |p| {
            find_track_index(&p.tracks, &track).map(|i| p.tracks[i].clone())
        })
        .ok()
        .flatten()
    }

    fn count(&self, project: ProjectContext) -> u32 {
        let Some(guid) = resolve_project(self, &project) else {
            return 0;
        };
        self.with_project(&guid, |p| p.tracks.len() as u32)
            .unwrap_or(0)
    }

    fn selected(&self, project: ProjectContext) -> Vec<Track> {
        let Some(guid) = resolve_project(self, &project) else {
            return Vec::new();
        };
        self.with_project(&guid, |p| {
            p.tracks.iter().filter(|t| t.selected).cloned().collect()
        })
        .unwrap_or_default()
    }

    fn master(&self, project: ProjectContext) -> Option<Track> {
        // Standalone synthesizes a master track on demand — there's no
        // persistent master row in `ProjectState::tracks`.
        let _ = project;
        Some(Track {
            guid: "master".to_string(),
            index: 0,
            name: "MASTER".to_string(),
            ..Default::default()
        })
    }

    fn set_muted(&self, project: ProjectContext, track: TrackRef, muted: bool) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = find_track_index(&p.tracks, &track).ok_or_else(not_found_track)?;
            p.tracks[i].muted = muted;
            Ok::<(), DawError>(())
        })?
    }

    fn set_soloed(&self, project: ProjectContext, track: TrackRef, soloed: bool) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = find_track_index(&p.tracks, &track).ok_or_else(not_found_track)?;
            p.tracks[i].soloed = soloed;
            Ok::<(), DawError>(())
        })?
    }

    fn set_solo_exclusive(&self, project: ProjectContext, track: TrackRef) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            for t in p.tracks.iter_mut() {
                t.soloed = false;
            }
            let i = find_track_index(&p.tracks, &track).ok_or_else(not_found_track)?;
            p.tracks[i].soloed = true;
            Ok::<(), DawError>(())
        })?
    }

    fn clear_all_solo(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            for t in p.tracks.iter_mut() {
                t.soloed = false;
            }
        })
    }

    fn set_armed(&self, project: ProjectContext, track: TrackRef, armed: bool) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = find_track_index(&p.tracks, &track).ok_or_else(not_found_track)?;
            p.tracks[i].armed = armed;
            Ok::<(), DawError>(())
        })?
    }

    fn set_volume(&self, project: ProjectContext, track: TrackRef, volume: f64) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = find_track_index(&p.tracks, &track).ok_or_else(not_found_track)?;
            p.tracks[i].volume = volume;
            Ok::<(), DawError>(())
        })?
    }

    fn set_pan(&self, project: ProjectContext, track: TrackRef, pan: f64) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = find_track_index(&p.tracks, &track).ok_or_else(not_found_track)?;
            p.tracks[i].pan = pan.clamp(-1.0, 1.0);
            Ok::<(), DawError>(())
        })?
    }

    fn set_selected(
        &self,
        project: ProjectContext,
        track: TrackRef,
        selected: bool,
    ) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = find_track_index(&p.tracks, &track).ok_or_else(not_found_track)?;
            p.tracks[i].selected = selected;
            Ok::<(), DawError>(())
        })?
    }

    fn select_exclusive(&self, project: ProjectContext, track: TrackRef) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            for t in p.tracks.iter_mut() {
                t.selected = false;
            }
            let i = find_track_index(&p.tracks, &track).ok_or_else(not_found_track)?;
            p.tracks[i].selected = true;
            Ok::<(), DawError>(())
        })?
    }

    fn clear_selection(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            for t in p.tracks.iter_mut() {
                t.selected = false;
            }
        })
    }

    fn mute_all(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            for t in p.tracks.iter_mut() {
                t.muted = true;
            }
        })
    }

    fn unmute_all(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            for t in p.tracks.iter_mut() {
                t.muted = false;
            }
        })
    }

    fn add(&self, project: ProjectContext, name: &str, at_index: Option<u32>) -> DawResult<String> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let new_guid = Uuid::new_v4().to_string();
            let pos = at_index
                .map(|i| (i as usize).min(p.tracks.len()))
                .unwrap_or(p.tracks.len());
            let track = Track {
                guid: new_guid.clone(),
                index: pos as u32,
                name: name.to_string(),
                ..Default::default()
            };
            p.tracks.insert(pos, track);
            // Re-index the tracks below the insertion point.
            for (i, t) in p.tracks.iter_mut().enumerate().skip(pos) {
                t.index = i as u32;
            }
            new_guid
        })
    }

    fn remove(&self, project: ProjectContext, track: TrackRef) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = find_track_index(&p.tracks, &track).ok_or_else(not_found_track)?;
            p.tracks.remove(i);
            for (idx, t) in p.tracks.iter_mut().enumerate() {
                t.index = idx as u32;
            }
            Ok::<(), DawError>(())
        })?
    }

    fn remove_all(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.tracks.clear();
        })
    }

    fn rename(&self, project: ProjectContext, track: TrackRef, name: &str) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = find_track_index(&p.tracks, &track).ok_or_else(not_found_track)?;
            p.tracks[i].name = name.to_string();
            Ok::<(), DawError>(())
        })?
    }

    fn set_color(&self, project: ProjectContext, track: TrackRef, color: u32) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = find_track_index(&p.tracks, &track).ok_or_else(not_found_track)?;
            p.tracks[i].color = if color == 0 { None } else { Some(color) };
            Ok::<(), DawError>(())
        })?
    }

    fn set_folder_depth(
        &self,
        _project: ProjectContext,
        _track: TrackRef,
        _folder_depth: i32,
    ) -> DawResult<()> {
        Ok(())
    }

    fn set_num_channels(
        &self,
        _project: ProjectContext,
        _track: TrackRef,
        _num_channels: u32,
    ) -> DawResult<()> {
        Ok(())
    }

    fn set_record_input(
        &self,
        _project: ProjectContext,
        _track: TrackRef,
        _input: RecordInput,
    ) -> DawResult<()> {
        Ok(())
    }

    fn reorder_selected(
        &self,
        _project: ProjectContext,
        _index: u32,
        _behavior: daw_proto::track::ReorderTracksBehavior,
    ) -> DawResult<()> {
        Ok(())
    }

    fn set_visibility(
        &self,
        _project: ProjectContext,
        _track: TrackRef,
        _visible_in_tcp: bool,
        _visible_in_mixer: bool,
    ) -> DawResult<()> {
        Ok(())
    }

    fn set_tcp_height(
        &self,
        _project: ProjectContext,
        _track: TrackRef,
        _height_pixels: u32,
    ) -> DawResult<()> {
        Ok(())
    }

    async fn subscribe(
        &self,
        _project: ProjectContext,
        _tx: vox::Tx<daw_proto::track::TrackStreamEvent>,
    ) {
        // Standalone has no event source; subscriber gets nothing.
    }
}
