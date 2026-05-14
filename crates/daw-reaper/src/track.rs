//! `impl Tracks for Reaper` — sync trait + REAPER C API.
//!
//! Mounting goes through `daw_proto::track::serve(Reaper)`. The
//! dispatcher (REAPER's main thread queue) is pulled off the backend
//! via `HasDispatcher` on `Reaper`. Each method body assumes it's
//! running on the main thread — that contract is enforced by the
//! bridge before the call lands here.
//!
//! Helpers (`resolve_project`, `resolve_track`, `build_track_info`,
//! `assign_parent_guids`) are kept `pub(crate)` so other modules
//! (midi, batch, etc.) can reuse them.

use std::cell::RefCell;

use daw_proto::Tracks;
use daw_proto::{DawError, DawResult, ProjectContext, Track, TrackRef};
use reaper_high::{GroupingBehavior, Project, Reaper as ReaperHigh};
use reaper_medium::GangBehavior;

use crate::main_thread;
use crate::project_context::{find_project_by_guid, project_guid};

// ── Per-thread project cache ───────────────────────────────────────────
//
// `resolve_project(ProjectContext::Project(guid))` is hot — most calls
// resolve to the same project the previous call did. Cache the
// resolved `Project` per thread to avoid the FFI loop over tabs.

thread_local! {
    static CURRENT_PROJECT_CACHE: RefCell<Option<(String, reaper_high::Project)>> =
        const { RefCell::new(None) };
}

/// Cache the current project's guid for the duration of a batch.
/// Must be called from the main thread. Call `clear_project_cache()`
/// when done.
pub(crate) fn set_project_cache(guid: String, project: reaper_high::Project) {
    CURRENT_PROJECT_CACHE.with(|c| {
        *c.borrow_mut() = Some((guid, project));
    });
}

/// Clear the project cache after a batch completes.
pub(crate) fn clear_project_cache() {
    CURRENT_PROJECT_CACHE.with(|c| {
        *c.borrow_mut() = None;
    });
}

// ── Helpers ────────────────────────────────────────────────────────────

pub(crate) fn resolve_project(ctx: &ProjectContext) -> Option<reaper_high::Project> {
    match ctx {
        ProjectContext::Current => Some(ReaperHigh::get().current_project()),
        ProjectContext::Project(guid) => {
            let cached = CURRENT_PROJECT_CACHE.with(|c| {
                c.borrow()
                    .as_ref()
                    .filter(|(cached_guid, _)| cached_guid == guid)
                    .map(|(_, proj)| *proj)
            });
            if let Some(proj) = cached {
                return Some(proj);
            }
            let current = ReaperHigh::get().current_project();
            if project_guid(&current) == *guid {
                return Some(current);
            }
            find_project_by_guid(guid)
        }
    }
}

/// Public alias used by sibling modules (midi.rs etc).
pub fn resolve_track_pub(
    project: &reaper_high::Project,
    track_ref: &TrackRef,
) -> Option<reaper_high::Track> {
    resolve_track(project, track_ref)
}

pub(crate) fn resolve_track(
    project: &reaper_high::Project,
    track_ref: &TrackRef,
) -> Option<reaper_high::Track> {
    let track = match track_ref {
        TrackRef::Guid(guid) => {
            let mut found = None;
            for i in 0..project.track_count() {
                if let Some(track) = project.track_by_index(i)
                    && track.guid().to_string_without_braces() == *guid
                {
                    found = Some(track);
                    break;
                }
            }
            found?
        }
        TrackRef::Index(idx) => project.track_by_index(*idx)?,
        TrackRef::Master => project.master_track().ok()?,
    };
    if !main_thread::is_track_valid(project, &track) {
        return None;
    }
    Some(track)
}

pub(crate) fn build_track_info(track: &reaper_high::Track) -> Track {
    let guid = track.guid().to_string_without_braces();
    let index = track.index().unwrap_or(0);
    let name = track
        .name()
        .map(|n| n.to_str().to_string())
        .unwrap_or_else(|| {
            if track.is_master_track() {
                "MASTER".to_string()
            } else {
                format!("Track {}", index + 1)
            }
        });

    let color = track
        .custom_color()
        .map(|c| ((c.r as u32) << 16) | ((c.g as u32) << 8) | (c.b as u32));

    let volume = track.volume().get();
    let pan = track.pan().reaper_value().get();
    let muted = track.is_muted();
    let soloed = track.is_solo();
    let armed = track.is_armed(false);
    let selected = track.is_selected();
    let folder_depth = track.folder_depth_change();
    let is_folder = folder_depth > 0;
    let fx_count = track.normal_fx_chain().fx_count();
    let input_fx_count = track.input_fx_chain().fx_count();
    let visible_in_tcp = track.is_shown(reaper_medium::TrackArea::Tcp);
    let visible_in_mixer = track.is_shown(reaper_medium::TrackArea::Mcp);

    Track {
        guid,
        index,
        name,
        color,
        muted,
        soloed,
        armed,
        selected,
        volume,
        pan,
        parent_guid: None,
        folder_depth,
        is_folder,
        visible_in_tcp,
        visible_in_mixer,
        fx_count,
        input_fx_count,
    }
}

pub(crate) fn assign_parent_guids(tracks: &mut [Track]) {
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

/// Insert a track in the current project, returning its guid. Used by
/// places that already hold a main-thread proof (no need to go through
/// the singleton trait).
pub fn add_track_on_main_thread(name: &str, at_index: Option<u32>) -> Option<String> {
    let proj = ReaperHigh::get().current_project();
    let index = at_index.unwrap_or_else(|| proj.track_count());
    let new_track = proj.insert_track_at(index).ok()?;
    new_track.set_name(name);
    Some(new_track.guid().to_string_without_braces())
}

// ── Tracks impl ────────────────────────────────────────────────────────

fn not_found_proj() -> DawError {
    DawError::not_found("Project", "context")
}

fn not_found_track() -> DawError {
    DawError::not_found("Track", "")
}

impl Tracks for crate::Reaper {
    fn all(&self, project: ProjectContext) -> Vec<Track> {
        let Some(proj) = resolve_project(&project) else {
            return Vec::new();
        };
        let mut tracks: Vec<Track> = proj.tracks().map(|t| build_track_info(&t)).collect();
        assign_parent_guids(&mut tracks);
        tracks
    }

    fn get(&self, project: ProjectContext, track: TrackRef) -> Option<Track> {
        let proj = resolve_project(&project)?;
        let t = resolve_track(&proj, &track)?;
        Some(build_track_info(&t))
    }

    fn count(&self, project: ProjectContext) -> u32 {
        resolve_project(&project)
            .map(|p| p.track_count())
            .unwrap_or(0)
    }

    fn selected(&self, project: ProjectContext) -> Vec<Track> {
        let Some(proj) = resolve_project(&project) else {
            return Vec::new();
        };
        proj.tracks()
            .filter(|t| t.is_selected())
            .map(|t| build_track_info(&t))
            .collect()
    }

    fn master(&self, project: ProjectContext) -> Option<Track> {
        let proj = resolve_project(&project)?;
        proj.master_track().ok().as_ref().map(build_track_info)
    }

    fn set_muted(&self, project: ProjectContext, track: TrackRef, muted: bool) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        let t = resolve_track(&proj, &track).ok_or_else(not_found_track)?;
        if muted {
            t.mute(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
        } else {
            t.unmute(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
        }
        Ok(())
    }

    fn set_soloed(&self, project: ProjectContext, track: TrackRef, soloed: bool) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        let t = resolve_track(&proj, &track).ok_or_else(not_found_track)?;
        if soloed {
            t.solo(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
        } else {
            t.unsolo(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
        }
        Ok(())
    }

    fn set_solo_exclusive(&self, project: ProjectContext, track: TrackRef) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        for t in proj.tracks() {
            if t.is_solo() {
                t.unsolo(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
            }
        }
        let t = resolve_track(&proj, &track).ok_or_else(not_found_track)?;
        t.solo(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
        Ok(())
    }

    fn clear_all_solo(&self, project: ProjectContext) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        for t in proj.tracks() {
            if t.is_solo() {
                t.unsolo(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
            }
        }
        Ok(())
    }

    fn set_armed(&self, project: ProjectContext, track: TrackRef, armed: bool) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        let t = resolve_track(&proj, &track).ok_or_else(not_found_track)?;
        if armed {
            t.arm(
                false,
                GangBehavior::DenyGang,
                GroupingBehavior::PreventGrouping,
            );
        } else {
            t.disarm(
                false,
                GangBehavior::DenyGang,
                GroupingBehavior::PreventGrouping,
            );
        }
        Ok(())
    }

    fn set_volume(&self, project: ProjectContext, track: TrackRef, volume: f64) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        let t = resolve_track(&proj, &track).ok_or_else(not_found_track)?;
        let val = reaper_medium::ReaperVolumeValue::new(volume)
            .map_err(|e| DawError::operation_failed(format!("invalid volume: {e:?}")))?;
        let _ = t.set_volume_smart(val, Default::default());
        Ok(())
    }

    fn set_pan(&self, project: ProjectContext, track: TrackRef, pan: f64) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        let t = resolve_track(&proj, &track).ok_or_else(not_found_track)?;
        let val = reaper_medium::ReaperPanValue::new_panic(pan.clamp(-1.0, 1.0));
        let _ = t.set_pan_smart(val, Default::default());
        Ok(())
    }

    fn set_selected(
        &self,
        project: ProjectContext,
        track: TrackRef,
        selected: bool,
    ) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        let t = resolve_track(&proj, &track).ok_or_else(not_found_track)?;
        if selected {
            t.select();
        } else {
            t.unselect();
        }
        Ok(())
    }

    fn select_exclusive(&self, project: ProjectContext, track: TrackRef) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        let t = resolve_track(&proj, &track).ok_or_else(not_found_track)?;
        t.select_exclusively();
        Ok(())
    }

    fn clear_selection(&self, project: ProjectContext) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        for t in proj.tracks() {
            if t.is_selected() {
                t.unselect();
            }
        }
        Ok(())
    }

    fn mute_all(&self, project: ProjectContext) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        for t in proj.tracks() {
            if !t.is_muted() {
                t.mute(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
            }
        }
        Ok(())
    }

    fn unmute_all(&self, project: ProjectContext) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        for t in proj.tracks() {
            if t.is_muted() {
                t.unmute(GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
            }
        }
        Ok(())
    }

    fn add(&self, project: ProjectContext, name: &str, at_index: Option<u32>) -> DawResult<String> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        let index = at_index.unwrap_or_else(|| proj.track_count());
        let new_track = proj
            .insert_track_at(index)
            .map_err(|e| DawError::operation_failed(format!("insert_track_at failed: {e:?}")))?;
        new_track.set_name(name);
        Ok(new_track.guid().to_string_without_braces())
    }

    fn remove(&self, project: ProjectContext, track: TrackRef) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        let t = resolve_track(&proj, &track).ok_or_else(not_found_track)?;
        proj.remove_track(&t);
        Ok(())
    }

    fn remove_all(&self, project: ProjectContext) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        let count = proj.track_count();
        for i in (0..count).rev() {
            if let Some(t) = proj.track_by_index(i) {
                proj.remove_track(&t);
            }
        }
        Ok(())
    }

    fn rename(&self, project: ProjectContext, track: TrackRef, name: &str) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        let t = resolve_track(&proj, &track).ok_or_else(not_found_track)?;
        t.set_name(name);
        Ok(())
    }

    fn set_color(&self, project: ProjectContext, track: TrackRef, color: u32) -> DawResult<()> {
        let proj = resolve_project(&project).ok_or_else(not_found_proj)?;
        let t = resolve_track(&proj, &track).ok_or_else(not_found_track)?;
        if color == 0 {
            t.set_custom_color(None);
        } else {
            let r = ((color >> 16) & 0xFF) as u8;
            let g = ((color >> 8) & 0xFF) as u8;
            let b = (color & 0xFF) as u8;
            t.set_custom_color(Some(reaper_medium::RgbColor::rgb(r, g, b)));
        }
        Ok(())
    }
}

// `Project` is used only for the `Project` re-export visibility check
// inside `resolve_project`; quiet the unused-import lint when no
// methods reference the bare name.
#[allow(dead_code)]
fn _force_project_in_scope(_: &Project) {}
