//! Root sync handle: [`ReaperMainThread`].

use std::marker::PhantomData;

use daw_proto::Daw;
use daw_proto::{DawError, DawResult, LastTouchedFx, ProjectInfo};
use reaper_high::Reaper;
use reaper_medium::{ProjectRef, TrackFxLocation};

use crate::project::project_to_info;
use crate::project_context::{MAX_PROJECT_TABS, project_guid};

use super::ReaperProject;

/// Synchronous, zero-overhead root handle for REAPER's main thread.
///
/// Construct with [`ReaperMainThread::try_new`]. The `PhantomData<*const ()>`
/// marker makes this `!Send + !Sync` so it cannot accidentally be moved to a
/// worker thread.
pub struct ReaperMainThread {
    _not_send_sync: PhantomData<*const ()>,
}

impl ReaperMainThread {
    /// Construct a new handle.
    ///
    /// Returns `None` if the REAPER high-level API is not yet initialized.
    ///
    /// # Safety contract
    ///
    /// The caller must be running on REAPER's main thread.
    pub fn try_new() -> Option<Self> {
        std::panic::catch_unwind(|| {
            let _ = Reaper::get();
        })
        .ok()?;
        Some(Self {
            _not_send_sync: PhantomData,
        })
    }
}

impl Daw for ReaperMainThread {
    type Project<'a>
        = ReaperProject<'a>
    where
        Self: 'a;

    fn current_project(&self) -> DawResult<Self::Project<'_>> {
        let reaper = Reaper::get();
        let project = reaper.current_project();
        let guid = project_guid(&project);
        Ok(ReaperProject::new(self, guid))
    }

    fn project(&self, guid: &str) -> DawResult<Self::Project<'_>> {
        // Validate that the project actually exists.
        let _ = super::resolve_project(guid)?;
        Ok(ReaperProject::new(self, guid.to_string()))
    }

    fn projects(&self) -> Vec<ProjectInfo> {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();
        let mut out = Vec::new();
        for tab_index in 0..MAX_PROJECT_TABS {
            let Some(result) = medium.enum_projects(ProjectRef::Tab(tab_index), 0) else {
                break;
            };
            let project = reaper_high::Project::new(result.project);
            out.push(project_to_info(&project));
        }
        out
    }

    fn show_console_msg(&self, msg: &str) {
        Reaper::get().show_console_msg(msg);
    }

    fn last_touched_fx(&self) -> Option<LastTouchedFx> {
        let reaper = Reaper::get();
        let result = reaper.medium_reaper().get_last_touched_fx()?;
        use reaper_medium::GetLastTouchedFxResult::*;
        match result {
            TrackFx {
                track_location,
                fx_location,
                param_index,
            } => {
                let project = reaper.current_project();
                let track = match track_location {
                    reaper_medium::TrackLocation::MasterTrack => project.master_track().ok()?,
                    reaper_medium::TrackLocation::NormalTrack(idx) => {
                        project.track_by_index(idx)?
                    }
                };
                let track_guid = track.guid().to_string_without_braces();
                let (fx_index, is_input_fx) = match fx_location {
                    TrackFxLocation::NormalFxChain(idx) => (idx, false),
                    TrackFxLocation::InputFxChain(idx) => (idx, true),
                    _ => return None,
                };
                Some(LastTouchedFx {
                    track_guid,
                    fx_index,
                    param_index,
                    is_input_fx,
                })
            }
            _ => None,
        }
    }
}

// Silence unused-import warning for DawError when not referenced explicitly.
#[allow(dead_code)]
fn _assert_error_in_scope(e: DawError) -> DawError {
    e
}
