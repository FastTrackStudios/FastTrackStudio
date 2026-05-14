//! Native sync REAPER implementation of the `daw_proto::sync` traits.
//!
//! [`ReaperMainThread`] is a `!Send + !Sync` root handle that calls
//! `reaper_high::Reaper::get()` directly. It is the zero-overhead hot path
//! for in-process REAPER extension code that already runs on the main thread.
//!
//! Sub-handles ([`ReaperProject`], [`ReaperTransport`], etc.) borrow
//! `&'a ReaperMainThread` and inherit its `!Send + !Sync` discipline via the
//! borrow.

pub mod daw;
pub mod ext_state;
pub mod fx_chains;
pub mod fx_params;
pub mod items;
// markers ported to a singleton `Reaper` backend at the crate root —
// see `crate::marker`. The borrowed-view pattern (`ReaperMarkers<'a>`)
// retired with the port.
pub mod project;
// regions ported to architect::rpc — see `crate::region`.
pub mod routing;
pub mod takes;
pub mod tempo_map;
// tracks ported to architect::rpc — `impl Tracks for Reaper` lives
// at `crate::track`. The borrowed `ReaperTracks<'a>` view retired
// with the port.
pub mod transport;

pub use daw::ReaperMainThread;
pub use ext_state::ReaperExtState;
pub use fx_chains::ReaperFxChains;
pub use fx_params::ReaperFxParams;
pub use items::ReaperItems;
pub use project::ReaperProject;
pub use routing::ReaperRouting;
pub use takes::ReaperTakes;
pub use tempo_map::ReaperTempoMap;
pub use transport::ReaperTransport;

// =============================================================================
// Internal helpers shared across sync sub-handles
// =============================================================================

use crate::project_context::project_guid;
use daw_proto::{DawError, DawResult};
use reaper_high::{Project, Reaper};
use reaper_medium::{ProjectContext as ReaperProjectContext, ProjectRef};

/// Resolve a stored project GUID to a live `reaper_high::Project`.
///
/// If the GUID is empty, returns the current project. Otherwise walks open
/// project tabs and returns the matching one. Returns `DawError::NotFound`
/// if no tab matches.
pub(crate) fn resolve_project(guid: &str) -> DawResult<Project> {
    let reaper = Reaper::get();
    if guid.is_empty() {
        return Ok(reaper.current_project());
    }
    let medium = reaper.medium_reaper();
    for tab_index in 0..crate::project_context::MAX_PROJECT_TABS {
        let Some(result) = medium.enum_projects(ProjectRef::Tab(tab_index), 0) else {
            break;
        };
        let project = Project::new(result.project);
        if project_guid(&project) == guid {
            return Ok(project);
        }
    }
    Err(DawError::not_found("Project", guid))
}

/// Resolve a stored project GUID to a `reaper_medium::ProjectContext`.
///
/// Empty GUID → `CurrentProject`. Otherwise tries to find the matching tab.
pub(crate) fn resolve_reaper_ctx(guid: &str) -> DawResult<ReaperProjectContext> {
    let project = resolve_project(guid)?;
    Ok(ReaperProjectContext::Proj(project.raw()))
}

/// Find a track on a project by GUID (linear scan).
pub(crate) fn find_track_by_guid(project: &Project, guid: &str) -> Option<reaper_high::Track> {
    for i in 0..project.track_count() {
        if let Some(track) = project.track_by_index(i)
            && track.guid().to_string_without_braces() == guid
        {
            return Some(track);
        }
    }
    None
}
