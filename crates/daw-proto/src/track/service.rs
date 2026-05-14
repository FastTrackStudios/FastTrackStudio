//! Tracks service trait.
//!
//! Stateless singleton backends — `ProjectContext` flows through every
//! call. `#[architect::rpc]` derives the async vox client + `serve`
//! function; backends impl `Tracks` directly. See
//! `daw-proto/src/marker/service.rs` for the same pattern.
//!
//! Scope was trimmed from the previous async `TrackService` to the
//! verbs the daw-control facade actually drives. Retired surfaces
//! (track chunks, hierarchy apply, visibility, ext state, input
//! monitoring, record input, folder depth, channel counts, subscribe)
//! land on follow-on sibling traits if/when a real consumer needs them.

use super::{Track, TrackRef};
use crate::{DawResult, ProjectContext};
use facet::Facet;

/// Track-scoped ext state payload — groups section + key + value into
/// a single Facet struct. Kept here so batch op definitions can name
/// it without dragging in the full ext-state surface.
#[derive(Clone, Debug, Facet)]
pub struct TrackExtStateRequest {
    pub section: String,
    pub key: String,
    pub value: String,
}

#[architect_rpc_derive::rpc]
pub trait Tracks {
    // ── Queries ─────────────────────────────────────────────────────

    /// Every track in the project, in mixer order.
    fn all(&self, project: ProjectContext) -> Vec<Track>;

    /// One track by reference (guid or index), if it still exists.
    fn get(&self, project: ProjectContext, track: TrackRef) -> Option<Track>;

    /// Total number of tracks (master excluded).
    fn count(&self, project: ProjectContext) -> u32;

    /// All currently selected tracks.
    fn selected(&self, project: ProjectContext) -> Vec<Track>;

    /// The master track.
    fn master(&self, project: ProjectContext) -> Option<Track>;

    // ── Mute / solo / arm ───────────────────────────────────────────

    fn set_muted(&self, project: ProjectContext, track: TrackRef, muted: bool) -> DawResult<()>;

    fn set_soloed(&self, project: ProjectContext, track: TrackRef, soloed: bool) -> DawResult<()>;

    fn set_solo_exclusive(&self, project: ProjectContext, track: TrackRef) -> DawResult<()>;

    fn clear_all_solo(&self, project: ProjectContext) -> DawResult<()>;

    fn set_armed(&self, project: ProjectContext, track: TrackRef, armed: bool) -> DawResult<()>;

    // ── Volume / pan ────────────────────────────────────────────────

    fn set_volume(&self, project: ProjectContext, track: TrackRef, volume: f64) -> DawResult<()>;

    fn set_pan(&self, project: ProjectContext, track: TrackRef, pan: f64) -> DawResult<()>;

    // ── Selection ───────────────────────────────────────────────────

    fn set_selected(
        &self,
        project: ProjectContext,
        track: TrackRef,
        selected: bool,
    ) -> DawResult<()>;

    fn select_exclusive(&self, project: ProjectContext, track: TrackRef) -> DawResult<()>;

    fn clear_selection(&self, project: ProjectContext) -> DawResult<()>;

    // ── Bulk mute ───────────────────────────────────────────────────

    fn mute_all(&self, project: ProjectContext) -> DawResult<()>;

    fn unmute_all(&self, project: ProjectContext) -> DawResult<()>;

    // ── Mutation ────────────────────────────────────────────────────

    /// Insert a track. `at_index = None` appends at the end. Returns
    /// the new track's guid.
    fn add(&self, project: ProjectContext, name: &str, at_index: Option<u32>) -> DawResult<String>;

    fn remove(&self, project: ProjectContext, track: TrackRef) -> DawResult<()>;

    fn remove_all(&self, project: ProjectContext) -> DawResult<()>;

    fn rename(&self, project: ProjectContext, track: TrackRef, name: &str) -> DawResult<()>;

    fn set_color(&self, project: ProjectContext, track: TrackRef, color: u32) -> DawResult<()>;
}
