//! Project service traits.
//!
//! Async `ProjectService` covers full project lifecycle, undo, save,
//! info, ruler lanes, command running, and streaming. The sync
//! `Project` trait is the per-project handle (just `guid` + `info`)
//! used by the legacy `Daw::current_project()` / `project()`
//! navigation surface; most sub-domain accessors have already been
//! lifted out by their architect::rpc ports.

use super::{ProjectContext, ProjectEvent, ProjectInfo};
use crate::UndoScope;
use vox::{Tx, service};

/// Service for managing projects.
#[service]
pub trait ProjectService {
    async fn get_current(&self) -> Option<ProjectInfo>;

    async fn get(&self, project_id: String) -> Option<ProjectInfo>;

    async fn list(&self) -> Vec<ProjectInfo>;

    /// Make `project_id` the currently focused project. Returns true if
    /// found.
    async fn select(&self, project_id: String) -> bool;

    /// Open an existing `.rpp` file in a new tab.
    async fn open(&self, path: String) -> Option<ProjectInfo>;

    /// Create a new empty project tab.
    async fn create(&self) -> Option<ProjectInfo>;

    /// Close a specific project tab by GUID.
    async fn close(&self, project_id: String) -> bool;

    /// Project at the given 0-based tab slot, if any.
    async fn get_by_slot(&self, slot: u32) -> Option<ProjectInfo>;

    // ── Undo ────────────────────────────────────────────────────────

    async fn begin_undo_block(&self, project: ProjectContext, label: String);

    async fn end_undo_block(
        &self,
        project: ProjectContext,
        label: String,
        scope: Option<UndoScope>,
    );

    async fn undo(&self, project: ProjectContext) -> bool;
    async fn redo(&self, project: ProjectContext) -> bool;

    async fn last_undo_label(&self, project: ProjectContext) -> Option<String>;
    async fn last_redo_label(&self, project: ProjectContext) -> Option<String>;

    // ── Actions / Commands ──────────────────────────────────────────

    /// Run a REAPER action/command by string id (named or numeric).
    async fn run_command(&self, project: ProjectContext, command: String) -> bool;

    // ── Save ────────────────────────────────────────────────────────

    async fn save(&self, project: ProjectContext);

    /// Save all open projects (REAPER action 40897).
    async fn save_all(&self);

    // ── Project info (GetSetProjectInfo / _String) ──────────────────

    async fn get_project_info_string(&self, project: ProjectContext, key: String) -> String;
    async fn set_project_info_string(&self, project: ProjectContext, key: String, value: String);
    async fn get_project_info(&self, project: ProjectContext, key: String) -> f64;
    async fn set_project_info(&self, project: ProjectContext, key: String, value: f64);

    // ── Ruler lanes (v7.62+) ────────────────────────────────────────

    async fn set_ruler_lane_name(&self, project: ProjectContext, lane_index: u32, name: String);
    async fn get_ruler_lane_name(&self, project: ProjectContext, lane_index: u32) -> String;
    async fn ruler_lane_count(&self, project: ProjectContext) -> u32;

    // ── Streaming ───────────────────────────────────────────────────

    /// Subscribe to project state changes (opened / closed /
    /// current_changed / changed / projects_changed).
    async fn subscribe(&self, tx: Tx<ProjectEvent>);
}

/// Per-project sync handle trait — entry point off `Daw::current_project()`.
///
/// Sub-domain accessors (transport / regions / markers / tracks /
/// tempo_map / ext_state / fx_chains / fx_params / items / takes /
/// routing) have all been lifted out by their architect::rpc ports;
/// clients reach those services through their `Client` with a
/// `ProjectContext` per call.
///
/// Named `Project` (not `ProjectHandle`) to match the established
/// `Daw::Project<'a>` GAT.
pub trait Projects {
    fn guid(&self) -> &str;
    fn info(&self) -> crate::DawResult<crate::ProjectInfo>;
}
