//! Per-project remote handle: [`RemoteProject`].
//!
//! Post-architect::rpc port, `Projects` is a flat trait keyed by
//! `ProjectContext`. This handle survives mostly for source
//! compatibility with the `Daw::project()` accessor. All methods
//! dispatch onto REAPER's main thread via `super::dispatch` and
//! forward to the underlying `crate::Reaper` singleton.

use daw_proto::{Daw, DawResult, ProjectContext, ProjectInfo, Projects as ProjectTrait, UndoScope};

use super::{ReaperRemote, dispatch, main_thread};

pub struct RemoteProject<'a> {
    pub(crate) remote: &'a ReaperRemote,
    #[allow(dead_code)]
    pub(crate) guid: String,
}

impl<'a> RemoteProject<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote, guid: String) -> Self {
        Self { remote, guid }
    }
}

impl<'a> ProjectTrait for RemoteProject<'a> {
    fn info(&self, project: ProjectContext) -> DawResult<ProjectInfo> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            main_thread()?.project(&guid)?.info(project)
        })
    }

    // All other methods are stubbed — the remote handle predates the
    // architect::rpc port and is being retired. Callers that actually
    // need these surfaces should mount the `Projects` trait directly
    // via `project::serve(Reaper)` and call the client.
    fn current(&self) -> Option<ProjectInfo> {
        None
    }
    fn get(&self, _id: &str) -> Option<ProjectInfo> {
        None
    }
    fn list(&self) -> Vec<ProjectInfo> {
        Vec::new()
    }
    fn get_by_slot(&self, _slot: u32) -> Option<ProjectInfo> {
        None
    }
    fn select(&self, _id: &str) -> bool {
        false
    }
    fn open(&self, _path: &str) -> Option<ProjectInfo> {
        None
    }
    fn create(&self) -> Option<ProjectInfo> {
        None
    }
    fn close(&self, _id: &str) -> bool {
        false
    }
    fn begin_undo_block(&self, _project: ProjectContext, _label: &str) {}
    fn end_undo_block(&self, _project: ProjectContext, _label: &str, _scope: Option<UndoScope>) {}
    fn undo(&self, _project: ProjectContext) -> bool {
        false
    }
    fn redo(&self, _project: ProjectContext) -> bool {
        false
    }
    fn last_undo_label(&self, _project: ProjectContext) -> Option<String> {
        None
    }
    fn last_redo_label(&self, _project: ProjectContext) -> Option<String> {
        None
    }
    fn run_command(&self, _project: ProjectContext, _command: &str) -> bool {
        false
    }
    fn save(&self, _project: ProjectContext) {}
    fn save_all(&self) {}
    fn get_project_info_string(&self, _project: ProjectContext, _key: &str) -> String {
        String::new()
    }
    fn set_project_info_string(&self, _project: ProjectContext, _key: &str, _value: &str) {}
    fn get_project_info(&self, _project: ProjectContext, _key: &str) -> f64 {
        0.0
    }
    fn set_project_info(&self, _project: ProjectContext, _key: &str, _value: f64) {}
    fn get_project_config(&self, _project: ProjectContext, _key: &str) -> Option<f64> {
        None
    }
    fn set_project_config(&self, _project: ProjectContext, _key: &str, _value: f64) -> bool {
        false
    }
    fn set_ruler_lane_name(&self, _project: ProjectContext, _lane_index: u32, _name: &str) {}
    fn get_ruler_lane_name(&self, _project: ProjectContext, _lane_index: u32) -> String {
        String::new()
    }
    fn ruler_lane_count(&self, _project: ProjectContext) -> u32 {
        0
    }
}
