//! `impl Projects for Standalone` — stub. Per-project handle pattern
//! retired with the architect::rpc port; all methods take
//! `ProjectContext` now. Full impl pending — currently todo!() to keep
//! the workspace compiling.

use daw_proto::{DawResult, ProjectContext, ProjectInfo, Projects, UndoScope};

use super::daw::Standalone;

/// Thin per-project handle for `Daw::current_project()` / `project()`.
/// Post-port `Projects` methods all take `ProjectContext` so this
/// wrapper just forwards to `Standalone`.
pub struct StandaloneProject<'a> {
    daw: &'a Standalone,
    #[allow(dead_code)]
    guid: String,
}

impl<'a> StandaloneProject<'a> {
    pub(crate) fn new(daw: &'a Standalone, guid: String) -> Self {
        Self { daw, guid }
    }
}

impl<'a> Projects for StandaloneProject<'a> {
    fn info(&self, project: ProjectContext) -> DawResult<ProjectInfo> {
        self.daw.info(project)
    }
    fn current(&self) -> Option<ProjectInfo> {
        self.daw.current()
    }
    fn get(&self, id: &str) -> Option<ProjectInfo> {
        self.daw.get(id)
    }
    fn list(&self) -> Vec<ProjectInfo> {
        self.daw.list()
    }
    fn get_by_slot(&self, slot: u32) -> Option<ProjectInfo> {
        self.daw.get_by_slot(slot)
    }
    fn select(&self, id: &str) -> bool {
        self.daw.select(id)
    }
    fn open(&self, path: &str) -> Option<ProjectInfo> {
        self.daw.open(path)
    }
    fn create(&self) -> Option<ProjectInfo> {
        self.daw.create()
    }
    fn close(&self, id: &str) -> bool {
        self.daw.close(id)
    }
    fn begin_undo_block(&self, project: ProjectContext, label: &str) {
        self.daw.begin_undo_block(project, label)
    }
    fn end_undo_block(&self, project: ProjectContext, label: &str, scope: Option<UndoScope>) {
        self.daw.end_undo_block(project, label, scope)
    }
    fn undo(&self, project: ProjectContext) -> bool {
        self.daw.undo(project)
    }
    fn redo(&self, project: ProjectContext) -> bool {
        self.daw.redo(project)
    }
    fn last_undo_label(&self, project: ProjectContext) -> Option<String> {
        self.daw.last_undo_label(project)
    }
    fn last_redo_label(&self, project: ProjectContext) -> Option<String> {
        self.daw.last_redo_label(project)
    }
    fn run_command(&self, project: ProjectContext, command: &str) -> bool {
        self.daw.run_command(project, command)
    }
    fn save(&self, project: ProjectContext) {
        self.daw.save(project)
    }
    fn save_all(&self) {
        self.daw.save_all()
    }
    fn get_project_info_string(&self, project: ProjectContext, key: &str) -> String {
        self.daw.get_project_info_string(project, key)
    }
    fn set_project_info_string(&self, project: ProjectContext, key: &str, value: &str) {
        self.daw.set_project_info_string(project, key, value)
    }
    fn get_project_info(&self, project: ProjectContext, key: &str) -> f64 {
        self.daw.get_project_info(project, key)
    }
    fn set_project_info(&self, project: ProjectContext, key: &str, value: f64) {
        self.daw.set_project_info(project, key, value)
    }
    fn set_ruler_lane_name(&self, project: ProjectContext, lane_index: u32, name: &str) {
        self.daw.set_ruler_lane_name(project, lane_index, name)
    }
    fn get_ruler_lane_name(&self, project: ProjectContext, lane_index: u32) -> String {
        self.daw.get_ruler_lane_name(project, lane_index)
    }
    fn ruler_lane_count(&self, project: ProjectContext) -> u32 {
        self.daw.ruler_lane_count(project)
    }
}

impl Projects for Standalone {
    fn info(&self, _project: ProjectContext) -> DawResult<ProjectInfo> {
        todo!("standalone Projects::info")
    }
    fn current(&self) -> Option<ProjectInfo> {
        todo!()
    }
    fn get(&self, _project_id: &str) -> Option<ProjectInfo> {
        todo!()
    }
    fn list(&self) -> Vec<ProjectInfo> {
        Vec::new()
    }
    fn get_by_slot(&self, _slot: u32) -> Option<ProjectInfo> {
        todo!()
    }
    fn select(&self, _project_id: &str) -> bool {
        todo!()
    }
    fn open(&self, _path: &str) -> Option<ProjectInfo> {
        todo!()
    }
    fn create(&self) -> Option<ProjectInfo> {
        todo!()
    }
    fn close(&self, _project_id: &str) -> bool {
        todo!()
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
    fn set_ruler_lane_name(&self, _project: ProjectContext, _lane_index: u32, _name: &str) {}
    fn get_ruler_lane_name(&self, _project: ProjectContext, _lane_index: u32) -> String {
        String::new()
    }
    fn ruler_lane_count(&self, _project: ProjectContext) -> u32 {
        0
    }
}
