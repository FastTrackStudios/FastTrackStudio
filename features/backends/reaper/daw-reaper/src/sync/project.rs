//! Per-project sync handle: [`ReaperProject`].
//!
//! Post-architect::rpc port, `Projects` is a flat trait keyed by
//! `ProjectContext`. This per-project handle forwards every method to
//! `crate::Reaper` (which carries the real impl) while remembering its
//! captured GUID. Most callers should use `crate::Reaper` directly via
//! the architect-rpc mount; this handle exists for the `Daw` trait's
//! `current_project()` / `project()` accessors.
#![allow(dead_code)]

use daw_proto::{DawResult, ProjectContext, ProjectInfo, Projects as ProjectTrait, UndoScope};

use super::ReaperMainThread;

pub struct ReaperProject<'a> {
    pub(crate) mt: &'a ReaperMainThread,
    #[allow(dead_code)]
    pub(crate) guid: String,
}

impl<'a> ReaperProject<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread, guid: String) -> Self {
        Self { mt, guid }
    }
}

impl<'a> ProjectTrait for ReaperProject<'a> {
    fn info(&self, project: ProjectContext) -> DawResult<ProjectInfo> {
        crate::Reaper.info(project)
    }
    fn current(&self) -> Option<ProjectInfo> {
        crate::Reaper.current()
    }
    fn get(&self, id: &str) -> Option<ProjectInfo> {
        crate::Reaper.get(id)
    }
    fn list(&self) -> Vec<ProjectInfo> {
        crate::Reaper.list()
    }
    fn get_by_slot(&self, slot: u32) -> Option<ProjectInfo> {
        crate::Reaper.get_by_slot(slot)
    }
    fn select(&self, id: &str) -> bool {
        crate::Reaper.select(id)
    }
    fn open(&self, path: &str) -> Option<ProjectInfo> {
        crate::Reaper.open(path)
    }
    fn create(&self) -> Option<ProjectInfo> {
        crate::Reaper.create()
    }
    fn close(&self, id: &str) -> bool {
        crate::Reaper.close(id)
    }
    fn begin_undo_block(&self, project: ProjectContext, label: &str) {
        crate::Reaper.begin_undo_block(project, label)
    }
    fn end_undo_block(&self, project: ProjectContext, label: &str, scope: Option<UndoScope>) {
        crate::Reaper.end_undo_block(project, label, scope)
    }
    fn undo(&self, project: ProjectContext) -> bool {
        crate::Reaper.undo(project)
    }
    fn redo(&self, project: ProjectContext) -> bool {
        crate::Reaper.redo(project)
    }
    fn last_undo_label(&self, project: ProjectContext) -> Option<String> {
        crate::Reaper.last_undo_label(project)
    }
    fn last_redo_label(&self, project: ProjectContext) -> Option<String> {
        crate::Reaper.last_redo_label(project)
    }
    fn run_command(&self, project: ProjectContext, command: &str) -> bool {
        crate::Reaper.run_command(project, command)
    }
    fn save(&self, project: ProjectContext) {
        crate::Reaper.save(project)
    }
    fn save_all(&self) {
        crate::Reaper.save_all()
    }
    fn get_project_info_string(&self, project: ProjectContext, key: &str) -> String {
        crate::Reaper.get_project_info_string(project, key)
    }
    fn set_project_info_string(&self, project: ProjectContext, key: &str, value: &str) {
        crate::Reaper.set_project_info_string(project, key, value)
    }
    fn get_project_info(&self, project: ProjectContext, key: &str) -> f64 {
        crate::Reaper.get_project_info(project, key)
    }
    fn set_project_info(&self, project: ProjectContext, key: &str, value: f64) {
        crate::Reaper.set_project_info(project, key, value)
    }
    fn get_project_config(&self, project: ProjectContext, key: &str) -> Option<f64> {
        crate::Reaper.get_project_config(project, key)
    }
    fn set_project_config(&self, project: ProjectContext, key: &str, value: f64) -> bool {
        crate::Reaper.set_project_config(project, key, value)
    }
    fn set_ruler_lane_name(&self, project: ProjectContext, lane_index: u32, name: &str) {
        crate::Reaper.set_ruler_lane_name(project, lane_index, name)
    }
    fn get_ruler_lane_name(&self, project: ProjectContext, lane_index: u32) -> String {
        crate::Reaper.get_ruler_lane_name(project, lane_index)
    }
    fn ruler_lane_count(&self, project: ProjectContext) -> u32 {
        crate::Reaper.ruler_lane_count(project)
    }
}
