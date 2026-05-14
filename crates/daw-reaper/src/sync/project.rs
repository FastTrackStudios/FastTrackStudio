//! Per-project sync handle: [`ReaperProject`].

use daw_proto::Projects as ProjectTrait;
use daw_proto::{DawResult, ProjectInfo};

use crate::project::project_to_info;

use super::ReaperMainThread;

/// A handle scoped to a single REAPER project tab.
pub struct ReaperProject<'a> {
    pub(crate) mt: &'a ReaperMainThread,
    pub(crate) guid: String,
}

impl<'a> ReaperProject<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread, guid: String) -> Self {
        Self { mt, guid }
    }
}

impl<'a> ProjectTrait for ReaperProject<'a> {
    fn guid(&self) -> &str {
        &self.guid
    }

    fn info(&self) -> DawResult<ProjectInfo> {
        let project = super::resolve_project(&self.guid)?;
        Ok(project_to_info(&project))
    }
}
