//! Per-project sync handle: [`ReaperProject`].

use daw_proto::sync::Project as ProjectTrait;
use daw_proto::{DawResult, ProjectInfo};

use crate::project::project_to_info;

use super::{ReaperItems, ReaperMainThread, ReaperRouting};

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
    type Items<'b>
        = ReaperItems<'b>
    where
        Self: 'b;
    type Routing<'b>
        = ReaperRouting<'b>
    where
        Self: 'b;

    fn guid(&self) -> &str {
        &self.guid
    }

    fn info(&self) -> DawResult<ProjectInfo> {
        let project = super::resolve_project(&self.guid)?;
        Ok(project_to_info(&project))
    }

    fn items(&self) -> Self::Items<'_> {
        ReaperItems::new(self.mt, &self.guid)
    }

    fn routing(&self) -> Self::Routing<'_> {
        ReaperRouting::new(self.mt, &self.guid)
    }
}
