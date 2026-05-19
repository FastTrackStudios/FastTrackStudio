//! Remote toolbar handle: [`RemoteToolbar`].

use daw_proto::{Daw as _, Toolbar as ToolbarTrait};
use daw_proto::{DawResult, ToolbarButton, ToolbarTarget};

use super::{ReaperRemote, dispatch, dispatch_read};

pub struct RemoteToolbar<'a> {
    remote: &'a ReaperRemote,
}

impl<'a> RemoteToolbar<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote) -> Self {
        Self { remote }
    }
}

impl<'a> ToolbarTrait for RemoteToolbar<'a> {
    fn is_available(&self) -> bool {
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return false;
            };
            mt.toolbar().is_available()
        })
    }

    fn add_button(&self, button: ToolbarButton, workflow_id: &str) -> DawResult<()> {
        let button = button.clone();
        let workflow_id = workflow_id.to_string();
        dispatch(self.remote, move || {
            let mt = super::main_thread()?;
            mt.toolbar().add_button(button, &workflow_id)
        })
    }

    fn update_button(&self, button: ToolbarButton, workflow_id: &str) -> DawResult<()> {
        let button = button.clone();
        let workflow_id = workflow_id.to_string();
        dispatch(self.remote, move || {
            let mt = super::main_thread()?;
            mt.toolbar().update_button(button, &workflow_id)
        })
    }

    fn remove_button(&self, target: ToolbarTarget, cmd_name: &str) -> DawResult<()> {
        let cmd_name = cmd_name.to_string();
        dispatch(self.remote, move || {
            let mt = super::main_thread()?;
            mt.toolbar().remove_button(target, &cmd_name)
        })
    }
}
