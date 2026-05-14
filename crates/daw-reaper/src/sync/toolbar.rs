//! Sync `Toolbar` impl — direct calls to the same `*_immediate` helpers
//! that the async `ToolbarService` uses.

use daw_proto::{DawError, DawResult, ToolbarButton, ToolbarTarget, sync::Toolbar};

use crate::toolbar as tb;

use super::ReaperMainThread;

pub struct ReaperToolbar<'a> {
    _mt: &'a ReaperMainThread,
}

impl<'a> ReaperToolbar<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread) -> Self {
        Self { _mt: mt }
    }
}

fn ensure_available() -> DawResult<()> {
    if tb::is_api_available() {
        Ok(())
    } else {
        Err(DawError::not_supported(
            "Dynamic toolbar API not available in this REAPER build",
        ))
    }
}

impl<'a> Toolbar for ReaperToolbar<'a> {
    fn is_available(&self) -> bool {
        tb::is_api_available()
    }

    fn add_button(&self, button: ToolbarButton, workflow_id: &str) -> DawResult<()> {
        ensure_available()?;
        tb::add_button_immediate(&button, workflow_id)
            .map(|_| ())
            .map_err(DawError::operation_failed)
    }

    fn update_button(&self, button: ToolbarButton, workflow_id: &str) -> DawResult<()> {
        ensure_available()?;
        tb::update_button_immediate(&button, workflow_id)
            .map(|_| ())
            .map_err(DawError::operation_failed)
    }

    fn remove_button(&self, target: ToolbarTarget, cmd_name: &str) -> DawResult<()> {
        ensure_available()?;
        tb::remove_button_immediate(&target, cmd_name).map_err(DawError::operation_failed)
    }
}
