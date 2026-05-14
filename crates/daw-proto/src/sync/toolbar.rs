//! Host-toolbar button management.
//!
//! Minimal sync surface: add a button, remove a button, and check whether the
//! host supports toolbar manipulation. Snapshots and bulk capture stay on the
//! async service.

use crate::{DawResult, ToolbarButton, ToolbarTarget};

pub trait Toolbar {
    fn is_available(&self) -> bool;

    fn add_button(&self, button: ToolbarButton, workflow_id: &str) -> DawResult<()>;
    fn update_button(&self, button: ToolbarButton, workflow_id: &str) -> DawResult<()>;
    fn remove_button(&self, target: ToolbarTarget, cmd_name: &str) -> DawResult<()>;
}
