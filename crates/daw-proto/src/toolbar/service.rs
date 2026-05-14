//! Toolbar service traits.
//!
//! Operations are queued and applied from the host's timer callback
//! to avoid re-entrancy issues inside DAW callbacks.

use super::{ToolbarButton, ToolbarIcon, ToolbarResult, ToolbarTarget, TrackedButton};
use crate::DawResult;
use vox::service;

#[service]
pub trait ToolbarService {
    /// Add a toolbar button. Returns the resolved command ID.
    ///
    /// If the button already exists, this is a no-op. `workflow_id`
    /// groups buttons for batch removal.
    async fn add_button(&self, button: ToolbarButton, workflow_id: String) -> ToolbarResult;

    /// Update an existing toolbar button (or add if not present).
    async fn update_button(&self, button: ToolbarButton, workflow_id: String) -> ToolbarResult;

    async fn remove_button(&self, target: ToolbarTarget, command_name: String) -> ToolbarResult;

    /// Move a toolbar button to a zero-based position.
    async fn move_button(
        &self,
        target: ToolbarTarget,
        command_name: String,
        position: u32,
    ) -> ToolbarResult;

    /// Set or clear a button's icon while preserving label + flags.
    async fn set_button_icon(
        &self,
        target: ToolbarTarget,
        command_name: String,
        icon: Option<ToolbarIcon>,
    ) -> ToolbarResult;

    /// Remove all buttons belonging to a workflow.
    async fn remove_workflow_buttons(&self, workflow_id: String) -> ToolbarResult;

    async fn is_available(&self) -> bool;
    async fn get_tracked_buttons(&self) -> Vec<TrackedButton>;
}

/// Sync handle counterpart — minimal subset for in-process use.
/// Snapshots / bulk capture stay on the async service.
pub trait Toolbar {
    fn is_available(&self) -> bool;
    fn add_button(&self, button: ToolbarButton, workflow_id: &str) -> DawResult<()>;
    fn update_button(&self, button: ToolbarButton, workflow_id: &str) -> DawResult<()>;
    fn remove_button(&self, target: ToolbarTarget, cmd_name: &str) -> DawResult<()>;
}
