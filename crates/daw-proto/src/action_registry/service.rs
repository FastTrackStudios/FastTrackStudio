//! Action registry service traits.

use super::{ActionEvent, ActionExecutionResult, ActionListRequest, ActionListResponse};
use crate::DawResult;
use vox::{Tx, service};

/// Dynamic action registration for DAW extensions and clients.
///
/// Extensions use this service to register REAPER actions at runtime.
/// Registered actions appear in REAPER's action list and can be
/// assigned keyboard shortcuts.
#[service]
pub trait ActionRegistryService {
    /// Register a new REAPER action.
    ///
    /// - `command_name`: Unique identifier (e.g., "FTS_SIGNAL_ARM").
    /// - `description`: Human-readable label.
    /// - `show_in_menu`: If true, appears in REAPER's Extensions menu.
    /// - `toggleable`: If true, REAPER shows an on/off indicator.
    ///
    /// Returns the numeric command ID assigned by REAPER, or 0 on
    /// failure.
    async fn register_action(
        &self,
        command_name: String,
        description: String,
        show_in_menu: bool,
        toggleable: bool,
    ) -> u32;

    /// Returns `true` if the action was found and unregistered.
    async fn unregister_action(&self, command_name: String) -> bool;

    /// Check if an action is registered (by us or any other extension).
    async fn is_registered(&self, command_name: String) -> bool;

    /// Look up the numeric command ID for a named action.
    async fn lookup_command_id(&self, command_name: String) -> Option<u32>;

    /// Subscribe to action trigger events. Receives events for ALL
    /// actions registered through this service — filter by
    /// `command_name` if needed.
    async fn subscribe_actions(&self, tx: Tx<ActionEvent>);

    /// Check if an action is actually present in REAPER's action list
    /// (main section). Verifies the gaccel entry exists.
    async fn is_in_action_list(&self, command_name: String) -> bool;

    /// Enumerate actions in REAPER's main action list.
    async fn list_actions(&self, request: ActionListRequest) -> ActionListResponse;

    /// Execute a native DAW command by numeric ID. Maps to
    /// `Main_OnCommandEx(command_id, 0, current_project)`.
    async fn execute_command(&self, command_id: u32);

    /// Execute a named action (custom or native).
    async fn execute_named_action(&self, command_name: String) -> bool;

    /// Execute any REAPER action and return resolved metadata.
    /// Accepts numeric ID or named command identifier.
    async fn execute_action(&self, action_id: String) -> ActionExecutionResult;

    /// Set the toggle state for a toggleable action. REAPER queries
    /// state synchronously on the main thread.
    async fn set_toggle_state(&self, command_name: String, is_on: bool);

    /// Returns `None` if not registered or not toggleable.
    async fn get_toggle_state(&self, command_name: String) -> Option<bool>;
}

/// Sync handle counterpart — setup-time registration helpers used
/// during plugin/extension init.
pub trait ActionRegistry {
    /// Returns the host-assigned command id.
    fn register(&self, cmd_name: &str, description: &str) -> DawResult<u32>;
    fn register_in_menu(&self, cmd_name: &str, description: &str) -> DawResult<u32>;
    fn register_toggle(&self, cmd_name: &str, description: &str) -> DawResult<u32>;
    fn register_toggle_in_menu(&self, cmd_name: &str, description: &str) -> DawResult<u32>;
    fn unregister(&self, cmd_name: &str) -> DawResult<()>;
}
