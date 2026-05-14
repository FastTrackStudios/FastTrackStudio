//! Custom action registration with the host (REAPER, etc.).
//!
//! Setup-time service used during plugin/extension init to register named
//! command IDs that the user can bind to keys, toolbars, or menus.

use crate::DawResult;

pub trait ActionRegistry {
    /// Register a plain action; returns the host-assigned command id.
    fn register(&self, cmd_name: &str, description: &str) -> DawResult<u32>;

    /// Register an action that also appears in the host's main menu.
    fn register_in_menu(&self, cmd_name: &str, description: &str) -> DawResult<u32>;

    /// Register a toggle (on/off) action.
    fn register_toggle(&self, cmd_name: &str, description: &str) -> DawResult<u32>;

    /// Register a toggle action that also appears in the main menu.
    fn register_toggle_in_menu(&self, cmd_name: &str, description: &str) -> DawResult<u32>;

    fn unregister(&self, cmd_name: &str) -> DawResult<()>;
}
