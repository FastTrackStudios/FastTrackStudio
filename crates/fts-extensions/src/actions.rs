//! FTS action definitions.
//!
//! Each entry is `(command_id, display_name, handler)`.
//! Actions are registered with REAPER under the "FTS: <display_name>" label.

use std::sync::Arc;

pub type ActionDefs = Vec<(String, String, Arc<dyn Fn() + Send + Sync>)>;

/// Build the list of all FTS utility actions.
pub fn build_action_defs() -> ActionDefs {
    vec![
        // Tempo grid actions will be added here
        // Item editing actions will be added here
    ]
}
