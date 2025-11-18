//! Dummy actions for FastTrackStudio REAPER Extension

use crate::action_registry::{ActionDef, register_actions};
use reaper_high::Reaper;
use tracing::info;

/// Dummy action handler - shows a message in REAPER console
fn dummy_action_handler() {
    let reaper = Reaper::get();
    reaper.show_console_msg("FastTrackStudio: Dummy action executed!\n");
    info!("Dummy action executed");
}

/// Register all actions for FastTrackStudio extension
pub fn register_all_actions() {
    let actions = vec![
        ActionDef {
            command_id: "FTS_DUMMY_ACTION",
            display_name: "Dummy Action".to_string(),
            handler: dummy_action_handler,
            appears_in_menu: true, // Show in menu
        },
    ];
    
    register_actions(&actions, "FastTrackStudio");
}

