//! FTS-Input Actions
//!
//! Actions for controlling the FTS-Input key sequence system.

use crate::infrastructure::action_registry::{ActionDef, register_actions};
use crate::input::handler::InputHandler;
use reaper_high::Reaper;
use tracing::info;

/// Toggle FTS-Input interception on/off
fn toggle_input_interception_handler() {
    let is_enabled = InputHandler::toggle();
    let reaper = Reaper::get();

    let status = if is_enabled { "enabled" } else { "disabled" };
    reaper.show_console_msg(format!("FTS-Input interception: {}\n", status));
    info!("FTS-Input interception toggled: {}", status);

    // Wake up REAPER to refresh action states
    if let Err(e) = reaper.wake_up() {
        tracing::warn!("Failed to wake up REAPER after toggle: {}", e);
    }
}

/// Get the current toggle state for FTS-Input interception
fn get_input_interception_state() -> bool {
    InputHandler::is_enabled()
}

/// Toggle FTS-Input passthrough mode on/off
fn toggle_input_passthrough_handler() {
    let is_passthrough = InputHandler::toggle_passthrough();
    let reaper = Reaper::get();

    let status = if is_passthrough {
        "enabled (logging only)"
    } else {
        "disabled (intercepting)"
    };
    reaper.show_console_msg(format!("FTS-Input passthrough mode: {}\n", status));
    info!("FTS-Input passthrough mode toggled: {}", status);

    // Wake up REAPER to refresh action states
    if let Err(e) = reaper.wake_up() {
        tracing::warn!("Failed to wake up REAPER after toggle: {}", e);
    }
}

/// Get the current toggle state for FTS-Input passthrough mode
fn get_input_passthrough_state() -> bool {
    InputHandler::is_passthrough()
}

/// Register all FTS-Input actions
pub fn register_input_actions() {
    let actions = vec![
        ActionDef {
            command_id: "FTS_INPUT_TOGGLE",
            display_name: "Toggle FTS-Input Interception".to_string(),
            handler: toggle_input_interception_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: Some(get_input_interception_state),
        },
        ActionDef {
            command_id: "FTS_INPUT_TOGGLE_PASSTHROUGH",
            display_name: "Toggle FTS-Input Passthrough Mode".to_string(),
            handler: toggle_input_passthrough_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: Some(get_input_passthrough_state),
        },
    ];

    register_actions(&actions, "Input");
}
