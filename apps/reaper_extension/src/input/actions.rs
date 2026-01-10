//! FTS-Input Actions
//!
//! Actions for controlling the FTS-Input key sequence system.

use crate::infrastructure::action_registry::{ActionDef, register_actions};
use crate::input::continuous_action::start_continuous_action;
use crate::input::handler::InputHandler;
use crate::input::tempo::{register_move_grid_actions, set_move_grid_variant, MoveGridVariant};
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

/// Handler for Move Closest Measure Grid Line to Mouse action
fn move_measure_grid_to_mouse_handler() {
    set_move_grid_variant(MoveGridVariant::ClosestMeasure);
    if !start_continuous_action("FTS_MOVE_MEASURE_GRID_TO_MOUSE") {
        let reaper = Reaper::get();
        reaper.show_console_msg("Failed to start Move Measure Grid action\n");
    }
}

/// Handler for Move Closest Measure Grid Line to Mouse (Constrained) action
/// This variant adds an anchor marker one measure before to prevent stretching
fn move_measure_grid_to_mouse_constrained_handler() {
    set_move_grid_variant(MoveGridVariant::ClosestMeasureConstrained);
    if !start_continuous_action("FTS_MOVE_MEASURE_GRID_TO_MOUSE_CONSTRAINED") {
        let reaper = Reaper::get();
        reaper.show_console_msg("Failed to start Move Measure Grid (Constrained) action\n");
    }
}

/// Handler for Move Closest Measure Grid Line to Mouse (Fully Constrained) action
/// This variant adds anchor markers both before AND after to only affect two adjacent measures
fn move_measure_grid_to_mouse_fully_constrained_handler() {
    set_move_grid_variant(MoveGridVariant::ClosestMeasureFullyConstrained);
    if !start_continuous_action("FTS_MOVE_MEASURE_GRID_TO_MOUSE_FULLY_CONSTRAINED") {
        let reaper = Reaper::get();
        reaper.show_console_msg("Failed to start Move Measure Grid (Fully Constrained) action\n");
    }
}

/// Handler for Move Closest Grid Line to Mouse action
fn move_grid_to_mouse_handler() {
    set_move_grid_variant(MoveGridVariant::ClosestGrid);
    if !start_continuous_action("FTS_MOVE_GRID_TO_MOUSE") {
        let reaper = Reaper::get();
        reaper.show_console_msg("Failed to start Move Grid action\n");
    }
}

/// Handler for Move Closest Tempo Marker to Mouse action
fn move_tempo_to_mouse_handler() {
    set_move_grid_variant(MoveGridVariant::ClosestTempo);
    if !start_continuous_action("FTS_MOVE_TEMPO_TO_MOUSE") {
        let reaper = Reaper::get();
        reaper.show_console_msg("Failed to start Move Tempo action\n");
    }
}

/// Register all FTS-Input actions
pub fn register_input_actions() {
    // First, register the continuous actions with the continuous action system
    register_move_grid_actions();

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
        // Move Grid actions
        ActionDef {
            command_id: "FTS_MOVE_MEASURE_GRID_TO_MOUSE",
            display_name: "FTS: Move closest measure grid line to mouse cursor (perform until shortcut released)".to_string(),
            handler: move_measure_grid_to_mouse_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_MOVE_MEASURE_GRID_TO_MOUSE_CONSTRAINED",
            display_name: "FTS: Move closest measure grid line to mouse cursor - constrained (perform until shortcut released)".to_string(),
            handler: move_measure_grid_to_mouse_constrained_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_MOVE_MEASURE_GRID_TO_MOUSE_FULLY_CONSTRAINED",
            display_name: "FTS: Move closest measure grid line to mouse cursor - fully constrained (perform until shortcut released)".to_string(),
            handler: move_measure_grid_to_mouse_fully_constrained_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_MOVE_GRID_TO_MOUSE",
            display_name: "FTS: Move closest grid line to mouse cursor (perform until shortcut released)".to_string(),
            handler: move_grid_to_mouse_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_MOVE_TEMPO_TO_MOUSE",
            display_name: "FTS: Move closest tempo marker to mouse cursor (perform until shortcut released)".to_string(),
            handler: move_tempo_to_mouse_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
    ];

    register_actions(&actions, "Input");
}
