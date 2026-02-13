//! FTS-Input Actions
//!
//! Actions for controlling the FTS-Input key sequence system.

use crate::infrastructure::action_registry::{ActionDef, register_actions};
use crate::infrastructure::workflow_selector;
use crate::input::continuous_action::start_continuous_action;
use crate::input::handler::InputHandler;
use crate::input::item_actions;
use crate::input::keybinds;
use crate::input::mouse_modifiers::core::{MouseModifierFlag, set_mouse_modifier};
use crate::input::mouse_modifiers::manager as mouse_manager;
use crate::input::mouse_modifiers::preset as mouse_preset;
use crate::input::tempo::{
    MoveGridVariant, register_move_grid_actions, set_move_grid_variant,
    snap_grid_to_transient_constrained_handler, snap_grid_to_transient_fully_constrained_handler,
    snap_grid_to_transient_handler,
};
use crate::input::workflows;
use reaper_high::Reaper;
use tracing::{debug, info};

/// Toggle FTS-Input interception on/off
fn toggle_input_interception_handler() {
    let is_enabled = InputHandler::toggle();
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    if is_enabled {
        match mouse_preset::backup_current_modifiers_if_missing(&medium_reaper) {
            Ok(true) => info!("Created user mouse modifier backup"),
            Ok(false) => debug!("User mouse modifier backup already exists"),
            Err(e) => tracing::warn!("Failed to create user mouse modifier backup: {}", e),
        }
    } else {
        mouse_manager::disable_all_overrides();
        if let Err(e) = mouse_preset::restore_user_backup(&medium_reaper) {
            tracing::warn!("Failed to restore user mouse modifiers from backup: {}", e);
            reaper.show_console_msg(format!(
                "FTS-Input: failed to restore mouse modifier backup ({})\n",
                e
            ));
        } else {
            reaper.show_console_msg("FTS-Input: restored mouse modifiers from backup\n");
            info!("Restored user mouse modifiers from backup");
        }
    }

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

/// Toggle FTS-Input debug logging on/off
fn toggle_input_debug_logging_handler() {
    let is_enabled = InputHandler::toggle_debug_logging();
    info!(
        "FTS-Input debug logging toggled: {}",
        if is_enabled { "enabled" } else { "disabled" }
    );

    // Wake up REAPER to refresh action states
    if let Err(e) = Reaper::get().wake_up() {
        tracing::warn!("Failed to wake up REAPER after toggle: {}", e);
    }
}

/// Get the current toggle state for FTS-Input debug logging
fn get_input_debug_logging_state() -> bool {
    InputHandler::is_debug_logging()
}

/// Toggle debug mouse context logging on/off
fn toggle_debug_mouse_context_handler() {
    let is_enabled = workflows::toggle_debug_mouse_context();
    let reaper = Reaper::get();

    let status = if is_enabled { "enabled" } else { "disabled" };
    reaper.show_console_msg(format!(
        "Debug Mouse Context: {} (click anywhere to see context)\n",
        status
    ));
    info!("Debug Mouse Context toggled: {}", status);

    wake_reaper();
}

/// Get the current toggle state for debug mouse context
fn get_debug_mouse_context_state() -> bool {
    workflows::is_debug_mouse_context_enabled()
}

/// Handler for Move Closest Measure Grid Line to Mouse action
fn move_measure_grid_to_mouse_handler() {
    set_move_grid_variant(MoveGridVariant::ClosestMeasure);
    if !start_continuous_action("FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE") {
        let reaper = Reaper::get();
        reaper.show_console_msg("Failed to start Move Measure Grid action\n");
    }
}

/// Handler for Move Closest Measure Grid Line to Mouse (Constrained) action
/// This variant adds an anchor marker one measure before to prevent stretching
fn move_measure_grid_to_mouse_constrained_handler() {
    set_move_grid_variant(MoveGridVariant::ClosestMeasureConstrained);
    if !start_continuous_action("FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_CONSTRAINED") {
        let reaper = Reaper::get();
        reaper.show_console_msg("Failed to start Move Measure Grid (Constrained) action\n");
    }
}

/// Handler for Move Closest Measure Grid Line to Mouse (Fully Constrained) action
/// This variant adds anchor markers both before AND after to only affect two adjacent measures
fn move_measure_grid_to_mouse_fully_constrained_handler() {
    set_move_grid_variant(MoveGridVariant::ClosestMeasureFullyConstrained);
    if !start_continuous_action("FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_FULLY_CONSTRAINED") {
        let reaper = Reaper::get();
        reaper.show_console_msg("Failed to start Move Measure Grid (Fully Constrained) action\n");
    }
}

/// Handler for Move Closest Grid Line to Mouse action
fn move_grid_to_mouse_handler() {
    set_move_grid_variant(MoveGridVariant::ClosestGrid);
    if !start_continuous_action("FTS_TEMPO_MOVE_GRID_TO_MOUSE") {
        let reaper = Reaper::get();
        reaper.show_console_msg("Failed to start Move Grid action\n");
    }
}

/// Handler for Move Closest Tempo Marker to Mouse action
fn move_tempo_to_mouse_handler() {
    set_move_grid_variant(MoveGridVariant::ClosestTempo);
    if !start_continuous_action("FTS_TEMPO_MOVE_MARKER_TO_MOUSE") {
        let reaper = Reaper::get();
        reaper.show_console_msg("Failed to start Move Tempo action\n");
    }
}

// === Keybind Preset Actions ===

/// Apply mouse modifier profile for a preset and log the state
fn apply_preset_mouse_modifiers(preset_name: &str) {
    // Map keybind preset names to mouse modifier profile names
    let profile_name = match preset_name.to_lowercase().as_str() {
        "fastrackstudio" => "fastrackstudio",
        "logic" => "logic",
        "reaper" | "reavim" | _ => "reaper",
    };

    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    match mouse_preset::backup_current_modifiers_if_missing(&medium_reaper) {
        Ok(true) => info!("Created user mouse modifier backup"),
        Ok(false) => debug!("User mouse modifier backup already exists"),
        Err(e) => tracing::warn!("Failed to create user mouse modifier backup: {}", e),
    }

    // Apply the mouse modifier profile
    mouse_manager::set_profile(profile_name);
    mouse_manager::log_state();
}

/// Handler for setting preset to FastTrackStudio
fn set_preset_fts_handler() {
    keybinds::set_preset("fastrackstudio");
    let reaper = Reaper::get();
    reaper.show_console_msg("Keybind preset: FastTrackStudio\n");
    info!("Keybind preset changed to: FastTrackStudio");

    // Apply mouse modifiers for this preset
    apply_preset_mouse_modifiers("fastrackstudio");

    wake_reaper();
}

/// Handler for setting preset to Reaper
fn set_preset_reaper_handler() {
    keybinds::set_preset("reaper");
    let reaper = Reaper::get();
    reaper.show_console_msg("Keybind preset: Reaper (default)\n");
    info!("Keybind preset changed to: Reaper");

    // Apply mouse modifiers for this preset
    apply_preset_mouse_modifiers("reaper");

    wake_reaper();
}

/// Handler for setting preset to Logic
fn set_preset_logic_handler() {
    keybinds::set_preset("logic");
    let reaper = Reaper::get();
    reaper.show_console_msg("Keybind preset: Logic Pro\n");
    info!("Keybind preset changed to: Logic");

    // Apply mouse modifiers for this preset
    apply_preset_mouse_modifiers("logic");

    wake_reaper();
}

/// Handler for setting preset to ReaVim
fn set_preset_reavim_handler() {
    keybinds::set_preset("reavim");
    let reaper = Reaper::get();
    reaper.show_console_msg("Keybind preset: ReaVim (vim-style)\n");
    info!("Keybind preset changed to: ReaVim");

    // Apply mouse modifiers for this preset
    apply_preset_mouse_modifiers("reavim");

    wake_reaper();
}

/// Handler for toggling Tempo Map overlay
fn toggle_tempo_map_overlay_handler() {
    // Toggle keybind overlay
    let is_enabled = keybinds::toggle_override("tempo_map");

    // Also toggle mouse modifier overlay (keeps them in sync)
    mouse_manager::toggle_override("tempo_map");

    let reaper = Reaper::get();
    let status = if is_enabled { "enabled" } else { "disabled" };
    reaper.show_console_msg(format!("Tempo Map overlay: {}\n", status));
    info!("Tempo Map overlay toggled: {}", status);

    // Log current mouse modifier state
    mouse_manager::log_state();

    wake_reaper();
}

/// Get toggle state for Tempo Map overlay
fn get_tempo_map_overlay_state() -> bool {
    keybinds::is_override_active("tempo_map")
}

fn toggle_quick_edit_overlay_handler() {
    // Toggle mouse modifier overlay
    let is_enabled = mouse_manager::toggle_override("quick_edit");

    let reaper = Reaper::get();
    let status = if is_enabled { "enabled" } else { "disabled" };
    reaper.show_console_msg(format!("Quick Edit overlay: {}\n", status));
    info!("Quick Edit overlay toggled: {}", status);

    // Log current mouse modifier state
    mouse_manager::log_state();

    wake_reaper();
}

fn get_quick_edit_overlay_state() -> bool {
    mouse_manager::is_override_active("quick_edit")
}

// === Workflow Actions ===

fn toggle_tempo_mapping_workflow_handler() {
    match workflows::toggle("tempo_mapping") {
        Ok(is_active) => {
            let status = if is_active {
                "activated"
            } else {
                "deactivated"
            };
            info!("Tempo Mapping workflow {}", status);
        }
        Err(e) => {
            tracing::warn!("Failed to toggle Tempo Mapping workflow: {}", e);
        }
    }
    refresh_toolbar("FTS_WORKFLOW_TEMPO_MAPPING");
    wake_reaper();
}

fn get_tempo_mapping_workflow_state() -> bool {
    workflows::is_active("tempo_mapping")
}

fn toggle_fast_slip_edit_workflow_handler() {
    // Simple toggle - no need for native arming workarounds since we use
    // custom click interception via ArmedClickAction
    match workflows::toggle("fast_slip_edit") {
        Ok(is_active) => {
            let status = if is_active {
                "activated"
            } else {
                "deactivated"
            };
            info!("Fast Slip Edit workflow {}", status);
        }
        Err(e) => {
            tracing::warn!("Failed to toggle Fast Slip Edit workflow: {}", e);
        }
    }

    refresh_toolbar("FTS_WORKFLOW_FAST_SLIP_EDIT");
    wake_reaper();
}

fn get_fast_slip_edit_workflow_state() -> bool {
    workflows::is_active("fast_slip_edit")
}

fn deactivate_workflow_handler() {
    if workflows::deactivate() {
        info!("Workflow deactivated");
    } else {
        info!("No active workflow to deactivate");
    }
    wake_reaper();
}

fn reset_all_overrides_handler() {
    let reaper = Reaper::get();

    // Deactivate any active workflow first
    workflows::deactivate();

    // Disable all keybind overlays
    keybinds::disable_all_overrides();

    // Disable all mouse modifier overlays
    mouse_manager::disable_all_overrides();

    reaper.show_console_msg("All overlays cleared, returned to base profile\n");
    info!("Reset all overrides - returned to base profile");

    // Log current state
    mouse_manager::log_state();

    wake_reaper();
}

fn reset_mouse_to_profile_handler() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    // Disable all mouse modifier overlays (returns to base profile)
    mouse_manager::disable_all_overrides();

    if let Err(e) = mouse_preset::restore_user_backup(&medium_reaper) {
        tracing::warn!("Failed to restore user mouse modifiers from backup: {}", e);
        reaper.show_console_msg(format!(
            "FTS: failed to restore mouse modifier backup ({})\n",
            e
        ));
    } else {
        reaper.show_console_msg("Mouse modifiers restored from backup\n");
        info!("Restored mouse modifiers from backup");
    }

    // Log current state
    mouse_manager::log_state();

    wake_reaper();
}

/// "Run" action for Fast Slip Edit workflow - this is the armable action
/// When armed and clicked, this runs the workflow's primary action (split with crossfade)
fn run_fast_slip_edit_handler() {
    // Check if the workflow is active
    if workflows::is_active("fast_slip_edit") {
        // Run the workflow's primary action (split with crossfade)
        item_actions::split_items_with_crossfade_left();
        info!("Fast Slip Edit: Executed split with crossfade");
    } else {
        // Workflow not active - just activate it (user probably meant to toggle)
        info!("Fast Slip Edit workflow not active, activating...");
        let _ = workflows::activate("fast_slip_edit");
    }
}

// === Item Editing Actions ===

fn split_items_crossfade_left_handler() {
    item_actions::split_items_with_crossfade_left();
}

/// Helper to refresh REAPER UI after state changes
/// Uses RefreshToolbar2 to update toggle button states
fn refresh_toolbar(command_id: &str) {
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();

    // Try to get the command ID for our action
    if let Some(cmd_id) = crate::infrastructure::action_registry::get_command_id(command_id) {
        unsafe {
            // Section 0 = main section
            medium.low().RefreshToolbar2(0, cmd_id.get() as i32);
        }
        debug!(command_id = %command_id, cmd_id = cmd_id.get(), "Refreshed toolbar");
    }
}

/// Helper to wake up REAPER after state changes
fn wake_reaper() {
    if let Err(e) = Reaper::get().wake_up() {
        tracing::warn!("Failed to wake up REAPER: {}", e);
    }
}

// === Dev Actions for Mouse Modifier Discovery ===

/// Dev action: Test and log mouse modifier behavior IDs for MM_CTX_ITEM_CLK
/// Sets behavior IDs 0-30 to different modifier flags so you can check REAPER preferences
fn dev_test_mouse_modifier_ids_handler() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    reaper.show_console_msg("\n=== Mouse Modifier Behavior ID Test ===\n");
    reaper.show_console_msg("Context: MM_CTX_ITEM_CLK (Media Item - left click)\n");
    reaper.show_console_msg("Setting behavior IDs to modifier flags for discovery...\n\n");

    // Test context - can be changed to test other contexts
    let context = "MM_CTX_ITEM_CLK";

    // Set IDs 16-25 to flags 0-9 to discover the unknown behaviors
    reaper.show_console_msg("Setting IDs 16-25 to discover unknown behaviors:\n");
    reaper.show_console_msg("  Flag 0 (Default) = ID 16\n");
    reaper.show_console_msg("  Flag 1 (Shift) = ID 17\n");
    reaper.show_console_msg("  Flag 2 (Cmd) = ID 18\n");
    reaper.show_console_msg("  Flag 3 (Shift+Cmd) = ID 19\n");
    reaper.show_console_msg("  Flag 4 (Alt) = ID 20\n");
    reaper.show_console_msg("  Flag 5 (Shift+Alt) = ID 21\n");
    reaper.show_console_msg("  Flag 6 (Cmd+Alt) = ID 22\n");
    reaper.show_console_msg("  Flag 7 (Shift+Cmd+Alt) = ID 23\n");
    reaper.show_console_msg("  Flag 8 (Win/Ctrl-Mac) = ID 24\n");
    reaper.show_console_msg("  Flag 9 (Shift+Win) = ID 25\n\n");

    // Set IDs 16-25 to flags 0-9
    for (i, id) in (16..=25).enumerate() {
        let flag = MouseModifierFlag::from_flag(i as i32);
        let behavior_str = format!("{} m", id);
        if let Err(e) = set_mouse_modifier(context, flag, &behavior_str, medium_reaper) {
            reaper.show_console_msg(format!("  Error setting ID {}: {}\n", id, e));
        }
    }

    reaper.show_console_msg(
        "Done! Open REAPER Preferences > Mouse Modifiers > Media item > left click\n",
    );
    reaper.show_console_msg("to see what behavior names correspond to each modifier.\n\n");
    reaper.show_console_msg("The modifier flags map as follows:\n");
    reaper.show_console_msg("  Default action = ID 16\n");
    reaper.show_console_msg("  Shift = ID 17\n");
    reaper.show_console_msg("  Cmd = ID 18\n");
    reaper.show_console_msg("  Shift+Cmd = ID 19\n");
    reaper.show_console_msg("  Opt = ID 20 (should be 'Extend razor edit area' per docs)\n");
    reaper.show_console_msg("  Shift+Opt = ID 21\n");
    reaper.show_console_msg("  Cmd+Opt = ID 22\n");
    reaper.show_console_msg("  Shift+Cmd+Opt = ID 23\n");
    reaper.show_console_msg("  Ctrl (Mac) / Win (Windows) = ID 24\n");
    reaper.show_console_msg("  Shift+Ctrl/Win = ID 25\n");

    info!("Mouse modifier behavior ID test complete - check REAPER preferences");
}

/// Dev action: Reset MM_CTX_ITEM_CLK to defaults
fn dev_reset_item_click_modifiers_handler() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    reaper.show_console_msg("\n=== Resetting MM_CTX_ITEM_CLK to defaults ===\n");

    let context = "MM_CTX_ITEM_CLK";

    // Reset all 16 modifier flags to default (-1)
    for flag_num in 0..16 {
        let flag = MouseModifierFlag::from_flag(flag_num);
        let _ = set_mouse_modifier(context, flag, "-1", medium_reaper);
    }

    reaper.show_console_msg("Done! All Media Item click modifiers reset to defaults.\n");
    info!("MM_CTX_ITEM_CLK reset to defaults");
}

/// Get all FTS-Input action definitions (for batch registration)
pub fn get_input_action_defs() -> Vec<ActionDef> {
    // First, register the continuous actions with the continuous action system
    register_move_grid_actions();

    vec![
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
        ActionDef {
            command_id: "FTS_INPUT_TOGGLE_DEBUG_LOGGING",
            display_name: "Toggle FTS-Input Debug Logging (logs all keys to console)".to_string(),
            handler: toggle_input_debug_logging_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: Some(get_input_debug_logging_state),
        },
        ActionDef {
            command_id: "FTS_DEBUG_MOUSE_CONTEXT",
            display_name: "Toggle Debug Mouse Context (logs MM_CTX on every click)".to_string(),
            handler: toggle_debug_mouse_context_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: Some(get_debug_mouse_context_state),
        },
        // Move Grid actions (TempoMap section)
        ActionDef {
            command_id: "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE",
            display_name: "Move closest measure grid line to mouse cursor (perform until shortcut released)".to_string(),
            handler: move_measure_grid_to_mouse_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_CONSTRAINED",
            display_name: "Move closest measure grid line to mouse cursor - constrained (perform until shortcut released)".to_string(),
            handler: move_measure_grid_to_mouse_constrained_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_FULLY_CONSTRAINED",
            display_name: "Move closest measure grid line to mouse cursor - fully constrained (perform until shortcut released)".to_string(),
            handler: move_measure_grid_to_mouse_fully_constrained_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_TEMPO_MOVE_GRID_TO_MOUSE",
            display_name: "Move closest grid line to mouse cursor (perform until shortcut released)".to_string(),
            handler: move_grid_to_mouse_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_TEMPO_MOVE_MARKER_TO_MOUSE",
            display_name: "Move closest tempo marker to mouse cursor (perform until shortcut released)".to_string(),
            handler: move_tempo_to_mouse_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        // Snap Grid to Transient actions (TempoMap section)
        ActionDef {
            command_id: "FTS_TEMPO_SNAP_GRID_TO_TRANSIENT",
            display_name: "Snap closest measure grid line to next transient".to_string(),
            handler: snap_grid_to_transient_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_TEMPO_SNAP_GRID_TO_TRANSIENT_CONSTRAINED",
            display_name: "Snap closest measure grid line to next transient - constrained".to_string(),
            handler: snap_grid_to_transient_constrained_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_TEMPO_SNAP_GRID_TO_TRANSIENT_FULLY_CONSTRAINED",
            display_name: "Snap closest measure grid line to next transient - fully constrained".to_string(),
            handler: snap_grid_to_transient_fully_constrained_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        // Keybind Preset actions (Keybinds section)
        ActionDef {
            command_id: "FTS_KEYBIND_SET_PRESET_FTS",
            display_name: "Set keybind preset: FastTrackStudio".to_string(),
            handler: set_preset_fts_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_KEYBIND_SET_PRESET_REAPER",
            display_name: "Set keybind preset: Reaper (default)".to_string(),
            handler: set_preset_reaper_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_KEYBIND_SET_PRESET_LOGIC",
            display_name: "Set keybind preset: Logic Pro".to_string(),
            handler: set_preset_logic_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_KEYBIND_SET_PRESET_REAVIM",
            display_name: "Set keybind preset: ReaVim (vim-style)".to_string(),
            handler: set_preset_reavim_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_KEYBIND_TOGGLE_TEMPO_MAP",
            display_name: "Toggle Tempo Map keybinds overlay".to_string(),
            handler: toggle_tempo_map_overlay_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: Some(get_tempo_map_overlay_state),
        },
        ActionDef {
            command_id: "FTS_TOGGLE_QUICK_EDIT",
            display_name: "Toggle Quick Edit mouse modifiers overlay".to_string(),
            handler: toggle_quick_edit_overlay_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: Some(get_quick_edit_overlay_state),
        },
        // === Workflow Actions ===
        ActionDef {
            command_id: "FTS_WORKFLOW_TEMPO_MAPPING",
            display_name: "Workflow: Tempo Mapping".to_string(),
            handler: toggle_tempo_mapping_workflow_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: Some(get_tempo_mapping_workflow_state),
        },
        ActionDef {
            command_id: "FTS_WORKFLOW_FAST_SLIP_EDIT",
            display_name: "Workflow: Fast Slip Edit".to_string(),
            handler: toggle_fast_slip_edit_workflow_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: Some(get_fast_slip_edit_workflow_state),
        },
        ActionDef {
            command_id: "FTS_WORKFLOW_DEACTIVATE",
            display_name: "Workflow: Deactivate current workflow".to_string(),
            handler: deactivate_workflow_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_RESET_ALL_OVERRIDES",
            display_name: "FTS: Reset all overlays (return to base profile)".to_string(),
            handler: reset_all_overrides_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_MOUSE_RESET_TO_PROFILE",
            display_name: "FTS: Reset mouse modifiers to base profile".to_string(),
            handler: reset_mouse_to_profile_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        // === Workflow Run Actions (armable) ===
        ActionDef {
            command_id: "FTS_WORKFLOW_RUN_FAST_SLIP_EDIT",
            display_name: "Workflow Run: Fast Slip Edit (armable)".to_string(),
            handler: run_fast_slip_edit_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        // === Item Editing Actions ===
        ActionDef {
            command_id: "FTS_SPLIT_ITEMS_CROSSFADE_LEFT",
            display_name: "FTS: Split selected items at cursor with crossfade on left".to_string(),
            handler: split_items_crossfade_left_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        // === Profile Selector ===
        ActionDef {
            command_id: "FTS_PROFILE_SELECTOR",
            display_name: "FTS: Profile Selector (popup menu)".to_string(),
            handler: workflow_selector::profile_selector_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: Some(workflow_selector::get_profile_selector_state),
        },
        // === Workflow Selector ===
        ActionDef {
            command_id: "FTS_WORKFLOW_SELECTOR",
            display_name: "FTS: Workflow Selector (popup menu)".to_string(),
            handler: workflow_selector::workflow_selector_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: Some(workflow_selector::get_workflow_selector_state),
        },
        // === Dev Actions for Mouse Modifier Discovery ===
        ActionDef {
            command_id: "FTS_DEV_TEST_MOUSE_MODIFIER_IDS",
            display_name: "Test Mouse Modifier Behavior IDs (sets IDs 16-25 to discover names)".to_string(),
            handler: dev_test_mouse_modifier_ids_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
        ActionDef {
            command_id: "FTS_DEV_RESET_ITEM_CLICK_MODIFIERS",
            display_name: "Reset Media Item Click Modifiers to defaults".to_string(),
            handler: dev_reset_item_click_modifiers_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            toggle_state: None,
        },
    ]
}

/// Register all FTS-Input actions (legacy - prefer using get_input_action_defs for batch registration)
pub fn register_input_actions() {
    info!("🎯 register_input_actions() called - registering FTS-Input actions");
    let actions = get_input_action_defs();
    info!("📝 Calling register_actions with {} actions", actions.len());
    register_actions(&actions, "Input");
    info!("✅ register_input_actions() completed");
}
