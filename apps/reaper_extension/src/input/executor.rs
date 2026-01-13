//! Input Executor
//!
//! Executes commands by running the associated REAPER actions.

use crate::input::state::{ActionKey, ActionType, Command};
use helgoboss_midi::U7;
use reaper_high::Reaper;
use reaper_medium::{ActionValueChange, CommandId, ProjectContext, WindowContext};
use tracing::{debug, warn};

/// Execute a command
pub fn execute_command(command: &Command) -> Result<(), Box<dyn std::error::Error>> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    debug!("Executing command: {:?}", command);

    // Execute each action in the sequence
    for (i, action_type) in command.action_sequence.iter().enumerate() {
        let action_key = &command.action_keys[i];

        // Look up the command ID
        let cmd_id = if let Ok(numeric_id) = action_key.identifier.parse::<u32>() {
            CommandId::new(numeric_id)
        } else if let Some(named_id) =
            medium_reaper.named_command_lookup(action_key.identifier.as_str())
        {
            named_id
        } else {
            warn!("Could not find command: {}", action_key.identifier);
            continue;
        };

        // Execute with repetition if specified
        let repetitions = action_key.repetition_count.unwrap_or(1);
        for _ in 0..repetitions {
            medium_reaper.main_on_command_ex(cmd_id, 1, ProjectContext::CurrentProject);
        }

        debug!(
            "Executed action: {} ({} times)",
            action_key.identifier, repetitions
        );
    }

    Ok(())
}

/// Execute a composed action sequence
/// This handles special composition logic (e.g., timeline_operator + timeline_motion)
pub fn execute_composed_command(command: &Command) -> Result<(), Box<dyn std::error::Error>> {
    // For now, just execute normally
    // TODO: Add composition logic similar to reaper-keys' action_sequence.lua
    execute_command(command)
}

/// Execute an action by its identifier (action name or numeric ID)
pub fn execute_action(action_id: &str) -> Result<(), Box<dyn std::error::Error>> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    // Look up command ID
    let cmd_id = if let Ok(numeric_id) = action_id.parse::<u32>() {
        CommandId::new(numeric_id)
    } else if let Some(named_id) = medium_reaper.named_command_lookup(action_id) {
        named_id
    } else {
        return Err(format!("Could not find command: {}", action_id).into());
    };

    // Execute the action
    medium_reaper.main_on_command_ex(cmd_id, 1, ProjectContext::CurrentProject);
    debug!("Executed action: {}", action_id);

    Ok(())
}

/// Execute an action for wheel/relative input
///
/// This executes an action that responds to wheel/relative input using
/// REAPER's KBD_OnMainActionEx API with relative mode encoding.
///
/// # Arguments
/// * `action_id` - Action name or numeric ID
/// * `delta` - Wheel delta (positive = up, negative = down)
///
/// # Safety
/// This function uses unsafe code to call the low-level REAPER API.
pub fn execute_wheel_action(
    action_id: &str,
    delta: i16,
) -> Result<(), Box<dyn std::error::Error>> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    // Look up command ID
    let cmd_id = if let Ok(numeric_id) = action_id.parse::<u32>() {
        CommandId::new(numeric_id)
    } else if let Some(named_id) = medium_reaper.named_command_lookup(action_id) {
        named_id
    } else {
        return Err(format!("Could not find command: {}", action_id).into());
    };

    // Create the ActionValueChange using Relative1 mode:
    // - 127 → -1 (scroll down)
    // - 1 → +1 (scroll up)
    // We scale based on wheel delta magnitude for smoother scrolling
    let abs_delta = delta.unsigned_abs().min(127) as u8;
    let value_change = if delta > 0 {
        // Scroll up: value = 1 means +1
        // For larger deltas, repeat or use higher value
        ActionValueChange::Relative1(U7::new(abs_delta.min(63)))
    } else {
        // Scroll down: value = 127 means -1
        // For Relative1: values > 64 are negative (127 - value = magnitude)
        ActionValueChange::Relative1(U7::new(128 - abs_delta.min(63)))
    };

    // Execute using kbd_on_main_action_ex which properly handles relative values
    // SAFETY: We're passing valid command ID and project context
    unsafe {
        medium_reaper.kbd_on_main_action_ex(
            cmd_id,
            value_change,
            WindowContext::NoWindow,
            ProjectContext::CurrentProject,
        );
    }

    debug!(
        "Executed wheel action: {} (delta={}, value_change={:?})",
        action_id, delta, value_change
    );

    Ok(())
}

/// MIDI Editor section ID (32060)
const MIDI_EDITOR_SECTION_ID: u32 = 32060;

/// Execute a wheel/relative input action in the MIDI Editor
///
/// This uses `kbd_RunCommandThroughHooks` with the MIDI Editor section to support
/// smooth wheel scrolling with relative mode encoding, just like the main section.
///
/// # Arguments
/// * `action_id` - Action name or numeric ID (from MIDI Editor section)
/// * `delta` - Wheel delta (positive = up, negative = down)
///
/// # Safety
/// This function uses unsafe code to call the low-level REAPER API.
pub fn execute_midi_editor_wheel_action(
    action_id: &str,
    delta: i16,
) -> Result<(), Box<dyn std::error::Error>> {
    use reaper_medium::SectionId;

    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let low_reaper = medium_reaper.low();

    // Look up command ID
    let cmd_id = if let Ok(numeric_id) = action_id.parse::<i32>() {
        numeric_id
    } else if let Some(named_id) = medium_reaper.named_command_lookup(action_id) {
        named_id.get() as i32
    } else {
        return Err(format!("Could not find command: {}", action_id).into());
    };

    // Get the active MIDI editor window
    let Some(midi_editor_hwnd) = medium_reaper.midi_editor_get_active() else {
        return Err("No active MIDI editor".into());
    };

    // Calculate relative mode values (same as execute_wheel_action)
    // Relative1 mode: 1-63 = positive, 65-127 = negative
    let abs_delta = delta.unsigned_abs().min(63) as i32;
    let (val, valhw, relmode) = if delta > 0 {
        // Positive: val = magnitude, relmode = 1
        (abs_delta, 0, 1)
    } else {
        // Negative: val = 128 - magnitude, relmode = 1
        (128 - abs_delta, 0, 1)
    };

    // Get the MIDI Editor section and execute through hooks
    let midi_section_id = SectionId::new(MIDI_EDITOR_SECTION_ID);

    let result = medium_reaper.section_from_unique_id(midi_section_id, |section_info| {
        // Get the raw section pointer
        let section_ptr = section_info.raw().as_ptr();

        // Call kbd_RunCommandThroughHooks with the MIDI editor section
        // This supports relative mode for smooth scrolling
        unsafe {
            low_reaper.kbd_RunCommandThroughHooks(
                section_ptr,
                &cmd_id,
                &val,
                &valhw,
                &relmode,
                midi_editor_hwnd.as_ptr(),
            )
        }
    });

    match result {
        Some(handled) => {
            if handled {
                debug!(
                    "Executed MIDI editor wheel action: {} (delta={}, val={}, relmode={})",
                    action_id, delta, val, relmode
                );
            } else {
                warn!(
                    "MIDI editor wheel action not handled: {} (delta={})",
                    action_id, delta
                );
            }
            Ok(())
        }
        None => Err("Could not get MIDI editor section".into()),
    }
}
