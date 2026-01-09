//! Input Executor
//!
//! Executes commands by running the associated REAPER actions.

use crate::input::state::{ActionKey, ActionType, Command};
use reaper_high::Reaper;
use reaper_medium::{CommandId, ProjectContext};
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
