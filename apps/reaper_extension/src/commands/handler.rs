//! Command Handler
//!
//! Main handler that processes keypresses and manages the command system.
//! Uses TranslateAccel to intercept keypresses before REAPER processes them.

use crate::commands::bindings::Bindings;
use crate::commands::executor::execute_composed_command;
use crate::commands::matcher::{default_action_sequences, match_sequence};
use crate::commands::state::{CommandState, Context, KeyPress, Mode};
use reaper_high::Reaper;
use reaper_medium::{
    AccelMsgKind, AcceleratorBehavior, AcceleratorKeyCode, AcceleratorPosition, TranslateAccel,
    TranslateAccelArgs, TranslateAccelResult,
};
use std::sync::OnceLock;
// TODO: Add text focus detection
use tracing::{debug, info, warn};

/// Global bindings instance
static BINDINGS: OnceLock<Bindings> = OnceLock::new();

/// Global action sequences
static ACTION_SEQUENCES: OnceLock<crate::commands::matcher::ActionSequences> = OnceLock::new();

/// Global state for whether FTS-Commands interception is enabled
static INTERCEPTION_ENABLED: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(true);

/// Global state for whether FTS-Commands should eat keys or just log them (passthrough mode)
static PASSTHROUGH_MODE: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Command handler that processes keyboard input via TranslateAccel
///
/// This intercepts keypresses BEFORE REAPER processes them, allowing us
/// to build key sequences similar to reaper-keys.
pub struct CommandHandler {
    // Handler state
}

impl CommandHandler {
    pub fn new() -> Self {
        // Initialize bindings and sequences
        BINDINGS.get_or_init(|| Bindings::default());
        ACTION_SEQUENCES.get_or_init(|| default_action_sequences());

        Self {}
    }

    /// Convert a key code and modifiers to a key string representation
    fn key_to_string(
        key: AcceleratorKeyCode,
        behavior: &enumflags2::BitFlags<AcceleratorBehavior>,
    ) -> String {
        let key_code = key.get();

        // Check modifiers
        let ctrl = behavior.contains(AcceleratorBehavior::Control);
        let alt = behavior.contains(AcceleratorBehavior::Alt);
        let shift = behavior.contains(AcceleratorBehavior::Shift);

        // Build modifier prefix
        let mut prefix = String::new();
        if ctrl {
            prefix.push_str("<C-");
        }
        if alt {
            prefix.push_str("<M-");
        }
        if shift {
            prefix.push_str("<S-");
        }

        // Convert key code to string representation
        let key_str = match key_code {
            // Letters (A-Z)
            65..=90 => {
                let ch = if shift {
                    char::from_u32(key_code as u32).unwrap_or('?')
                } else {
                    char::from_u32((key_code + 32) as u32).unwrap_or('?') // Convert to lowercase
                };
                ch.to_string()
            }
            // Numbers (0-9)
            48..=57 => char::from_u32(key_code as u32).unwrap_or('?').to_string(),
            // Special keys
            8 => "<BS>".to_string(),
            9 => "<TAB>".to_string(),
            13 => "<return>".to_string(),
            27 => "<ESC>".to_string(),
            32 => "<SPC>".to_string(),
            // Arrow keys (virtual key codes)
            0x25 => "<left>".to_string(),  // VK_LEFT
            0x26 => "<up>".to_string(),    // VK_UP
            0x27 => "<right>".to_string(), // VK_RIGHT
            0x28 => "<down>".to_string(),  // VK_DOWN
            // Special characters
            33 => "!".to_string(),
            34 => "\"".to_string(),
            35 => "#".to_string(),
            36 => "$".to_string(),
            37 => "%".to_string(),
            38 => "&".to_string(),
            40 => "(".to_string(),
            41 => ")".to_string(),
            42 => "*".to_string(),
            43 => "+".to_string(),
            44 => ",".to_string(),
            45 => "-".to_string(),
            46 => ".".to_string(),
            47 => "/".to_string(),
            58 => ":".to_string(),
            59 => ";".to_string(),
            60 => "<".to_string(),
            61 => "=".to_string(),
            62 => ">".to_string(),
            63 => "?".to_string(),
            64 => "@".to_string(),
            91 => "[".to_string(),
            92 => "\\".to_string(),
            93 => "]".to_string(),
            94 => "^".to_string(),
            95 => "_".to_string(),
            96 => "`".to_string(),
            123 => "{".to_string(),
            124 => "|".to_string(),
            125 => "}".to_string(),
            126 => "~".to_string(),
            _ => format!("<{}>", key_code),
        };

        if prefix.is_empty() {
            key_str
        } else {
            format!("{}{}>", prefix, key_str)
        }
    }

    /// Process a keypress
    /// Returns (should_eat, is_complete_command)
    fn process_keypress(
        &mut self,
        key: AcceleratorKeyCode,
        behavior: &enumflags2::BitFlags<AcceleratorBehavior>,
        context: Context,
        context_name: &str,
    ) -> (bool, bool) {
        // TODO: Don't process if text is focused
        // For now, process all keypresses

        // Convert key to string representation
        let key_str = Self::key_to_string(key, behavior);

        let keypress = KeyPress {
            key: key_str,
            context,
        };

        // Get current state
        let mut state = CommandState::get();

        // Append key to sequence
        state.append_key(keypress);

        // Try to match against bindings
        let bindings = BINDINGS.get().unwrap();
        let sequences = ACTION_SEQUENCES.get().unwrap();

        if let Some((command, completions)) = match_sequence(&state, bindings, sequences) {
            // If we have a complete command, execute it
            if !command.action_sequence.is_empty() {
                debug!("Matched command: {:?}", command);

                // Log key sequence to REAPER console (context already logged above)
                let reaper = Reaper::get();
                reaper.show_console_msg(format!(
                    "FTS-Commands: Key sequence '{}' executed in {}\n",
                    state.key_sequence, context_name
                ));

                // Execute the command
                if let Err(e) = execute_composed_command(&command) {
                    warn!("Failed to execute command: {}", e);
                }

                // Store as last command for repeat
                state.last_command = Some(command);

                // Clear sequence
                state.clear_sequence();

                // Save state
                CommandState::set(state);

                return (true, true); // We handled this keypress and it's a complete command
            }

            // If we have completions, we're building a sequence - eat the key
            if let Some(completions) = completions {
                debug!("Possible completions: {:?}", completions);
                // Save state with the new key in sequence
                CommandState::set(state);
                return (true, false); // Eat the key while building sequence
            } else {
                // No match and no completions - invalid sequence
                debug!("Invalid key sequence: {}", state.key_sequence);
                state.clear_sequence();
                CommandState::set(state);
                return (false, false); // Don't eat, invalid sequence
            }
        }

        // No match at all - don't eat
        CommandState::set(state);
        (false, false)
    }

    /// Determine context from current window/section
    fn determine_context() -> (Context, String) {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();

        // Check if MIDI editor is active
        if medium_reaper.midi_editor_get_active().is_some() {
            (Context::Midi, "MIDI Editor".to_string())
        } else {
            (Context::Main, "Main Window".to_string())
        }
    }
}

/// TranslateAccel implementation for intercepting keypresses
impl TranslateAccel for CommandHandler {
    fn call(&mut self, args: TranslateAccelArgs) -> TranslateAccelResult {
        // Check if interception is enabled
        if !INTERCEPTION_ENABLED.load(std::sync::atomic::Ordering::Relaxed) {
            return TranslateAccelResult::PassOnToWindow;
        }

        let msg_type = args.msg.message();

        // Only process KeyDown and SysKeyDown events
        if msg_type != AccelMsgKind::KeyDown && msg_type != AccelMsgKind::SysKeyDown {
            return TranslateAccelResult::PassOnToWindow;
        }

        let key = args.msg.key();
        let behavior = args.msg.behavior();
        let (context, context_name) = Self::determine_context();

        // Convert key to string for logging
        let key_str = Self::key_to_string(key, &behavior);

        // Log ALL keypresses to REAPER console for testing
        let reaper = Reaper::get();
        reaper.show_console_msg(format!(
            "FTS-Commands: Key '{}' pressed in {} (Context: {:?})\n",
            key_str, context_name, context
        ));

        // Process the keypress
        let (should_eat, is_complete_command) =
            self.process_keypress(key, &behavior, context, &context_name);

        // If in passthrough mode, always pass through even if we handled it
        if PASSTHROUGH_MODE.load(std::sync::atomic::Ordering::Relaxed) {
            TranslateAccelResult::PassOnToWindow
        } else if should_eat {
            // Eat the key - either we matched a complete command or we're building a sequence
            TranslateAccelResult::Eat
        } else {
            // No match, pass through to REAPER
            TranslateAccelResult::PassOnToWindow
        }
    }
}

impl CommandHandler {
    /// Check if interception is enabled
    pub fn is_enabled() -> bool {
        INTERCEPTION_ENABLED.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Set interception enabled state
    pub fn set_enabled(enabled: bool) {
        INTERCEPTION_ENABLED.store(enabled, std::sync::atomic::Ordering::Relaxed);
        info!(
            "FTS-Commands interception {}",
            if enabled { "enabled" } else { "disabled" }
        );
    }

    /// Toggle interception enabled state
    pub fn toggle() -> bool {
        let new_state = !Self::is_enabled();
        Self::set_enabled(new_state);
        new_state
    }

    /// Check if passthrough mode is enabled
    pub fn is_passthrough() -> bool {
        PASSTHROUGH_MODE.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Set passthrough mode
    pub fn set_passthrough(enabled: bool) {
        PASSTHROUGH_MODE.store(enabled, std::sync::atomic::Ordering::Relaxed);
        info!(
            "FTS-Commands passthrough mode {}",
            if enabled { "enabled" } else { "disabled" }
        );
    }

    /// Toggle passthrough mode
    pub fn toggle_passthrough() -> bool {
        let new_state = !Self::is_passthrough();
        Self::set_passthrough(new_state);
        new_state
    }
}

/// Register the command handler
pub fn register_command_handler() -> Result<(), Box<dyn std::error::Error>> {
    info!("Registering FTS-Commands handler");

    let reaper = Reaper::get();
    let handler = Box::new(CommandHandler::new());

    // Register with accelerator register at the front (intercepts before REAPER processes)
    reaper
        .medium_session()
        .plugin_register_add_accelerator_register(handler, AcceleratorPosition::Front)?;

    info!("FTS-Commands handler registered successfully");

    Ok(())
}
