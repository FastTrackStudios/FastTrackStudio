//! Input Handler
//!
//! Main handler that processes keypresses and manages the input system.
//! Uses TranslateAccel to intercept keypresses before REAPER processes them.
//!
//! Key events flow through the `input::InputProcessor` state machine via the
//! `processor` module. The processor handles both single-key bindings and
//! multi-key sequences (including which-key prefix trees) natively.

use crate::input::keybinds::KeybindContext;
use crate::input::state::Context;
use crate::input::window_detection;
use input::command::InputCommand;
use reaper_high::Reaper;
use reaper_low::raw;
use reaper_medium::{
    AccelMsgKind, AcceleratorBehavior, AcceleratorKeyCode, AcceleratorPosition,
    TranslateAccel, TranslateAccelArgs, TranslateAccelResult,
};
use swell_ui::Window;
use tracing::{debug, info};

/// Global state for whether FTS-Input interception is enabled
static INTERCEPTION_ENABLED: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Global state for whether FTS-Input should eat keys or just log them (passthrough mode)
static PASSTHROUGH_MODE: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Global state for debug logging mode (logs all key events to REAPER console)
static DEBUG_LOGGING: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Global state for whether the handler is currently registered
/// When false, the handler is not registered at all (completely transparent)
static HANDLER_REGISTERED: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Input handler that processes keyboard input via TranslateAccel
///
/// This intercepts keypresses BEFORE REAPER processes them, allowing us
/// to build key sequences similar to reaper-keys.
pub struct InputHandler {
    // Handler state
}

impl InputHandler {
    pub fn new() -> Self {
        Self {}
    }

    /// Convert a key code and modifiers to a key string representation (for debug logging)
    fn key_to_string(
        key: AcceleratorKeyCode,
        behavior: &enumflags2::BitFlags<AcceleratorBehavior>,
    ) -> String {
        let key_code = key.get();

        // Check modifiers
        let ctrl = behavior.contains(AcceleratorBehavior::Control);
        let alt = behavior.contains(AcceleratorBehavior::Alt);
        let mut shift = behavior.contains(AcceleratorBehavior::Shift);

        // On macOS: Command (⌘) is reported as ctrl, so we map it to M (Meta).
        #[cfg(target_os = "macos")]
        let (cmd, ctrl_key) = (ctrl, false);
        #[cfg(not(target_os = "macos"))]
        let (cmd, ctrl_key) = (false, ctrl);

        #[cfg(target_os = "macos")]
        let key_str = {
            if let Some((base, inferred_shift)) = match key_code {
                44 => Some((",", false)),
                46 => Some((".", false)),
                47 => Some(("/", false)),
                59 => Some((";", false)),
                39 => Some(("'", false)),
                45 => Some(("-", false)),
                61 => Some(("=", false)),
                91 => Some(("[", false)),
                93 => Some(("]", false)),
                92 => Some(("\\", false)),
                96 => Some(("`", false)),
                60 => Some((",", true)),
                62 => Some((".", true)),
                63 => Some(("/", true)),
                58 => Some((";", true)),
                34 => Some(("'", true)),
                95 => Some(("-", true)),
                43 => Some(("=", true)),
                123 => Some(("[", true)),
                125 => Some(("]", true)),
                124 => Some(("\\", true)),
                126 => Some(("`", true)),
                _ => None,
            } {
                if inferred_shift {
                    shift = true;
                }
                base.to_string()
            } else {
                match key_code {
                    16 | 160 | 161 => return "shift".to_string(),
                    17 | 162 | 163 => return "cmd".to_string(),
                    18 | 164 | 165 => return "alt".to_string(),
                    91 => return "ctrl".to_string(),
                    92 => return "ctrl".to_string(),
                    65..=90 => char::from_u32((key_code + 32) as u32)
                        .unwrap_or('?')
                        .to_string(),
                    48..=57 => char::from_u32(key_code as u32).unwrap_or('?').to_string(),
                    8 => "backspace".to_string(),
                    9 => "tab".to_string(),
                    13 => "enter".to_string(),
                    27 => "esc".to_string(),
                    32 => "space".to_string(),
                    0x25 => "left".to_string(),
                    0x26 => "up".to_string(),
                    0x27 => "right".to_string(),
                    0x28 => "down".to_string(),
                    0x70..=0x7B => format!("f{}", key_code - 0x70 + 1),
                    0x21 => "pageup".to_string(),
                    0x22 => "pagedown".to_string(),
                    0x23 => "end".to_string(),
                    0x24 => "home".to_string(),
                    0x2D => "insert".to_string(),
                    0x2E => "delete".to_string(),
                    0xBA => ";".to_string(),
                    0xBB => "=".to_string(),
                    0xBC => ",".to_string(),
                    0xBD => "-".to_string(),
                    0xBE => ".".to_string(),
                    0xBF => "/".to_string(),
                    0xC0 => "`".to_string(),
                    0xDB => "[".to_string(),
                    0xDC => "\\".to_string(),
                    0xDD => "]".to_string(),
                    0xDE => "'".to_string(),
                    _ => format!("key{}", key_code),
                }
            }
        };

        #[cfg(not(target_os = "macos"))]
        let key_str = match key_code {
            16 | 160 | 161 => return "shift".to_string(),
            17 | 162 | 163 => return "ctrl".to_string(),
            18 | 164 | 165 => return "alt".to_string(),
            91 => return "lmeta".to_string(),
            92 => return "rmeta".to_string(),
            65..=90 => char::from_u32((key_code + 32) as u32)
                .unwrap_or('?')
                .to_string(),
            48..=57 => char::from_u32(key_code as u32).unwrap_or('?').to_string(),
            8 => "backspace".to_string(),
            9 => "tab".to_string(),
            13 => "enter".to_string(),
            27 => "esc".to_string(),
            32 => "space".to_string(),
            0x25 => "left".to_string(),
            0x26 => "up".to_string(),
            0x27 => "right".to_string(),
            0x28 => "down".to_string(),
            0x70..=0x7B => format!("f{}", key_code - 0x70 + 1),
            0x21 => "pageup".to_string(),
            0x22 => "pagedown".to_string(),
            0x23 => "end".to_string(),
            0x24 => "home".to_string(),
            0x2D => "insert".to_string(),
            0x2E => "delete".to_string(),
            0xBA => ";".to_string(),
            0xBB => "=".to_string(),
            0xBC => ",".to_string(),
            0xBD => "-".to_string(),
            0xBE => ".".to_string(),
            0xBF => "/".to_string(),
            0xC0 => "`".to_string(),
            0xDB => "[".to_string(),
            0xDC => "\\".to_string(),
            0xDD => "]".to_string(),
            0xDE => "'".to_string(),
            _ => format!("key{}", key_code),
        };

        let mut modifiers = Vec::new();
        if ctrl_key {
            modifiers.push("C");
        }
        if cmd {
            modifiers.push("M");
        }
        if shift {
            modifiers.push("S");
        }
        if alt {
            modifiers.push("A");
        }

        if modifiers.is_empty() {
            key_str
        } else {
            format!("<{}-{}>", modifiers.join("-"), key_str)
        }
    }

    /// Check if text input is currently focused
    fn is_text_focused() -> bool {
        if let Some(window) = Window::focused() {
            let hwnd = window.raw_hwnd();
            let reaper = Reaper::get();
            let medium_reaper = reaper.medium_reaper();
            // SAFETY: We got the HWND from Window::focused(), so it should be valid
            unsafe { medium_reaper.is_window_text_field(hwnd) }
        } else {
            false
        }
    }

    /// Determine context from current focused window
    /// Returns (Context, context_name, window_title)
    /// Made public so wheel_hook can use it
    pub fn determine_context() -> (Context, String, String) {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        window_detection::detect_context_from_focus_compat(&medium_reaper)
    }
}

/// TranslateAccel implementation for intercepting keypresses
impl TranslateAccel for InputHandler {
    fn call(&mut self, args: TranslateAccelArgs) -> TranslateAccelResult {
        // CRITICAL: If interception is disabled, return NotOurWindow IMMEDIATELY
        if !INTERCEPTION_ENABLED.load(std::sync::atomic::Ordering::Relaxed) {
            return TranslateAccelResult::NotOurWindow;
        }

        // Check the raw message type to detect mouse wheel and other events
        let raw_msg = args.msg.raw();
        let raw_message_type = raw_msg.message;
        let msg_type = args.msg.message();

        match msg_type {
            AccelMsgKind::KeyDown
            | AccelMsgKind::KeyUp
            | AccelMsgKind::SysKeyDown
            | AccelMsgKind::SysKeyUp
            | AccelMsgKind::Char => {
                // Normal keyboard events
            }
            _ => {
                let reaper = Reaper::get();
                reaper.show_console_msg(format!(
                    "FTS-Input: Non-keyboard message type: {:?} (raw: 0x{:X} = {})\n",
                    msg_type, raw_message_type, raw_message_type
                ));
            }
        }

        // Detect mouse wheel events
        if raw_message_type == raw::WM_MOUSEWHEEL || raw_message_type == raw::WM_MOUSEHWHEEL {
            return Self::handle_mouse_wheel(args, raw_message_type);
        }

        let msg_type = args.msg.message();

        // Handle key release for continuous actions
        if msg_type == AccelMsgKind::KeyUp || msg_type == AccelMsgKind::SysKeyUp {
            if DEBUG_LOGGING.load(std::sync::atomic::Ordering::Relaxed) {
                let key = args.msg.key();
                let behavior = args.msg.behavior();
                let key_str = Self::key_to_string(key, &behavior);
                let (_context, context_name, _) = Self::determine_context();
                let reaper = Reaper::get();
                reaper.show_console_msg(format!(
                    "[DEBUG] KeyUp: '{}' (raw: {}) in {} | modifiers: ctrl={} shift={} alt={}\n",
                    key_str,
                    key.get(),
                    context_name,
                    behavior.contains(AcceleratorBehavior::Control),
                    behavior.contains(AcceleratorBehavior::Shift),
                    behavior.contains(AcceleratorBehavior::Alt),
                ));
            }
            crate::input::continuous_action::stop_all_continuous_actions();
            if PASSTHROUGH_MODE.load(std::sync::atomic::Ordering::Relaxed)
                || Self::is_text_focused()
            {
                return TranslateAccelResult::NotOurWindow;
            }
            return TranslateAccelResult::Eat;
        }

        // Only process KeyDown and SysKeyDown events
        if msg_type != AccelMsgKind::KeyDown && msg_type != AccelMsgKind::SysKeyDown {
            return TranslateAccelResult::NotOurWindow;
        }

        // If text is focused, always pass through
        if Self::is_text_focused() {
            return TranslateAccelResult::NotOurWindow;
        }

        let key = args.msg.key();
        let behavior = args.msg.behavior();

        // Determine context and update the processor's context
        let (context, context_name, _window_title) = Self::determine_context();
        let keybind_context = Self::context_to_keybind_context(&context);

        // Debug logging
        if DEBUG_LOGGING.load(std::sync::atomic::Ordering::Relaxed) {
            let key_str = Self::key_to_string(key, &behavior);
            let reaper = Reaper::get();
            reaper.show_console_msg(format!(
                "[DEBUG] KeyDown: '{}' (raw: {}) in {} | modifiers: ctrl={} shift={} alt={}\n",
                key_str,
                key.get(),
                context_name,
                behavior.contains(AcceleratorBehavior::Control),
                behavior.contains(AcceleratorBehavior::Shift),
                behavior.contains(AcceleratorBehavior::Alt),
            ));
        }

        // === Cmd+W: Toggle which-key cheat sheet ===
        // Check for Meta+w before sending to processor
        {
            let key_str = Self::key_to_string(key, &behavior);
            if key_str == "<M-w>" {
                if crate::input::which_key_overlay::is_visible() {
                    crate::input::which_key_overlay::hide();
                } else {
                    crate::input::which_key_overlay::show_all_prefixes();
                }
                return TranslateAccelResult::Eat;
            }

            // Esc dismisses the cheat sheet overlay if visible and no sequence is active
            if key_str == "esc"
                && crate::input::which_key_overlay::is_visible()
                && !crate::input::processor::needs_timeout()
            {
                crate::input::which_key_overlay::hide();
                return TranslateAccelResult::Eat;
            }
        }

        // === Process key through InputProcessor ===
        // Update context before processing
        {
            let mut proc = crate::input::processor::get_processor().write().unwrap();
            proc.set_reaper_context(keybind_context);
        }

        let commands = crate::input::processor::process_key(key, &behavior);

        // Handle the commands from the processor
        let mut handled = false;
        for command in &commands {
            match command {
                InputCommand::Action(action_id) => {
                    debug!(action = %action_id, context = %context_name, "Action resolved");
                    if DEBUG_LOGGING.load(std::sync::atomic::Ordering::Relaxed) {
                        Reaper::get().show_console_msg(format!(
                            "[DEBUG] Execute: action '{}' in {}\n",
                            action_id, context_name
                        ));
                    }
                    Self::execute_action(action_id.as_str());
                    handled = true;
                }
                InputCommand::ActionWithArgs { action, args } => {
                    debug!(action = %action, count = ?args.count, context = %context_name, "Action resolved (with args)");
                    if DEBUG_LOGGING.load(std::sync::atomic::Ordering::Relaxed) {
                        Reaper::get().show_console_msg(format!(
                            "[DEBUG] Execute: action '{}' (count={:?}) in {}\n",
                            action, args.count, context_name
                        ));
                    }
                    // Execute the action, potentially repeating for count
                    let count = args.count.unwrap_or(1);
                    for _ in 0..count {
                        Self::execute_action(action.as_str());
                    }
                    handled = true;
                }
                InputCommand::Pending { display: pending_display } => {
                    // Show which-key overlay with continuations from the trie
                    let proc = crate::input::processor::get_processor().read().unwrap();
                    if let Some(trie) = proc.normal_keytrie() {
                        let pending_chords = pending_display_to_chords(pending_display);
                        let continuations =
                            crate::input::keybinds::bridge::trie_continuations_at(
                                trie,
                                &pending_chords,
                            );
                        if !continuations.is_empty() {
                            crate::input::which_key_overlay::show(pending_display, &continuations);
                        }
                    }
                    drop(proc);

                    debug!(pending = %pending_display, "Pending sequence");
                    handled = true;
                }
                InputCommand::Unhandled(_) => {
                    // Hide overlay if visible
                    if crate::input::which_key_overlay::is_visible() {
                        crate::input::which_key_overlay::hide();
                    }
                    // Not handled — fall through
                }
                InputCommand::SwitchMode(mode) => {
                    debug!(mode = %mode, "Mode switch");
                    handled = true;
                }
                InputCommand::PushMode(mode) => {
                    debug!(mode = %mode, "Mode push");
                    handled = true;
                }
                InputCommand::PopMode => {
                    debug!("Mode pop");
                    handled = true;
                }
                InputCommand::InsertText(_) => {
                    // In REAPER context, insert text isn't used
                    handled = true;
                }
            }
        }

        if handled {
            // Hide overlay if action was executed (not pending)
            if !commands.iter().any(|c| matches!(c, InputCommand::Pending { .. })) {
                if crate::input::which_key_overlay::is_visible() {
                    crate::input::which_key_overlay::hide();
                }
            }
            TranslateAccelResult::Eat
        } else if PASSTHROUGH_MODE.load(std::sync::atomic::Ordering::Relaxed) {
            TranslateAccelResult::NotOurWindow
        } else {
            TranslateAccelResult::Eat
        }
    }
}

/// Convert a pending display string back to KeyChords for trie lookup.
///
/// The display uses compact notation like "v", "fe", "C-s" etc.
/// This is a best-effort parse for common cases.
fn pending_display_to_chords(display: &str) -> Vec<input::key::KeyChord> {
    use input::key::{KeyChord, KeyCode, Modifiers};

    let mut chords = Vec::new();

    // The display format from the processor is compact: each chord is
    // rendered as-is without separators. Single characters are one char,
    // modified keys have "C-", "M-", etc. prefixes.
    let mut chars = display.chars().peekable();

    while let Some(c) = chars.next() {
        // Check for modifier prefixes like "C-", "M-", "S-", "A-"
        if matches!(c, 'C' | 'M' | 'S' | 'A') && chars.peek() == Some(&'-') {
            let mut mods = Modifiers::NONE;
            let mut current = c;

            loop {
                match current {
                    'C' => mods.ctrl = true,
                    'M' => mods.meta = true,
                    'S' => mods.shift = true,
                    'A' => mods.alt = true,
                    _ => {}
                }
                chars.next(); // consume '-'

                // Check if next is another modifier
                if let Some(&next_c) = chars.peek() {
                    if matches!(next_c, 'C' | 'M' | 'S' | 'A') {
                        let _after = display.chars().nth(display.len()); // peek further
                        // Simplified: just check if there's a '-' after
                        chars.next();
                        if chars.peek() == Some(&'-') {
                            current = next_c;
                            continue;
                        } else {
                            // This was the key, not a modifier
                            chords.push(KeyChord::new(
                                KeyCode::Character(next_c.to_lowercase().to_string()),
                                mods,
                            ));
                            break;
                        }
                    } else {
                        // Next char is the key
                        let key_char = chars.next().unwrap_or('?');
                        chords.push(KeyChord::new(
                            KeyCode::Character(key_char.to_lowercase().to_string()),
                            mods,
                        ));
                        break;
                    }
                } else {
                    break;
                }
            }
        } else {
            // Simple character key
            chords.push(KeyChord::plain(KeyCode::Character(
                c.to_lowercase().to_string(),
            )));
        }
    }

    chords
}

impl InputHandler {
    /// Handle mouse wheel events
    fn handle_mouse_wheel(args: TranslateAccelArgs, message_type: u32) -> TranslateAccelResult {
        let raw_msg = args.msg.raw();
        let delta = (raw_msg.wParam as i32 >> 16) as i16;
        let is_horizontal = message_type == raw::WM_MOUSEHWHEEL;
        let (_context, context_name, _window_title) = Self::determine_context();

        let direction = if delta > 0 { "up" } else { "down" };
        let wheel_type = if is_horizontal {
            "horizontal wheel"
        } else {
            "wheel"
        };

        let reaper = Reaper::get();
        reaper.show_console_msg(format!(
            "Mouse {} {} in {}\n",
            wheel_type, direction, context_name
        ));

        if PASSTHROUGH_MODE.load(std::sync::atomic::Ordering::Relaxed) {
            TranslateAccelResult::NotOurWindow
        } else {
            TranslateAccelResult::Eat
        }
    }

    /// Check if interception is enabled
    pub fn is_enabled() -> bool {
        INTERCEPTION_ENABLED.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Set interception enabled state
    pub fn set_enabled(enabled: bool) {
        let was_enabled = INTERCEPTION_ENABLED.load(std::sync::atomic::Ordering::Relaxed);
        INTERCEPTION_ENABLED.store(enabled, std::sync::atomic::Ordering::Relaxed);

        if enabled && !was_enabled {
            if !HANDLER_REGISTERED.load(std::sync::atomic::Ordering::Relaxed) {
                if let Err(e) = register_input_handler() {
                    tracing::warn!("Failed to register input handler: {}", e);
                } else {
                    HANDLER_REGISTERED.store(true, std::sync::atomic::Ordering::Relaxed);
                }
            }

            if let Err(e) = crate::input::wheel_hook::install_main_window_hook() {
                tracing::warn!("Failed to install wheel hook: {}", e);
            }
            if let Err(e) = crate::input::wheel_hook::install_arrange_view_hook() {
                tracing::warn!("Failed to install arrange view hook: {}", e);
            }
            crate::input::wheel_hook::check_and_hook_midi_editors();

            info!("FTS-Input interception enabled");
        } else if !enabled && was_enabled {
            info!(
                "FTS-Input interception disabled (handler remains registered but returns NotOurWindow for all keys)"
            );
        }
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
            "FTS-Input passthrough mode {}",
            if enabled { "enabled" } else { "disabled" }
        );
    }

    /// Toggle passthrough mode
    pub fn toggle_passthrough() -> bool {
        let new_state = !Self::is_passthrough();
        Self::set_passthrough(new_state);
        new_state
    }

    /// Check if debug logging is enabled
    pub fn is_debug_logging() -> bool {
        DEBUG_LOGGING.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Set debug logging mode
    pub fn set_debug_logging(enabled: bool) {
        DEBUG_LOGGING.store(enabled, std::sync::atomic::Ordering::Relaxed);
        info!(
            "FTS-Input debug logging {}",
            if enabled { "enabled" } else { "disabled" }
        );
    }

    /// Toggle debug logging mode
    pub fn toggle_debug_logging() -> bool {
        let new_state = !Self::is_debug_logging();
        Self::set_debug_logging(new_state);
        new_state
    }

    /// Convert internal Context to KeybindContext
    pub fn context_to_keybind_context(context: &Context) -> KeybindContext {
        match context {
            Context::Main => KeybindContext::Main,
            Context::Midi => KeybindContext::Midi,
            Context::MidiEventListEditor => KeybindContext::Midi,
            Context::MidiInlineEditor => KeybindContext::MidiInline,
            Context::MediaExplorer => KeybindContext::MediaExplorer,
            Context::CrossfadeEditor => KeybindContext::Main,
            Context::Global => KeybindContext::Global,
        }
    }

    /// Execute an action by its command ID (either numeric or named)
    fn execute_action(action: &str) {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();

        // Try parsing as numeric action ID first
        if let Ok(cmd_id) = action.parse::<u32>() {
            debug!(action = %action, cmd_id = cmd_id, "Executing numeric action");
            unsafe {
                medium_reaper.low().Main_OnCommand(cmd_id as i32, 0);
            }
            return;
        }

        // Try looking up named command
        if let Some(cmd_id) = medium_reaper.named_command_lookup(action) {
            debug!(action = %action, cmd_id = ?cmd_id, "Executing named action");
            unsafe {
                medium_reaper.low().Main_OnCommand(cmd_id.get() as i32, 0);
            }
            return;
        }

        // Also try with underscore prefix (REAPER convention)
        let prefixed = format!("_{}", action);
        if let Some(cmd_id) = medium_reaper.named_command_lookup(prefixed.as_str()) {
            debug!(action = %action, cmd_id = ?cmd_id, "Executing named action (prefixed)");
            unsafe {
                medium_reaper.low().Main_OnCommand(cmd_id.get() as i32, 0);
            }
            return;
        }

        tracing::warn!(action = %action, "Could not find action to execute");
    }
}

/// Register the input handler
/// This should only be called when FTS-input is enabled
pub fn register_input_handler() -> Result<(), Box<dyn std::error::Error>> {
    if HANDLER_REGISTERED.load(std::sync::atomic::Ordering::Relaxed) {
        return Ok(());
    }

    info!("Registering FTS-Input handler");

    // Initialize the input processor with defaults
    crate::input::processor::init();

    // Initialize the mouse modifier manager with default profiles
    super::mouse_modifiers::manager::init();

    let reaper = Reaper::get();
    let handler = Box::new(InputHandler::new());

    reaper
        .medium_session()
        .plugin_register_add_accelerator_register(handler, AcceleratorPosition::Front)?;

    HANDLER_REGISTERED.store(true, std::sync::atomic::Ordering::Relaxed);
    info!("FTS-Input handler registered successfully");

    Ok(())
}
