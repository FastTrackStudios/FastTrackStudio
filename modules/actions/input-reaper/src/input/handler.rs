//! Input Handler
//!
//! Main handler that processes keypresses and manages the input system.
//! Uses TranslateAccel to intercept keypresses before REAPER processes them.

use crate::input::keybinds::{self, KeybindContext};
use crate::input::state::Context;
use crate::input::window_detection;
use reaper_high::Reaper;
use reaper_low::raw;
use reaper_medium::{
    AccelMsgKind, AcceleratorBehavior, AcceleratorKeyCode, AcceleratorPosition, Hwnd,
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

    /// Convert a key code and modifiers to a key string representation
    fn key_to_string(
        key: AcceleratorKeyCode,
        behavior: &enumflags2::BitFlags<AcceleratorBehavior>,
    ) -> String {
        let key_code = key.get();

        // Check modifiers
        let ctrl = behavior.contains(AcceleratorBehavior::Control);
        let alt = behavior.contains(AcceleratorBehavior::Alt);
        let mut shift = behavior.contains(AcceleratorBehavior::Shift);

        // Build modifier string (order: C, S, A, M to match parser convention)
        // On macOS: Command (⌘) is reported as ctrl, so we map it to M (Meta).
        #[cfg(target_os = "macos")]
        let (cmd, ctrl_key) = (ctrl, false);
        #[cfg(not(target_os = "macos"))]
        let (cmd, ctrl_key) = (false, ctrl);

        // Convert key code to string representation
        #[cfg(target_os = "macos")]
        let key_str = {
            // SWELL can emit ASCII codes for punctuation in modified chords.
            // Normalize shifted ASCII symbols to their base key and infer shift.
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
                    // Modifier keys (return early, don't apply modifier prefixes)
                    16 | 160 | 161 => return "shift".to_string(), // VK_SHIFT, VK_LSHIFT, VK_RSHIFT
                    17 | 162 | 163 => return "cmd".to_string(), // On macOS, VK_CONTROL = Command
                    18 | 164 | 165 => return "alt".to_string(), // VK_MENU (Alt/Option), VK_LMENU, VK_RMENU
                    91 => return "ctrl".to_string(),             // On macOS, VK_LWIN = Control
                    92 => return "ctrl".to_string(),             // On macOS, VK_RWIN = Control
                    // Letters (A-Z)
                    65..=90 => char::from_u32((key_code + 32) as u32).unwrap_or('?').to_string(),
                    // Numbers (0-9)
                    48..=57 => char::from_u32(key_code as u32).unwrap_or('?').to_string(),
                    // Special keys
                    8 => "backspace".to_string(),
                    9 => "tab".to_string(),
                    13 => "enter".to_string(),
                    27 => "esc".to_string(),
                    32 => "space".to_string(),
                    // Arrow keys (virtual key codes)
                    0x25 => "left".to_string(),
                    0x26 => "up".to_string(),
                    0x27 => "right".to_string(),
                    0x28 => "down".to_string(),
                    // Function keys
                    0x70..=0x7B => format!("f{}", key_code - 0x70 + 1),
                    // Other special keys
                    0x21 => "pageup".to_string(),
                    0x22 => "pagedown".to_string(),
                    0x23 => "end".to_string(),
                    0x24 => "home".to_string(),
                    0x2D => "insert".to_string(),
                    0x2E => "delete".to_string(), // forward delete
                    // OEM keys
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
            // Modifier keys (return early, don't apply modifier prefixes)
            16 | 160 | 161 => return "shift".to_string(), // VK_SHIFT, VK_LSHIFT, VK_RSHIFT
            17 | 162 | 163 => return "ctrl".to_string(), // VK_CONTROL, VK_LCONTROL, VK_RCONTROL
            18 | 164 | 165 => return "alt".to_string(), // VK_MENU (Alt/Option), VK_LMENU, VK_RMENU
            91 => return "lmeta".to_string(), // VK_LWIN (Left Win)
            92 => return "rmeta".to_string(), // VK_RWIN (Right Win)
            // Letters (A-Z)
            65..=90 => {
                // Always use lowercase for the key name
                char::from_u32((key_code + 32) as u32)
                    .unwrap_or('?')
                    .to_string()
            }
            // Numbers (0-9)
            48..=57 => char::from_u32(key_code as u32).unwrap_or('?').to_string(),
            // Special keys
            8 => "backspace".to_string(),
            9 => "tab".to_string(),
            13 => "enter".to_string(),
            27 => "esc".to_string(),
            32 => "space".to_string(),
            // Arrow keys (virtual key codes)
            0x25 => "left".to_string(),  // VK_LEFT
            0x26 => "up".to_string(),    // VK_UP
            0x27 => "right".to_string(), // VK_RIGHT
            0x28 => "down".to_string(),  // VK_DOWN
            // Function keys
            0x70..=0x7B => format!("f{}", key_code - 0x70 + 1), // F1-F12
            // Other special keys
            0x21 => "pageup".to_string(),   // VK_PRIOR
            0x22 => "pagedown".to_string(), // VK_NEXT
            0x23 => "end".to_string(),      // VK_END
            0x24 => "home".to_string(),     // VK_HOME
            0x2D => "insert".to_string(),   // VK_INSERT
            0x2E => "delete".to_string(),   // VK_DELETE
            // OEM keys (these vary by keyboard layout, using US layout)
            0xBA => ";".to_string(),  // VK_OEM_1 (;:)
            0xBB => "=".to_string(),  // VK_OEM_PLUS (=+)
            0xBC => ",".to_string(),  // VK_OEM_COMMA
            0xBD => "-".to_string(),  // VK_OEM_MINUS
            0xBE => ".".to_string(),  // VK_OEM_PERIOD
            0xBF => "/".to_string(),  // VK_OEM_2 (/?)
            0xC0 => "`".to_string(),  // VK_OEM_3 (`~)
            0xDB => "[".to_string(),  // VK_OEM_4 ([{)
            0xDC => "\\".to_string(), // VK_OEM_5 (\|)
            0xDD => "]".to_string(),  // VK_OEM_6 (]})
            0xDE => "'".to_string(),  // VK_OEM_7 ('")
            _ => format!("key{}", key_code),
        };

        let mut modifiers = Vec::new();
        if ctrl_key {
            modifiers.push("C"); // Control key (not Command on macOS)
        }
        if cmd {
            modifiers.push("M"); // Command/Meta key
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
            // Format as <C-S-A-key> style
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
    ///
    /// This is now a thin wrapper around window_detection::detect_context_from_focus
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
        // NotOurWindow tells REAPER "this handler doesn't care about this key, continue processing normally"
        // This is different from PassOnToWindow - NotOurWindow is more transparent
        if !INTERCEPTION_ENABLED.load(std::sync::atomic::Ordering::Relaxed) {
            return TranslateAccelResult::NotOurWindow;
        }

        // Check the raw message type to detect mouse wheel and other events
        let raw_msg = args.msg.raw();
        let raw_message_type = raw_msg.message;
        let msg_type = args.msg.message();

        // DEBUG: Log all non-keyboard message types to see what we're receiving
        // This will help us understand if mouse wheel events come through TranslateAccel
        // WM_MOUSEWHEEL = 0x020A = 522, WM_MOUSEHWHEEL = 0x020E = 526
        match msg_type {
            AccelMsgKind::KeyDown
            | AccelMsgKind::KeyUp
            | AccelMsgKind::SysKeyDown
            | AccelMsgKind::SysKeyUp
            | AccelMsgKind::Char => {
                // Normal keyboard events - don't log
            }
            _ => {
                // Log unknown or non-keyboard message types
                let reaper = Reaper::get();
                reaper.show_console_msg(format!(
                    "FTS-Input: Non-keyboard message type: {:?} (raw: 0x{:X} = {})\n",
                    msg_type, raw_message_type, raw_message_type
                ));
            }
        }

        // Detect mouse wheel events (WM_MOUSEWHEEL = 0x020A = 522, WM_MOUSEHWHEEL = 0x020E = 526)
        if raw_message_type == raw::WM_MOUSEWHEEL || raw_message_type == raw::WM_MOUSEHWHEEL {
            return Self::handle_mouse_wheel(args, raw_message_type);
        }

        // Handle keyboard events
        let msg_type = args.msg.message();

        // Handle key release for continuous actions
        // This must happen even when interception is disabled to properly stop continuous actions
        if msg_type == AccelMsgKind::KeyUp || msg_type == AccelMsgKind::SysKeyUp {
            // Debug log key releases
            if DEBUG_LOGGING.load(std::sync::atomic::Ordering::Relaxed) {
                let key = args.msg.key();
                let behavior = args.msg.behavior();
                let key_str = Self::key_to_string(key, &behavior);
                let (context, context_name, _) = Self::determine_context();
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
            // Stop any running continuous action on key release
            crate::input::continuous_action::stop_all_continuous_actions();
            if PASSTHROUGH_MODE.load(std::sync::atomic::Ordering::Relaxed)
                || Self::is_text_focused()
            {
                return TranslateAccelResult::NotOurWindow;
            }
            return TranslateAccelResult::Eat;
        }

        // Only process KeyDown and SysKeyDown events for keyboard
        if msg_type != AccelMsgKind::KeyDown && msg_type != AccelMsgKind::SysKeyDown {
            // Unknown message type - might be other events we don't handle yet
            return TranslateAccelResult::NotOurWindow;
        }

        // If text is focused, always pass through immediately - don't interfere with text input
        if Self::is_text_focused() {
            return TranslateAccelResult::NotOurWindow;
        }

        let key = args.msg.key();
        let behavior = args.msg.behavior();

        // Convert key to string for logging
        let key_str = Self::key_to_string(key, &behavior);

        // Check for 'r' key to log arrange view right drag default action
        if key_str == "r" || key_str == "R" {
            if INTERCEPTION_ENABLED.load(std::sync::atomic::Ordering::Relaxed) {
                let reaper = Reaper::get();
                let medium_reaper = reaper.medium_reaper();

                // Get the arrange view right drag default action (modifier flag 0 = no modifiers)
                use crate::input::mouse_modifiers::behaviors::get_mouse_modifier_name;
                use crate::input::mouse_modifiers::core::{MouseModifierFlag, get_mouse_modifier};
                use crate::input::mouse_modifiers::types::{
                    MouseButtonInput, MouseModifierContext,
                };

                let context_str = "MM_CTX_ARRANGE_RMOUSE";
                let flag = MouseModifierFlag::new(false, false, false, false); // Default action (no modifiers)

                if let Some(action_str) = get_mouse_modifier(context_str, flag, &medium_reaper) {
                    // Parse the action
                    use crate::input::mouse_modifiers::actions::MouseModifierAction as ParsedAction;
                    if let Ok(parsed_action) = ParsedAction::parse(&action_str) {
                        // Get the context enum
                        let context_enum = MouseModifierContext::ArrangeView(
                            crate::input::mouse_modifiers::types::ArrangeViewInteraction::Right(
                                MouseButtonInput::RightDrag,
                            ),
                        );

                        // Get the display name
                        let display_name = get_mouse_modifier_name(
                            &context_enum,
                            MouseButtonInput::RightDrag,
                            parsed_action.command_id,
                        );

                        reaper.show_console_msg(format!(
                            "Arrange View -> Right Drag (Default): {} ({})\n",
                            display_name, action_str
                        ));
                    } else {
                        reaper.show_console_msg(format!(
                            "Arrange View -> Right Drag (Default): {} (could not parse)\n",
                            action_str
                        ));
                    }
                } else {
                    reaper.show_console_msg(
                        "Arrange View -> Right Drag (Default): No action assigned\n",
                    );
                }

                // Eat the 'r' key so REAPER doesn't process it
                return TranslateAccelResult::Eat;
            }
        }

        // Check for 'a' key to log mouse cursor context (BR_GetMouseCursorContext equivalent)
        if key_str == "a" || key_str == "A" {
            let reaper = Reaper::get();
            let medium_reaper = reaper.medium_reaper();
            let (window, segment, details) =
                crate::input::mouse_context::get_mouse_cursor_context(&medium_reaper);

            // Also get the full context for display
            let (context, context_name, window_title) = Self::determine_context();

            reaper.show_console_msg(format!(
                "FTS-Input: Mouse Cursor Context:\n  Window: '{}'\n  Segment: '{}'\n  Details: '{}'\n  Context: {:?} ({})\n  Window Title: '{}'\n",
                window, segment, details, context, context_name, window_title
            ));

            // Eat the 'a' key so REAPER doesn't process it
            return TranslateAccelResult::Eat;
        }

        // Check for 'm' key to log all mouse modifiers
        if key_str == "m" || key_str == "M" {
            let reaper = Reaper::get();
            let medium_reaper = reaper.medium_reaper();
            crate::input::mouse_modifiers::preset::log_all_modifiers(&medium_reaper);

            // Eat the 'm' key so REAPER doesn't process it
            return TranslateAccelResult::Eat;
        }

        // Determine context - this now also returns the window title it found
        let (context, context_name, _window_title) = Self::determine_context();

        // Debug logging for key down events
        if DEBUG_LOGGING.load(std::sync::atomic::Ordering::Relaxed) {
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

        // Try to resolve the key to an action via the keybind system
        if let Some(action) = Self::try_resolve_keybind(&key_str, &context) {
            debug!(key = %key_str, action = %action, context = %context_name, "Keybind resolved");
            if DEBUG_LOGGING.load(std::sync::atomic::Ordering::Relaxed) {
                Reaper::get().show_console_msg(format!(
                    "[DEBUG] Execute: key '{}' -> action '{}' in {}\n",
                    key_str, action, context_name
                ));
            }
            Self::execute_action(&action);
            return TranslateAccelResult::Eat;
        }

        // No keybind found:
        // - passthrough ON -> allow REAPER/native handling
        // - passthrough OFF (intercept ON) -> consume event
        if PASSTHROUGH_MODE.load(std::sync::atomic::Ordering::Relaxed) {
            TranslateAccelResult::NotOurWindow
        } else {
            TranslateAccelResult::Eat
        }
    }
}

impl InputHandler {
    /// Handle mouse wheel events
    /// WM_MOUSEWHEEL: wParam high word = delta (positive = forward, negative = backward)
    ///                low word = key states
    ///                lParam = point coordinates
    /// WM_MOUSEHWHEEL: same structure but for horizontal wheel
    fn handle_mouse_wheel(args: TranslateAccelArgs, message_type: u32) -> TranslateAccelResult {
        let raw_msg = args.msg.raw();

        // Extract wheel delta from wParam (high 16 bits)
        // Delta is typically 120 or multiples (WHEEL_DELTA = 120)
        let delta = (raw_msg.wParam as i32 >> 16) as i16;

        // Extract key states from wParam (low 16 bits)
        let key_states = (raw_msg.wParam as u32) & 0xFFFF;
        let ctrl = (key_states & 0x0008) != 0; // MK_CONTROL
        let shift = (key_states & 0x0004) != 0; // MK_SHIFT
        let alt = (key_states & 0x0020) != 0; // MK_ALT (if applicable)

        // Determine wheel direction
        let is_horizontal = message_type == raw::WM_MOUSEHWHEEL;
        let direction = if delta > 0 { "up/right" } else { "down/left" };

        // Determine context
        let (context, context_name, _window_title) = Self::determine_context();

        // Determine wheel direction and type
        let direction = if delta > 0 { "up" } else { "down" };
        let wheel_type = if is_horizontal {
            "horizontal wheel"
        } else {
            "wheel"
        };

        // Log mouse wheel event
        let reaper = Reaper::get();
        reaper.show_console_msg(format!(
            "Mouse {} {} in {}\n",
            wheel_type, direction, context_name
        ));

        // Determine what to do with the wheel event:
        // - If passthrough mode is ON: log and pass through to REAPER (NotOurWindow)
        // - If passthrough mode is OFF: log and intercept the event (Eat)
        if PASSTHROUGH_MODE.load(std::sync::atomic::Ordering::Relaxed) {
            TranslateAccelResult::NotOurWindow
        } else {
            // TODO: Check if we have wheel bindings configured
            // For now, intercept when passthrough is off
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

        // Register handler when enabling (REAPER doesn't support unregistering, so we register once)
        if enabled && !was_enabled {
            // Enabling: register the handler if not already registered
            if !HANDLER_REGISTERED.load(std::sync::atomic::Ordering::Relaxed) {
                if let Err(e) = register_input_handler() {
                    tracing::warn!("Failed to register input handler: {}", e);
                } else {
                    HANDLER_REGISTERED.store(true, std::sync::atomic::Ordering::Relaxed);
                }
            }

            // Also install wheel hook for mouse wheel events
            if let Err(e) = crate::input::wheel_hook::install_main_window_hook() {
                tracing::warn!("Failed to install wheel hook: {}", e);
            }

            // Install hook on arrange view window (critical for click interception)
            if let Err(e) = crate::input::wheel_hook::install_arrange_view_hook() {
                tracing::warn!("Failed to install arrange view hook: {}", e);
            }

            // Check for and hook MIDI editor windows
            crate::input::wheel_hook::check_and_hook_midi_editors();

            info!("FTS-Input interception enabled");
        } else if !enabled && was_enabled {
            // Disabling: mark as disabled
            // Note: We keep the handler registered (can't unregister)
            // But it will return NotOurWindow for all keys when INTERCEPTION_ENABLED is false
            // This makes it completely transparent - REAPER will process keys normally
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
        let reaper = Reaper::get();
        if enabled {
            reaper.show_console_msg(
                "FTS-Input: Debug logging ENABLED - all key events will be logged\n",
            );
        } else {
            reaper.show_console_msg("FTS-Input: Debug logging DISABLED\n");
        }
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

    /// Convert internal Context to KeybindContext for resolver lookup
    pub fn context_to_keybind_context(context: &Context) -> KeybindContext {
        match context {
            Context::Main => KeybindContext::Main,
            Context::Midi => KeybindContext::Midi,
            Context::MidiEventListEditor => KeybindContext::Midi, // Treat as MIDI
            Context::MidiInlineEditor => KeybindContext::MidiInline,
            Context::MediaExplorer => KeybindContext::MediaExplorer,
            Context::CrossfadeEditor => KeybindContext::Main, // Treat as Main
            Context::Global => KeybindContext::Global,
        }
    }

    /// Try to resolve a key to an action using the keybind system
    /// Returns Some(action) if a binding was found and executed, None otherwise
    fn try_resolve_keybind(key_str: &str, context: &Context) -> Option<String> {
        let keybind_context = Self::context_to_keybind_context(context);
        keybinds::resolve(keybind_context, key_str)
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
    // Check if already registered
    if HANDLER_REGISTERED.load(std::sync::atomic::Ordering::Relaxed) {
        return Ok(());
    }

    info!("Registering FTS-Input handler");

    // Initialize the keybind system with defaults
    keybinds::init();

    // Initialize the mouse modifier manager with default profiles
    super::mouse_modifiers::manager::init();

    let reaper = Reaper::get();
    let handler = Box::new(InputHandler::new());

    // Register with accelerator register at the FRONT (before REAPER processes)
    // This matches the pattern used by the commands handler
    // When disabled, we pass through immediately so REAPER processes normally
    reaper
        .medium_session()
        .plugin_register_add_accelerator_register(handler, AcceleratorPosition::Front)?;

    HANDLER_REGISTERED.store(true, std::sync::atomic::Ordering::Relaxed);
    info!("FTS-Input handler registered successfully");

    Ok(())
}
