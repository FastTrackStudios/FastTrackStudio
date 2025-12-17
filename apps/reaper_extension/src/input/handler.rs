//! Input Handler
//!
//! Main handler that processes keypresses and manages the input system.
//! Uses TranslateAccel to intercept keypresses before REAPER processes them.

use reaper_high::Reaper;
use reaper_medium::{
    AccelMsgKind, AcceleratorPosition, TranslateAccel, TranslateAccelArgs, TranslateAccelResult,
    AcceleratorKeyCode, AcceleratorBehavior, Hwnd,
};
use reaper_low::raw;
use swell_ui::Window;
use crate::input::state::Context;
use tracing::info;

/// Global state for whether FTS-Input interception is enabled
static INTERCEPTION_ENABLED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Global state for whether FTS-Input should eat keys or just log them (passthrough mode)
static PASSTHROUGH_MODE: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Global state for whether the handler is currently registered
/// When false, the handler is not registered at all (completely transparent)
static HANDLER_REGISTERED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

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
    fn key_to_string(key: AcceleratorKeyCode, behavior: &enumflags2::BitFlags<AcceleratorBehavior>) -> String {
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
            },
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
    
    /// Check if text input is currently focused
    fn is_text_focused() -> bool {
        if let Some(window) = Window::focused() {
            let hwnd = window.raw_hwnd();
            let reaper = Reaper::get();
            let medium_reaper = reaper.medium_reaper();
            // SAFETY: We got the HWND from Window::focused(), so it should be valid
            unsafe {
                medium_reaper.is_window_text_field(hwnd)
            }
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
        
        // Get the currently focused window
        let mut found_window_title = String::new();
        
        if let Some(focused_window) = Window::focused() {
            let focused_hwnd = focused_window.raw_hwnd();
            
            // Get window title for logging (try focused window first)
            if found_window_title.is_empty() {
                if let Ok(title) = focused_window.text() {
                    found_window_title = title.clone();
                }
            }
            
            // Check if the focused window is the active MIDI editor
            if let Some(midi_editor_hwnd) = medium_reaper.midi_editor_get_active() {
                // Check if the focused window is the MIDI editor or a child of it
                let is_in_midi_editor = if focused_hwnd.as_ptr() == midi_editor_hwnd.as_ptr() {
                    true
                } else {
                    // Check if the focused window is a child of the MIDI editor
                    let mut current = Some(focused_window);
                    let mut found = false;
                    while let Some(window) = current {
                        if window.raw_hwnd().as_ptr() == midi_editor_hwnd.as_ptr() {
                            found = true;
                            // Get title from this window if we don't have one yet
                            if found_window_title.is_empty() {
                                if let Ok(title) = window.text() {
                                    found_window_title = title;
                                }
                            }
                            break;
                        }
                        current = window.parent();
                    }
                    found
                };
                
                if is_in_midi_editor {
                    // Get the MIDI editor mode to determine if it's Event List Editor
                    // Mode: 0 = piano roll, 1 = event list, -1 = invalid
                    let mode = unsafe {
                        medium_reaper.low().MIDIEditor_GetMode(midi_editor_hwnd.as_ptr())
                    };
                    
                    if mode == 1 {
                        // Event List Editor mode
                        return (Context::MidiEventListEditor, "MIDI Event List Editor".to_string(), found_window_title);
                    } else {
                        // Piano roll mode (or other)
                        return (Context::Midi, "MIDI Editor".to_string(), found_window_title);
                    }
                }
            }
            
            // Check if the focused window is the Media Explorer
            // We identify it by checking the window title
            if let Ok(window_title) = focused_window.text() {
                found_window_title = window_title.clone();
                // Media Explorer window title typically contains "Media Explorer" or similar
                let title_lower = window_title.to_lowercase();
                if title_lower.contains("media explorer") || title_lower.contains("mediaexplorer") {
                    return (Context::MediaExplorer, "Media Explorer".to_string(), found_window_title);
                }
            }
            
            // Also check parent windows for Media Explorer
            let mut current = Some(focused_window);
            while let Some(window) = current {
                if let Ok(window_title) = window.text() {
                    if found_window_title.is_empty() {
                        found_window_title = window_title.clone();
                    }
                    let title_lower = window_title.to_lowercase();
                    if title_lower.contains("media explorer") || title_lower.contains("mediaexplorer") {
                        return (Context::MediaExplorer, "Media Explorer".to_string(), found_window_title);
                    }
                }
                current = window.parent();
            }
            
            // Check if the focused window is the MIDI Inline Editor
            // The inline editor is a child window of the arrange view (main window)
            // We need to check if we're in the main window and walk up/down the hierarchy
            // to find the inline editor window
            
            // First, make sure we're NOT in the MIDI editor (inline editor is separate)
            let is_in_midi_editor = if let Some(midi_editor_hwnd) = medium_reaper.midi_editor_get_active() {
                let mut current = Some(focused_window);
                let mut found = false;
                while let Some(window) = current {
                    if window.raw_hwnd().as_ptr() == midi_editor_hwnd.as_ptr() {
                        found = true;
                        break;
                    }
                    current = window.parent();
                }
                found
            } else {
                false
            };
            
            // If we're not in the MIDI editor, check if we might be in the inline editor
            if !is_in_midi_editor {
                // Walk up the parent chain to find the main window
                let main_hwnd = medium_reaper.get_main_hwnd();
                let mut current = Some(focused_window);
                let mut is_in_main_window = false;
                
                while let Some(window) = current {
                    if window.raw_hwnd().as_ptr() == main_hwnd.as_ptr() {
                        is_in_main_window = true;
                        break;
                    }
                    current = window.parent();
                }
                
                // If we're in the main window, check if the focused window or its parents
                // might be the inline editor by checking window titles
                if is_in_main_window {
                    let mut check_window = Some(focused_window);
                    while let Some(window) = check_window {
                        if let Ok(window_title) = window.text() {
                            if found_window_title.is_empty() {
                                found_window_title = window_title.clone();
                            }
                            let title_lower = window_title.to_lowercase();
                            
                            // Check for inline editor indicators in the title
                            if (title_lower.contains("inline") || title_lower.contains("midi inline")) 
                                && (title_lower.contains("midi") || title_lower.contains("editor")) {
                                return (Context::MidiInlineEditor, "MIDI Inline Editor".to_string(), found_window_title);
                            }
                            
                            // Also check child windows of this window for inline editor
                            for child in window.children() {
                                if let Ok(child_title) = child.text() {
                                    let child_title_lower = child_title.to_lowercase();
                                    if (child_title_lower.contains("inline") || child_title_lower.contains("midi inline")) 
                                        && (child_title_lower.contains("midi") || child_title_lower.contains("editor")) {
                                        return (Context::MidiInlineEditor, "MIDI Inline Editor".to_string(), found_window_title);
                                    }
                                }
                            }
                        }
                        check_window = window.parent();
                    }
                }
            }
            
            // Check if the focused window is the Crossfade Editor
            // We identify it by checking the window title (no API available)
            if let Ok(window_title) = focused_window.text() {
                if found_window_title.is_empty() {
                    found_window_title = window_title.clone();
                }
                let title_lower = window_title.to_lowercase();
                if title_lower.contains("crossfade") && title_lower.contains("editor") {
                    return (Context::CrossfadeEditor, "Crossfade Editor".to_string(), found_window_title);
                }
            }
            
            // Also check parent windows for editors (excluding MIDI Event List, which is handled above)
            let mut current = Some(focused_window);
            while let Some(window) = current {
                if let Ok(window_title) = window.text() {
                    if found_window_title.is_empty() {
                        found_window_title = window_title.clone();
                    }
                    let title_lower = window_title.to_lowercase();
                    
                    // Check for MIDI Inline Editor
                    if (title_lower.contains("inline") || title_lower.contains("midi inline")) 
                        && (title_lower.contains("midi") || title_lower.contains("editor")) {
                        return (Context::MidiInlineEditor, "MIDI Inline Editor".to_string(), found_window_title);
                    }
                    
                    // Check for Crossfade Editor
                    if title_lower.contains("crossfade") && title_lower.contains("editor") {
                        return (Context::CrossfadeEditor, "Crossfade Editor".to_string(), found_window_title);
                    }
                }
                current = window.parent();
            }
        }
        
        // Default to main window context
        (Context::Main, "Main Window".to_string(), found_window_title)
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
            AccelMsgKind::KeyDown | AccelMsgKind::KeyUp | AccelMsgKind::SysKeyDown | AccelMsgKind::SysKeyUp | AccelMsgKind::Char => {
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
        
        // Determine context - this now also returns the window title it found
        let (context, context_name, window_title) = Self::determine_context();
        
        // Convert key to string for logging
        let key_str = Self::key_to_string(key, &behavior);
        
        // Log ALL keypresses to REAPER console for testing (only when interception is enabled)
        let reaper = Reaper::get();
        reaper.show_console_msg(format!(
            "FTS-Input: Key '{}' pressed in {} (Context: {:?}, Window: '{}')\n",
            key_str, context_name, context, window_title
        ));
        
        // Determine what to do with the key:
        // - If passthrough mode is ON: log and pass through to REAPER (NotOurWindow)
        // - If passthrough mode is OFF: log and intercept the key (Eat)
        if PASSTHROUGH_MODE.load(std::sync::atomic::Ordering::Relaxed) {
            // Passthrough ON: Log but let REAPER handle the key
            TranslateAccelResult::NotOurWindow
        } else {
            // Passthrough OFF: Log and intercept the key (prevent REAPER from processing it)
            // This allows us to intercept keys for custom handling
            // TODO: Eventually check if we have key bindings configured for this key
            // If yes, process the binding and Eat the key
            // If no, we might want to still Eat it or pass it through based on configuration
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
        let ctrl = (key_states & 0x0008) != 0;  // MK_CONTROL
        let shift = (key_states & 0x0004) != 0; // MK_SHIFT
        let alt = (key_states & 0x0020) != 0;   // MK_ALT (if applicable)
        
        // Determine wheel direction
        let is_horizontal = message_type == raw::WM_MOUSEHWHEEL;
        let direction = if delta > 0 { "up/right" } else { "down/left" };
        
        // Determine context
        let (context, context_name, window_title) = Self::determine_context();
        
        // Build modifier string
        let mut modifiers = Vec::new();
        if ctrl { modifiers.push("Ctrl"); }
        if shift { modifiers.push("Shift"); }
        if alt { modifiers.push("Alt"); }
        let modifier_str = if modifiers.is_empty() {
            String::new()
        } else {
            format!("+{}", modifiers.join("+"))
        };
        
        // Log mouse wheel event
        let reaper = Reaper::get();
        reaper.show_console_msg(format!(
            "FTS-Input: {} wheel {} (delta: {}){} in {} (Context: {:?}, Window: '{}')\n",
            if is_horizontal { "Horizontal" } else { "Vertical" },
            direction,
            delta,
            modifier_str,
            context_name,
            context,
            window_title
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
            
            // Check for and hook MIDI editor windows
            crate::input::wheel_hook::check_and_hook_midi_editors();
            
            info!("FTS-Input interception enabled");
        } else if !enabled && was_enabled {
            // Disabling: mark as disabled
            // Note: We keep the handler registered (can't unregister)
            // But it will return NotOurWindow for all keys when INTERCEPTION_ENABLED is false
            // This makes it completely transparent - REAPER will process keys normally
            info!("FTS-Input interception disabled (handler remains registered but returns NotOurWindow for all keys)");
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
        info!("FTS-Input passthrough mode {}", if enabled { "enabled" } else { "disabled" });
    }
    
    /// Toggle passthrough mode
    pub fn toggle_passthrough() -> bool {
        let new_state = !Self::is_passthrough();
        Self::set_passthrough(new_state);
        new_state
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
    
    let reaper = Reaper::get();
    let handler = Box::new(InputHandler::new());
    
    // Register with accelerator register at the FRONT (before REAPER processes)
    // This matches the pattern used by the commands handler
    // When disabled, we pass through immediately so REAPER processes normally
    reaper.medium_session().plugin_register_add_accelerator_register(
        handler,
        AcceleratorPosition::Front,
    )?;
    
    HANDLER_REGISTERED.store(true, std::sync::atomic::Ordering::Relaxed);
    info!("FTS-Input handler registered successfully");
    
    Ok(())
}
