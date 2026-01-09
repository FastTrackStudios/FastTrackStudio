//! Mouse Event Hook
//!
//! Uses window procedure (WndProc) hooking to intercept mouse wheel and click events.
//! This is necessary because TranslateAccel only handles keyboard accelerators.
//!
//! Handles:
//! - WM_MOUSEWHEEL / WM_MOUSEHWHEEL - Mouse wheel events
//! - WM_LBUTTONDOWN / WM_RBUTTONDOWN / WM_MBUTTONDOWN - Mouse click events
//!
//! Logs mouse context on clicks and when context changes (on wheel events).

use reaper_high::Reaper;
use reaper_low::Swell;
use reaper_low::raw::{
    GWL_WNDPROC, HWND, LPARAM, LRESULT, POINT, UINT, WM_LBUTTONDOWN, WM_MBUTTONDOWN,
    WM_MOUSEHWHEEL, WM_MOUSEWHEEL, WM_RBUTTONDOWN, WPARAM,
};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::mem;
use std::sync::atomic::{AtomicBool, Ordering};
use swell_ui::Window;
use tracing::info;

/// Global state for whether wheel hook is installed
static WHEEL_HOOK_INSTALLED: AtomicBool = AtomicBool::new(false);

/// Storage for original window procedures
thread_local! {
    static ORIGINAL_PROCS: RefCell<HashMap<HWND, unsafe extern "C" fn(HWND, UINT, WPARAM, LPARAM) -> LRESULT>> = RefCell::new(HashMap::new());
    /// Track which windows we've already hooked
    static HOOKED_WINDOWS: RefCell<HashSet<HWND>> = RefCell::new(HashSet::new());
    /// Track previous mouse context to detect changes
    static PREVIOUS_CONTEXT: RefCell<Option<(crate::input::state::Context, String)>> = RefCell::new(None);
}

/// Determine context from a specific HWND (used for mouse position-based detection)
fn determine_context_from_hwnd(
    hwnd: HWND,
    medium_reaper: &reaper_medium::Reaper,
) -> (crate::input::state::Context, String, String) {
    use crate::input::state::Context;

    // Try to create a Window from the HWND
    let window = if let Some(w) = Window::new(hwnd) {
        w
    } else {
        // Fallback to keyboard focus context
        return crate::input::handler::InputHandler::determine_context();
    };

    let mut found_window_title = String::new();

    // Check if this is the main window
    let main_hwnd = medium_reaper.get_main_hwnd();
    if window.raw_hwnd().as_ptr() == main_hwnd.as_ptr() {
        if let Ok(title) = window.text() {
            found_window_title = title.clone();
        }
        return (Context::Main, "Main".to_string(), found_window_title);
    }

    // Check if this window or any of its parents is a MIDI editor window
    if let Some(midi_editor_hwnd) = medium_reaper.midi_editor_get_active() {
        let mut current = Some(window);
        while let Some(w) = current {
            if w.raw_hwnd().as_ptr() == midi_editor_hwnd.as_ptr() {
                if let Ok(title) = w.text() {
                    found_window_title = title.clone();
                }
                // Check MIDI editor mode to distinguish between piano roll and event list
                let mode = unsafe {
                    medium_reaper
                        .low()
                        .MIDIEditor_GetMode(midi_editor_hwnd.as_ptr())
                };
                match mode {
                    0 => {
                        return (
                            Context::Midi,
                            "MIDI Editor (Piano Roll)".to_string(),
                            found_window_title,
                        );
                    }
                    1 => {
                        return (
                            Context::MidiEventListEditor,
                            "MIDI Event List Editor".to_string(),
                            found_window_title,
                        );
                    }
                    _ => return (Context::Midi, "MIDI Editor".to_string(), found_window_title),
                }
            }
            current = w.parent();
        }
    }

    // Also check if the mouse window itself matches the MIDI editor (direct match)
    if let Some(midi_editor_hwnd) = medium_reaper.midi_editor_get_active() {
        if hwnd == midi_editor_hwnd.as_ptr() {
            if let Ok(title) = window.text() {
                found_window_title = title.clone();
            }
            let mode = unsafe {
                medium_reaper
                    .low()
                    .MIDIEditor_GetMode(midi_editor_hwnd.as_ptr())
            };
            match mode {
                0 => {
                    return (
                        Context::Midi,
                        "MIDI Editor (Piano Roll)".to_string(),
                        found_window_title,
                    );
                }
                1 => {
                    return (
                        Context::MidiEventListEditor,
                        "MIDI Event List Editor".to_string(),
                        found_window_title,
                    );
                }
                _ => return (Context::Midi, "MIDI Editor".to_string(), found_window_title),
            }
        }
    }

    // Check window title for Media Explorer
    if let Ok(title) = window.text() {
        found_window_title = title.clone();
        let title_lower = title.to_lowercase();
        if title_lower.contains("media explorer") || title_lower.contains("mediaexplorer") {
            return (
                Context::MediaExplorer,
                "Media Explorer".to_string(),
                found_window_title,
            );
        }
        if title_lower.contains("crossfade") && title_lower.contains("editor") {
            return (
                Context::CrossfadeEditor,
                "Crossfade Editor".to_string(),
                found_window_title,
            );
        }
    }

    // Check parent windows
    let mut current = window.parent();
    while let Some(w) = current {
        if let Ok(title) = w.text() {
            if found_window_title.is_empty() {
                found_window_title = title.clone();
            }
            let title_lower = title.to_lowercase();

            if title_lower.contains("media explorer") || title_lower.contains("mediaexplorer") {
                return (
                    Context::MediaExplorer,
                    "Media Explorer".to_string(),
                    found_window_title,
                );
            }
            if title_lower.contains("crossfade") && title_lower.contains("editor") {
                return (
                    Context::CrossfadeEditor,
                    "Crossfade Editor".to_string(),
                    found_window_title,
                );
            }
            // Check for inline editor
            if (title_lower.contains("inline") || title_lower.contains("midi inline"))
                && (title_lower.contains("midi") || title_lower.contains("editor"))
            {
                return (
                    Context::MidiInlineEditor,
                    "MIDI Inline Editor".to_string(),
                    found_window_title,
                );
            }
        }
        current = w.parent();
    }

    // Default to Main if we can't determine
    (Context::Main, "Main".to_string(), found_window_title)
}

/// Log mouse context (called on clicks and context changes)
fn log_mouse_context(
    context: crate::input::state::Context,
    context_name: &str,
    _window_title: &str,
    event_type: &str,
) {
    let reaper = Reaper::get();

    // Check if context changed
    let context_changed = PREVIOUS_CONTEXT.with(|prev| {
        let mut prev_borrow = prev.borrow_mut();
        let changed = match prev_borrow.as_ref() {
            Some((prev_ctx, prev_name)) => *prev_ctx != context || prev_name != context_name,
            None => true, // First time, always log
        };

        if changed {
            *prev_borrow = Some((context, context_name.to_string()));
        }
        changed
    });

    // Log if context changed or on every click
    if context_changed || event_type == "click" {
        // Don't log here - let the caller log with event details
    }
}

/// Our custom window procedure that intercepts mouse wheel and click events
unsafe extern "C" fn wheel_hook_proc(hwnd: HWND, msg: UINT, w: WPARAM, l: LPARAM) -> LRESULT {
    // Check if interception is enabled
    if !crate::input::handler::InputHandler::is_enabled() {
        // Pass through to original procedure
        return call_original_proc(hwnd, msg, w, l);
    }

    // Handle mouse click events
    match msg {
        WM_LBUTTONDOWN | WM_RBUTTONDOWN | WM_MBUTTONDOWN => {
            // Get mouse position from lParam
            let x = l as i32 & 0xFFFF;
            let y = (l as i32 >> 16) & 0xFFFF;
            let pt = POINT { x, y };

            // Convert to screen coordinates
            let swell = Swell::get();
            let mut pt_screen = pt;
            swell.ClientToScreen(hwnd, &mut pt_screen);

            // Determine context from mouse position
            let reaper = Reaper::get();
            let medium_reaper = reaper.medium_reaper();
            let (context, context_name, _window_title) =
                crate::input::mouse_context::get_context_from_mouse_position(&medium_reaper);

            // Determine button name
            let button = match msg {
                WM_LBUTTONDOWN => "Left",
                WM_RBUTTONDOWN => "Right",
                WM_MBUTTONDOWN => "Middle",
                _ => "Unknown",
            };

            // Log mouse click with context
            log_mouse_context(context, &context_name, &_window_title, "click");
            reaper.show_console_msg(format!("{} click in {}\n", button, context_name));

            // Always pass through mouse clicks to REAPER (don't intercept)
            return call_original_proc(hwnd, msg, w, l);
        }
        _ => {}
    }

    // Handle mouse wheel events
    match msg {
        WM_MOUSEWHEEL | WM_MOUSEHWHEEL => {
            let delta = ((w as u32) >> 16) as i16;
            let is_horizontal = msg == WM_MOUSEHWHEEL;

            // Extract key states from wParam (low 16 bits)
            let key_states = (w as u32) & 0xFFFF;
            let ctrl = (key_states & 0x0008) != 0; // MK_CONTROL
            let shift = (key_states & 0x0004) != 0; // MK_SHIFT
            let alt = (key_states & 0x0020) != 0; // MK_ALT

            // Determine context from mouse position (not keyboard focus)
            // Use the mouse context module which ports BR_MouseInfo::GetContext
            let reaper = Reaper::get();
            let medium_reaper = reaper.medium_reaper();
            let (context, context_name, _window_title) =
                crate::input::mouse_context::get_context_from_mouse_position(&medium_reaper);

            // Log context change on wheel events
            log_mouse_context(context, &context_name, &_window_title, "wheel");

            // Determine wheel direction and type
            let direction = if delta > 0 { "up" } else { "down" };
            let wheel_type = if is_horizontal {
                "horizontal wheel"
            } else {
                "wheel"
            };

            // Log mouse wheel event
            reaper.show_console_msg(format!(
                "Mouse {} {} in {}\n",
                wheel_type, direction, context_name
            ));

            // Check passthrough mode
            if crate::input::handler::InputHandler::is_passthrough() {
                // Passthrough ON: Log but let REAPER handle it
                call_original_proc(hwnd, msg, w, l)
            } else {
                // Passthrough OFF: Eat the message (return 0)
                0
            }
        }
        _ => {
            // Pass all other messages through to original procedure
            call_original_proc(hwnd, msg, w, l)
        }
    }
}

/// Call the original window procedure
/// Uses try_with to avoid RefCell borrow panics if already borrowed
unsafe fn call_original_proc(hwnd: HWND, msg: UINT, w: WPARAM, l: LPARAM) -> LRESULT {
    // Use try_with to avoid panic if RefCell is already borrowed
    // This can happen if the timer callback is running simultaneously
    match ORIGINAL_PROCS.try_with(|orig_map| orig_map.borrow().get(&hwnd).cloned()) {
        Ok(Some(orig_fn)) => orig_fn(hwnd, msg, w, l),
        Ok(None) | Err(_) => {
            // Fallback to default window procedure if we can't get the original
            // or if RefCell is already borrowed
            Swell::get().DefWindowProc(hwnd, msg, w, l)
        }
    }
}

/// Install wheel event hook on a window
pub fn install_wheel_hook(hwnd: HWND) -> Result<(), Box<dyn std::error::Error>> {
    let swell = Swell::get();

    // Check if already hooked (use try_with to avoid panic if already borrowed)
    let already_hooked = match HOOKED_WINDOWS.try_with(|hooked| hooked.borrow().contains(&hwnd)) {
        Ok(true) => return Ok(()),
        Ok(false) => false,
        Err(_) => {
            // If we can't check, assume not hooked and proceed
            // This is safe because we check again before inserting
            false
        }
    };

    // Use try_with to avoid RefCell borrow panic
    match ORIGINAL_PROCS.try_with(|m| {
        let mut map = m.borrow_mut();

        // Double-check we haven't already hooked this window
        if map.contains_key(&hwnd) {
            return Ok(());
        }

        unsafe {
            // Get the original window procedure
            let get_window_long = swell
                .pointers()
                .GetWindowLong
                .ok_or("GetWindowLong not available")?;
            let old_ptr = get_window_long(hwnd, GWL_WNDPROC);

            // Convert to function pointer
            let orig_fn: unsafe extern "C" fn(HWND, UINT, WPARAM, LPARAM) -> LRESULT =
                mem::transmute(old_ptr);

            // Store original procedure
            map.insert(hwnd, orig_fn);

            // Install our hook
            let set_window_long = swell
                .pointers()
                .SetWindowLong
                .ok_or("SetWindowLong not available")?;
            set_window_long(hwnd, GWL_WNDPROC, wheel_hook_proc as isize);
        }

        // Mark as hooked (use try_with to avoid panic)
        let _ = HOOKED_WINDOWS.try_with(|hooked| {
            hooked.borrow_mut().insert(hwnd);
        });

        Ok(())
    }) {
        Ok(result) => result,
        Err(_) => {
            // If RefCell is already borrowed, log and return error
            tracing::warn!("Could not install wheel hook: RefCell already borrowed");
            Err("RefCell already borrowed".into())
        }
    }
}

/// Install wheel hook on the main REAPER window
pub fn install_main_window_hook() -> Result<(), Box<dyn std::error::Error>> {
    if WHEEL_HOOK_INSTALLED.load(Ordering::Relaxed) {
        return Ok(());
    }

    let reaper = Reaper::get();
    let main_hwnd = reaper.medium_reaper().get_main_hwnd();

    // Convert Hwnd to raw HWND pointer
    install_wheel_hook(main_hwnd.as_ptr())?;

    WHEEL_HOOK_INSTALLED.store(true, Ordering::Relaxed);
    info!("Mouse wheel hook installed on main window");

    Ok(())
}

/// Check for and hook MIDI editor windows
/// Call this periodically to ensure all MIDI editor windows are hooked
pub fn check_and_hook_midi_editors() {
    if !crate::input::handler::InputHandler::is_enabled() {
        return;
    }

    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    // Check the active MIDI editor
    if let Some(midi_editor_hwnd) = medium_reaper.midi_editor_get_active() {
        let midi_hwnd = midi_editor_hwnd.as_ptr();

        // Check if we've already hooked this window
        let already_hooked = HOOKED_WINDOWS.with(|hooked| hooked.borrow().contains(&midi_hwnd));

        if !already_hooked {
            if let Err(e) = install_wheel_hook(midi_hwnd) {
                tracing::warn!("Failed to hook MIDI editor window: {}", e);
            } else {
                info!("Hooked MIDI editor window for wheel events");
                reaper.show_console_msg(
                    "🎹 Hooked MIDI editor window for wheel events\n".to_string(),
                );
            }
        }
    }
}

/// Restore all window procedure hooks
pub fn restore_all_hooks() {
    let swell = Swell::get();

    ORIGINAL_PROCS.with(|m| {
        let mut map = m.borrow_mut();

        unsafe {
            if let Some(set_window_long) = swell.pointers().SetWindowLong {
                for (hwnd, orig_fn) in map.drain() {
                    set_window_long(hwnd, GWL_WNDPROC, orig_fn as isize);
                }
            }
        }
    });

    HOOKED_WINDOWS.with(|hooked| {
        hooked.borrow_mut().clear();
    });

    WHEEL_HOOK_INSTALLED.store(false, Ordering::Relaxed);
    info!("All wheel hooks restored");
}
