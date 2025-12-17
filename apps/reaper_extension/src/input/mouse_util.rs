//! Mouse Utility
//!
//! Port of BR_MouseUtil.cpp from SWS for determining mouse position and context.
//! Uses GetMousePosition and WindowFromPoint to accurately detect which window
//! the mouse is over, independent of keyboard focus.

use reaper_high::Reaper;
use reaper_low::raw::{HWND, POINT};
use reaper_low::Swell;
use reaper_medium::Reaper as MediumReaper;
use swell_ui::Window;
use crate::input::state::Context;

/// Check if an HWND is a MIDI editor window by walking up the parent chain
/// Returns (is_midi_editor, midi_editor_hwnd, subview_hwnd)
/// Based on BR_MouseInfo::IsHwndMidiEditor
pub fn is_hwnd_midi_editor(hwnd: HWND, medium_reaper: &MediumReaper) -> Option<(HWND, Option<HWND>)> {
    // Check if this window itself is a MIDI editor
    let mode = unsafe {
        medium_reaper.low().MIDIEditor_GetMode(hwnd)
    };
    
    if mode != -1 {
        // This is a MIDI editor window
        return Some((hwnd, None));
    }
    
    // Walk up the parent chain to find a MIDI editor parent
    let mut current = hwnd;
    while !current.is_null() {
        let parent = if let Some(window) = Window::new(current) {
            window.parent().map(|w| w.raw_hwnd().as_ptr()).unwrap_or(std::ptr::null_mut())
        } else {
            std::ptr::null_mut()
        };
        
        if parent.is_null() {
            break;
        }
        
        // Check if parent is a MIDI editor
        let parent_mode = unsafe {
            medium_reaper.low().MIDIEditor_GetMode(parent)
        };
        
        if parent_mode != -1 {
            // Found MIDI editor parent, current is the subview
            return Some((parent, Some(current)));
        }
        
        current = parent;
    }
    
    None
}

/// Get mouse position in screen coordinates
pub fn get_mouse_position(medium_reaper: &MediumReaper) -> (i32, i32) {
    let low_reaper = medium_reaper.low();
    let mut mouse_x: i32 = 0;
    let mut mouse_y: i32 = 0;
    unsafe {
        low_reaper.GetMousePosition(&mut mouse_x, &mut mouse_y);
    }
    (mouse_x, mouse_y)
}

/// Get the window under the mouse cursor
pub fn get_window_under_mouse(mouse_x: i32, mouse_y: i32) -> HWND {
    let mouse_point = POINT {
        x: mouse_x,
        y: mouse_y,
    };
    unsafe {
        Swell::get().WindowFromPoint(mouse_point)
    }
}

/// Determine context from mouse position (not keyboard focus)
/// This is the main function that should be used for wheel events
pub fn determine_context_from_mouse_position(medium_reaper: &MediumReaper) -> (Context, String, String) {
    // Get mouse position
    let (mouse_x, mouse_y) = get_mouse_position(medium_reaper);
    
    // Find window under mouse
    let mouse_window_hwnd = get_window_under_mouse(mouse_x, mouse_y);
    
    if mouse_window_hwnd.is_null() {
        // Fallback to keyboard focus if we can't determine mouse window
        return crate::input::handler::InputHandler::determine_context();
    }
    
    // Check if it's a MIDI editor
    if let Some((midi_editor_hwnd, _subview)) = is_hwnd_midi_editor(mouse_window_hwnd, medium_reaper) {
        let window = Window::new(mouse_window_hwnd).or_else(|| Window::new(midi_editor_hwnd));
        let window_title = window.as_ref()
            .and_then(|w| w.text().ok())
            .unwrap_or_default();
        
        // Check MIDI editor mode
        let mode = unsafe {
            medium_reaper.low().MIDIEditor_GetMode(midi_editor_hwnd)
        };
        
        match mode {
            0 => (Context::Midi, "MIDI Editor (Piano Roll)".to_string(), window_title),
            1 => (Context::MidiEventListEditor, "MIDI Event List Editor".to_string(), window_title),
            _ => (Context::Midi, "MIDI Editor".to_string(), window_title),
        }
    } else {
        // Not a MIDI editor, use the existing context detection logic
        determine_context_from_hwnd(mouse_window_hwnd, medium_reaper)
    }
}

/// Determine context from a specific HWND
fn determine_context_from_hwnd(hwnd: HWND, medium_reaper: &MediumReaper) -> (Context, String, String) {
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
    
    // Check window title for Media Explorer
    if let Ok(title) = window.text() {
        found_window_title = title.clone();
        let title_lower = title.to_lowercase();
        if title_lower.contains("media explorer") || title_lower.contains("mediaexplorer") {
            return (Context::MediaExplorer, "Media Explorer".to_string(), found_window_title);
        }
        if title_lower.contains("crossfade") && title_lower.contains("editor") {
            return (Context::CrossfadeEditor, "Crossfade Editor".to_string(), found_window_title);
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
                return (Context::MediaExplorer, "Media Explorer".to_string(), found_window_title);
            }
            if title_lower.contains("crossfade") && title_lower.contains("editor") {
                return (Context::CrossfadeEditor, "Crossfade Editor".to_string(), found_window_title);
            }
            // Check for inline editor
            if (title_lower.contains("inline") || title_lower.contains("midi inline")) 
                && (title_lower.contains("midi") || title_lower.contains("editor")) {
                return (Context::MidiInlineEditor, "MIDI Inline Editor".to_string(), found_window_title);
            }
        }
        current = w.parent();
    }
    
    // Default to Main if we can't determine
    (Context::Main, "Main".to_string(), found_window_title)
}
