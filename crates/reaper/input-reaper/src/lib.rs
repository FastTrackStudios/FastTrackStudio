//! REAPER-backed input runtime used by `reaper-extension`.
//!
//! This wraps the reference `input/*` implementation and keeps a stable API
//! for extension local actions.

// This crate is an FFI bridge to REAPER's C API — raw pointers are pervasive
// and making every function `unsafe` would be impractical. The pointers come
// from REAPER callbacks and are valid for the duration of each call.
#![allow(clippy::not_unsafe_ptr_arg_deref)]

pub mod infrastructure;
pub mod input;

use input::handler::InputHandler;
use once_cell::sync::Lazy;
use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};

static INITIALIZED: AtomicBool = AtomicBool::new(false);
static CURRENT_PROFILE: Lazy<Mutex<InputProfile>> =
    Lazy::new(|| Mutex::new(InputProfile::FastTrackStudio));

#[derive(Debug, Clone)]
pub struct InputRuntimeConfig {
    pub eat_handled_keys: bool,
    pub context_tags: Vec<String>,
    pub context_vars: Vec<(String, String)>,
}

impl Default for InputRuntimeConfig {
    fn default() -> Self {
        Self {
            eat_handled_keys: true,
            context_tags: Vec::new(),
            context_vars: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputProfile {
    FastTrackStudio,
    Logic,
    ProTools,
}

impl InputProfile {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::FastTrackStudio => "FastTrackStudio",
            Self::Logic => "Logic",
            Self::ProTools => "Pro Tools",
        }
    }

    fn preset_name(self) -> &'static str {
        match self {
            Self::FastTrackStudio => "fastrackstudio",
            Self::Logic => "logic",
            // Pro Tools profile is not fully ported yet; use Reaper defaults as fallback.
            Self::ProTools => "reaper",
        }
    }

    fn mouse_profile_name(self) -> &'static str {
        match self {
            Self::FastTrackStudio => "fastrackstudio",
            Self::Logic => "logic",
            // Pro Tools profile is not fully ported yet; use Reaper defaults as fallback.
            Self::ProTools => "reaper",
        }
    }
}

pub fn register_with_default_keymap(config: InputRuntimeConfig) -> Result<(), String> {
    register_with_config(config)
}

pub fn register_with_config(_config: InputRuntimeConfig) -> Result<(), String> {
    if !INITIALIZED.swap(true, Ordering::Relaxed) {
        input::keybinds::init();
        input::mouse_modifiers::manager::init();
        input::workflows::init();

        input::handler::register_input_handler().map_err(|e| e.to_string())?;
    }

    InputHandler::set_debug_logging(false);
    set_profile(current_profile())?;
    set_enabled(false);

    Ok(())
}

pub fn check_and_hook_windows() {
    if !is_enabled() {
        return;
    }

    input::wheel_hook::check_and_hook_arrange_view();
    input::wheel_hook::check_and_hook_midi_editors();
}

pub fn is_enabled() -> bool {
    InputHandler::is_enabled()
}

pub fn set_enabled(enabled: bool) {
    InputHandler::set_enabled(enabled);
}

pub fn toggle_enabled() -> bool {
    InputHandler::toggle()
}

pub fn is_intercepting() -> bool {
    !InputHandler::is_passthrough()
}

pub fn set_intercepting(enabled: bool) {
    InputHandler::set_passthrough(!enabled);
}

pub fn toggle_intercepting() -> bool {
    let enabled = !is_intercepting();
    set_intercepting(enabled);
    enabled
}

pub fn is_debug_logging() -> bool {
    InputHandler::is_debug_logging()
}

pub fn set_debug_logging(enabled: bool) {
    InputHandler::set_debug_logging(enabled);
}

pub fn toggle_debug_logging() -> bool {
    InputHandler::toggle_debug_logging()
}

pub fn current_profile() -> InputProfile {
    CURRENT_PROFILE
        .lock()
        .map(|profile| *profile)
        .unwrap_or(InputProfile::FastTrackStudio)
}

pub fn set_profile(profile: InputProfile) -> Result<(), String> {
    let preset_ok = input::keybinds::set_preset(profile.preset_name());
    let mouse_ok = input::mouse_modifiers::manager::set_profile(profile.mouse_profile_name());

    if !preset_ok {
        return Err(format!(
            "Failed to activate keybind preset '{}'",
            profile.preset_name()
        ));
    }
    if !mouse_ok {
        return Err(format!(
            "Failed to activate mouse profile '{}'",
            profile.mouse_profile_name()
        ));
    }

    if let Ok(mut state) = CURRENT_PROFILE.lock() {
        *state = profile;
    }

    input::mouse_modifiers::manager::log_state();
    Ok(())
}

/// Check for which-key sequence timeout.
///
/// Called from the REAPER timer callback (~30fps). If a which-key sequence
/// has been idle for longer than `timeout_ms`, it resets and logs a timeout
/// message to the console, and hides the overlay.
pub fn check_which_key_timeout() {
    if input::processor::needs_timeout() {
        let commands = input::processor::timeout_expired();
        if commands.is_empty() {
            // Timeout expired with no match — hide the overlay
            input::which_key_overlay::hide();
        }
    }
}

/// Refresh the which-key overlay position/render.
///
/// Called from the timer callback so the overlay tracks the arrange view
/// if the user resizes or moves the window.
pub fn refresh_which_key_overlay() {
    input::which_key_overlay::refresh();
}

pub fn log_state_to_console() {
    let reaper = reaper_high::Reaper::get();
    reaper.show_console_msg(format!(
        "FTS Input: enabled={} intercept={} debug={} profile={} preset={} workflow={:?}\n",
        is_enabled(),
        is_intercepting(),
        is_debug_logging(),
        current_profile().as_str(),
        input::keybinds::active_preset_name(),
        input::workflows::active_workflow_name(),
    ));
}
