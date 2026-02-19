use dioxus::prelude::*;

use actions_proto::ids::standalone as standalone_ids;
use input::InputCommand;
use input_dioxus::ACTION_CONTEXT;
use session::session_actions;
use session_ui::Session;

use crate::{COMMAND_PALETTE_OPEN, COMMAND_PALETTE_QUERY, DOCK_MODE};

pub(crate) fn handle_dock_preset_shortcut(e: &KeyboardEvent) -> bool {
    // Dock mode: F5-F9 switch screenset presets (no modifiers)
    if *DOCK_MODE.peek() {
        let mods = e.modifiers();
        if !mods.ctrl() && !mods.alt() && !mods.shift() && !mods.meta() {
            let preset_index = match e.key() {
                Key::F5 => Some(0),
                Key::F6 => Some(1),
                Key::F7 => Some(2),
                Key::F8 => Some(3),
                Key::F9 => Some(4),
                Key::F10 => Some(5),
                _ => None,
            };
            if let Some(idx) = preset_index {
                e.prevent_default();
                // Auto-save departing preset
                let current_layout = dock_dioxus::DOCK_LAYOUT.read().clone();
                let current_index = *dock_dioxus::DOCK_ACTIVE_PRESET_INDEX.read();
                {
                    let mut presets = dock_dioxus::DOCK_PRESETS.write();
                    if let Some(departing) = presets.presets.get_mut(current_index) {
                        departing.layout = current_layout;
                    }
                }
                // Load target preset
                let presets = dock_dioxus::DOCK_PRESETS.read();
                if let Some(preset) = presets.presets.get(idx) {
                    {
                        let mut workspace = dock_dioxus::DOCK_WORKSPACE.write();
                        let main_window = workspace.main_window;
                        if let Some(main) = workspace.windows.get_mut(&main_window) {
                            main.layout = preset.layout.clone();
                        }
                    }
                    *dock_dioxus::DOCK_LAYOUT.write() = preset.layout.clone();
                    *dock_dioxus::DOCK_ACTIVE_PRESET_INDEX.write() = idx;
                }
                return true;
            }
        }
    }
    false
}

pub(crate) fn dispatch_input_commands(commands: Vec<InputCommand>) -> bool {
    let mut handled = false;

    for command in commands {
        match command {
            InputCommand::Unhandled(_) => {}
            InputCommand::Action(action) => {
                dispatch_action(action.as_str());
                handled = true;
            }
            InputCommand::ActionWithArgs { action, .. } => {
                dispatch_action(action.as_str());
                handled = true;
            }
            InputCommand::SwitchMode(mode) | InputCommand::PushMode(mode) => {
                ACTION_CONTEXT.write().set_mode(mode.as_str());
                handled = true;
            }
            InputCommand::PopMode => {
                handled = true;
            }
            InputCommand::Pending { .. } => {
                handled = true;
            }
            InputCommand::InsertText(_) => {
                handled = true;
            }
        }
    }

    handled
}

pub(crate) fn dispatch_action(action_id: &str) {
    match action_id {
        id if id == session_actions::TOGGLE_PLAYBACK.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().toggle_playback().await;
            });
        }
        id if id == session_actions::TOGGLE_SONG_LOOP.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().toggle_song_loop().await;
            });
        }
        id if id == session_actions::SMART_NEXT.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().next_section().await;
            });
        }
        id if id == session_actions::SMART_PREVIOUS.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().previous_section().await;
            });
        }
        id if id == session_actions::NEXT_SONG.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().next_song().await;
            });
        }
        id if id == session_actions::PREVIOUS_SONG.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().previous_song().await;
            });
        }
        id if id == session_actions::NEXT_SECTION.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().next_section().await;
            });
        }
        id if id == session_actions::PREVIOUS_SECTION.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().previous_section().await;
            });
        }
        id if id == standalone_ids::COMMAND_PALETTE.as_str() => {
            *COMMAND_PALETTE_OPEN.write() = true;
            *COMMAND_PALETTE_QUERY.write() = String::new();
        }
        id if id == standalone_ids::TOGGLE_DARK_MODE.as_str() => {
            tracing::info!("Toggle dark mode triggered (not yet implemented)");
        }
        id if id == standalone_ids::OPEN_SETTINGS.as_str() => {
            tracing::info!("Open settings triggered (not yet implemented)");
        }
        _ => {
            tracing::debug!(action_id, "No handler registered for input action");
        }
    };
}
