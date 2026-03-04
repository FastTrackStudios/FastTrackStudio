use dioxus::prelude::*;

use actions_proto::ids::standalone as standalone_ids;
use input::InputCommand;
use input_dioxus::ACTION_CONTEXT;
use session::session_actions;
use session_ui::Session;
use signal_proto::actions::signal_actions;

use crate::{COMMAND_PALETTE_OPEN, COMMAND_PALETTE_QUERY, DOCK_MODE, TOP_PAGE};

// Global signals for Signal patch switching and navigation.
// Written here by action handlers, read by the performance tab via use_effect.
pub(crate) static SIGNAL_PATCH_IDS: GlobalSignal<Vec<String>> = Signal::global(Vec::new);
pub(crate) static SIGNAL_ACTIVE_PATCH_ID: GlobalSignal<Option<String>> = Signal::global(|| None);
pub(crate) static SIGNAL_NAV_NEXT_SONG: GlobalSignal<u64> = Signal::global(|| 0);
pub(crate) static SIGNAL_NAV_PREV_SONG: GlobalSignal<u64> = Signal::global(|| 0);
pub(crate) static SIGNAL_NAV_NEXT_SECTION: GlobalSignal<u64> = Signal::global(|| 0);
pub(crate) static SIGNAL_NAV_PREV_SECTION: GlobalSignal<u64> = Signal::global(|| 0);

/// Generation counter for patch-switch debouncing.
/// Each keypress bumps this; the spawned task only activates if its generation
/// is still current, so rapid key-mashing only fires the last one.
static PATCH_SWITCH_GEN: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// Handle number keys 1–8 to switch Signal patches when on the rig page.
pub(crate) fn handle_signal_patch_shortcut(e: &KeyboardEvent) -> bool {
    // Only active on the Signal ("rig") page
    if *TOP_PAGE.peek() != "rig" {
        return false;
    }

    let mods = e.modifiers();
    if mods.ctrl() || mods.alt() || mods.meta() {
        return false;
    }

    let idx = match e.key() {
        Key::Character(ref c) if c == "1" => Some(0usize),
        Key::Character(ref c) if c == "2" => Some(1),
        Key::Character(ref c) if c == "3" => Some(2),
        Key::Character(ref c) if c == "4" => Some(3),
        Key::Character(ref c) if c == "5" => Some(4),
        Key::Character(ref c) if c == "6" => Some(5),
        Key::Character(ref c) if c == "7" => Some(6),
        Key::Character(ref c) if c == "8" => Some(7),
        _ => None,
    };

    let Some(idx) = idx else { return false };

    let patch_ids = SIGNAL_PATCH_IDS.peek();
    let Some(patch_id_str) = patch_ids.get(idx).cloned() else {
        return false;
    };

    // Skip if this patch is already active
    if SIGNAL_ACTIVE_PATCH_ID.peek().as_deref() == Some(patch_id_str.as_str()) {
        return true;
    }

    // Bump generation — any in-flight older activation will see a stale gen and bail
    let gen = PATCH_SWITCH_GEN.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;

    // Update the tile highlight immediately (before the async activate completes)
    *SIGNAL_ACTIVE_PATCH_ID.write() = Some(patch_id_str.clone());

    let signal = signal_ui::use_signal_service();

    spawn(async move {
        // Small delay so rapid keypresses coalesce — only the last one proceeds
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;

        // Check if a newer keypress has superseded us
        if PATCH_SWITCH_GEN.load(std::sync::atomic::Ordering::Relaxed) != gen {
            return;
        }

        let profile = signal.profiles().find_by_name("All-Around").await;
        if let Ok(Some(profile)) = profile {
            let patch_id: signal::profile::PatchId = patch_id_str.clone().into();
            match signal
                .profiles()
                .activate(profile.id.clone(), Some(patch_id))
                .await
            {
                Ok(_) => {
                    tracing::info!(
                        "Hotkey-activated patch '{patch_id_str}' in profile '{}'",
                        profile.name
                    );
                }
                Err(e) => {
                    tracing::warn!("Hotkey patch activation failed: {e:?}");
                }
            }
        }
    });

    true
}

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

        // ── Signal navigation actions (triggered by MIDI or command palette) ──
        // These bump global counters that the performance tab watches via use_effect.
        id if id == signal_actions::NEXT_SONG.as_str() => {
            *SIGNAL_NAV_NEXT_SONG.write() += 1;
        }
        id if id == signal_actions::PREVIOUS_SONG.as_str() => {
            *SIGNAL_NAV_PREV_SONG.write() += 1;
        }
        id if id == signal_actions::NEXT_SECTION.as_str() => {
            *SIGNAL_NAV_NEXT_SECTION.write() += 1;
        }
        id if id == signal_actions::PREVIOUS_SECTION.as_str() => {
            *SIGNAL_NAV_PREV_SECTION.write() += 1;
        }

        // ── Load Variant 1–24 (direct patch activation by index) ──
        id if id.starts_with("fts.signal.load_variant.") => {
            if let Some(idx_str) = id.strip_prefix("fts.signal.load_variant.") {
                if let Ok(variant_num) = idx_str.parse::<usize>() {
                    // variant_num is 1-based, SIGNAL_PATCH_IDS is 0-based
                    let idx = variant_num.saturating_sub(1);
                    let patch_ids = SIGNAL_PATCH_IDS.peek();
                    if let Some(patch_id_str) = patch_ids.get(idx).cloned() {
                        // Skip if already active
                        if SIGNAL_ACTIVE_PATCH_ID.peek().as_deref() != Some(patch_id_str.as_str())
                        {
                            let gen = PATCH_SWITCH_GEN
                                .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
                                + 1;
                            *SIGNAL_ACTIVE_PATCH_ID.write() = Some(patch_id_str.clone());

                            if let Some(signal) = try_consume_context::<signal::Signal>() {
                                spawn(async move {
                                    tokio::time::sleep(std::time::Duration::from_millis(50))
                                        .await;
                                    if PATCH_SWITCH_GEN
                                        .load(std::sync::atomic::Ordering::Relaxed)
                                        != gen
                                    {
                                        return;
                                    }
                                    let profile =
                                        signal.profiles().find_by_name("All-Around").await;
                                    if let Ok(Some(profile)) = profile {
                                        let patch_id: signal::profile::PatchId =
                                            patch_id_str.clone().into();
                                        match signal
                                            .profiles()
                                            .activate(profile.id.clone(), Some(patch_id))
                                            .await
                                        {
                                            Ok(_) => {
                                                tracing::info!(
                                                    "Activated patch '{patch_id_str}' (variant {variant_num})"
                                                );
                                            }
                                            Err(e) => {
                                                tracing::warn!(
                                                    "Patch activation failed: {e:?}"
                                                );
                                            }
                                        }
                                    }
                                });
                            }
                        }
                    }
                }
            }
        }

        _ => {
            tracing::debug!(action_id, "No handler registered for input action");
        }
    };
}
