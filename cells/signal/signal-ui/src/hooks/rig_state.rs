//! Hook for syncing rig service state to global signals.
//!
//! Subscribes to the rig service via `SignalControl` and populates
//! the global signals that UI components read from.

use crate::prelude::*;
use crate::signals::{
    RIG_AVAILABLE_PRESETS, RIG_AVAILABLE_PROFILES, RIG_AVAILABLE_SETLISTS, RIG_CONNECTED,
    RIG_CURRENT_PRESET, RIG_CURRENT_PRESET_SNAPSHOT_ID, RIG_CURRENT_SCENE, RIG_CURRENT_SETLIST,
    RIG_CURRENT_SONG, RIG_INFO, RIG_LOADING, RIG_MODULES, RIG_PRELOADED_PRESETS, RIG_PROFILE,
    RIG_SCENE_INDEX, RIG_SERVICE, RIG_SETLIST_SONGS, RIG_SONG_INDEX,
};
use signal_control::{RigControlEvent, SignalControl};

/// Hook that subscribes to rig service events and updates global signals.
///
/// Reads `SignalControl` from the `RIG_SERVICE` global signal.
///
/// 1. Fetches initial rig state
/// 2. Subscribes to rig events via `SignalControl::subscribe()`
/// 3. Updates global signals when events arrive
///
/// Call this once at app startup (e.g. in `RigLayout` or the root component).
pub fn use_rig_subscription() {
    use_effect(move || {
        let Some(ctl) = RIG_SERVICE.read().clone() else {
            tracing::warn!("use_rig_subscription: RIG_SERVICE not initialized");
            return;
        };

        spawn(async move {
            tracing::info!("use_rig_subscription: starting");

            *RIG_CONNECTED.write() = true;
            *RIG_LOADING.write() = true;

            // ── Fetch initial state ──────────────────────────────────

            if let Some(profile) = ctl.get_current_profile().await {
                tracing::info!("Loaded profile '{}'", profile.name);
                *RIG_PROFILE.write() = Some(profile);
            }

            let profiles = ctl.get_available_profiles().await;
            tracing::debug!("{} profiles available", profiles.len());
            *RIG_AVAILABLE_PROFILES.write() = profiles;

            if let Some(rig) = ctl.get_current_rig().await {
                tracing::info!("Loaded rig '{}' ({:?})", rig.name, rig.instrument_type);
                *RIG_INFO.write() = Some(rig);
            }

            let presets = ctl.get_available_presets().await;
            tracing::debug!("{} presets available", presets.len());
            *RIG_AVAILABLE_PRESETS.write() = presets;

            if let Some(preset) = ctl.get_current_preset().await {
                tracing::info!("Current preset '{}'", preset.name);
                if let Some(first_scene) = preset.scenes.first() {
                    *RIG_CURRENT_PRESET_SNAPSHOT_ID.write() = Some(first_scene.id);
                }
                *RIG_CURRENT_PRESET.write() = Some(preset);
            }

            // Materialize modules from the current preset for UI display
            let modules = ctl.get_current_modules();
            tracing::info!("{} modules resolved for UI", modules.len());
            *RIG_MODULES.write() = modules;

            let setlists = ctl.get_available_setlists().await;
            tracing::info!("{} setlists available", setlists.len());
            *RIG_AVAILABLE_SETLISTS.write() = setlists;

            if let Some(setlist) = ctl.get_current_setlist().await {
                tracing::info!(
                    "Current setlist '{}' ({} songs)",
                    setlist.name,
                    setlist.song_count
                );
                *RIG_CURRENT_SETLIST.write() = Some(setlist);
            }

            let songs = ctl.get_setlist_songs().await;
            tracing::info!("{} songs in setlist", songs.len());
            *RIG_SETLIST_SONGS.write() = songs;

            if let Some(song) = ctl.get_current_song().await {
                tracing::info!("Current song '{}' ({} scenes)", song.name, song.scene_count);
                *RIG_CURRENT_SONG.write() = Some(song.clone());
                *RIG_SONG_INDEX.write() = song.index;
            }

            if let Some(scene) = ctl.get_current_scene().await {
                tracing::info!(
                    "Current scene '{}' (preset: {})",
                    scene.name,
                    scene.preset_name
                );
                *RIG_CURRENT_SCENE.write() = Some(scene);
                // scene.index may exist — set it if available
            }

            *RIG_LOADING.write() = false;

            // ── Subscribe to live events ─────────────────────────────

            let mut rx = ctl.subscribe().await;
            tracing::debug!("Subscribed to rig events");

            loop {
                match rx.recv().await {
                    Ok(Some(event)) => {
                        tracing::debug!("Received rig event");
                        handle_event(&ctl, event).await;
                    }
                    Ok(None) => {
                        tracing::warn!("Rig event channel closed");
                        break;
                    }
                    Err(e) => {
                        tracing::warn!("Rig event deserialization error: {:?}", e);
                        continue;
                    }
                }
            }

            tracing::info!("use_rig_subscription: ended");
            *RIG_CONNECTED.write() = false;
        });
    });
}

/// Handle a single rig event and update the corresponding global signals.
async fn handle_event(ctl: &SignalControl, event: RigControlEvent) {
    match event {
        RigControlEvent::ProfileLoaded { profile } => {
            tracing::info!("Profile loaded: '{}'", profile.name);
            *RIG_PROFILE.write() = Some(profile);
        }
        RigControlEvent::PresetLoaded {
            preset,
            scene_index,
        } => {
            tracing::info!("Preset loaded: '{}' (scene {})", preset.name, scene_index);
            let scene_id = preset.scenes.get(scene_index).map(|s| s.id);
            if let Some(id) = scene_id {
                *RIG_CURRENT_PRESET_SNAPSHOT_ID.write() = Some(id);
            }
            *RIG_CURRENT_PRESET.write() = Some(preset);
            *RIG_MODULES.write() = ctl.get_current_modules();
        }
        RigControlEvent::SongChanged { song_index } => {
            tracing::info!("Song changed to index {}", song_index);
            *RIG_SONG_INDEX.write() = song_index;
            if let Some(song) = ctl.get_current_song().await {
                *RIG_CURRENT_SONG.write() = Some(song);
            }
        }
        RigControlEvent::SceneIndexChanged { scene_index } => {
            tracing::info!("Scene changed to index {}", scene_index);
            *RIG_SCENE_INDEX.write() = scene_index;
            if let Some(scene) = ctl.get_current_scene().await {
                *RIG_CURRENT_SCENE.write() = Some(scene);
            }
            if let Some(preset) = ctl.get_current_preset().await {
                *RIG_CURRENT_PRESET.write() = Some(preset);
            }
            *RIG_MODULES.write() = ctl.get_current_modules();
        }
        RigControlEvent::PreloadCompleted { handle: _ } => {
            tracing::debug!("Preload completed");
        }
        RigControlEvent::TransitionStarted { .. } => {
            *RIG_LOADING.write() = true;
        }
        RigControlEvent::TransitionCompleted { .. } => {
            *RIG_LOADING.write() = false;
        }
        _ => {}
    }
}

/// Alias — just delegates to `use_rig_subscription()`.
pub fn use_rig_init() {
    use_rig_subscription();
}
