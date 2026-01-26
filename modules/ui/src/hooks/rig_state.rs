//! Hook for syncing rig service state to global signals
//!
//! This hook subscribes to the rig service via ROAM and populates the global
//! signals that UI components read from:
//! - `RIG_PROFILE` - Currently loaded profile
//! - `RIG_INFO` - Current rig information
//! - `RIG_CURRENT_PRESET` - Currently loaded preset
//! - `RIG_CURRENT_SCENE` - Current scene in performance mode
//! - etc.

use crate::context::rig::use_rig_service;
use dioxus::prelude::*;
use fts::rig::{
    RigEvent, RIG_AVAILABLE_PRESETS, RIG_AVAILABLE_PROFILES, RIG_AVAILABLE_SETLISTS,
    RIG_CONNECTED, RIG_CURRENT_PRESET, RIG_CURRENT_SCENE, RIG_CURRENT_SETLIST, RIG_CURRENT_SONG,
    RIG_CURRENT_SNAPSHOT_ID, RIG_GLOBAL_BLOCKS, RIG_INFO, RIG_LOADING, RIG_PRELOADED_PRESETS,
    RIG_PROFILE, RIG_SCENE_INDEX, RIG_SECTIONS, RIG_SETLIST_SONGS, RIG_SONG_INDEX,
};

/// Hook that subscribes to rig service events and updates global signals
///
/// This hook:
/// 1. Fetches the initial rig state from the service
/// 2. Subscribes to rig events via ROAM channels
/// 3. Updates global signals when the service notifies of changes
///
/// Call this hook in a component that wraps your rig UI.
///
/// # Example
/// ```ignore
/// #[component]
/// fn RigApp() -> Element {
///     use_rig_subscription();
///
///     rsx! {
///         PresetSelector { /* reads from RIG_AVAILABLE_PRESETS */ }
///         SceneNavigator { /* reads from RIG_CURRENT_SCENE */ }
///     }
/// }
/// ```
pub fn use_rig_subscription() {
    let ctx = use_rig_service();

    // Initialize and subscribe on mount
    use_effect(move || {
        let client = ctx.client.clone();

        spawn(async move {
            tracing::info!("use_rig_subscription: starting subscription");

            // Mark as connected
            *RIG_CONNECTED.write() = true;
            *RIG_LOADING.write() = true;

            // Fetch initial profile
            if let Some(profile) = client.get_current_profile().await {
                tracing::info!(
                    "use_rig_subscription: loaded profile '{}'",
                    profile.name
                );
                *RIG_PROFILE.write() = Some(profile);
            }

            // Fetch available profiles
            let profiles = client.get_available_profiles().await;
            tracing::debug!(
                "use_rig_subscription: {} profiles available",
                profiles.len()
            );
            *RIG_AVAILABLE_PROFILES.write() = profiles;

            // Fetch current rig
            if let Some(rig) = client.get_current_rig().await {
                tracing::info!(
                    "use_rig_subscription: loaded rig '{}' ({:?})",
                    rig.name,
                    rig.instrument_type
                );
                *RIG_INFO.write() = Some(rig);
            }

            // Fetch sections
            let sections = client.get_sections().await;
            tracing::debug!("use_rig_subscription: {} sections", sections.len());
            *RIG_SECTIONS.write() = sections;

            // Fetch global blocks
            let blocks = client.get_global_blocks().await;
            tracing::debug!("use_rig_subscription: {} global blocks", blocks.len());
            *RIG_GLOBAL_BLOCKS.write() = blocks;

            // Fetch available presets
            let presets = client.get_available_presets().await;
            tracing::debug!("use_rig_subscription: {} presets available", presets.len());
            *RIG_AVAILABLE_PRESETS.write() = presets;

            // Fetch current preset
            if let Some(preset) = client.get_current_preset().await {
                tracing::info!("use_rig_subscription: current preset '{}'", preset.name);
                *RIG_CURRENT_PRESET.write() = Some(preset);
            }

            // Fetch available setlists
            let setlists = client.get_available_setlists().await;
            tracing::info!(
                "use_rig_subscription: {} setlists available",
                setlists.len()
            );
            *RIG_AVAILABLE_SETLISTS.write() = setlists;

            // Fetch current setlist
            if let Some(setlist) = client.get_current_setlist().await {
                tracing::info!(
                    "use_rig_subscription: current setlist '{}' with {} songs",
                    setlist.name,
                    setlist.song_count
                );
                *RIG_CURRENT_SETLIST.write() = Some(setlist);
            }

            // Fetch setlist songs
            let songs = client.get_setlist_songs().await;
            tracing::info!("use_rig_subscription: {} songs in setlist", songs.len());
            *RIG_SETLIST_SONGS.write() = songs;

            // Fetch current song/scene (if in performance mode)
            if let Some(song) = client.get_current_song().await {
                tracing::info!(
                    "use_rig_subscription: current song '{}' with {} scenes",
                    song.name,
                    song.scene_count
                );
                *RIG_CURRENT_SONG.write() = Some(song.clone());
                *RIG_SONG_INDEX.write() = song.index;
            }

            if let Some(scene) = client.get_current_scene().await {
                tracing::info!(
                    "use_rig_subscription: current scene '{}' (preset: {})",
                    scene.name,
                    scene.preset_name
                );
                *RIG_CURRENT_SCENE.write() = Some(scene.clone());
                *RIG_SCENE_INDEX.write() = scene.index;
            }

            *RIG_LOADING.write() = false;

            // Create a channel for receiving rig events
            let (tx, mut rx) = roam::channel::<RigEvent>();

            // Subscribe to rig events
            client.subscribe(tx).await;
            tracing::debug!("use_rig_subscription: subscribed to rig events");

            // Listen for updates and sync to global signals
            while let Ok(Some(event)) = rx.recv().await {
                handle_rig_event(&client, event).await;
            }
            tracing::debug!("use_rig_subscription: subscription loop ended");

            *RIG_CONNECTED.write() = false;
        });
    });
}

/// Handle a rig event and update global signals
async fn handle_rig_event<S: fts::rig::RigService + Send + Sync + 'static>(
    client: &fts::rig::LocalRigClient<S>,
    event: RigEvent,
) {
    match event {
        RigEvent::ProfileLoaded { profile } => {
            tracing::info!("RigEvent: profile loaded '{}'", profile.name);
            *RIG_PROFILE.write() = Some(profile);
        }
        RigEvent::RigLoaded { rig } => {
            tracing::info!("RigEvent: rig loaded '{}'", rig.name);
            *RIG_INFO.write() = Some(rig);
        }
        RigEvent::PresetLoaded { preset } => {
            tracing::info!("RigEvent: preset loaded '{}'", preset.name);
            *RIG_CURRENT_PRESET.write() = Some(preset);
        }
        RigEvent::SnapshotActivated {
            preset_id: _,
            snapshot_id,
        } => {
            tracing::debug!("RigEvent: snapshot activated {:?}", snapshot_id);
            *RIG_CURRENT_SNAPSHOT_ID.write() = Some(snapshot_id);
        }
        RigEvent::SetlistLoaded { setlist } => {
            tracing::info!("RigEvent: setlist loaded '{}'", setlist.name);
            *RIG_CURRENT_SETLIST.write() = Some(setlist);
            // Refetch songs for the new setlist
            let songs = client.get_setlist_songs().await;
            *RIG_SETLIST_SONGS.write() = songs;
        }
        RigEvent::SongChanged { song_index } => {
            tracing::debug!("RigEvent: song changed to index {}", song_index);
            *RIG_SONG_INDEX.write() = song_index;
            // Refetch song info
            if let Some(song) = client.get_current_song().await {
                *RIG_CURRENT_SONG.write() = Some(song);
            }
        }
        RigEvent::SceneChanged {
            song_index,
            scene_index,
        } => {
            tracing::debug!(
                "RigEvent: scene changed to song {} scene {}",
                song_index,
                scene_index
            );
            *RIG_SONG_INDEX.write() = song_index;
            *RIG_SCENE_INDEX.write() = scene_index;
            // Refetch scene and preset info
            if let Some(scene) = client.get_current_scene().await {
                tracing::debug!("RigEvent: updated scene to '{}'", scene.name);
                *RIG_CURRENT_SCENE.write() = Some(scene);
            }
            if let Some(preset) = client.get_current_preset().await {
                tracing::debug!("RigEvent: updated preset to '{}'", preset.name);
                *RIG_CURRENT_PRESET.write() = Some(preset);
            }
        }
        RigEvent::PreloadCompleted { preset_id } => {
            tracing::debug!("RigEvent: preset {:?} preloaded", preset_id);
            let mut preloaded = RIG_PRELOADED_PRESETS.write();
            if !preloaded.contains(&preset_id) {
                preloaded.push(preset_id);
            }
        }
        RigEvent::ParameterChanged {
            block_id,
            param_index,
            value,
        } => {
            tracing::trace!(
                "RigEvent: parameter changed block={:?} param={} value={}",
                block_id,
                param_index,
                value
            );
            // Parameter changes are typically handled by parameter-specific subscriptions
        }
        RigEvent::BlockChanged { block_id } => {
            tracing::debug!("RigEvent: block {:?} changed", block_id);
            // Refetch blocks
            let blocks = client.get_global_blocks().await;
            *RIG_GLOBAL_BLOCKS.write() = blocks;
        }
        RigEvent::SectionChanged { section_id } => {
            tracing::debug!("RigEvent: section {:?} changed", section_id);
            // Refetch sections
            let sections = client.get_sections().await;
            *RIG_SECTIONS.write() = sections;
        }
        RigEvent::LayerChanged {
            section_id,
            layer_index,
        } => {
            tracing::debug!(
                "RigEvent: layer {} in section {:?} changed",
                layer_index,
                section_id
            );
            // Would update layer state here
        }
        RigEvent::TransitionStarted {
            from_preset,
            to_preset,
        } => {
            tracing::debug!(
                "RigEvent: transition started from {:?} to {:?}",
                from_preset,
                to_preset
            );
            *RIG_LOADING.write() = true;
        }
        RigEvent::TransitionCompleted => {
            tracing::debug!("RigEvent: transition completed");
            *RIG_LOADING.write() = false;
        }
    }
}

/// Hook that fetches initial rig data and populates signals
///
/// This is a simpler version that just loads the initial data without
/// continuous subscription. Use this when you only need to load once.
///
/// NOTE: Prefer `use_rig_subscription()` for full functionality including
/// live event updates.
pub fn use_rig_init() {
    // Just delegate to the full subscription hook
    use_rig_subscription();
}
