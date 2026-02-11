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
use uuid::Uuid;

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
            tracing::debug!("{} presets available (mock)", presets.len());
            *RIG_AVAILABLE_PRESETS.write() = presets;

            if let Some(preset) = ctl.get_current_preset().await {
                tracing::info!("Current preset '{}'", preset.name);
                if let Some(first_scene) = preset.scenes.first() {
                    *RIG_CURRENT_PRESET_SNAPSHOT_ID.write() = Some(first_scene.id);
                }
                *RIG_CURRENT_PRESET.write() = Some(preset);
            }

            // Materialize modules from the current preset for UI display —
            // but only if RIG_MODULES hasn't already been populated by a
            // user action (e.g. load_preset from DB).
            if RIG_MODULES.read().is_empty() {
                let modules = ctl.get_current_modules();
                tracing::info!("{} modules resolved for UI (initial)", modules.len());
                *RIG_MODULES.write() = modules;
            } else {
                tracing::info!(
                    "Skipping initial module load — {} modules already present",
                    RIG_MODULES.read().len()
                );
            }

            let setlists = ctl.get_available_setlists().await;
            tracing::info!("{} setlists available", setlists.len());
            *RIG_AVAILABLE_SETLISTS.write() = setlists;

            // ── DB-backed sidebar data ───────────────────────────────
            // If a database is connected, overlay DB presets/profiles/songs
            // on top of mock data so the sidebars show persisted content.
            populate_sidebars_from_db(&ctl).await;

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
            tracing::info!(
                "Preset loaded event: '{}' (scene {})",
                preset.name,
                scene_index
            );
            let scene_id = preset.scenes.get(scene_index).map(|s| s.id);
            if let Some(id) = scene_id {
                *RIG_CURRENT_PRESET_SNAPSHOT_ID.write() = Some(id);
            }
            *RIG_CURRENT_PRESET.write() = Some(preset);

            // Only overwrite modules from mock if RIG_MODULES is currently
            // empty. When load_preset already populated DB modules, the
            // mock event arrives AFTER and would clobber them.
            if RIG_MODULES.read().is_empty() {
                let modules = ctl.get_current_modules();
                tracing::info!("PresetLoaded: setting {} mock modules", modules.len());
                *RIG_MODULES.write() = modules;

                // Rebuild node graph so grid/node views update
                let modules = crate::signals::RIG_MODULES.read();
                if !modules.is_empty() {
                    let graph =
                        crate::components::rig_grid::node_graph::NodeGraph::build_from_modules(
                            &modules,
                        );
                    tracing::info!(
                        "Rebuilt node graph from event ({} modules)",
                        graph.modules.len()
                    );
                    *crate::signals::RIG_NODE_GRAPH.write() = graph;
                }
            } else {
                tracing::info!(
                    "PresetLoaded: skipping mock overwrite — {} modules already loaded",
                    RIG_MODULES.read().len()
                );
            }
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

            // Only overwrite modules from mock if empty (same guard as PresetLoaded)
            if RIG_MODULES.read().is_empty() {
                *RIG_MODULES.write() = ctl.get_current_modules();

                let modules = crate::signals::RIG_MODULES.read();
                if !modules.is_empty() {
                    let graph =
                        crate::components::rig_grid::node_graph::NodeGraph::build_from_modules(
                            &modules,
                        );
                    *crate::signals::RIG_NODE_GRAPH.write() = graph;
                }
            }
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

/// Refresh `RIG_AVAILABLE_PRESETS` from the SQLite database.
///
/// Converts DB entity models into the `PresetInfo` types that sidebar
/// components consume. Call this after creating/deleting presets to keep
/// the sidebar in sync with the database.
pub(crate) async fn refresh_presets_from_db(ctl: &SignalControl) {
    use signal_control::{PresetInfo, PresetSnapshotInfo};

    if let Ok(db_presets) = ctl.list_rig_presets().await {
        if !db_presets.is_empty() {
            let mut preset_infos: Vec<PresetInfo> = Vec::with_capacity(db_presets.len());

            for p in &db_presets {
                let snapshots = ctl
                    .list_rig_preset_snapshots(p.id)
                    .await
                    .unwrap_or_default();
                let scene_names: Vec<String> = snapshots.iter().map(|s| s.name.clone()).collect();
                let scenes: Vec<PresetSnapshotInfo> = snapshots
                    .iter()
                    .map(|s| PresetSnapshotInfo {
                        id: s.id,
                        name: s.name.clone(),
                    })
                    .collect();

                let category = p.category.as_str().unwrap_or("").to_string();

                preset_infos.push(PresetInfo {
                    id: p.id,
                    name: p.name.clone(),
                    category,
                    rating: 0,
                    description: p.description.clone(),
                    scene_count: scenes.len(),
                    scene_names,
                    scenes,
                    is_template: p.is_template,
                });
            }

            tracing::info!("{} presets loaded from DB", preset_infos.len());
            *RIG_AVAILABLE_PRESETS.write() = preset_infos;
        }
    }
}

/// Populate sidebar signals from the SQLite database.
///
/// Converts DB entity models into the `PresetInfo`/`ProfileInfo` types that
/// the existing sidebar components already consume. This replaces mock data
/// with real persisted content when a DB is available.
async fn populate_sidebars_from_db(ctl: &SignalControl) {
    use signal_control::{ProfileInfo, ProfileSceneInfo};

    // ── Presets ──────────────────────────────────────────────────
    refresh_presets_from_db(ctl).await;

    // ── Profiles ─────────────────────────────────────────────────
    if let Ok(db_profiles) = ctl.list_profiles().await {
        if !db_profiles.is_empty() {
            let mut profile_infos: Vec<ProfileInfo> = Vec::with_capacity(db_profiles.len());

            for prof in &db_profiles {
                let templates = ctl.list_scene_templates(prof.id).await.unwrap_or_default();
                let scene_names: Vec<String> = templates.iter().map(|t| t.name.clone()).collect();
                // Resolve preset names for scene display
                let db_presets = RIG_AVAILABLE_PRESETS.read();
                let scenes: Vec<ProfileSceneInfo> = templates
                    .iter()
                    .enumerate()
                    .map(|(i, t)| {
                        let preset_name = db_presets
                            .iter()
                            .find(|p| p.id == t.preset_id)
                            .map(|p| p.name.clone())
                            .unwrap_or_else(|| "Unknown".to_string());
                        let preset_snapshot_name = t.snapshot_id.and_then(|sid| {
                            db_presets
                                .iter()
                                .find(|p| p.id == t.preset_id)
                                .and_then(|p| p.scenes.iter().find(|s| s.id == sid))
                                .map(|s| s.name.clone())
                        });
                        ProfileSceneInfo {
                            index: i,
                            name: t.name.clone(),
                            preset_id: t.preset_id,
                            preset_name,
                            preset_snapshot_id: t.snapshot_id,
                            preset_snapshot_name,
                        }
                    })
                    .collect();

                profile_infos.push(ProfileInfo {
                    id: prof.id,
                    name: prof.name.clone(),
                    rig_id: prof.rig_id,
                    scene_count: scenes.len(),
                    scene_names,
                    scenes,
                    description: prof.description.clone(),
                    is_template: prof.is_template,
                });
            }

            tracing::info!(
                "{} profiles loaded from DB (replacing mock)",
                profile_infos.len()
            );
            *RIG_AVAILABLE_PROFILES.write() = profile_infos;
        }
    }

    // ── Node Graph from Modules ─────────────────────────────────
    {
        let modules = crate::signals::RIG_MODULES.read();
        if !modules.is_empty() {
            let graph =
                crate::components::rig_grid::node_graph::NodeGraph::build_from_modules(&modules);
            tracing::info!(
                "Built node graph from {} DB modules ({} graph modules)",
                modules.len(),
                graph.modules.len()
            );
            *crate::signals::RIG_NODE_GRAPH.write() = graph;
        }
    }

    // ── Songs ────────────────────────────────────────────────────
    if let Ok(db_songs) = ctl.list_songs().await {
        if !db_songs.is_empty() {
            let mut song_infos: Vec<crate::signals::SongInfo> = Vec::with_capacity(db_songs.len());

            for (idx, song) in db_songs.iter().enumerate() {
                let scenes = ctl.list_song_scenes(song.id).await.unwrap_or_default();
                let scene_names: Vec<String> = scenes.iter().map(|s| s.name.clone()).collect();
                let scene_ids: Vec<Uuid> = scenes.iter().map(|s| s.id).collect();

                song_infos.push(crate::signals::SongInfo {
                    index: idx,
                    name: song.name.clone(),
                    artist: song.artist.clone(),
                    scene_count: scenes.len(),
                    scene_names,
                    scene_ids,
                    current_scene_index: None,
                    is_template: song.is_template,
                });
            }

            tracing::info!("{} songs loaded from DB (replacing mock)", song_infos.len());
            *RIG_SETLIST_SONGS.write() = song_infos;
        }
    }
}

/// Alias — just delegates to `use_rig_subscription()`.
pub fn use_rig_init() {
    use_rig_subscription();
}
