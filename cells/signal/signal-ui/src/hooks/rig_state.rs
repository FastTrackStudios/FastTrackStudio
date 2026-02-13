//! Hook for syncing rig service state to global signals.
//!
//! All data flows through the `RigControlService` trait. The active backend
//! (mock or standalone DB-backed) handles queries internally — the UI never
//! talks to the database directly.

use crate::prelude::*;
use crate::signals::{
    RIG_AVAILABLE_PRESETS, RIG_AVAILABLE_PROFILES, RIG_CONNECTED, RIG_CURRENT_SECTION,
    RIG_CURRENT_SONG, RIG_LOADING, RIG_PROFILE, RIG_SECTION_INDEX, RIG_SERVICE, RIG_SETLIST_SONGS,
    RIG_SONG_INDEX,
};
use signal_control::{RigControlEvent, SignalControl};

/// Hook that subscribes to rig service events and updates global signals.
///
/// All data is loaded from the SQLite database via `populate_sidebars_from_db`.
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

            // ── Load all data from DB ────────────────────────────────
            populate_sidebars_from_db(&ctl).await;

            *RIG_LOADING.write() = false;

            // ── Subscribe to live events ─────────────────────────────

            let mut rx = ctl.subscribe().await;
            tracing::debug!("Subscribed to rig events");

            loop {
                match rx.recv().await {
                    Ok(Some(event)) => {
                        tracing::debug!("Received rig event");
                        handle_event(event).await;
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
///
/// These events come from live DAW interactions (song/section navigation,
/// transitions). They update index signals — the actual data is already
/// loaded from the DB.
async fn handle_event(event: RigControlEvent) {
    match event {
        RigControlEvent::ProfileLoaded { profile } => {
            tracing::info!("Profile loaded: '{}'", profile.name);
            *RIG_PROFILE.write() = Some(profile);
            // Service auto-broadcasts PatchLoaded for the first patch,
            // so RIG_CURRENT_PATCH is updated by the next event.
        }
        RigControlEvent::PatchLoaded { patch } => {
            tracing::info!("Patch loaded: '{}' (index={})", patch.name, patch.index);
            *crate::signals::RIG_CURRENT_PATCH.write() = Some(patch);
        }
        RigControlEvent::PresetSelected { preset } => {
            tracing::info!("Preset selected: '{}'", preset.name);
            *crate::signals::RIG_CURRENT_PRESET.write() = Some(preset);
        }
        RigControlEvent::ModulesChanged { modules } => {
            tracing::info!("Modules changed: {} modules", modules.len());
            *crate::signals::RIG_MODULES.write() = modules;
            // Rebuild node graph from updated modules
            let mods = crate::signals::RIG_MODULES.read();
            if !mods.is_empty() {
                let graph =
                    crate::components::rig_grid::node_graph::NodeGraph::build_from_modules(&mods);
                *crate::signals::RIG_NODE_GRAPH.write() = graph;
            }
        }
        RigControlEvent::SongChanged { song_index } => {
            tracing::info!("Song changed to index {}", song_index);
            *RIG_SONG_INDEX.write() = song_index;

            // Look up the song from our DB-loaded list
            let songs = RIG_SETLIST_SONGS.read();
            if let Some(song) = songs.get(song_index).cloned() {
                *RIG_CURRENT_SONG.write() = Some(song);
            }
        }
        RigControlEvent::SectionChanged { section_index } => {
            tracing::info!("Section changed to index {}", section_index);
            *RIG_SECTION_INDEX.write() = section_index;

            // Look up section name from the current song's DB-loaded data
            let current_song = RIG_CURRENT_SONG.read();
            if let Some(ref song) = *current_song {
                if let Some(section_name) = song.section_names.get(section_index) {
                    *RIG_CURRENT_SECTION.write() = Some(crate::signals::SectionInfo {
                        name: section_name.clone(),
                        index: section_index,
                        has_overrides: false,
                    });
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

/// Refresh `RIG_AVAILABLE_PRESETS` via the service trait.
pub(crate) async fn refresh_presets_from_db(ctl: &SignalControl) {
    let presets = ctl.get_available_presets().await;
    tracing::info!("{} presets loaded via service", presets.len());
    *RIG_AVAILABLE_PRESETS.write() = presets;
}

/// Populate sidebar signals via the service trait.
///
/// The active backend (mock or standalone) handles entity→Info conversion
/// internally. This function just calls trait methods and writes to signals.
pub(crate) async fn populate_sidebars_from_db(ctl: &SignalControl) {
    // ── Presets ──────────────────────────────────────────────────
    refresh_presets_from_db(ctl).await;

    // ── Profiles ─────────────────────────────────────────────────
    let profiles = ctl.get_available_profiles().await;
    tracing::info!("{} profiles loaded via service", profiles.len());
    *RIG_AVAILABLE_PROFILES.write() = profiles;

    // ── Node Graph from Modules ─────────────────────────────────
    {
        let modules = crate::signals::RIG_MODULES.read();
        if !modules.is_empty() {
            let graph =
                crate::components::rig_grid::node_graph::NodeGraph::build_from_modules(&modules);
            *crate::signals::RIG_NODE_GRAPH.write() = graph;
        }
    }

    // ── Songs ────────────────────────────────────────────────────
    let songs = ctl.get_setlist_songs().await;
    if !songs.is_empty() {
        tracing::info!("{} songs loaded via service", songs.len());

        // Set first song as current
        if let Some(first) = songs.first().cloned() {
            *RIG_CURRENT_SONG.write() = Some(first.clone());
            *RIG_SONG_INDEX.write() = 0;

            // Set current section to the default (or first)
            let section_idx = first.default_section_index.unwrap_or(0);
            if let Some(section_name) = first.section_names.get(section_idx) {
                *RIG_CURRENT_SECTION.write() = Some(crate::signals::SectionInfo {
                    name: section_name.clone(),
                    index: section_idx,
                    has_overrides: false,
                });
                *RIG_SECTION_INDEX.write() = section_idx;
            }
        }

        // Build setlist from songs (service provides synthetic setlist)
        let setlist = ctl.get_current_setlist().await;
        *crate::signals::RIG_CURRENT_SETLIST.write() = setlist;

        *RIG_SETLIST_SONGS.write() = songs;
    }
}

/// Alias — just delegates to `use_rig_subscription()`.
pub fn use_rig_init() {
    use_rig_subscription();
}
