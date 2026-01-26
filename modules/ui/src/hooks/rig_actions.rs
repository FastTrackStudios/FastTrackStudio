//! Hook providing rig action callbacks
//!
//! This hook wraps the RigService.execute() method, providing
//! convenient callbacks for UI components to trigger rig commands.
//! Uses ROAM's async interface for full compatibility with all service implementations.

use crate::context::rig::use_rig_service;
use dioxus::prelude::*;
use fts::rig::{
    RigCommand, RIG_AVAILABLE_SETLISTS, RIG_CURRENT_PRESET, RIG_CURRENT_SETLIST,
    RIG_CURRENT_SNAPSHOT_ID, RIG_SETLIST_SONGS,
};
use uuid::Uuid;

/// Collection of rig action callbacks
#[derive(Clone)]
pub struct RigActions {
    /// Load a profile by ID
    pub load_profile: Callback<Uuid>,
    /// Load a rig by ID
    pub load_rig: Callback<Uuid>,
    /// Load a preset by ID
    pub load_preset: Callback<Uuid>,
    /// Load a preset with a specific snapshot
    pub load_preset_with_snapshot: Callback<(Uuid, Uuid)>,
    /// Activate a snapshot on the current preset
    pub activate_snapshot: Callback<Uuid>,
    /// Go to a specific scene by index
    pub go_to_scene: Callback<usize>,
    /// Go to the next scene
    pub next_scene: Callback<()>,
    /// Go to the previous scene
    pub prev_scene: Callback<()>,
    /// Go to a specific song by index
    pub go_to_song: Callback<usize>,
    /// Go to the next song
    pub next_song: Callback<()>,
    /// Go to the previous song
    pub prev_song: Callback<()>,
    /// Preload a preset by ID
    pub preload_preset: Callback<Uuid>,
    /// Preload all presets for a song
    pub preload_song: Callback<usize>,
    /// Set a block parameter value (f64)
    pub set_parameter: Callback<(Uuid, u32, f64)>,
    /// Set a block parameter value (f32 convenience)
    pub set_block_parameter: Callback<(Uuid, u32, f32)>,
    /// Toggle block bypass
    pub toggle_block_bypass: Callback<Uuid>,
    /// Toggle section enabled
    pub toggle_section: Callback<Uuid>,
    /// Load a setlist by ID
    pub load_setlist: Callback<Uuid>,
}

/// Hook that provides rig action callbacks
///
/// Uses the rig service from context to execute commands via ROAM.
/// Commands are executed asynchronously using `spawn()`.
///
/// # Example
/// ```ignore
/// let actions = use_rig_actions();
///
/// rsx! {
///     button {
///         onclick: move |_| actions.next_scene.call(()),
///         "Next Scene"
///     }
/// }
/// ```
pub fn use_rig_actions() -> RigActions {
    let ctx = use_rig_service();

    RigActions {
        load_profile: {
            let client = ctx.client.clone();
            Callback::new(move |profile_id: Uuid| {
                let client = client.clone();
                spawn(async move {
                    client.execute(RigCommand::LoadProfile { profile_id }).await;
                });
            })
        },
        load_rig: {
            let client = ctx.client.clone();
            Callback::new(move |rig_id: Uuid| {
                let client = client.clone();
                spawn(async move {
                    client.execute(RigCommand::LoadRig { rig_id }).await;
                });
            })
        },
        load_preset: {
            let client = ctx.client.clone();
            Callback::new(move |preset_id: Uuid| {
                let client = client.clone();
                spawn(async move {
                    client.execute(RigCommand::LoadPreset { preset_id }).await;
                    // Also fetch and update global signal directly (workaround for subscription issues)
                    if let Some(preset) = client.get_current_preset().await {
                        tracing::debug!("load_preset: updating signal to '{}'", preset.name);
                        *RIG_CURRENT_PRESET.write() = Some(preset);
                    }
                });
            })
        },
        load_preset_with_snapshot: {
            let client = ctx.client.clone();
            Callback::new(move |(preset_id, snapshot_id): (Uuid, Uuid)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(RigCommand::LoadPresetWithSnapshot {
                            preset_id,
                            snapshot_id,
                        })
                        .await;
                    // Also fetch and update global signals directly (workaround for subscription issues)
                    if let Some(preset) = client.get_current_preset().await {
                        tracing::info!("load_preset_with_snapshot: updating RIG_CURRENT_PRESET to '{}', RIG_CURRENT_SNAPSHOT_ID to {:?}", preset.name, snapshot_id);
                        *RIG_CURRENT_PRESET.write() = Some(preset);
                        *RIG_CURRENT_SNAPSHOT_ID.write() = Some(snapshot_id);
                        tracing::info!("load_preset_with_snapshot: signals updated");
                    } else {
                        tracing::warn!("load_preset_with_snapshot: get_current_preset returned None");
                    }
                });
            })
        },
        activate_snapshot: {
            Callback::new(move |snapshot_id: Uuid| {
                // Note: ActivateSnapshot requires current preset ID
                // This is a placeholder - use load_preset_with_snapshot instead
                tracing::debug!("activate_snapshot called with {:?} - use load_preset_with_snapshot instead", snapshot_id);
            })
        },
        go_to_scene: {
            let client = ctx.client.clone();
            Callback::new(move |scene_index: usize| {
                let client = client.clone();
                spawn(async move {
                    client.execute(RigCommand::GoToScene { scene_index }).await;
                });
            })
        },
        next_scene: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                spawn(async move {
                    client.execute(RigCommand::NextScene).await;
                });
            })
        },
        prev_scene: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                spawn(async move {
                    client.execute(RigCommand::PreviousScene).await;
                });
            })
        },
        go_to_song: {
            let client = ctx.client.clone();
            Callback::new(move |song_index: usize| {
                let client = client.clone();
                spawn(async move {
                    client.execute(RigCommand::GoToSong { song_index }).await;
                });
            })
        },
        next_song: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                spawn(async move {
                    client.execute(RigCommand::NextSong).await;
                });
            })
        },
        prev_song: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                spawn(async move {
                    client.execute(RigCommand::PreviousSong).await;
                });
            })
        },
        preload_preset: {
            let client = ctx.client.clone();
            Callback::new(move |preset_id: Uuid| {
                let client = client.clone();
                spawn(async move {
                    client.execute(RigCommand::PreloadPreset { preset_id }).await;
                });
            })
        },
        preload_song: {
            let client = ctx.client.clone();
            Callback::new(move |song_index: usize| {
                let client = client.clone();
                spawn(async move {
                    client.execute(RigCommand::PreloadSong { song_index }).await;
                });
            })
        },
        set_parameter: {
            let client = ctx.client.clone();
            Callback::new(move |(block_id, param_index, value): (Uuid, u32, f64)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(RigCommand::SetParameter {
                            block_id,
                            param_index,
                            value,
                        })
                        .await;
                });
            })
        },
        set_block_parameter: {
            let client = ctx.client.clone();
            Callback::new(move |(block_id, param_index, value): (Uuid, u32, f32)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(RigCommand::SetParameter {
                            block_id,
                            param_index,
                            value: f64::from(value),
                        })
                        .await;
                });
            })
        },
        toggle_block_bypass: {
            let client = ctx.client.clone();
            Callback::new(move |block_id: Uuid| {
                let client = client.clone();
                spawn(async move {
                    // Toggle bypass (we'd need current state, so set to false for now)
                    client.execute(RigCommand::SetBlockBypassed { block_id, bypassed: false }).await;
                });
            })
        },
        toggle_section: {
            let client = ctx.client.clone();
            Callback::new(move |section_id: Uuid| {
                let client = client.clone();
                spawn(async move {
                    // Toggle section (we'd need current state, so set to true for now)
                    client.execute(RigCommand::SetSectionEnabled { section_id, enabled: true }).await;
                });
            })
        },
        load_setlist: {
            let client = ctx.client.clone();
            Callback::new(move |setlist_id: Uuid| {
                let client = client.clone();
                spawn(async move {
                    client.execute(RigCommand::LoadSetlist { setlist_id }).await;
                    // Update global signals directly (workaround for subscription issues)
                    if let Some(setlist) = client.get_current_setlist().await {
                        tracing::debug!("load_setlist: updating signal to '{}'", setlist.name);
                        *RIG_CURRENT_SETLIST.write() = Some(setlist);
                    }
                    // Also refresh the songs list
                    let songs = client.get_setlist_songs().await;
                    *RIG_SETLIST_SONGS.write() = songs;
                    // And refresh available setlists
                    let setlists = client.get_available_setlists().await;
                    *RIG_AVAILABLE_SETLISTS.write() = setlists;
                });
            })
        },
    }
}
