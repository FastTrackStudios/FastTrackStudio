//! Hook providing rig action callbacks.
//!
//! Wraps `SignalControl` methods into Dioxus `Callback`s that UI
//! components can invoke directly (e.g. `actions.next_scene.call(())`).

use crate::prelude::*;
use crate::signals::{
    RIG_CURRENT_PRESET, RIG_CURRENT_PRESET_SNAPSHOT_ID, RIG_LAST_APPLIED_SNAPSHOT, RIG_PROFILE,
    RIG_SERVICE,
};
use signal_control::{PreloadPriority, RigControlCommand, SignalControl};
use uuid::Uuid;

/// Collection of rig action callbacks for UI components.
#[derive(Clone)]
pub struct RigActions {
    pub load_profile: Callback<Uuid>,
    pub load_profile_scene: Callback<(Uuid, usize)>,
    pub load_rig: Callback<Uuid>,
    pub load_preset: Callback<Uuid>,
    pub load_preset_snapshot: Callback<(Uuid, usize)>,
    pub load_preset_with_snapshot: Callback<(Uuid, Uuid)>,
    pub activate_snapshot: Callback<Uuid>,
    pub go_to_scene: Callback<usize>,
    pub next_scene: Callback<()>,
    pub prev_scene: Callback<()>,
    pub go_to_song: Callback<usize>,
    pub next_song: Callback<()>,
    pub prev_song: Callback<()>,
    pub preload_preset: Callback<Uuid>,
    pub preload_song: Callback<usize>,
    pub set_parameter: Callback<(Uuid, u32, f64)>,
    pub set_block_parameter: Callback<(Uuid, u32, f32)>,
    pub toggle_block_bypass: Callback<Uuid>,
    pub toggle_section: Callback<Uuid>,
    pub load_setlist: Callback<Uuid>,
}

/// Hook that provides rig action callbacks.
///
/// Reads `SignalControl` from the `RIG_SERVICE` global signal.
/// All commands are dispatched asynchronously via `SignalControl`.
///
/// # Panics
/// Panics if `RIG_SERVICE` has not been initialized (call `init_rig_service()` first).
pub fn use_rig_actions() -> RigActions {
    let ctl = RIG_SERVICE
        .read()
        .clone()
        .expect("RIG_SERVICE not initialized — call init_rig_service() first");

    RigActions {
        load_profile: {
            let ctl = ctl.clone();
            Callback::new(move |profile_id: Uuid| {
                let ctl = ctl.clone();
                spawn(async move {
                    let profiles = ctl.get_available_profiles().await;
                    if let Some(profile) = profiles.iter().find(|p| p.id == profile_id) {
                        if let Some(first_scene) = profile.scenes.first() {
                            tracing::info!(
                                "load_profile: '{}' scene '{}'",
                                profile.name,
                                first_scene.name
                            );

                            let snapshot_idx = resolve_preset_snapshot_index(
                                &ctl,
                                first_scene.preset_id,
                                first_scene.preset_snapshot_id,
                            )
                            .await;

                            ctl.load_preset_with_scene(first_scene.preset_id, snapshot_idx)
                                .await;

                            if let Some(preset) = ctl.get_current_preset().await {
                                *RIG_CURRENT_PRESET.write() = Some(preset);
                            }
                            *RIG_PROFILE.write() = Some(profile.clone());
                        }
                    }
                });
            })
        },
        load_profile_scene: {
            let ctl = ctl.clone();
            Callback::new(move |(profile_id, scene_index): (Uuid, usize)| {
                let ctl = ctl.clone();
                spawn(async move {
                    let profiles = ctl.get_available_profiles().await;
                    if let Some(profile) = profiles.iter().find(|p| p.id == profile_id) {
                        if let Some(scene) = profile.scenes.get(scene_index) {
                            tracing::info!(
                                "load_profile_scene: '{}' scene '{}'",
                                profile.name,
                                scene.name
                            );

                            let snapshot_idx = resolve_preset_snapshot_index(
                                &ctl,
                                scene.preset_id,
                                scene.preset_snapshot_id,
                            )
                            .await;

                            ctl.load_preset_with_scene(scene.preset_id, snapshot_idx)
                                .await;

                            if let Some(preset) = ctl.get_current_preset().await {
                                *RIG_CURRENT_PRESET.write() = Some(preset);
                            }
                            *RIG_PROFILE.write() = Some(profile.clone());
                        }
                    }
                });
            })
        },
        load_rig: {
            let ctl = ctl.clone();
            Callback::new(move |rig_id: Uuid| {
                let ctl = ctl.clone();
                spawn(async move {
                    // In rig-control, you load profiles, not rigs directly.
                    // Treat rig_id as a profile_id for backwards compat.
                    ctl.load_profile(rig_id).await;
                });
            })
        },
        load_preset: {
            let ctl = ctl.clone();
            Callback::new(move |preset_id: Uuid| {
                let ctl = ctl.clone();
                spawn(async move {
                    ctl.load_preset_with_scene(preset_id, 0).await;
                    if let Some(preset) = ctl.get_current_preset().await {
                        *RIG_CURRENT_PRESET.write() = Some(preset);
                    }
                });
            })
        },
        load_preset_snapshot: {
            let ctl = ctl.clone();
            Callback::new(move |(preset_id, scene_index): (Uuid, usize)| {
                let ctl = ctl.clone();
                spawn(async move {
                    ctl.load_preset_with_scene(preset_id, scene_index).await;
                    if let Some(preset) = ctl.get_current_preset().await {
                        *RIG_CURRENT_PRESET.write() = Some(preset);
                    }
                });
            })
        },
        load_preset_with_snapshot: {
            let ctl = ctl.clone();
            Callback::new(move |(preset_id, _snapshot_id): (Uuid, Uuid)| {
                let ctl = ctl.clone();
                spawn(async move {
                    ctl.load_preset_with_scene(preset_id, 0).await;
                    if let Some(preset) = ctl.get_current_preset().await {
                        *RIG_CURRENT_PRESET.write() = Some(preset);
                    }
                });
            })
        },
        activate_snapshot: {
            let ctl = ctl.clone();
            Callback::new(move |snapshot_id: Uuid| {
                let ctl = ctl.clone();
                spawn(async move {
                    // Find the current preset and resolve the scene index for this snapshot
                    let preset = RIG_CURRENT_PRESET.read().clone();
                    if let Some(preset) = preset {
                        if let Some(scene_index) =
                            preset.scenes.iter().position(|s| s.id == snapshot_id)
                        {
                            tracing::info!(
                                "activate_snapshot: applying '{}' (scene {})",
                                preset.scenes[scene_index].name,
                                scene_index,
                            );

                            ctl.load_preset_with_scene(preset.id, scene_index).await;

                            // Update tracking signals
                            *RIG_LAST_APPLIED_SNAPSHOT.write() = Some(snapshot_id);
                            *RIG_CURRENT_PRESET_SNAPSHOT_ID.write() = Some(snapshot_id);

                            if let Some(updated_preset) = ctl.get_current_preset().await {
                                *RIG_CURRENT_PRESET.write() = Some(updated_preset);
                            }
                        } else {
                            tracing::warn!(
                                "activate_snapshot: snapshot {:?} not found in preset '{}'",
                                snapshot_id,
                                preset.name,
                            );
                        }
                    } else {
                        tracing::warn!("activate_snapshot: no preset loaded");
                    }
                });
            })
        },
        go_to_scene: {
            let ctl = ctl.clone();
            Callback::new(move |scene_index: usize| {
                let ctl = ctl.clone();
                spawn(async move {
                    ctl.execute(RigControlCommand::SwitchScene { scene_index })
                        .await;
                });
            })
        },
        next_scene: {
            let ctl = ctl.clone();
            Callback::new(move |_: ()| {
                let ctl = ctl.clone();
                spawn(async move { ctl.next_scene().await });
            })
        },
        prev_scene: {
            let ctl = ctl.clone();
            Callback::new(move |_: ()| {
                let ctl = ctl.clone();
                spawn(async move { ctl.previous_scene().await });
            })
        },
        go_to_song: {
            let ctl = ctl.clone();
            Callback::new(move |song_index: usize| {
                let ctl = ctl.clone();
                spawn(async move {
                    ctl.execute(RigControlCommand::LoadSongScene {
                        song_index,
                        scene_index: 0,
                    })
                    .await;
                });
            })
        },
        next_song: {
            let ctl = ctl.clone();
            Callback::new(move |_: ()| {
                let ctl = ctl.clone();
                spawn(async move { ctl.next_song().await });
            })
        },
        prev_song: {
            let ctl = ctl.clone();
            Callback::new(move |_: ()| {
                let ctl = ctl.clone();
                spawn(async move { ctl.previous_song().await });
            })
        },
        preload_preset: {
            let ctl = ctl.clone();
            Callback::new(move |preset_id: Uuid| {
                let ctl = ctl.clone();
                spawn(async move {
                    ctl.execute(RigControlCommand::Preload {
                        preset_id,
                        scene_index: 0,
                        priority: PreloadPriority::Low,
                    })
                    .await;
                });
            })
        },
        preload_song: {
            let ctl = ctl.clone();
            Callback::new(move |song_index: usize| {
                let ctl = ctl.clone();
                spawn(async move {
                    let songs = ctl.get_setlist_songs().await;
                    if let Some(song) = songs.get(song_index) {
                        tracing::debug!("Preload song '{}' (placeholder)", song.name);
                    }
                });
            })
        },
        set_parameter: Callback::new(
            move |(_module_id, _param_index, _value): (Uuid, u32, f64)| {
                tracing::warn!("set_parameter: not yet implemented in signal-control");
            },
        ),
        set_block_parameter: Callback::new(
            move |(_module_id, _param_index, _value): (Uuid, u32, f32)| {
                tracing::warn!("set_block_parameter: not yet implemented in signal-control");
            },
        ),
        toggle_block_bypass: Callback::new(move |_module_id: Uuid| {
            tracing::warn!("toggle_block_bypass: not yet implemented");
        }),
        toggle_section: Callback::new(move |_section_id: Uuid| {
            tracing::debug!("toggle_section: sections replaced by module slots");
        }),
        load_setlist: Callback::new(move |_setlist_id: Uuid| {
            tracing::debug!("load_setlist: use song navigation instead");
        }),
    }
}

/// Resolve the scene index within a preset from a snapshot ID.
///
/// If the profile scene specifies a `preset_snapshot_id`, we look it up
/// in the preset's scene list to find the numeric index. Falls back to 0.
async fn resolve_preset_snapshot_index(
    ctl: &SignalControl,
    preset_id: Uuid,
    preset_snapshot_id: Option<Uuid>,
) -> usize {
    let Some(snapshot_id) = preset_snapshot_id else {
        return 0;
    };
    let presets = ctl.get_available_presets().await;
    presets
        .iter()
        .find(|p| p.id == preset_id)
        .and_then(|p| p.scenes.iter().position(|s| s.id == snapshot_id))
        .unwrap_or(0)
}
