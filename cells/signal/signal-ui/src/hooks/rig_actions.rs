//! Hook providing rig action callbacks.
//!
//! Wraps `SignalControl` methods into Dioxus `Callback`s that UI
//! components can invoke directly (e.g. `actions.next_scene.call(())`).

use crate::hooks::rig_state::refresh_presets_from_db;
use crate::prelude::*;
use crate::signals::{
    RIG_AVAILABLE_PRESETS, RIG_AVAILABLE_PROFILES, RIG_CURRENT_PRESET,
    RIG_CURRENT_PRESET_SNAPSHOT_ID, RIG_CURRENT_SONG, RIG_FX_CHAIN, RIG_LAST_APPLIED_SNAPSHOT,
    RIG_MODULES, RIG_NODE_FX_BINDINGS, RIG_NODE_GRAPH, RIG_PROFILE, RIG_SCENE_INDEX, RIG_SERVICE,
    RIG_SETLIST_SONGS,
};
use signal_control::defaults::templates;
use signal_control::template::RigTemplate;
use signal_control::{PreloadPriority, RigControlCommand, SignalControl};
use uuid::Uuid;

/// Data submitted from the create entity modal.
#[derive(Debug, Clone, PartialEq)]
pub struct CreateEntityData {
    pub name: String,
    pub category: String,
    pub description: String,
    pub tags: Vec<String>,
    /// For presets: index into the known template list (None = blank).
    /// 0 = Guitar Rig, 1 = Vocal Rig.
    pub template_index: Option<usize>,
}

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
    pub create_preset: Callback<CreateEntityData>,
    pub create_profile: Callback<CreateEntityData>,
    pub create_song: Callback<CreateEntityData>,
    pub create_scene: Callback<CreateEntityData>,
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
                        // Immediately set profile for instant sidebar expansion
                        *RIG_PROFILE.write() = Some(profile.clone());

                        if let Some(first_scene) = profile.scenes.first() {
                            tracing::info!(
                                "load_profile: '{}' scene '{}'",
                                profile.name,
                                first_scene.name
                            );

                            // Immediately set preset from cached list for instant UI expansion
                            let preset_info = RIG_AVAILABLE_PRESETS
                                .read()
                                .iter()
                                .find(|p| p.id == first_scene.preset_id)
                                .cloned();
                            if let Some(info) = preset_info {
                                *RIG_CURRENT_PRESET.write() = Some(info);
                            }

                            let snapshot_idx = resolve_preset_snapshot_index(
                                &ctl,
                                first_scene.preset_id,
                                first_scene.preset_snapshot_id,
                            )
                            .await;

                            ctl.load_preset_with_scene(first_scene.preset_id, snapshot_idx)
                                .await;

                            // Update with authoritative data after async load
                            if let Some(preset) = ctl.get_current_preset().await {
                                *RIG_CURRENT_PRESET.write() = Some(preset);
                            }
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
                        // Immediately set profile for instant sidebar expansion
                        *RIG_PROFILE.write() = Some(profile.clone());

                        if let Some(scene) = profile.scenes.get(scene_index) {
                            tracing::info!(
                                "load_profile_scene: '{}' scene '{}'",
                                profile.name,
                                scene.name
                            );

                            // Immediately set preset from cached list for instant UI expansion
                            let preset_info = RIG_AVAILABLE_PRESETS
                                .read()
                                .iter()
                                .find(|p| p.id == scene.preset_id)
                                .cloned();
                            if let Some(info) = preset_info {
                                *RIG_CURRENT_PRESET.write() = Some(info);
                            }
                            // Track which snapshot is active for the scene
                            *RIG_CURRENT_PRESET_SNAPSHOT_ID.write() = scene.preset_snapshot_id;

                            let snapshot_idx = resolve_preset_snapshot_index(
                                &ctl,
                                scene.preset_id,
                                scene.preset_snapshot_id,
                            )
                            .await;

                            ctl.load_preset_with_scene(scene.preset_id, snapshot_idx)
                                .await;

                            // Update with authoritative data after async load
                            if let Some(preset) = ctl.get_current_preset().await {
                                *RIG_CURRENT_PRESET.write() = Some(preset);
                            }
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
                    tracing::info!("load_preset: loading preset {preset_id}");

                    // Capture old state before overwriting (needed for profile scene matching)
                    let old_preset_id = RIG_CURRENT_PRESET.read().as_ref().map(|p| p.id);
                    let old_snapshot_id = *RIG_CURRENT_PRESET_SNAPSHOT_ID.read();

                    // 1. Set RIG_CURRENT_PRESET from the available presets list
                    //    (fixes selected state immediately — same IDs as sidebar)
                    let preset_info = RIG_AVAILABLE_PRESETS
                        .read()
                        .iter()
                        .find(|p| p.id == preset_id)
                        .cloned();
                    if let Some(ref info) = preset_info {
                        tracing::info!(
                            "load_preset: selected '{}' ({} scenes)",
                            info.name,
                            info.scene_count
                        );
                        *RIG_CURRENT_PRESET.write() = Some(info.clone());
                    }
                    // Clear old snapshot since we're switching to a different preset
                    *RIG_CURRENT_PRESET_SNAPSHOT_ID.write() = None;

                    // 2. Profile mode: update the active scene template's preset reference
                    if let Some(profile) = RIG_PROFILE.read().clone() {
                        // Find the scene that was active before the user clicked a new preset.
                        // Match by old preset_id + old snapshot_id to identify the exact scene.
                        let active_scene = profile
                            .scenes
                            .iter()
                            .find(|s| {
                                let snapshot_match = old_snapshot_id
                                    .map(|sid| s.preset_snapshot_id == Some(sid))
                                    .unwrap_or(false);
                                snapshot_match || Some(s.preset_id) == old_preset_id
                            })
                            .or_else(|| profile.scenes.first());

                        if let Some(scene) = active_scene {
                            // Look up scene template ID from DB
                            if let Ok(templates) = ctl.list_scene_templates(profile.id).await {
                                if let Some(tmpl) = templates.get(scene.index) {
                                    tracing::info!(
                                        "load_preset: updating profile '{}' scene '{}' preset → {}",
                                        profile.name,
                                        tmpl.name,
                                        preset_id,
                                    );
                                    if let Err(e) = ctl
                                        .update_scene_template(
                                            tmpl.id,
                                            None,
                                            Some(preset_id),
                                            Some(None),
                                        )
                                        .await
                                    {
                                        tracing::warn!(
                                            "load_preset: failed to update scene template: {e}"
                                        );
                                    } else {
                                        // Refresh the profile's scene data in sidebar
                                        refresh_profile_in_signals(&ctl, profile.id).await;
                                    }
                                }
                            }
                        }
                    }

                    // 2b. Song mode: update the active song section's preset reference
                    if RIG_PROFILE.read().is_none() {
                        if let Some(song) = RIG_CURRENT_SONG.read().clone() {
                            let scene_idx = *RIG_SCENE_INDEX.read();
                            if let Some(scene_id) = song.scene_ids.get(scene_idx) {
                                tracing::info!(
                                    "load_preset: updating song '{}' section {} preset → {}",
                                    song.name,
                                    scene_idx,
                                    preset_id,
                                );
                                if let Err(e) = ctl
                                    .update_song_scene(*scene_id, None, Some(preset_id), Some(None))
                                    .await
                                {
                                    tracing::warn!("load_preset: failed to update song scene: {e}");
                                }
                            }
                        }
                    }

                    // 3. Try to build modules from DB data
                    let db_modules = build_modules_from_db(&ctl, preset_id).await;
                    if !db_modules.is_empty() {
                        tracing::info!("load_preset: built {} modules from DB", db_modules.len());
                        *RIG_MODULES.write() = db_modules;
                    } else {
                        // Fall back to mock service for non-DB presets
                        tracing::info!("load_preset: no DB modules, falling back to mock");
                        ctl.load_preset_with_scene(preset_id, 0).await;
                        *RIG_MODULES.write() = ctl.get_current_modules();
                    }

                    // 4. Rebuild node graph
                    rebuild_node_graph();
                });
            })
        },
        load_preset_snapshot: {
            let ctl = ctl.clone();
            Callback::new(move |(preset_id, scene_index): (Uuid, usize)| {
                let ctl = ctl.clone();
                spawn(async move {
                    tracing::info!("load_preset_snapshot: preset={preset_id} scene={scene_index}");
                    // Set selection from sidebar list
                    let preset_info = RIG_AVAILABLE_PRESETS
                        .read()
                        .iter()
                        .find(|p| p.id == preset_id)
                        .cloned();
                    if let Some(ref info) = preset_info {
                        *RIG_CURRENT_PRESET.write() = Some(info.clone());
                    }
                    let snapshot_id = preset_info
                        .as_ref()
                        .and_then(|info| info.scenes.get(scene_index).map(|s| s.id));
                    *RIG_CURRENT_PRESET_SNAPSHOT_ID.write() = snapshot_id;

                    // Song mode: update song section with preset + snapshot
                    if RIG_PROFILE.read().is_none() {
                        if let Some(song) = RIG_CURRENT_SONG.read().clone() {
                            let song_scene_idx = *RIG_SCENE_INDEX.read();
                            if let Some(scene_db_id) = song.scene_ids.get(song_scene_idx) {
                                tracing::info!(
                                    "load_preset_snapshot: updating song '{}' section {} → preset={}, snapshot={:?}",
                                    song.name, song_scene_idx, preset_id, snapshot_id,
                                );
                                if let Err(e) = ctl
                                    .update_song_scene(
                                        *scene_db_id,
                                        None,
                                        Some(preset_id),
                                        Some(snapshot_id),
                                    )
                                    .await
                                {
                                    tracing::warn!(
                                        "load_preset_snapshot: failed to update song scene: {e}"
                                    );
                                }
                            }
                        }
                    }

                    // Build modules from DB
                    let db_modules = build_modules_from_db(&ctl, preset_id).await;
                    if !db_modules.is_empty() {
                        *RIG_MODULES.write() = db_modules;
                    } else {
                        ctl.load_preset_with_scene(preset_id, scene_index).await;
                        *RIG_MODULES.write() = ctl.get_current_modules();
                    }
                    rebuild_node_graph();
                });
            })
        },
        load_preset_with_snapshot: {
            let ctl = ctl.clone();
            Callback::new(move |(preset_id, _snapshot_id): (Uuid, Uuid)| {
                let ctl = ctl.clone();
                spawn(async move {
                    let preset_info = RIG_AVAILABLE_PRESETS
                        .read()
                        .iter()
                        .find(|p| p.id == preset_id)
                        .cloned();
                    if let Some(ref info) = preset_info {
                        *RIG_CURRENT_PRESET.write() = Some(info.clone());
                    }

                    let db_modules = build_modules_from_db(&ctl, preset_id).await;
                    if !db_modules.is_empty() {
                        *RIG_MODULES.write() = db_modules;
                    } else {
                        ctl.load_preset_with_scene(preset_id, 0).await;
                        *RIG_MODULES.write() = ctl.get_current_modules();
                    }
                    rebuild_node_graph();
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
        set_parameter: Callback::new(move |(node_id, param_index, value): (Uuid, u32, f64)| {
            spawn(async move {
                let binding = match RIG_NODE_FX_BINDINGS.read().get(&node_id).cloned() {
                    Some(b) => b,
                    None => {
                        tracing::debug!("set_parameter: no DAW binding for node {}", node_id);
                        return;
                    }
                };
                let Some(chain) = RIG_FX_CHAIN.read().clone() else {
                    tracing::debug!("set_parameter: no active FX chain binding");
                    return;
                };
                let Ok(Some(handle)) = chain.by_guid(&binding.fx_guid).await else {
                    tracing::warn!(
                        "set_parameter: plugin {} not found in current chain",
                        binding.fx_guid
                    );
                    return;
                };

                if let Err(e) = handle.param(param_index).set(value).await {
                    tracing::warn!(
                        "set_parameter: failed {}[{}] -> {:.3}: {}",
                        binding.fx_guid,
                        param_index,
                        value,
                        e
                    );
                }
            });
        }),
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
        create_preset: {
            let ctl = ctl.clone();
            Callback::new(move |data: CreateEntityData| {
                let ctl = ctl.clone();
                spawn(async move {
                    tracing::info!(
                        "create_preset: '{}' template={:?} category='{}' tags={:?}",
                        data.name,
                        data.template_index,
                        data.category,
                        data.tags
                    );

                    // Resolve selected template
                    let template: Option<RigTemplate> =
                        data.template_index.and_then(resolve_template_by_index);

                    let category = if data.category.is_empty() {
                        "Uncategorized".to_string()
                    } else {
                        data.category.clone()
                    };
                    let tags_json: Vec<serde_json::Value> =
                        data.tags.iter().map(|t| serde_json::json!(t)).collect();
                    let desc = if data.description.is_empty() {
                        None
                    } else {
                        Some(data.description.as_str())
                    };

                    if let Some(tmpl) = &template {
                        tracing::info!(
                            "Creating preset '{}' from template '{}'",
                            data.name,
                            tmpl.name
                        );

                        // Instantiate template into domain objects
                        let (mut preset, module_presets): (
                            signal_control::preset::Preset,
                            Vec<signal_control::module_preset::ModulePreset>,
                        ) = tmpl.instantiate();
                        preset.name = data.name.clone();

                        // Create each module preset in the DB using the template-generated ID
                        // so Preset.module_assignments can find them by ModulePresetId.
                        for mp in &module_presets {
                            let blocks_json = serde_json::json!(mp
                                .blocks
                                .iter()
                                .map(|mb| {
                                    serde_json::json!({
                                        "block": {
                                            "name": mb.block.name,
                                            "block_type": format!("{:?}", mb.block.block_type),
                                            "alias": mb.block.alias,
                                            "description": mb.block.description,
                                            "is_placeholder": mb.block.is_placeholder(),
                                        },
                                        "local_col": mb.local_col,
                                        "local_row": mb.local_row,
                                    })
                                })
                                .collect::<Vec<_>>());
                            // Store grid dimensions as module-level metadata
                            let macros_json = serde_json::json!({
                                "grid_width": mp.grid_width,
                                "grid_height": mp.grid_height,
                            });
                            if let Err(e) = ctl
                                .create_module_preset_with_id(
                                    mp.id.as_uuid(),
                                    &mp.name,
                                    mp.module_type.display_name(),
                                    mp.description.as_deref(),
                                    blocks_json,
                                    macros_json,
                                )
                                .await
                            {
                                tracing::warn!("Failed to create module preset '{}': {e}", mp.name);
                            }
                        }

                        // Create the rig preset with full Preset data (including module_assignments)
                        match ctl
                            .create_rig_preset::<signal_control::preset::Preset>(
                                &data.name,
                                desc,
                                serde_json::json!(category),
                                serde_json::json!(tags_json),
                                &preset,
                            )
                            .await
                        {
                            Ok(id) => {
                                tracing::info!(
                                    "Created preset '{}' ({id}) from template",
                                    data.name
                                );
                                refresh_presets_from_db(&ctl).await;
                                // Select the new preset by finding it in the refreshed list
                                let info = RIG_AVAILABLE_PRESETS
                                    .read()
                                    .iter()
                                    .find(|p| p.id == id)
                                    .cloned();
                                if let Some(info) = info {
                                    *RIG_CURRENT_PRESET.write() = Some(info);
                                }
                                // Build modules from DB
                                let db_modules = build_modules_from_db(&ctl, id).await;
                                if !db_modules.is_empty() {
                                    *RIG_MODULES.write() = db_modules;
                                }
                                rebuild_node_graph();
                            }
                            Err(e) => tracing::error!("Failed to create preset: {e}"),
                        }
                    } else {
                        // Blank preset — store a real Preset object (not null)
                        let preset = signal_control::preset::Preset::new(
                            &data.name,
                            signal_control::category::PresetCategory::default(),
                        );
                        match ctl
                            .create_rig_preset::<signal_control::preset::Preset>(
                                &data.name,
                                desc,
                                serde_json::json!(category),
                                serde_json::json!(tags_json),
                                &preset,
                            )
                            .await
                        {
                            Ok(id) => {
                                tracing::info!("Created blank preset '{}' ({id})", data.name);
                                refresh_presets_from_db(&ctl).await;
                                let info = RIG_AVAILABLE_PRESETS
                                    .read()
                                    .iter()
                                    .find(|p| p.id == id)
                                    .cloned();
                                if let Some(info) = info {
                                    *RIG_CURRENT_PRESET.write() = Some(info);
                                }
                                // Blank preset has no modules — clear
                                *RIG_MODULES.write() = Vec::new();
                                rebuild_node_graph();
                            }
                            Err(e) => tracing::error!("Failed to create preset: {e}"),
                        }
                    }
                });
            })
        },
        create_profile: {
            let ctl = ctl.clone();
            Callback::new(move |data: CreateEntityData| {
                let ctl = ctl.clone();
                spawn(async move {
                    let desc = if data.description.is_empty() {
                        None
                    } else {
                        Some(data.description.as_str())
                    };
                    match ctl.create_profile(&data.name, Uuid::nil(), desc).await {
                        Ok(id) => {
                            tracing::info!("Created profile '{}' ({id})", data.name);
                            *RIG_AVAILABLE_PROFILES.write() = ctl.get_available_profiles().await;
                        }
                        Err(e) => tracing::error!("Failed to create profile: {e}"),
                    }
                });
            })
        },
        create_song: {
            let ctl = ctl.clone();
            Callback::new(move |data: CreateEntityData| {
                let ctl = ctl.clone();
                spawn(async move {
                    match ctl.create_song(&data.name, None, false).await {
                        Ok(id) => {
                            tracing::info!("Created song '{}' ({id})", data.name);
                            *RIG_SETLIST_SONGS.write() = ctl.get_setlist_songs().await;
                        }
                        Err(e) => tracing::warn!("create_song: {e} (requires database backend)"),
                    }
                });
            })
        },
        create_scene: {
            let ctl = ctl.clone();
            Callback::new(move |data: CreateEntityData| {
                let ctl = ctl.clone();
                spawn(async move {
                    let song = RIG_CURRENT_SONG.read().clone();
                    let Some(song) = song else {
                        tracing::warn!("create_scene: no current song");
                        return;
                    };
                    tracing::info!(
                        "create_scene: '{}' for song '{}' (index {})",
                        data.name,
                        song.name,
                        song.index
                    );
                    // TODO: Wire up add_song_scene once SongInfo carries a UUID
                    *RIG_SETLIST_SONGS.write() = ctl.get_setlist_songs().await;
                });
            })
        },
    }
}

/// Refresh a single profile's scene data in RIG_AVAILABLE_PROFILES and RIG_PROFILE.
///
/// Re-fetches scene templates from the DB and rebuilds the `ProfileSceneInfo` list
/// so the sidebar immediately reflects updated preset assignments.
async fn refresh_profile_in_signals(ctl: &SignalControl, profile_id: Uuid) {
    use signal_control::ProfileSceneInfo;

    let Ok(templates) = ctl.list_scene_templates(profile_id).await else {
        return;
    };

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
    drop(db_presets);

    let scene_names: Vec<String> = scenes.iter().map(|s| s.name.clone()).collect();

    // Update in RIG_AVAILABLE_PROFILES
    let mut profiles = RIG_AVAILABLE_PROFILES.write();
    if let Some(p) = profiles.iter_mut().find(|p| p.id == profile_id) {
        p.scene_count = scenes.len();
        p.scene_names = scene_names.clone();
        p.scenes = scenes.clone();
    }
    drop(profiles);

    // Update in RIG_PROFILE if it's the current one
    let mut current = RIG_PROFILE.write();
    if let Some(ref mut p) = *current {
        if p.id == profile_id {
            p.scene_count = scenes.len();
            p.scene_names = scene_names;
            p.scenes = scenes;
        }
    }
}

/// Rebuild RIG_NODE_GRAPH from the current RIG_MODULES.
fn rebuild_node_graph() {
    let modules = RIG_MODULES.read();
    if !modules.is_empty() {
        let graph =
            crate::components::rig_grid::node_graph::NodeGraph::build_from_modules(&modules);
        tracing::info!("Rebuilt node graph ({} modules)", graph.modules.len());
        *RIG_NODE_GRAPH.write() = graph;
    }
}

/// Log module details for debugging.
fn log_modules(modules: &[signal_control::module::Module]) {
    for m in modules {
        let block_names: Vec<&str> = m.blocks.iter().map(|b| b.block.name.as_str()).collect();
        let placeholder_count = m.blocks.iter().filter(|b| b.block.is_placeholder()).count();
        tracing::info!(
            "  module '{}' ({}): {} blocks {:?} ({} placeholder)",
            m.name,
            m.module_type.display_name(),
            m.blocks.len(),
            block_names,
            placeholder_count
        );
    }
}

/// Build Module objects from DB data for a given preset ID.
///
/// Loads the Preset from the DB (Facet-deserialized), iterates its
/// module_assignments, loads each ModulePreset from the DB, and
/// reconstructs Module objects with blocks parsed from JSON.
async fn build_modules_from_db(
    ctl: &SignalControl,
    preset_id: Uuid,
) -> Vec<signal_control::module::Module> {
    use signal_control::block::{Block, BlockType, PluginId};
    use signal_control::module::{Module, ModuleBlock, ModuleType};
    use signal_control::normalized::Order;

    // Load the full Preset object from DB
    let preset: Option<signal_control::preset::Preset> = match ctl
        .get_rig_preset::<signal_control::preset::Preset>(preset_id)
        .await
    {
        Ok(p) => p,
        Err(e) => {
            tracing::warn!("build_modules_from_db: failed to load preset: {e}");
            return Vec::new();
        }
    };

    let Some(preset) = preset else {
        tracing::info!("build_modules_from_db: preset {preset_id} not in DB");
        return Vec::new();
    };

    let mut modules = Vec::new();

    for assignment in &preset.module_assignments {
        if !assignment.enabled {
            continue;
        }

        let mp_id = assignment.module_preset_id.as_uuid();
        let mp_row = match ctl.get_module_preset(mp_id).await {
            Ok(Some(row)) => row,
            Ok(None) => {
                tracing::warn!("build_modules_from_db: module preset {mp_id} not found in DB");
                continue;
            }
            Err(e) => {
                tracing::warn!("build_modules_from_db: failed to load module preset {mp_id}: {e}");
                continue;
            }
        };

        let module_type =
            ModuleType::from_container_name(&mp_row.module_type).unwrap_or(ModuleType::Custom);
        let mut module = Module::new(&mp_row.name, module_type);

        // Restore grid dimensions from macros metadata
        if let Some(gw) = mp_row.macros.get("grid_width").and_then(|v| v.as_u64()) {
            module.grid_width = Some(gw as usize);
        }
        if let Some(gh) = mp_row.macros.get("grid_height").and_then(|v| v.as_u64()) {
            module.grid_height = Some(gh as usize);
        }

        // Parse blocks from JSON
        if let Some(blocks_arr) = mp_row.blocks.as_array() {
            for block_json in blocks_arr {
                let block_obj = block_json.get("block").unwrap_or(block_json);
                let name = block_obj
                    .get("name")
                    .and_then(|v| v.as_str())
                    .unwrap_or("Unknown")
                    .to_string();
                let block_type_str = block_obj
                    .get("block_type")
                    .and_then(|v| v.as_str())
                    .unwrap_or("Custom");
                let block_type = parse_block_type(block_type_str);
                let alias = block_obj
                    .get("alias")
                    .and_then(|v| v.as_str())
                    .map(String::from);
                let description = block_obj
                    .get("description")
                    .and_then(|v| v.as_str())
                    .map(String::from);

                let mut block =
                    Block::new(name, PluginId::unassigned()).with_block_type(block_type);
                block.alias = alias;
                block.description = description;
                let order = Order::new(module.blocks.len() as u8);
                let mut mb = ModuleBlock::new(block, order);

                // Restore 2D grid position
                mb.local_col = block_json
                    .get("local_col")
                    .and_then(|v| v.as_u64())
                    .map(|v| v as usize);
                mb.local_row = block_json
                    .get("local_row")
                    .and_then(|v| v.as_u64())
                    .map(|v| v as usize);

                module.add_block(mb);
            }
        }

        modules.push(module);
    }

    log_modules(&modules);
    modules
}

/// Parse a BlockType from its Debug/variant name string.
fn parse_block_type(s: &str) -> signal_control::block::BlockType {
    use signal_control::block::BlockType;
    match s {
        "Input" => BlockType::Input,
        "Compressor" => BlockType::Compressor,
        "Drive" => BlockType::Drive,
        "Amp" => BlockType::Amp,
        "Cabinet" => BlockType::Cabinet,
        "Eq" => BlockType::Eq,
        "Modulation" => BlockType::Modulation,
        "Delay" => BlockType::Delay,
        "Reverb" => BlockType::Reverb,
        "Gate" => BlockType::Gate,
        "Volume" => BlockType::Volume,
        "Pitch" => BlockType::Pitch,
        "Tremolo" => BlockType::Tremolo,
        "Limiter" => BlockType::Limiter,
        "Send" => BlockType::Send,
        "Special" => BlockType::Special,
        "Freeze" => BlockType::Freeze,
        "DeEsser" => BlockType::DeEsser,
        "Saturator" => BlockType::Saturator,
        "Tuner" => BlockType::Tuner,
        "Chorus" => BlockType::Chorus,
        "Flanger" => BlockType::Flanger,
        "Phaser" => BlockType::Phaser,
        "RingModulator" => BlockType::RingModulator,
        "Wah" => BlockType::Wah,
        "Filter" => BlockType::Filter,
        "Doubler" => BlockType::Doubler,
        "Panner" => BlockType::Panner,
        "Vibrato" => BlockType::Vibrato,
        "Rotary" => BlockType::Rotary,
        "Crossover" => BlockType::Crossover,
        "Boost" => BlockType::Boost,
        _ => BlockType::Custom,
    }
}

/// Look up a `RigTemplate` by index into the known built-in templates.
///
/// 0 = Guitar Rig, 1 = Vocal Rig. Returns None for out-of-bounds.
fn resolve_template_by_index(index: usize) -> Option<RigTemplate> {
    match index {
        0 => Some(templates::guitar_rig_template()),
        1 => Some(templates::vocal_rig_template()),
        _ => None,
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
