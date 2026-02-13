//! Hook providing rig action callbacks.
//!
//! Wraps `SignalControl` methods into Dioxus `Callback`s that UI
//! components can invoke directly (e.g. `actions.next_scene.call(())`).

use crate::callback_types::{
    MacroChange, PresetSnapshotSelect, PresetWithSnapshot, ProfileSceneSelect, SetBlockParameter,
    SetParameter,
};
use crate::hooks::rig_state::refresh_presets_from_db;
use crate::prelude::*;
use crate::signals::{
    RIG_AVAILABLE_PROFILES, RIG_CURRENT_PRESET, RIG_CURRENT_SONG, RIG_FX_CHAIN,
    RIG_LAST_APPLIED_SNAPSHOT, RIG_MODULES, RIG_NODE_FX_BINDINGS, RIG_NODE_GRAPH, RIG_PROFILE,
    RIG_SERVICE, RIG_SETLIST_SONGS, RIG_SONG_INDEX,
};
use signal_control::id::{PatchId, ProfileId, RigPresetId};
use signal_control::SignalControl;
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
    pub load_profile: Callback<ProfileId>,
    pub load_profile_scene: Callback<ProfileSceneSelect>,
    pub load_rig: Callback<ProfileId>,
    pub load_preset: Callback<RigPresetId>,
    pub load_preset_snapshot: Callback<PresetSnapshotSelect>,
    pub load_preset_with_snapshot: Callback<PresetWithSnapshot>,
    pub activate_snapshot: Callback<RigPresetId>,
    pub go_to_scene: Callback<usize>,
    pub next_scene: Callback<()>,
    pub prev_scene: Callback<()>,
    pub go_to_song: Callback<usize>,
    pub next_song: Callback<()>,
    pub prev_song: Callback<()>,
    pub preload_preset: Callback<RigPresetId>,
    pub preload_song: Callback<usize>,
    pub set_parameter: Callback<SetParameter>,
    pub set_block_parameter: Callback<SetBlockParameter>,
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
            Callback::new(move |profile_id: ProfileId| {
                let ctl = ctl.clone();
                spawn(async move {
                    // Service handles: set current profile, auto-select first patch,
                    // broadcast ProfileLoaded + PatchLoaded events.
                    ctl.load_profile(profile_id).await;
                });
            })
        },
        load_profile_scene: {
            let ctl = ctl.clone();
            Callback::new(move |sel: ProfileSceneSelect| {
                let ctl = ctl.clone();
                spawn(async move {
                    // Service handles: set current patch, broadcast PatchLoaded event.
                    ctl.load_patch(sel.profile_id, sel.scene_index).await;
                });
            })
        },
        load_rig: {
            let ctl = ctl.clone();
            Callback::new(move |rig_id: ProfileId| {
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
            Callback::new(move |preset_id: RigPresetId| {
                let ctl = ctl.clone();
                spawn(async move {
                    tracing::info!("load_preset: loading preset {preset_id}");

                    // 1. Service handles: set current_preset, build modules,
                    //    broadcast PresetSelected + ModulesChanged events.
                    ctl.select_preset(preset_id).await;

                    // 2. Auto-select default snapshot from the preset's scenes
                    let preset_info = RIG_CURRENT_PRESET.read().clone();
                    if let Some(ref info) = preset_info {
                        let default_scene = info
                            .scenes
                            .iter()
                            .find(|s| s.is_default)
                            .or_else(|| info.scenes.first());
                        if let Some(scene) = default_scene {
                            *RIG_LAST_APPLIED_SNAPSHOT.write() =
                                Some(RigPresetId::from_uuid(scene.id));
                        }
                    }

                    // 3. Profile mode: update the active patch's preset reference
                    //    (TODO Phase 4: move to AssignPresetToPatch command)
                    let current_profile = { RIG_PROFILE.read().clone() };
                    if let Some(profile) = current_profile {
                        if let Ok(templates) = ctl.list_scene_templates(profile.id.as_uuid()).await
                        {
                            if let Some(tmpl) = templates.first() {
                                if let Err(e) = ctl
                                    .update_scene_template(
                                        tmpl.id,
                                        None,
                                        Some(preset_id.as_uuid()),
                                        Some(None),
                                    )
                                    .await
                                {
                                    tracing::warn!(
                                        "load_preset: failed to update scene template: {e}"
                                    );
                                } else {
                                    refresh_profile_in_signals(&ctl, profile.id).await;
                                }
                            }
                        }
                    }
                });
            })
        },
        load_preset_snapshot: {
            let ctl = ctl.clone();
            Callback::new(move |sel: PresetSnapshotSelect| {
                let PresetSnapshotSelect {
                    preset_id,
                    snapshot_index: _,
                } = sel;
                let ctl = ctl.clone();
                spawn(async move {
                    // Service handles: preset selection, module building,
                    // broadcasts PresetSelected + ModulesChanged.
                    ctl.select_preset(preset_id).await;
                });
            })
        },
        load_preset_with_snapshot: {
            let ctl = ctl.clone();
            Callback::new(move |sel: PresetWithSnapshot| {
                let PresetWithSnapshot {
                    preset_id,
                    snapshot_id: _,
                } = sel;
                let ctl = ctl.clone();
                spawn(async move {
                    // Service handles: preset selection, module building,
                    // broadcasts PresetSelected + ModulesChanged.
                    ctl.select_preset(preset_id).await;
                });
            })
        },
        activate_snapshot: {
            Callback::new(move |snapshot_id: RigPresetId| {
                spawn(async move {
                    tracing::info!("activate_snapshot: applying snapshot {snapshot_id}");
                    *RIG_LAST_APPLIED_SNAPSHOT.write() = Some(snapshot_id);
                    // Snapshot application is handled by the engine via ApplySnapshot command.
                    // The UI just tracks which snapshot was last activated.
                });
            })
        },
        go_to_scene: {
            let ctl = ctl.clone();
            Callback::new(move |section_index: usize| {
                let ctl = ctl.clone();
                spawn(async move {
                    let song_index = *RIG_SONG_INDEX.read();
                    ctl.load_song_section(song_index, section_index).await;
                });
            })
        },
        next_scene: {
            let ctl = ctl.clone();
            Callback::new(move |_: ()| {
                let ctl = ctl.clone();
                spawn(async move { ctl.next_section().await });
            })
        },
        prev_scene: {
            let ctl = ctl.clone();
            Callback::new(move |_: ()| {
                let ctl = ctl.clone();
                spawn(async move { ctl.previous_section().await });
            })
        },
        go_to_song: {
            let ctl = ctl.clone();
            Callback::new(move |song_index: usize| {
                let ctl = ctl.clone();
                spawn(async move {
                    ctl.load_song_section(song_index, 0).await;
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
            Callback::new(move |_preset_id: RigPresetId| {
                // Preloading is now section-based, not preset-based.
                // Individual preset preloading is handled internally by the engine.
                tracing::debug!("preload_preset: preloading is now section-based");
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
        set_parameter: Callback::new(move |sp: SetParameter| {
            let SetParameter {
                node_id,
                param_index,
                value,
            } = sp;
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
        set_block_parameter: Callback::new(move |_sp: SetBlockParameter| {
            tracing::warn!("set_block_parameter: not yet implemented in signal-control");
        }),
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

                    // Create a blank preset — template instantiation will be restored
                    // once defaults/templates.rs is revived with new domain types.
                    let preset = signal_control::preset::PresetMetadata::new(
                        &data.name,
                        signal_control::category::PresetCategory::default(),
                    );
                    match ctl
                        .create_rig_preset::<signal_control::preset::PresetMetadata>(
                            &data.name,
                            desc,
                            serde_json::json!(category),
                            serde_json::json!(tags_json),
                            &preset,
                        )
                        .await
                    {
                        Ok(id) => {
                            tracing::info!("Created preset '{}' ({id})", data.name);
                            // Refresh the presets cache so the new preset appears in the sidebar
                            refresh_presets_from_db(&ctl).await;
                            // Select the newly created preset via service
                            let preset_id = RigPresetId::from_uuid(id);
                            ctl.select_preset(preset_id).await;
                            // Build modules (TODO Phase 3: move to service)
                            let db_modules = build_modules_from_db(&ctl, id).await;
                            *RIG_MODULES.write() = db_modules;
                            rebuild_node_graph();
                        }
                        Err(e) => tracing::error!("Failed to create preset: {e}"),
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

/// Refresh a single profile's patch data in RIG_AVAILABLE_PROFILES and RIG_PROFILE.
///
/// Re-fetches scene templates from the DB and rebuilds the `PatchInfo` list
/// so the sidebar immediately reflects updated preset assignments.
async fn refresh_profile_in_signals(ctl: &SignalControl, profile_id: ProfileId) {
    use signal_control::PatchInfo;

    let Ok(templates) = ctl.list_scene_templates(profile_id.as_uuid()).await else {
        return;
    };

    let patches: Vec<PatchInfo> = templates
        .iter()
        .enumerate()
        .map(|(i, t)| PatchInfo {
            id: PatchId::from_uuid(t.id),
            name: t.name.clone(),
            index: i,
        })
        .collect();

    // Update in RIG_AVAILABLE_PROFILES
    let mut profiles = RIG_AVAILABLE_PROFILES.write();
    if let Some(p) = profiles.iter_mut().find(|p| p.id == profile_id) {
        p.patch_count = patches.len();
        p.patches = patches.clone();
    }
    drop(profiles);

    // Update in RIG_PROFILE if it's the current one
    let mut current = RIG_PROFILE.write();
    if let Some(ref mut p) = *current {
        if p.id == profile_id {
            p.patch_count = patches.len();
            p.patches = patches;
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

    // Load the raw preset row and parse module_assignments from its JSON data blob.
    // The old `Preset` type embedded module_assignments directly; in the new domain
    // model those live in scenes, but the DB still holds the legacy JSON structure.
    let row = match ctl.get_rig_preset_row(preset_id).await {
        Ok(Some(r)) => r,
        Ok(None) => {
            tracing::info!("build_modules_from_db: preset {preset_id} not in DB");
            return Vec::new();
        }
        Err(e) => {
            tracing::warn!("build_modules_from_db: failed to load preset: {e}");
            return Vec::new();
        }
    };

    // Extract module_assignments array from the data JSON blob
    let assignments = match row
        .data
        .get("module_assignments")
        .and_then(|v| v.as_array())
    {
        Some(arr) => arr.clone(),
        None => {
            tracing::info!("build_modules_from_db: no module_assignments in preset data");
            return Vec::new();
        }
    };

    let mut modules = Vec::new();

    for assignment in &assignments {
        let enabled = assignment
            .get("enabled")
            .and_then(|v| v.as_bool())
            .unwrap_or(true);
        if !enabled {
            continue;
        }

        let Some(mp_id_str) = assignment.get("module_preset_id").and_then(|v| {
            v.as_str()
                .or_else(|| v.get("uuid").and_then(|u| u.as_str()))
        }) else {
            continue;
        };
        let Ok(mp_id) = mp_id_str.parse::<Uuid>() else {
            tracing::warn!("build_modules_from_db: invalid module_preset_id: {mp_id_str}");
            continue;
        };
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

        let module_type = mp_row.module_type_parsed().unwrap_or_else(|| {
            tracing::warn!(
                "build_modules_from_db: unknown module_type '{}' for preset {}, falling back to Custom",
                mp_row.module_type,
                mp_row.id,
            );
            ModuleType::Custom
        });
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
                let block_type =
                    signal_control::block::BlockType::from_variant_name(block_type_str)
                        .unwrap_or_else(|| {
                            tracing::warn!(
                                "build_modules_from_db: unknown block_type '{block_type_str}' in module '{}', falling back to Custom",
                                mp_row.name,
                            );
                            signal_control::block::BlockType::Custom
                        });
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
