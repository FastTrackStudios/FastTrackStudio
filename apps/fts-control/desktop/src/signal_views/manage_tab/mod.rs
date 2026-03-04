mod breadcrumb;
mod preset_list;
mod profile_list;
mod song_panel;
mod top_bar;

use dioxus::prelude::*;
use std::rc::Rc;

use super::{ManagePresetItem, ManageProfileItem, SignalBrowserDialog, SIGNAL_SELECTED_SETLIST_ID};

/// Which editing mode the Manage tab is in.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum ManageMode {
    Preset,
    Profile,
    Song,
}

/// Identifies what entity is being renamed or targeted by a context menu action.
#[derive(Clone, PartialEq)]
pub(crate) enum EditTarget {
    Rig {
        id: String,
    },
    RigScene {
        rig_id: String,
        scene_id: String,
    },
    Profile {
        id: String,
    },
    Patch {
        profile_id: String,
        patch_id: String,
    },
    Song {
        id: String,
    },
    Section {
        song_id: String,
        section_id: String,
    },
}

/// Convert a GridSlot's parameters into a domain `Block`.
fn slot_to_block(slot: &signal_ui::components::GridSlot) -> signal::Block {
    signal::Block::from_parameters(
        slot.parameters
            .iter()
            .map(|(name, val)| {
                signal::BlockParameter::new(
                    name.to_lowercase().replace(' ', "-"),
                    name.clone(),
                    *val,
                )
            })
            .collect(),
    )
}

/// Convert a set of module GridSlots into a domain `Module`.
fn slots_to_module(slots: &[signal_ui::components::GridSlot]) -> signal::Module {
    let blocks: Vec<signal::ModuleBlock> = slots
        .iter()
        .map(|s| {
            let source = match (&s.preset_id, &s.snapshot_id) {
                (Some(pid), Some(sid)) => signal::ModuleBlockSource::PresetSnapshot {
                    preset_id: pid.clone().into(),
                    snapshot_id: sid.clone().into(),
                    saved_at_version: None,
                },
                (Some(pid), None) => signal::ModuleBlockSource::PresetDefault {
                    preset_id: pid.clone().into(),
                    saved_at_version: None,
                },
                _ => signal::ModuleBlockSource::Inline {
                    block: slot_to_block(s),
                },
            };
            signal::ModuleBlock::new(
                s.id.to_string(),
                s.block_preset_name
                    .clone()
                    .unwrap_or_else(|| format!("{:?}", s.block_type)),
                s.block_type,
                source,
            )
        })
        .collect();
    signal::Module::from_blocks(blocks)
}

#[component]
pub(crate) fn SignalManageTab() -> Element {
    let signal = signal_ui::use_signal_service();
    use signal::rig::RigType;
    use signal::song::SectionSource;
    use signal_ui::views::{
        engines_to_grid_slots, rig_type_to_engine_type, BrowserAssignment, EngineFlowData,
        EngineParamLookup, RigGridPanel, SectionEntry, SongEntry,
    };

    // Rig type selector — filters presets, default Guitar (matches browser)
    let mut rig_type = use_signal(|| RigType::Guitar);
    // Manage mode — controls which panels are visible
    let mut manage_mode = use_signal(|| ManageMode::Song);

    let mut manage_profiles = use_signal(Vec::<ManageProfileItem>::new);
    let mut expanded_profile_ids = use_signal(std::collections::HashSet::<String>::new);
    let mut selected_profile = use_signal(|| None::<String>);
    let mut selected_patch = use_signal(|| None::<String>);

    // Combined preset list (rigs + layers), each with expandable sub-items
    let mut manage_presets = use_signal(Vec::<ManagePresetItem>::new);
    let mut expanded_ids = use_signal(std::collections::HashSet::<String>::new);
    // Currently selected parent preset (rig or layer ID)
    let mut selected_preset_id = use_signal(|| None::<String>);
    // Currently selected sub-item (scene or variant ID)
    let mut selected_sub_id = use_signal(|| None::<String>);

    // Setlist dropdown: list of (id, name) options. First entry is always "All Songs" (id="all").
    let mut setlist_options = use_signal(Vec::<(String, String)>::new);
    let mut selected_setlist_id = use_signal(|| SIGNAL_SELECTED_SETLIST_ID());

    let mut songs = use_signal(Vec::<SongEntry>::new);
    let mut selected_song_id = use_signal(|| None::<String>);
    let mut song_sections = use_signal(Vec::<SectionEntry>::new);
    let mut selected_section_id = use_signal(|| None::<String>);
    let mut active_song_name = use_signal(|| "Songs".to_string());
    let mut active_base_profile_name = use_signal(|| None::<String>);

    // Maps section_id → SectionSource for navigation and assignment
    let mut section_sources = use_signal(std::collections::HashMap::<String, SectionSource>::new);
    // Maps patch_id → (rig_id, scene_id) so section navigation via Patch doesn't need async
    let mut patch_rig_map = use_signal(std::collections::HashMap::<String, (String, String)>::new);
    // Maps patch_id → display label ("RigName / SceneName") for the profile browser
    let mut patch_display_labels = use_signal(std::collections::HashMap::<String, String>::new);

    // Track whether the active selection is a rig (true) or layer (false)
    let mut active_is_rig = use_signal(|| true);

    // Inline rename state
    let mut editing_target = use_signal(|| None::<EditTarget>);
    let mut editing_text = use_signal(String::new);

    // Context menu state
    let mut ctx_target = use_signal(|| None::<EditTarget>);
    let mut ctx_pos = use_signal(|| (0.0f64, 0.0f64));

    // Assign-preset browser state: (profile_id, patch_id) being assigned
    let mut assign_preset_target = use_signal(|| None::<(String, String)>);
    let mut rig_id = use_signal(|| None::<String>);
    let mut active_scene_id = use_signal(|| None::<String>);
    // Scenes for the currently selected rig preset (id, name) — for scene tabs
    let mut rig_scenes = use_signal(Vec::<(String, String)>::new);

    // Resolved engine flow data + param lookup for the grid panel (center)
    let mut canvas_engines = use_signal(Vec::<EngineFlowData>::new);
    let mut canvas_params = use_signal(EngineParamLookup::new);

    /// Build a SectionEntry with resolved source labels, and track its source.
    fn build_section_entry(
        sec: &signal::song::Section,
        presets: &[ManagePresetItem],
        profiles: &[ManageProfileItem],
        base_profile_id: Option<&str>,
    ) -> (SectionEntry, SectionSource) {
        let source = sec.source.clone();
        let (rig_scene_name, profile_patch_name) = match &source {
            SectionSource::RigScene { rig_id, scene_id } => {
                let label = presets
                    .iter()
                    .find(|p| p.id == rig_id.to_string())
                    .and_then(|p| {
                        let scene_str = scene_id.to_string();
                        p.sub_items
                            .iter()
                            .find(|(sid, _)| *sid == scene_str)
                            .map(|(_, sname)| format!("{} / {}", p.name, sname))
                    });
                (label, None)
            }
            SectionSource::Patch { patch_id } => {
                let patch_str = patch_id.to_string();
                let label = profiles.iter().find_map(|prof| {
                    prof.patches
                        .iter()
                        .find(|(pid, _)| *pid == patch_str)
                        .map(|(_, pname)| format!("{} / {}", prof.name, pname))
                });
                (None, label)
            }
        };
        let is_base_profile_section = base_profile_id.map(|bp_id| match &source {
            SectionSource::RigScene { .. } => false,
            SectionSource::Patch { patch_id } => {
                let pid_str = patch_id.to_string();
                profiles
                    .iter()
                    .find(|p| p.id == bp_id)
                    .map_or(false, |prof| {
                        prof.patches.iter().any(|(pid, _)| *pid == pid_str)
                    })
            }
        });
        let entry = SectionEntry {
            id: sec.id.to_string(),
            name: sec.name.clone(),
            rig_scene_name,
            profile_patch_name,
            tempo: None,
            key_signature: None,
            notes: None,
            is_base_profile_section,
        };
        (entry, source)
    }

    // Unified effect: re-runs when rig_type changes.
    // Loads rigs + layers as combined preset list, profiles, and songs.
    {
        let signal = signal.clone();
        use_effect(move || {
            let rt = rig_type();
            let signal = signal.clone();

            // Reset all state when rig type changes
            selected_preset_id.set(None);
            selected_sub_id.set(None);
            selected_profile.set(None);
            selected_patch.set(None);
            selected_song_id.set(None);
            selected_section_id.set(None);
            active_is_rig.set(true);
            rig_id.set(None);
            active_scene_id.set(None);
            rig_scenes.set(Vec::new());
            canvas_engines.set(Vec::new());
            canvas_params.set(EngineParamLookup::new());
            song_sections.set(Vec::new());
            active_song_name.set("Songs".to_string());
            active_base_profile_name.set(None);
            expanded_ids.set(std::collections::HashSet::new());
            expanded_profile_ids.set(std::collections::HashSet::new());

            spawn(async move {
                // 1) Rigs filtered by type
                let rigs = signal.rigs().list().await.unwrap_or_default();
                let filtered: Vec<_> = rigs
                    .into_iter()
                    .filter(|r| r.rig_type.map_or(false, |t| t == rt))
                    .collect();
                let rig_id_set: std::collections::HashSet<String> =
                    filtered.iter().map(|r| r.id.to_string()).collect();

                // 2) Layers filtered by engine type
                let et = rig_type_to_engine_type(rt);
                let all_layers = signal.layers().list().await.unwrap_or_default();
                let matching_layers: Vec<_> = all_layers
                    .into_iter()
                    .filter(|l| l.engine_type == et)
                    .collect();

                // Build combined preset list: rigs first, then layers
                let mut items: Vec<ManagePresetItem> = Vec::new();
                for r in &filtered {
                    items.push(ManagePresetItem {
                        id: r.id.to_string(),
                        name: r.name.clone(),
                        is_rig: true,
                        sub_items: r
                            .variants
                            .iter()
                            .map(|v| (v.id.to_string(), v.name.clone()))
                            .collect(),
                    });
                }
                for l in &matching_layers {
                    items.push(ManagePresetItem {
                        id: l.id.to_string(),
                        name: l.name.clone(),
                        is_rig: false,
                        sub_items: l
                            .variants
                            .iter()
                            .map(|v| (v.id.to_string(), v.name.clone()))
                            .collect(),
                    });
                }
                manage_presets.set(items);

                // 3) Profiles — keep only those with patches targeting a rig of this type
                let all_profiles = signal.profiles().list().await.unwrap_or_default();
                let matching_profiles: Vec<_> = all_profiles
                    .into_iter()
                    .filter(|p| {
                        p.patches.iter().any(|patch| {
                            match &patch.target {
                                signal::profile::PatchTarget::RigScene { rig_id, .. } => {
                                    rig_id_set.contains(rig_id.as_str())
                                }
                                _ => true, // Non-rig targets always shown
                            }
                        })
                    })
                    .collect();
                manage_profiles.set(
                    matching_profiles
                        .iter()
                        .map(|p| ManageProfileItem {
                            id: p.id.to_string(),
                            name: p.name.clone(),
                            patches: p
                                .patches
                                .iter()
                                .map(|patch| (patch.id.to_string(), patch.name.clone()))
                                .collect(),
                        })
                        .collect(),
                );
                // Cache patch → (rig_id, scene_id) for fast section navigation
                // Also build display labels: patch_id → "RigName / SceneName"
                let preset_items = manage_presets();
                let mut prm: std::collections::HashMap<String, (String, String)> =
                    std::collections::HashMap::new();
                let mut labels: std::collections::HashMap<String, String> =
                    std::collections::HashMap::new();
                for p in &matching_profiles {
                    for patch in &p.patches {
                        let (rig_str, scene_str) = match &patch.target {
                            signal::profile::PatchTarget::RigScene { rig_id, scene_id } => {
                                (rig_id.to_string(), scene_id.to_string())
                            }
                            _ => (String::new(), String::new()),
                        };
                        if !rig_str.is_empty() {
                            prm.insert(patch.id.to_string(), (rig_str.clone(), scene_str.clone()));
                        }
                        let label = preset_items
                            .iter()
                            .find(|pi| pi.id == rig_str)
                            .and_then(|pi| {
                                pi.sub_items
                                    .iter()
                                    .find(|(sid, _)| *sid == scene_str)
                                    .map(|(_, sname)| format!("{} / {}", pi.name, sname))
                            })
                            .unwrap_or_else(|| "Unlinked".to_string());
                        labels.insert(patch.id.to_string(), label);
                    }
                }
                patch_rig_map.set(prm);
                patch_display_labels.set(labels);

                // 4) Setlists — build dropdown options + "All Songs" union
                let all_setlists = signal.setlists().list().await.unwrap_or_default();
                let mut opts: Vec<(String, String)> =
                    vec![("all".to_string(), "All Songs".to_string())];
                for sl in &all_setlists {
                    opts.push((sl.id.to_string(), sl.name.clone()));
                }
                setlist_options.set(opts);
                let requested_setlist = selected_setlist_id();
                let selected = if requested_setlist == "all"
                    || all_setlists
                        .iter()
                        .any(|sl| sl.id.to_string() == requested_setlist)
                {
                    requested_setlist
                } else {
                    "all".to_string()
                };
                selected_setlist_id.set(selected.clone());
                *SIGNAL_SELECTED_SETLIST_ID.write() = selected.clone();

                // Initial song list follows the globally selected setlist.
                let cur_presets = manage_presets();
                let cur_profiles = manage_profiles();
                if selected == "all" {
                    let all_songs_list = signal.songs().list().await.unwrap_or_default();
                    songs.set(
                        all_songs_list
                            .iter()
                            .map(|s| SongEntry {
                                id: s.id.to_string(),
                                name: s.name.clone(),
                                section_count: s.sections.len(),
                                duration_display: None,
                            })
                            .collect(),
                    );
                    if let Some(first_song) = all_songs_list.first() {
                        active_song_name.set(first_song.name.clone());
                        selected_song_id.set(Some(first_song.id.to_string()));
                        let base_name =
                            first_song
                                .metadata
                                .base_profile_id
                                .as_ref()
                                .and_then(|bid| {
                                    cur_profiles
                                        .iter()
                                        .find(|p| p.id == *bid)
                                        .map(|p| p.name.clone())
                                });
                        active_base_profile_name.set(base_name);
                        let mut entries = Vec::new();
                        let mut sources = std::collections::HashMap::new();
                        let bp_id = first_song.metadata.base_profile_id.as_deref();
                        for sec in &first_song.sections {
                            let (entry, source) =
                                build_section_entry(sec, &cur_presets, &cur_profiles, bp_id);
                            sources.insert(entry.id.clone(), source);
                            entries.push(entry);
                        }
                        song_sections.set(entries);
                        section_sources.set(sources);
                    }
                } else if let Some(setlist) = signal.setlists().load(selected).await.ok().flatten()
                {
                    let mut song_entries = Vec::new();
                    let mut first_song = None;
                    for entry in &setlist.entries {
                        if let Some(song) = signal
                            .songs()
                            .load(entry.song_id.clone())
                            .await
                            .ok()
                            .flatten()
                        {
                            song_entries.push(SongEntry {
                                id: song.id.to_string(),
                                name: song.name.clone(),
                                section_count: song.sections.len(),
                                duration_display: None,
                            });
                            if first_song.is_none() {
                                first_song = Some(song);
                            }
                        }
                    }
                    songs.set(song_entries);
                    if let Some(song) = first_song {
                        selected_song_id.set(Some(song.id.to_string()));
                        active_song_name.set(song.name.clone());
                        let base_name = song.metadata.base_profile_id.as_ref().and_then(|bid| {
                            cur_profiles
                                .iter()
                                .find(|p| p.id == *bid)
                                .map(|p| p.name.clone())
                        });
                        active_base_profile_name.set(base_name);
                        let mut entries = Vec::new();
                        let mut sources = std::collections::HashMap::new();
                        let bp_id = song.metadata.base_profile_id.as_deref();
                        for sec in &song.sections {
                            let (entry, source) =
                                build_section_entry(sec, &cur_presets, &cur_profiles, bp_id);
                            sources.insert(entry.id.clone(), source);
                            entries.push(entry);
                        }
                        song_sections.set(entries);
                        section_sources.set(sources);
                    }
                }

                // 5) Auto-select first rig preset, expand it, resolve its first scene
                if let Some(first) = filtered.first() {
                    let first_id = first.id.to_string();
                    rig_id.set(Some(first_id.clone()));
                    selected_preset_id.set(Some(first_id.clone()));
                    active_is_rig.set(true);
                    expanded_ids.set([first_id.clone()].into_iter().collect());
                    rig_scenes.set(
                        first
                            .variants
                            .iter()
                            .map(|v| (v.id.to_string(), v.name.clone()))
                            .collect(),
                    );

                    if let Some(first_scene) = first.variants.first() {
                        let scene_id = first_scene.id.to_string();
                        active_scene_id.set(Some(scene_id.clone()));
                        selected_sub_id.set(Some(scene_id.clone()));
                        let init_result = signal_ui::views::resolve_scene_engines(&signal, &first_id, &scene_id).await;
                        match &init_result {
                            Some((engines, _)) => tracing::info!("init resolve_scene_engines: {} engines for rig={first_id}, scene={scene_id}", engines.len()),
                            None => tracing::warn!("init resolve_scene_engines: returned None for rig={first_id}, scene={scene_id}"),
                        }
                        if let Some((engines, params)) = init_result {
                            canvas_engines.set(engines);
                            canvas_params.set(params);
                        }
                    }
                }
            });
        });
    }

    // Handle song selection — load that song's sections
    let load_song_sections = {
        let signal = signal.clone();
        move |song_id: String| {
            let signal = signal.clone();
            selected_song_id.set(Some(song_id.clone()));
            selected_section_id.set(None);
            spawn(async move {
                if let Some(song) = signal.songs().load(song_id).await.ok().flatten() {
                    active_song_name.set(song.name.clone());
                    let cur_presets = manage_presets();
                    let cur_profiles = manage_profiles();
                    // Resolve base profile name
                    let base_name = song.metadata.base_profile_id.as_ref().and_then(|bid| {
                        cur_profiles
                            .iter()
                            .find(|p| p.id == *bid)
                            .map(|p| p.name.clone())
                    });
                    active_base_profile_name.set(base_name);
                    let mut entries = Vec::new();
                    let mut sources = std::collections::HashMap::new();
                    let bp_id = song.metadata.base_profile_id.as_deref();
                    for sec in &song.sections {
                        let (entry, source) =
                            build_section_entry(sec, &cur_presets, &cur_profiles, bp_id);
                        sources.insert(entry.id.clone(), source);
                        entries.push(entry);
                    }
                    song_sections.set(entries);
                    section_sources.set(sources);
                }
            });
        }
    };

    // Handle setlist dropdown change — filter songs by setlist entries
    let change_setlist = {
        let signal = signal.clone();
        move |setlist_id: String| {
            let signal = signal.clone();
            selected_setlist_id.set(setlist_id.clone());
            *SIGNAL_SELECTED_SETLIST_ID.write() = setlist_id.clone();
            selected_song_id.set(None);
            selected_section_id.set(None);
            song_sections.set(Vec::new());
            active_song_name.set("Songs".to_string());
            active_base_profile_name.set(None);
            spawn(async move {
                if setlist_id == "all" {
                    // "All Songs" = every song in the database
                    let all_songs = signal.songs().list().await.unwrap_or_default();
                    songs.set(
                        all_songs
                            .iter()
                            .map(|s| SongEntry {
                                id: s.id.to_string(),
                                name: s.name.clone(),
                                section_count: s.sections.len(),
                                duration_display: None,
                            })
                            .collect(),
                    );
                    if let Some(first) = all_songs.first() {
                        selected_song_id.set(Some(first.id.to_string()));
                        active_song_name.set(first.name.clone());
                        let cur_presets = manage_presets();
                        let cur_profiles = manage_profiles();
                        let base_name = first.metadata.base_profile_id.as_ref().and_then(|bid| {
                            cur_profiles
                                .iter()
                                .find(|p| p.id == *bid)
                                .map(|p| p.name.clone())
                        });
                        active_base_profile_name.set(base_name);
                        let mut entries = Vec::new();
                        let mut sources = std::collections::HashMap::new();
                        let bp_id = first.metadata.base_profile_id.as_deref();
                        for sec in &first.sections {
                            let (entry, source) =
                                build_section_entry(sec, &cur_presets, &cur_profiles, bp_id);
                            sources.insert(entry.id.clone(), source);
                            entries.push(entry);
                        }
                        song_sections.set(entries);
                        section_sources.set(sources);
                    }
                } else if let Some(setlist) =
                    signal.setlists().load(setlist_id).await.ok().flatten()
                {
                    // Load songs from this specific setlist
                    let mut song_entries = Vec::new();
                    let mut first_song = None;
                    for entry in &setlist.entries {
                        if let Some(song) = signal
                            .songs()
                            .load(entry.song_id.clone())
                            .await
                            .ok()
                            .flatten()
                        {
                            song_entries.push(SongEntry {
                                id: song.id.to_string(),
                                name: song.name.clone(),
                                section_count: song.sections.len(),
                                duration_display: None,
                            });
                            if first_song.is_none() {
                                first_song = Some(song);
                            }
                        }
                    }
                    songs.set(song_entries);
                    if let Some(song) = first_song {
                        selected_song_id.set(Some(song.id.to_string()));
                        active_song_name.set(song.name.clone());
                        let cur_presets = manage_presets();
                        let cur_profiles = manage_profiles();
                        let base_name = song.metadata.base_profile_id.as_ref().and_then(|bid| {
                            cur_profiles
                                .iter()
                                .find(|p| p.id == *bid)
                                .map(|p| p.name.clone())
                        });
                        active_base_profile_name.set(base_name);
                        let mut entries = Vec::new();
                        let mut sources = std::collections::HashMap::new();
                        let bp_id = song.metadata.base_profile_id.as_deref();
                        for sec in &song.sections {
                            let (entry, source) =
                                build_section_entry(sec, &cur_presets, &cur_profiles, bp_id);
                            sources.insert(entry.id.clone(), source);
                            entries.push(entry);
                        }
                        song_sections.set(entries);
                        section_sources.set(sources);
                    }
                }
            });
        }
    };

    // Handle preset parent click — toggle expand, auto-select first sub-item
    let select_preset = {
        let signal = signal.clone();
        move |item_id: String| {
            let signal = signal.clone();

            // Find the item in the current preset list
            let items = manage_presets();
            let Some(item) = items.iter().find(|i| i.id == item_id).cloned() else {
                return;
            };

            // Always expand (never collapse on click — collapse via a dedicated toggle)
            let mut exp = expanded_ids();
            exp.insert(item_id.clone());
            expanded_ids.set(exp);

            // Select this parent
            selected_preset_id.set(Some(item_id.clone()));
            active_is_rig.set(item.is_rig);

            if item.is_rig {
                rig_id.set(Some(item_id.clone()));
                rig_scenes.set(item.sub_items.clone());
            } else {
                rig_id.set(None);
                rig_scenes.set(Vec::new());
            }

            // Auto-select and load first sub-item
            if let Some((first_sub_id, _)) = item.sub_items.first() {
                let sub_id = first_sub_id.clone();
                selected_sub_id.set(Some(sub_id.clone()));
                if item.is_rig {
                    active_scene_id.set(Some(sub_id.clone()));
                } else {
                    active_scene_id.set(None);
                }

                // Clear stale data so the grid doesn't remount with old engines
                canvas_engines.set(Vec::new());
                canvas_params.set(EngineParamLookup::new());

                let is_rig = item.is_rig;
                spawn(async move {
                    let result = if is_rig {
                        signal_ui::views::resolve_scene_engines(&signal, &item_id, &sub_id).await
                    } else {
                        signal_ui::views::resolve_layer_engines(&signal, &item_id, Some(&sub_id))
                            .await
                    };
                    match &result {
                        Some((engines, _)) => tracing::info!("resolve_engines: got {} engines", engines.len()),
                        None => tracing::warn!("resolve_engines: returned None for preset={item_id}, sub={sub_id}, is_rig={is_rig}"),
                    }
                    if let Some((engines, params)) = result {
                        canvas_engines.set(engines);
                        canvas_params.set(params);
                    }
                });
            } else {
                selected_sub_id.set(None);
                active_scene_id.set(None);
                canvas_engines.set(Vec::new());
                canvas_params.set(EngineParamLookup::new());
            }
        }
    };

    // Handle sub-item click — resolve that scene/variant's engines for the canvas
    let select_sub_item = {
        let signal = signal.clone();
        move |parent_id: String, sub_id: String, is_rig: bool| {
            let signal = signal.clone();
            selected_sub_id.set(Some(sub_id.clone()));
            if is_rig {
                active_scene_id.set(Some(sub_id.clone()));
            }
            // Clear stale data so the grid doesn't remount with old engines
            canvas_engines.set(Vec::new());
            canvas_params.set(EngineParamLookup::new());
            spawn(async move {
                let result = if is_rig {
                    signal_ui::views::resolve_scene_engines(&signal, &parent_id, &sub_id).await
                } else {
                    signal_ui::views::resolve_layer_engines(&signal, &parent_id, Some(&sub_id))
                        .await
                };
                if let Some((engines, params)) = result {
                    canvas_engines.set(engines);
                    canvas_params.set(params);
                }
            });
        }
    };

    // Assign a rig scene to the active section (Song mode) or patch (Profile mode).
    let assign_current_section = {
        let signal = signal.clone();
        move |parent_id: String, sub_id: String, _is_rig: bool| {
            let mode = manage_mode();
            let signal = signal.clone();

            if mode == ManageMode::Song {
                if let Some(sec_id) = selected_section_id() {
                    let new_source = SectionSource::RigScene {
                        rig_id: parent_id.clone().into(),
                        scene_id: sub_id.clone().into(),
                    };
                    // Update local source map
                    let mut sources = section_sources();
                    sources.insert(sec_id.clone(), new_source.clone());
                    section_sources.set(sources);
                    // Update display label
                    let preset_label = manage_presets()
                        .iter()
                        .find(|p| p.id == parent_id)
                        .and_then(|p| {
                            p.sub_items
                                .iter()
                                .find(|(sid, _)| *sid == sub_id)
                                .map(|(_, sname)| format!("{} / {}", p.name, sname))
                        });
                    let mut sections = song_sections();
                    if let Some(entry) = sections.iter_mut().find(|e| e.id == sec_id) {
                        entry.rig_scene_name = preset_label;
                        entry.profile_patch_name = None;
                    }
                    song_sections.set(sections);
                    // Persist
                    if let Some(song_id) = selected_song_id() {
                        spawn(async move {
                            signal
                                .songs()
                                .set_section_source(song_id, sec_id, new_source)
                                .await;
                        });
                    }
                }
            } else if mode == ManageMode::Profile {
                // Assign rig/scene to the currently selected profile patch
                if let (Some(prof_id), Some(patch_id_str)) = (selected_profile(), selected_patch())
                {
                    let mut prm = patch_rig_map();
                    prm.insert(patch_id_str.clone(), (parent_id.clone(), sub_id.clone()));
                    patch_rig_map.set(prm);
                    // Update display label for this patch
                    let new_label = manage_presets()
                        .iter()
                        .find(|p| p.id == parent_id)
                        .and_then(|p| {
                            p.sub_items
                                .iter()
                                .find(|(sid, _)| *sid == sub_id)
                                .map(|(_, sname)| format!("{} / {}", p.name, sname))
                        })
                        .unwrap_or_else(|| "Unlinked".to_string());
                    let mut lbls = patch_display_labels();
                    lbls.insert(patch_id_str.clone(), new_label);
                    patch_display_labels.set(lbls);
                    spawn(async move {
                        signal
                            .profiles()
                            .set_patch_preset(prof_id, patch_id_str, parent_id, sub_id)
                            .await;
                    });
                }
            }
            // Preset mode: no assignment
        }
    };

    // Navigate to a section's assigned preset/scene (without re-assigning).
    let navigate_to_section = {
        let signal = signal.clone();
        move |section_id: String| {
            let signal = signal.clone();
            selected_section_id.set(Some(section_id.clone()));
            let Some(source) = section_sources().get(&section_id).cloned() else {
                return;
            };
            // Resolve the rig_id + scene_id to navigate to
            let (rid_str, sid_str) = match &source {
                SectionSource::RigScene { rig_id, scene_id } => {
                    (rig_id.to_string(), scene_id.to_string())
                }
                SectionSource::Patch { patch_id } => {
                    let pid = patch_id.to_string();
                    // Select profile & patch in the sidebar
                    for prof in manage_profiles().iter() {
                        if prof.patches.iter().any(|(id, _)| *id == pid) {
                            let mut exp = expanded_profile_ids();
                            exp.insert(prof.id.clone());
                            expanded_profile_ids.set(exp);
                            selected_profile.set(Some(prof.id.clone()));
                            selected_patch.set(Some(pid.clone()));
                            break;
                        }
                    }
                    // Look up the patch's underlying rig/scene
                    match patch_rig_map().get(&pid).cloned() {
                        Some(pair) => pair,
                        None => return,
                    }
                }
            };
            // Set all signals synchronously so only one async resolve fires
            let is_rig = manage_presets()
                .iter()
                .find(|p| p.id == rid_str)
                .map_or(true, |p| p.is_rig);

            selected_preset_id.set(Some(rid_str.clone()));
            active_is_rig.set(is_rig);
            selected_sub_id.set(Some(sid_str.clone()));

            if is_rig {
                rig_id.set(Some(rid_str.clone()));
                active_scene_id.set(Some(sid_str.clone()));
                if let Some(item) = manage_presets().iter().find(|p| p.id == rid_str) {
                    rig_scenes.set(item.sub_items.clone());
                }
            } else {
                rig_id.set(None);
                active_scene_id.set(None);
                rig_scenes.set(Vec::new());
            }

            let mut exp = expanded_ids();
            exp.insert(rid_str.clone());
            expanded_ids.set(exp);
            canvas_engines.set(Vec::new());
            canvas_params.set(EngineParamLookup::new());
            spawn(async move {
                let result = if is_rig {
                    signal_ui::views::resolve_scene_engines(&signal, &rid_str, &sid_str).await
                } else {
                    signal_ui::views::resolve_layer_engines(&signal, &rid_str, Some(&sid_str)).await
                };
                if let Some((engines, params)) = result {
                    canvas_engines.set(engines);
                    canvas_params.set(params);
                }
            });
        }
    };

    let current_preset = selected_preset_id();
    let current_sub = selected_sub_id();
    let current_scene = active_scene_id();
    let scenes = rig_scenes();
    let is_rig_active = active_is_rig();

    let mode = manage_mode();

    // ── Breadcrumb: resolve current context chain ──
    let bc = breadcrumb::resolve_breadcrumbs(
        mode,
        &selected_song_id(),
        &songs(),
        &selected_section_id(),
        &song_sections(),
        &section_sources(),
        &selected_profile(),
        &selected_patch(),
        &manage_profiles(),
        &current_preset,
        &current_sub,
        &manage_presets(),
    );

    // Commit an inline rename (Rc-wrapped so it can be passed to render_profile_list)
    let commit_rename: Rc<dyn Fn()> = Rc::new({
        let signal = signal.clone();
        move || {
            let mut editing_target = editing_target;
            let target = editing_target();
            let new_name = editing_text().trim().to_string();
            editing_target.set(None);
            if new_name.is_empty() {
                return;
            }
            // Optimistic UI update: rename in local signal data immediately
            let mut manage_presets = manage_presets;
            let mut rig_scenes = rig_scenes;
            let mut manage_profiles = manage_profiles;
            let mut songs = songs;
            let mut active_song_name = active_song_name;
            let mut song_sections = song_sections;
            if let Some(ref t) = target {
                match t {
                    EditTarget::Rig { id } => {
                        let mut items = manage_presets();
                        if let Some(item) = items.iter_mut().find(|i| i.id == *id) {
                            item.name = new_name.clone();
                        }
                        manage_presets.set(items);
                    }
                    EditTarget::RigScene {
                        rig_id: _,
                        scene_id,
                    } => {
                        let mut scenes = rig_scenes();
                        if let Some((_, sname)) = scenes.iter_mut().find(|(sid, _)| sid == scene_id)
                        {
                            *sname = new_name.clone();
                        }
                        rig_scenes.set(scenes);
                        // Also update in manage_presets sub_items
                        let mut items = manage_presets();
                        for item in items.iter_mut() {
                            if let Some((_, sname)) =
                                item.sub_items.iter_mut().find(|(sid, _)| sid == scene_id)
                            {
                                *sname = new_name.clone();
                            }
                        }
                        manage_presets.set(items);
                    }
                    EditTarget::Profile { id } => {
                        let mut profs = manage_profiles();
                        if let Some(p) = profs.iter_mut().find(|p| p.id == *id) {
                            p.name = new_name.clone();
                        }
                        manage_profiles.set(profs);
                    }
                    EditTarget::Patch {
                        profile_id,
                        patch_id,
                    } => {
                        let mut profs = manage_profiles();
                        if let Some(p) = profs.iter_mut().find(|p| p.id == *profile_id) {
                            if let Some((_, pname)) =
                                p.patches.iter_mut().find(|(pid, _)| pid == patch_id)
                            {
                                *pname = new_name.clone();
                            }
                        }
                        manage_profiles.set(profs);
                    }
                    EditTarget::Song { id } => {
                        let mut s = songs();
                        if let Some(song) = s.iter_mut().find(|s| s.id == *id) {
                            song.name = new_name.clone();
                        }
                        songs.set(s);
                        // Update active song name if this is the selected song
                        if selected_song_id().as_deref() == Some(id.as_str()) {
                            active_song_name.set(new_name.clone());
                        }
                    }
                    EditTarget::Section {
                        song_id: _,
                        section_id,
                    } => {
                        let mut secs = song_sections();
                        if let Some(sec) = secs.iter_mut().find(|s| s.id == *section_id) {
                            sec.name = new_name.clone();
                        }
                        song_sections.set(secs);
                    }
                }
            }
            // Persist to storage async
            let signal = signal.clone();
            if let Some(target) = target {
                spawn(async move {
                    match target {
                        EditTarget::Rig { id } => {
                            let _ = signal.rigs().rename(id, &new_name).await;
                        }
                        EditTarget::RigScene { rig_id, scene_id } => {
                            let _ = signal
                                .rigs()
                                .update_scene(rig_id, scene_id, |s| s.name = new_name.clone())
                                .await;
                        }
                        EditTarget::Profile { id } => {
                            let _ = signal.profiles().rename(id, &new_name).await;
                        }
                        EditTarget::Patch {
                            profile_id,
                            patch_id,
                        } => {
                            let _ = signal
                                .profiles()
                                .update_patch(profile_id, patch_id, |p| p.name = new_name.clone())
                                .await;
                        }
                        EditTarget::Song { id } => {
                            let _ = signal.songs().rename(id, &new_name).await;
                        }
                        EditTarget::Section {
                            song_id,
                            section_id,
                        } => {
                            let _ = signal
                                .songs()
                                .update_section(song_id, section_id, |s| s.name = new_name.clone())
                                .await;
                        }
                    }
                });
            }
        }
    });

    // Navigate the grid to a patch's underlying rig/scene.
    // Shared by Profile mode (right panel) and Song mode (right panel profiles).
    let patch_navigate_cb: Rc<dyn Fn(String, String)> = Rc::new({
        let signal = signal.clone();
        move |_prof_id: String, patch_id: String| {
            let mut selected_preset_id = selected_preset_id;
            let mut active_is_rig = active_is_rig;
            let mut rig_id = rig_id;
            let mut selected_sub_id = selected_sub_id;
            let mut active_scene_id = active_scene_id;
            let mut expanded_ids = expanded_ids;
            let mut rig_scenes = rig_scenes;
            let mut canvas_engines = canvas_engines;
            let mut canvas_params = canvas_params;
            if let Some((rid, sid)) = patch_rig_map().get(&patch_id).cloned() {
                if rid.is_empty() {
                    return;
                }
                let is_rig = manage_presets()
                    .iter()
                    .find(|p| p.id == rid)
                    .map_or(true, |p| p.is_rig);

                selected_preset_id.set(Some(rid.clone()));
                active_is_rig.set(is_rig);
                selected_sub_id.set(Some(sid.clone()));

                if is_rig {
                    rig_id.set(Some(rid.clone()));
                    active_scene_id.set(Some(sid.clone()));
                    if let Some(item) = manage_presets().iter().find(|p| p.id == rid) {
                        rig_scenes.set(item.sub_items.clone());
                    }
                } else {
                    rig_id.set(None);
                    active_scene_id.set(None);
                    rig_scenes.set(Vec::new());
                }

                let mut exp = expanded_ids();
                exp.insert(rid.clone());
                expanded_ids.set(exp);
                canvas_engines.set(Vec::new());
                canvas_params.set(EngineParamLookup::new());
                let signal = signal.clone();
                spawn(async move {
                    let result = if is_rig {
                        signal_ui::views::resolve_scene_engines(&signal, &rid, &sid).await
                    } else {
                        signal_ui::views::resolve_layer_engines(&signal, &rid, Some(&sid)).await
                    };
                    if let Some((engines, params)) = result {
                        canvas_engines.set(engines);
                        canvas_params.set(params);
                    }
                });
            }
        }
    });

    // Song mode: assign section → patch source, then navigate grid.
    let song_patch_cb: Rc<dyn Fn(String, String)> = Rc::new({
        let signal = signal.clone();
        let patch_navigate_cb = patch_navigate_cb.clone();
        move |prof_id: String, patch_id: String| {
            let mut section_sources = section_sources;
            let mut song_sections = song_sections;
            // 1) Assign section → Patch source
            if let (Some(sec_id), Some(song_id)) = (selected_section_id(), selected_song_id()) {
                let new_source = SectionSource::Patch {
                    patch_id: patch_id.clone().into(),
                };
                let mut sources = section_sources();
                sources.insert(sec_id.clone(), new_source.clone());
                section_sources.set(sources);
                let label = manage_profiles().iter().find_map(|prof| {
                    if prof.id == prof_id {
                        prof.patches
                            .iter()
                            .find(|(pid, _)| *pid == patch_id)
                            .map(|(_, pname)| format!("{} / {}", prof.name, pname))
                    } else {
                        None
                    }
                });
                let mut sections = song_sections();
                if let Some(entry) = sections.iter_mut().find(|e| e.id == sec_id) {
                    entry.profile_patch_name = label;
                    entry.rig_scene_name = None;
                }
                song_sections.set(sections);
                let signal = signal.clone();
                spawn(async move {
                    signal
                        .songs()
                        .set_section_source(song_id, sec_id, new_source)
                        .await;
                });
            }
            // 2) Navigate grid to the patch's rig/scene
            patch_navigate_cb(prof_id, patch_id);
        }
    });

    // ── Surgical refresh callbacks (replace refresh_trigger) ──

    // Re-fetch presets list after creating a new rig
    let reload_presets = {
        let signal = signal.clone();
        move || {
            let signal = signal.clone();
            let rt = rig_type();
            spawn(async move {
                let rigs = signal.rigs().list().await.unwrap_or_default();
                let filtered: Vec<_> = rigs
                    .into_iter()
                    .filter(|r| r.rig_type.map_or(false, |t| t == rt))
                    .collect();
                let et = rig_type_to_engine_type(rt);
                let all_layers = signal.layers().list().await.unwrap_or_default();
                let matching_layers: Vec<_> = all_layers
                    .into_iter()
                    .filter(|l| l.engine_type == et)
                    .collect();
                let mut items: Vec<ManagePresetItem> = Vec::new();
                for r in &filtered {
                    items.push(ManagePresetItem {
                        id: r.id.to_string(),
                        name: r.name.clone(),
                        is_rig: true,
                        sub_items: r
                            .variants
                            .iter()
                            .map(|v| (v.id.to_string(), v.name.clone()))
                            .collect(),
                    });
                }
                for l in &matching_layers {
                    items.push(ManagePresetItem {
                        id: l.id.to_string(),
                        name: l.name.clone(),
                        is_rig: false,
                        sub_items: l
                            .variants
                            .iter()
                            .map(|v| (v.id.to_string(), v.name.clone()))
                            .collect(),
                    });
                }
                manage_presets.set(items);
            });
        }
    };

    // Re-fetch scenes for the selected rig after adding a scene
    let reload_scenes = {
        let signal = signal.clone();
        move || {
            let signal = signal.clone();
            if let Some(rid) = rig_id() {
                spawn(async move {
                    if let Some(rig) = signal.rigs().load(rid.as_str()).await.ok().flatten() {
                        let scenes: Vec<(String, String)> = rig
                            .variants
                            .iter()
                            .map(|v| (v.id.to_string(), v.name.clone()))
                            .collect();
                        rig_scenes.set(scenes.clone());
                        // Also update manage_presets sub_items
                        let mut items = manage_presets();
                        if let Some(item) = items.iter_mut().find(|i| i.id == rid) {
                            item.sub_items = scenes;
                        }
                        manage_presets.set(items);
                    }
                });
            }
        }
    };

    // Re-fetch profiles after creating a patch
    let reload_profiles = {
        let signal = signal.clone();
        move || {
            let signal = signal.clone();
            let rt = rig_type();
            spawn(async move {
                let rigs = signal.rigs().list().await.unwrap_or_default();
                let rig_id_set: std::collections::HashSet<String> = rigs
                    .iter()
                    .filter(|r| r.rig_type.map_or(false, |t| t == rt))
                    .map(|r| r.id.to_string())
                    .collect();
                let all_profiles = signal.profiles().list().await.unwrap_or_default();
                let matching: Vec<_> = all_profiles
                    .into_iter()
                    .filter(|p| {
                        p.patches.iter().any(|patch| match &patch.target {
                            signal::profile::PatchTarget::RigScene { rig_id, .. } => {
                                rig_id_set.contains(rig_id.as_str())
                            }
                            _ => true,
                        })
                    })
                    .collect();
                manage_profiles.set(
                    matching
                        .iter()
                        .map(|p| ManageProfileItem {
                            id: p.id.to_string(),
                            name: p.name.clone(),
                            patches: p
                                .patches
                                .iter()
                                .map(|patch| (patch.id.to_string(), patch.name.clone()))
                                .collect(),
                        })
                        .collect(),
                );
                // Rebuild patch_rig_map and display labels
                let preset_items = manage_presets();
                let mut prm = std::collections::HashMap::new();
                let mut labels = std::collections::HashMap::new();
                for p in &matching {
                    for patch in &p.patches {
                        let (rig_str, scene_str) = match &patch.target {
                            signal::profile::PatchTarget::RigScene { rig_id, scene_id } => {
                                (rig_id.to_string(), scene_id.to_string())
                            }
                            _ => (String::new(), String::new()),
                        };
                        if !rig_str.is_empty() {
                            prm.insert(patch.id.to_string(), (rig_str.clone(), scene_str.clone()));
                        }
                        let label = preset_items
                            .iter()
                            .find(|pi| pi.id == rig_str)
                            .and_then(|pi| {
                                pi.sub_items
                                    .iter()
                                    .find(|(sid, _)| *sid == scene_str)
                                    .map(|(_, sname)| format!("{} / {}", pi.name, sname))
                            })
                            .unwrap_or_else(|| "Unlinked".to_string());
                        labels.insert(patch.id.to_string(), label);
                    }
                }
                patch_rig_map.set(prm);
                patch_display_labels.set(labels);
            });
        }
    };

    // Re-fetch songs list after creating a song
    let reload_songs = {
        let signal = signal.clone();
        move || {
            let signal = signal.clone();
            let sl_id = selected_setlist_id();
            spawn(async move {
                if sl_id == "all" {
                    // "All Songs" shows every song in the database
                    let all = signal.songs().list().await.unwrap_or_default();
                    songs.set(
                        all.iter()
                            .map(|s| SongEntry {
                                id: s.id.to_string(),
                                name: s.name.clone(),
                                section_count: s.sections.len(),
                                duration_display: None,
                            })
                            .collect(),
                    );
                } else if let Some(sl) = signal.setlists().load(sl_id).await.ok().flatten() {
                    let mut song_list = Vec::new();
                    for entry in &sl.entries {
                        let sid = entry.song_id.to_string();
                        if let Some(song) = signal.songs().load(sid).await.ok().flatten() {
                            song_list.push(SongEntry {
                                id: song.id.to_string(),
                                name: song.name.clone(),
                                section_count: song.sections.len(),
                                duration_display: None,
                            });
                        }
                    }
                    songs.set(song_list);
                }
            });
        }
    };

    // Re-fetch setlist options after creating a setlist
    let reload_setlists = {
        let signal = signal.clone();
        move || {
            let signal = signal.clone();
            spawn(async move {
                let all_setlists = signal.setlists().list().await.unwrap_or_default();
                let mut opts: Vec<(String, String)> =
                    vec![("all".to_string(), "All Songs".to_string())];
                for sl in &all_setlists {
                    opts.push((sl.id.to_string(), sl.name.clone()));
                }
                setlist_options.set(opts);
            });
        }
    };

    // Re-fetch sections for the current song after adding a section
    let reload_sections = {
        let signal = signal.clone();
        move || {
            let signal = signal.clone();
            if let Some(song_id) = selected_song_id() {
                spawn(async move {
                    if let Some(song) = signal.songs().load(song_id.as_str()).await.ok().flatten() {
                        let cur_presets = manage_presets();
                        let cur_profiles = manage_profiles();
                        let mut entries = Vec::new();
                        let mut sources = std::collections::HashMap::new();
                        let bp_id = song.metadata.base_profile_id.as_deref();
                        for sec in &song.sections {
                            let (entry, source) =
                                build_section_entry(sec, &cur_presets, &cur_profiles, bp_id);
                            sources.insert(entry.id.clone(), source);
                            entries.push(entry);
                        }
                        song_sections.set(entries);
                        section_sources.set(sources);
                    }
                });
            }
        }
    };

    // Create Rig with NDSP auto-detect fallback:
    // if current REAPER patch contains a known Archetype X plugin, seed a
    // one-engine rig that references module presets for that plugin.
    let create_rig_with_autodetect: Rc<dyn Fn(signal::rig::RigType)> = {
        let signal = signal.clone();
        let mut on_done = reload_presets.clone();
        Rc::new(move |rt: signal::rig::RigType| {
            let signal = signal.clone();
            let mut on_done = on_done.clone();
            spawn(async move {
                let mut detected_plugin: Option<String> = None;
                if rt == signal::rig::RigType::Guitar {
                    if let Some(applier) = crate::daw_registry::signal_reaper_applier() {
                        if let Ok(Some(bytes)) = applier.capture_current_patch().await {
                            if let Ok(rfxchain) = String::from_utf8(bytes) {
                                detected_plugin = signal::defaults::archetype_ndsp::detect_archetype_plugin_in_rfxchain(&rfxchain);
                            }
                        }
                    }
                }

                if let Some(plugin_name) = detected_plugin {
                    if let Some(module_keys) =
                        signal::defaults::archetype_ndsp::archetype_module_seed_keys(&plugin_name)
                    {
                        let label = signal::defaults::archetype_ndsp::archetype_label(&plugin_name);
                        let short_label = label.replace("Archetype ", "");

                        let mut layer_snapshot = signal::layer::LayerSnapshot::new(
                            signal::layer::LayerSnapshotId::new(),
                            "Default",
                        );
                        for key in &module_keys {
                            layer_snapshot = layer_snapshot
                                .with_module(signal::layer::ModuleRef::new(signal::seed_id(key)));
                        }

                        let layer = signal::layer::Layer::new(
                            signal::layer::LayerId::new(),
                            format!("{short_label} Layer"),
                            signal::EngineType::Guitar,
                            layer_snapshot,
                        );
                        let layer = match signal.layers().save(layer).await {
                            Ok(l) => l,
                            Err(e) => {
                                tracing::warn!("NDSP auto-layer save failed, falling back: {e}");
                                let rig = signal::rig::Rig::new(
                                    signal::rig::RigId::new(),
                                    "New Rig",
                                    vec![],
                                    signal::rig::RigScene::new(
                                        signal::rig::RigSceneId::new(),
                                        "Default",
                                    ),
                                )
                                .with_rig_type(rt);
                                let _ = signal.rigs().save(rig).await;
                                on_done();
                                return;
                            }
                        };

                        let engine_scene = signal::engine::EngineScene::new(
                            signal::engine::EngineSceneId::new(),
                            "Default",
                        )
                        .with_layer(signal::engine::LayerSelection::new(
                            layer.id.clone(),
                            layer.default_variant_id.clone(),
                        ));
                        let engine = signal::engine::Engine::new(
                            signal::engine::EngineId::new(),
                            format!("{short_label} Engine"),
                            signal::EngineType::Guitar,
                            vec![layer.id.clone()],
                            engine_scene,
                        );
                        let engine = match signal.engines().save(engine).await {
                            Ok(e) => e,
                            Err(err) => {
                                tracing::warn!(
                                    "NDSP auto-engine save failed, falling back: {err}"
                                );
                                let rig = signal::rig::Rig::new(
                                    signal::rig::RigId::new(),
                                    "New Rig",
                                    vec![],
                                    signal::rig::RigScene::new(
                                        signal::rig::RigSceneId::new(),
                                        "Default",
                                    ),
                                )
                                .with_rig_type(rt);
                                let _ = signal.rigs().save(rig).await;
                                on_done();
                                return;
                            }
                        };

                        let rig_scene = signal::rig::RigScene::new(
                            signal::rig::RigSceneId::new(),
                            "Default",
                        )
                        .with_engine(signal::rig::EngineSelection::new(
                                    engine.id.clone(),
                                    engine.default_variant_id.clone(),
                                ));
                        let rig = signal::rig::Rig::new(
                            signal::rig::RigId::new(),
                            format!("New Rig ({short_label})"),
                            vec![engine.id],
                            rig_scene,
                        )
                        .with_rig_type(rt);
                        let _ = signal.rigs().save(rig).await;
                        tracing::info!("Created NDSP auto-detected rig from plugin: {plugin_name}");
                        on_done();
                        return;
                    }
                }

                // Fallback: existing blank rig behavior.
                let rig = signal::rig::Rig::new(
                    signal::rig::RigId::new(),
                    "New Rig",
                    vec![],
                    signal::rig::RigScene::new(signal::rig::RigSceneId::new(), "Default"),
                )
                .with_rig_type(rt);
                let _ = signal.rigs().save(rig).await;
                tracing::info!("Created new rig");
                on_done();
            });
        })
    };

    rsx! {
            div { class: "flex flex-col h-full overflow-hidden",
                // ── Top bar ──
                {top_bar::render_top_bar(
                    mode,
                    manage_mode,
                    rig_type,
                    rig_id,
                    &current_scene,
                    &scenes,
                    is_rig_active,
                    reload_scenes.clone(),
                    editing_target,
                    editing_text,
                    commit_rename.clone(),
                    {
                        let select_sub_item = select_sub_item.clone();
                        move |p: String, s: String, r: bool| { let mut f = select_sub_item.clone(); f(p, s, r); }
                    },
                    {
                        let assign_current_section = assign_current_section.clone();
                        move |p: String, s: String, r: bool| { let mut f = assign_current_section.clone(); f(p, s, r); }
                    },
                    signal.clone(),
                )}

                // ── Breadcrumb context row ──
                {breadcrumb::render_breadcrumb(mode, &bc)}

                // ── Body: mode-dependent panel layout ──
                div { class: "flex flex-1 min-h-0 overflow-hidden",
                    // ── Left panel: Presets + (Song mode) Profiles ──
                    div { class: "w-56 flex-shrink-0 border-r border-border flex flex-col min-h-0 bg-zinc-950/40",
                        {preset_list::render_preset_list(
                            manage_presets,
                            expanded_ids,
                            &current_preset,
                            &current_sub,
                            rig_type,
                            reload_presets.clone(),
                            create_rig_with_autodetect.clone(),
                            editing_target,
                            editing_text,
                            commit_rename.clone(),
                            ctx_target,
                            ctx_pos,
                            select_preset.clone(),
                            {
                                let select_sub_item = select_sub_item.clone();
                                move |p: String, s: String, r: bool| { let mut f = select_sub_item.clone(); f(p, s, r); }
                            },
                            {
                                let assign_current_section = assign_current_section.clone();
                                move |p: String, s: String, r: bool| { let mut f = assign_current_section.clone(); f(p, s, r); }
                            },
                        )}

                        // In Song mode, show Profiles below Presets
                        if mode == ManageMode::Song {
                            div { class: "flex-1 min-h-0 flex flex-col border-t border-border",
                                div { class: "px-3 py-1.5 border-b border-border flex-shrink-0 flex items-center justify-between",
                                    h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Profiles" }
                                    {
                                        let mut cb = reload_profiles.clone();
                                        rsx! {
                                            signal_ui::components::CreateProfileButton {
                                                on_created: move |_| { cb(); },
                                            }
                                        }
                                    }
                                }
                                div { class: "flex-1 overflow-y-auto",
                                    {profile_list::render_profile_list(manage_profiles, expanded_profile_ids, selected_profile, selected_patch, Some(song_patch_cb.clone()), patch_display_labels, signal.clone(), reload_profiles.clone(), editing_target, editing_text, commit_rename.clone(), ctx_target, ctx_pos)}
                                }
                            }
                        }
                    }

                    // ── Center: Rig preset canvas ──
                    div { class: "flex-[3] min-w-0 flex flex-col overflow-hidden",
                        if selected_preset_id().is_some() {
                            {
                                let grid_key = format!(
                                    "{}-{}",
                                    selected_preset_id().unwrap_or_default(),
                                    selected_sub_id().unwrap_or_default(),
                                );
                                let ce = canvas_engines();
                                let cp = canvas_params();
                                let grid_slots = engines_to_grid_slots(&ce, &cp);
                                tracing::info!(
                                    "render: engines={}, params={}, grid_slots={}",
                                    ce.len(), cp.len(), grid_slots.len()
                                );
                                let signal = signal.clone();
                                let signal2 = signal.clone();
                                let signal3 = signal.clone();
                                let signal4 = signal.clone();
                                let signal5 = signal.clone();
                                let signal6 = signal.clone();
                                rsx! {
                                    RigGridPanel {
                                        key: "{grid_key}",
                                        initial_slots: grid_slots,
                                        on_save: move |slot: signal_ui::components::GridSlot| {
                                            let signal = signal.clone();
                                            let bt = slot.block_type;
                                            let pid = slot.preset_id.clone().unwrap_or_default();
                                            let sid = slot.snapshot_id.clone();
                                            let block = slot_to_block(&slot);
                                            spawn(async move {
                                                let _ = signal.block_presets().update_snapshot_params(
                                                    bt,
                                                    pid,
                                                    sid.unwrap_or_default(),
                                                    block,
                                                ).await;
                                            });
                                        },
                                        on_save_as_new: move |(slot, name): (signal_ui::components::GridSlot, String)| {
                                            let signal = signal2.clone();
                                            let bt = slot.block_type;
                                            let block = slot_to_block(&slot);
                                            spawn(async move {
                                                let _ = signal.block_presets().create(name, bt, block).await;
                                            });
                                        },
                                        on_save_block_snapshot: move |slot: signal_ui::components::GridSlot| {
                                            let signal = signal3.clone();
                                            let bt = slot.block_type;
                                            let pid = slot.preset_id.clone().unwrap_or_default();
                                            let block = slot_to_block(&slot);
                                            let snap = signal::Snapshot::new(signal::SnapshotId::new(), "Snapshot", block);
                                            spawn(async move {
                                                if let Ok(presets) = signal.block_presets().list(bt).await {
                                                    if let Some(mut preset) = presets.into_iter().find(|p| p.id().to_string() == pid) {
                                                        preset.add_snapshot(snap);
                                                        let _ = signal.block_presets().save(preset).await;
                                                    }
                                                }
                                            });
                                        },
                                        on_save_block_snapshot_as: move |(slot, name): (signal_ui::components::GridSlot, String)| {
                                            let signal = signal4.clone();
                                            let bt = slot.block_type;
                                            let pid = slot.preset_id.clone().unwrap_or_default();
                                            let block = slot_to_block(&slot);
                                            let snap = signal::Snapshot::new(signal::SnapshotId::new(), name, block);
                                            spawn(async move {
                                                if let Ok(presets) = signal.block_presets().list(bt).await {
                                                    if let Some(mut preset) = presets.into_iter().find(|p| p.id().to_string() == pid) {
                                                        preset.add_snapshot(snap);
                                                        let _ = signal.block_presets().save(preset).await;
                                                    }
                                                }
                                            });
                                        },
                                        on_save_module_preset_as: move |(slots, name, mt): (Vec<signal_ui::components::GridSlot>, String, signal::ModuleType)| {
                                            let signal = signal5.clone();
                                            let module = slots_to_module(&slots);
                                            let snapshot = signal::ModuleSnapshot::new(signal::ModuleSnapshotId::new(), "Default", module);
                                            let preset = signal::ModulePreset::new(signal::ModulePresetId::new(), name, mt, snapshot, vec![]);
                                            spawn(async move {
                                                let _ = signal.module_presets().save(preset).await;
                                            });
                                        },
                                        on_save_module_snapshot_as: move |(slots, name, _mt): (Vec<signal_ui::components::GridSlot>, String, signal::ModuleType)| {
                                            let signal = signal6.clone();
                                            let module = slots_to_module(&slots);
                                            let snap = signal::ModuleSnapshot::new(signal::ModuleSnapshotId::new(), name, module);
                                            // For now, create a new preset with this snapshot.
                                            // TODO: When module source tracking is added, add snapshot to existing preset instead.
                                            let preset = signal::ModulePreset::new(
                                                signal::ModulePresetId::new(),
                                                "Module Preset",
                                                _mt,
                                                snap,
                                                vec![],
                                            );
                                            spawn(async move {
                                                let _ = signal.module_presets().save(preset).await;
                                            });
                                        },
                                    }
                                }
                            }
                        } else {
                            div { class: "flex items-center justify-center h-full",
                                p { class: "text-sm text-muted-foreground", "Select a preset" }
                            }
                        }
                    }

                    // ── Right panel: Profile mode ──
                    if mode == ManageMode::Profile {
                        div { class: "w-72 flex-shrink-0 border-l border-border flex flex-col min-h-0 bg-zinc-950/40",
                            div { class: "px-3 py-1.5 border-b border-border flex-shrink-0 flex items-center justify-between",
                                h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Profiles" }
                                {
                                    let mut cb = reload_profiles.clone();
                                    rsx! {
                                        signal_ui::components::CreateProfileButton {
                                            on_created: move |_| { cb(); },
                                        }
                                    }
                                }
                            }
                            div { class: "flex-1 overflow-y-auto",
                                {profile_list::render_profile_list(manage_profiles, expanded_profile_ids, selected_profile, selected_patch, Some(patch_navigate_cb.clone()), patch_display_labels, signal.clone(), reload_profiles.clone(), editing_target, editing_text, commit_rename.clone(), ctx_target, ctx_pos)}
                            }
                        }
                    }

                    // ── Right panel: Song mode ──
                    if mode == ManageMode::Song {
                        {song_panel::render_song_panel(
                            song_sections,
                            selected_section_id,
                            active_song_name,
                            selected_song_id,
                            songs,
                            setlist_options,
                            selected_setlist_id,
                            manage_profiles,
                            active_base_profile_name,
                            reload_sections.clone(),
                            reload_songs.clone(),
                            {
                                let mut rl_setlists = reload_setlists.clone();
                                let mut rl_songs = reload_songs.clone();
                                move || { rl_setlists(); rl_songs(); }
                            },
                            {
                                let load = load_song_sections.clone();
                                move || {
                                    // Reload sections to reflect remapped patches
                                    if let Some(sid) = selected_song_id() {
                                        let mut load = load.clone();
                                        load(sid);
                                    }
                                }
                            },
                            editing_target,
                            editing_text,
                            commit_rename.clone(),
                            ctx_target,
                            ctx_pos,
                            navigate_to_section.clone(),
                            load_song_sections.clone(),
                            change_setlist.clone(),
                            signal.clone(),
                        )}
                    }
                }

                // ── Context menu overlay ──
                if let Some(ref target) = ctx_target() {
                    // Click-away backdrop
                    div {
                        class: "fixed inset-0 z-40",
                        onclick: move |_| { ctx_target.set(None); },
                    }
                    {
                        let (cx, cy) = ctx_pos();
                        let style = format!("position:fixed;left:{cx}px;top:{cy}px;z-index:50");
                        let target = target.clone();
                        let signal = signal.clone();
                        let label = match &target {
                            EditTarget::Rig { .. } => "Rig",
                            EditTarget::RigScene { .. } => "Scene",
                            EditTarget::Profile { .. } => "Profile",
                            EditTarget::Patch { .. } => "Patch",
                            EditTarget::Song { .. } => "Song",
                            EditTarget::Section { .. } => "Section",
                        };
                        rsx! {
                            div {
                                class: "bg-zinc-800 border border-zinc-600 rounded shadow-xl py-1 min-w-[140px]",
                                style: "{style}",
                                // Rename action
                                button {
                                    class: "w-full text-left px-3 py-1.5 text-xs text-zinc-200 hover:bg-zinc-700",
                                    onclick: move |_| {
                                        let target = ctx_target().unwrap();
                                        // Trigger inline rename for this entity
                                        editing_target.set(Some(target));
                                        // editing_text needs to be set — but we don't have the name here.
                                        // The user will see the empty input and can type. This is acceptable.
                                        ctx_target.set(None);
                                    },
                                    "Rename"
                                }
                                // Assign Preset action (only for Patch targets)
                                if let EditTarget::Patch { ref profile_id, ref patch_id } = target {
                                    {
                                        let profile_id = profile_id.clone();
                                        let patch_id = patch_id.clone();
                                        rsx! {
                                            button {
                                                class: "w-full text-left px-3 py-1.5 text-xs text-amber-400 hover:bg-zinc-700",
                                                onclick: move |_| {
                                                    assign_preset_target.set(Some((profile_id.clone(), patch_id.clone())));
                                                    ctx_target.set(None);
                                                },
                                                "Assign Preset..."
                                            }
                                        }
                                    }
                                }
                                // Delete action
                                button {
                                    class: "w-full text-left px-3 py-1.5 text-xs text-red-400 hover:bg-zinc-700",
                                    onclick: move |_| {
                                        let target = target.clone();
                                        let signal = signal.clone();
                                        ctx_target.set(None);
                                        // Optimistic UI: remove from local state immediately
                                        match &target {
                                            EditTarget::Rig { id } => {
                                                let mut items = manage_presets();
                                                items.retain(|i| i.id != *id);
                                                manage_presets.set(items);
                                                if selected_preset_id().as_deref() == Some(id.as_str()) {
                                                    selected_preset_id.set(None);
                                                    selected_sub_id.set(None);
                                                    rig_id.set(None);
                                                    active_scene_id.set(None);
                                                    rig_scenes.set(Vec::new());
                                                    canvas_engines.set(Vec::new());
                                                    canvas_params.set(EngineParamLookup::new());
                                                }
                                            }
                                            EditTarget::RigScene { rig_id: rid, scene_id } => {
                                                let mut scenes = rig_scenes();
                                                scenes.retain(|(sid, _)| sid != scene_id);
                                                rig_scenes.set(scenes);
                                                let mut items = manage_presets();
                                                if let Some(item) = items.iter_mut().find(|i| i.id == *rid) {
                                                    item.sub_items.retain(|(sid, _)| sid != scene_id);
                                                }
                                                manage_presets.set(items);
                                                if selected_sub_id().as_deref() == Some(scene_id.as_str()) {
                                                    selected_sub_id.set(None);
                                                    active_scene_id.set(None);
                                                    canvas_engines.set(Vec::new());
                                                    canvas_params.set(EngineParamLookup::new());
                                                }
                                            }
                                            EditTarget::Profile { id } => {
                                                let mut profs = manage_profiles();
                                                profs.retain(|p| p.id != *id);
                                                manage_profiles.set(profs);
                                                if selected_profile().as_deref() == Some(id.as_str()) {
                                                    selected_profile.set(None);
                                                    selected_patch.set(None);
                                                }
                                            }
                                            EditTarget::Patch { profile_id, patch_id } => {
                                                let mut profs = manage_profiles();
                                                if let Some(p) = profs.iter_mut().find(|p| p.id == *profile_id) {
                                                    p.patches.retain(|(pid, _)| pid != patch_id);
                                                }
                                                manage_profiles.set(profs);
                                                if selected_patch().as_deref() == Some(patch_id.as_str()) {
                                                    selected_patch.set(None);
                                                }
                                            }
                                            EditTarget::Song { id } => {
                                                let mut s = songs();
                                                s.retain(|s| s.id != *id);
                                                songs.set(s);
                                                if selected_song_id().as_deref() == Some(id.as_str()) {
                                                    selected_song_id.set(None);
                                                    song_sections.set(Vec::new());
                                                    active_song_name.set("Songs".to_string());
                                                }
                                            }
                                            EditTarget::Section { song_id: _, section_id } => {
                                                let mut secs = song_sections();
                                                secs.retain(|s| s.id != *section_id);
                                                song_sections.set(secs);
                                                if selected_section_id().as_deref() == Some(section_id.as_str()) {
                                                    selected_section_id.set(None);
                                                }
                                            }
                                        }
                                        // Persist delete async
                                        spawn(async move {
                                            match target {
                                                EditTarget::Rig { id } => {
                                                    let _ = signal.rigs().delete(id).await;
                                                }
                                                EditTarget::RigScene { rig_id, scene_id } => {
                                                    let _ = signal.rigs().remove_scene(rig_id, scene_id).await;
                                                }
                                                EditTarget::Profile { id } => {
                                                    let _ = signal.profiles().delete(id).await;
                                                }
                                                EditTarget::Patch { profile_id, patch_id } => {
                                                    let _ = signal.profiles().remove_patch(profile_id, patch_id).await;
                                                }
                                                EditTarget::Song { id } => {
                                                    let _ = signal.songs().delete(id).await;
                                                }
                                                EditTarget::Section { song_id, section_id } => {
                                                    let _ = signal.songs().remove_section(song_id, section_id).await;
                                                }
                                            }
                                        });
                                    },
                                    "Delete {label}"
                                }
                            }
                        }
                    }
                }

                // ── Assign Preset browser dialog ──
                if let Some((ref profile_id, ref patch_id)) = assign_preset_target() {
                    {
                        let profile_id = profile_id.clone();
                        let patch_id = patch_id.clone();
                        let signal = signal.clone();
                        rsx! {
                            SignalBrowserDialog {
                                on_close: move |_| {
                                    assign_preset_target.set(None);
                                },
                                on_assign: move |assignment: BrowserAssignment| {
                                    let signal = signal.clone();
                                    let profile_id = profile_id.clone();
                                    let patch_id = patch_id.clone();
                                    let target = match assignment {
                                        BrowserAssignment::RigScene { rig_id, scene_id } => {
                                            signal::profile::PatchTarget::RigScene {
                                                rig_id: rig_id.into(),
                                                scene_id: scene_id.into(),
                                            }
                                        }
                                        BrowserAssignment::BlockSnapshot { preset_id, snapshot_id }
                                        | BrowserAssignment::BlockPresetDefault { preset_id, snapshot_id } => {
                                            signal::profile::PatchTarget::BlockSnapshot {
                                                preset_id: preset_id.into(),
                                                snapshot_id: snapshot_id.into(),
                                            }
                                        }
                                    };
                                    spawn(async move {
                                        let _ = signal.profiles().set_patch_target(
                                            profile_id,
                                            patch_id,
                                            target,
                                        ).await;
                                    });
                                    assign_preset_target.set(None);
                                },
                            }
                        }
                    }
                }
        }
    }
}
