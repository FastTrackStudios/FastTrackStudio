use dioxus::prelude::*;
use std::rc::Rc;

use lumen_blocks::components::dropdown::{
    Dropdown, DropdownContent, DropdownItem, DropdownTrigger,
};

use super::{ManagePresetItem, ManageProfileItem};

/// Which editing mode the Manage tab is in.
#[derive(Clone, Copy, PartialEq, Eq)]
enum ManageMode {
    Preset,
    Profile,
    Song,
}

#[component]
pub(crate) fn SignalManageTab() -> Element {
    let signal = signal_ui::use_signal_service();
    use signal::rig::RigType;
    use signal::song::SectionSource;
    use signal_ui::views::{
        engines_to_grid_slots, rig_type_to_engine_type, EngineFlowData, EngineParamLookup,
        RigGridPanel, SectionEntry, SongEditor, SongEntry,
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
    let mut selected_setlist_id = use_signal(|| "all".to_string());

    let mut songs = use_signal(Vec::<SongEntry>::new);
    let mut selected_song_id = use_signal(|| None::<String>);
    let mut song_sections = use_signal(Vec::<SectionEntry>::new);
    let mut selected_section_id = use_signal(|| None::<String>);
    let mut active_song_name = use_signal(|| "Songs".to_string());

    // Maps section_id → SectionSource for navigation and assignment
    let mut section_sources = use_signal(std::collections::HashMap::<String, SectionSource>::new);
    // Maps patch_id → (rig_id, scene_id) so section navigation via Patch doesn't need async
    let mut patch_rig_map = use_signal(std::collections::HashMap::<String, (String, String)>::new);
    // Maps patch_id → display label ("RigName / SceneName") for the profile browser
    let mut patch_display_labels = use_signal(std::collections::HashMap::<String, String>::new);

    // Track whether the active selection is a rig (true) or layer (false)
    let mut active_is_rig = use_signal(|| true);
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
        let entry = SectionEntry {
            id: sec.id.to_string(),
            name: sec.name.clone(),
            rig_scene_name,
            profile_patch_name,
            tempo: None,
            key_signature: None,
            notes: None,
        };
        (entry, source)
    }

    // Refresh trigger - increment this to force reload
    let mut refresh_trigger = use_signal(|| 0_u32);

    // Unified effect: re-runs when rig_type changes or refresh_trigger changes.
    // Loads rigs + layers as combined preset list, profiles, and songs.
    {
        let signal = signal.clone();
        use_effect(move || {
            let rt = rig_type();
            let _trigger = refresh_trigger(); // Read to track dependency
            let signal = signal.clone();

            // Reset all state
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
            expanded_ids.set(std::collections::HashSet::new());
            expanded_profile_ids.set(std::collections::HashSet::new());

            spawn(async move {
                // 1) Rigs filtered by type
                let rigs = signal.rigs().list().await;
                let filtered: Vec<_> = rigs
                    .into_iter()
                    .filter(|r| r.rig_type.map_or(false, |t| t == rt))
                    .collect();
                let rig_id_set: std::collections::HashSet<String> =
                    filtered.iter().map(|r| r.id.to_string()).collect();

                // 2) Layers filtered by engine type
                let et = rig_type_to_engine_type(rt);
                let all_layers = signal.layers().list().await;
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
                let all_profiles = signal.profiles().list().await;
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
                        prm.insert(patch.id.to_string(), (rig_str.clone(), scene_str.clone()));
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
                let all_setlists = signal.setlists().list().await;
                let mut opts: Vec<(String, String)> =
                    vec![("all".to_string(), "All Songs".to_string())];
                for sl in &all_setlists {
                    opts.push((sl.id.to_string(), sl.name.clone()));
                }
                setlist_options.set(opts);
                selected_setlist_id.set("all".to_string());

                // "All Songs" = union of all songs from all setlists (deduped)
                let mut seen_song_ids = std::collections::HashSet::new();
                let mut all_union_songs = Vec::new();
                for sl in &all_setlists {
                    for entry in &sl.entries {
                        let sid = entry.song_id.to_string();
                        if seen_song_ids.insert(sid.clone()) {
                            if let Some(song) = signal.songs().load(sid).await {
                                all_union_songs.push(song);
                            }
                        }
                    }
                }
                songs.set(
                    all_union_songs
                        .iter()
                        .map(|s| SongEntry {
                            id: s.id.to_string(),
                            name: s.name.clone(),
                            section_count: s.sections.len(),
                            duration_display: None,
                        })
                        .collect(),
                );

                // Auto-select first song
                let cur_presets = manage_presets();
                let cur_profiles = manage_profiles();
                if let Some(first_song) = all_union_songs.first() {
                    active_song_name.set(first_song.name.clone());
                    selected_song_id.set(Some(first_song.id.to_string()));
                    let mut entries = Vec::new();
                    let mut sources = std::collections::HashMap::new();
                    for sec in &first_song.sections {
                        let (entry, source) = build_section_entry(sec, &cur_presets, &cur_profiles);
                        sources.insert(entry.id.clone(), source);
                        entries.push(entry);
                    }
                    song_sections.set(entries);
                    section_sources.set(sources);
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
                        if let Some((engines, params)) =
                            signal_ui::views::resolve_scene_engines(&signal, &first_id, &scene_id)
                                .await
                        {
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
                if let Some(song) = signal.songs().load(song_id).await {
                    active_song_name.set(song.name.clone());
                    let cur_presets = manage_presets();
                    let cur_profiles = manage_profiles();
                    let mut entries = Vec::new();
                    let mut sources = std::collections::HashMap::new();
                    for sec in &song.sections {
                        let (entry, source) = build_section_entry(sec, &cur_presets, &cur_profiles);
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
            selected_song_id.set(None);
            selected_section_id.set(None);
            song_sections.set(Vec::new());
            active_song_name.set("Songs".to_string());
            spawn(async move {
                if setlist_id == "all" {
                    // "All Songs" = union of all songs from all setlists (deduped)
                    let all_setlists = signal.setlists().list().await;
                    let mut seen_ids = std::collections::HashSet::new();
                    let mut all_union = Vec::new();
                    for sl in &all_setlists {
                        for entry in &sl.entries {
                            let sid = entry.song_id.to_string();
                            if seen_ids.insert(sid.clone()) {
                                if let Some(song) = signal.songs().load(sid).await {
                                    all_union.push(song);
                                }
                            }
                        }
                    }
                    songs.set(
                        all_union
                            .iter()
                            .map(|s| SongEntry {
                                id: s.id.to_string(),
                                name: s.name.clone(),
                                section_count: s.sections.len(),
                                duration_display: None,
                            })
                            .collect(),
                    );
                    if let Some(first) = all_union.first() {
                        selected_song_id.set(Some(first.id.to_string()));
                        active_song_name.set(first.name.clone());
                        let cur_presets = manage_presets();
                        let cur_profiles = manage_profiles();
                        let mut entries = Vec::new();
                        let mut sources = std::collections::HashMap::new();
                        for sec in &first.sections {
                            let (entry, source) =
                                build_section_entry(sec, &cur_presets, &cur_profiles);
                            sources.insert(entry.id.clone(), source);
                            entries.push(entry);
                        }
                        song_sections.set(entries);
                        section_sources.set(sources);
                    }
                } else if let Some(setlist) = signal.setlists().load(setlist_id).await {
                    // Load songs from this specific setlist
                    let mut song_entries = Vec::new();
                    let mut first_song = None;
                    for entry in &setlist.entries {
                        if let Some(song) = signal.songs().load(entry.song_id.clone()).await {
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
                        let mut entries = Vec::new();
                        let mut sources = std::collections::HashMap::new();
                        for sec in &song.sections {
                            let (entry, source) =
                                build_section_entry(sec, &cur_presets, &cur_profiles);
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
    let bc_song = if mode == ManageMode::Song {
        selected_song_id()
            .and_then(|sid| songs().iter().find(|s| s.id == sid).map(|s| s.name.clone()))
    } else {
        None
    };
    let bc_section = if mode == ManageMode::Song {
        selected_section_id().and_then(|sid| {
            song_sections()
                .iter()
                .find(|s| s.id == sid)
                .map(|s| s.name.clone())
        })
    } else {
        None
    };
    // Profile/Patch: resolve from section source (if Patch), else from direct selection
    let (bc_profile, bc_patch) = {
        // First try: from current section's source
        let from_source = selected_section_id()
            .and_then(|sid| section_sources().get(&sid).cloned())
            .and_then(|source| match source {
                SectionSource::Patch { patch_id } => {
                    let patch_str = patch_id.to_string();
                    manage_profiles().iter().find_map(|prof| {
                        prof.patches
                            .iter()
                            .find(|(pid, _)| *pid == patch_str)
                            .map(|(_, pname)| (Some(prof.name.clone()), Some(pname.clone())))
                    })
                }
                _ => None,
            });
        // Fallback: from directly selected profile/patch signals
        from_source.unwrap_or_else(|| {
            let prof_name = selected_profile().and_then(|pid| {
                manage_profiles()
                    .iter()
                    .find(|p| p.id == pid)
                    .map(|p| p.name.clone())
            });
            let patch_name = selected_patch().and_then(|patch_id| {
                manage_profiles().iter().find_map(|prof| {
                    prof.patches
                        .iter()
                        .find(|(pid, _)| *pid == patch_id)
                        .map(|(_, pname)| pname.clone())
                })
            });
            (prof_name, patch_name)
        })
    };
    let bc_preset = current_preset.as_ref().and_then(|pid| {
        manage_presets()
            .iter()
            .find(|p| &p.id == pid)
            .map(|p| p.name.clone())
    });
    let bc_scene = current_preset.as_ref().and_then(|pid| {
        current_sub.as_ref().and_then(|sid| {
            manage_presets()
                .iter()
                .find(|p| &p.id == pid)
                .and_then(|p| {
                    p.sub_items
                        .iter()
                        .find(|(id, _)| id == sid)
                        .map(|(_, n)| n.clone())
                })
        })
    });

    rsx! {
        div { class: "flex flex-col h-full overflow-hidden",
            // ── Top bar: Capture + Mode tabs + Rig type selector + Scene tabs ──
            div { class: "flex items-center gap-2 px-3 py-1.5 border-b border-border bg-zinc-900/40 flex-shrink-0 overflow-x-auto",
                // Capture preset button
                signal_ui::components::CaptureButton {}
                // Mode tabs
                for &(m, label) in &[(ManageMode::Song, "Song"), (ManageMode::Profile, "Profile"), (ManageMode::Preset, "Preset")] {
                    {
                        let is_active = mode == m;
                        rsx! {
                            button {
                                key: "{label}",
                                class: if is_active {
                                    "px-2.5 py-1 text-xs font-semibold rounded bg-zinc-600 text-zinc-100"
                                } else {
                                    "px-2.5 py-1 text-xs text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 rounded"
                                },
                                onclick: move |_| manage_mode.set(m),
                                "{label}"
                            }
                        }
                    }
                }

                // Divider
                div { class: "w-px h-4 bg-zinc-700 mx-1 flex-shrink-0" }

                // Rig type selector
                span { class: "text-[10px] text-zinc-500 mr-1 flex-shrink-0", "Rig:" }
                for &rt in &[RigType::Guitar, RigType::Bass, RigType::Keys, RigType::Vocals] {
                    {
                        let is_active = rig_type() == rt;
                        let label = match rt {
                            RigType::Guitar => "Guitar",
                            RigType::Bass => "Bass",
                            RigType::Keys => "Keys",
                            RigType::Vocals => "Vocals",
                            _ => "Other",
                        };
                        rsx! {
                            button {
                                key: "{label}",
                                class: if is_active {
                                    "px-2 py-0.5 text-[11px] font-medium rounded bg-zinc-600 text-zinc-100"
                                } else {
                                    "px-2 py-0.5 text-[11px] text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 rounded"
                                },
                                onclick: move |_| rig_type.set(rt),
                                "{label}"
                            }
                        }
                    }
                }

                // Scene tabs (only shown for rig presets)
                if is_rig_active && !scenes.is_empty() {
                    div { class: "w-px h-4 bg-zinc-700 mx-1 flex-shrink-0" }
                    span { class: "text-[10px] text-zinc-500 mr-1 flex-shrink-0", "Scenes" }
                    for (sid, sname) in scenes.iter() {
                        {
                            let is_active = current_scene.as_deref() == Some(sid.as_str());
                            let scene_id = sid.clone();
                            let rid = rig_id().unwrap_or_default();
                            let mut on_click = select_sub_item.clone();
                            let mut on_assign = assign_current_section.clone();
                            rsx! {
                                button {
                                    key: "{sid}",
                                    class: if is_active {
                                        "px-2.5 py-1 text-xs font-medium rounded bg-zinc-700 text-zinc-100"
                                    } else {
                                        "px-2.5 py-1 text-xs text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 rounded"
                                    },
                                    onclick: move |_| {
                                        on_click(rid.clone(), scene_id.clone(), true);
                                        on_assign(rid.clone(), scene_id.clone(), true);
                                    },
                                    "{sname}"
                                }
                            }
                        }
                    }
                }
            }

            // ── Breadcrumb context row ──
            if mode != ManageMode::Preset {
                div { class: "flex items-center gap-1 px-3 py-1 border-b border-border/50 bg-zinc-950/30 flex-shrink-0 text-[10px]",
                    if let Some(ref name) = bc_song {
                        span { class: "text-zinc-500", "Song:" }
                        span { class: "text-zinc-300 mr-2", "{name}" }
                    }
                    if let Some(ref name) = bc_section {
                        span { class: "text-zinc-600", "\u{203A}" }
                        span { class: "text-zinc-500 ml-1", "Section:" }
                        span { class: "text-zinc-300 mr-2", "{name}" }
                    }
                    if let Some(ref name) = bc_profile {
                        span { class: "text-zinc-600", "\u{203A}" }
                        span { class: "text-zinc-500 ml-1", "Profile:" }
                        span { class: "text-zinc-300 mr-2", "{name}" }
                    }
                    if let Some(ref name) = bc_patch {
                        span { class: "text-zinc-600", "\u{203A}" }
                        span { class: "text-zinc-500 ml-1", "Patch:" }
                        span { class: "text-zinc-300 mr-2", "{name}" }
                    }
                    if let Some(ref name) = bc_preset {
                        span { class: "text-zinc-600", "\u{203A}" }
                        span { class: "text-zinc-500 ml-1", "Preset:" }
                        span { class: "text-zinc-300 mr-2", "{name}" }
                    }
                    if let Some(ref name) = bc_scene {
                        span { class: "text-zinc-600", "\u{203A}" }
                        span { class: "text-zinc-500 ml-1", "Scene:" }
                        span { class: "text-zinc-300", "{name}" }
                    }
                    if bc_song.is_none() && bc_preset.is_none() {
                        span { class: "text-zinc-600 italic", "No selection" }
                    }
                }
            }

            // ── Body: mode-dependent panel layout ──
            div { class: "flex flex-1 min-h-0 overflow-hidden",
                // ── Left panel: Presets (always) + Profiles (Song mode only) ──
                div { class: "w-56 flex-shrink-0 border-r border-border flex flex-col min-h-0 bg-zinc-950/40",
                    // Preset list — fills all space in Preset/Profile mode, top ~60% in Song mode
                    div {
                        class: if mode == ManageMode::Song {
                            "flex-[3] min-h-0 flex flex-col border-b border-border"
                        } else {
                            "flex-1 min-h-0 flex flex-col"
                        },
                        div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                            h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Presets" }
                        }
                        div { class: "flex-1 overflow-y-auto",
                            for item in manage_presets().iter().cloned() {
                                {
                                    let is_sel = current_preset.as_deref() == Some(item.id.as_str());
                                    let is_expanded = expanded_ids().contains(&item.id);
                                    let item_key = item.id.clone();
                                    let item_click_id = item.id.clone();
                                    let first_sub = item.sub_items.first().map(|(sid, _)| sid.clone());
                                    let is_rig = item.is_rig;
                                    let mut on_select = select_preset.clone();
                                    let mut on_assign = assign_current_section.clone();
                                    rsx! {
                                        div { key: "{item_key}",
                                            button {
                                                class: if is_sel {
                                                    "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                                                } else {
                                                    "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                                                },
                                                onclick: move |_| {
                                                    on_select(item_click_id.clone());
                                                    // Auto-assign the default (first) scene/snapshot
                                                    if let Some(ref sub_id) = first_sub {
                                                        on_assign(item_click_id.clone(), sub_id.clone(), is_rig);
                                                    }
                                                },
                                                div { class: "flex items-center gap-1.5",
                                                    span { class: "text-[10px] text-zinc-500 w-3 flex-shrink-0",
                                                        if is_expanded { "\u{25BE}" } else { "\u{25B8}" }
                                                    }
                                                    span {
                                                        class: if item.is_rig {
                                                            "text-[9px] px-1 rounded bg-zinc-600 text-zinc-300 flex-shrink-0"
                                                        } else {
                                                            "text-[9px] px-1 rounded bg-zinc-700 text-zinc-400 flex-shrink-0"
                                                        },
                                                        if item.is_rig { "RIG" } else { "LYR" }
                                                    }
                                                    span { class: "text-sm text-zinc-200 truncate flex-1", "{item.name}" }
                                                    span { class: "text-[10px] text-zinc-500 flex-shrink-0",
                                                        "{item.sub_items.len()}"
                                                    }
                                                }
                                            }
                                            if is_expanded {
                                                for (sub_id, sub_name) in item.sub_items.iter() {
                                                    {
                                                        let is_sub_sel = current_sub.as_deref() == Some(sub_id.as_str());
                                                        let parent_id = item.id.clone();
                                                        let sub_id_click = sub_id.clone();
                                                        let is_rig = item.is_rig;
                                                        let mut on_sub = select_sub_item.clone();
                                                        let mut on_assign = assign_current_section.clone();
                                                        rsx! {
                                                            button {
                                                                key: "{sub_id}",
                                                                class: if is_sub_sel {
                                                                    "w-full text-left pl-8 pr-3 py-1.5 text-xs bg-zinc-700/40 text-zinc-200 border-b border-zinc-800/30"
                                                                } else {
                                                                    "w-full text-left pl-8 pr-3 py-1.5 text-xs text-zinc-400 hover:bg-zinc-800/40 hover:text-zinc-300 border-b border-zinc-800/30"
                                                                },
                                                                onclick: move |_| {
                                                                    on_sub(parent_id.clone(), sub_id_click.clone(), is_rig);
                                                                    on_assign(parent_id.clone(), sub_id_click.clone(), is_rig);
                                                                },
                                                                "{sub_name}"
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Profiles on the left — only in Song mode
                    if mode == ManageMode::Song {
                        {
                            let signal = signal.clone();
                            let song_patch_cb: Rc<dyn Fn(String, String)> = Rc::new(move |prof_id: String, patch_id: String| {
                                let mut section_sources = section_sources;
                                let mut song_sections = song_sections;
                                let mut selected_preset_id = selected_preset_id;
                                let mut active_is_rig = active_is_rig;
                                let mut rig_id = rig_id;
                                let mut selected_sub_id = selected_sub_id;
                                let mut active_scene_id = active_scene_id;
                                let mut expanded_ids = expanded_ids;
                                let mut rig_scenes = rig_scenes;
                                let mut canvas_engines = canvas_engines;
                                let mut canvas_params = canvas_params;
                                // 1) Assign section → Patch source
                                if let (Some(sec_id), Some(song_id)) = (selected_section_id(), selected_song_id()) {
                                    let new_source = SectionSource::Patch { patch_id: patch_id.clone().into() };
                                    let mut sources = section_sources();
                                    sources.insert(sec_id.clone(), new_source.clone());
                                    section_sources.set(sources);
                                    let label = manage_profiles().iter().find_map(|prof| {
                                        if prof.id == prof_id {
                                            prof.patches.iter()
                                                .find(|(pid, _)| *pid == patch_id)
                                                .map(|(_, pname)| format!("{} / {}", prof.name, pname))
                                        } else { None }
                                    });
                                    let mut sections = song_sections();
                                    if let Some(entry) = sections.iter_mut().find(|e| e.id == sec_id) {
                                        entry.profile_patch_name = label;
                                        entry.rig_scene_name = None;
                                    }
                                    song_sections.set(sections);
                                    let signal = signal.clone();
                                    spawn(async move { signal.songs().set_section_source(song_id, sec_id, new_source).await; });
                                }
                                // 2) Navigate grid to the patch's rig/scene
                                if let Some((rid, sid)) = patch_rig_map().get(&patch_id).cloned() {
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
                            });
                            rsx! {
                                div { class: "flex-[2] min-h-0 flex flex-col border-t border-border",
                                    div { class: "px-3 py-2 border-b border-border flex-shrink-0 flex items-center justify-between",
                                        h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Profiles" }
                                        signal_ui::components::CreateProfileButton {
                                            on_created: move |_| {
                                                refresh_trigger.set(refresh_trigger() + 1);
                                            }
                                        }
                                    }
                                    div { class: "flex-1 overflow-y-auto",
                                        {render_profile_list(manage_profiles, expanded_profile_ids, selected_profile, selected_patch, Some(song_patch_cb), patch_display_labels)}
                                    }
                                }
                            }
                        }
                    }
                }

                // ── Center: Rig preset canvas ──
                div { class: "flex-1 min-w-0 flex flex-col overflow-hidden",
                    div { class: "flex-1 min-h-0 overflow-hidden flex flex-col",
                        if !canvas_engines().is_empty() {
                            {
                                let grid_key = format!(
                                    "{}-{}",
                                    selected_preset_id().unwrap_or_default(),
                                    selected_sub_id().unwrap_or_default(),
                                );
                                let grid_slots = engines_to_grid_slots(&canvas_engines(), &canvas_params());
                                let signal = signal.clone();
                                rsx! {
                                    RigGridPanel {
                                        key: "{grid_key}",
                                        initial_slots: grid_slots,
                                        on_save: move |slot: signal_ui::components::GridSlot| {
                                            let signal = signal.clone();
                                            let bt = slot.block_type;
                                            let pid = slot.preset_id.clone().unwrap_or_default();
                                            let sid = slot.snapshot_id.clone();
                                            let block = signal::Block::from_parameters(
                                                slot.parameters.iter()
                                                    .map(|(name, val)| signal::BlockParameter::new(
                                                        name.to_lowercase().replace(' ', "-"),
                                                        name.clone(),
                                                        *val,
                                                    ))
                                                    .collect()
                                            );
                                            spawn(async move {
                                                signal.block_presets().update_snapshot_params(
                                                    bt,
                                                    pid,
                                                    sid.unwrap_or_default(),
                                                    block,
                                                ).await;
                                            });
                                        },
                                    }
                                }
                            }
                        } else if rig_id().is_some() {
                            div { class: "flex items-center justify-center h-full",
                                p { class: "text-sm text-muted-foreground", "Loading rig graph..." }
                            }
                        } else {
                            div { class: "flex items-center justify-center h-full",
                                p { class: "text-sm text-muted-foreground", "Select a preset" }
                            }
                        }
                    }
                }

                // ── Right panel: Profile mode → profiles; Song mode → sections/songs ──
                if mode == ManageMode::Profile {
                    {
                        let signal = signal.clone();
                        let profile_patch_cb: Rc<dyn Fn(String, String)> = Rc::new(move |_prof_id: String, patch_id: String| {
                            let mut selected_preset_id = selected_preset_id;
                            let mut active_is_rig = active_is_rig;
                            let mut rig_id = rig_id;
                            let mut selected_sub_id = selected_sub_id;
                            let mut active_scene_id = active_scene_id;
                            let mut expanded_ids = expanded_ids;
                            let mut rig_scenes = rig_scenes;
                            let mut canvas_engines = canvas_engines;
                            let mut canvas_params = canvas_params;
                            // Navigate grid to the patch's rig/scene
                            if let Some((rid, sid)) = patch_rig_map().get(&patch_id).cloned() {
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
                        });
                        rsx! {
                            div { class: "w-64 flex-shrink-0 border-l border-border flex flex-col min-h-0 bg-zinc-950/40",
                                div { class: "px-3 py-2 border-b border-border flex-shrink-0 flex items-center justify-between",
                                    h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Profiles" }
                                    signal_ui::components::CreateProfileButton {
                                        on_created: move |_| {
                                            refresh_trigger.set(refresh_trigger() + 1);
                                        }
                                    }
                                }
                                div { class: "flex-1 overflow-y-auto",
                                    {render_profile_list(manage_profiles, expanded_profile_ids, selected_profile, selected_patch, Some(profile_patch_cb), patch_display_labels)}
                                }
                            }
                        }
                    }
                }

                if mode == ManageMode::Song {
                    div { class: "w-72 flex-shrink-0 border-l border-border flex flex-col min-h-0 bg-zinc-950/40",
                        // Sections for selected song (top)
                        div { class: "flex-[3] min-h-0 flex flex-col border-b border-border",
                            div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                                h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider",
                                    "{active_song_name}"
                                }
                            }
                            div { class: "flex-1 overflow-y-auto",
                                {
                                    let mut nav = navigate_to_section.clone();
                                    rsx! {
                                        SongEditor {
                                            song_name: String::new(),
                                            sections: song_sections(),
                                            selected_section_id: selected_section_id(),
                                            on_select_section: move |id: String| { nav(id); },
                                        }
                                    }
                                }
                            }
                        }

                        // Songs in setlist (bottom) — setlist dropdown above song list
                        div { class: "flex-[2] min-h-0 flex flex-col",
                            // Setlist selector dropdown with + Song button
                            div { class: "px-3 py-2 border-b border-border flex-shrink-0 flex items-center justify-between gap-2",
                                {
                                    let current_label = setlist_options()
                                        .iter()
                                        .find(|(id, _)| id == &selected_setlist_id())
                                        .map(|(_, name)| name.clone())
                                        .unwrap_or_else(|| "All Songs".to_string());
                                    rsx! {
                                        Dropdown {
                                            DropdownTrigger {
                                                button {
                                                    class: "w-full flex items-center justify-between bg-zinc-800 border border-zinc-700 rounded px-2 py-1 text-xs text-zinc-200 hover:bg-zinc-700 transition-colors",
                                                    span { "{current_label}" }
                                                    span { class: "text-zinc-500 text-[10px] ml-1", "\u{25BE}" }
                                                }
                                            }
                                            DropdownContent {
                                                width: "w-56".to_string(),
                                                for (idx, (sid, sname)) in setlist_options().iter().enumerate() {
                                                    {
                                                        let sid_val = sid.clone();
                                                        let sname_val = sname.clone();
                                                        let mut change = change_setlist.clone();
                                                        rsx! {
                                                            DropdownItem {
                                                                value: sid_val.clone(),
                                                                index: idx,
                                                                on_select: move |val: String| { change(val); },
                                                                "{sname_val}"
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                signal_ui::components::CreateSongButton {
                                    on_created: move |_| {
                                        refresh_trigger.set(refresh_trigger() + 1);
                                    }
                                }
                            }
                            div { class: "flex-1 overflow-y-auto",
                                for song in songs().iter() {
                                    {
                                        let is_sel = selected_song_id().as_deref() == Some(song.id.as_str());
                                        let song_id = song.id.clone();
                                        let mut load = load_song_sections.clone();
                                        rsx! {
                                            button {
                                                key: "{song_id}",
                                                class: if is_sel {
                                                    "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                                                } else {
                                                    "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                                                },
                                                onclick: move |_| { load(song_id.clone()); },
                                                div { class: "flex items-center gap-1.5",
                                                    span { class: "text-sm text-zinc-200 truncate flex-1", "{song.name}" }
                                                    span { class: "text-[10px] text-zinc-500 flex-shrink-0",
                                                        "{song.section_count}"
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Renders the expandable profile list with patches.
/// Shared between Song mode (left panel) and Profile mode (right panel).

/// Renders the expandable profile list with patches.
/// Shared between Song mode (left panel) and Profile mode (right panel).
/// `on_patch_click` is called with (profile_id, patch_id) when a patch is clicked.
fn render_profile_list(
    manage_profiles: Signal<Vec<ManageProfileItem>>,
    mut expanded_profile_ids: Signal<std::collections::HashSet<String>>,
    mut selected_profile: Signal<Option<String>>,
    mut selected_patch: Signal<Option<String>>,
    on_patch_click: Option<Rc<dyn Fn(String, String)>>,
    patch_labels: Signal<std::collections::HashMap<String, String>>,
) -> Element {
    rsx! {
        for prof in manage_profiles().iter().cloned() {
            {
                let is_sel = selected_profile().as_deref() == Some(prof.id.as_str());
                let is_expanded = expanded_profile_ids().contains(&prof.id);
                let prof_key = prof.id.clone();
                let prof_click_id = prof.id.clone();
                rsx! {
                    div { key: "{prof_key}",
                        button {
                            class: if is_sel {
                                "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                            } else {
                                "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                            },
                            onclick: move |_| {
                                let mut exp = expanded_profile_ids();
                                if exp.contains(&prof_click_id) {
                                    exp.remove(&prof_click_id);
                                } else {
                                    exp.insert(prof_click_id.clone());
                                }
                                expanded_profile_ids.set(exp);
                                selected_profile.set(Some(prof_click_id.clone()));
                            },
                            div { class: "flex items-center gap-1.5",
                                span { class: "text-[10px] text-zinc-500 w-3 flex-shrink-0",
                                    if is_expanded { "\u{25BE}" } else { "\u{25B8}" }
                                }
                                span { class: "text-sm text-zinc-200 truncate flex-1", "{prof.name}" }
                                span { class: "text-[10px] text-zinc-500 flex-shrink-0",
                                    "{prof.patches.len()}"
                                }
                            }
                        }
                        if is_expanded {
                            for (patch_id, patch_name) in prof.patches.iter() {
                                {
                                    let is_patch_sel = selected_patch().as_deref() == Some(patch_id.as_str());
                                    let pid = patch_id.clone();
                                    let prof_id_for_cb = prof.id.clone();
                                    let cb = on_patch_click.clone();
                                    let label = patch_labels().get(patch_id).cloned();
                                    rsx! {
                                        button {
                                            key: "{patch_id}",
                                            class: if is_patch_sel {
                                                "w-full text-left pl-8 pr-3 py-1.5 bg-zinc-700/40 text-zinc-200 border-b border-zinc-800/30"
                                            } else {
                                                "w-full text-left pl-8 pr-3 py-1.5 text-zinc-400 hover:bg-zinc-800/40 hover:text-zinc-300 border-b border-zinc-800/30"
                                            },
                                            onclick: move |_| {
                                                selected_patch.set(Some(pid.clone()));
                                                if let Some(ref cb) = cb {
                                                    cb(prof_id_for_cb.clone(), pid.clone());
                                                }
                                            },
                                            div { class: "flex flex-col gap-0.5",
                                                span { class: "text-xs truncate", "{patch_name}" }
                                                if let Some(lbl) = &label {
                                                    span { class: "text-[9px] text-zinc-500 truncate", "{lbl}" }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Editor tab — split grid + gradient inspector
// ---------------------------------------------------------------------------
