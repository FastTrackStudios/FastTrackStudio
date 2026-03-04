use dioxus::prelude::*;

use super::ManagePresetItem;
use crate::daw_registry::fx_guid_for_block;

// ---------------------------------------------------------------------------
// Editor tab — split grid + gradient inspector
// ---------------------------------------------------------------------------

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
pub(crate) fn SignalEditorTab() -> Element {
    let signal = signal_ui::use_signal_service();
    use signal::rig::RigType;
    use signal_ui::components::{GridSelection, GridSlot};
    use signal_ui::views::{
        engines_to_grid_slots, BlockDetailPanel, EditorInspectorPanel, EngineFlowData,
        EngineParamLookup, RigGridPanel,
    };

    // Rig type selector — filters presets
    let mut rig_type = use_signal(|| RigType::Guitar);

    // Preset list (rigs only for simplicity)
    let mut presets = use_signal(Vec::<ManagePresetItem>::new);
    let mut expanded_ids = use_signal(std::collections::HashSet::<String>::new);
    let mut selected_preset_id = use_signal(|| None::<String>);
    let mut selected_sub_id = use_signal(|| None::<String>);
    let mut rig_scenes = use_signal(Vec::<(String, String)>::new);
    let mut active_scene_id = use_signal(|| None::<String>);

    // Resolved engine flow data + param lookup for the grid
    let mut canvas_engines = use_signal(Vec::<EngineFlowData>::new);
    let mut canvas_params = use_signal(EngineParamLookup::new);

    // Selection state lifted from RigGridPanel for the inspector
    let mut editor_selection = use_signal(|| None::<GridSelection>);
    let mut editor_chain = use_signal(Vec::<GridSlot>::new);

    // Block detail panel expanded state
    let mut detail_expanded = use_signal(|| false);


    // Load presets when rig_type changes
    {
        let signal = signal.clone();
        use_effect(move || {
            let signal = signal.clone();
            let rt = rig_type();
            selected_preset_id.set(None);
            selected_sub_id.set(None);
            active_scene_id.set(None);
            rig_scenes.set(Vec::new());
            canvas_engines.set(Vec::new());
            canvas_params.set(EngineParamLookup::new());
            editor_selection.set(None);
            editor_chain.set(Vec::new());

            spawn(async move {
                let rigs = signal.rigs().list().await.unwrap_or_default();
                let filtered: Vec<_> = rigs
                    .into_iter()
                    .filter(|r| r.rig_type.map_or(false, |t| t == rt))
                    .collect();

                let items: Vec<ManagePresetItem> = filtered
                    .iter()
                    .map(|r| ManagePresetItem {
                        id: r.id.to_string(),
                        name: r.name.clone(),
                        is_rig: true,
                        sub_items: r
                            .variants
                            .iter()
                            .map(|v| (v.id.to_string(), v.name.clone()))
                            .collect(),
                    })
                    .collect();
                presets.set(items);

                // Auto-select first rig + first scene
                if let Some(first) = filtered.first() {
                    let first_id = first.id.to_string();
                    selected_preset_id.set(Some(first_id.clone()));
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

    // Handle preset click
    let select_preset = {
        let signal = signal.clone();
        move |item_id: String| {
            let signal = signal.clone();
            let items = presets();
            let Some(item) = items.iter().find(|i| i.id == item_id).cloned() else {
                return;
            };
            let mut exp = expanded_ids();
            exp.insert(item_id.clone());
            expanded_ids.set(exp);
            selected_preset_id.set(Some(item_id.clone()));
            rig_scenes.set(item.sub_items.clone());
            editor_selection.set(None);

            if let Some((first_sub_id, _)) = item.sub_items.first() {
                let sub_id = first_sub_id.clone();
                selected_sub_id.set(Some(sub_id.clone()));
                active_scene_id.set(Some(sub_id.clone()));
                canvas_engines.set(Vec::new());
                canvas_params.set(EngineParamLookup::new());
                spawn(async move {
                    if let Some((engines, params)) =
                        signal_ui::views::resolve_scene_engines(&signal, &item_id, &sub_id).await
                    {
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

    // Handle scene tab click
    let select_scene = {
        let signal = signal.clone();
        move |parent_id: String, sub_id: String| {
            let signal = signal.clone();
            selected_sub_id.set(Some(sub_id.clone()));
            active_scene_id.set(Some(sub_id.clone()));
            editor_selection.set(None);
            canvas_engines.set(Vec::new());
            canvas_params.set(EngineParamLookup::new());
            spawn(async move {
                if let Some((engines, params)) =
                    signal_ui::views::resolve_scene_engines(&signal, &parent_id, &sub_id).await
                {
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

    rsx! {
        div { class: "flex flex-col h-full overflow-hidden",
            // ── Top bar: Rig type + Scene tabs ──
            div { class: "flex items-center gap-2 px-3 py-1.5 border-b border-border bg-zinc-900/40 flex-shrink-0 overflow-x-auto",
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

                // Scene tabs
                if !scenes.is_empty() {
                    div { class: "w-px h-4 bg-zinc-700 mx-1 flex-shrink-0" }
                    span { class: "text-[10px] text-zinc-500 mr-1 flex-shrink-0", "Scenes" }
                    for (sid, sname) in scenes.iter() {
                        {
                            let is_active = current_scene.as_deref() == Some(sid.as_str());
                            let scene_id = sid.clone();
                            let rid = selected_preset_id().unwrap_or_default();
                            let mut on_click = select_scene.clone();
                            rsx! {
                                button {
                                    key: "{sid}",
                                    class: if is_active {
                                        "px-2.5 py-1 text-xs font-medium rounded bg-zinc-700 text-zinc-100"
                                    } else {
                                        "px-2.5 py-1 text-xs text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 rounded"
                                    },
                                    onclick: move |_| {
                                        on_click(rid.clone(), scene_id.clone());
                                    },
                                    "{sname}"
                                }
                            }
                        }
                    }
                }
            }

            // ── Body: preset sidebar | grid | inspector ──
            div { class: "flex flex-1 min-h-0 overflow-hidden",
                // ── Left: Preset list ──
                div { class: "w-52 flex-shrink-0 border-r border-border flex flex-col min-h-0 bg-zinc-950/40",
                    div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                        h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Presets" }
                    }
                    div { class: "flex-1 overflow-y-auto",
                        for item in presets().iter().cloned() {
                            {
                                let is_sel = current_preset.as_deref() == Some(item.id.as_str());
                                let is_expanded = expanded_ids().contains(&item.id);
                                let item_key = item.id.clone();
                                let item_click_id = item.id.clone();
                                let mut on_select = select_preset.clone();
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
                                            },
                                            div { class: "flex items-center gap-1.5",
                                                span { class: "text-[10px] text-zinc-500 w-3 flex-shrink-0",
                                                    if is_expanded { "\u{25BE}" } else { "\u{25B8}" }
                                                }
                                                span { class: "text-[9px] px-1 rounded bg-zinc-600 text-zinc-300 flex-shrink-0",
                                                    "RIG"
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
                                                    let mut on_scene = select_scene.clone();
                                                    rsx! {
                                                        button {
                                                            key: "{sub_id}",
                                                            class: if is_sub_sel {
                                                                "w-full text-left pl-8 pr-3 py-1.5 text-xs bg-zinc-700/40 text-zinc-200 border-b border-zinc-800/30"
                                                            } else {
                                                                "w-full text-left pl-8 pr-3 py-1.5 text-xs text-zinc-400 hover:bg-zinc-800/40 hover:text-zinc-300 border-b border-zinc-800/30"
                                                            },
                                                            onclick: move |_| {
                                                                on_scene(parent_id.clone(), sub_id_click.clone());
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

                // ── Center: Grid ──
                div { class: "flex-[3] min-w-0 flex flex-col overflow-hidden",
                    if !canvas_engines().is_empty() {
                        {
                            let grid_key = format!(
                                "editor-{}-{}",
                                selected_preset_id().unwrap_or_default(),
                                selected_sub_id().unwrap_or_default(),
                            );
                            let grid_slots = engines_to_grid_slots(&canvas_engines(), &canvas_params());
                            // Keep chain in sync for the inspector
                            let inspector_slots = grid_slots.clone();
                            editor_chain.set(inspector_slots);
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
                                    on_selection_change: move |sel: Option<GridSelection>| {
                                        editor_selection.set(sel);
                                    },
                                    on_param_change: move |(id, name, value): (_, String, f32)| {
                                        // Update inspector chain too
                                        let mut current = editor_chain();
                                        if let Some(slot) = current.iter_mut().find(|s| s.id == id) {
                                            if let Some(p) = slot.parameters.iter_mut().find(|(n, _)| *n == name) {
                                                p.1 = value;
                                            }
                                            // Push to REAPER if we have a mapping
                                            if let Some(ref pid) = slot.preset_id {
                                                if let Some(fx_guid) = fx_guid_for_block(pid) {
                                                    let param_name = name.clone();
                                                    spawn(async move {
                                                        if let Some(daw) = crate::daw_registry::signal_daw() {
                                                            if let Ok(project) = daw.current_project().await {
                                                                let tracks = project.tracks().all().await.unwrap_or_default();
                                                                for t in &tracks {
                                                                    if let Ok(Some(handle)) = project.tracks().by_guid(&t.guid).await {
                                                                        if let Ok(Some(fx)) = handle.fx_chain().by_guid(&fx_guid).await {
                                                                            let _ = fx.param_by_name(&param_name).set(value as f64).await;
                                                                            break;
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    });
                                                }
                                            }
                                        }
                                        editor_chain.set(current);
                                    },
                                    on_save: move |slot: GridSlot| {
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
                                    on_save_as_new: move |(slot, name): (GridSlot, String)| {
                                        let signal = signal2.clone();
                                        let bt = slot.block_type;
                                        let block = slot_to_block(&slot);
                                        spawn(async move {
                                            let _ = signal.block_presets().create(name, bt, block).await;
                                        });
                                    },
                                    on_save_block_snapshot: move |slot: GridSlot| {
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
                                    on_save_block_snapshot_as: move |(slot, name): (GridSlot, String)| {
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
                                    on_save_module_preset_as: move |(slots, name, mt): (Vec<GridSlot>, String, signal::ModuleType)| {
                                        let signal = signal5.clone();
                                        let module = slots_to_module(&slots);
                                        let snapshot = signal::ModuleSnapshot::new(signal::ModuleSnapshotId::new(), "Default", module);
                                        let preset = signal::ModulePreset::new(signal::ModulePresetId::new(), name, mt, snapshot, vec![]);
                                        spawn(async move {
                                            let _ = signal.module_presets().save(preset).await;
                                        });
                                    },
                                    on_save_module_snapshot_as: move |(slots, name, _mt): (Vec<GridSlot>, String, signal::ModuleType)| {
                                        let signal = signal6.clone();
                                        let module = slots_to_module(&slots);
                                        let snap = signal::ModuleSnapshot::new(signal::ModuleSnapshotId::new(), name, module);
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
                    } else if selected_preset_id().is_some() {
                        div { class: "flex items-center justify-center h-full",
                            p { class: "text-sm text-muted-foreground", "Loading rig graph..." }
                        }
                    } else {
                        div { class: "flex items-center justify-center h-full",
                            p { class: "text-sm text-muted-foreground", "Select a preset" }
                        }
                    }
                }

                // ── Right: Editor Inspector ──
                {
                    let signal = signal.clone();
                    let signal_save = signal.clone();
                    rsx! {
                        div { class: "w-80 flex-shrink-0 border-l border-border overflow-y-auto bg-zinc-950/30",
                            EditorInspectorPanel {
                                selection: editor_selection(),
                                chain: editor_chain(),
                                on_param_change: move |(id, name, value): (_, String, f32)| {
                                    // Mirror param change into editor_chain
                                    let mut current = editor_chain();
                                    if let Some(slot) = current.iter_mut().find(|s| s.id == id) {
                                        if let Some(p) = slot.parameters.iter_mut().find(|(n, _)| *n == name) {
                                            p.1 = value;
                                        }
                                        // Push to REAPER if mapped
                                        if let Some(ref pid) = slot.preset_id {
                                            if let Some(fx_guid) = fx_guid_for_block(pid) {
                                                let param_name = name.clone();
                                                spawn(async move {
                                                    if let Some(daw) = crate::daw_registry::signal_daw() {
                                                        if let Ok(project) = daw.current_project().await {
                                                            let tracks = project.tracks().all().await.unwrap_or_default();
                                                            for t in &tracks {
                                                                if let Ok(Some(handle)) = project.tracks().by_guid(&t.guid).await {
                                                                    if let Ok(Some(fx)) = handle.fx_chain().by_guid(&fx_guid).await {
                                                                        let _ = fx.param_by_name(&param_name).set(value as f64).await;
                                                                        break;
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                });
                                            }
                                        }
                                    }
                                    editor_chain.set(current);
                                },
                                on_save: move |slot: GridSlot| {
                                    let signal = signal_save.clone();
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
                                on_save_as_new: move |(slot, name): (GridSlot, String)| {
                                    let signal = signal.clone();
                                    let bt = slot.block_type;
                                    let block = slot_to_block(&slot);
                                    spawn(async move {
                                        let _ = signal.block_presets().create(name, bt, block).await;
                                    });
                                },
                                on_expand_detail: move |_| {
                                    detail_expanded.set(true);
                                },
                            }
                        }
                    }
                }

                // ── Block Detail Panel overlay (expanded) ──
                if detail_expanded() {
                    {
                        // Build a domain Block from the currently selected slot
                        let sel_block = editor_selection()
                            .as_ref()
                            .and_then(|sel| match sel {
                                GridSelection::Block(id) => {
                                    editor_chain().iter().find(|s| s.id == *id).cloned()
                                }
                                _ => None,
                            });
                        if let Some(ref slot) = sel_block {
                            let block = slot_to_block(slot);
                            let bt = slot.block_type;
                            rsx! {
                                div { class: "w-96 flex-shrink-0 border-l border-zinc-800",
                                    BlockDetailPanel {
                                        block,
                                        block_type: bt,
                                        on_close: move |_| {
                                            detail_expanded.set(false);
                                        },
                                    }
                                }
                            }
                        } else {
                            rsx! {
                                div { class: "w-96 flex-shrink-0 border-l border-zinc-800 flex items-center justify-center",
                                    div { class: "text-xs text-zinc-600 text-center px-4",
                                        "Select a block to view its detail panel."
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
// Browser dialog (near-full-screen)
// ---------------------------------------------------------------------------
