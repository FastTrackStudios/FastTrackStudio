//! Detail Panel — 3-column layout for editing the selected block/module.
//!
//! Layout:
//! ┌──────────────┬─────────────────────────────────────┬──────────────┐
//! │  Left 15%    │           Middle 65%                │  Right 15%   │
//! │  Preset /    │  Tabs: Macro | Detail | Advanced    │  Reserved    │
//! │  Snapshot    │                                     │  (future)    │
//! │  Selector    │                                     │              │
//! └──────────────┴─────────────────────────────────────┴──────────────┘

use super::grid_view::GridSelection;
use super::module_editor_view::CompositionSlot;
use crate::components::rig_grid::block_colors::block_type_color;
use crate::prelude::*;
use crate::signals::RIG_SERVICE;
use tracing::warn;
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// Detail tabs
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum DetailTab {
    #[default]
    Macro,
    Detail,
    Advanced,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum AdvancedSubTab {
    #[default]
    Parameters,
    Chunk,
}

// ─────────────────────────────────────────────────────────────────────────────
// DetailPanel
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct DetailPanelProps {
    /// Current selection (block or module, or None).
    pub selection: Option<GridSelection>,
    /// The full composition chain — used to look up slots by ID or module group.
    pub chain: Vec<CompositionSlot>,
    /// Callback when a block preset is assigned to the selected slot.
    /// Payload: (slot_id, preset_id, preset_name)
    pub on_preset_assigned: Option<EventHandler<(Uuid, Uuid, String)>>,
}

/// 3-column detail panel for editing the selected block or module.
#[component]
pub fn DetailPanel(props: DetailPanelProps) -> Element {
    let mut active_tab = use_signal(|| DetailTab::Macro);
    let mut advanced_sub = use_signal(|| AdvancedSubTab::Parameters);
    let tab = active_tab();

    // Block preset / snapshot state for the left sidebar
    let mut block_presets = use_signal(Vec::<signal_control::block_preset::Model>::new);
    // Map of preset_id -> snapshots (loaded on expand)
    let mut preset_snapshots = use_signal(
        std::collections::HashMap::<Uuid, Vec<signal_control::block_snapshot::Model>>::new,
    );
    let mut expanded_preset_id = use_signal(|| None::<Uuid>);
    let mut active_preset_id = use_signal(|| None::<Uuid>);
    let mut active_snapshot_id = use_signal(|| None::<Uuid>);

    // Save preset/snapshot dialog state
    let mut show_save_preset_dialog = use_signal(|| false);
    let mut save_preset_name = use_signal(String::new);
    let mut show_save_snapshot_dialog = use_signal(|| false);
    let mut save_snapshot_name = use_signal(String::new);

    // Module preset / snapshot state
    let mut module_presets = use_signal(Vec::<signal_control::module_preset_entity::Model>::new);
    let mut module_snapshots = use_signal(
        std::collections::HashMap::<Uuid, Vec<signal_control::module_snapshot::Model>>::new,
    );
    let mut mod_expanded_preset_id = use_signal(|| None::<Uuid>);
    let mut mod_active_preset_id = use_signal(|| None::<Uuid>);
    let mut mod_active_snapshot_id = use_signal(|| None::<Uuid>);

    // Resolve selection into concrete data
    let selected_slot: Option<CompositionSlot> = match &props.selection {
        Some(GridSelection::Block(id)) => props.chain.iter().find(|s| s.id == *id).cloned(),
        _ => None,
    };

    let selected_module_name: Option<String> = match &props.selection {
        Some(GridSelection::Module(name)) => Some(name.clone()),
        _ => None,
    };

    // For module selection, gather all blocks in that module
    let module_blocks: Vec<CompositionSlot> = if let Some(ref name) = selected_module_name {
        props
            .chain
            .iter()
            .filter(|s| s.module_group.as_deref() == Some(name.as_str()))
            .cloned()
            .collect()
    } else {
        Vec::new()
    };

    // Get the module type color from the first block in the module
    let module_color = module_blocks
        .first()
        .map(|s| block_type_color(s.block_type));

    // Track selection identity in signals so use_effect re-fires on change
    let mut sel_block_type_sig = use_signal(|| None::<String>);
    let mut sel_mod_type_sig = use_signal(|| None::<String>);

    // Update tracked selection signals from props (runs every render)
    let new_block_type = selected_slot
        .as_ref()
        .map(|s| s.block_type.display_name().to_string());
    let new_mod_type = module_blocks
        .first()
        .and_then(|s| s.module_type)
        .map(|mt| mt.display_name().to_string());
    if *sel_block_type_sig.read() != new_block_type {
        *sel_block_type_sig.write() = new_block_type;
    }
    if *sel_mod_type_sig.read() != new_mod_type {
        *sel_mod_type_sig.write() = new_mod_type;
    }

    // Auto-fetch block presets when the selected block changes
    {
        use_effect(move || {
            let bt = sel_block_type_sig.read().clone();

            // Reset state when block changes
            expanded_preset_id.set(None);
            active_preset_id.set(None);
            active_snapshot_id.set(None);
            show_save_preset_dialog.set(false);
            show_save_snapshot_dialog.set(false);
            preset_snapshots.write().clear();

            if let Some(block_type) = bt {
                spawn(async move {
                    let Some(ctl) = RIG_SERVICE.read().clone() else {
                        return;
                    };
                    match ctl.list_block_presets(Some(&block_type)).await {
                        Ok(presets) => block_presets.set(presets),
                        Err(e) => warn!("Failed to load block presets: {e}"),
                    }
                });
            } else {
                block_presets.set(Vec::new());
            }
        });
    }

    // Auto-fetch module presets when the selected module changes
    {
        use_effect(move || {
            let mt = sel_mod_type_sig.read().clone();

            mod_expanded_preset_id.set(None);
            mod_active_preset_id.set(None);
            mod_active_snapshot_id.set(None);
            module_snapshots.write().clear();

            if let Some(mod_type) = mt {
                spawn(async move {
                    let Some(ctl) = RIG_SERVICE.read().clone() else {
                        return;
                    };
                    match ctl.list_module_presets(Some(&mod_type)).await {
                        Ok(presets) => module_presets.set(presets),
                        Err(e) => warn!("Failed to load module presets: {e}"),
                    }
                });
            } else {
                module_presets.set(Vec::new());
            }
        });
    }

    let bp_list = block_presets.cloned();
    let snap_map = preset_snapshots.cloned();
    let expanded_id = *expanded_preset_id.read();
    let active_pid = *active_preset_id.read();
    let active_sid = *active_snapshot_id.read();

    let mp_list = module_presets.cloned();
    let mod_snap_map = module_snapshots.cloned();
    let mod_expanded_id = *mod_expanded_preset_id.read();
    let mod_active_pid = *mod_active_preset_id.read();
    let mod_active_sid = *mod_active_snapshot_id.read();

    let has_selection = props.selection.is_some();
    let is_module = selected_module_name.is_some();

    rsx! {
        div { class: "h-full w-full flex overflow-hidden",
            // ── Left column (15%): Preset / Snapshot selector ──
            div { class: "w-[15%] flex-shrink-0 border-r border-zinc-800/40 flex flex-col min-h-0 overflow-hidden",
                div { class: "px-3 py-2 border-b border-zinc-800/30 flex-shrink-0",
                    span { class: "text-[9px] font-bold text-zinc-500 uppercase tracking-[0.1em]",
                        "Presets"
                    }
                }
                div { class: "flex-1 overflow-y-auto min-h-0 px-2 py-1.5",
                    if let Some(ref slot) = selected_slot {
                        // ── Block selected: expandable preset browser ──
                        {
                            let color = block_type_color(slot.block_type);
                            let dot_style = format!("background-color: {};", color.bg);
                            let name = slot.block_preset_name.as_deref()
                                .unwrap_or(slot.block_type.display_name());
                            let module_name = slot.module_group.as_deref().unwrap_or("\u{2014}");
                            let slot_id = slot.id;
                            rsx! {
                                div { class: "flex flex-col gap-2",
                                    // Module group header
                                    div { class: "flex items-center gap-1.5 px-2 py-1 rounded-md bg-zinc-800/20",
                                        div {
                                            class: "w-1.5 h-1.5 rounded-full flex-shrink-0",
                                            style: "{dot_style}",
                                        }
                                        span { class: "text-[9px] font-semibold text-zinc-400 uppercase tracking-wide",
                                            "{module_name}"
                                        }
                                    }
                                    // Current block assignment
                                    div { class: "flex items-center gap-2 px-2 py-1.5 rounded-md bg-zinc-800/40 border border-zinc-700/30",
                                        div {
                                            class: "w-2 h-2 rounded-full flex-shrink-0",
                                            style: "{dot_style}",
                                        }
                                        div { class: "min-w-0",
                                            span { class: "text-[10px] font-medium text-zinc-200 block truncate", "{name}" }
                                            span { class: "text-[9px] text-zinc-500", "{slot.block_type.display_name()}" }
                                        }
                                    }
                                    // Expandable preset list from DB
                                    div { class: "flex flex-col",
                                        if bp_list.is_empty() {
                                            p { class: "text-[9px] text-zinc-600 italic px-2 py-1",
                                                "No saved presets"
                                            }
                                        } else {
                                            for preset in bp_list.iter() {
                                                {
                                                    let pid = preset.id;
                                                    let pname = preset.name.clone();
                                                    let is_active = active_pid == Some(pid);
                                                    let is_expanded = expanded_id == Some(pid);
                                                    let snaps = snap_map.get(&pid).cloned().unwrap_or_default();
                                                    let has_snaps = !snaps.is_empty();
                                                    let snap_count = snaps.len();
                                                    rsx! {
                                                        div { key: "{pid}",
                                                            class: "border-b border-zinc-800/50",
                                                            // Preset header row
                                                            div {
                                                                class: if is_active {
                                                                    "px-3 py-2 cursor-pointer bg-zinc-800 border-l-2 border-green-500 transition-colors"
                                                                } else {
                                                                    "px-3 py-2 cursor-pointer hover:bg-zinc-800/50 border-l-2 border-transparent transition-colors"
                                                                },
                                                                onclick: {
                                                                    let pname = pname.clone();
                                                                    move |_| {
                                                                        // Activate this preset
                                                                        active_preset_id.set(Some(pid));
                                                                        active_snapshot_id.set(None);

                                                                        // Notify parent to update composition chain
                                                                        if let Some(ref cb) = props.on_preset_assigned {
                                                                            cb.call((slot_id, pid, pname.clone()));
                                                                        }

                                                                        // Toggle expand / fetch snapshots
                                                                        if expanded_id == Some(pid) {
                                                                            expanded_preset_id.set(None);
                                                                        } else {
                                                                            expanded_preset_id.set(Some(pid));
                                                                            if !preset_snapshots.read().contains_key(&pid) {
                                                                                spawn(async move {
                                                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                                                    match ctl.list_block_snapshots(pid).await {
                                                                                        Ok(snaps) => {
                                                                                            preset_snapshots.write().insert(pid, snaps);
                                                                                        }
                                                                                        Err(e) => warn!("Failed to load block snapshots: {e}"),
                                                                                    }
                                                                                });
                                                                            }
                                                                        }
                                                                    }
                                                                },
                                                                div { class: "flex items-start justify-between",
                                                                    div { class: "flex-1 min-w-0",
                                                                        div { class: "flex items-center gap-1.5",
                                                                            span { class: "text-[10px] text-zinc-600 flex-shrink-0",
                                                                                if is_expanded { "\u{25BE}" } else { "\u{25B8}" }
                                                                            }
                                                                            span {
                                                                                class: if is_active {
                                                                                    "font-medium text-xs text-green-400 truncate"
                                                                                } else {
                                                                                    "font-medium text-xs text-zinc-200 truncate"
                                                                                },
                                                                                "{pname}"
                                                                            }
                                                                        }
                                                                        if snap_count > 0 {
                                                                            {
                                                                                let label = if snap_count == 1 { "snapshot" } else { "snapshots" };
                                                                                rsx! {
                                                                                    div { class: "text-[10px] text-zinc-500 pl-4",
                                                                                        "{snap_count} {label}"
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                            // Expanded snapshot list
                                                            if is_expanded {
                                                                div { class: "bg-zinc-850 py-1",
                                                                    if !has_snaps {
                                                                        p { class: "pl-6 pr-3 py-1.5 text-xs text-zinc-600 italic",
                                                                            "No snapshots"
                                                                        }
                                                                    }
                                                                    for snap in snaps.iter() {
                                                                        {
                                                                            let sid = snap.id;
                                                                            let snap_name = snap.name.clone();
                                                                            let is_default = snap.is_default;
                                                                            let snap_active = active_sid == Some(sid);
                                                                            rsx! {
                                                                                div {
                                                                                    key: "{sid}",
                                                                                    class: if snap_active {
                                                                                        "pl-6 pr-3 py-1.5 text-xs cursor-pointer bg-green-500/10 text-green-400 transition-colors"
                                                                                    } else {
                                                                                        "pl-6 pr-3 py-1.5 text-xs cursor-pointer hover:bg-zinc-800/50 text-zinc-400 hover:text-zinc-300 transition-colors"
                                                                                    },
                                                                                    onclick: {
                                                                                        let display = format!("{} / {}", pname, snap_name);
                                                                                        move |_| {
                                                                                            active_preset_id.set(Some(pid));
                                                                                            active_snapshot_id.set(Some(sid));

                                                                                            // Notify parent to update composition chain
                                                                                            if let Some(ref cb) = props.on_preset_assigned {
                                                                                                cb.call((slot_id, pid, display.clone()));
                                                                                            }
                                                                                        }
                                                                                    },
                                                                                    div { class: "flex items-center gap-1.5",
                                                                                        span { class: "truncate flex-1",
                                                                                            "\u{2022} {snap_name}"
                                                                                        }
                                                                                        if is_default {
                                                                                            span { class: "text-[8px] text-emerald-400/60 flex-shrink-0", "default" }
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
                                    // ── Save buttons ──
                                    div { class: "flex flex-col gap-1.5 pt-2 border-t border-zinc-800/30",
                                        // Save as New Preset
                                        {
                                            let block_type_str = slot.block_type.display_name().to_string();
                                            let plugin_name_for_save = slot.plugin_name.clone();
                                            rsx! {
                                                button {
                                                    class: "w-full px-2 py-1.5 rounded-md text-[10px] font-semibold \
                                                            bg-emerald-500/15 text-emerald-300 border border-emerald-500/25 \
                                                            hover:bg-emerald-500/25 hover:border-emerald-500/40 transition-all duration-150",
                                                    onclick: move |_| {
                                                        show_save_preset_dialog.set(true);
                                                        save_preset_name.set(String::new());
                                                    },
                                                    "+ Save as Preset"
                                                }
                                                // Save preset dialog
                                                if *show_save_preset_dialog.read() {
                                                    div { class: "flex flex-col gap-1",
                                                        input {
                                                            class: "w-full px-2 py-1 rounded text-xs bg-zinc-800 border border-emerald-500/40 \
                                                                    text-zinc-200 placeholder-zinc-600 outline-none",
                                                            placeholder: "Preset name...",
                                                            value: "{save_preset_name}",
                                                            autofocus: true,
                                                            oninput: move |evt| save_preset_name.set(evt.value().clone()),
                                                            onkeydown: {
                                                                let bt = block_type_str.clone();
                                                                let pn = plugin_name_for_save.clone();
                                                                move |evt| {
                                                                    if evt.key() == Key::Enter {
                                                                        let val = save_preset_name().trim().to_string();
                                                                        if !val.is_empty() {
                                                                            show_save_preset_dialog.set(false);
                                                                            let bt = bt.clone();
                                                                            let pn = pn.clone();
                                                                            spawn(async move {
                                                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                                match ctl.create_block_preset(
                                                                                    &val, &bt,
                                                                                    pn.as_ref().map(|n| serde_json::json!({"plugin_name": n})),
                                                                                    None, None,
                                                                                ).await {
                                                                                    Ok(id) => {
                                                                                        tracing::info!("Saved block preset '{val}' ({bt}) → {id}");
                                                                                        active_preset_id.set(Some(id));
                                                                                        // Refresh the preset list
                                                                                        match ctl.list_block_presets(Some(&bt)).await {
                                                                                            Ok(presets) => block_presets.set(presets),
                                                                                            Err(e) => warn!("Refresh failed: {e}"),
                                                                                        }
                                                                                    }
                                                                                    Err(e) => warn!("Save preset failed: {e}"),
                                                                                }
                                                                            });
                                                                        }
                                                                    } else if evt.key() == Key::Escape {
                                                                        show_save_preset_dialog.set(false);
                                                                    }
                                                                }
                                                            },
                                                        }
                                                    }
                                                }
                                                // Save Snapshot (only when a preset is active)
                                                if active_pid.is_some() {
                                                    button {
                                                        class: "w-full px-2 py-1.5 rounded-md text-[10px] font-semibold \
                                                                bg-blue-500/15 text-blue-300 border border-blue-500/25 \
                                                                hover:bg-blue-500/25 hover:border-blue-500/40 transition-all duration-150",
                                                        onclick: move |_| {
                                                            show_save_snapshot_dialog.set(true);
                                                            save_snapshot_name.set(String::new());
                                                        },
                                                        "+ Save Snapshot"
                                                    }
                                                    if *show_save_snapshot_dialog.read() {
                                                        div { class: "flex flex-col gap-1",
                                                            input {
                                                                class: "w-full px-2 py-1 rounded text-xs bg-zinc-800 border border-blue-500/40 \
                                                                        text-zinc-200 placeholder-zinc-600 outline-none",
                                                                placeholder: "Snapshot name...",
                                                                value: "{save_snapshot_name}",
                                                                autofocus: true,
                                                                oninput: move |evt| save_snapshot_name.set(evt.value().clone()),
                                                                onkeydown: move |evt| {
                                                                    if evt.key() == Key::Enter {
                                                                        let val = save_snapshot_name().trim().to_string();
                                                                        if !val.is_empty() {
                                                                            show_save_snapshot_dialog.set(false);
                                                                            let pid = active_pid.unwrap();
                                                                            spawn(async move {
                                                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                                match ctl.create_block_snapshot(
                                                                                    pid, &val,
                                                                                    serde_json::json!({}),
                                                                                    None,
                                                                                    false,
                                                                                ).await {
                                                                                    Ok(id) => {
                                                                                        tracing::info!("Saved block snapshot '{val}' → {id}");
                                                                                        active_snapshot_id.set(Some(id));
                                                                                        // Refresh snapshots for this preset
                                                                                        match ctl.list_block_snapshots(pid).await {
                                                                                            Ok(snaps) => { preset_snapshots.write().insert(pid, snaps); }
                                                                                            Err(e) => warn!("Refresh snapshots failed: {e}"),
                                                                                        }
                                                                                    }
                                                                                    Err(e) => warn!("Save snapshot failed: {e}"),
                                                                                }
                                                                            });
                                                                        }
                                                                    } else if evt.key() == Key::Escape {
                                                                        show_save_snapshot_dialog.set(false);
                                                                    }
                                                                },
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
                    } else if let Some(ref mod_name) = selected_module_name {
                        // ── Module selected: show module-level preset browser ──
                        {
                            let dot_style = module_color.as_ref()
                                .map(|c| format!("background-color: {};", c.bg))
                                .unwrap_or_default();
                            rsx! {
                                div { class: "flex flex-col",
                                    // Module identity header
                                    div { class: "flex items-center gap-1.5 px-3 py-2 border-b border-zinc-800/30",
                                        div {
                                            class: "w-2.5 h-2.5 rounded-full flex-shrink-0",
                                            style: "{dot_style}",
                                        }
                                        span { class: "text-[10px] font-semibold text-zinc-200 uppercase tracking-wide",
                                            "{mod_name}"
                                        }
                                    }
                                    // Expandable module preset list
                                    div { class: "flex-1 overflow-y-auto",
                                        if mp_list.is_empty() {
                                            p { class: "px-3 py-2 text-xs text-zinc-600 italic",
                                                "No saved module presets"
                                            }
                                        } else {
                                            for mp in mp_list.iter() {
                                                {
                                                    let mpid = mp.id;
                                                    let mpname = mp.name.clone();
                                                    let is_active = mod_active_pid == Some(mpid);
                                                    let is_expanded = mod_expanded_id == Some(mpid);
                                                    let snaps = mod_snap_map.get(&mpid).cloned().unwrap_or_default();
                                                    let has_snaps = !snaps.is_empty();
                                                    let snap_count = snaps.len();
                                                    rsx! {
                                                        div { key: "{mpid}",
                                                            class: "border-b border-zinc-800/50",
                                                            // Preset header row
                                                            div {
                                                                class: if is_active {
                                                                    "px-3 py-2 cursor-pointer bg-zinc-800 border-l-2 border-green-500 transition-colors"
                                                                } else {
                                                                    "px-3 py-2 cursor-pointer hover:bg-zinc-800/50 border-l-2 border-transparent transition-colors"
                                                                },
                                                                onclick: move |_| {
                                                                    if mod_expanded_id == Some(mpid) {
                                                                        mod_expanded_preset_id.set(None);
                                                                    } else {
                                                                        mod_expanded_preset_id.set(Some(mpid));
                                                                        if !module_snapshots.read().contains_key(&mpid) {
                                                                            spawn(async move {
                                                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                                                match ctl.list_module_snapshots(mpid).await {
                                                                                    Ok(snaps) => {
                                                                                        module_snapshots.write().insert(mpid, snaps);
                                                                                    }
                                                                                    Err(e) => warn!("Failed to load module snapshots: {e}"),
                                                                                }
                                                                            });
                                                                        }
                                                                    }
                                                                },
                                                                ondoubleclick: move |_| {
                                                                    mod_active_preset_id.set(Some(mpid));
                                                                    mod_active_snapshot_id.set(None);
                                                                },
                                                                div { class: "flex items-start justify-between",
                                                                    div { class: "flex-1 min-w-0",
                                                                        div { class: "flex items-center gap-1.5",
                                                                            span { class: "text-[10px] text-zinc-600 flex-shrink-0",
                                                                                if is_expanded { "\u{25BE}" } else { "\u{25B8}" }
                                                                            }
                                                                            span {
                                                                                class: if is_active {
                                                                                    "font-medium text-xs text-green-400 truncate"
                                                                                } else {
                                                                                    "font-medium text-xs text-zinc-200 truncate"
                                                                                },
                                                                                "{mpname}"
                                                                            }
                                                                        }
                                                                        if snap_count > 0 {
                                                                            {
                                                                                let label = if snap_count == 1 { "snapshot" } else { "snapshots" };
                                                                                rsx! {
                                                                                    div { class: "text-[10px] text-zinc-500 pl-4",
                                                                                        "{snap_count} {label}"
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                            // Expanded snapshot list
                                                            if is_expanded {
                                                                div { class: "bg-zinc-850 py-1",
                                                                    if !has_snaps {
                                                                        p { class: "pl-6 pr-3 py-1.5 text-xs text-zinc-600 italic",
                                                                            "No snapshots"
                                                                        }
                                                                    }
                                                                    for snap in snaps.iter() {
                                                                        {
                                                                            let sid = snap.id;
                                                                            let snap_name = snap.name.clone();
                                                                            let is_default = snap.is_default;
                                                                            let snap_active = mod_active_sid == Some(sid);
                                                                            rsx! {
                                                                                div {
                                                                                    key: "{sid}",
                                                                                    class: if snap_active {
                                                                                        "pl-6 pr-3 py-1.5 text-xs cursor-pointer bg-green-500/10 text-green-400 transition-colors"
                                                                                    } else {
                                                                                        "pl-6 pr-3 py-1.5 text-xs cursor-pointer hover:bg-zinc-800/50 text-zinc-400 hover:text-zinc-300 transition-colors"
                                                                                    },
                                                                                    onclick: move |_| {
                                                                                        mod_active_preset_id.set(Some(mpid));
                                                                                        mod_active_snapshot_id.set(Some(sid));
                                                                                    },
                                                                                    div { class: "flex items-center gap-1.5",
                                                                                        span { class: "truncate flex-1",
                                                                                            "\u{2022} {snap_name}"
                                                                                        }
                                                                                        if is_default {
                                                                                            span { class: "text-[8px] text-emerald-400/60 flex-shrink-0", "default" }
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
                                }
                            }
                        }
                    } else {
                        div { class: "flex items-center justify-center h-full",
                            p { class: "text-[9px] text-zinc-600", "Select a block or module" }
                        }
                    }
                }
            }

            // ── Middle column (65%): Tabbed content ──
            div { class: "flex-1 flex flex-col min-h-0 min-w-0 overflow-hidden",
                // Selection indicator
                if let Some(ref sel) = props.selection {
                    {
                        let (sel_kind, sel_label, sel_color, sel_template, sel_bypassed) = match sel {
                            GridSelection::Block(id) => {
                                let slot = props.chain.iter().find(|s| s.id == *id);
                                let label = slot.map(|s| {
                                    s.block_preset_name.as_deref()
                                        .unwrap_or(s.block_type.display_name())
                                        .to_string()
                                }).unwrap_or_else(|| "Unknown".to_string());
                                let color = slot.map(|s| block_type_color(s.block_type));
                                let tmpl = slot.map_or(false, |s| s.is_template);
                                let byp = slot.map_or(false, |s| s.bypassed);
                                ("Block", label, color, tmpl, byp)
                            }
                            GridSelection::Module(name) => {
                                let module_slots: Vec<&CompositionSlot> = props.chain.iter()
                                    .filter(|s| s.module_group.as_deref() == Some(name.as_str()))
                                    .collect();
                                let first_block = module_slots.first().copied();
                                let color = first_block.map(|s| block_type_color(s.block_type));
                                let tmpl = !module_slots.is_empty() && module_slots.iter().all(|s| s.is_template);
                                let byp = !module_slots.is_empty() && module_slots.iter().all(|s| s.bypassed);
                                ("Module", name.clone(), color, tmpl, byp)
                            }
                        };
                        let dot_bg = sel_color.as_ref().map(|c| c.bg).unwrap_or("#888");
                        rsx! {
                            div { class: "px-3 py-1.5 border-b border-zinc-800/30 flex items-center gap-2 flex-shrink-0 bg-zinc-800/20",
                                div {
                                    class: "w-2 h-2 rounded-full flex-shrink-0",
                                    style: "background-color: {dot_bg};",
                                }
                                span { class: "text-[9px] font-bold text-zinc-500 uppercase tracking-wide",
                                    "{sel_kind}"
                                }
                                span { class: "text-[10px] font-medium text-zinc-300 truncate",
                                    "{sel_label}"
                                }
                                if sel_template {
                                    span { class: "px-1.5 py-0.5 rounded text-[8px] font-bold uppercase tracking-wide bg-zinc-700/50 text-zinc-400 border border-dashed border-zinc-600/40",
                                        "Template"
                                    }
                                }
                                if sel_bypassed {
                                    span { class: "px-1.5 py-0.5 rounded text-[8px] font-bold uppercase tracking-wide bg-amber-900/30 text-amber-500/70 border border-amber-700/30",
                                        "Bypassed"
                                    }
                                }
                            }
                        }
                    }
                }
                // Tab bar
                div { class: "px-3 py-1.5 border-b border-zinc-800/30 flex items-center gap-1 flex-shrink-0",
                    {
                        let tabs = if is_module {
                            vec![
                                (DetailTab::Macro, "Overview"),
                                (DetailTab::Detail, "Blocks"),
                                (DetailTab::Advanced, "Advanced"),
                            ]
                        } else {
                            vec![
                                (DetailTab::Macro, "Macro"),
                                (DetailTab::Detail, "Detail"),
                                (DetailTab::Advanced, "Advanced"),
                            ]
                        };
                        rsx! {
                            for (t, label) in tabs {
                                button {
                                    key: "{label}",
                                    class: if tab == t {
                                        "px-3 py-1 rounded-md text-[10px] font-semibold text-zinc-200 bg-zinc-700/60 transition-colors"
                                    } else {
                                        "px-3 py-1 rounded-md text-[10px] font-medium text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800/40 transition-colors"
                                    },
                                    onclick: move |_| active_tab.set(t),
                                    "{label}"
                                }
                            }
                        }
                    }
                    // Advanced sub-tabs (when Advanced is active, block mode only)
                    if tab == DetailTab::Advanced && !is_module {
                        div { class: "ml-3 pl-3 border-l border-zinc-800/30 flex items-center gap-0.5",
                            {
                                let sub_tabs = [
                                    (AdvancedSubTab::Parameters, "Parameters"),
                                    (AdvancedSubTab::Chunk, "Chunk"),
                                ];
                                rsx! {
                                    for (st, label) in sub_tabs {
                                        button {
                                            key: "adv-{label}",
                                            class: if advanced_sub() == st {
                                                "px-2 py-0.5 rounded text-[9px] font-semibold text-zinc-300 bg-zinc-800/60 transition-colors"
                                            } else {
                                                "px-2 py-0.5 rounded text-[9px] font-medium text-zinc-600 hover:text-zinc-400 hover:bg-zinc-800/30 transition-colors"
                                            },
                                            onclick: move |_| advanced_sub.set(st),
                                            "{label}"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                // Tab content
                div { class: "flex-1 overflow-y-auto min-h-0 px-3 py-2",
                    if !has_selection {
                        div { class: "flex items-center justify-center h-full",
                            p { class: "text-xs text-zinc-600",
                                "Select a block or module in the grid to edit"
                            }
                        }
                    } else if let Some(ref slot) = selected_slot {
                        // Block-level tab content
                        match tab {
                            DetailTab::Macro => rsx! {
                                MacroTabContent { slot: slot.clone() }
                            },
                            DetailTab::Detail => rsx! {
                                DetailTabContent { slot: slot.clone() }
                            },
                            DetailTab::Advanced => rsx! {
                                AdvancedTabContent {
                                    slot: slot.clone(),
                                    sub_tab: advanced_sub(),
                                }
                            },
                        }
                    } else if is_module {
                        // Module-level tab content
                        match tab {
                            DetailTab::Macro => rsx! {
                                ModuleOverviewContent {
                                    module_name: selected_module_name.clone().unwrap(),
                                    blocks: module_blocks.clone(),
                                }
                            },
                            DetailTab::Detail => rsx! {
                                ModuleBlocksContent {
                                    module_name: selected_module_name.clone().unwrap(),
                                    blocks: module_blocks.clone(),
                                }
                            },
                            DetailTab::Advanced => rsx! {
                                ModuleAdvancedContent {
                                    module_name: selected_module_name.clone().unwrap(),
                                    blocks: module_blocks.clone(),
                                }
                            },
                        }
                    }
                }
            }

            // ── Right column (15%): Reserved ──
            div { class: "w-[15%] flex-shrink-0 border-l border-zinc-800/40 flex flex-col min-h-0 overflow-hidden",
                div { class: "px-3 py-2 border-b border-zinc-800/30 flex-shrink-0",
                    span { class: "text-[9px] font-bold text-zinc-500 uppercase tracking-[0.1em]",
                        "Controls"
                    }
                }
                div { class: "flex-1 flex items-center justify-center",
                    p { class: "text-[9px] text-zinc-700 italic text-center px-3",
                        "Reserved for routing, MIDI learn, automation"
                    }
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Block-level tab content (unchanged)
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct SlotProps {
    slot: CompositionSlot,
}

/// Macro tab: predefined "big knob" controls for the selected block.
#[component]
fn MacroTabContent(props: SlotProps) -> Element {
    let color = block_type_color(props.slot.block_type);
    let dot_bg = color.bg;
    let name = props
        .slot
        .block_preset_name
        .as_deref()
        .unwrap_or(props.slot.block_type.display_name());

    rsx! {
        div { class: "flex flex-col gap-3",
            div { class: "flex items-center gap-2",
                div {
                    class: "w-3 h-3 rounded-full flex-shrink-0",
                    style: "background-color: {dot_bg};",
                }
                span { class: "text-xs font-semibold text-zinc-200", "{name}" }
                span { class: "text-[10px] text-zinc-500", "Macro Controls" }
            }
            // Placeholder macro knobs
            div { class: "grid grid-cols-4 gap-3",
                for label in ["Gain", "Tone", "Level", "Mix"] {
                    div { class: "flex flex-col items-center gap-1.5",
                        div {
                            class: "w-10 h-10 rounded-full border-2 border-zinc-700/50 \
                                    bg-zinc-800/30 flex items-center justify-center",
                            span { class: "text-[10px] text-zinc-500 font-mono", "0" }
                        }
                        span { class: "text-[9px] text-zinc-500 font-medium", "{label}" }
                    }
                }
            }
            p { class: "text-[9px] text-zinc-600 italic mt-2",
                "Macro controls will be populated from block parameter definitions"
            }
        }
    }
}

/// Detail tab: full parameter list with inline sliders.
#[component]
fn DetailTabContent(props: SlotProps) -> Element {
    let color = block_type_color(props.slot.block_type);
    let dot_bg = color.bg;
    let name = props
        .slot
        .block_preset_name
        .as_deref()
        .unwrap_or(props.slot.block_type.display_name());

    rsx! {
        div { class: "flex flex-col gap-3",
            div { class: "flex items-center gap-2",
                div {
                    class: "w-3 h-3 rounded-full flex-shrink-0",
                    style: "background-color: {dot_bg};",
                }
                span { class: "text-xs font-semibold text-zinc-200", "{name}" }
                span { class: "text-[10px] text-zinc-500", "All Parameters" }
            }
            // Placeholder parameter list
            div { class: "flex flex-col gap-1.5",
                for (i, param_name) in ["Parameter 1", "Parameter 2", "Parameter 3", "Parameter 4"].iter().enumerate() {
                    div { class: "flex items-center gap-3 px-2 py-1 rounded hover:bg-zinc-800/30",
                        span { class: "text-[10px] text-zinc-400 w-24 flex-shrink-0", "{param_name}" }
                        div { class: "flex-1 h-1.5 bg-zinc-800 rounded-full overflow-hidden",
                            div {
                                class: "h-full rounded-full",
                                style: "width: {(i + 1) * 20}%; background-color: {dot_bg};",
                            }
                        }
                        span { class: "text-[9px] text-zinc-500 font-mono w-8 text-right",
                            "{(i + 1) * 20}%"
                        }
                    }
                }
            }
            p { class: "text-[9px] text-zinc-600 italic mt-2",
                "Full parameter list will be populated from the plugin's exposed parameters"
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct AdvancedTabProps {
    slot: CompositionSlot,
    sub_tab: AdvancedSubTab,
}

/// Advanced tab: raw parameter table or chunk data.
#[component]
fn AdvancedTabContent(props: AdvancedTabProps) -> Element {
    let color = block_type_color(props.slot.block_type);
    let dot_bg = color.bg;
    let name = props
        .slot
        .block_preset_name
        .as_deref()
        .unwrap_or(props.slot.block_type.display_name());

    rsx! {
        div { class: "flex flex-col gap-3",
            div { class: "flex items-center gap-2",
                div {
                    class: "w-3 h-3 rounded-full flex-shrink-0",
                    style: "background-color: {dot_bg};",
                }
                span { class: "text-xs font-semibold text-zinc-200", "{name}" }
                span { class: "text-[10px] text-zinc-500",
                    if props.sub_tab == AdvancedSubTab::Parameters {
                        "Raw Parameters"
                    } else {
                        "State Chunk"
                    }
                }
            }
            match props.sub_tab {
                AdvancedSubTab::Parameters => rsx! {
                    // Raw parameter table
                    div { class: "border border-zinc-800/50 rounded-lg overflow-hidden",
                        // Header
                        div { class: "grid grid-cols-4 gap-px bg-zinc-800/30 px-3 py-1.5",
                            span { class: "text-[9px] font-semibold text-zinc-500", "Index" }
                            span { class: "text-[9px] font-semibold text-zinc-500", "Name" }
                            span { class: "text-[9px] font-semibold text-zinc-500", "Value" }
                            span { class: "text-[9px] font-semibold text-zinc-500", "Range" }
                        }
                        // Placeholder rows
                        for i in 0..4u32 {
                            div { class: "grid grid-cols-4 gap-px px-3 py-1 border-t border-zinc-800/30 hover:bg-zinc-800/20",
                                span { class: "text-[9px] font-mono text-zinc-500", "{i}" }
                                span { class: "text-[9px] text-zinc-400", "Param {i}" }
                                span { class: "text-[9px] font-mono text-zinc-300", "0.50" }
                                span { class: "text-[9px] font-mono text-zinc-600", "0.0 - 1.0" }
                            }
                        }
                    }
                    p { class: "text-[9px] text-zinc-600 italic",
                        "Raw parameter table will show all plugin parameters with their current values"
                    }
                },
                AdvancedSubTab::Chunk => rsx! {
                    // Chunk data display
                    div { class: "border border-zinc-800/50 rounded-lg p-3 bg-zinc-950/50",
                        pre { class: "text-[9px] font-mono text-zinc-500 whitespace-pre-wrap break-all leading-relaxed",
                            "// State chunk data will be displayed here\n\
                             // when a plugin is loaded into this block.\n\
                             //\n\
                             // This shows the raw REAPER FX state chunk,\n\
                             // useful for debugging and advanced editing."
                        }
                    }
                },
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Module-level tab content
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct ModuleProps {
    module_name: String,
    blocks: Vec<CompositionSlot>,
}

/// Module Overview tab: shows module type, block count, and grid position.
#[component]
fn ModuleOverviewContent(props: ModuleProps) -> Element {
    let block_count = props.blocks.len();
    let first = props.blocks.first();
    let module_type_name = first
        .and_then(|s| s.module_type)
        .map(|mt| mt.display_name().to_string())
        .unwrap_or_else(|| "Custom".to_string());
    let color = first.map(|s| block_type_color(s.block_type));
    let dot_bg = color.as_ref().map(|c| c.bg).unwrap_or("#666");

    // Compute grid span
    let min_col = props.blocks.iter().map(|s| s.col).min().unwrap_or(0);
    let max_col = props.blocks.iter().map(|s| s.col).max().unwrap_or(0);
    let min_row = props.blocks.iter().map(|s| s.row).min().unwrap_or(0);
    let max_row = props.blocks.iter().map(|s| s.row).max().unwrap_or(0);
    let cols_span = max_col - min_col + 1;
    let rows_span = max_row - min_row + 1;

    rsx! {
        div { class: "flex flex-col gap-4",
            // Module header
            div { class: "flex items-center gap-3",
                div {
                    class: "w-4 h-4 rounded-md flex-shrink-0",
                    style: "background-color: {dot_bg};",
                }
                div {
                    span { class: "text-sm font-semibold text-zinc-100 block", "{props.module_name}" }
                    span { class: "text-[10px] text-zinc-500", "Module \u{00b7} {module_type_name}" }
                }
            }

            // Stats grid
            div { class: "grid grid-cols-3 gap-3",
                // Block count
                div { class: "flex flex-col gap-0.5 px-3 py-2 rounded-lg bg-zinc-800/30 border border-zinc-800/40",
                    span { class: "text-[9px] font-bold text-zinc-500 uppercase tracking-wide", "Blocks" }
                    span { class: "text-lg font-semibold text-zinc-200", "{block_count}" }
                }
                // Grid span
                div { class: "flex flex-col gap-0.5 px-3 py-2 rounded-lg bg-zinc-800/30 border border-zinc-800/40",
                    span { class: "text-[9px] font-bold text-zinc-500 uppercase tracking-wide", "Grid Size" }
                    span { class: "text-lg font-semibold text-zinc-200", "{cols_span}\u{00d7}{rows_span}" }
                }
                // Position
                div { class: "flex flex-col gap-0.5 px-3 py-2 rounded-lg bg-zinc-800/30 border border-zinc-800/40",
                    span { class: "text-[9px] font-bold text-zinc-500 uppercase tracking-wide", "Position" }
                    span { class: "text-lg font-semibold text-zinc-200",
                        "({min_col},{min_row})"
                    }
                }
            }

            // Block type breakdown
            div { class: "flex flex-col gap-1.5",
                span { class: "text-[9px] font-bold text-zinc-500 uppercase tracking-wide", "Signal Chain" }
                for slot in props.blocks.iter() {
                    {
                        let sc = block_type_color(slot.block_type);
                        let sname = slot.block_preset_name.as_deref()
                            .unwrap_or(slot.block_type.display_name());
                        rsx! {
                            div { class: "flex items-center gap-2 px-2 py-1 rounded-md hover:bg-zinc-800/20",
                                div {
                                    class: "w-2 h-2 rounded-full flex-shrink-0",
                                    style: "background-color: {sc.bg};",
                                }
                                span { class: "text-[10px] text-zinc-300 font-medium", "{sname}" }
                                span { class: "text-[9px] text-zinc-600 ml-auto font-mono",
                                    "({slot.col},{slot.row})"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Module Blocks tab: lists all blocks in the module with their types and positions.
#[component]
fn ModuleBlocksContent(props: ModuleProps) -> Element {
    let first = props.blocks.first();
    let dot_bg = first
        .map(|s| block_type_color(s.block_type).bg)
        .unwrap_or("#666");

    rsx! {
        div { class: "flex flex-col gap-3",
            div { class: "flex items-center gap-2",
                div {
                    class: "w-3 h-3 rounded-md flex-shrink-0",
                    style: "background-color: {dot_bg};",
                }
                span { class: "text-xs font-semibold text-zinc-200", "{props.module_name}" }
                span { class: "text-[10px] text-zinc-500", "{props.blocks.len()} blocks" }
            }
            // Block table
            div { class: "border border-zinc-800/50 rounded-lg overflow-hidden",
                // Header
                div { class: "grid grid-cols-4 gap-px bg-zinc-800/30 px-3 py-1.5",
                    span { class: "text-[9px] font-semibold text-zinc-500", "Block" }
                    span { class: "text-[9px] font-semibold text-zinc-500", "Type" }
                    span { class: "text-[9px] font-semibold text-zinc-500", "Plugin" }
                    span { class: "text-[9px] font-semibold text-zinc-500", "Position" }
                }
                for slot in props.blocks.iter() {
                    {
                        let sc = block_type_color(slot.block_type);
                        let sname = slot.block_preset_name.as_deref()
                            .unwrap_or(slot.block_type.display_name());
                        let plugin = slot.plugin_name.as_deref().unwrap_or("—");
                        rsx! {
                            div { class: "grid grid-cols-4 gap-px px-3 py-1.5 border-t border-zinc-800/30 hover:bg-zinc-800/20",
                                div { class: "flex items-center gap-1.5",
                                    div {
                                        class: "w-1.5 h-1.5 rounded-full flex-shrink-0",
                                        style: "background-color: {sc.bg};",
                                    }
                                    span { class: "text-[9px] text-zinc-300 font-medium truncate", "{sname}" }
                                }
                                span { class: "text-[9px] text-zinc-400", "{slot.block_type.display_name()}" }
                                span { class: "text-[9px] text-zinc-500 truncate", "{plugin}" }
                                span { class: "text-[9px] font-mono text-zinc-600", "({slot.col},{slot.row})" }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Module Advanced tab: module-level settings and configuration.
#[component]
fn ModuleAdvancedContent(props: ModuleProps) -> Element {
    let first = props.blocks.first();
    let module_type_name = first
        .and_then(|s| s.module_type)
        .map(|mt| mt.display_name().to_string())
        .unwrap_or_else(|| "Custom".to_string());
    let dot_bg = first
        .map(|s| block_type_color(s.block_type).bg)
        .unwrap_or("#666");

    rsx! {
        div { class: "flex flex-col gap-3",
            div { class: "flex items-center gap-2",
                div {
                    class: "w-3 h-3 rounded-md flex-shrink-0",
                    style: "background-color: {dot_bg};",
                }
                span { class: "text-xs font-semibold text-zinc-200", "{props.module_name}" }
                span { class: "text-[10px] text-zinc-500", "Advanced \u{00b7} {module_type_name}" }
            }
            // Module-level settings
            div { class: "flex flex-col gap-2",
                div { class: "flex items-center gap-3 px-3 py-2 rounded-lg bg-zinc-800/20 border border-zinc-800/40",
                    span { class: "text-[10px] text-zinc-400 w-28 flex-shrink-0", "Module Type" }
                    span { class: "text-[10px] text-zinc-200 font-medium", "{module_type_name}" }
                }
                div { class: "flex items-center gap-3 px-3 py-2 rounded-lg bg-zinc-800/20 border border-zinc-800/40",
                    span { class: "text-[10px] text-zinc-400 w-28 flex-shrink-0", "Block Count" }
                    span { class: "text-[10px] text-zinc-200 font-medium", "{props.blocks.len()}" }
                }
                div { class: "flex items-center gap-3 px-3 py-2 rounded-lg bg-zinc-800/20 border border-zinc-800/40",
                    span { class: "text-[10px] text-zinc-400 w-28 flex-shrink-0", "Bypass" }
                    span { class: "text-[10px] text-zinc-500 italic", "Not implemented" }
                }
                div { class: "flex items-center gap-3 px-3 py-2 rounded-lg bg-zinc-800/20 border border-zinc-800/40",
                    span { class: "text-[10px] text-zinc-400 w-28 flex-shrink-0", "Wet/Dry Mix" }
                    span { class: "text-[10px] text-zinc-500 italic", "Not implemented" }
                }
            }
            p { class: "text-[9px] text-zinc-600 italic mt-2",
                "Module-level bypass, wet/dry, and routing settings will be configurable here"
            }
        }
    }
}
