//! Preset Editor View — compose modules into full rig configurations.
//!
//! Left: Preset browser with search and category filter
//! Center: Visual multi-module grid editor showing each module as a colored
//!         container with block previews inside. Modules can be dragged to
//!         reorder in the signal chain.
//! Right: Quick guide / module library
//!
//! Presets are the top-level entity: they assign module presets to each
//! module type slot in the signal chain, and store named snapshots
//! (Verse, Chorus, Solo) for scene-level recall.

use crate::components::rig_grid::block_colors::{block_type_color, BlockColor};
use crate::components::shared::EntityEditor;
use crate::prelude::*;
use crate::signals::{RIG_AVAILABLE_PRESETS, RIG_SERVICE};
use signal_control::block::BlockType;
use signal_control::defaults::templates;
use signal_control::module::ModuleType;
use signal_control::template::RigTemplate;
use tracing::{debug, info, warn};
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// Module chain definition (signal flow order)
// ─────────────────────────────────────────────────────────────────────────────

struct ChainSlotDef {
    module_type: ModuleType,
    name: &'static str,
    color: &'static str,
    icon: &'static str,
}

fn signal_chain_order() -> Vec<ChainSlotDef> {
    vec![
        ChainSlotDef {
            module_type: ModuleType::Eq,
            name: "EQ",
            color: "#22C55E",
            icon: "E",
        },
        ChainSlotDef {
            module_type: ModuleType::Dynamics,
            name: "Dynamics",
            color: "#3B82F6",
            icon: "C",
        },
        ChainSlotDef {
            module_type: ModuleType::Drive,
            name: "Drive",
            color: "#F97316",
            icon: "D",
        },
        ChainSlotDef {
            module_type: ModuleType::Amp,
            name: "Amp",
            color: "#EAB308",
            icon: "A",
        },
        ChainSlotDef {
            module_type: ModuleType::PostEq,
            name: "Post EQ",
            color: "#22C55E",
            icon: "Q",
        },
        ChainSlotDef {
            module_type: ModuleType::Modulation,
            name: "Modulation",
            color: "#A855F7",
            icon: "M",
        },
        ChainSlotDef {
            module_type: ModuleType::Time,
            name: "Time",
            color: "#06B6D4",
            icon: "T",
        },
        ChainSlotDef {
            module_type: ModuleType::Motion,
            name: "Motion",
            color: "#C084FC",
            icon: "W",
        },
        ChainSlotDef {
            module_type: ModuleType::Special,
            name: "Special",
            color: "#EC4899",
            icon: "S",
        },
        ChainSlotDef {
            module_type: ModuleType::Master,
            name: "Master",
            color: "#6B7280",
            icon: "O",
        },
    ]
}

// ─────────────────────────────────────────────────────────────────────────────
// Module type color (same mapping as module_editor_view.rs)
// ─────────────────────────────────────────────────────────────────────────────

fn module_type_color(mt: ModuleType) -> BlockColor {
    let bt = match mt {
        ModuleType::Drive => BlockType::Drive,
        ModuleType::Amp => BlockType::Amp,
        ModuleType::Eq | ModuleType::PostEq => BlockType::Eq,
        ModuleType::Dynamics => BlockType::Compressor,
        ModuleType::Modulation | ModuleType::VocalModulation => BlockType::Modulation,
        ModuleType::Time => BlockType::Delay,
        ModuleType::Motion => BlockType::Tremolo,
        ModuleType::Special | ModuleType::PreFx => BlockType::Special,
        ModuleType::Master => BlockType::Volume,
        _ => BlockType::Custom,
    };
    block_type_color(bt)
}

/// Parse a block_type string from JSON into a BlockType enum.
fn parse_block_type(s: &str) -> BlockType {
    match s {
        "Eq" => BlockType::Eq,
        "Compressor" => BlockType::Compressor,
        "Drive" => BlockType::Drive,
        "Amp" => BlockType::Amp,
        "Cabinet" => BlockType::Cabinet,
        "Delay" => BlockType::Delay,
        "Reverb" => BlockType::Reverb,
        "Modulation" => BlockType::Modulation,
        "Chorus" => BlockType::Chorus,
        "Flanger" => BlockType::Flanger,
        "Phaser" => BlockType::Phaser,
        "Tremolo" => BlockType::Tremolo,
        "Vibrato" => BlockType::Vibrato,
        "Rotary" => BlockType::Rotary,
        "Pitch" => BlockType::Pitch,
        "Gate" => BlockType::Gate,
        "Limiter" => BlockType::Limiter,
        "Volume" => BlockType::Volume,
        "Boost" => BlockType::Boost,
        "Wah" => BlockType::Wah,
        "Filter" => BlockType::Filter,
        "Freeze" => BlockType::Freeze,
        "Special" => BlockType::Special,
        "Doubler" => BlockType::Doubler,
        "Crossover" => BlockType::Crossover,
        "Send" => BlockType::Send,
        _ => BlockType::Custom,
    }
}

/// A parsed block from a module preset's JSON, for display purposes only.
#[derive(Clone)]
struct PreviewBlock {
    name: String,
    block_type: BlockType,
    is_placeholder: bool,
}

/// Parse the blocks JSON from a module preset model into preview blocks.
fn parse_preview_blocks(model: &signal_control::module_preset_entity::Model) -> Vec<PreviewBlock> {
    // Support both legacy (array) and current (object with "blocks" key) formats
    let block_array = if model.blocks.is_object() {
        model
            .blocks
            .get("blocks")
            .and_then(|v| v.as_array())
            .cloned()
            .unwrap_or_default()
    } else {
        model.blocks.as_array().cloned().unwrap_or_default()
    };

    block_array
        .iter()
        .map(|b| {
            let block_type_str = b
                .get("block")
                .and_then(|bl| bl.get("block_type"))
                .and_then(|v| v.as_str())
                .unwrap_or("Custom");
            let name = b
                .get("block")
                .and_then(|bl| bl.get("name"))
                .and_then(|v| v.as_str())
                .unwrap_or("?")
                .to_string();
            let is_placeholder = b
                .get("block")
                .and_then(|bl| bl.get("is_placeholder"))
                .and_then(|v| v.as_bool())
                .unwrap_or(false);
            PreviewBlock {
                name,
                block_type: parse_block_type(block_type_str),
                is_placeholder,
            }
        })
        .collect()
}

// ─────────────────────────────────────────────────────────────────────────────
// Module drag state
// ─────────────────────────────────────────────────────────────────────────────

/// Tracks an in-progress module container drag for reordering.
#[derive(Debug, Clone, PartialEq)]
struct ModuleDragState {
    /// Index in the signal_chain_order being dragged.
    origin_idx: usize,
    /// Display name of the module type.
    module_name: String,
    /// Current hover target index.
    hover_idx: Option<usize>,
    /// Starting mouse Y.
    start_y: f64,
    /// Current mouse Y.
    mouse_y: f64,
}

// ─────────────────────────────────────────────────────────────────────────────
// Main Component
// ─────────────────────────────────────────────────────────────────────────────

#[component]
pub fn PresetEditorView() -> Element {
    // All editor state is component-local — fresh on each mount, no stale globals.
    let mut preset_selected_id = use_signal(|| None::<Uuid>);
    let mut preset_list = use_signal(Vec::<signal_control::preset_entity::Model>::new);
    let mut preset_snapshots = use_signal(Vec::<signal_control::snapshot_entity::Model>::new);
    let mut available_module_presets =
        use_signal(Vec::<signal_control::module_preset_entity::Model>::new);
    let mut module_assignments = use_signal(std::collections::HashMap::<String, Uuid>::new);
    let mut preset_editor_status = use_signal(|| "Select a preset".to_string());
    let mut preset_search = use_signal(String::new);
    let mut chain_order_sig = use_signal(Vec::<usize>::new);

    // ── Async Refresh Helpers (capture local signals) ────────────────────────

    let refresh_preset_list = move || {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_rig_presets().await {
                Ok(presets) => preset_list.set(presets),
                Err(e) => warn!("Failed to load presets: {e}"),
            }
        })
    };

    let refresh_preset_snapshots = move |preset_id: Uuid| {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_rig_preset_snapshots(preset_id).await {
                Ok(snaps) => preset_snapshots.set(snaps),
                Err(e) => warn!("Failed to load preset snapshots: {e}"),
            }
        })
    };

    let refresh_available_modules = move || {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_module_presets(None).await {
                Ok(mods) => available_module_presets.set(mods),
                Err(e) => warn!("Failed to load module presets: {e}"),
            }
        })
    };

    // Load on mount
    use_effect(move || {
        refresh_preset_list();
        refresh_available_modules();
    });

    // Re-fetch local preset list whenever the global available presets signal changes
    // (e.g. after CreateEntityModal creates a new preset via rig_actions)
    {
        let global_presets_len = RIG_AVAILABLE_PRESETS.read().len();
        use_effect(move || {
            let _ = global_presets_len; // reactive dependency
            refresh_preset_list();
        });
    }

    // Clone data out of signals so read guards are dropped before event handlers.
    let selected_id = *preset_selected_id.read();
    let presets = preset_list.cloned();
    let snapshots = preset_snapshots.cloned();
    let available_modules = available_module_presets.cloned();
    let assignments = module_assignments.cloned();
    let status = preset_editor_status.cloned();
    let search_text = preset_search.cloned();
    let chain_order = chain_order_sig.cloned();

    let mut show_new_dialog = use_signal(|| false);
    let mut new_name = use_signal(String::new);
    let mut show_rename_dialog = use_signal(|| false);
    let mut rename_value = use_signal(String::new);

    // Template selection: None = blank, Some(idx) = template index
    // 0 = Guitar Rig, 1 = Vocal Rig
    let mut selected_template_idx = use_signal(|| None::<usize>);

    // Module drag state for reordering
    let mut module_drag = use_signal(|| None::<ModuleDragState>);

    // Expanded module for inline assignment picker
    let mut expanded_module = use_signal(|| None::<String>);

    // Filter presets by search
    let filtered_presets: Vec<_> = presets
        .iter()
        .filter(|p| {
            if search_text.is_empty() {
                return true;
            }
            let q = search_text.to_lowercase();
            p.name.to_lowercase().contains(&q)
        })
        .cloned()
        .collect();

    let selected_preset = selected_id.and_then(|id| presets.iter().find(|p| p.id == id).cloned());

    // Build ordered chain: use custom order or default
    let default_chain = signal_chain_order();
    let chain_len = default_chain.len();
    let ordered_indices: Vec<usize> = if chain_order.is_empty() {
        (0..chain_len).collect()
    } else {
        chain_order.clone()
    };

    // Module drag cursor
    let cursor = if module_drag().is_some() {
        "grabbing"
    } else {
        "default"
    };

    rsx! {
        EntityEditor {
            accent_gradient: Some("from-amber-500 via-rose-400 to-violet-500".to_string()),
            left_width: "w-60".to_string(),
            right_width: "w-56".to_string(),
            left: rsx! {
                    // Header + search
                    div { class: "px-3 py-3 border-b border-border/30 flex-shrink-0",
                        div { class: "flex items-center justify-between mb-2",
                            h2 { class: "text-[11px] font-bold text-zinc-400 uppercase tracking-[0.15em]",
                                "Rig Presets"
                            }
                            button {
                                class: "flex items-center gap-1 px-2 py-1 rounded-md text-[9px] font-semibold \
                                        bg-amber-500/15 text-amber-300 border border-amber-500/25 \
                                        hover:bg-amber-500/25 transition-all duration-150",
                                onclick: move |_| {
                                    new_name.set(String::new());
                                    selected_template_idx.set(None);
                                    show_new_dialog.set(true);
                                },
                                "+"
                            }
                        }
                        input {
                            class: "w-full bg-zinc-800/60 border border-zinc-700/40 rounded-md px-2.5 py-1.5 text-[11px] text-zinc-300 \
                                    outline-none focus:border-amber-500/30 placeholder:text-zinc-600 transition-all",
                            r#type: "text",
                            placeholder: "Search presets...",
                            value: "{search_text}",
                            oninput: move |evt| preset_search.set(evt.value().clone()),
                        }
                    }

                    // New preset dialog with template selection
                    if show_new_dialog() {
                        div { class: "px-3 py-2 border-b border-border/30 bg-zinc-900/60 flex flex-col gap-2 flex-shrink-0",
                            // Template cards
                            span { class: "text-[9px] font-semibold text-zinc-500 uppercase tracking-wider", "Template" }
                            div { class: "flex gap-1.5",
                                // Blank
                                button {
                                    class: if selected_template_idx().is_none() {
                                        "flex-1 flex flex-col items-center gap-1 px-2 py-2 rounded-lg border-2 \
                                         border-amber-500/50 bg-amber-500/10 transition-all"
                                    } else {
                                        "flex-1 flex flex-col items-center gap-1 px-2 py-2 rounded-lg border \
                                         border-zinc-700/40 bg-zinc-800/40 hover:border-zinc-600/50 transition-all"
                                    },
                                    onclick: move |_| {
                                        selected_template_idx.set(None);
                                        if new_name().is_empty() {
                                            new_name.set(String::new());
                                        }
                                    },
                                    span { class: "text-sm", "\u{2610}" }
                                    span { class: "text-[9px] font-medium text-zinc-300", "Blank" }
                                }
                                // Guitar Rig
                                button {
                                    class: if selected_template_idx() == Some(0) {
                                        "flex-1 flex flex-col items-center gap-1 px-2 py-2 rounded-lg border-2 \
                                         border-amber-500/50 bg-amber-500/10 transition-all"
                                    } else {
                                        "flex-1 flex flex-col items-center gap-1 px-2 py-2 rounded-lg border \
                                         border-zinc-700/40 bg-zinc-800/40 hover:border-zinc-600/50 transition-all"
                                    },
                                    onclick: move |_| {
                                        selected_template_idx.set(Some(0));
                                        if new_name().is_empty() {
                                            new_name.set("Guitar Rig".to_string());
                                        }
                                    },
                                    span { class: "text-sm", "\u{1F3B8}" }
                                    span { class: "text-[9px] font-medium text-zinc-300", "Guitar" }
                                    span { class: "text-[8px] text-zinc-500", "11 modules" }
                                }
                                // Vocal Rig
                                button {
                                    class: if selected_template_idx() == Some(1) {
                                        "flex-1 flex flex-col items-center gap-1 px-2 py-2 rounded-lg border-2 \
                                         border-amber-500/50 bg-amber-500/10 transition-all"
                                    } else {
                                        "flex-1 flex flex-col items-center gap-1 px-2 py-2 rounded-lg border \
                                         border-zinc-700/40 bg-zinc-800/40 hover:border-zinc-600/50 transition-all"
                                    },
                                    onclick: move |_| {
                                        selected_template_idx.set(Some(1));
                                        if new_name().is_empty() {
                                            new_name.set("Vocal Rig".to_string());
                                        }
                                    },
                                    span { class: "text-sm", "\u{1F3A4}" }
                                    span { class: "text-[9px] font-medium text-zinc-300", "Vocal" }
                                    span { class: "text-[8px] text-zinc-500", "5 modules" }
                                }
                            }

                            // Name input
                            input {
                                class: "w-full bg-zinc-800/80 border border-zinc-700/50 rounded-md px-2.5 py-1.5 text-xs text-zinc-200 \
                                        outline-none focus:border-amber-500/40 placeholder:text-zinc-600",
                                r#type: "text",
                                placeholder: "Preset name...",
                                value: "{new_name}",
                                autofocus: true,
                                oninput: move |evt| new_name.set(evt.value().clone()),
                                onkeydown: move |evt| {
                                    if evt.key() == Key::Escape {
                                        show_new_dialog.set(false);
                                    }
                                },
                            }

                            // Create / Cancel buttons
                            div { class: "flex gap-1",
                                button {
                                    class: "flex-1 px-2 py-1 rounded text-[9px] font-semibold bg-amber-500/80 text-white \
                                            hover:bg-amber-500 transition-colors disabled:opacity-25",
                                    disabled: new_name().trim().is_empty(),
                                    onclick: move |_| {
                                        let val = new_name().trim().to_string();
                                        if val.is_empty() { return; }
                                        let tmpl_idx = selected_template_idx();
                                        show_new_dialog.set(false);
                                        spawn(async move {
                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return; };

                                            // Build template if selected
                                            let template: Option<RigTemplate> = match tmpl_idx {
                                                Some(0) => Some(templates::guitar_rig_template()),
                                                Some(1) => Some(templates::vocal_rig_template()),
                                                _ => None,
                                            };

                                            if let Some(tmpl) = template {
                                                // Instantiate template → domain objects
                                                let (mut preset, module_presets): (signal_control::preset::Preset, Vec<signal_control::module_preset::ModulePreset>) = tmpl.instantiate();
                                                preset.name = val.clone();

                                                // Create each module preset in the DB
                                                for mp in &module_presets {
                                                    let blocks_json = serde_json::json!(
                                                        mp.blocks.iter().map(|mb| {
                                                            serde_json::json!({
                                                                "block": {
                                                                    "name": mb.block.name,
                                                                    "block_type": format!("{:?}", mb.block.block_type),
                                                                    "alias": mb.block.alias,
                                                                    "description": mb.block.description,
                                                                    "is_placeholder": mb.block.is_placeholder(),
                                                                }
                                                            })
                                                        }).collect::<Vec<_>>()
                                                    );
                                                    if let Err(e) = ctl.create_module_preset(
                                                        &mp.name,
                                                        mp.module_type.display_name(),
                                                        mp.description.as_deref(),
                                                        blocks_json,
                                                        serde_json::json!([]),
                                                    ).await {
                                                        warn!("Failed to create module preset '{}': {e}", mp.name);
                                                    }
                                                }

                                                // Create the rig preset
                                                match ctl.create_rig_preset::<signal_control::preset::Preset>(
                                                    &val,
                                                    preset.description.as_deref(),
                                                    serde_json::json!("Clean"),
                                                    serde_json::json!([]),
                                                    &preset,
                                                ).await {
                                                    Ok(id) => {
                                                        info!("Created rig preset from template: {id}");
                                                        preset_editor_status.set(format!("Created '{}' from template", val));
                                                        if let Ok(list) = ctl.list_rig_presets().await {
                                                            preset_list.set(list);
                                                        }
                                                        preset_selected_id.set(Some(id));
                                                        preset_snapshots.set(Vec::new());
                                                        module_assignments.set(std::collections::HashMap::new());
                                                    }
                                                    Err(e) => {
                                                        warn!("Create preset from template failed: {e}");
                                                        preset_editor_status.set(format!("Failed: {e}"));
                                                    }
                                                }
                                            } else {
                                                // Blank preset (original behavior)
                                                match ctl.create_rig_preset::<signal_control::preset::Preset>(
                                                    &val, None,
                                                    serde_json::json!("Clean"),
                                                    serde_json::json!([]),
                                                    &signal_control::preset::Preset::new(
                                                        &val,
                                                        signal_control::category::PresetCategory::default(),
                                                    ),
                                                ).await {
                                                    Ok(id) => {
                                                        info!("Created blank rig preset: {id}");
                                                        preset_editor_status.set(format!("Created '{}'", val));
                                                        if let Ok(list) = ctl.list_rig_presets().await {
                                                            preset_list.set(list);
                                                        }
                                                        preset_selected_id.set(Some(id));
                                                        preset_snapshots.set(Vec::new());
                                                        module_assignments.set(std::collections::HashMap::new());
                                                    }
                                                    Err(e) => {
                                                        warn!("Create preset failed: {e}");
                                                        preset_editor_status.set(format!("Failed: {e}"));
                                                    }
                                                }
                                            }

                                            // Refresh module list since templates create module presets
                                            if let Ok(mods) = ctl.list_module_presets(None).await {
                                                available_module_presets.set(mods);
                                            }
                                        });
                                    },
                                    "Create"
                                }
                                button {
                                    class: "px-2 py-1 rounded text-[9px] text-zinc-500 hover:text-zinc-300 transition-colors",
                                    onclick: move |_| show_new_dialog.set(false),
                                    "Cancel"
                                }
                            }
                        }
                    }

                    // Preset list
                    div { class: "flex-1 overflow-y-auto min-h-0 px-2 py-1.5",
                        if filtered_presets.is_empty() {
                            div { class: "flex items-center justify-center h-full",
                                p { class: "text-xs text-zinc-600",
                                    if presets.is_empty() { "No presets yet" } else { "No matches" }
                                }
                            }
                        } else {
                            for preset in filtered_presets.iter() {
                                {
                                    let pid = preset.id;
                                    let is_selected = selected_id == Some(pid);
                                    let pname = preset.name.clone();
                                    let is_fav = preset.is_favorite;

                                    rsx! {
                                        div {
                                            key: "{pid}",
                                            class: if is_selected {
                                                "flex items-center gap-2 px-3 py-2 rounded-lg transition-all duration-100 cursor-pointer \
                                                 bg-zinc-800/70 border border-zinc-600/40"
                                            } else {
                                                "flex items-center gap-2 px-3 py-2 rounded-lg transition-all duration-100 cursor-pointer \
                                                 hover:bg-zinc-800/30 border border-transparent"
                                            },
                                            onclick: move |_| {
                                                info!("Preset clicked: {pid}");
                                                preset_selected_id.set(Some(pid));
                                                module_assignments.set(std::collections::HashMap::new());
                                                chain_order_sig.set(Vec::new());
                                                refresh_preset_snapshots(pid);

                                                // Load module assignments from preset data JSON
                                                spawn(async move {
                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                    match ctl.get_rig_preset_row(pid).await {
                                                        Ok(Some(row)) => {
                                                            info!("Loading preset data for {pid}: {}", row.data);
                                                            // Try to extract module_assignments from the data JSON
                                                            if let Some(data) = row.data.as_object() {
                                                                if let Some(assigns) = data.get("module_assignments").and_then(|v| v.as_array()) {
                                                                    let mut map = std::collections::HashMap::new();
                                                                    for a in assigns {
                                                                        if let (Some(mt), Some(mp_id)) = (
                                                                            a.get("module_type").and_then(|v| v.as_str()),
                                                                            a.get("module_preset_id").and_then(|v| v.as_str()),
                                                                        ) {
                                                                            if let Ok(id) = Uuid::parse_str(mp_id) {
                                                                                map.insert(mt.to_string(), id);
                                                                            }
                                                                        }
                                                                    }
                                                                    info!("Loaded {} module assignments for preset {pid}", map.len());
                                                                    if !map.is_empty() {
                                                                        module_assignments.set(map);
                                                                    }
                                                                } else {
                                                                    info!("No module_assignments field in preset {pid} data");
                                                                }
                                                            } else {
                                                                info!("Preset {pid} data is not an object: {}", row.data);
                                                            }
                                                        }
                                                        Ok(None) => {
                                                            warn!("Preset {pid} not found in database");
                                                        }
                                                        Err(e) => {
                                                            warn!("Failed to load preset {pid}: {e}");
                                                        }
                                                    }
                                                });
                                            },
                                            div {
                                                class: if is_selected {
                                                    "w-1.5 h-1.5 rounded-full bg-amber-400 flex-shrink-0"
                                                } else {
                                                    "w-1.5 h-1.5 rounded-full bg-zinc-700 flex-shrink-0"
                                                },
                                            }
                                            div { class: "flex-1 min-w-0",
                                                span { class: "text-xs font-medium text-zinc-200 truncate block",
                                                    "{pname}"
                                                }
                                            }
                                            if is_fav {
                                                span { class: "text-[9px] text-amber-400 flex-shrink-0", "\u{2605}" }
                                            }
                                            if is_selected {
                                                div { class: "flex items-center gap-0.5 flex-shrink-0",
                                                    button {
                                                        class: "p-1 rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-700/50 transition-colors",
                                                        title: "Rename",
                                                        onclick: move |evt| {
                                                            evt.stop_propagation();
                                                            rename_value.set(preset_list.read().iter().find(|p| p.id == pid).map(|p| p.name.clone()).unwrap_or_default());
                                                            show_rename_dialog.set(true);
                                                        },
                                                        span { class: "text-[10px]", "\u{270E}" }
                                                    }
                                                    button {
                                                        class: "p-1 rounded text-zinc-500 hover:text-red-400 hover:bg-red-500/10 transition-colors",
                                                        title: "Delete",
                                                        onclick: move |evt| {
                                                            evt.stop_propagation();
                                                            spawn(async move {
                                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                                if let Err(e) = ctl.delete_rig_preset(pid).await {
                                                                    warn!("Delete failed: {e}");
                                                                } else {
                                                                    preset_editor_status.set("Preset deleted".into());
                                                                    if *preset_selected_id.read() == Some(pid) {
                                                                        preset_selected_id.set(None);
                                                                        preset_snapshots.set(Vec::new());
                                                                    }
                                                                    if let Ok(list) = ctl.list_rig_presets().await {
                                                                        preset_list.set(list);
                                                                    }
                                                                }
                                                            });
                                                        },
                                                        span { class: "text-[10px]", "\u{2715}" }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
            },
            center: rsx! {
                    if selected_preset.is_none() {
                        div { class: "flex-1 flex items-center justify-center",
                            div { class: "text-center max-w-xs",
                                div { class: "w-12 h-12 rounded-xl bg-zinc-800/60 border border-zinc-700/40 flex items-center justify-center mx-auto mb-4",
                                    span { class: "text-xl text-zinc-600", "\u{266B}" }
                                }
                                p { class: "text-sm font-medium text-zinc-400 mb-1", "Select a Preset" }
                                p { class: "text-xs text-zinc-600 leading-relaxed",
                                    "Choose a preset from the left or create a new one to configure its module assignments and snapshots"
                                }
                            }
                        }
                    } else {
                        // Rename dialog
                        if show_rename_dialog() {
                            div { class: "px-4 py-2.5 border-b border-border/30 bg-zinc-900/60 flex items-center gap-2.5 flex-shrink-0",
                                span { class: "text-[10px] text-zinc-500 font-medium", "Rename:" }
                                input {
                                    class: "flex-1 bg-zinc-800/80 border border-zinc-700/50 rounded-md px-2.5 py-1.5 text-xs text-zinc-200 \
                                            outline-none focus:border-amber-500/40 placeholder:text-zinc-600",
                                    r#type: "text",
                                    value: "{rename_value}",
                                    autofocus: true,
                                    oninput: move |evt| rename_value.set(evt.value().clone()),
                                    onkeydown: move |evt| {
                                        if evt.key() == Key::Enter {
                                            let val = rename_value().trim().to_string();
                                            if !val.is_empty() {
                                                show_rename_dialog.set(false);
                                                if let Some(pid) = selected_id {
                                                    spawn(async move {
                                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                        if let Err(e) = ctl.update_rig_preset_metadata(pid, Some(&val), None, None, None, None).await {
                                                            warn!("Rename failed: {e}");
                                                        } else if let Ok(list) = ctl.list_rig_presets().await {
                                                            preset_list.set(list);
                                                        }
                                                    });
                                                }
                                            }
                                        } else if evt.key() == Key::Escape {
                                            show_rename_dialog.set(false);
                                        }
                                    },
                                }
                                button {
                                    class: "px-3 py-1.5 rounded-md text-[10px] font-semibold bg-amber-500/80 text-white hover:bg-amber-500 transition-colors",
                                    onclick: move |_| {
                                        let val = rename_value().trim().to_string();
                                        show_rename_dialog.set(false);
                                        if let Some(pid) = selected_id {
                                            spawn(async move {
                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                let _ = ctl.update_rig_preset_metadata(pid, Some(&val), None, None, None, None).await;
                                                if let Ok(list) = ctl.list_rig_presets().await {
                                                    preset_list.set(list);
                                                }
                                            });
                                        }
                                    },
                                    "Save"
                                }
                                button {
                                    class: "px-2 py-1.5 rounded-md text-[10px] text-zinc-500 hover:text-zinc-300 transition-colors",
                                    onclick: move |_| show_rename_dialog.set(false),
                                    "Cancel"
                                }
                            }
                        }

                        // ── Visual Module Grid (top ~60%) ────────
                        div {
                            class: "h-[60%] flex flex-col min-h-0 border-b border-border/20 flex-shrink-0",
                            style: "cursor: {cursor};",

                            // Header
                            div { class: "px-4 py-2.5 border-b border-border/30 flex items-center gap-3 flex-shrink-0 bg-zinc-900/30",
                                if let Some(ref p) = selected_preset {
                                    span { class: "text-xs font-bold text-zinc-200", "{p.name}" }
                                }
                                span { class: "text-[10px] font-bold text-zinc-500 uppercase tracking-[0.1em]",
                                    "Signal Chain"
                                }
                                div { class: "flex-1" }
                                span { class: "text-[9px] text-zinc-600", "drag modules to reorder \u{2022} click to assign" }
                            }

                            // Module grid — scrollable area with drag support
                            div {
                                class: "flex-1 overflow-y-auto min-h-0 px-3 py-3",

                                onmousemove: move |evt| {
                                    if let Some(mut d) = module_drag() {
                                        let my = evt.client_coordinates().y;
                                        d.mouse_y = my;
                                        // Compute hover target based on mouse delta
                                        // Each module card is ~80px tall with ~8px gap
                                        let card_pitch = 88.0;
                                        let delta = my - d.start_y;
                                        let idx_delta = (delta / card_pitch).round() as isize;
                                        let new_idx = (d.origin_idx as isize + idx_delta)
                                            .max(0)
                                            .min(chain_len as isize - 1) as usize;
                                        d.hover_idx = if new_idx != d.origin_idx {
                                            Some(new_idx)
                                        } else {
                                            None
                                        };
                                        module_drag.set(Some(d));
                                    }
                                },

                                onmouseup: move |_| {
                                    if let Some(d) = module_drag() {
                                        if let Some(target_idx) = d.hover_idx {
                                            // Reorder: swap the two positions in chain order
                                            let mut order = if chain_order_sig.read().is_empty() {
                                                (0..chain_len).collect::<Vec<_>>()
                                            } else {
                                                chain_order_sig.read().clone()
                                            };
                                            let item = order.remove(d.origin_idx);
                                            order.insert(target_idx, item);
                                            chain_order_sig.set(order);
                                        }
                                        module_drag.set(None);
                                    }
                                },

                                onmouseleave: move |_| {
                                    module_drag.set(None);
                                },

                                // Render each module slot as a colored container
                                div { class: "flex flex-col gap-2",
                                    for (visual_pos, &chain_idx) in ordered_indices.iter().enumerate() {
                                        {
                                            if chain_idx >= default_chain.len() {
                                                return rsx! {};
                                            }
                                            let slot_def = &default_chain[chain_idx];
                                            let mt = slot_def.module_type;
                                            let mt_name = slot_def.name;
                                            let icon = slot_def.icon;
                                            let mc = module_type_color(mt);
                                            let type_key = mt.display_name().to_string();

                                            let assigned_id = assignments.get(&type_key).copied();
                                            let assigned_model = assigned_id.and_then(|id| {
                                                available_modules.iter().find(|m| m.id == id)
                                            });
                                            let assigned_name = assigned_model.map(|m| m.name.clone());
                                            let preview_blocks = assigned_model
                                                .map(parse_preview_blocks)
                                                .unwrap_or_default();
                                            let all_blocks_placeholder = !preview_blocks.is_empty()
                                                && preview_blocks.iter().all(|b| b.is_placeholder);
                                            let any_block_placeholder = preview_blocks.iter().any(|b| b.is_placeholder);

                                            // Module presets available for this type
                                            let type_modules: Vec<_> = available_modules.iter()
                                                .filter(|m| m.module_type == type_key)
                                                .cloned()
                                                .collect();

                                            let is_being_dragged = module_drag().as_ref().map_or(false, |d| d.origin_idx == visual_pos);
                                            let drag_hover = module_drag().as_ref().and_then(|d| d.hover_idx);
                                            let is_drop_target = drag_hover == Some(visual_pos);
                                            let is_expanded = expanded_module() == Some(type_key.clone());

                                            // Container colors
                                            let container_bg = if is_being_dragged {
                                                format!(
                                                    "background-color: {}08; border-color: {}20; opacity: 0.4;",
                                                    mc.bg, mc.bg
                                                )
                                            } else if is_drop_target {
                                                format!(
                                                    "background-color: {}20; border-color: #22d3ee; box-shadow: 0 0 8px rgba(34,211,238,0.3);",
                                                    mc.bg
                                                )
                                            } else {
                                                format!(
                                                    "background-color: {}12; border-color: {}35; backdrop-filter: blur(4px);",
                                                    mc.bg, mc.bg
                                                )
                                            };
                                            let title_bg = format!(
                                                "background-color: {}25; border-bottom: 1px solid {}30;",
                                                mc.bg, mc.bg
                                            );

                                            // Connection line between modules
                                            let show_connector = visual_pos > 0;

                                            let type_key_for_expand = type_key.clone();
                                            let type_key_for_clear = type_key.clone();
                                            let type_key_for_assign = type_key.clone();

                                            rsx! {
                                                // Connection line
                                                if show_connector {
                                                    div { class: "flex justify-center -my-1",
                                                        div {
                                                            class: "w-px h-3",
                                                            style: "background-color: {mc.bg}40;",
                                                        }
                                                    }
                                                }

                                                // Module container card
                                                div {
                                                    key: "{type_key}-{visual_pos}",
                                                    class: if is_being_dragged {
                                                        "rounded-xl border-2 border-dashed flex flex-col overflow-hidden transition-all duration-150"
                                                    } else {
                                                        "rounded-xl border-2 flex flex-col overflow-hidden transition-all duration-150 \
                                                         hover:brightness-110"
                                                    },
                                                    style: "{container_bg}",

                                                    // Title bar — draggable handle
                                                    div {
                                                        class: "flex items-center gap-2 px-3 select-none cursor-grab active:cursor-grabbing",
                                                        style: "height: 32px; {title_bg}",

                                                        onmousedown: move |evt| {
                                                            evt.stop_propagation();
                                                            module_drag.set(Some(ModuleDragState {
                                                                origin_idx: visual_pos,
                                                                module_name: mt_name.to_string(),
                                                                hover_idx: None,
                                                                start_y: evt.client_coordinates().y,
                                                                mouse_y: evt.client_coordinates().y,
                                                            }));
                                                        },

                                                        // Module type icon
                                                        div {
                                                            class: "w-5 h-5 rounded flex items-center justify-center text-[9px] font-bold flex-shrink-0",
                                                            style: "background-color: {mc.bg}30; color: {mc.fg};",
                                                            "{icon}"
                                                        }
                                                        // Module type name
                                                        span {
                                                            class: "text-[11px] font-semibold tracking-wide",
                                                            style: "color: {mc.fg};",
                                                            "{mt_name}"
                                                        }

                                                        // Assigned preset name
                                                        if let Some(ref aname) = assigned_name {
                                                            span {
                                                                class: "text-[10px] opacity-50",
                                                                style: "color: {mc.fg};",
                                                                "\u{2022}"
                                                            }
                                                            span {
                                                                class: "text-[10px] font-medium opacity-70 truncate",
                                                                style: "color: {mc.fg};",
                                                                "{aname}"
                                                            }
                                                        }

                                                        div { class: "flex-1" }

                                                        // Assign/change button
                                                        button {
                                                            class: "px-2 py-0.5 rounded text-[8px] font-semibold \
                                                                    hover:bg-white/10 transition-colors",
                                                            style: "color: {mc.fg}; opacity: 0.6;",
                                                            onclick: move |evt| {
                                                                evt.stop_propagation();
                                                                if is_expanded {
                                                                    expanded_module.set(None);
                                                                } else {
                                                                    expanded_module.set(Some(type_key_for_expand.clone()));
                                                                }
                                                            },
                                                            if assigned_name.is_some() { "change" } else { "assign" }
                                                        }

                                                        // Clear / Revert button (if assigned)
                                                        if assigned_id.is_some() {
                                                            button {
                                                                class: if any_block_placeholder {
                                                                    "px-1.5 py-0.5 rounded text-[8px] font-medium text-zinc-400/60 hover:text-zinc-300 \
                                                                     hover:bg-zinc-700/30 transition-colors"
                                                                } else {
                                                                    "px-1.5 py-0.5 rounded text-[8px] text-red-400/60 hover:text-red-400 \
                                                                     hover:bg-red-500/10 transition-colors"
                                                                },
                                                                onclick: move |evt| {
                                                                    evt.stop_propagation();
                                                                    module_assignments.write().remove(&type_key_for_clear);
                                                                },
                                                                if any_block_placeholder { "revert" } else { "\u{2715}" }
                                                            }
                                                        }
                                                    }

                                                    // Block preview area — mini block pills inside the module
                                                    if !preview_blocks.is_empty() {
                                                        div {
                                                            class: "flex items-center gap-1.5 px-3 py-2 flex-wrap",
                                                            for (bidx, block) in preview_blocks.iter().enumerate() {
                                                                {
                                                                    let bc = block_type_color(block.block_type);
                                                                    let pill_style = if block.is_placeholder {
                                                                        format!(
                                                                            "background-color: {}10; border-color: {}30; color: {}; opacity: 0.6;",
                                                                            bc.bg, bc.bg, bc.fg
                                                                        )
                                                                    } else {
                                                                        format!(
                                                                            "background-color: {}20; border-color: {}40; color: {};",
                                                                            bc.bg, bc.bg, bc.fg
                                                                        )
                                                                    };
                                                                    let dot_style = format!("background-color: {};", bc.bg);
                                                                    let pill_class = if block.is_placeholder {
                                                                        "flex items-center gap-1 px-1.5 py-0.5 rounded border border-dashed text-[9px] font-medium"
                                                                    } else {
                                                                        "flex items-center gap-1 px-1.5 py-0.5 rounded border text-[9px] font-medium"
                                                                    };

                                                                    rsx! {
                                                                        // Connection arrow between blocks
                                                                        if bidx > 0 {
                                                                            span {
                                                                                class: "text-[8px] flex-shrink-0",
                                                                                style: "color: {mc.bg}40;",
                                                                                "\u{2192}"
                                                                            }
                                                                        }
                                                                        // Block pill
                                                                        div {
                                                                            key: "block-{bidx}",
                                                                            class: "{pill_class}",
                                                                            style: "{pill_style}",
                                                                            div {
                                                                                class: "w-1.5 h-1.5 rounded-full flex-shrink-0",
                                                                                style: "{dot_style}",
                                                                            }
                                                                            span { class: "truncate max-w-[80px]",
                                                                                "{block.name}"
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    } else if assigned_id.is_none() {
                                                        // Empty unassigned slot
                                                        div { class: "px-3 py-2",
                                                            span { class: "text-[10px] text-zinc-600 italic",
                                                                "No module assigned — click assign to load a preset"
                                                            }
                                                        }
                                                    } else {
                                                        // Assigned but empty blocks or all-placeholder
                                                        div { class: "px-3 py-2",
                                                            span { class: "text-[10px] text-zinc-600 italic",
                                                                if all_blocks_placeholder {
                                                                    "Template placeholders — assign plugins in Block Editor"
                                                                } else {
                                                                    "Empty block chain"
                                                                }
                                                            }
                                                        }
                                                    }

                                                    // Expanded assignment picker
                                                    if is_expanded {
                                                        div {
                                                            class: "border-t px-3 py-2",
                                                            style: "border-color: {mc.bg}25; background-color: {mc.bg}08;",
                                                            if type_modules.is_empty() {
                                                                span { class: "text-[10px] text-zinc-600 italic",
                                                                    "No module presets available. Create one in the Module Editor."
                                                                }
                                                            } else {
                                                                div { class: "flex items-center gap-1.5 flex-wrap",
                                                                    for mp in type_modules.iter() {
                                                                        {
                                                                            let mp_id = mp.id;
                                                                            let mp_name = mp.name.clone();
                                                                            let is_current = assigned_id == Some(mp_id);
                                                                            let key = type_key_for_assign.clone();

                                                                            rsx! {
                                                                                button {
                                                                                    key: "{mp_id}",
                                                                                    class: if is_current {
                                                                                        "px-2.5 py-1 rounded-md text-[10px] font-semibold \
                                                                                         border-2 transition-all"
                                                                                    } else {
                                                                                        "px-2.5 py-1 rounded-md text-[10px] font-medium \
                                                                                         bg-zinc-800/60 text-zinc-400 border border-zinc-700/40 \
                                                                                         hover:bg-zinc-700/50 hover:text-zinc-200 transition-all"
                                                                                    },
                                                                                    style: if is_current {
                                                                                        format!("background-color: {}25; border-color: {}; color: {};", mc.bg, mc.bg, mc.fg)
                                                                                    } else {
                                                                                        String::new()
                                                                                    },
                                                                                    onclick: move |_| {
                                                                                        module_assignments.write().insert(key.clone(), mp_id);
                                                                                        expanded_module.set(None);
                                                                                    },
                                                                                    "{mp_name}"
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

                        // ── Preset Snapshots (bottom ~40%) ────────
                        div { class: "flex-1 flex flex-col min-h-0",
                            div { class: "px-4 py-2 border-b border-border/30 flex items-center justify-between flex-shrink-0",
                                div { class: "flex items-center gap-2",
                                    span { class: "text-[10px] font-bold text-zinc-500 uppercase tracking-[0.1em]",
                                        "Snapshots"
                                    }
                                    if !snapshots.is_empty() {
                                        span { class: "text-[9px] font-mono text-zinc-600", "{snapshots.len()}" }
                                    }
                                }
                                button {
                                    class: "flex items-center gap-1 px-2.5 py-1 rounded-md text-[10px] font-semibold \
                                            bg-cyan-500/10 text-cyan-400 border border-cyan-500/20 \
                                            hover:bg-cyan-500/20 transition-all duration-150 \
                                            disabled:opacity-25 disabled:cursor-not-allowed",
                                    disabled: selected_id.is_none(),
                                    onclick: move |_| {
                                        if let Some(pid) = selected_id {
                                            let snap_name = format!("Scene {}", snapshots.len() + 1);
                                            spawn(async move {
                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                let snap_data = signal_control::preset::Snapshot::new(&snap_name);
                                                match ctl.save_rig_preset_snapshot(pid, &snap_name, &snap_data).await {
                                                    Ok(id) => {
                                                        debug!("Created preset snapshot: {id}");
                                                        if let Ok(snaps) = ctl.list_rig_preset_snapshots(pid).await {
                                                            preset_snapshots.set(snaps);
                                                        }
                                                    }
                                                    Err(e) => warn!("Create snapshot failed: {e}"),
                                                }
                                            });
                                        }
                                    },
                                    span { class: "text-cyan-300", "+" }
                                    "New Scene"
                                }
                            }

                            div { class: "flex-1 overflow-y-auto min-h-0 px-3 py-2",
                                if snapshots.is_empty() {
                                    div { class: "flex items-center justify-center h-full",
                                        p { class: "text-xs text-zinc-600",
                                            if selected_id.is_some() {
                                                "No scenes — click New Scene to add one"
                                            } else {
                                                "Select a preset"
                                            }
                                        }
                                    }
                                } else {
                                    div { class: "grid grid-cols-3 gap-2",
                                        for snap in snapshots.iter() {
                                            {
                                                let sid = snap.id;
                                                let snap_name = snap.name.clone();
                                                rsx! {
                                                    div {
                                                        key: "{sid}",
                                                        class: "flex flex-col gap-1.5 px-3 py-2.5 rounded-lg border border-zinc-800/50 \
                                                               hover:bg-zinc-800/30 hover:border-zinc-700/40 cursor-pointer transition-all duration-100",
                                                        span { class: "text-xs font-medium text-zinc-200 truncate", "{snap_name}" }
                                                        div { class: "flex gap-1",
                                                            button {
                                                                class: "flex-1 px-2 py-1 rounded text-[9px] font-semibold \
                                                                        bg-emerald-500/10 text-emerald-400 border border-emerald-500/20 \
                                                                        hover:bg-emerald-500/20 transition-all",
                                                                "Activate"
                                                            }
                                                            button {
                                                                class: "px-2 py-1 rounded text-[9px] text-red-400/50 hover:text-red-400 \
                                                                        hover:bg-red-500/10 border border-transparent hover:border-red-500/20 transition-all",
                                                                onclick: move |_| {
                                                                    spawn(async move {
                                                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                                        if let Ok(true) = ctl.delete_rig_preset_snapshot(sid).await {
                                                                            if let Some(pid) = *preset_selected_id.read() {
                                                                                if let Ok(snaps) = ctl.list_rig_preset_snapshots(pid).await {
                                                                                    preset_snapshots.set(snaps);
                                                                                }
                                                                            }
                                                                        }
                                                                    });
                                                                },
                                                                "\u{2715}"
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
            },
            right: Some(rsx! {
                    div { class: "px-4 py-3 border-b border-border/30 flex-shrink-0",
                        h3 { class: "text-[11px] font-bold text-zinc-400 uppercase tracking-[0.15em]",
                            "Quick Guide"
                        }
                    }
                    div { class: "flex-1 overflow-y-auto min-h-0 px-4 py-3",
                        div { class: "flex flex-col gap-4",
                            div {
                                p { class: "text-[10px] font-semibold text-zinc-400 mb-1", "1. Create a Preset" }
                                p { class: "text-[10px] text-zinc-600 leading-relaxed",
                                    "Click + in the left panel to name your rig preset."
                                }
                            }
                            div {
                                p { class: "text-[10px] font-semibold text-zinc-400 mb-1", "2. Assign Modules" }
                                p { class: "text-[10px] text-zinc-600 leading-relaxed",
                                    "Click 'assign' on each module slot to pick a module preset. Block chains preview inline."
                                }
                            }
                            div {
                                p { class: "text-[10px] font-semibold text-zinc-400 mb-1", "3. Reorder" }
                                p { class: "text-[10px] text-zinc-600 leading-relaxed",
                                    "Drag module title bars to reorder them in the signal chain."
                                }
                            }
                            div {
                                p { class: "text-[10px] font-semibold text-zinc-400 mb-1", "4. Create Scenes" }
                                p { class: "text-[10px] text-zinc-600 leading-relaxed",
                                    "Scenes capture module snapshot assignments for live recall (Verse, Chorus, Solo)."
                                }
                            }
                            div {
                                p { class: "text-[10px] font-semibold text-zinc-400 mb-1", "Workflow" }
                                p { class: "text-[10px] text-zinc-600 leading-relaxed",
                                    "Blocks \u{2192} Modules \u{2192} Presets \u{2192} Scenes"
                                }
                                p { class: "text-[10px] text-zinc-500 mt-1 leading-relaxed",
                                    "Build block presets in the Blocks tab, compose them into modules in the Modules tab, then assign modules here."
                                }
                            }
                        }
                    }
            }),
            status: rsx! {
                div { class: "w-1.5 h-1.5 rounded-full bg-amber-400/60" }
                span { class: "text-[10px] text-zinc-500 font-mono truncate flex-1", "{status}" }
            },
        }
    }
}
