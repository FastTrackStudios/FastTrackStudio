//! Module Editor View — compose blocks into purpose-driven module presets.
//!
//! Left: Module type browser with DB preset counts
//! Center: Module preset manager + block composition chain + snapshot grid
//! Right: Block preset picker for assigning blocks to slots
//!
//! Modules are user-composed chains of arbitrary block types. A "Drive Module"
//! can be any combination of boosts, drives, EQs, etc. in any order.

use crate::components::block_editor::library::{predefined_block_types, BlockTypeDefinition};
use crate::components::rig_grid::block_colors::block_type_color;
use crate::prelude::*;
use crate::signals::RIG_SERVICE;
use signal_control::block::BlockType;
use signal_control::module::ModuleType;
use tracing::{debug, warn};
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// Module type definitions for the sidebar
// ─────────────────────────────────────────────────────────────────────────────

struct ModuleTypeDef {
    module_type: ModuleType,
    name: &'static str,
    color: &'static str,
    description: &'static str,
}

fn guitar_module_types() -> Vec<ModuleTypeDef> {
    vec![
        ModuleTypeDef {
            module_type: ModuleType::Drive,
            name: "Drive",
            color: "text-orange-400",
            description: "Boost, OD, distortion",
        },
        ModuleTypeDef {
            module_type: ModuleType::Amp,
            name: "Amp",
            color: "text-amber-400",
            description: "Amp + cabinet + room",
        },
        ModuleTypeDef {
            module_type: ModuleType::Eq,
            name: "EQ",
            color: "text-emerald-400",
            description: "Tone shaping",
        },
        ModuleTypeDef {
            module_type: ModuleType::Dynamics,
            name: "Dynamics",
            color: "text-blue-400",
            description: "Comp, gate, limiter",
        },
        ModuleTypeDef {
            module_type: ModuleType::Modulation,
            name: "Modulation",
            color: "text-purple-400",
            description: "Chorus, flanger, phaser",
        },
        ModuleTypeDef {
            module_type: ModuleType::Time,
            name: "Time",
            color: "text-cyan-400",
            description: "Delay, reverb, freeze",
        },
        ModuleTypeDef {
            module_type: ModuleType::Motion,
            name: "Motion",
            color: "text-violet-300",
            description: "Tremolo, vibrato, rotary",
        },
        ModuleTypeDef {
            module_type: ModuleType::Special,
            name: "Special",
            color: "text-pink-400",
            description: "Wah, pitch, filter",
        },
        ModuleTypeDef {
            module_type: ModuleType::PostEq,
            name: "Post EQ",
            color: "text-emerald-300",
            description: "Post-amp shaping",
        },
        ModuleTypeDef {
            module_type: ModuleType::Master,
            name: "Master",
            color: "text-zinc-300",
            description: "Final output stage",
        },
    ]
}

// ─────────────────────────────────────────────────────────────────────────────
// State: block composition for the currently edited module preset
// ─────────────────────────────────────────────────────────────────────────────

/// A block slot in the module composition chain.
#[derive(Debug, Clone, PartialEq)]
struct CompositionSlot {
    id: Uuid,
    block_type: BlockType,
    /// Assigned block preset ID from DB (None = empty slot)
    block_preset_id: Option<Uuid>,
    /// Display name of the assigned block preset
    block_preset_name: Option<String>,
    /// Plugin name for subtitle
    plugin_name: Option<String>,
}

// Global signals for module editor state
static SELECTED_MODULE_TYPE: GlobalSignal<Option<ModuleType>> = Signal::global(|| None);
static SELECTED_MODULE_PRESET_ID: GlobalSignal<Option<Uuid>> = Signal::global(|| None);
static MODULE_PRESETS: GlobalSignal<Vec<signal_control::module_preset_entity::Model>> =
    Signal::global(Vec::new);
static MODULE_SNAPSHOTS: GlobalSignal<Vec<signal_control::module_snapshot::Model>> =
    Signal::global(Vec::new);
static MODULE_TYPE_COUNTS: GlobalSignal<std::collections::HashMap<String, usize>> =
    Signal::global(std::collections::HashMap::new);
static MODULE_EDITOR_STATUS: GlobalSignal<String> =
    Signal::global(|| "Select a module type".to_string());

/// The block composition chain being edited.
static COMPOSITION_CHAIN: GlobalSignal<Vec<CompositionSlot>> = Signal::global(Vec::new);

/// Which slot in the chain is selected (for the right panel block picker).
static SELECTED_SLOT_ID: GlobalSignal<Option<Uuid>> = Signal::global(|| None);

/// Whether we're showing the "add block type" picker.
static SHOW_ADD_BLOCK_PICKER: GlobalSignal<bool> = Signal::global(|| false);

// ─────────────────────────────────────────────────────────────────────────────
// DB helpers
// ─────────────────────────────────────────────────────────────────────────────

async fn refresh_module_type_counts() {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    let Ok(all) = ctl.list_module_presets(None).await else {
        return;
    };
    let mut counts = std::collections::HashMap::new();
    for p in &all {
        *counts.entry(p.module_type.clone()).or_insert(0usize) += 1;
    }
    *MODULE_TYPE_COUNTS.write() = counts;
}

async fn refresh_module_presets(module_type: &str) {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    match ctl.list_module_presets(Some(module_type)).await {
        Ok(presets) => *MODULE_PRESETS.write() = presets,
        Err(e) => warn!("Failed to load module presets: {e}"),
    }
}

async fn refresh_module_snapshots(preset_id: Uuid) {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    match ctl.list_module_snapshots(preset_id).await {
        Ok(snaps) => *MODULE_SNAPSHOTS.write() = snaps,
        Err(e) => warn!("Failed to load module snapshots: {e}"),
    }
}

/// Parse blocks JSON from a module preset model into CompositionSlots.
fn parse_composition_from_model(
    model: &signal_control::module_preset_entity::Model,
) -> Vec<CompositionSlot> {
    // blocks is a JSON array of ModuleBlock objects
    let blocks = model.blocks.as_array().cloned().unwrap_or_default();
    blocks
        .iter()
        .enumerate()
        .map(|(idx, b)| {
            let block_type_str = b
                .get("block")
                .and_then(|bl| bl.get("block_type"))
                .and_then(|v| v.as_str())
                .unwrap_or("Custom");
            let block_type = parse_block_type(block_type_str);
            let name = b
                .get("block")
                .and_then(|bl| bl.get("name"))
                .and_then(|v| v.as_str())
                .map(String::from);
            let plugin = b
                .get("block")
                .and_then(|bl| bl.get("plugin_id"))
                .and_then(|v| v.get("name"))
                .and_then(|n| n.as_str())
                .map(String::from);
            let id_str = b.get("id").and_then(|v| v.as_str());
            let id = id_str
                .and_then(|s| Uuid::parse_str(s).ok())
                .unwrap_or_else(Uuid::new_v4);

            CompositionSlot {
                id,
                block_type,
                block_preset_id: None, // module blocks reference blocks inline, not by preset ID
                block_preset_name: name,
                plugin_name: plugin,
            }
        })
        .collect()
}

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

// ─────────────────────────────────────────────────────────────────────────────
// Main Component
// ─────────────────────────────────────────────────────────────────────────────

#[component]
pub fn ModuleEditorView() -> Element {
    let module_types = use_signal(guitar_module_types);
    let selected_type = *SELECTED_MODULE_TYPE.read();
    let selected_preset_id = *SELECTED_MODULE_PRESET_ID.read();
    let presets = MODULE_PRESETS.read();
    let snapshots = MODULE_SNAPSHOTS.read();
    let type_counts = MODULE_TYPE_COUNTS.read();
    let chain = COMPOSITION_CHAIN.read();
    let status = MODULE_EDITOR_STATUS.read().clone();
    let selected_slot_id = *SELECTED_SLOT_ID.read();
    let show_add_picker = *SHOW_ADD_BLOCK_PICKER.read();

    // Dialog state
    let mut show_new_preset_dialog = use_signal(|| false);
    let mut new_preset_name = use_signal(String::new);
    let mut show_rename_dialog = use_signal(|| false);
    let mut rename_value = use_signal(String::new);

    // Load counts on mount
    use_future(move || async move {
        refresh_module_type_counts().await;
    });

    let preset_count = presets.len();
    let chain_len = chain.len();

    rsx! {
        div { class: "h-full w-full flex flex-col overflow-hidden",
            // Accent strip
            div { class: "h-[2px] w-full bg-gradient-to-r from-purple-500 via-orange-400 to-emerald-500 flex-shrink-0" }

            div { class: "flex-1 flex min-h-0 overflow-hidden",

                // ══════════════════════════════════════════════════
                // LEFT: Module Type Browser
                // ══════════════════════════════════════════════════
                div { class: "w-52 flex-shrink-0 border-r border-border/50 flex flex-col min-h-0 bg-zinc-950/50",
                    div { class: "px-4 py-3 border-b border-border/30 flex-shrink-0",
                        h2 { class: "text-[11px] font-bold text-zinc-400 uppercase tracking-[0.15em]",
                            "Module Types"
                        }
                    }
                    div { class: "flex-1 overflow-y-auto min-h-0 px-2 py-2",
                        for def in module_types.read().iter() {
                            {
                                let mt = def.module_type;
                                let name = def.name;
                                let color = def.color;
                                let is_active = selected_type == Some(mt);
                                let type_key = mt.display_name().to_string();
                                let count = type_counts.get(&type_key).copied().unwrap_or(0);

                                rsx! {
                                    button {
                                        key: "{name}",
                                        class: if is_active {
                                            "w-full flex items-center gap-3 px-3 py-2 rounded-lg text-left transition-all duration-150 \
                                             bg-zinc-800/80 border border-zinc-600/50 shadow-sm shadow-black/20"
                                        } else {
                                            "w-full flex items-center gap-3 px-3 py-2 rounded-lg text-left transition-all duration-150 \
                                             hover:bg-zinc-800/40 border border-transparent"
                                        },
                                        onclick: move |_| {
                                            *SELECTED_MODULE_TYPE.write() = Some(mt);
                                            *SELECTED_MODULE_PRESET_ID.write() = None;
                                            COMPOSITION_CHAIN.write().clear();
                                            MODULE_SNAPSHOTS.write().clear();
                                            *SELECTED_SLOT_ID.write() = None;
                                            let type_name = mt.display_name().to_string();
                                            *MODULE_EDITOR_STATUS.write() = format!("Selected: {}", name);
                                            spawn(async move {
                                                refresh_module_presets(&type_name).await;
                                            });
                                        },
                                        {
                                            let opacity = if is_active { "1.0" } else { "0.4" };
                                            rsx! {
                                                div {
                                                    class: "w-1 h-6 rounded-full flex-shrink-0 {color}",
                                                    style: "background-color: currentColor; opacity: {opacity};",
                                                }
                                            }
                                        }
                                        div { class: "flex-1 min-w-0 flex items-center justify-between",
                                            span {
                                                class: if is_active {
                                                    "text-xs font-semibold text-zinc-100 truncate"
                                                } else {
                                                    "text-xs font-medium text-zinc-400 truncate"
                                                },
                                                "{name}"
                                            }
                                            if count > 0 {
                                                span {
                                                    class: if is_active {
                                                        "text-[9px] font-mono text-zinc-300 bg-zinc-700/60 px-1.5 py-0.5 rounded flex-shrink-0"
                                                    } else {
                                                        "text-[9px] font-mono text-zinc-500 bg-zinc-800/40 px-1.5 py-0.5 rounded flex-shrink-0"
                                                    },
                                                    "{count}"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // ══════════════════════════════════════════════════
                // CENTER: Module Preset Manager + Composition
                // ══════════════════════════════════════════════════
                div { class: "flex-1 flex flex-col min-h-0 min-w-0 overflow-hidden",
                    if selected_type.is_none() {
                        div { class: "flex-1 flex items-center justify-center",
                            div { class: "text-center max-w-xs",
                                div { class: "w-12 h-12 rounded-xl bg-zinc-800/60 border border-zinc-700/40 flex items-center justify-center mx-auto mb-4",
                                    span { class: "text-xl text-zinc-600", "\u{2B50}" }
                                }
                                p { class: "text-sm font-medium text-zinc-400 mb-1",
                                    "Select a Module Type"
                                }
                                p { class: "text-xs text-zinc-600 leading-relaxed",
                                    "Choose a module type from the left to manage presets and compose block chains"
                                }
                            }
                        }
                    } else {
                        // ── Toolbar ──────────────────────────────
                        div { class: "px-4 py-2.5 border-b border-border/30 flex items-center gap-3 flex-shrink-0 bg-zinc-900/30",
                            if let Some(mt) = selected_type {
                                span { class: "text-xs font-bold text-zinc-200 tracking-wide",
                                    "{mt.display_name()}"
                                }
                            }
                            {
                                let plural = if preset_count != 1 { "s" } else { "" };
                                rsx! {
                                    span { class: "text-[10px] text-zinc-600 font-mono",
                                        "{preset_count} preset{plural}"
                                    }
                                }
                            }
                            div { class: "flex-1" }
                            button {
                                class: "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-[10px] font-semibold \
                                        bg-purple-500/15 text-purple-300 border border-purple-500/25 \
                                        hover:bg-purple-500/25 hover:border-purple-500/40 transition-all duration-150",
                                onclick: move |_| {
                                    new_preset_name.set(String::new());
                                    show_new_preset_dialog.set(true);
                                },
                                span { class: "text-purple-400", "+" }
                                "New Module Preset"
                            }
                        }

                        // New preset dialog
                        if show_new_preset_dialog() {
                            InlineDialog {
                                label: "Preset Name",
                                placeholder: "e.g., Blues Stack, Heavy Lead...",
                                value: new_preset_name(),
                                accent_color: "purple",
                                on_input: move |v: String| new_preset_name.set(v),
                                on_submit: move |name: String| {
                                    show_new_preset_dialog.set(false);
                                    let mt = selected_type.unwrap();
                                    spawn(async move {
                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                        match ctl.create_module_preset(
                                            &name,
                                            mt.display_name(),
                                            None,
                                            serde_json::json!([]),
                                            serde_json::json!([]),
                                        ).await {
                                            Ok(id) => {
                                                debug!("Created module preset: {id}");
                                                *MODULE_EDITOR_STATUS.write() = format!("Created '{}'", name);
                                                refresh_module_presets(mt.display_name()).await;
                                                refresh_module_type_counts().await;
                                                *SELECTED_MODULE_PRESET_ID.write() = Some(id);
                                                COMPOSITION_CHAIN.write().clear();
                                                MODULE_SNAPSHOTS.write().clear();
                                            }
                                            Err(e) => {
                                                warn!("Create module preset failed: {e}");
                                                *MODULE_EDITOR_STATUS.write() = format!("Failed: {e}");
                                            }
                                        }
                                    });
                                },
                                on_cancel: move |_| show_new_preset_dialog.set(false),
                            }
                        }

                        // Rename dialog
                        if show_rename_dialog() {
                            InlineDialog {
                                label: "Rename",
                                placeholder: "New name...",
                                value: rename_value(),
                                accent_color: "purple",
                                on_input: move |v: String| rename_value.set(v),
                                on_submit: move |new_name: String| {
                                    show_rename_dialog.set(false);
                                    if let Some(pid) = selected_preset_id {
                                        spawn(async move {
                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                            if let Err(e) = ctl.update_module_preset(pid, Some(&new_name), None, None, None, None).await {
                                                warn!("Rename failed: {e}");
                                            } else {
                                                *MODULE_EDITOR_STATUS.write() = format!("Renamed to '{}'", new_name);
                                                if let Some(mt) = selected_type {
                                                    refresh_module_presets(mt.display_name()).await;
                                                }
                                            }
                                        });
                                    }
                                },
                                on_cancel: move |_| show_rename_dialog.set(false),
                            }
                        }

                        // ── Preset List (top 35%) ────────────────
                        div { class: "h-[35%] flex flex-col min-h-0 border-b border-border/20 flex-shrink-0",
                            div { class: "flex-1 overflow-y-auto min-h-0",
                                if presets.is_empty() {
                                    div { class: "flex items-center justify-center h-full px-6",
                                        p { class: "text-xs text-zinc-500", "No presets — create one above" }
                                    }
                                } else {
                                    div { class: "px-2 py-1.5",
                                        for preset in presets.iter() {
                                            {
                                                let pid = preset.id;
                                                let is_selected = selected_preset_id == Some(pid);
                                                let pname = preset.name.clone();
                                                // Parse block count from JSON
                                                let block_count = preset.blocks.as_array().map(|a| a.len()).unwrap_or(0);

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
                                                            *SELECTED_MODULE_PRESET_ID.write() = Some(pid);
                                                            *SELECTED_SLOT_ID.write() = None;
                                                            *SHOW_ADD_BLOCK_PICKER.write() = false;
                                                            // Parse blocks into composition chain
                                                            let model = MODULE_PRESETS.read().iter().find(|p| p.id == pid).cloned();
                                                            if let Some(m) = model {
                                                                *COMPOSITION_CHAIN.write() = parse_composition_from_model(&m);
                                                            }
                                                            spawn(async move {
                                                                refresh_module_snapshots(pid).await;
                                                            });
                                                        },
                                                        div {
                                                            class: if is_selected {
                                                                "w-1.5 h-1.5 rounded-full bg-purple-400 flex-shrink-0"
                                                            } else {
                                                                "w-1.5 h-1.5 rounded-full bg-zinc-700 flex-shrink-0"
                                                            },
                                                        }
                                                        div { class: "flex-1 min-w-0",
                                                            span { class: "text-xs font-medium text-zinc-200 truncate block", "{pname}" }
                                                            {
                                                                let bp = if block_count != 1 { "s" } else { "" };
                                                                rsx! {
                                                                    span { class: "text-[10px] text-zinc-500 font-mono",
                                                                        "{block_count} block{bp}"
                                                                    }
                                                                }
                                                            }
                                                        }
                                                        if is_selected {
                                                            div { class: "flex items-center gap-0.5 flex-shrink-0",
                                                                button {
                                                                    class: "p-1 rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-700/50 transition-colors",
                                                                    title: "Rename",
                                                                    onclick: move |evt| {
                                                                        evt.stop_propagation();
                                                                        rename_value.set(MODULE_PRESETS.read().iter().find(|p| p.id == pid).map(|p| p.name.clone()).unwrap_or_default());
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
                                                                            if let Err(e) = ctl.delete_module_preset(pid).await {
                                                                                warn!("Delete failed: {e}");
                                                                            } else {
                                                                                *MODULE_EDITOR_STATUS.write() = "Preset deleted".into();
                                                                                if selected_preset_id == Some(pid) {
                                                                                    *SELECTED_MODULE_PRESET_ID.write() = None;
                                                                                    COMPOSITION_CHAIN.write().clear();
                                                                                    MODULE_SNAPSHOTS.write().clear();
                                                                                }
                                                                                if let Some(mt) = selected_type {
                                                                                    refresh_module_presets(mt.display_name()).await;
                                                                                    refresh_module_type_counts().await;
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
                            }
                        }

                        // ── Block Composition Chain (middle) ─────
                        div { class: "h-[35%] flex flex-col min-h-0 border-b border-border/20 flex-shrink-0",
                            div { class: "px-4 py-2 border-b border-border/30 flex items-center justify-between flex-shrink-0",
                                div { class: "flex items-center gap-2",
                                    span { class: "text-[10px] font-bold text-zinc-500 uppercase tracking-[0.1em]",
                                        "Signal Chain"
                                    }
                                    if chain_len > 0 {
                                        {
                                            let block_plural = if chain_len != 1 { "s" } else { "" };
                                            rsx! {
                                                span { class: "text-[9px] font-mono text-zinc-600",
                                                    "{chain_len} block{block_plural}"
                                                }
                                            }
                                        }
                                    }
                                }
                                if selected_preset_id.is_some() {
                                    button {
                                        class: "flex items-center gap-1 px-2.5 py-1 rounded-md text-[10px] font-semibold \
                                                bg-emerald-500/10 text-emerald-400 border border-emerald-500/20 \
                                                hover:bg-emerald-500/20 transition-all duration-150",
                                        onclick: move |_| {
                                            *SHOW_ADD_BLOCK_PICKER.write() = true;
                                            *SELECTED_SLOT_ID.write() = None;
                                        },
                                        span { class: "text-emerald-300", "+" }
                                        "Add Block"
                                    }
                                }
                            }

                            div { class: "flex-1 overflow-y-auto min-h-0 px-3 py-2",
                                if selected_preset_id.is_none() {
                                    div { class: "flex items-center justify-center h-full",
                                        p { class: "text-xs text-zinc-600", "Select a preset to edit its block chain" }
                                    }
                                } else if chain.is_empty() {
                                    div { class: "flex items-center justify-center h-full",
                                        div { class: "text-center",
                                            p { class: "text-xs text-zinc-500 mb-1", "Empty chain" }
                                            p { class: "text-[10px] text-zinc-600",
                                                "Click \"Add Block\" to start building your signal chain"
                                            }
                                        }
                                    }
                                } else {
                                    // Horizontal chain strip
                                    div { class: "flex items-center gap-1.5 flex-wrap",
                                        for (idx, slot) in chain.iter().enumerate() {
                                            {
                                                let slot_id = slot.id;
                                                let is_slot_selected = selected_slot_id == Some(slot_id);
                                                let bt = slot.block_type;
                                                let color = block_type_color(bt);
                                                let slot_name = slot.block_preset_name.as_deref().unwrap_or(bt.display_name());

                                                rsx! {
                                                    div {
                                                        key: "{slot_id}",
                                                        class: "flex items-center gap-1.5",
                                                        // Connection arrow between blocks
                                                        if idx > 0 {
                                                            span { class: "text-zinc-700 text-[10px] flex-shrink-0", "\u{2192}" }
                                                        }
                                                        // Block card
                                                        div {
                                                            class: if is_slot_selected {
                                                                "flex flex-col items-center gap-0.5 px-3 py-2 rounded-lg border cursor-pointer transition-all duration-100 \
                                                                 bg-zinc-800/60 border-purple-500/40 min-w-[80px]"
                                                            } else {
                                                                "flex flex-col items-center gap-0.5 px-3 py-2 rounded-lg border cursor-pointer transition-all duration-100 \
                                                                 hover:bg-zinc-800/30 border-zinc-800/50 min-w-[80px]"
                                                            },
                                                            onclick: move |_| {
                                                                *SELECTED_SLOT_ID.write() = Some(slot_id);
                                                                *SHOW_ADD_BLOCK_PICKER.write() = false;
                                                            },
                                                            // Color dot + type
                                                            div { class: "flex items-center gap-1.5",
                                                                div {
                                                                    class: "w-2.5 h-2.5 rounded-full flex-shrink-0",
                                                                    style: "background-color: {color.bg};",
                                                                }
                                                                span { class: "text-[9px] text-zinc-400 font-mono uppercase",
                                                                    "{bt.display_name()}"
                                                                }
                                                            }
                                                            // Preset name
                                                            span { class: "text-[10px] text-zinc-200 font-medium truncate max-w-[100px] text-center",
                                                                "{slot_name}"
                                                            }
                                                            // Remove button
                                                            if is_slot_selected {
                                                                button {
                                                                    class: "mt-0.5 text-[8px] text-red-400/60 hover:text-red-400 transition-colors",
                                                                    onclick: move |evt| {
                                                                        evt.stop_propagation();
                                                                        COMPOSITION_CHAIN.write().retain(|s| s.id != slot_id);
                                                                        *SELECTED_SLOT_ID.write() = None;
                                                                        save_composition_chain();
                                                                    },
                                                                    "remove"
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

                        // ── Module Snapshots (bottom) ────────────
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
                                    disabled: selected_preset_id.is_none(),
                                    onclick: move |_| {
                                        if let Some(pid) = selected_preset_id {
                                            let snap_name = format!("Snap {}", snapshots.len() + 1);
                                            spawn(async move {
                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                match ctl.create_module_snapshot(pid, &snap_name, serde_json::json!({}), false).await {
                                                    Ok(id) => {
                                                        debug!("Created module snapshot: {id}");
                                                        refresh_module_snapshots(pid).await;
                                                    }
                                                    Err(e) => warn!("Create snapshot failed: {e}"),
                                                }
                                            });
                                        }
                                    },
                                    span { class: "text-cyan-300", "+" }
                                    "New Snapshot"
                                }
                            }

                            div { class: "flex-1 overflow-y-auto min-h-0 px-3 py-2",
                                if selected_preset_id.is_none() || snapshots.is_empty() {
                                    div { class: "flex items-center justify-center h-full",
                                        p { class: "text-xs text-zinc-600",
                                            if selected_preset_id.is_none() {
                                                "Select a preset to view snapshots"
                                            } else {
                                                "No snapshots yet"
                                            }
                                        }
                                    }
                                } else {
                                    div { class: "grid grid-cols-2 gap-2",
                                        for snap in snapshots.iter() {
                                            {
                                                let sid = snap.id;
                                                let snap_name = snap.name.clone();
                                                let is_default = snap.is_default;
                                                rsx! {
                                                    div {
                                                        key: "{sid}",
                                                        class: "flex flex-col gap-1 px-3 py-2.5 rounded-lg border border-zinc-800/50 \
                                                               hover:bg-zinc-800/30 cursor-pointer transition-all duration-100",
                                                        div { class: "flex items-center justify-between",
                                                            span { class: "text-xs font-medium text-zinc-200 truncate", "{snap_name}" }
                                                            if is_default {
                                                                span { class: "text-[8px] text-emerald-400/70 bg-emerald-500/10 px-1 rounded", "default" }
                                                            }
                                                        }
                                                        div { class: "flex gap-1",
                                                            button {
                                                                class: "flex-1 px-2 py-1 rounded text-[9px] font-semibold \
                                                                        bg-cyan-500/10 text-cyan-400 border border-cyan-500/20 \
                                                                        hover:bg-cyan-500/20 transition-all",
                                                                "Recall"
                                                            }
                                                            button {
                                                                class: "px-2 py-1 rounded text-[9px] text-red-400/50 hover:text-red-400 \
                                                                        hover:bg-red-500/10 border border-transparent hover:border-red-500/20 transition-all",
                                                                onclick: move |_| {
                                                                    spawn(async move {
                                                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                                        if let Ok(true) = ctl.delete_module_snapshot(sid).await {
                                                                            if let Some(pid) = *SELECTED_MODULE_PRESET_ID.read() {
                                                                                refresh_module_snapshots(pid).await;
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
                }

                // ══════════════════════════════════════════════════
                // RIGHT: Block Picker / Slot Detail
                // ══════════════════════════════════════════════════
                div { class: "w-64 flex-shrink-0 border-l border-border/50 flex flex-col min-h-0 bg-zinc-950/40",
                    if show_add_picker {
                        // Block type picker — choose what kind of block to add
                        div { class: "px-4 py-3 border-b border-border/30 flex-shrink-0",
                            div { class: "flex items-center justify-between",
                                h3 { class: "text-[11px] font-bold text-zinc-400 uppercase tracking-[0.15em]",
                                    "Add Block"
                                }
                                button {
                                    class: "text-[9px] text-zinc-500 hover:text-zinc-300 px-1.5 py-0.5 rounded hover:bg-zinc-700/40 transition-colors",
                                    onclick: move |_| *SHOW_ADD_BLOCK_PICKER.write() = false,
                                    "Cancel"
                                }
                            }
                            p { class: "text-[10px] text-zinc-600 mt-1",
                                "Pick a block type to add to the chain"
                            }
                        }
                        div { class: "flex-1 overflow-y-auto min-h-0 px-2 py-1.5",
                            for def in predefined_block_types().iter() {
                                {
                                    let bt = def.block_type;
                                    let name = def.display_name;
                                    let color_info = block_type_color(bt);

                                    rsx! {
                                        button {
                                            key: "{name}",
                                            class: "w-full flex items-center gap-2.5 px-3 py-2 rounded-lg text-left \
                                                    hover:bg-zinc-800/40 border border-transparent transition-all duration-100",
                                            onclick: move |_| {
                                                let new_slot = CompositionSlot {
                                                    id: Uuid::new_v4(),
                                                    block_type: bt,
                                                    block_preset_id: None,
                                                    block_preset_name: None,
                                                    plugin_name: None,
                                                };
                                                COMPOSITION_CHAIN.write().push(new_slot);
                                                *SHOW_ADD_BLOCK_PICKER.write() = false;
                                                save_composition_chain();
                                                *MODULE_EDITOR_STATUS.write() = format!("Added {} block", name);
                                            },
                                            div {
                                                class: "w-3 h-3 rounded-full flex-shrink-0",
                                                style: "background-color: {color_info.bg};",
                                            }
                                            div { class: "flex-1 min-w-0",
                                                span { class: "text-xs font-medium text-zinc-200", "{name}" }
                                                p { class: "text-[9px] text-zinc-500", "{def.description}" }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else if selected_slot_id.is_some() {
                        // Slot detail — show what's assigned, allow reassignment
                        {
                            let slot = chain.iter().find(|s| selected_slot_id == Some(s.id));
                            if let Some(slot) = slot {
                                let bt = slot.block_type;
                                let color_info = block_type_color(bt);
                                let slot_name = slot.block_preset_name.clone().unwrap_or_else(|| "Unassigned".to_string());
                                rsx! {
                                    div { class: "px-4 py-3 border-b border-border/30 flex-shrink-0",
                                        h3 { class: "text-[11px] font-bold text-zinc-400 uppercase tracking-[0.15em]",
                                            "Slot Detail"
                                        }
                                    }
                                    div { class: "px-4 py-3",
                                        div { class: "flex items-center gap-2 mb-3",
                                            div {
                                                class: "w-4 h-4 rounded-full flex-shrink-0",
                                                style: "background-color: {color_info.bg};",
                                            }
                                            div {
                                                p { class: "text-xs font-bold text-zinc-200", "{bt.display_name()}" }
                                                p { class: "text-[10px] text-zinc-500", "{slot_name}" }
                                            }
                                        }
                                        if let Some(ref plugin) = slot.plugin_name {
                                            p { class: "text-[10px] text-zinc-600 font-mono mb-3", "{plugin}" }
                                        }
                                        p { class: "text-[10px] text-zinc-600 leading-relaxed",
                                            "Block presets from the Block Editor tab can be assigned here in a future update."
                                        }
                                    }
                                }
                            } else {
                                rsx! {
                                    div { class: "flex-1 flex items-center justify-center",
                                        p { class: "text-xs text-zinc-600", "Slot not found" }
                                    }
                                }
                            }
                        }
                    } else {
                        // Default: instructions
                        div { class: "px-4 py-3 border-b border-border/30 flex-shrink-0",
                            h3 { class: "text-[11px] font-bold text-zinc-400 uppercase tracking-[0.15em]",
                                "Block Library"
                            }
                        }
                        div { class: "flex-1 flex items-center justify-center",
                            div { class: "text-center px-4",
                                p { class: "text-xs text-zinc-500 mb-2 leading-relaxed",
                                    "Build your signal chain by adding blocks to the composition area"
                                }
                                p { class: "text-[10px] text-zinc-600 leading-relaxed",
                                    "Click a block in the chain to see its detail, or click \"Add Block\" to extend the chain"
                                }
                            }
                        }
                    }
                }
            }

            // Status bar
            div { class: "px-4 py-1.5 border-t border-border/30 flex items-center gap-3 flex-shrink-0 bg-zinc-950/60",
                div { class: "w-1.5 h-1.5 rounded-full bg-purple-400/60" }
                span { class: "text-[10px] text-zinc-500 font-mono truncate flex-1", "{status}" }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Save composition chain back to DB
// ─────────────────────────────────────────────────────────────────────────────

fn save_composition_chain() {
    let Some(preset_id) = *SELECTED_MODULE_PRESET_ID.read() else {
        return;
    };
    let chain = COMPOSITION_CHAIN.read().clone();

    // Convert chain to JSON blocks array
    let blocks_json: serde_json::Value = serde_json::json!(
        chain.iter().enumerate().map(|(idx, slot)| {
            serde_json::json!({
                "id": slot.id.to_string(),
                "block": {
                    "name": slot.block_preset_name.as_deref().unwrap_or(slot.block_type.display_name()),
                    "block_type": slot.block_type.display_name(),
                },
                "order": { "value": idx as u32 },
            })
        }).collect::<Vec<_>>()
    );

    spawn(async move {
        let Some(ctl) = RIG_SERVICE.read().clone() else {
            return;
        };
        if let Err(e) = ctl
            .update_module_preset(preset_id, None, None, Some(blocks_json), None, None)
            .await
        {
            warn!("Failed to save block chain: {e}");
            *MODULE_EDITOR_STATUS.write() = format!("Save failed: {e}");
        } else {
            debug!("Saved block chain for preset {preset_id}");
        }
    });
}

// ─────────────────────────────────────────────────────────────────────────────
// Inline Dialog (reusable)
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct InlineDialogProps {
    label: String,
    placeholder: String,
    value: String,
    accent_color: String,
    on_input: EventHandler<String>,
    on_submit: EventHandler<String>,
    on_cancel: EventHandler<()>,
}

#[component]
fn InlineDialog(props: InlineDialogProps) -> Element {
    let value_for_key = props.value.clone();
    let value_for_btn = props.value.clone();
    let is_empty = props.value.trim().is_empty();

    rsx! {
        div { class: "px-4 py-2.5 border-b border-border/30 bg-zinc-900/60 flex items-center gap-2.5 flex-shrink-0",
            span { class: "text-[10px] text-zinc-500 font-medium whitespace-nowrap", "{props.label}:" }
            input {
                class: "flex-1 bg-zinc-800/80 border border-zinc-700/50 rounded-md px-2.5 py-1.5 text-xs text-zinc-200 \
                        outline-none focus:border-purple-500/40 focus:ring-1 focus:ring-purple-500/20 \
                        placeholder:text-zinc-600 transition-all duration-150",
                r#type: "text",
                placeholder: "{props.placeholder}",
                value: "{props.value}",
                autofocus: true,
                oninput: move |evt| props.on_input.call(evt.value().clone()),
                onkeydown: move |evt| {
                    if evt.key() == Key::Enter {
                        let val = value_for_key.trim().to_string();
                        if !val.is_empty() {
                            props.on_submit.call(val);
                        }
                    } else if evt.key() == Key::Escape {
                        props.on_cancel.call(());
                    }
                },
            }
            button {
                class: "px-3 py-1.5 rounded-md text-[10px] font-semibold bg-purple-500/80 text-white \
                        hover:bg-purple-500 transition-colors disabled:opacity-25",
                disabled: is_empty,
                onclick: move |_| {
                    let val = value_for_btn.trim().to_string();
                    if !val.is_empty() {
                        props.on_submit.call(val);
                    }
                },
                "Save"
            }
            button {
                class: "px-2 py-1.5 rounded-md text-[10px] font-medium text-zinc-500 hover:text-zinc-300 transition-colors",
                onclick: move |_| props.on_cancel.call(()),
                "Cancel"
            }
        }
    }
}
