//! Module Editor View — compose blocks into purpose-driven module presets.
//!
//! Left: Module type browser with DB preset counts
//! Center: Module preset manager + block composition chain + snapshot grid
//! Right: Block preset picker for assigning blocks to slots
//!
//! Modules are user-composed chains of arbitrary block types. A "Drive Module"
//! can be any combination of boosts, drives, EQs, etc. in any order.

use crate::components::rig_grid::block_colors::block_type_color;
use crate::components::shared::EntityEditor;
use crate::prelude::*;
use crate::signals::RIG_SERVICE;
use signal_control::block::BlockType;
use signal_control::module::ModuleType;
use tracing::{debug, warn};
use uuid::Uuid;

use crate::components::rig_grid::block_colors::BlockColor;

// ─────────────────────────────────────────────────────────────────────────────
// Module type → color mapping
// ─────────────────────────────────────────────────────────────────────────────

/// Map a module type to its display color (reuses block_type_color via the
/// same mapping as node_graph.rs `module_type_to_block_type`).
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
pub(crate) struct CompositionSlot {
    pub id: Uuid,
    pub block_type: BlockType,
    /// Assigned block preset ID from DB (None = empty slot)
    pub block_preset_id: Option<Uuid>,
    /// Display name of the assigned block preset
    pub block_preset_name: Option<String>,
    /// Plugin name for subtitle
    pub plugin_name: Option<String>,
    /// Grid column position (0-indexed). Legacy serial chains default to (idx, 0).
    pub col: usize,
    /// Grid row position (0-indexed).
    pub row: usize,
}

/// Chain view mode: Grid (2D) or List (horizontal strip).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum ChainViewMode {
    #[default]
    Grid,
    List,
}

/// Active tab in the block detail panel.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum BlockDetailTab {
    #[default]
    Settings,
    Presets,
    Snapshots,
    ModuleSnapshots,
}

/// Result of parsing a module preset's blocks JSON.
/// Contains both the composition chain and any explicit grid connections.
struct ParsedComposition {
    chain: Vec<CompositionSlot>,
    connections: Vec<super::grid_view::GridConnection>,
}

/// Parse blocks JSON from a module preset model into CompositionSlots + connections.
///
/// Supports two formats:
/// - **Legacy**: `blocks` is a plain JSON array of block objects (no connections)
/// - **Current**: `blocks` is `{"blocks": [...], "connections": [...]}`
fn parse_composition_from_model(
    model: &signal_control::module_preset_entity::Model,
) -> ParsedComposition {
    // Detect format: object with "blocks" key = new format, array = legacy
    let (block_array, conn_array) = if model.blocks.is_object() {
        let blocks = model
            .blocks
            .get("blocks")
            .and_then(|v| v.as_array())
            .cloned()
            .unwrap_or_default();
        let conns = model
            .blocks
            .get("connections")
            .and_then(|v| v.as_array())
            .cloned()
            .unwrap_or_default();
        (blocks, conns)
    } else {
        // Legacy: plain array of block objects
        let blocks = model.blocks.as_array().cloned().unwrap_or_default();
        (blocks, Vec::new())
    };

    let chain = block_array
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

            // Read grid position; legacy serial chains default to (idx, 0)
            let col = b
                .get("col")
                .and_then(|v| v.as_u64())
                .map(|v| v as usize)
                .unwrap_or(idx);
            let row = b
                .get("row")
                .and_then(|v| v.as_u64())
                .map(|v| v as usize)
                .unwrap_or(0);

            CompositionSlot {
                id,
                block_type,
                block_preset_id: None, // module blocks reference blocks inline, not by preset ID
                block_preset_name: name,
                plugin_name: plugin,
                col,
                row,
            }
        })
        .collect();

    let connections = conn_array
        .iter()
        .filter_map(|c| {
            let from = c.get("from")?.as_str()?;
            let to = c.get("to")?.as_str()?;
            Some(super::grid_view::GridConnection {
                from_slot_id: Uuid::parse_str(from).ok()?,
                to_slot_id: Uuid::parse_str(to).ok()?,
            })
        })
        .collect();

    ParsedComposition { chain, connections }
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
    // Component-local state signals
    let module_types = use_signal(guitar_module_types);
    let mut selected_module_type = use_signal(|| None::<ModuleType>);
    let mut selected_module_preset_id = use_signal(|| None::<Uuid>);
    let mut module_presets = use_signal(Vec::<signal_control::module_preset_entity::Model>::new);
    let mut module_snapshots = use_signal(Vec::<signal_control::module_snapshot::Model>::new);
    let mut module_type_counts = use_signal(std::collections::HashMap::<String, usize>::new);
    let mut module_editor_status = use_signal(|| "Select a module type".to_string());
    let mut composition_chain = use_signal(Vec::<CompositionSlot>::new);
    let mut selected_slot_id = use_signal(|| None::<Uuid>);
    let mut show_add_block_picker = use_signal(|| false);
    let mut chain_view_mode = use_signal(ChainViewMode::default);
    let mut block_detail_tab = use_signal(BlockDetailTab::default);

    // Dialog state
    let mut show_new_preset_dialog = use_signal(|| false);
    let mut new_preset_name = use_signal(String::new);
    let mut show_rename_dialog = use_signal(|| false);
    let mut rename_value = use_signal(String::new);

    // Async refresh helpers (capture local signals)
    let refresh_module_type_counts = move || {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_module_presets(None).await {
                Ok(all) => {
                    let mut counts = std::collections::HashMap::new();
                    for p in &all {
                        *counts.entry(p.module_type.clone()).or_insert(0usize) += 1;
                    }
                    module_type_counts.set(counts);
                }
                Err(_) => {}
            }
        })
    };

    let refresh_module_presets = move |module_type: String| {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_module_presets(Some(&module_type)).await {
                Ok(presets) => module_presets.set(presets),
                Err(e) => warn!("Failed to load module presets: {e}"),
            }
        })
    };

    let refresh_module_snapshots = move |preset_id: Uuid| {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_module_snapshots(preset_id).await {
                Ok(snaps) => module_snapshots.set(snaps),
                Err(e) => warn!("Failed to load module snapshots: {e}"),
            }
        })
    };

    // Load counts on mount
    use_effect(move || {
        refresh_module_type_counts();
    });

    // Pre-clone values for template use
    let selected_type = *selected_module_type.read();
    let selected_preset_id = *selected_module_preset_id.read();
    let presets = module_presets.cloned();
    let snapshots = module_snapshots.cloned();
    let type_counts = module_type_counts.cloned();
    let chain = composition_chain.cloned();
    let status = module_editor_status.cloned();
    let slot_id = *selected_slot_id.read();
    let view_mode = *chain_view_mode.read();
    let detail_tab = *block_detail_tab.read();

    // Resolved selected slot for the bottom detail panel
    let selected_slot = slot_id.and_then(|id| chain.iter().find(|s| s.id == id).cloned());

    let preset_count = presets.len();
    let chain_len = chain.len();

    rsx! {
        EntityEditor {
            accent_gradient: Some("from-purple-500 via-orange-400 to-emerald-500".to_string()),
            left_width: "w-52".to_string(),
            left: rsx! {
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
                                        selected_module_type.set(Some(mt));
                                        selected_module_preset_id.set(None);
                                        composition_chain.set(Vec::new());
                                        super::grid_view::GRID_CONNECTIONS.write().clear();
                                        module_snapshots.set(Vec::new());
                                        selected_slot_id.set(None);
                                        let type_name = mt.display_name().to_string();
                                        module_editor_status.set(format!("Selected: {}", name));
                                        refresh_module_presets(type_name);
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
            },
            center: rsx! {
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
                                    let type_name = mt.display_name().to_string();
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
                                                module_editor_status.set(format!("Created '{}'", name));

                                                // Refresh presets and counts
                                                let Some(ctl2) = RIG_SERVICE.read().clone() else { return; };
                                                if let Ok(presets) = ctl2.list_module_presets(Some(&type_name)).await {
                                                    module_presets.set(presets);
                                                }
                                                if let Ok(all) = ctl2.list_module_presets(None).await {
                                                    let mut counts = std::collections::HashMap::new();
                                                    for p in &all {
                                                        *counts.entry(p.module_type.clone()).or_insert(0usize) += 1;
                                                    }
                                                    module_type_counts.set(counts);
                                                }

                                                selected_module_preset_id.set(Some(id));
                                                composition_chain.set(Vec::new());
                                                super::grid_view::GRID_CONNECTIONS.write().clear();
                                                module_snapshots.set(Vec::new());
                                            }
                                            Err(e) => {
                                                warn!("Create module preset failed: {e}");
                                                module_editor_status.set(format!("Failed: {e}"));
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
                                        let type_name = selected_type.map(|mt| mt.display_name().to_string());
                                        spawn(async move {
                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                            if let Err(e) = ctl.update_module_preset(pid, Some(&new_name), None, None, None, None).await {
                                                warn!("Rename failed: {e}");
                                            } else {
                                                module_editor_status.set(format!("Renamed to '{}'", new_name));
                                                if let Some(ref tn) = type_name {
                                                    if let Ok(presets) = ctl.list_module_presets(Some(tn)).await {
                                                        module_presets.set(presets);
                                                    }
                                                }
                                            }
                                        });
                                    }
                                },
                                on_cancel: move |_| show_rename_dialog.set(false),
                            }
                        }

                        // ── Preset List (top ~28%) ───────────────
                        div { class: "h-[28%] flex flex-col min-h-0 border-b border-border/20 flex-shrink-0",
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
                                                            selected_module_preset_id.set(Some(pid));
                                                            selected_slot_id.set(None);
                                                            show_add_block_picker.set(false);
                                                            let model = module_presets.read().iter().find(|p| p.id == pid).cloned();
                                                            if let Some(m) = model {
                                                                let parsed = parse_composition_from_model(&m);
                                                                composition_chain.set(parsed.chain);
                                                                *super::grid_view::GRID_CONNECTIONS.write() = parsed.connections;
                                                            }
                                                            refresh_module_snapshots(pid);
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
                                                                        rename_value.set(module_presets.read().iter().find(|p| p.id == pid).map(|p| p.name.clone()).unwrap_or_default());
                                                                        show_rename_dialog.set(true);
                                                                    },
                                                                    span { class: "text-[10px]", "\u{270E}" }
                                                                }
                                                                button {
                                                                    class: "p-1 rounded text-zinc-500 hover:text-red-400 hover:bg-red-500/10 transition-colors",
                                                                    title: "Delete",
                                                                    onclick: move |evt| {
                                                                        evt.stop_propagation();
                                                                        let type_name = selected_type.map(|mt| mt.display_name().to_string());
                                                                        spawn(async move {
                                                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                                            if let Err(e) = ctl.delete_module_preset(pid).await {
                                                                                warn!("Delete failed: {e}");
                                                                            } else {
                                                                                module_editor_status.set("Preset deleted".into());
                                                                                if selected_preset_id == Some(pid) {
                                                                                    selected_module_preset_id.set(None);
                                                                                    composition_chain.set(Vec::new());
                                                                                    super::grid_view::GRID_CONNECTIONS.write().clear();
                                                                                    module_snapshots.set(Vec::new());
                                                                                }
                                                                                if let Some(ref tn) = type_name {
                                                                                    if let Ok(presets) = ctl.list_module_presets(Some(tn)).await {
                                                                                        module_presets.set(presets);
                                                                                    }
                                                                                    if let Ok(all) = ctl.list_module_presets(None).await {
                                                                                        let mut counts = std::collections::HashMap::new();
                                                                                        for p in &all {
                                                                                            *counts.entry(p.module_type.clone()).or_insert(0usize) += 1;
                                                                                        }
                                                                                        module_type_counts.set(counts);
                                                                                    }
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

                        // ── Signal Chain: Grid or List (middle ~47%) ─
                        div { class: "h-[47%] flex flex-col min-h-0 border-b border-border/20 flex-shrink-0",
                            // Chain header with view mode toggle
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
                                div { class: "flex items-center gap-1",
                                    // View mode toggle
                                    {
                                        let is_grid = view_mode == ChainViewMode::Grid;
                                        rsx! {
                                            div { class: "flex items-center bg-zinc-800/60 rounded-md p-0.5",
                                                button {
                                                    class: if is_grid {
                                                        "px-2 py-1 rounded text-[9px] font-semibold bg-zinc-700 text-zinc-200 transition-colors"
                                                    } else {
                                                        "px-2 py-1 rounded text-[9px] font-medium text-zinc-500 hover:text-zinc-300 transition-colors"
                                                    },
                                                    title: "Grid view",
                                                    onclick: move |_| chain_view_mode.set(ChainViewMode::Grid),
                                                    "\u{25A6}"
                                                }
                                                button {
                                                    class: if !is_grid {
                                                        "px-2 py-1 rounded text-[9px] font-semibold bg-zinc-700 text-zinc-200 transition-colors"
                                                    } else {
                                                        "px-2 py-1 rounded text-[9px] font-medium text-zinc-500 hover:text-zinc-300 transition-colors"
                                                    },
                                                    title: "List view",
                                                    onclick: move |_| chain_view_mode.set(ChainViewMode::List),
                                                    "\u{2261}"
                                                }
                                            }
                                        }
                                    }
                                    // Add block button (list mode only)
                                    if view_mode == ChainViewMode::List && selected_preset_id.is_some() {
                                        button {
                                            class: "flex items-center gap-1 px-2.5 py-1 rounded-md text-[10px] font-semibold \
                                                    bg-emerald-500/10 text-emerald-400 border border-emerald-500/20 \
                                                    hover:bg-emerald-500/20 transition-all duration-150",
                                            onclick: move |_| {
                                                show_add_block_picker.set(true);
                                                selected_slot_id.set(None);
                                            },
                                            span { class: "text-emerald-300", "+" }
                                            "Add"
                                        }
                                    }
                                }
                            }

                            // Chain content area — no overflow-hidden here so the
                            // fixed-position block picker can escape the transform stacking context.
                            div { class: "flex-1 min-h-0",
                                if selected_preset_id.is_none() {
                                    div { class: "flex items-center justify-center h-full",
                                        p { class: "text-xs text-zinc-600", "Select a preset to edit its block chain" }
                                    }
                                } else if view_mode == ChainViewMode::Grid {
                                    // ── Dynamic Grid View in colored module container ────────
                                    {
                                        let mc = selected_type.map(module_type_color);
                                        let preset_name = selected_preset_id
                                            .and_then(|pid| presets.iter().find(|p| p.id == pid))
                                            .map(|p| p.name.as_str())
                                            .unwrap_or("Untitled");
                                        let module_name = selected_type
                                            .map(|mt| mt.display_name())
                                            .unwrap_or("Module");
                                        let container_bg = mc.as_ref().map_or(
                                            "background-color: #ffffff08;".to_string(),
                                            |c| format!(
                                                "background-color: {}15; border-color: {}40; backdrop-filter: blur(4px);",
                                                c.bg, c.bg
                                            ),
                                        );
                                        let title_bg = mc.as_ref().map_or(
                                            "background-color: #ffffff10; border-bottom: 1px solid #ffffff15;".to_string(),
                                            |c| format!(
                                                "background-color: {}30; border-bottom: 1px solid {}40;",
                                                c.bg, c.bg
                                            ),
                                        );
                                        let title_fg = mc.as_ref().map_or("#a1a1aa", |c| c.fg);
                                        rsx! {
                                            div {
                                                class: "h-full w-full rounded-xl border-2 flex flex-col overflow-visible",
                                                style: "{container_bg}",
                                                // Title bar matching node_graph_module pattern
                                                div {
                                                    class: "flex items-center gap-2 px-3 flex-shrink-0 select-none",
                                                    style: "height: 32px; {title_bg}",
                                                    span {
                                                        class: "text-xs font-semibold tracking-wide",
                                                        style: "color: {title_fg};",
                                                        "{module_name}"
                                                    }
                                                    span {
                                                        class: "text-[10px] opacity-50",
                                                        style: "color: {title_fg};",
                                                        "\u{2022}"
                                                    }
                                                    span {
                                                        class: "text-[10px] font-medium opacity-70 truncate",
                                                        style: "color: {title_fg};",
                                                        "{preset_name}"
                                                    }
                                                }
                                                // Grid content
                                                div { class: "flex-1 min-h-0",
                                                    super::grid_view::DynamicGridView {
                                                        chain: chain.clone(),
                                                        selected_slot_id: slot_id,
                                                        connections: super::grid_view::GRID_CONNECTIONS.cloned(),
                                                        on_chain_change: move |new_chain: Vec<CompositionSlot>| {
                                                            composition_chain.set(new_chain);
                                                            save_composition_chain(selected_preset_id, composition_chain.cloned());
                                                        },
                                                        on_connections_change: move |new_conns: Vec<super::grid_view::GridConnection>| {
                                                            *super::grid_view::GRID_CONNECTIONS.write() = new_conns;
                                                            save_composition_chain(selected_preset_id, composition_chain.cloned());
                                                        },
                                                        on_select: move |id: Option<Uuid>| {
                                                            selected_slot_id.set(id);
                                                        },
                                                    }
                                                }
                                            }
                                        }
                                    }
                                } else if chain.is_empty() {
                                    div { class: "flex items-center justify-center h-full",
                                        div { class: "text-center",
                                            p { class: "text-xs text-zinc-500 mb-1", "Empty chain" }
                                            p { class: "text-[10px] text-zinc-600",
                                                "Click \"Add\" to start building your signal chain"
                                            }
                                        }
                                    }
                                } else {
                                    // ── List view: horizontal chain strip ──
                                    div { class: "flex items-center gap-1.5 flex-wrap px-3 py-2",
                                        for (idx, slot) in chain.iter().enumerate() {
                                            {
                                                let slot_id_val = slot.id;
                                                let is_slot_selected = slot_id == Some(slot_id_val);
                                                let bt = slot.block_type;
                                                let color = block_type_color(bt);
                                                let slot_name = slot.block_preset_name.as_deref().unwrap_or(bt.display_name());
                                                let dot_style = format!("background-color: {};", color.bg);

                                                rsx! {
                                                    div {
                                                        key: "{slot_id_val}",
                                                        class: "flex items-center gap-1.5",
                                                        if idx > 0 {
                                                            span { class: "text-zinc-700 text-[10px] flex-shrink-0", "\u{2192}" }
                                                        }
                                                        div {
                                                            class: if is_slot_selected {
                                                                "flex flex-col items-center gap-0.5 px-3 py-2 rounded-lg border cursor-pointer transition-all duration-100 \
                                                                 bg-zinc-800/60 border-purple-500/40 min-w-[80px]"
                                                            } else {
                                                                "flex flex-col items-center gap-0.5 px-3 py-2 rounded-lg border cursor-pointer transition-all duration-100 \
                                                                 hover:bg-zinc-800/30 border-zinc-800/50 min-w-[80px]"
                                                            },
                                                            onclick: move |_| {
                                                                selected_slot_id.set(Some(slot_id_val));
                                                                show_add_block_picker.set(false);
                                                            },
                                                            div { class: "flex items-center gap-1.5",
                                                                div {
                                                                    class: "w-2.5 h-2.5 rounded-full flex-shrink-0",
                                                                    style: "{dot_style}",
                                                                }
                                                                span { class: "text-[9px] text-zinc-400 font-mono uppercase",
                                                                    "{bt.display_name()}"
                                                                }
                                                            }
                                                            span { class: "text-[10px] text-zinc-200 font-medium truncate max-w-[100px] text-center",
                                                                "{slot_name}"
                                                            }
                                                            if is_slot_selected {
                                                                button {
                                                                    class: "mt-0.5 text-[8px] text-red-400/60 hover:text-red-400 transition-colors",
                                                                    onclick: move |evt| {
                                                                        evt.stop_propagation();
                                                                        let mut new_chain = composition_chain.cloned();
                                                                        new_chain.retain(|s| s.id != slot_id_val);
                                                                        composition_chain.set(new_chain);
                                                                        selected_slot_id.set(None);
                                                                        save_composition_chain(selected_preset_id, composition_chain.cloned());
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

                        // ── Block Detail / Module Snapshots (bottom ~25%) ──
                        div { class: "flex-1 flex flex-col min-h-0",
                            // Tab bar
                            div { class: "px-3 py-1.5 border-b border-border/30 flex items-center gap-0.5 flex-shrink-0 bg-zinc-900/20",
                                {
                                    let tabs = [
                                        (BlockDetailTab::Settings, "Block Settings"),
                                        (BlockDetailTab::Presets, "Block Presets"),
                                        (BlockDetailTab::Snapshots, "Block Snapshots"),
                                        (BlockDetailTab::ModuleSnapshots, "Module Snapshots"),
                                    ];
                                    rsx! {
                                        for (tab, label) in tabs {
                                            button {
                                                key: "{label}",
                                                class: if detail_tab == tab {
                                                    "px-2.5 py-1 rounded-md text-[10px] font-semibold text-zinc-200 bg-zinc-700/60 transition-colors"
                                                } else {
                                                    "px-2.5 py-1 rounded-md text-[10px] font-medium text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800/40 transition-colors"
                                                },
                                                onclick: move |_| block_detail_tab.set(tab),
                                                "{label}"
                                            }
                                        }
                                    }
                                }
                            }

                            // Tab content
                            div { class: "flex-1 overflow-y-auto min-h-0 px-3 py-2",
                                match detail_tab {
                                    BlockDetailTab::Settings => rsx! {
                                        // Block Settings tab
                                        if let Some(slot) = selected_slot.as_ref() {
                                            {
                                                let bt = slot.block_type;
                                                let color = block_type_color(bt);
                                                let slot_name = slot.block_preset_name.as_deref().unwrap_or("Unassigned");
                                                let dot_style = format!("background-color: {};", color.bg);
                                                let slot_id_for_remove = slot.id;
                                                rsx! {
                                                    div { class: "flex flex-col gap-3",
                                                        // Block type + name
                                                        div { class: "flex items-center gap-2.5",
                                                            div {
                                                                class: "w-5 h-5 rounded-full flex-shrink-0",
                                                                style: "{dot_style}",
                                                            }
                                                            div {
                                                                p { class: "text-xs font-bold text-zinc-200", "{bt.display_name()}" }
                                                                p { class: "text-[10px] text-zinc-500", "{slot_name}" }
                                                            }
                                                        }
                                                        // Plugin name
                                                        if let Some(ref plugin) = slot.plugin_name {
                                                            div { class: "px-2 py-1.5 rounded-md bg-zinc-800/40 border border-zinc-800/60",
                                                                p { class: "text-[10px] text-zinc-400 font-mono", "{plugin}" }
                                                            }
                                                        }
                                                        // Grid position
                                                        div { class: "flex items-center gap-2",
                                                            span { class: "text-[9px] text-zinc-600 font-mono",
                                                                "Position: col {slot.col}, row {slot.row}"
                                                            }
                                                        }
                                                        // Remove button
                                                        button {
                                                            class: "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-[10px] font-semibold \
                                                                    bg-red-500/10 text-red-400 border border-red-500/20 \
                                                                    hover:bg-red-500/20 transition-all duration-150 w-fit",
                                                            onclick: move |_| {
                                                                let mut new_chain = composition_chain.cloned();
                                                                new_chain.retain(|s| s.id != slot_id_for_remove);
                                                                composition_chain.set(new_chain);
                                                                selected_slot_id.set(None);
                                                                save_composition_chain(selected_preset_id, composition_chain.cloned());
                                                            },
                                                            "Remove Block"
                                                        }
                                                    }
                                                }
                                            }
                                        } else {
                                            div { class: "flex items-center justify-center h-full",
                                                p { class: "text-xs text-zinc-600",
                                                    "Select a block in the grid to edit its settings"
                                                }
                                            }
                                        }
                                    },
                                    BlockDetailTab::Presets => rsx! {
                                        // Block Presets tab — show saved presets for the selected block type
                                        if let Some(ref slot) = selected_slot {
                                            div { class: "flex flex-col gap-1.5",
                                                p { class: "text-[10px] text-zinc-500 mb-1",
                                                    "Saved presets for {slot.block_type.display_name()} blocks:"
                                                }
                                                p { class: "text-[10px] text-zinc-600 italic",
                                                    "Use the Block Editor tab to capture and manage block presets."
                                                }
                                            }
                                        } else {
                                            div { class: "flex items-center justify-center h-full",
                                                p { class: "text-xs text-zinc-600",
                                                    "Select a block to browse its presets"
                                                }
                                            }
                                        }
                                    },
                                    BlockDetailTab::Snapshots => rsx! {
                                        // Block Snapshots tab
                                        if selected_slot.is_some() {
                                            div { class: "flex items-center justify-center h-full",
                                                p { class: "text-[10px] text-zinc-600 italic",
                                                    "Assign a block preset first, then manage snapshots here."
                                                }
                                            }
                                        } else {
                                            div { class: "flex items-center justify-center h-full",
                                                p { class: "text-xs text-zinc-600",
                                                    "Select a block to view its snapshots"
                                                }
                                            }
                                        }
                                    },
                                    BlockDetailTab::ModuleSnapshots => rsx! {
                                        // Module Snapshots tab — the existing snapshot grid moved here
                                        if selected_preset_id.is_none() || snapshots.is_empty() {
                                            div { class: "flex items-center justify-center h-full",
                                                div { class: "text-center",
                                                    p { class: "text-xs text-zinc-600 mb-1",
                                                        if selected_preset_id.is_none() {
                                                            "Select a preset to view module snapshots"
                                                        } else {
                                                            "No snapshots yet"
                                                        }
                                                    }
                                                    if selected_preset_id.is_some() {
                                                        button {
                                                            class: "flex items-center gap-1 px-2.5 py-1 rounded-md text-[10px] font-semibold \
                                                                    bg-cyan-500/10 text-cyan-400 border border-cyan-500/20 \
                                                                    hover:bg-cyan-500/20 transition-all duration-150 mx-auto mt-2",
                                                            onclick: move |_| {
                                                                if let Some(pid) = selected_preset_id {
                                                                    let snap_count = snapshots.len();
                                                                    let snap_name = format!("Snap {}", snap_count + 1);
                                                                    spawn(async move {
                                                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                                        match ctl.create_module_snapshot(pid, &snap_name, serde_json::json!({}), false).await {
                                                                            Ok(id) => {
                                                                                debug!("Created module snapshot: {id}");
                                                                                if let Ok(snaps) = ctl.list_module_snapshots(pid).await {
                                                                                    module_snapshots.set(snaps);
                                                                                }
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
                                                }
                                            }
                                        } else {
                                            div { class: "flex flex-col gap-2",
                                                // Header with add button
                                                div { class: "flex items-center justify-between mb-1",
                                                    span { class: "text-[9px] font-mono text-zinc-600",
                                                        "{snapshots.len()} snapshots"
                                                    }
                                                    button {
                                                        class: "flex items-center gap-1 px-2 py-0.5 rounded text-[9px] font-semibold \
                                                                bg-cyan-500/10 text-cyan-400 border border-cyan-500/20 \
                                                                hover:bg-cyan-500/20 transition-all",
                                                        onclick: move |_| {
                                                            if let Some(pid) = selected_preset_id {
                                                                let snap_count = snapshots.len();
                                                                let snap_name = format!("Snap {}", snap_count + 1);
                                                                spawn(async move {
                                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                                    match ctl.create_module_snapshot(pid, &snap_name, serde_json::json!({}), false).await {
                                                                        Ok(id) => {
                                                                            debug!("Created module snapshot: {id}");
                                                                            if let Ok(snaps) = ctl.list_module_snapshots(pid).await {
                                                                                module_snapshots.set(snaps);
                                                                            }
                                                                        }
                                                                        Err(e) => warn!("Create snapshot failed: {e}"),
                                                                    }
                                                                });
                                                            }
                                                        },
                                                        "+"
                                                        "New"
                                                    }
                                                }
                                                // Snapshot grid
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
                                                                                let pid_for_refresh = selected_preset_id;
                                                                                spawn(async move {
                                                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                                                    if let Ok(true) = ctl.delete_module_snapshot(sid).await {
                                                                                        if let Some(pid) = pid_for_refresh {
                                                                                            if let Ok(snaps) = ctl.list_module_snapshots(pid).await {
                                                                                                module_snapshots.set(snaps);
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
                                    },
                                }
                            }
                        }
                }
            },
            status: rsx! {
                div { class: "w-1.5 h-1.5 rounded-full bg-purple-400/60" }
                span { class: "text-[10px] text-zinc-500 font-mono truncate flex-1", "{status}" }
            },
        }

        // Block picker portal — rendered here (above any CSS transform) so
        // position:fixed works relative to the true viewport.
        if let Some((pc, pr)) = *super::grid_view::PICKER_CELL.read() {
            {
                let (click_x, click_y) = *super::grid_view::PICKER_CLICK_POS.read();
                rsx! {
                    super::grid_view::BlockPickerDropdown {
                        col: pc,
                        row: pr,
                        click_x: click_x,
                        click_y: click_y,
                        on_add_slot: move |new_slot: CompositionSlot| {
                            let mut new_chain = composition_chain.cloned();
                            new_chain.push(new_slot);
                            composition_chain.set(new_chain);
                            save_composition_chain(selected_preset_id, composition_chain.cloned());
                            *super::grid_view::PICKER_CELL.write() = None;
                        },
                        on_close: move |_| {
                            *super::grid_view::PICKER_CELL.write() = None;
                        },
                    }
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Save composition chain back to DB
// ─────────────────────────────────────────────────────────────────────────────

fn save_composition_chain(preset_id: Option<Uuid>, chain: Vec<CompositionSlot>) {
    let Some(preset_id) = preset_id else {
        return;
    };
    let connections = super::grid_view::GRID_CONNECTIONS.read().clone();

    // Convert chain + connections to JSON object:
    // { "blocks": [...], "connections": [...] }
    let blocks_array: Vec<serde_json::Value> = chain
        .iter()
        .enumerate()
        .map(|(idx, slot)| {
            serde_json::json!({
                "id": slot.id.to_string(),
                "block": {
                    "name": slot.block_preset_name.as_deref().unwrap_or(slot.block_type.display_name()),
                    "block_type": slot.block_type.display_name(),
                },
                "order": { "value": idx as u32 },
                "col": slot.col,
                "row": slot.row,
            })
        })
        .collect();

    let connections_array: Vec<serde_json::Value> = connections
        .iter()
        .map(|conn| {
            serde_json::json!({
                "from": conn.from_slot_id.to_string(),
                "to": conn.to_slot_id.to_string(),
            })
        })
        .collect();

    let blocks_json = serde_json::json!({
        "blocks": blocks_array,
        "connections": connections_array,
    });

    spawn(async move {
        let Some(ctl) = RIG_SERVICE.read().clone() else {
            return;
        };
        if let Err(e) = ctl
            .update_module_preset(preset_id, None, None, Some(blocks_json), None, None)
            .await
        {
            warn!("Failed to save block chain: {e}");
        } else {
            debug!("Saved block chain for preset {preset_id}");
        }
    });
}

// ─────────────────────────────────────────────────────────────────────────────
// Inline Dialog (modal overlay)
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
    let placeholder = props.placeholder.clone();
    let input_value = props.value.clone();

    // Map accent_color to tailwind classes
    let (focus_border, focus_ring, btn_bg, btn_hover) = match props.accent_color.as_str() {
        "orange" => (
            "focus:border-orange-500/50",
            "focus:ring-orange-500/20",
            "bg-orange-500",
            "hover:bg-orange-400",
        ),
        "cyan" => (
            "focus:border-cyan-500/50",
            "focus:ring-cyan-500/20",
            "bg-cyan-500",
            "hover:bg-cyan-400",
        ),
        _ => (
            "focus:border-purple-500/50",
            "focus:ring-purple-500/20",
            "bg-purple-500",
            "hover:bg-purple-400",
        ),
    };

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center bg-black/60",
            onclick: move |_| props.on_cancel.call(()),
            // Modal card — stop propagation so events don't reach the root handler
            div {
                class: "bg-zinc-900 border border-zinc-700/60 rounded-xl shadow-2xl shadow-black/40 \
                        w-full max-w-md mx-4 overflow-hidden",
                onclick: move |evt| evt.stop_propagation(),
                onkeydown: move |evt| evt.stop_propagation(),
                // Header
                div { class: "px-5 py-4 border-b border-zinc-800/60",
                    h3 { class: "text-sm font-semibold text-zinc-100", "{props.label}" }
                }
                // Body
                div { class: "px-5 py-4",
                    input {
                        class: "w-full bg-zinc-800/80 border border-zinc-700/50 rounded-lg px-3.5 py-2.5 text-sm text-zinc-200 \
                                outline-none {focus_border} focus:ring-1 {focus_ring} \
                                placeholder:text-zinc-600 transition-all duration-150",
                        r#type: "text",
                        placeholder: placeholder,
                        value: input_value,
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
                }
                // Footer
                div { class: "px-5 py-3 border-t border-zinc-800/60 flex items-center justify-end gap-2",
                    button {
                        class: "px-4 py-2 rounded-lg text-xs font-medium text-zinc-400 \
                                hover:text-zinc-200 hover:bg-zinc-800 transition-colors",
                        onclick: move |_| props.on_cancel.call(()),
                        "Cancel"
                    }
                    button {
                        class: "px-4 py-2 rounded-lg text-xs font-semibold {btn_bg} text-white \
                                {btn_hover} transition-colors disabled:opacity-30 disabled:cursor-not-allowed",
                        disabled: is_empty,
                        onclick: move |_| {
                            let val = value_for_btn.trim().to_string();
                            if !val.is_empty() {
                                props.on_submit.call(val);
                            }
                        },
                        "Save"
                    }
                }
            }
        }
    }
}
