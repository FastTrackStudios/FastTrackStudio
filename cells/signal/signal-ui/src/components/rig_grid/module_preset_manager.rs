//! Module preset save/load manager.
//!
//! Provides:
//! - `ModulePreset` -- serializable preset data (nodes + internal wires)
//! - `ModulePresetSaveDialog` -- modal for naming and tagging a new preset
//! - `ModulePresetBrowser` -- modal for searching, filtering, and loading presets
//! - Global signals to control dialog visibility and target module

use crate::components::rig_grid::node_graph::{GraphModule, Node, Wire};
use crate::hooks::use_fuzzy_search;
use crate::prelude::*;
use crate::signals::RIG_NODE_GRAPH;
use facet::Facet;
use signal_control::block::BlockType;
use signal_storage::{load_value, save_value, SqliteBackend};
use std::sync::Arc;
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// Preset Data Types
// ─────────────────────────────────────────────────────────────────────────────

/// A saved module preset containing the module's nodes and internal wiring.
///
/// When loaded, the preset replaces the target module's `nodes` and
/// `internal_wires` while preserving its position, external connections,
/// and identity (id, name, ports).
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ModulePreset {
    /// Unique preset ID.
    pub id: Uuid,
    /// User-assigned preset name.
    pub name: String,
    /// Optional tags for filtering (e.g. "heavy", "clean", "ambient").
    pub tags: Vec<String>,
    /// The block type this preset was captured from.
    pub block_type: BlockType,
    /// Captured nodes (positions are relative to the module origin).
    pub nodes: Vec<Node>,
    /// Captured internal wires.
    pub internal_wires: Vec<Wire>,
}

/// Index of all saved module presets -- stored as a single KV entry so we
/// can list/search without scanning individual keys.
#[derive(Debug, Clone, Default, PartialEq, Facet)]
pub struct ModulePresetIndex {
    pub presets: Vec<ModulePresetEntry>,
}

/// Lightweight entry in the preset index (no node data, just metadata).
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ModulePresetEntry {
    pub id: Uuid,
    pub name: String,
    pub tags: Vec<String>,
    pub block_type: BlockType,
}

// ─────────────────────────────────────────────────────────────────────────────
// KV Keys
// ─────────────────────────────────────────────────────────────────────────────

const PRESET_INDEX_KEY: &str = "rig:module_preset_index";

fn preset_data_key(id: Uuid) -> String {
    format!("rig:module_preset:{id}")
}

// ─────────────────────────────────────────────────────────────────────────────
// Global Signals
// ─────────────────────────────────────────────────────────────────────────────

/// When `Some(module_id)`, the save-preset dialog is open for that module.
pub static MODULE_PRESET_SAVE_OPEN: GlobalSignal<Option<Uuid>> = Signal::global(|| None);

/// When `Some(module_id)`, the load-preset browser is open for that module.
pub static MODULE_PRESET_LOAD_OPEN: GlobalSignal<Option<Uuid>> = Signal::global(|| None);

/// Shared persistence backend (reuses the same instance as graph_persistence).
static PRESET_KV_BACKEND: GlobalSignal<Option<Arc<SqliteBackend>>> = Signal::global(|| None);

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────

/// Ensure the shared KV backend is available, opening it lazily if needed.
async fn ensure_backend() -> Option<Arc<SqliteBackend>> {
    if let Some(existing) = PRESET_KV_BACKEND.read().clone() {
        return Some(existing);
    }
    match SqliteBackend::from_default_path().await {
        Ok(b) => {
            let shared = Arc::new(b);
            *PRESET_KV_BACKEND.write() = Some(shared.clone());
            Some(shared)
        }
        Err(e) => {
            tracing::warn!("Failed to open preset persistence backend: {e}");
            None
        }
    }
}

/// Load the preset index from storage.
async fn load_preset_index(backend: &SqliteBackend) -> ModulePresetIndex {
    match load_value::<ModulePresetIndex>(backend, PRESET_INDEX_KEY).await {
        Ok(Some(index)) => index,
        Ok(None) => ModulePresetIndex::default(),
        Err(e) => {
            tracing::warn!("Failed to load preset index: {e}");
            ModulePresetIndex::default()
        }
    }
}

/// Capture the current module state as a preset.
fn capture_preset_from_module(
    module: &GraphModule,
    name: String,
    tags: Vec<String>,
) -> ModulePreset {
    ModulePreset {
        id: Uuid::new_v4(),
        name,
        tags,
        block_type: module.block_type,
        nodes: module.nodes.clone(),
        internal_wires: module.internal_wires.clone(),
    }
}

/// Apply a preset to a module, replacing nodes and internal wires while
/// preserving the module's identity, position, and external connections.
fn apply_preset_to_module(module: &mut GraphModule, preset: &ModulePreset) {
    // Replace internal state with preset data, assigning fresh IDs so
    // multiple modules can load the same preset without ID collisions.
    let mut id_map = std::collections::HashMap::new();
    let mut new_nodes = preset.nodes.clone();
    for node in &mut new_nodes {
        let old_id = node.id;
        let new_id = Uuid::new_v4();
        node.id = new_id;
        id_map.insert(old_id, new_id);
    }

    let mut new_wires = preset.internal_wires.clone();
    for wire in &mut new_wires {
        wire.id = Uuid::new_v4();
        if let Some(&mapped) = id_map.get(&wire.from_node) {
            wire.from_node = mapped;
        }
        if let Some(&mapped) = id_map.get(&wire.to_node) {
            wire.to_node = mapped;
        }
    }

    module.nodes = new_nodes;
    module.internal_wires = new_wires;
}

// ─────────────────────────────────────────────────────────────────────────────
// Save Dialog Component
// ─────────────────────────────────────────────────────────────────────────────

/// Modal dialog for saving a module's current configuration as a preset.
///
/// Reads the target module from `MODULE_PRESET_SAVE_OPEN`. On save, persists
/// the preset data + updates the preset index in SQLite.
#[component]
pub fn ModulePresetSaveDialog() -> Element {
    let module_id = match *MODULE_PRESET_SAVE_OPEN.read() {
        Some(id) => id,
        None => return rsx! {},
    };

    let mut preset_name = use_signal(String::new);
    let mut tag_input = use_signal(String::new);
    let mut tags = use_signal(Vec::<String>::new);
    let mut saving = use_signal(|| false);
    let mut error_msg = use_signal(|| Option::<String>::None);

    // Look up the module to show its name in the dialog title
    let module_name = {
        let graph = RIG_NODE_GRAPH.read();
        graph
            .find_module(module_id)
            .map(|m| m.name.clone())
            .unwrap_or_else(|| "Unknown".to_string())
    };

    let on_close = move |_: ()| {
        *MODULE_PRESET_SAVE_OPEN.write() = None;
    };

    let mut on_add_tag = move |_| {
        let tag = tag_input().trim().to_string();
        if !tag.is_empty() && !tags().contains(&tag) {
            tags.write().push(tag);
            tag_input.set(String::new());
        }
    };

    let on_save = move |_| {
        let name = preset_name().trim().to_string();
        if name.is_empty() {
            error_msg.set(Some("Preset name cannot be empty".to_string()));
            return;
        }
        saving.set(true);
        error_msg.set(None);

        let current_tags = tags();
        spawn(async move {
            let Some(backend) = ensure_backend().await else {
                saving.set(false);
                error_msg.set(Some("Storage unavailable".to_string()));
                return;
            };

            // Capture the preset from the current graph state
            let preset = {
                let graph = RIG_NODE_GRAPH.read();
                match graph.find_module(module_id) {
                    Some(module) => capture_preset_from_module(module, name, current_tags),
                    None => {
                        saving.set(false);
                        error_msg.set(Some("Module not found".to_string()));
                        return;
                    }
                }
            };

            // Save preset data
            let data_key = preset_data_key(preset.id);
            if let Err(e) = save_value(backend.as_ref(), &data_key, &preset).await {
                tracing::warn!("Failed to save preset data: {e}");
                saving.set(false);
                error_msg.set(Some(format!("Save failed: {e}")));
                return;
            }

            // Update index
            let mut index = load_preset_index(backend.as_ref()).await;
            index.presets.push(ModulePresetEntry {
                id: preset.id,
                name: preset.name.clone(),
                tags: preset.tags.clone(),
                block_type: preset.block_type,
            });
            if let Err(e) = save_value(backend.as_ref(), PRESET_INDEX_KEY, &index).await {
                tracing::warn!("Failed to save preset index: {e}");
                saving.set(false);
                error_msg.set(Some(format!("Index update failed: {e}")));
                return;
            }

            tracing::info!("Saved module preset '{}' ({})", preset.name, preset.id);
            saving.set(false);
            *MODULE_PRESET_SAVE_OPEN.write() = None;
        });
    };

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center bg-black/70 backdrop-blur-sm",
            onclick: move |_| on_close(()),

            // Dialog
            div {
                class: "bg-zinc-900 rounded-xl border border-zinc-700 shadow-2xl w-[420px] \
                        flex flex-col overflow-hidden",
                onclick: |e| e.stop_propagation(),

                // Header
                div { class: "flex items-center justify-between px-4 py-3 border-b border-zinc-800",
                    h2 { class: "text-lg font-semibold text-zinc-200",
                        "Save Preset -- {module_name}"
                    }
                    button {
                        class: "p-1 rounded hover:bg-zinc-800 text-zinc-400 \
                                hover:text-zinc-200 transition-colors",
                        onclick: move |_| on_close(()),
                        svg {
                            class: "w-5 h-5",
                            fill: "none",
                            stroke: "currentColor",
                            stroke_width: "2",
                            view_box: "0 0 24 24",
                            path {
                                stroke_linecap: "round",
                                stroke_linejoin: "round",
                                d: "M6 18L18 6M6 6l12 12",
                            }
                        }
                    }
                }

                // Body
                div { class: "p-4 flex flex-col gap-4",
                    // Name input
                    div { class: "flex flex-col gap-1",
                        label { class: "text-xs font-medium text-zinc-400", "Preset Name" }
                        input {
                            class: "w-full px-3 py-2 text-sm bg-zinc-800 border border-zinc-700 \
                                    rounded-lg placeholder:text-zinc-500 focus:outline-none \
                                    focus:ring-1 focus:ring-zinc-600 text-zinc-300",
                            r#type: "text",
                            placeholder: "My Awesome Preset",
                            value: "{preset_name}",
                            oninput: move |e| preset_name.set(e.value().clone()),
                        }
                    }

                    // Tags input
                    div { class: "flex flex-col gap-1",
                        label { class: "text-xs font-medium text-zinc-400", "Tags (optional)" }
                        div { class: "flex gap-2",
                            input {
                                class: "flex-1 px-3 py-2 text-sm bg-zinc-800 border border-zinc-700 \
                                        rounded-lg placeholder:text-zinc-500 focus:outline-none \
                                        focus:ring-1 focus:ring-zinc-600 text-zinc-300",
                                r#type: "text",
                                placeholder: "e.g. heavy, clean, ambient",
                                value: "{tag_input}",
                                oninput: move |e| tag_input.set(e.value().clone()),
                                onkeydown: move |e| {
                                    if e.key() == Key::Enter {
                                        on_add_tag(());
                                    }
                                },
                            }
                            button {
                                class: "px-3 py-2 text-sm bg-zinc-700 hover:bg-zinc-600 \
                                        rounded-lg text-zinc-300 transition-colors",
                                onclick: move |_| on_add_tag(()),
                                "Add"
                            }
                        }

                        // Tag pills
                        if !tags().is_empty() {
                            div { class: "flex flex-wrap gap-1 mt-1",
                                for (idx, tag) in tags().iter().enumerate() {
                                    {
                                        let tag_display = tag.clone();
                                        rsx! {
                                            span {
                                                key: "{idx}",
                                                class: "inline-flex items-center gap-1 px-2 py-0.5 \
                                                        text-xs bg-zinc-700 text-zinc-300 rounded-full",
                                                "{tag_display}"
                                                button {
                                                    class: "text-zinc-500 hover:text-zinc-200 ml-0.5",
                                                    onclick: move |_| {
                                                        tags.write().retain(|t| t != &tag_display);
                                                    },
                                                    "x"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Error message
                    if let Some(err) = error_msg() {
                        div { class: "text-xs text-red-400 bg-red-500/10 px-3 py-2 rounded",
                            "{err}"
                        }
                    }
                }

                // Footer
                div { class: "flex items-center justify-end gap-2 px-4 py-3 border-t border-zinc-800",
                    button {
                        class: "px-4 py-2 text-sm rounded-lg text-zinc-400 hover:text-zinc-200 \
                                hover:bg-zinc-800 transition-colors",
                        onclick: move |_| on_close(()),
                        "Cancel"
                    }
                    button {
                        class: "px-4 py-2 text-sm rounded-lg bg-blue-600 hover:bg-blue-500 \
                                text-white font-medium transition-colors disabled:opacity-50 \
                                disabled:cursor-not-allowed",
                        disabled: saving(),
                        onclick: on_save,
                        if saving() { "Saving..." } else { "Save Preset" }
                    }
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Preset Browser Component
// ─────────────────────────────────────────────────────────────────────────────

/// Modal browser for searching, filtering, and loading module presets.
///
/// Reads the target module from `MODULE_PRESET_LOAD_OPEN`. On load, applies
/// the selected preset to the module, replacing its nodes and internal wires
/// while preserving position and external connections.
#[component]
pub fn ModulePresetBrowser() -> Element {
    let module_id = match *MODULE_PRESET_LOAD_OPEN.read() {
        Some(id) => id,
        None => return rsx! {},
    };

    let mut search_query = use_signal(String::new);
    let mut presets = use_signal(Vec::<ModulePresetEntry>::new);
    let mut loading = use_signal(|| true);
    let mut applying = use_signal(|| false);
    let mut error_msg = use_signal(|| Option::<String>::None);

    // Look up the target module's block type for filtering
    let target_block_type = {
        let graph = RIG_NODE_GRAPH.read();
        graph.find_module(module_id).map(|m| m.block_type)
    };

    let module_name = {
        let graph = RIG_NODE_GRAPH.read();
        graph
            .find_module(module_id)
            .map(|m| m.name.clone())
            .unwrap_or_else(|| "Unknown".to_string())
    };

    // Load preset index on mount
    use_effect(move || {
        spawn(async move {
            loading.set(true);
            let Some(backend) = ensure_backend().await else {
                loading.set(false);
                return;
            };
            let index = load_preset_index(backend.as_ref()).await;
            presets.set(index.presets);
            loading.set(false);
        });
    });

    // Filter presets matching the module's block type
    let filtered_by_type = use_memo(move || {
        let all = presets();
        match target_block_type {
            Some(bt) => all.into_iter().filter(|p| p.block_type == bt).collect(),
            None => all,
        }
    });

    // Apply fuzzy search on top of type-filtered presets
    let search_results =
        use_fuzzy_search(filtered_by_type, search_query, |p: &ModulePresetEntry| {
            let tag_str = p.tags.join(" ");
            format!("{} {}", p.name, tag_str)
        });

    let on_close = move |_: ()| {
        *MODULE_PRESET_LOAD_OPEN.write() = None;
    };

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center bg-black/70 backdrop-blur-sm",
            onclick: move |_| on_close(()),

            // Dialog
            div {
                class: "bg-zinc-900 rounded-xl border border-zinc-700 shadow-2xl w-[520px] \
                        max-h-[70vh] flex flex-col overflow-hidden",
                onclick: |e| e.stop_propagation(),

                // Header
                div { class: "flex items-center justify-between px-4 py-3 border-b border-zinc-800",
                    h2 { class: "text-lg font-semibold text-zinc-200",
                        "Load Preset -- {module_name}"
                    }
                    button {
                        class: "p-1 rounded hover:bg-zinc-800 text-zinc-400 \
                                hover:text-zinc-200 transition-colors",
                        onclick: move |_| on_close(()),
                        svg {
                            class: "w-5 h-5",
                            fill: "none",
                            stroke: "currentColor",
                            stroke_width: "2",
                            view_box: "0 0 24 24",
                            path {
                                stroke_linecap: "round",
                                stroke_linejoin: "round",
                                d: "M6 18L18 6M6 6l12 12",
                            }
                        }
                    }
                }

                // Search input
                div { class: "px-4 py-3 border-b border-zinc-800",
                    input {
                        class: "w-full px-3 py-2 text-sm bg-zinc-800 border border-zinc-700 \
                                rounded-lg placeholder:text-zinc-500 focus:outline-none \
                                focus:ring-1 focus:ring-zinc-600 text-zinc-300",
                        r#type: "text",
                        placeholder: "Search presets...",
                        value: "{search_query}",
                        oninput: move |e| search_query.set(e.value().clone()),
                    }
                }

                // Preset list
                div { class: "flex-1 overflow-y-auto p-4",
                    if loading() {
                        div { class: "flex items-center justify-center h-24 text-zinc-500 text-sm",
                            "Loading presets..."
                        }
                    } else if search_results.read().is_empty() {
                        div { class: "flex items-center justify-center h-24 text-zinc-500 text-sm",
                            "No presets found"
                        }
                    } else {
                        div { class: "flex flex-col gap-2",
                            for entry in search_results.read().iter() {
                                {
                                    let preset_id = entry.id;
                                    let preset_name = entry.name.clone();
                                    let preset_tags = entry.tags.clone();
                                    rsx! {
                                        PresetListItem {
                                            key: "{preset_id}",
                                            name: preset_name,
                                            tags: preset_tags,
                                            disabled: applying(),
                                            on_load: move |_| {
                                                applying.set(true);
                                                error_msg.set(None);
                                                spawn(async move {
                                                    let Some(backend) = ensure_backend().await else {
                                                        applying.set(false);
                                                        error_msg.set(Some("Storage unavailable".to_string()));
                                                        return;
                                                    };

                                                    let data_key = preset_data_key(preset_id);
                                                    match load_value::<ModulePreset>(backend.as_ref(), &data_key).await {
                                                        Ok(Some(preset)) => {
                                                            let mut graph = RIG_NODE_GRAPH.write();
                                                            if let Some(module) = graph.find_module_mut(module_id) {
                                                                apply_preset_to_module(module, &preset);
                                                                tracing::info!(
                                                                    "Loaded preset '{}' into module '{}'",
                                                                    preset.name,
                                                                    module.name
                                                                );
                                                            }
                                                            drop(graph);
                                                            applying.set(false);
                                                            *MODULE_PRESET_LOAD_OPEN.write() = None;
                                                        }
                                                        Ok(None) => {
                                                            applying.set(false);
                                                            error_msg.set(Some("Preset data not found".to_string()));
                                                        }
                                                        Err(e) => {
                                                            applying.set(false);
                                                            error_msg.set(Some(format!("Load failed: {e}")));
                                                        }
                                                    }
                                                });
                                            },
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Error message
                if let Some(err) = error_msg() {
                    div { class: "px-4 py-2 border-t border-zinc-800",
                        div { class: "text-xs text-red-400 bg-red-500/10 px-3 py-2 rounded",
                            "{err}"
                        }
                    }
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Preset List Item
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct PresetListItemProps {
    name: String,
    tags: Vec<String>,
    disabled: bool,
    on_load: EventHandler<()>,
}

#[component]
fn PresetListItem(props: PresetListItemProps) -> Element {
    rsx! {
        div {
            class: "flex items-center justify-between p-3 rounded-lg bg-zinc-800 \
                    hover:bg-zinc-750 border border-zinc-700 hover:border-zinc-600 \
                    transition-all group",

            // Left: name and tags
            div { class: "flex-1 min-w-0",
                div { class: "font-medium text-sm text-zinc-200 truncate",
                    "{props.name}"
                }
                if !props.tags.is_empty() {
                    div { class: "flex flex-wrap gap-1 mt-1",
                        for tag in &props.tags {
                            span {
                                class: "text-[10px] px-1.5 py-0.5 rounded-full \
                                        bg-zinc-700 text-zinc-400",
                                "{tag}"
                            }
                        }
                    }
                }
            }

            // Right: load button
            button {
                class: "ml-3 px-3 py-1.5 text-xs rounded-lg bg-blue-600 hover:bg-blue-500 \
                        text-white font-medium transition-colors opacity-0 \
                        group-hover:opacity-100 disabled:opacity-50 disabled:cursor-not-allowed",
                disabled: props.disabled,
                onclick: move |_| props.on_load.call(()),
                "Load"
            }
        }
    }
}
