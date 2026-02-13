//! Preset Editor View — compose modules into full rig configurations.
//!
//! Three-panel layout (matches module_editor pattern):
//!
//! Left (w-64): Preset browser with DB load, fuzzy search, category filter,
//!              favorite toggle, snapshot count badge. "New Preset" button.
//! Center: Preset configuration — editable name, module assignment table
//!         (one row per module type in signal chain), preset snapshots grid,
//!         and tags editor.
//! Right (w-72): Contextual detail — selected snapshot detail with per-module
//!              overrides, or selected module detail.

use crate::components::rig_grid::block_colors::{block_type_color, BlockColor};
use crate::components::shared::EntityEditor;
use crate::prelude::*;
use crate::signals::{RIG_AVAILABLE_PRESETS, RIG_SERVICE};
use signal_control::block::BlockType;
use signal_control::module::ModuleType;
use tracing::{debug, info, warn};
use uuid::Uuid;

/// Lightweight wrapper for storing module assignments in the preset `data` column.
/// Must implement Facet for the `update_rig_preset_data` API.
#[derive(Debug, Clone, facet::Facet)]
struct PresetAssignmentData {
    module_assignments: Vec<ModuleAssignmentEntry>,
}

/// A single module type -> module preset ID mapping.
#[derive(Debug, Clone, facet::Facet)]
struct ModuleAssignmentEntry {
    module_type: String,
    module_preset_id: String,
}

// ─────────────────────────────────────────────────────────────────────────────
// Module chain definition (signal flow order)
// ─────────────────────────────────────────────────────────────────────────────

struct ChainSlotDef {
    module_type: ModuleType,
    name: &'static str,
    icon: &'static str,
}

fn signal_chain_order() -> Vec<ChainSlotDef> {
    vec![
        ChainSlotDef {
            module_type: ModuleType::Eq,
            name: "EQ",
            icon: "E",
        },
        ChainSlotDef {
            module_type: ModuleType::Dynamics,
            name: "Dynamics",
            icon: "C",
        },
        ChainSlotDef {
            module_type: ModuleType::Drive,
            name: "Drive",
            icon: "D",
        },
        ChainSlotDef {
            module_type: ModuleType::Amp,
            name: "Amp",
            icon: "A",
        },
        ChainSlotDef {
            module_type: ModuleType::PostEq,
            name: "Post EQ",
            icon: "Q",
        },
        ChainSlotDef {
            module_type: ModuleType::Modulation,
            name: "Modulation",
            icon: "M",
        },
        ChainSlotDef {
            module_type: ModuleType::Time,
            name: "Time",
            icon: "T",
        },
        ChainSlotDef {
            module_type: ModuleType::Motion,
            name: "Motion",
            icon: "W",
        },
        ChainSlotDef {
            module_type: ModuleType::Special,
            name: "Special",
            icon: "S",
        },
        ChainSlotDef {
            module_type: ModuleType::Master,
            name: "Master",
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

// ─────────────────────────────────────────────────────────────────────────────
// Category filter tabs
// ─────────────────────────────────────────────────────────────────────────────

/// Category filter for the preset browser sidebar.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum CategoryFilter {
    #[default]
    All,
    Clean,
    Crunch,
    Drive,
    Lead,
    Ambient,
}

impl CategoryFilter {
    fn label(self) -> &'static str {
        match self {
            Self::All => "All",
            Self::Clean => "Clean",
            Self::Crunch => "Crunch",
            Self::Drive => "Drive",
            Self::Lead => "Lead",
            Self::Ambient => "Amb",
        }
    }

    fn all() -> &'static [CategoryFilter] {
        &[
            CategoryFilter::All,
            CategoryFilter::Clean,
            CategoryFilter::Crunch,
            CategoryFilter::Drive,
            CategoryFilter::Lead,
            CategoryFilter::Ambient,
        ]
    }

    /// Check if a preset's category JSON matches this filter.
    fn matches_category(self, category_json: &serde_json::Value) -> bool {
        if self == Self::All {
            return true;
        }
        let target = match self {
            Self::Clean => "Clean",
            Self::Crunch => "Crunch",
            Self::Drive => "Drive",
            Self::Lead => "Lead",
            Self::Ambient => "Ambient",
            Self::All => return true,
        };
        // Category JSON can be a string like "Clean" or an object with base_tone field
        if let Some(s) = category_json.as_str() {
            return s.eq_ignore_ascii_case(target);
        }
        if let Some(obj) = category_json.as_object() {
            // Check all variants: Generic, Genre, SubGenre, etc.
            for (_key, val) in obj {
                if let Some(inner) = val.as_object() {
                    if let Some(bt) = inner.get("base_tone").and_then(|v| v.as_str()) {
                        return bt.eq_ignore_ascii_case(target);
                    }
                }
                if let Some(bt) = val.as_str() {
                    if bt.eq_ignore_ascii_case(target) {
                        return true;
                    }
                }
            }
        }
        false
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Right panel detail mode
// ─────────────────────────────────────────────────────────────────────────────

/// What the right panel is showing.
#[derive(Debug, Clone, PartialEq)]
enum DetailMode {
    /// Quick guide (default, nothing selected).
    Guide,
    /// Snapshot detail view.
    Snapshot(Uuid),
    /// Module assignment detail.
    ModuleSlot(String),
}

// ─────────────────────────────────────────────────────────────────────────────
// Main Component
// ─────────────────────────────────────────────────────────────────────────────

#[component]
pub fn PresetEditorView() -> Element {
    // All editor state is component-local -- fresh on each mount, no stale globals.
    let mut preset_selected_id = use_signal(|| None::<Uuid>);
    let mut preset_list = use_signal(Vec::<signal_control::preset_entity::Model>::new);
    let mut preset_snapshots = use_signal(Vec::<signal_control::snapshot_entity::Model>::new);
    let mut available_module_presets =
        use_signal(Vec::<signal_control::module_preset_entity::Model>::new);
    let mut module_assignments = use_signal(std::collections::HashMap::<String, Uuid>::new);
    let mut preset_editor_status = use_signal(|| "Select a preset".to_string());
    let mut preset_search = use_signal(String::new);
    let mut category_filter = use_signal(CategoryFilter::default);
    let mut detail_mode = use_signal(|| DetailMode::Guide);
    let mut tag_input = use_signal(String::new);

    // Snapshot counts cache: preset_id -> count
    let mut snapshot_counts = use_signal(std::collections::HashMap::<Uuid, usize>::new);

    // Dialog state
    let mut show_new_dialog = use_signal(|| false);
    let mut new_name = use_signal(String::new);
    let mut show_rename_dialog = use_signal(|| false);
    let mut rename_value = use_signal(String::new);
    let mut show_new_snapshot_dialog = use_signal(|| false);
    let mut new_snapshot_name = use_signal(String::new);

    // ── Async Refresh Helpers ────────────────────────────────────────

    let refresh_preset_list = move || {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_rig_presets().await {
                Ok(presets) => {
                    // Also fetch snapshot counts for each preset
                    let mut counts = std::collections::HashMap::new();
                    for p in &presets {
                        if let Ok(snaps) = ctl.list_rig_preset_snapshots(p.id).await {
                            counts.insert(p.id, snaps.len());
                        }
                    }
                    snapshot_counts.set(counts);
                    preset_list.set(presets);
                }
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
                Ok(snaps) => {
                    snapshot_counts.write().insert(preset_id, snaps.len());
                    preset_snapshots.set(snaps);
                }
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

    // Re-fetch when global available presets signal changes
    {
        let global_presets_len = RIG_AVAILABLE_PRESETS.read().len();
        use_effect(move || {
            let _ = global_presets_len;
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
    let active_filter = *category_filter.read();
    let current_detail = detail_mode.cloned();
    let snap_counts = snapshot_counts.cloned();

    // Filter presets by search and category
    let filtered_presets: Vec<_> = presets
        .iter()
        .filter(|p| {
            // Text search
            if !search_text.is_empty() {
                let q = search_text.to_lowercase();
                if !p.name.to_lowercase().contains(&q) {
                    return false;
                }
            }
            // Category filter
            active_filter.matches_category(&p.category)
        })
        .cloned()
        .collect();

    let selected_preset = selected_id.and_then(|id| presets.iter().find(|p| p.id == id).cloned());
    let has_preset_selected = selected_preset.is_some();

    // Pre-extract tags JSON so we can use it in multiple closures without moving selected_preset.
    let preset_tags_json: serde_json::Value = selected_preset
        .as_ref()
        .map(|p| p.tags.clone())
        .unwrap_or(serde_json::json!([]));
    let preset_tags: Vec<String> = preset_tags_json
        .as_array()
        .map(|a| {
            a.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    // Helper: persist module assignments to DB
    let save_assignments =
        move |preset_id: Uuid, assigns: std::collections::HashMap<String, Uuid>| {
            spawn(async move {
                let Some(ctl) = RIG_SERVICE.read().clone() else {
                    return;
                };
                let entries: Vec<ModuleAssignmentEntry> = assigns
                    .iter()
                    .map(|(mt, mp_id)| ModuleAssignmentEntry {
                        module_type: mt.clone(),
                        module_preset_id: mp_id.to_string(),
                    })
                    .collect();
                let data = PresetAssignmentData {
                    module_assignments: entries,
                };
                if let Err(e) = ctl.update_rig_preset_data(preset_id, &data).await {
                    warn!("Failed to save module assignments: {e}");
                } else {
                    debug!("Saved module assignments for preset {preset_id}");
                }
            })
        };

    // Default chain
    let default_chain = signal_chain_order();

    // Selected snapshot for right panel
    let selected_snapshot = match &current_detail {
        DetailMode::Snapshot(sid) => snapshots.iter().find(|s| s.id == *sid).cloned(),
        _ => None,
    };

    // Selected module slot for right panel
    let selected_module_slot = match &current_detail {
        DetailMode::ModuleSlot(ref key) => Some(key.clone()),
        _ => None,
    };

    rsx! {
        EntityEditor {
            accent_gradient: Some("from-amber-500 via-rose-400 to-violet-500".to_string()),
            left_width: "w-64".to_string(),
            right_width: "w-72".to_string(),
            left: rsx! {
                // ── Header + search ──────────────────────────────
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
                                show_new_dialog.set(true);
                            },
                            span { class: "text-amber-300", "+" }
                            "New"
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

                // ── Category filter tabs ─────────────────────────
                div { class: "px-3 py-1.5 border-b border-border/20 flex items-center gap-1 flex-shrink-0 overflow-x-auto",
                    for cat in CategoryFilter::all() {
                        {
                            let c = *cat;
                            let is_active = active_filter == c;
                            rsx! {
                                button {
                                    key: "{c.label()}",
                                    class: if is_active {
                                        "px-2 py-1 rounded text-[9px] font-semibold bg-amber-500/20 text-amber-300 \
                                         border border-amber-500/30 transition-colors"
                                    } else {
                                        "px-2 py-1 rounded text-[9px] font-medium text-zinc-500 \
                                         hover:text-zinc-300 hover:bg-zinc-800/40 border border-transparent transition-colors"
                                    },
                                    onclick: move |_| category_filter.set(c),
                                    "{c.label()}"
                                }
                            }
                        }
                    }
                }

                // ── New preset dialog ────────────────────────────
                if show_new_dialog() {
                    div { class: "px-3 py-2 border-b border-border/30 bg-zinc-900/60 flex flex-col gap-2 flex-shrink-0",
                        span { class: "text-[9px] font-semibold text-zinc-500 uppercase tracking-wider", "New Preset" }
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
                        div { class: "flex gap-1",
                            button {
                                class: "flex-1 px-2 py-1 rounded text-[9px] font-semibold bg-amber-500/80 text-white \
                                        hover:bg-amber-500 transition-colors disabled:opacity-25",
                                disabled: new_name().trim().is_empty(),
                                onclick: move |_| {
                                    let val = new_name().trim().to_string();
                                    if val.is_empty() { return; }
                                    show_new_dialog.set(false);
                                    spawn(async move {
                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                        let preset_meta = signal_control::preset::PresetMetadata::new(
                                            &val,
                                            signal_control::category::PresetCategory::default(),
                                        );
                                        match ctl.create_rig_preset::<signal_control::preset::PresetMetadata>(
                                            &val, None,
                                            serde_json::json!("Clean"),
                                            serde_json::json!([]),
                                            &preset_meta,
                                        ).await {
                                            Ok(id) => {
                                                info!("Created rig preset: {id}");
                                                preset_editor_status.set(format!("Created '{}'", val));
                                                if let Ok(list) = ctl.list_rig_presets().await {
                                                    preset_list.set(list);
                                                }
                                                preset_selected_id.set(Some(id));
                                                preset_snapshots.set(Vec::new());
                                                module_assignments.set(std::collections::HashMap::new());
                                                detail_mode.set(DetailMode::Guide);
                                            }
                                            Err(e) => {
                                                warn!("Create preset failed: {e}");
                                                preset_editor_status.set(format!("Failed: {e}"));
                                            }
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

                // ── Preset list ──────────────────────────────────
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
                                let cat_json = preset.category.clone();
                                let snap_count = snap_counts.get(&pid).copied().unwrap_or(0);

                                // Extract category display label
                                let cat_label = if let Some(s) = cat_json.as_str() {
                                    s.to_string()
                                } else {
                                    String::new()
                                };

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
                                            detail_mode.set(DetailMode::Guide);
                                            refresh_preset_snapshots(pid);

                                            // Load module assignments from preset data JSON
                                            spawn(async move {
                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                match ctl.get_rig_preset_row(pid).await {
                                                    Ok(Some(row)) => {
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
                                                                if !map.is_empty() {
                                                                    module_assignments.set(map);
                                                                }
                                                            }
                                                        }
                                                    }
                                                    Ok(None) => warn!("Preset {pid} not found"),
                                                    Err(e) => warn!("Failed to load preset {pid}: {e}"),
                                                }
                                            });
                                        },

                                        // Selection indicator
                                        div {
                                            class: if is_selected {
                                                "w-1.5 h-1.5 rounded-full bg-amber-400 flex-shrink-0"
                                            } else {
                                                "w-1.5 h-1.5 rounded-full bg-zinc-700 flex-shrink-0"
                                            },
                                        }

                                        // Name + category badge + snapshot count
                                        div { class: "flex-1 min-w-0",
                                            div { class: "flex items-center gap-1.5",
                                                span { class: "text-xs font-medium text-zinc-200 truncate",
                                                    "{pname}"
                                                }
                                                if !cat_label.is_empty() {
                                                    span { class: "text-[8px] font-medium px-1.5 py-0.5 rounded bg-zinc-800/80 text-zinc-500 flex-shrink-0",
                                                        "{cat_label}"
                                                    }
                                                }
                                            }
                                            if snap_count > 0 {
                                                {
                                                    let scene_plural = if snap_count != 1 { "s" } else { "" };
                                                    rsx! {
                                                        span { class: "text-[9px] text-zinc-600 font-mono",
                                                            "{snap_count} scene{scene_plural}"
                                                        }
                                                    }
                                                }
                                            }
                                        }

                                        // Favorite star
                                        if is_fav {
                                            span { class: "text-[9px] text-amber-400 flex-shrink-0", "\u{2605}" }
                                        }

                                        // Action buttons when selected
                                        if is_selected {
                                            div { class: "flex items-center gap-0.5 flex-shrink-0",
                                                // Favorite toggle
                                                button {
                                                    class: "p-1 rounded text-zinc-500 hover:text-amber-400 hover:bg-amber-500/10 transition-colors",
                                                    title: if is_fav { "Remove favorite" } else { "Add favorite" },
                                                    onclick: move |evt| {
                                                        evt.stop_propagation();
                                                        let new_fav = !is_fav;
                                                        spawn(async move {
                                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                            if let Err(e) = ctl.update_rig_preset_metadata(pid, None, None, None, None, Some(new_fav)).await {
                                                                warn!("Toggle favorite failed: {e}");
                                                            } else if let Ok(list) = ctl.list_rig_presets().await {
                                                                preset_list.set(list);
                                                            }
                                                        });
                                                    },
                                                    span { class: "text-[10px]",
                                                        if is_fav { "\u{2605}" } else { "\u{2606}" }
                                                    }
                                                }
                                                // Rename
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
                                                // Delete
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
                                                                    module_assignments.set(std::collections::HashMap::new());
                                                                    detail_mode.set(DetailMode::Guide);
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
                    // ── Empty state ──────────────────────────────
                    div { class: "flex-1 flex items-center justify-center",
                        div { class: "text-center max-w-xs",
                            div { class: "w-12 h-12 rounded-xl bg-zinc-800/60 border border-zinc-700/40 flex items-center justify-center mx-auto mb-4",
                                span { class: "text-xl text-zinc-600", "\u{266B}" }
                            }
                            p { class: "text-sm font-medium text-zinc-400 mb-1", "Select a Preset" }
                            p { class: "text-xs text-zinc-600 leading-relaxed",
                                "Choose a preset from the left or create a new one to configure its module assignments and snapshots."
                            }
                        }
                    }
                } else {
                    // ── Rename dialog ─────────────────────────────
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

                    // ── Preset header ─────────────────────────────
                    div { class: "px-4 py-2.5 border-b border-border/30 flex items-center gap-3 flex-shrink-0 bg-zinc-900/30",
                        if let Some(ref p) = selected_preset {
                            span { class: "text-xs font-bold text-zinc-200", "{p.name}" }
                            if p.is_favorite {
                                span { class: "text-[10px] text-amber-400", "\u{2605}" }
                            }
                        }
                        div { class: "flex-1" }
                        if let Some(ref p) = selected_preset {
                            {
                                let cat_display = if let Some(s) = p.category.as_str() {
                                    s.to_string()
                                } else {
                                    "Uncategorized".to_string()
                                };
                                rsx! {
                                    span { class: "text-[9px] font-medium px-2 py-0.5 rounded bg-zinc-800/80 text-zinc-500",
                                        "{cat_display}"
                                    }
                                }
                            }
                        }
                    }

                    // ── Module Assignment Table (top ~55%) ────────
                    div { class: "h-[55%] flex flex-col min-h-0 border-b border-border/20 flex-shrink-0",
                        // Section header
                        div { class: "px-4 py-2 border-b border-border/30 flex items-center gap-3 flex-shrink-0",
                            span { class: "text-[10px] font-bold text-zinc-500 uppercase tracking-[0.1em]",
                                "Signal Chain"
                            }
                            {
                                let assigned_count = assignments.len();
                                let total_count = default_chain.len();
                                rsx! {
                                    span { class: "text-[9px] font-mono text-zinc-600",
                                        "{assigned_count}/{total_count} assigned"
                                    }
                                }
                            }
                            div { class: "flex-1" }
                            span { class: "text-[9px] text-zinc-600", "click row to assign module" }
                        }

                        // Module rows
                        div { class: "flex-1 overflow-y-auto min-h-0",
                            for (idx, slot_def) in default_chain.iter().enumerate() {
                                {
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

                                    // Module presets for this type
                                    let type_modules: Vec<_> = available_modules.iter()
                                        .filter(|m| m.module_type == type_key)
                                        .cloned()
                                        .collect();

                                    let is_detail_selected = selected_module_slot.as_deref() == Some(&type_key);
                                    let type_key_for_detail = type_key.clone();
                                    let type_key_for_clear = type_key.clone();

                                    // Pre-extract style strings with hex colors
                                    let row_bg = format!("background-color: {}08;", mc.bg);
                                    let row_bg_selected = format!("background-color: {}15; border-color: {}40;", mc.bg, mc.bg);
                                    let icon_bg = format!("background-color: {}25; color: {};", mc.bg, mc.fg);
                                    let assigned_color = format!("color: {};", mc.fg);

                                    rsx! {
                                        div {
                                            key: "{type_key}-{idx}",
                                            class: if is_detail_selected {
                                                "flex items-center gap-3 px-4 py-2.5 border-b border-border/15 cursor-pointer \
                                                 transition-all duration-100 border-l-2"
                                            } else {
                                                "flex items-center gap-3 px-4 py-2.5 border-b border-border/15 cursor-pointer \
                                                 hover:brightness-110 transition-all duration-100 border-l-2 border-l-transparent"
                                            },
                                            style: if is_detail_selected { "{row_bg_selected}" } else { "{row_bg}" },
                                            onclick: move |_| {
                                                detail_mode.set(DetailMode::ModuleSlot(type_key_for_detail.clone()));
                                            },

                                            // Module type icon
                                            div {
                                                class: "w-6 h-6 rounded flex items-center justify-center text-[9px] font-bold flex-shrink-0",
                                                style: "{icon_bg}",
                                                "{icon}"
                                            }

                                            // Module type name
                                            div { class: "w-20 flex-shrink-0",
                                                span { class: "text-[11px] font-semibold text-zinc-300",
                                                    "{mt_name}"
                                                }
                                            }

                                            // Assigned module preset (or empty)
                                            div { class: "flex-1 min-w-0",
                                                if let Some(ref aname) = assigned_name {
                                                    div { class: "flex items-center gap-1.5",
                                                        div {
                                                            class: "w-1.5 h-1.5 rounded-full flex-shrink-0",
                                                            style: "background-color: {mc.bg};",
                                                        }
                                                        span {
                                                            class: "text-[11px] font-medium truncate",
                                                            style: "{assigned_color}",
                                                            "{aname}"
                                                        }
                                                    }
                                                } else {
                                                    span { class: "text-[10px] text-zinc-600 italic",
                                                        "-- not assigned --"
                                                    }
                                                }
                                            }

                                            // Module preset dropdown
                                            if !type_modules.is_empty() {
                                                {
                                                    let type_key_for_select = type_key.clone();
                                                    rsx! {
                                                        select {
                                                            class: "bg-zinc-800/60 border border-zinc-700/40 rounded px-2 py-1 text-[10px] text-zinc-300 \
                                                                    outline-none cursor-pointer max-w-[120px] flex-shrink-0",
                                                            value: if let Some(aid) = assigned_id { aid.to_string() } else { String::new() },
                                                            onchange: move |evt| {
                                                                let val = evt.value();
                                                                if val.is_empty() {
                                                                    module_assignments.write().remove(&type_key_for_select);
                                                                } else if let Ok(id) = Uuid::parse_str(&val) {
                                                                    module_assignments.write().insert(type_key_for_select.clone(), id);
                                                                }
                                                                // Persist
                                                                if let Some(pid) = selected_id {
                                                                    save_assignments(pid, module_assignments.cloned());
                                                                }
                                                            },
                                                            option { value: "", "Select..." }
                                                            for mp in type_modules.iter() {
                                                                {
                                                                    let mp_id = mp.id.to_string();
                                                                    let mp_name = mp.name.clone();
                                                                    rsx! {
                                                                        option {
                                                                            key: "{mp_id}",
                                                                            value: "{mp_id}",
                                                                            "{mp_name}"
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }

                                            // Clear button
                                            if assigned_id.is_some() {
                                                button {
                                                    class: "p-1 rounded text-zinc-600 hover:text-red-400 hover:bg-red-500/10 transition-colors flex-shrink-0",
                                                    title: "Clear assignment",
                                                    onclick: move |evt| {
                                                        evt.stop_propagation();
                                                        module_assignments.write().remove(&type_key_for_clear);
                                                        if let Some(pid) = selected_id {
                                                            save_assignments(pid, module_assignments.cloned());
                                                        }
                                                    },
                                                    span { class: "text-[9px]", "\u{2715}" }
                                                }
                                            }

                                            // Enabled toggle
                                            div {
                                                class: if assigned_id.is_some() {
                                                    "w-2 h-2 rounded-full flex-shrink-0"
                                                } else {
                                                    "w-2 h-2 rounded-full bg-zinc-800 flex-shrink-0"
                                                },
                                                style: if assigned_id.is_some() {
                                                    format!("background-color: {};", mc.bg)
                                                } else {
                                                    String::new()
                                                },
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // ── Preset Snapshots (bottom ~45%) ────────────
                    div { class: "flex-1 flex flex-col min-h-0",
                        // Section header
                        div { class: "px-4 py-2 border-b border-border/30 flex items-center justify-between flex-shrink-0",
                            div { class: "flex items-center gap-2",
                                span { class: "text-[10px] font-bold text-zinc-500 uppercase tracking-[0.1em]",
                                    "Snapshots / Scenes"
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
                                    new_snapshot_name.set(format!("Scene {}", snapshots.len() + 1));
                                    show_new_snapshot_dialog.set(true);
                                },
                                span { class: "text-cyan-300", "+" }
                                "New Scene"
                            }
                        }

                        // New snapshot inline dialog
                        if show_new_snapshot_dialog() {
                            div { class: "px-4 py-2 border-b border-border/20 bg-zinc-900/50 flex items-center gap-2 flex-shrink-0",
                                input {
                                    class: "flex-1 bg-zinc-800/80 border border-zinc-700/50 rounded-md px-2.5 py-1.5 text-xs text-zinc-200 \
                                            outline-none focus:border-cyan-500/40 placeholder:text-zinc-600",
                                    r#type: "text",
                                    placeholder: "Scene name...",
                                    value: "{new_snapshot_name}",
                                    autofocus: true,
                                    oninput: move |evt| new_snapshot_name.set(evt.value().clone()),
                                    onkeydown: move |evt| {
                                        if evt.key() == Key::Escape {
                                            show_new_snapshot_dialog.set(false);
                                        } else if evt.key() == Key::Enter {
                                            let snap_name = new_snapshot_name().trim().to_string();
                                            if !snap_name.is_empty() {
                                                show_new_snapshot_dialog.set(false);
                                                if let Some(pid) = selected_id {
                                                    spawn(async move {
                                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                        let snap_data = signal_control::preset::PresetMetadata::new(
                                                            &snap_name,
                                                            signal_control::category::PresetCategory::default(),
                                                        );
                                                        match ctl.save_rig_preset_snapshot(pid, &snap_name, &snap_data).await {
                                                            Ok(id) => {
                                                                debug!("Created preset snapshot: {id}");
                                                                if let Ok(snaps) = ctl.list_rig_preset_snapshots(pid).await {
                                                                    snapshot_counts.write().insert(pid, snaps.len());
                                                                    preset_snapshots.set(snaps);
                                                                }
                                                            }
                                                            Err(e) => warn!("Create snapshot failed: {e}"),
                                                        }
                                                    });
                                                }
                                            }
                                        }
                                    },
                                }
                                button {
                                    class: "px-2.5 py-1.5 rounded-md text-[10px] font-semibold bg-cyan-500/80 text-white hover:bg-cyan-500 transition-colors",
                                    onclick: move |_| {
                                        let snap_name = new_snapshot_name().trim().to_string();
                                        if !snap_name.is_empty() {
                                            show_new_snapshot_dialog.set(false);
                                            if let Some(pid) = selected_id {
                                                spawn(async move {
                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                    let snap_data = signal_control::preset::PresetMetadata::new(
                                                        &snap_name,
                                                        signal_control::category::PresetCategory::default(),
                                                    );
                                                    match ctl.save_rig_preset_snapshot(pid, &snap_name, &snap_data).await {
                                                        Ok(id) => {
                                                            debug!("Created preset snapshot: {id}");
                                                            if let Ok(snaps) = ctl.list_rig_preset_snapshots(pid).await {
                                                                snapshot_counts.write().insert(pid, snaps.len());
                                                                preset_snapshots.set(snaps);
                                                            }
                                                        }
                                                        Err(e) => warn!("Create snapshot failed: {e}"),
                                                    }
                                                });
                                            }
                                        }
                                    },
                                    "Create"
                                }
                                button {
                                    class: "px-2 py-1.5 rounded-md text-[10px] text-zinc-500 hover:text-zinc-300 transition-colors",
                                    onclick: move |_| show_new_snapshot_dialog.set(false),
                                    "Cancel"
                                }
                            }
                        }

                        // Snapshot grid
                        div { class: "flex-1 overflow-y-auto min-h-0 px-3 py-2",
                            if snapshots.is_empty() {
                                div { class: "flex items-center justify-center h-full",
                                    p { class: "text-xs text-zinc-600",
                                        if selected_id.is_some() {
                                            "No scenes yet -- click New Scene to add one"
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
                                            let is_snap_selected = matches!(&current_detail, DetailMode::Snapshot(id) if *id == sid);

                                            // Pre-extract border style
                                            let card_border = if is_snap_selected {
                                                "border-cyan-500/40 bg-cyan-500/5"
                                            } else {
                                                "border-zinc-800/50 hover:border-zinc-700/40 hover:bg-zinc-800/30"
                                            };

                                            rsx! {
                                                div {
                                                    key: "{sid}",
                                                    class: "flex flex-col gap-1.5 px-3 py-2.5 rounded-lg border {card_border} \
                                                           cursor-pointer transition-all duration-100",
                                                    onclick: move |_| {
                                                        detail_mode.set(DetailMode::Snapshot(sid));
                                                    },
                                                    span { class: "text-xs font-medium text-zinc-200 truncate", "{snap_name}" }
                                                    div { class: "flex gap-1",
                                                        button {
                                                            class: "flex-1 px-2 py-1 rounded text-[9px] font-semibold \
                                                                    bg-emerald-500/10 text-emerald-400 border border-emerald-500/20 \
                                                                    hover:bg-emerald-500/20 transition-all",
                                                            onclick: move |evt| {
                                                                evt.stop_propagation();
                                                                preset_editor_status.set(format!("Activated: {}", snap_name));
                                                            },
                                                            "Activate"
                                                        }
                                                        button {
                                                            class: "px-2 py-1 rounded text-[9px] text-red-400/50 hover:text-red-400 \
                                                                    hover:bg-red-500/10 border border-transparent hover:border-red-500/20 transition-all",
                                                            onclick: move |evt| {
                                                                evt.stop_propagation();
                                                                spawn(async move {
                                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                                    if let Ok(true) = ctl.delete_rig_preset_snapshot(sid).await {
                                                                        if let Some(pid) = *preset_selected_id.read() {
                                                                            if let Ok(snaps) = ctl.list_rig_preset_snapshots(pid).await {
                                                                                snapshot_counts.write().insert(pid, snaps.len());
                                                                                preset_snapshots.set(snaps);
                                                                            }
                                                                        }
                                                                        // If we were viewing this snapshot, go back to guide
                                                                        if matches!(*detail_mode.read(), DetailMode::Snapshot(id) if id == sid) {
                                                                            detail_mode.set(DetailMode::Guide);
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

                        // ── Tags section (pinned at bottom) ──────
                        if has_preset_selected {
                            div { class: "px-4 py-2 border-t border-border/20 flex-shrink-0",
                                div { class: "flex items-center gap-2 mb-1.5",
                                    span { class: "text-[9px] font-bold text-zinc-500 uppercase tracking-wider", "Tags" }
                                    input {
                                        class: "flex-1 bg-transparent border-none text-[10px] text-zinc-400 \
                                                outline-none placeholder:text-zinc-700",
                                        r#type: "text",
                                        placeholder: "Add tag...",
                                        value: "{tag_input}",
                                        oninput: move |evt| tag_input.set(evt.value().clone()),
                                        onkeydown: {
                                            let tags_for_add = preset_tags.clone();
                                            move |evt: Event<KeyboardData>| {
                                            if evt.key() == Key::Enter {
                                                let new_tag = tag_input().trim().to_string();
                                                if !new_tag.is_empty() {
                                                    tag_input.set(String::new());
                                                    if let Some(pid) = selected_id {
                                                        // Read current tags, add new one, save
                                                        let mut tags_vec = tags_for_add.clone();
                                                        if !tags_vec.contains(&new_tag) {
                                                            tags_vec.push(new_tag);
                                                        }
                                                        let new_tags_json = serde_json::json!(tags_vec);
                                                        spawn(async move {
                                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                            if let Err(e) = ctl.update_rig_preset_metadata(pid, None, None, None, Some(new_tags_json), None).await {
                                                                warn!("Failed to update tags: {e}");
                                                            } else if let Ok(list) = ctl.list_rig_presets().await {
                                                                preset_list.set(list);
                                                            }
                                                        });
                                                    }
                                                }
                                            }
                                        }},
                                    }
                                }
                                // Tag chips
                                {
                                    let tags = preset_tags.clone();
                                    if !tags.is_empty() {
                                        rsx! {
                                            div { class: "flex flex-wrap gap-1",
                                                for tag in tags.iter() {
                                                    {
                                                        let tag_name = tag.clone();
                                                        let tag_for_remove = tag.clone();
                                                        let all_tags_for_remove = tags.clone();
                                                        rsx! {
                                                            span {
                                                                key: "{tag_name}",
                                                                class: "inline-flex items-center gap-1 text-[9px] px-2 py-0.5 rounded-full \
                                                                        bg-zinc-800/80 text-zinc-400 border border-zinc-700/30",
                                                                "{tag_name}"
                                                                button {
                                                                    class: "text-zinc-600 hover:text-red-400 transition-colors",
                                                                    onclick: move |_| {
                                                                        if let Some(pid) = selected_id {
                                                                            let remove_tag = tag_for_remove.clone();
                                                                            let tags_vec: Vec<String> = all_tags_for_remove.clone()
                                                                                .into_iter()
                                                                                .filter(|t| *t != remove_tag)
                                                                                .collect();
                                                                            let new_tags_json = serde_json::json!(tags_vec);
                                                                            spawn(async move {
                                                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                                                if let Err(e) = ctl.update_rig_preset_metadata(pid, None, None, None, Some(new_tags_json), None).await {
                                                                                    warn!("Failed to remove tag: {e}");
                                                                                } else if let Ok(list) = ctl.list_rig_presets().await {
                                                                                    preset_list.set(list);
                                                                                }
                                                                            });
                                                                        }
                                                                    },
                                                                    "\u{2715}"
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    } else {
                                        rsx! {}
                                    }
                                }
                            }
                        }
                    }
                }
            },
            right: Some(rsx! {
                match &current_detail {
                    // ── Snapshot Detail ───────────────────────────
                    DetailMode::Snapshot(sid) => {
                        let _snap_id = *sid;
                        let snap = selected_snapshot.clone();
                        rsx! {
                            div { class: "px-4 py-3 border-b border-border/30 flex-shrink-0",
                                div { class: "flex items-center justify-between",
                                    h3 { class: "text-[11px] font-bold text-zinc-400 uppercase tracking-[0.15em]",
                                        "Scene Detail"
                                    }
                                    button {
                                        class: "text-[9px] text-zinc-600 hover:text-zinc-400 transition-colors",
                                        onclick: move |_| detail_mode.set(DetailMode::Guide),
                                        "\u{2715} close"
                                    }
                                }
                            }
                            div { class: "flex-1 overflow-y-auto min-h-0 px-4 py-3",
                                if let Some(ref s) = snap {
                                    div { class: "flex flex-col gap-4",
                                        // Snapshot name
                                        div {
                                            span { class: "text-[9px] font-semibold text-zinc-500 uppercase tracking-wider block mb-1",
                                                "Name"
                                            }
                                            span { class: "text-sm font-medium text-zinc-200", "{s.name}" }
                                        }

                                        // Per-module variation assignments
                                        div {
                                            span { class: "text-[9px] font-semibold text-zinc-500 uppercase tracking-wider block mb-2",
                                                "Module Overrides"
                                            }
                                            div { class: "flex flex-col gap-1.5",
                                                for slot_def in signal_chain_order().iter() {
                                                    {
                                                        let mt_name = slot_def.name;
                                                        let mc = module_type_color(slot_def.module_type);
                                                        let icon_style = format!("background-color: {}20; color: {};", mc.bg, mc.fg);
                                                        rsx! {
                                                            div { class: "flex items-center gap-2 px-2 py-1.5 rounded bg-zinc-900/40",
                                                                div {
                                                                    class: "w-4 h-4 rounded flex items-center justify-center text-[7px] font-bold flex-shrink-0",
                                                                    style: "{icon_style}",
                                                                    "{slot_def.icon}"
                                                                }
                                                                span { class: "text-[10px] text-zinc-400 flex-1", "{mt_name}" }
                                                                span { class: "text-[9px] text-zinc-600 italic", "default" }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }

                                        // Block bypass overrides placeholder
                                        div {
                                            span { class: "text-[9px] font-semibold text-zinc-500 uppercase tracking-wider block mb-1",
                                                "Block Overrides"
                                            }
                                            p { class: "text-[10px] text-zinc-600 italic",
                                                "No block overrides configured. Bypass states and parameter tweaks can be added here."
                                            }
                                        }

                                        // Created date
                                        div {
                                            span { class: "text-[9px] font-semibold text-zinc-500 uppercase tracking-wider block mb-1",
                                                "Created"
                                            }
                                            span { class: "text-[10px] text-zinc-500 font-mono",
                                                "{s.created_at}"
                                            }
                                        }
                                    }
                                } else {
                                    p { class: "text-xs text-zinc-600", "Scene not found" }
                                }
                            }
                        }
                    }

                    // ── Module Slot Detail ────────────────────────
                    DetailMode::ModuleSlot(ref type_key) => {
                        let tk = type_key.clone();
                        let assigned_id = assignments.get(&tk).copied();
                        let assigned_model = assigned_id.and_then(|id| {
                            available_modules.iter().find(|m| m.id == id).cloned()
                        });

                        // Parse module type from key
                        let chain_defs = signal_chain_order();
                        let slot_info = chain_defs.iter().find(|s| s.module_type.display_name() == tk.as_str());
                        let mt_name = slot_info.map(|s| s.name).unwrap_or("Module");
                        let mc = slot_info.map(|s| module_type_color(s.module_type));

                        // Pre-extract hex styles
                        let header_bg = mc.as_ref().map_or(
                            String::new(),
                            |c| format!("background-color: {}10;", c.bg),
                        );
                        let dot_style = mc.as_ref().map_or(
                            String::new(),
                            |c| format!("background-color: {};", c.bg),
                        );

                        rsx! {
                            div {
                                class: "px-4 py-3 border-b border-border/30 flex-shrink-0",
                                style: "{header_bg}",
                                div { class: "flex items-center justify-between",
                                    div { class: "flex items-center gap-2",
                                        div {
                                            class: "w-2 h-2 rounded-full flex-shrink-0",
                                            style: "{dot_style}",
                                        }
                                        h3 { class: "text-[11px] font-bold text-zinc-400 uppercase tracking-[0.15em]",
                                            "{mt_name}"
                                        }
                                    }
                                    button {
                                        class: "text-[9px] text-zinc-600 hover:text-zinc-400 transition-colors",
                                        onclick: move |_| detail_mode.set(DetailMode::Guide),
                                        "\u{2715} close"
                                    }
                                }
                            }
                            div { class: "flex-1 overflow-y-auto min-h-0 px-4 py-3",
                                div { class: "flex flex-col gap-4",
                                    if let Some(ref model) = assigned_model {
                                        // Assigned module info
                                        div {
                                            span { class: "text-[9px] font-semibold text-zinc-500 uppercase tracking-wider block mb-1",
                                                "Assigned Preset"
                                            }
                                            span { class: "text-sm font-medium text-zinc-200", "{model.name}" }
                                        }

                                        // Block count
                                        {
                                            let block_count = model.blocks.as_array()
                                                .or_else(|| model.blocks.get("blocks").and_then(|v| v.as_array()))
                                                .map(|a| a.len())
                                                .unwrap_or(0);
                                            let bp = if block_count != 1 { "s" } else { "" };
                                            rsx! {
                                                div {
                                                    span { class: "text-[9px] font-semibold text-zinc-500 uppercase tracking-wider block mb-1",
                                                        "Composition"
                                                    }
                                                    span { class: "text-[10px] text-zinc-400", "{block_count} block{bp}" }
                                                }
                                            }
                                        }

                                        // Available presets for this type
                                        div {
                                            span { class: "text-[9px] font-semibold text-zinc-500 uppercase tracking-wider block mb-2",
                                                "Other Presets"
                                            }
                                            {
                                                let type_modules: Vec<_> = available_modules.iter()
                                                    .filter(|m| m.module_type == tk)
                                                    .collect();
                                                rsx! {
                                                    div { class: "flex flex-col gap-1",
                                                        for mp in type_modules.iter() {
                                                            {
                                                                let mp_id = mp.id;
                                                                let mp_name = mp.name.clone();
                                                                let is_current = assigned_id == Some(mp_id);
                                                                let tk_for_assign = tk.clone();
                                                                rsx! {
                                                                    button {
                                                                        key: "{mp_id}",
                                                                        class: if is_current {
                                                                            "w-full text-left px-2.5 py-1.5 rounded text-[10px] font-medium \
                                                                             bg-zinc-800/80 text-zinc-200 border border-zinc-600/40"
                                                                        } else {
                                                                            "w-full text-left px-2.5 py-1.5 rounded text-[10px] text-zinc-400 \
                                                                             hover:bg-zinc-800/40 hover:text-zinc-200 border border-transparent transition-all"
                                                                        },
                                                                        onclick: move |_| {
                                                                            module_assignments.write().insert(tk_for_assign.clone(), mp_id);
                                                                            if let Some(pid) = selected_id {
                                                                                save_assignments(pid, module_assignments.cloned());
                                                                            }
                                                                        },
                                                                        "{mp_name}"
                                                                        if is_current {
                                                                            span { class: "text-[8px] text-zinc-500 ml-1", "(current)" }
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
                                        div {
                                            span { class: "text-[9px] font-semibold text-zinc-500 uppercase tracking-wider block mb-1",
                                                "Status"
                                            }
                                            p { class: "text-[10px] text-zinc-600 italic mb-3",
                                                "No module preset assigned to this slot."
                                            }
                                            // Quick-assign list
                                            {
                                                let type_modules: Vec<_> = available_modules.iter()
                                                    .filter(|m| m.module_type == tk)
                                                    .collect();
                                                if type_modules.is_empty() {
                                                    rsx! {
                                                        p { class: "text-[10px] text-zinc-600",
                                                            "No module presets of this type exist. Create one in the Design tab."
                                                        }
                                                    }
                                                } else {
                                                    rsx! {
                                                        span { class: "text-[9px] font-semibold text-zinc-500 uppercase tracking-wider block mb-2",
                                                            "Available Presets"
                                                        }
                                                        div { class: "flex flex-col gap-1",
                                                            for mp in type_modules.iter() {
                                                                {
                                                                    let mp_id = mp.id;
                                                                    let mp_name = mp.name.clone();
                                                                    let tk_for_assign = tk.clone();
                                                                    rsx! {
                                                                        button {
                                                                            key: "{mp_id}",
                                                                            class: "w-full text-left px-2.5 py-1.5 rounded text-[10px] text-zinc-400 \
                                                                                    hover:bg-zinc-800/40 hover:text-zinc-200 border border-transparent \
                                                                                    hover:border-zinc-700/30 transition-all",
                                                                            onclick: move |_| {
                                                                                module_assignments.write().insert(tk_for_assign.clone(), mp_id);
                                                                                if let Some(pid) = selected_id {
                                                                                    save_assignments(pid, module_assignments.cloned());
                                                                                }
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

                    // ── Quick Guide (default) ────────────────────
                    DetailMode::Guide => {
                        rsx! {
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
                                            "Each row in the signal chain has a dropdown to pick a module preset. Click a row to see details in this panel."
                                        }
                                    }
                                    div {
                                        p { class: "text-[10px] font-semibold text-zinc-400 mb-1", "3. Create Scenes" }
                                        p { class: "text-[10px] text-zinc-600 leading-relaxed",
                                            "Scenes capture module snapshot assignments for live recall (Verse, Chorus, Solo). Click a scene card to view its details here."
                                        }
                                    }
                                    div {
                                        p { class: "text-[10px] font-semibold text-zinc-400 mb-1", "4. Add Tags" }
                                        p { class: "text-[10px] text-zinc-600 leading-relaxed",
                                            "Tags help organize presets. Type in the tags field below the scene grid and press Enter."
                                        }
                                    }
                                    div {
                                        p { class: "text-[10px] font-semibold text-zinc-400 mb-1", "Workflow" }
                                        p { class: "text-[10px] text-zinc-600 leading-relaxed",
                                            "Blocks \u{2192} Modules \u{2192} Presets \u{2192} Scenes"
                                        }
                                        p { class: "text-[10px] text-zinc-500 mt-1 leading-relaxed",
                                            "Build block presets in the Blocks tab, compose them into modules in the Design tab, then assign modules here."
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }),
            status: rsx! {
                div { class: "w-1.5 h-1.5 rounded-full bg-amber-400/60" }
                span { class: "text-[10px] text-zinc-500 font-mono truncate flex-1", "{status}" }
                if has_preset_selected {
                    span { class: "text-[9px] text-zinc-600",
                        "{assignments.len()} modules assigned"
                    }
                }
            },
        }
    }
}
