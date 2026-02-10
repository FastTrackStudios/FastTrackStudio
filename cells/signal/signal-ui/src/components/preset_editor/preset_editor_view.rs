//! Preset Editor View — compose modules into full rig configurations.
//!
//! Left: Preset browser with search and category filter
//! Center: Module assignment table (signal chain) + preset snapshots
//! Right: Snapshot detail / module library
//!
//! Presets are the top-level entity: they assign module presets to each
//! module type slot in the signal chain, and store named snapshots
//! (Verse, Chorus, Solo) for scene-level recall.

use crate::prelude::*;
use crate::signals::RIG_SERVICE;
use signal_control::module::ModuleType;
use tracing::{debug, warn};
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
// State
// ─────────────────────────────────────────────────────────────────────────────

/// Selected preset in the browser.
static PRESET_SELECTED_ID: GlobalSignal<Option<Uuid>> = Signal::global(|| None);

/// Preset list from DB.
static PRESET_LIST: GlobalSignal<Vec<signal_control::preset_entity::Model>> =
    Signal::global(Vec::new);

/// Snapshots for the selected preset.
static PRESET_SNAPSHOTS: GlobalSignal<Vec<signal_control::snapshot_entity::Model>> =
    Signal::global(Vec::new);

/// Module presets available per type (for the assignment dropdowns).
static AVAILABLE_MODULE_PRESETS: GlobalSignal<Vec<signal_control::module_preset_entity::Model>> =
    Signal::global(Vec::new);

/// Current module assignments: ModuleType display name → assigned module preset ID.
static MODULE_ASSIGNMENTS: GlobalSignal<std::collections::HashMap<String, Uuid>> =
    Signal::global(std::collections::HashMap::new);

static PRESET_EDITOR_STATUS: GlobalSignal<String> =
    Signal::global(|| "Select a preset".to_string());

/// Search/filter text for the preset browser.
static PRESET_SEARCH: GlobalSignal<String> = Signal::global(String::new);

// ─────────────────────────────────────────────────────────────────────────────
// DB helpers
// ─────────────────────────────────────────────────────────────────────────────

async fn refresh_preset_list() {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    match ctl.list_rig_presets().await {
        Ok(presets) => *PRESET_LIST.write() = presets,
        Err(e) => warn!("Failed to load presets: {e}"),
    }
}

async fn refresh_preset_snapshots(preset_id: Uuid) {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    match ctl.list_rig_preset_snapshots(preset_id).await {
        Ok(snaps) => *PRESET_SNAPSHOTS.write() = snaps,
        Err(e) => warn!("Failed to load preset snapshots: {e}"),
    }
}

async fn refresh_available_modules() {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    match ctl.list_module_presets(None).await {
        Ok(mods) => *AVAILABLE_MODULE_PRESETS.write() = mods,
        Err(e) => warn!("Failed to load module presets: {e}"),
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Main Component
// ─────────────────────────────────────────────────────────────────────────────

#[component]
pub fn PresetEditorView() -> Element {
    let selected_id = *PRESET_SELECTED_ID.read();
    // Clone data out of signals so read guards are dropped before event handlers
    // can trigger writes (prevents AlreadyBorrowed panics during re-render).
    let presets = PRESET_LIST.cloned();
    let snapshots = PRESET_SNAPSHOTS.cloned();
    let available_modules = AVAILABLE_MODULE_PRESETS.cloned();
    let assignments = MODULE_ASSIGNMENTS.cloned();
    let status = PRESET_EDITOR_STATUS.cloned();
    let search_text = PRESET_SEARCH.cloned();

    let mut show_new_dialog = use_signal(|| false);
    let mut new_name = use_signal(String::new);
    let mut show_rename_dialog = use_signal(|| false);
    let mut rename_value = use_signal(String::new);

    // Load on mount
    use_future(move || async move {
        refresh_preset_list().await;
        refresh_available_modules().await;
    });

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

    rsx! {
        div { class: "h-full w-full flex flex-col overflow-hidden",
            // Accent strip
            div { class: "h-[2px] w-full bg-gradient-to-r from-amber-500 via-rose-400 to-violet-500 flex-shrink-0" }

            div { class: "flex-1 flex min-h-0 overflow-hidden",

                // ══════════════════════════════════════════════════
                // LEFT: Preset Browser
                // ══════════════════════════════════════════════════
                div { class: "w-60 flex-shrink-0 border-r border-border/50 flex flex-col min-h-0 bg-zinc-950/50",
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
                            oninput: move |evt| *PRESET_SEARCH.write() = evt.value().clone(),
                        }
                    }

                    // New preset dialog
                    if show_new_dialog() {
                        div { class: "px-3 py-2 border-b border-border/30 bg-zinc-900/60 flex flex-col gap-2 flex-shrink-0",
                            input {
                                class: "w-full bg-zinc-800/80 border border-zinc-700/50 rounded-md px-2.5 py-1.5 text-xs text-zinc-200 \
                                        outline-none focus:border-amber-500/40 placeholder:text-zinc-600",
                                r#type: "text",
                                placeholder: "Preset name...",
                                value: "{new_name}",
                                autofocus: true,
                                oninput: move |evt| new_name.set(evt.value().clone()),
                                onkeydown: move |evt| {
                                    if evt.key() == Key::Enter {
                                        let val = new_name().trim().to_string();
                                        if !val.is_empty() {
                                            show_new_dialog.set(false);
                                            spawn(async move {
                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
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
                                                        debug!("Created rig preset: {id}");
                                                        *PRESET_EDITOR_STATUS.write() = format!("Created '{}'", val);
                                                        refresh_preset_list().await;
                                                        *PRESET_SELECTED_ID.write() = Some(id);
                                                        PRESET_SNAPSHOTS.write().clear();
                                                        MODULE_ASSIGNMENTS.write().clear();
                                                    }
                                                    Err(e) => {
                                                        warn!("Create preset failed: {e}");
                                                        *PRESET_EDITOR_STATUS.write() = format!("Failed: {e}");
                                                    }
                                                }
                                            });
                                        }
                                    } else if evt.key() == Key::Escape {
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
                                        if !val.is_empty() {
                                            show_new_dialog.set(false);
                                            spawn(async move {
                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
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
                                                        refresh_preset_list().await;
                                                        *PRESET_SELECTED_ID.write() = Some(id);
                                                    }
                                                    Err(e) => warn!("Create failed: {e}"),
                                                }
                                            });
                                        }
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
                                                *PRESET_SELECTED_ID.write() = Some(pid);
                                                MODULE_ASSIGNMENTS.write().clear();
                                                spawn(async move {
                                                    refresh_preset_snapshots(pid).await;
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
                                                            rename_value.set(PRESET_LIST.read().iter().find(|p| p.id == pid).map(|p| p.name.clone()).unwrap_or_default());
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
                                                                    *PRESET_EDITOR_STATUS.write() = "Preset deleted".into();
                                                                    if selected_id == Some(pid) {
                                                                        *PRESET_SELECTED_ID.write() = None;
                                                                        PRESET_SNAPSHOTS.write().clear();
                                                                    }
                                                                    refresh_preset_list().await;
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

                // ══════════════════════════════════════════════════
                // CENTER: Preset Configuration
                // ══════════════════════════════════════════════════
                div { class: "flex-1 flex flex-col min-h-0 min-w-0 overflow-hidden",
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
                                                        } else {
                                                            refresh_preset_list().await;
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
                                                refresh_preset_list().await;
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

                        // ── Module Assignment Table (top 55%) ────
                        div { class: "h-[55%] flex flex-col min-h-0 border-b border-border/20 flex-shrink-0",
                            // Header
                            div { class: "px-4 py-2.5 border-b border-border/30 flex items-center gap-3 flex-shrink-0 bg-zinc-900/30",
                                if let Some(ref p) = selected_preset {
                                    span { class: "text-xs font-bold text-zinc-200", "{p.name}" }
                                }
                                span { class: "text-[10px] font-bold text-zinc-500 uppercase tracking-[0.1em]",
                                    "Signal Chain"
                                }
                                div { class: "flex-1" }
                                span { class: "text-[9px] text-zinc-600", "top \u{2192} bottom = signal flow" }
                            }

                            // Chain rows
                            div { class: "flex-1 overflow-y-auto min-h-0 px-3 py-2",
                                for (idx, slot_def) in signal_chain_order().iter().enumerate() {
                                    {
                                        let mt = slot_def.module_type;
                                        let mt_name = slot_def.name;
                                        let slot_color = slot_def.color;
                                        let icon = slot_def.icon;
                                        let assigned_id = assignments.get(mt.display_name()).copied();
                                        let assigned_name = assigned_id.and_then(|id| {
                                            available_modules.iter().find(|m| m.id == id).map(|m| m.name.clone())
                                        });
                                        // Module presets available for this type
                                        let type_modules: Vec<_> = available_modules.iter()
                                            .filter(|m| m.module_type == mt.display_name())
                                            .cloned()
                                            .collect();

                                        rsx! {
                                            // Connection line
                                            if idx > 0 {
                                                div { class: "flex justify-center",
                                                    div { class: "w-px h-2 bg-zinc-800" }
                                                }
                                            }

                                            div {
                                                class: "flex items-center gap-3 px-3 py-2 rounded-lg border border-zinc-800/50 \
                                                        hover:border-zinc-700/60 transition-all duration-100 mb-0.5",
                                                // Module type icon
                                                div {
                                                    class: "w-7 h-7 rounded-md flex items-center justify-center text-[10px] font-bold flex-shrink-0",
                                                    style: "background-color: {slot_color}20; color: {slot_color}; border: 1px solid {slot_color}30;",
                                                    "{icon}"
                                                }
                                                // Module type name
                                                div { class: "w-20 flex-shrink-0",
                                                    span { class: "text-[11px] font-semibold text-zinc-300", "{mt_name}" }
                                                }
                                                // Assignment indicator / dropdown area
                                                div { class: "flex-1 min-w-0",
                                                    if let Some(ref name) = assigned_name {
                                                        div { class: "flex items-center gap-2",
                                                            span { class: "text-[11px] text-zinc-200 font-medium truncate", "{name}" }
                                                            button {
                                                                class: "text-[8px] text-zinc-600 hover:text-red-400 transition-colors flex-shrink-0",
                                                                onclick: move |_| {
                                                                    MODULE_ASSIGNMENTS.write().remove(mt.display_name());
                                                                },
                                                                "clear"
                                                            }
                                                        }
                                                    } else if type_modules.is_empty() {
                                                        span { class: "text-[10px] text-zinc-600 italic", "No module presets" }
                                                    } else {
                                                        // Simple inline picker — show available modules
                                                        div { class: "flex items-center gap-1 flex-wrap",
                                                            for mp in type_modules.iter() {
                                                                {
                                                                    let mp_id = mp.id;
                                                                    let mp_name = mp.name.clone();
                                                                    let display_name_key = mt.display_name().to_string();
                                                                    rsx! {
                                                                        button {
                                                                            key: "{mp_id}",
                                                                            class: "px-2 py-0.5 rounded text-[9px] font-medium \
                                                                                    bg-zinc-800/60 text-zinc-400 border border-zinc-700/40 \
                                                                                    hover:bg-zinc-700/50 hover:text-zinc-200 transition-all",
                                                                            onclick: move |_| {
                                                                                MODULE_ASSIGNMENTS.write().insert(display_name_key.clone(), mp_id);
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

                        // ── Preset Snapshots (bottom 45%) ────────
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
                                                        refresh_preset_snapshots(pid).await;
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
                                                                            if let Some(pid) = *PRESET_SELECTED_ID.read() {
                                                                                refresh_preset_snapshots(pid).await;
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
                // RIGHT: Module Library / Help
                // ══════════════════════════════════════════════════
                div { class: "w-56 flex-shrink-0 border-l border-border/50 flex flex-col min-h-0 bg-zinc-950/40",
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
                                    "Each row in the signal chain represents a module type. Click a module preset button to assign it."
                                }
                            }
                            div {
                                p { class: "text-[10px] font-semibold text-zinc-400 mb-1", "3. Create Scenes" }
                                p { class: "text-[10px] text-zinc-600 leading-relaxed",
                                    "Scenes capture which module snapshot to use per slot. Use them for Verse, Chorus, Solo, etc."
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
                }
            }

            // Status bar
            div { class: "px-4 py-1.5 border-t border-border/30 flex items-center gap-3 flex-shrink-0 bg-zinc-950/60",
                div { class: "w-1.5 h-1.5 rounded-full bg-amber-400/60" }
                span { class: "text-[10px] text-zinc-500 font-mono truncate flex-1", "{status}" }
            }
        }
    }
}
