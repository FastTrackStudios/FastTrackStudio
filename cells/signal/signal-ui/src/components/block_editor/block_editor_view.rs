//! Block Editor View — three-panel layout for editing individual DSP blocks.
//!
//! Left: Block type browser with category grouping, icons, and DB preset counts
//! Center: Preset manager with DB-backed CRUD + snapshot grid with diff heatmap
//! Right: FX chain tree for capture source selection
//!
//! All data flows through `SignalControl` → SQLite. The in-memory `BLOCK_LIBRARY`
//! is retained as a write-back cache for DAW capture operations.

use super::capture;
use super::library::*;
use crate::prelude::*;
use crate::signals::RIG_SERVICE;
use signal_control::daw_bridge::{self, DawParamChange};
use tracing::{debug, warn};
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// Helper: Facet ↔ serde_json bridge
// ─────────────────────────────────────────────────────────────────────────────

/// Convert a Facet type to a `serde_json::Value` for DB storage.
fn facet_to_json_value<T: for<'a> facet::Facet<'a>>(value: &T) -> serde_json::Value {
    let json_str = facet_json::to_string(value).unwrap_or_else(|_| "{}".to_string());
    serde_json::from_str(&json_str).unwrap_or_default()
}

// ─────────────────────────────────────────────────────────────────────────────
// Helper: get FxChain from REAPER's selected track
// ─────────────────────────────────────────────────────────────────────────────

async fn get_current_fx_chain() -> Option<daw_control::FxChain> {
    let daw = daw_control::Daw::try_get()?;
    let project = daw.current_project().await.ok()?;
    let sel = project.tracks().selected().await.ok()?;
    let track = sel.into_iter().next()?;
    Some(track.fx_chain())
}

// ─────────────────────────────────────────────────────────────────────────────
// DB helpers
// ─────────────────────────────────────────────────────────────────────────────

/// Load preset counts per block type from DB into the global signal.
async fn refresh_type_counts() {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    let Ok(all) = ctl.list_block_presets(None).await else {
        return;
    };
    let mut counts = std::collections::HashMap::new();
    for p in &all {
        *counts.entry(p.block_type.clone()).or_insert(0usize) += 1;
    }
    *DB_BLOCK_TYPE_COUNTS.write() = counts;
}

/// Load presets for a specific block type from DB.
async fn refresh_presets_for_type(block_type: &str) {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    match ctl.list_block_presets(Some(block_type)).await {
        Ok(presets) => *DB_BLOCK_PRESETS.write() = presets,
        Err(e) => warn!("Failed to load block presets: {e}"),
    }
}

/// Load snapshots for a specific preset from DB.
async fn refresh_snapshots_for_preset(preset_id: Uuid) {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    match ctl.list_block_snapshots(preset_id).await {
        Ok(snaps) => *DB_BLOCK_SNAPSHOTS.write() = snaps,
        Err(e) => warn!("Failed to load block snapshots: {e}"),
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Main Component
// ─────────────────────────────────────────────────────────────────────────────

#[component]
pub fn BlockEditorView() -> Element {
    let block_types = use_signal(predefined_block_types);
    let selected_type = *SELECTED_BLOCK_TYPE.read();
    let selected_preset_id = *SELECTED_BLOCK_PRESET.read();
    let status = BLOCK_EDITOR_STATUS.read().clone();

    // Clone data out of signals so read guards are dropped before event handlers
    // can trigger writes (prevents AlreadyBorrowed panics during re-render).
    let db_presets = DB_BLOCK_PRESETS.cloned();
    let db_snapshots = DB_BLOCK_SNAPSHOTS.cloned();
    let type_counts = DB_BLOCK_TYPE_COUNTS.cloned();

    // In-memory fallback for capture operations
    let library = BLOCK_LIBRARY.cloned();

    // Dialog state
    let mut show_capture_preset_dialog = use_signal(|| false);
    let mut capture_preset_name = use_signal(String::new);
    let mut show_capture_snapshot_dialog = use_signal(|| false);
    let mut capture_snapshot_name = use_signal(String::new);
    let mut show_rename_dialog = use_signal(|| false);
    let mut rename_value = use_signal(String::new);
    let mut rename_target_id = use_signal(|| None::<Uuid>);

    // DAW connection state
    let mut daw_connected = use_signal(|| false);
    let mut daw_fx_list = use_signal(Vec::<daw_control::Fx>::new);

    // Load type counts on mount
    use_future(move || async move {
        refresh_type_counts().await;
    });

    // Poll DAW for FX list
    use_future(move || async move {
        loop {
            if daw_control::Daw::try_get().is_some() {
                daw_connected.set(true);
                if let Some(chain) = get_current_fx_chain().await {
                    if let Ok(fx_list) = chain.all().await {
                        daw_fx_list.set(fx_list);
                    }
                }
            }
            tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
        }
    });

    let is_connected = *daw_connected.read();
    let preset_count = db_presets.len();
    let snapshot_count = db_snapshots.len();
    let preset_plural = if preset_count != 1 { "s" } else { "" };

    // Selected preset details (used by sub-components via signal reads)
    let _selected_db_preset =
        selected_preset_id.and_then(|id| db_presets.iter().find(|p| p.id == id).cloned());

    rsx! {
        div { class: "h-full w-full flex flex-col overflow-hidden",
            // ── Accent strip at top ──────────────────────────────
            div { class: "h-[2px] w-full bg-gradient-to-r from-orange-500 via-amber-400 to-cyan-500 flex-shrink-0" }

            // ── Main content ─────────────────────────────────────
            div { class: "flex-1 flex min-h-0 overflow-hidden",

                // ══════════════════════════════════════════════════
                // LEFT: Block Type Browser
                // ══════════════════════════════════════════════════
                div { class: "w-56 flex-shrink-0 border-r border-border/50 flex flex-col min-h-0 bg-zinc-950/50",
                    // Header
                    div { class: "px-4 py-3 border-b border-border/30 flex-shrink-0",
                        h2 { class: "text-[11px] font-bold text-zinc-400 uppercase tracking-[0.15em]",
                            "Block Types"
                        }
                    }

                    // Scrollable type list grouped by category
                    div { class: "flex-1 overflow-y-auto min-h-0 px-2 py-2",
                        for category in block_type_categories() {
                            {
                                let cat_types: Vec<_> = block_types.read().iter()
                                    .filter(|d| d.category == category)
                                    .cloned()
                                    .collect();
                                if cat_types.is_empty() {
                                    return rsx! {};
                                }
                                rsx! {
                                    div { class: "mb-3",
                                        // Category label
                                        div { class: "px-2 py-1",
                                            span { class: "text-[9px] font-semibold text-zinc-600 uppercase tracking-[0.2em]",
                                                "{category}"
                                            }
                                        }
                                        // Types in this category
                                        for def in cat_types.iter() {
                                            {
                                                let bt = def.block_type;
                                                let display_name = def.display_name;
                                                let color = def.color;
                                                let _description = def.description;
                                                let is_active = selected_type == Some(bt);
                                                let type_key = bt.display_name().to_string();
                                                let count = type_counts.get(&type_key).copied().unwrap_or(0);
                                                // Also count in-memory presets
                                                let mem_count = library.iter().filter(|p| p.block_type == bt).count();
                                                let total_count = count + mem_count;

                                                rsx! {
                                                    button {
                                                        key: "{display_name}",
                                                        class: if is_active {
                                                            "w-full flex items-center gap-3 px-3 py-2 rounded-lg text-left transition-all duration-150 \
                                                             bg-zinc-800/80 border border-zinc-600/50 shadow-sm shadow-black/20"
                                                        } else {
                                                            "w-full flex items-center gap-3 px-3 py-2 rounded-lg text-left transition-all duration-150 \
                                                             hover:bg-zinc-800/40 border border-transparent"
                                                        },
                                                        onclick: move |_| {
                                                            *SELECTED_BLOCK_TYPE.write() = Some(bt);
                                                            *SELECTED_BLOCK_PRESET.write() = None;
                                                            *SELECTED_BLOCK_SNAPSHOT.write() = None;
                                                            DB_BLOCK_SNAPSHOTS.write().clear();
                                                            let type_name = bt.display_name().to_string();
                                                            *BLOCK_EDITOR_STATUS.write() = format!("Selected: {}", display_name);
                                                            spawn(async move {
                                                                refresh_presets_for_type(&type_name).await;
                                                            });
                                                        },
                                                        // Color indicator bar
                                                        {
                                                            let opacity = if is_active { "1.0" } else { "0.4" };
                                                            rsx! {
                                                                div {
                                                                    class: "w-1 h-6 rounded-full flex-shrink-0 {color}",
                                                                    style: "background-color: currentColor; opacity: {opacity};",
                                                                }
                                                            }
                                                        }
                                                        // Name + count
                                                        div { class: "flex-1 min-w-0 flex items-center justify-between",
                                                            span {
                                                                class: if is_active {
                                                                    "text-xs font-semibold text-zinc-100 truncate"
                                                                } else {
                                                                    "text-xs font-medium text-zinc-400 truncate"
                                                                },
                                                                "{display_name}"
                                                            }
                                                            if total_count > 0 {
                                                                span {
                                                                    class: if is_active {
                                                                        "text-[9px] font-mono text-zinc-300 bg-zinc-700/60 px-1.5 py-0.5 rounded flex-shrink-0"
                                                                    } else {
                                                                        "text-[9px] font-mono text-zinc-500 bg-zinc-800/40 px-1.5 py-0.5 rounded flex-shrink-0"
                                                                    },
                                                                    "{total_count}"
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

                // ══════════════════════════════════════════════════
                // CENTER: Preset Manager + Snapshots
                // ══════════════════════════════════════════════════
                div { class: "flex-1 flex flex-col min-h-0 min-w-0 overflow-hidden",

                    if selected_type.is_none() {
                        // Empty state
                        div { class: "flex-1 flex items-center justify-center",
                            div { class: "text-center max-w-xs",
                                div { class: "w-12 h-12 rounded-xl bg-zinc-800/60 border border-zinc-700/40 flex items-center justify-center mx-auto mb-4",
                                    span { class: "text-xl text-zinc-600", "\u{25A6}" }
                                }
                                p { class: "text-sm font-medium text-zinc-400 mb-1",
                                    "Select a Block Type"
                                }
                                p { class: "text-xs text-zinc-600 leading-relaxed",
                                    "Choose a block type from the left panel to manage its presets and parameter snapshots"
                                }
                            }
                        }
                    } else {
                        // ── Toolbar ──────────────────────────────
                        div { class: "px-4 py-2.5 border-b border-border/30 flex items-center gap-3 flex-shrink-0 bg-zinc-900/30",
                            // Active type indicator
                            if let Some(bt) = selected_type {
                                {
                                    let color_info = crate::components::rig_grid::block_colors::block_type_color(bt);
                                    rsx! {
                                        div {
                                            class: "flex items-center gap-2",
                                            div {
                                                class: "w-2.5 h-2.5 rounded-full",
                                                style: "background-color: {color_info.bg};",
                                            }
                                            span { class: "text-xs font-bold text-zinc-200 tracking-wide",
                                                "{bt.display_name()}"
                                            }
                                        }
                                    }
                                }
                            }
                            span { class: "text-[10px] text-zinc-600 font-mono",
                                "{preset_count} preset{preset_plural}"
                            }

                            div { class: "flex-1" }

                            // Capture preset button
                            button {
                                class: "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-[10px] font-semibold \
                                        bg-orange-500/15 text-orange-300 border border-orange-500/25 \
                                        hover:bg-orange-500/25 hover:border-orange-500/40 transition-all duration-150 \
                                        disabled:opacity-25 disabled:cursor-not-allowed",
                                disabled: !is_connected,
                                title: "Capture current FX state as a new block preset",
                                onclick: move |_| {
                                    capture_preset_name.set(String::new());
                                    show_capture_preset_dialog.set(true);
                                },
                                span { class: "text-orange-400", "+" }
                                "Capture Preset"
                            }
                        }

                        // Capture preset dialog
                        if show_capture_preset_dialog() {
                            CaptureDialog {
                                label: "Preset Name",
                                placeholder: "e.g., ProQ 3 Clean, NAM Marshall...",
                                value: capture_preset_name(),
                                on_input: move |v: String| capture_preset_name.set(v),
                                on_submit: move |name: String| {
                                    show_capture_preset_dialog.set(false);
                                    let bt = selected_type.unwrap();
                                    spawn(async move {
                                        handle_capture_preset(bt, name).await;
                                    });
                                },
                                on_cancel: move |_| show_capture_preset_dialog.set(false),
                            }
                        }

                        // ── Preset List (top — fixed height proportion) ─
                        div { class: "h-[45%] flex flex-col min-h-0 border-b border-border/20 flex-shrink-0",
                            div { class: "flex-1 overflow-y-auto min-h-0",
                                if db_presets.is_empty() && library.iter().filter(|p| selected_type.map_or(false, |st| p.block_type == st)).count() == 0 {
                                    div { class: "flex items-center justify-center h-full px-6",
                                        div { class: "text-center",
                                            p { class: "text-xs text-zinc-500 mb-1",
                                                "No presets yet"
                                            }
                                            p { class: "text-[10px] text-zinc-600",
                                                if is_connected {
                                                    "Set up a plugin in REAPER, select it in the right panel, then click Capture Preset"
                                                } else {
                                                    "Connect to REAPER to capture FX presets"
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    div { class: "px-2 py-1.5",
                                        // DB presets
                                        for preset in db_presets.iter() {
                                            {
                                                let pid = preset.id;
                                                let is_selected = selected_preset_id == Some(pid);
                                                let pname = preset.name.clone();
                                                let plugin_name = preset.plugin_preset_name.clone()
                                                    .or_else(|| {
                                                        preset.plugin_id.as_ref().and_then(|v| v.get("name").and_then(|n| n.as_str()).map(String::from))
                                                    });

                                                rsx! {
                                                    PresetRow {
                                                        key: "{pid}",
                                                        id: pid,
                                                        name: pname,
                                                        plugin_name: plugin_name,
                                                        is_selected: is_selected,
                                                        source: "db",
                                                        on_select: move |_| {
                                                            *SELECTED_BLOCK_PRESET.write() = Some(pid);
                                                            *SELECTED_BLOCK_SNAPSHOT.write() = None;
                                                            spawn(async move {
                                                                refresh_snapshots_for_preset(pid).await;
                                                            });
                                                        },
                                                        on_rename: move |_| {
                                                            rename_target_id.set(Some(pid));
                                                            rename_value.set(DB_BLOCK_PRESETS.read().iter().find(|p| p.id == pid).map(|p| p.name.clone()).unwrap_or_default());
                                                            show_rename_dialog.set(true);
                                                        },
                                                        on_delete: move |_| {
                                                            spawn(async move {
                                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                                                if let Err(e) = ctl.delete_block_preset(pid).await {
                                                                    warn!("Delete preset failed: {e}");
                                                                    *BLOCK_EDITOR_STATUS.write() = format!("Delete failed: {e}");
                                                                } else {
                                                                    *BLOCK_EDITOR_STATUS.write() = "Preset deleted".into();
                                                                    if selected_preset_id == Some(pid) {
                                                                        *SELECTED_BLOCK_PRESET.write() = None;
                                                                        DB_BLOCK_SNAPSHOTS.write().clear();
                                                                    }
                                                                    if let Some(bt) = selected_type {
                                                                        refresh_presets_for_type(bt.display_name()).await;
                                                                        refresh_type_counts().await;
                                                                    }
                                                                }
                                                            });
                                                        },
                                                    }
                                                }
                                            }
                                        }
                                        // In-memory presets (captured but not yet in DB)
                                        for preset in library.iter().filter(|p| selected_type.map_or(false, |st| p.block_type == st)) {
                                            {
                                                let pid = preset.id;
                                                let is_selected = selected_preset_id == Some(pid);
                                                rsx! {
                                                    PresetRow {
                                                        key: "{pid}",
                                                        id: pid,
                                                        name: preset.name.clone(),
                                                        plugin_name: preset.plugin_name.clone(),
                                                        is_selected: is_selected,
                                                        source: "mem",
                                                        on_select: move |_| {
                                                            *SELECTED_BLOCK_PRESET.write() = Some(pid);
                                                            *SELECTED_BLOCK_SNAPSHOT.write() = None;
                                                        },
                                                        on_rename: move |_| {},
                                                        on_delete: move |_| {},
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // Rename dialog (inline)
                        if show_rename_dialog() {
                            CaptureDialog {
                                label: "Rename",
                                placeholder: "New name...",
                                value: rename_value(),
                                on_input: move |v: String| rename_value.set(v),
                                on_submit: move |new_name: String| {
                                    show_rename_dialog.set(false);
                                    if let Some(target_id) = *rename_target_id.read() {
                                        spawn(async move {
                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return; };
                                            if let Err(e) = ctl.update_block_preset(target_id, Some(&new_name), None, None, None, None).await {
                                                warn!("Rename failed: {e}");
                                            } else {
                                                *BLOCK_EDITOR_STATUS.write() = format!("Renamed to '{}'", new_name);
                                                if let Some(bt) = selected_type {
                                                    refresh_presets_for_type(bt.display_name()).await;
                                                }
                                            }
                                        });
                                    }
                                },
                                on_cancel: move |_| show_rename_dialog.set(false),
                            }
                        }

                        // ── Snapshot Grid (bottom) ───────────────
                        div { class: "flex-1 flex flex-col min-h-0",
                            // Snapshot header
                            div { class: "px-4 py-2 border-b border-border/30 flex items-center justify-between flex-shrink-0",
                                div { class: "flex items-center gap-2",
                                    span { class: "text-[10px] font-bold text-zinc-500 uppercase tracking-[0.1em]",
                                        "Snapshots"
                                    }
                                    if snapshot_count > 0 {
                                        span { class: "text-[9px] font-mono text-zinc-600",
                                            "{snapshot_count}"
                                        }
                                    }
                                }
                                button {
                                    class: "flex items-center gap-1.5 px-2.5 py-1 rounded-md text-[10px] font-semibold \
                                            bg-cyan-500/10 text-cyan-400 border border-cyan-500/20 \
                                            hover:bg-cyan-500/20 hover:border-cyan-500/30 transition-all duration-150 \
                                            disabled:opacity-25 disabled:cursor-not-allowed",
                                    disabled: !is_connected || selected_preset_id.is_none(),
                                    title: "Capture current parameter values as a new snapshot",
                                    onclick: move |_| {
                                        capture_snapshot_name.set(format!("Snap {}", snapshot_count + 1));
                                        show_capture_snapshot_dialog.set(true);
                                    },
                                    span { class: "text-cyan-300", "+" }
                                    "Capture Snapshot"
                                }
                            }

                            // Capture snapshot dialog
                            if show_capture_snapshot_dialog() {
                                CaptureDialog {
                                    label: "Snapshot Name",
                                    placeholder: "e.g., Verse, Chorus, Lead...",
                                    value: capture_snapshot_name(),
                                    on_input: move |v: String| capture_snapshot_name.set(v),
                                    on_submit: move |name: String| {
                                        show_capture_snapshot_dialog.set(false);
                                        let Some(preset_id) = selected_preset_id else { return; };
                                        spawn(async move {
                                            handle_capture_snapshot(preset_id, name).await;
                                        });
                                    },
                                    on_cancel: move |_| show_capture_snapshot_dialog.set(false),
                                }
                            }

                            // Snapshot grid
                            div { class: "flex-1 overflow-y-auto min-h-0 px-3 py-2",
                                if selected_preset_id.is_none() {
                                    div { class: "flex items-center justify-center h-full",
                                        p { class: "text-xs text-zinc-600",
                                            "Select a preset to view its snapshots"
                                        }
                                    }
                                } else if db_snapshots.is_empty() {
                                    // Also check in-memory
                                    {
                                        let mem_preset = selected_preset_id.and_then(|id| library.iter().find(|p| p.id == id));
                                        if let Some(preset) = mem_preset {
                                            rsx! {
                                                InMemorySnapshotGrid { preset: preset.clone(), is_connected: is_connected }
                                            }
                                        } else {
                                            rsx! {
                                                div { class: "flex items-center justify-center h-full",
                                                    p { class: "text-xs text-zinc-600",
                                                        "No snapshots — capture one with the button above"
                                                    }
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    div { class: "grid grid-cols-2 gap-2",
                                        for (idx, snap) in db_snapshots.iter().enumerate() {
                                            {
                                                let sid = snap.id;
                                                let is_snap_selected = *SELECTED_BLOCK_SNAPSHOT.read() == Some(sid);
                                                rsx! {
                                                    DbSnapshotCard {
                                                        key: "{sid}",
                                                        snapshot: snap.clone(),
                                                        index: idx,
                                                        is_selected: is_snap_selected,
                                                        is_connected: is_connected,
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
                // RIGHT: FX Selector
                // ══════════════════════════════════════════════════
                div { class: "w-72 flex-shrink-0 border-l border-border/50 flex flex-col min-h-0",
                    FxSelector { fx_list: daw_fx_list }
                }
            }

            // ── Bottom: Status bar ───────────────────────────────
            div { class: "px-4 py-1.5 border-t border-border/30 flex items-center gap-3 flex-shrink-0 bg-zinc-950/60",
                // Connection indicator
                div {
                    class: if is_connected {
                        "w-1.5 h-1.5 rounded-full bg-emerald-400 shadow-sm shadow-emerald-400/50"
                    } else {
                        "w-1.5 h-1.5 rounded-full bg-zinc-600"
                    },
                }
                span { class: "text-[10px] text-zinc-500 font-mono truncate flex-1",
                    "{status}"
                }
                if !is_connected {
                    span { class: "text-[9px] text-amber-500/70 font-medium",
                        "DAW offline"
                    }
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Preset Row Component
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct PresetRowProps {
    id: Uuid,
    name: String,
    plugin_name: Option<String>,
    is_selected: bool,
    /// "db" or "mem" — determines if rename/delete are available
    source: String,
    on_select: EventHandler<()>,
    on_rename: EventHandler<()>,
    on_delete: EventHandler<()>,
}

#[component]
fn PresetRow(props: PresetRowProps) -> Element {
    let is_db = props.source == "db";

    rsx! {
        div {
            class: if props.is_selected {
                "flex items-center gap-2 px-3 py-2 rounded-lg transition-all duration-100 cursor-pointer \
                 bg-zinc-800/70 border border-zinc-600/40"
            } else {
                "flex items-center gap-2 px-3 py-2 rounded-lg transition-all duration-100 cursor-pointer \
                 hover:bg-zinc-800/30 border border-transparent"
            },
            onclick: move |_| props.on_select.call(()),
            // Preset indicator dot
            div {
                class: if props.is_selected {
                    "w-1.5 h-1.5 rounded-full bg-orange-400 flex-shrink-0"
                } else {
                    "w-1.5 h-1.5 rounded-full bg-zinc-700 flex-shrink-0"
                },
            }
            // Name and plugin
            div { class: "flex-1 min-w-0",
                div { class: "flex items-center gap-2",
                    span { class: "text-xs font-medium text-zinc-200 truncate",
                        "{props.name}"
                    }
                    if !is_db {
                        span { class: "text-[8px] text-amber-500/60 bg-amber-500/10 px-1 rounded flex-shrink-0",
                            "unsaved"
                        }
                    }
                }
                if let Some(ref plugin) = props.plugin_name {
                    p { class: "text-[10px] text-zinc-500 truncate mt-0.5 font-mono",
                        "{plugin}"
                    }
                }
            }
            // Actions (only for DB presets)
            if is_db && props.is_selected {
                div { class: "flex items-center gap-0.5 flex-shrink-0",
                    button {
                        class: "p-1 rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-700/50 transition-colors",
                        title: "Rename",
                        onclick: move |evt| {
                            evt.stop_propagation();
                            props.on_rename.call(());
                        },
                        span { class: "text-[10px]", "\u{270E}" }
                    }
                    button {
                        class: "p-1 rounded text-zinc-500 hover:text-red-400 hover:bg-red-500/10 transition-colors",
                        title: "Delete",
                        onclick: move |evt| {
                            evt.stop_propagation();
                            props.on_delete.call(());
                        },
                        span { class: "text-[10px]", "\u{2715}" }
                    }
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// DB Snapshot Card
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct DbSnapshotCardProps {
    snapshot: signal_control::block_snapshot::Model,
    index: usize,
    is_selected: bool,
    is_connected: bool,
}

#[component]
fn DbSnapshotCard(props: DbSnapshotCardProps) -> Element {
    let sid = props.snapshot.id;
    let snap_name = props.snapshot.name.clone();
    let is_default = props.snapshot.is_default;

    // Count parameters stored in the JSON
    let param_count: usize = props
        .snapshot
        .parameters
        .as_object()
        .and_then(|obj| obj.get("fx_states"))
        .and_then(|v| v.as_array())
        .map(|states| {
            states
                .iter()
                .map(|s| {
                    s.get("parameters")
                        .and_then(|p| p.as_array())
                        .map(|a| a.len())
                        .unwrap_or(0)
                })
                .sum()
        })
        .unwrap_or(0);

    rsx! {
        div {
            class: if props.is_selected {
                "flex flex-col gap-1.5 px-3 py-2.5 rounded-lg border transition-all duration-100 cursor-pointer \
                 bg-cyan-950/30 border-cyan-500/30"
            } else {
                "flex flex-col gap-1.5 px-3 py-2.5 rounded-lg border transition-all duration-100 cursor-pointer \
                 hover:bg-zinc-800/30 border-zinc-800/50"
            },
            onclick: move |_| {
                *SELECTED_BLOCK_SNAPSHOT.write() = Some(sid);
            },
            // Name row
            div { class: "flex items-center justify-between",
                div { class: "flex items-center gap-1.5",
                    span { class: "text-xs font-medium text-zinc-200 truncate",
                        "{snap_name}"
                    }
                    if is_default {
                        span { class: "text-[8px] text-emerald-400/70 bg-emerald-500/10 px-1 rounded",
                            "default"
                        }
                    }
                    if props.index == 0 {
                        span { class: "text-[8px] text-zinc-500 italic",
                            "baseline"
                        }
                    }
                }
                // Live indicator
                div {
                    class: if param_count > 0 {
                        "w-2 h-2 rounded-full bg-emerald-400/80"
                    } else {
                        "w-2 h-2 rounded-full bg-zinc-700"
                    },
                }
            }
            // Param summary
            if param_count > 0 {
                span { class: "text-[9px] text-zinc-500 font-mono",
                    "{param_count} params"
                }
            }
            // Recall button
            button {
                class: "w-full mt-0.5 px-2 py-1 rounded text-[9px] font-semibold \
                        bg-cyan-500/10 text-cyan-400 border border-cyan-500/20 \
                        hover:bg-cyan-500/20 transition-all duration-100 \
                        disabled:opacity-25 disabled:cursor-not-allowed",
                disabled: param_count == 0 || !props.is_connected,
                onclick: move |evt| {
                    evt.stop_propagation();
                    let snap_params = DB_BLOCK_SNAPSHOTS.read().iter()
                        .find(|s| s.id == sid)
                        .map(|s| s.parameters.clone());
                    if let Some(params) = snap_params {
                        spawn(async move {
                            let Some(chain) = get_current_fx_chain().await else { return; };
                            // Parse the JSON params back to DawParameterSnapshot via Facet
                            let json_str = serde_json::to_string(&params).unwrap_or_default();
                            match facet_json::from_str::<daw_bridge::DawParameterSnapshot>(&json_str) {
                                Ok(snap) => {
                                    match daw_bridge::apply_parameter_snapshot(&chain, &snap).await {
                                        Ok(result) => {
                                            *BLOCK_EDITOR_STATUS.write() = format!(
                                                "Recalled — {} params applied", result.changes.len()
                                            );
                                        }
                                        Err(e) => {
                                            *BLOCK_EDITOR_STATUS.write() = format!("Recall failed: {e}");
                                        }
                                    }
                                }
                                Err(e) => {
                                    *BLOCK_EDITOR_STATUS.write() = format!("Parse error: {e}");
                                }
                            }
                        });
                    }
                },
                "Recall"
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// In-Memory Snapshot Grid (for presets not yet saved to DB)
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct InMemorySnapshotGridProps {
    preset: BlockPresetSlot,
    is_connected: bool,
}

#[component]
fn InMemorySnapshotGrid(props: InMemorySnapshotGridProps) -> Element {
    if props.preset.snapshots.is_empty() {
        return rsx! {
            div { class: "flex items-center justify-center h-full",
                p { class: "text-xs text-zinc-600",
                    "No snapshots — capture one with the button above"
                }
            }
        };
    }

    let baseline = props
        .preset
        .snapshots
        .first()
        .and_then(|s| s.parameter_snapshot.as_ref());
    let snap_diffs: Vec<(BlockSnapshotSlot, Option<Vec<DawParamChange>>)> = props
        .preset
        .snapshots
        .iter()
        .enumerate()
        .map(|(idx, snap)| {
            let diff = if idx == 0 {
                None
            } else {
                match (baseline, &snap.parameter_snapshot) {
                    (Some(base), Some(current)) => {
                        Some(daw_bridge::diff_parameter_snapshots(base, current))
                    }
                    _ => None,
                }
            };
            (snap.clone(), diff)
        })
        .collect();

    rsx! {
        div { class: "grid grid-cols-2 gap-2",
            for (snap, diff) in snap_diffs.iter() {
                {
                    let sid = snap.id;
                    let is_snap_selected = *SELECTED_BLOCK_SNAPSHOT.read() == Some(sid);
                    let has_data = snap.parameter_snapshot.is_some();
                    let param_count: usize = snap.parameter_snapshot.as_ref()
                        .map(|ps| ps.fx_states.iter().map(|s| s.parameters.len()).sum())
                        .unwrap_or(0);
                    let diff_count = diff.as_ref().map(|d| d.len());
                    let top_changes: Vec<String> = diff.as_ref()
                        .map(|d| {
                            let mut sorted = d.clone();
                            sorted.sort_by(|a, b| b.magnitude().partial_cmp(&a.magnitude()).unwrap_or(std::cmp::Ordering::Equal));
                            sorted.iter().take(3).map(|c| {
                                let delta = ((c.to_value - c.from_value) * 100.0).round();
                                let sign = if delta > 0.0 { "+" } else { "" };
                                format!("{}: {sign}{delta:.0}%", c.param_name)
                            }).collect()
                        })
                        .unwrap_or_default();

                    rsx! {
                        div {
                            key: "{sid}",
                            class: if is_snap_selected {
                                "flex flex-col gap-1 px-3 py-2.5 rounded-lg border transition-all duration-100 cursor-pointer \
                                 bg-cyan-950/30 border-cyan-500/30"
                            } else {
                                "flex flex-col gap-1 px-3 py-2.5 rounded-lg border transition-all duration-100 cursor-pointer \
                                 hover:bg-zinc-800/30 border-zinc-800/50"
                            },
                            onclick: move |_| {
                                *SELECTED_BLOCK_SNAPSHOT.write() = Some(sid);
                            },
                            // Name row
                            div { class: "flex items-center justify-between",
                                span { class: "text-xs font-medium text-zinc-200 truncate",
                                    "{snap.name}"
                                }
                                if has_data {
                                    span { class: "w-2 h-2 rounded-full bg-emerald-400/80 flex-shrink-0" }
                                } else {
                                    span { class: "w-2 h-2 rounded-full bg-zinc-700 flex-shrink-0" }
                                }
                            }
                            // Stats
                            if has_data {
                                div { class: "flex items-center gap-2",
                                    span { class: "text-[9px] text-zinc-500 font-mono",
                                        "{param_count} params"
                                    }
                                    if let Some(dc) = diff_count {
                                        if dc > 0 {
                                            span { class: "text-[9px] text-amber-400 font-medium",
                                                "{dc} changed"
                                            }
                                        } else {
                                            span { class: "text-[9px] text-emerald-400/60",
                                                "identical"
                                            }
                                        }
                                    } else {
                                        span { class: "text-[9px] text-zinc-600 italic",
                                            "baseline"
                                        }
                                    }
                                }
                            }
                            // Top changes
                            if !top_changes.is_empty() {
                                div { class: "flex flex-col",
                                    for change_text in top_changes.iter() {
                                        span { class: "text-[8px] font-mono text-zinc-600 truncate",
                                            "{change_text}"
                                        }
                                    }
                                }
                            }
                            // Recall button
                            div { class: "mt-0.5",
                                button {
                                    class: "w-full px-2 py-1 rounded text-[9px] font-semibold \
                                            bg-cyan-500/10 text-cyan-400 border border-cyan-500/20 \
                                            hover:bg-cyan-500/20 transition-all duration-100 \
                                            disabled:opacity-25 disabled:cursor-not-allowed",
                                    disabled: !has_data || !props.is_connected,
                                    onclick: move |evt| {
                                        evt.stop_propagation();
                                        spawn(async move {
                                            let Some(chain) = get_current_fx_chain().await else { return; };
                                            let snap_data = {
                                                let lib = BLOCK_LIBRARY.read();
                                                lib.iter()
                                                    .flat_map(|p| p.snapshots.iter())
                                                    .find(|s| s.id == sid)
                                                    .and_then(|s| s.parameter_snapshot.clone())
                                            };
                                            if let Some(snap) = snap_data {
                                                match daw_bridge::apply_parameter_snapshot(&chain, &snap).await {
                                                    Ok(result) => {
                                                        *BLOCK_EDITOR_STATUS.write() = format!(
                                                            "Recalled — {} params applied", result.changes.len()
                                                        );
                                                    }
                                                    Err(e) => {
                                                        *BLOCK_EDITOR_STATUS.write() = format!("Recall failed: {e}");
                                                    }
                                                }
                                            }
                                        });
                                    },
                                    "Recall"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Capture Handlers
// ─────────────────────────────────────────────────────────────────────────────

/// Handle preset capture — creates both an in-memory slot and a DB record.
async fn handle_capture_preset(bt: signal_control::block::BlockType, name: String) {
    let Some(chain) = get_current_fx_chain().await else {
        *BLOCK_EDITOR_STATUS.write() = "No track available".into();
        return;
    };

    let selected_guids: Vec<String> = SELECTED_FX_GUIDS.read().iter().cloned().collect();
    if selected_guids.is_empty() {
        *BLOCK_EDITOR_STATUS.write() = "Select FX in the right panel first".into();
        return;
    }

    debug!(
        "Capture preset: {} FX selected: {:?}",
        selected_guids.len(),
        selected_guids
    );

    let fx_list = chain.all().await.unwrap_or_default();
    let plugin_names: Vec<String> = selected_guids
        .iter()
        .filter_map(|g| {
            fx_list
                .iter()
                .find(|f| f.guid == *g)
                .map(|f| f.plugin_name.clone())
        })
        .collect();
    let plugin_name = if plugin_names.len() == 1 {
        plugin_names[0].clone()
    } else {
        format!("{} FX", plugin_names.len())
    };

    // Capture RfxChain text
    let chunk = match capture::capture_selected_fx_rfxchain(&chain, &selected_guids).await {
        Ok(rfx_text) => {
            debug!(
                "Captured RfxChain block ({} bytes, {} FX)",
                rfx_text.len(),
                selected_guids.len()
            );
            Some(rfx_text)
        }
        Err(e) => {
            warn!("RfxChain capture failed: {e}");
            *BLOCK_EDITOR_STATUS.write() = format!("Capture failed: {e}");
            None
        }
    };

    // Capture parameter snapshot for selected FX
    let snap = match signal_control::daw_bridge::capture_parameter_snapshot(&chain, &name).await {
        Ok(full) => {
            let guid_set: std::collections::HashSet<&str> =
                selected_guids.iter().map(|s| s.as_str()).collect();
            let filtered_states: Vec<_> = full
                .fx_states
                .into_iter()
                .filter(|s| guid_set.contains(s.fx_guid.as_str()))
                .collect();
            if filtered_states.is_empty() {
                None
            } else {
                Some(signal_control::daw_bridge::DawParameterSnapshot {
                    name: full.name,
                    fx_states: filtered_states,
                })
            }
        }
        Err(e) => {
            warn!("Snapshot capture failed: {e}");
            None
        }
    };

    // Save to DB if service available
    let db_id = if let Some(ctl) = RIG_SERVICE.read().clone() {
        let plugin_id = serde_json::json!({ "name": &plugin_name, "guids": &selected_guids });
        match ctl
            .create_block_preset(&name, bt.display_name(), Some(plugin_id), None, None)
            .await
        {
            Ok(id) => {
                debug!("Saved block preset to DB: {id}");
                // Also save the initial snapshot to DB
                if let Some(ref snap_data) = snap {
                    let params_json = facet_to_json_value(snap_data);
                    match ctl
                        .create_block_snapshot(id, "Default", params_json, chunk.as_deref(), true)
                        .await
                    {
                        Ok(snap_id) => debug!("Saved initial snapshot to DB: {snap_id}"),
                        Err(e) => warn!("Failed to save snapshot to DB: {e}"),
                    }
                }
                Some(id)
            }
            Err(e) => {
                warn!("Failed to save preset to DB: {e}");
                None
            }
        }
    } else {
        None
    };

    // Also store in-memory for immediate UI feedback
    let preset_id = db_id.unwrap_or_else(Uuid::new_v4);
    let snap_id = Uuid::new_v4();

    let initial_snapshot = BlockSnapshotSlot {
        id: snap_id,
        name: "Default".to_string(),
        parameter_snapshot: snap,
        is_morphable: true,
    };

    let preset = BlockPresetSlot {
        id: preset_id,
        name: name.clone(),
        block_type: bt,
        plugin_name: Some(plugin_name.clone()),
        source_fx_guids: selected_guids,
        chunk_data: chunk,
        snapshots: vec![initial_snapshot],
        created_at: "Just now".to_string(),
    };

    // If saved to DB, refresh from DB; otherwise store in memory
    if db_id.is_some() {
        refresh_presets_for_type(bt.display_name()).await;
        refresh_type_counts().await;
        *SELECTED_BLOCK_PRESET.write() = Some(preset_id);
        refresh_snapshots_for_preset(preset_id).await;
    } else {
        BLOCK_LIBRARY.write().push(preset);
        *SELECTED_BLOCK_PRESET.write() = Some(preset_id);
        *SELECTED_BLOCK_SNAPSHOT.write() = Some(snap_id);
    }

    debug!("Captured block preset '{}' ({})", name, plugin_name);
    *BLOCK_EDITOR_STATUS.write() = format!("Captured '{}' from {}", name, plugin_name);
}

/// Handle snapshot capture — saves to DB if preset is in DB.
async fn handle_capture_snapshot(preset_id: Uuid, name: String) {
    let Some(chain) = get_current_fx_chain().await else {
        *BLOCK_EDITOR_STATUS.write() = "No track available".into();
        return;
    };

    // Get the source FX GUIDs from either DB preset or in-memory preset
    let fx_guids: Vec<String> = {
        // First check DB presets
        let db = DB_BLOCK_PRESETS.read();
        if let Some(preset) = db.iter().find(|p| p.id == preset_id) {
            preset
                .plugin_id
                .as_ref()
                .and_then(|v| v.get("guids"))
                .and_then(|v| serde_json::from_value::<Vec<String>>(v.clone()).ok())
                .unwrap_or_default()
        } else {
            // Fall back to in-memory library
            let lib = BLOCK_LIBRARY.read();
            lib.iter()
                .find(|p| p.id == preset_id)
                .map(|p| p.source_fx_guids.clone())
                .unwrap_or_default()
        }
    };

    if fx_guids.is_empty() {
        *BLOCK_EDITOR_STATUS.write() = "Preset has no source FX GUIDs".into();
        return;
    }

    // Capture parameter snapshot
    let snap = match signal_control::daw_bridge::capture_parameter_snapshot(&chain, &name).await {
        Ok(full) => {
            let guid_set: std::collections::HashSet<&str> =
                fx_guids.iter().map(|s| s.as_str()).collect();
            let filtered: Vec<_> = full
                .fx_states
                .into_iter()
                .filter(|s| guid_set.contains(s.fx_guid.as_str()))
                .collect();
            if filtered.is_empty() {
                *BLOCK_EDITOR_STATUS.write() = "Source FX not found on current track".into();
                return;
            }
            signal_control::daw_bridge::DawParameterSnapshot {
                name: full.name,
                fx_states: filtered,
            }
        }
        Err(e) => {
            *BLOCK_EDITOR_STATUS.write() = format!("Snapshot capture failed: {e}");
            return;
        }
    };

    // Try saving to DB
    let is_db_preset = DB_BLOCK_PRESETS.read().iter().any(|p| p.id == preset_id);
    if is_db_preset {
        if let Some(ctl) = RIG_SERVICE.read().clone() {
            let params_json = facet_to_json_value(&snap);
            match ctl
                .create_block_snapshot(preset_id, &name, params_json, None, false)
                .await
            {
                Ok(snap_id) => {
                    debug!("Saved snapshot to DB: {snap_id}");
                    *SELECTED_BLOCK_SNAPSHOT.write() = Some(snap_id);
                    refresh_snapshots_for_preset(preset_id).await;
                    *BLOCK_EDITOR_STATUS.write() = format!("Captured snapshot '{}'", name);
                    return;
                }
                Err(e) => {
                    warn!("Failed to save snapshot to DB: {e}");
                }
            }
        }
    }

    // Fallback: save in memory
    let snap_id = Uuid::new_v4();
    let slot = BlockSnapshotSlot {
        id: snap_id,
        name: name.clone(),
        parameter_snapshot: Some(snap),
        is_morphable: true,
    };

    let mut lib = BLOCK_LIBRARY.write();
    if let Some(preset) = lib.iter_mut().find(|p| p.id == preset_id) {
        preset.snapshots.push(slot);
    }
    drop(lib);

    *SELECTED_BLOCK_SNAPSHOT.write() = Some(snap_id);
    debug!("Captured snapshot '{}' for preset {}", name, preset_id);
    *BLOCK_EDITOR_STATUS.write() = format!("Captured snapshot '{}'", name);
}

// ─────────────────────────────────────────────────────────────────────────────
// FX Selector — checkable list of FX on the current track
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct FxSelectorProps {
    fx_list: Signal<Vec<daw_control::Fx>>,
}

#[component]
fn FxSelector(props: FxSelectorProps) -> Element {
    let fx_list = props.fx_list.read();
    let selected = SELECTED_FX_GUIDS.read();
    let selected_count = selected.len();

    rsx! {
        div { class: "h-full flex flex-col bg-zinc-950/40",
            // Header
            div { class: "px-4 py-3 border-b border-border/30 flex items-center justify-between flex-shrink-0",
                div { class: "flex items-center gap-2",
                    h3 { class: "text-[11px] font-bold text-zinc-400 uppercase tracking-[0.15em]",
                        "Capture Source"
                    }
                    if selected_count > 0 {
                        span { class: "text-[9px] font-mono text-orange-300 bg-orange-500/10 px-1.5 rounded",
                            "{selected_count}"
                        }
                    }
                }
                div { class: "flex items-center gap-1",
                    button {
                        class: "text-[9px] text-zinc-500 hover:text-zinc-300 px-1.5 py-0.5 rounded hover:bg-zinc-700/40 transition-colors",
                        title: "Select all FX",
                        onclick: move |_| {
                            let all_guids: std::collections::HashSet<String> = props.fx_list.read()
                                .iter()
                                .map(|f| f.guid.clone())
                                .collect();
                            *SELECTED_FX_GUIDS.write() = all_guids;
                        },
                        "All"
                    }
                    button {
                        class: "text-[9px] text-zinc-500 hover:text-zinc-300 px-1.5 py-0.5 rounded hover:bg-zinc-700/40 transition-colors",
                        title: "Clear selection",
                        onclick: move |_| {
                            SELECTED_FX_GUIDS.write().clear();
                        },
                        "None"
                    }
                }
            }

            // FX list
            div { class: "flex-1 overflow-y-auto px-2 py-1.5",
                if fx_list.is_empty() {
                    div { class: "text-center text-zinc-600 text-xs py-12",
                        p { "No FX on track" }
                        p { class: "text-[10px] mt-1 text-zinc-700",
                            "Select a track in REAPER"
                        }
                    }
                } else {
                    for fx in fx_list.iter() {
                        {
                            let guid = fx.guid.clone();
                            let guid_for_toggle = guid.clone();
                            let is_checked = selected.contains(&guid);
                            let name = fx.plugin_name.clone();
                            let fx_type = match fx.plugin_type {
                                daw_control::FxType::Vst3 => "V3",
                                daw_control::FxType::Vst2 => "V2",
                                daw_control::FxType::Au => "AU",
                                daw_control::FxType::Js => "JS",
                                daw_control::FxType::Clap => "CL",
                                _ => "FX",
                            };
                            let enabled_class = if fx.enabled {
                                "text-zinc-200"
                            } else {
                                "text-zinc-500 line-through"
                            };

                            rsx! {
                                button {
                                    key: "{guid}",
                                    class: if is_checked {
                                        "w-full flex items-center gap-2.5 px-2.5 py-2 rounded-lg text-left transition-all duration-100 \
                                         bg-orange-500/8 border border-orange-500/20"
                                    } else {
                                        "w-full flex items-center gap-2.5 px-2.5 py-2 rounded-lg text-left transition-all duration-100 \
                                         hover:bg-zinc-800/30 border border-transparent"
                                    },
                                    onclick: move |_| {
                                        let mut sel = SELECTED_FX_GUIDS.write();
                                        if sel.contains(&guid_for_toggle) {
                                            sel.remove(&guid_for_toggle);
                                        } else {
                                            sel.insert(guid_for_toggle.clone());
                                        }
                                    },
                                    // Checkbox
                                    div {
                                        class: if is_checked {
                                            "w-3.5 h-3.5 rounded border-2 border-orange-400 bg-orange-500 flex items-center justify-center flex-shrink-0"
                                        } else {
                                            "w-3.5 h-3.5 rounded border-2 border-zinc-600 flex items-center justify-center flex-shrink-0"
                                        },
                                        if is_checked {
                                            span { class: "text-[7px] text-white font-bold", "\u{2713}" }
                                        }
                                    }
                                    // FX type badge
                                    span { class: "text-[8px] text-zinc-600 font-mono w-4 text-center flex-shrink-0",
                                        "{fx_type}"
                                    }
                                    // Name
                                    span { class: "flex-1 text-[11px] truncate {enabled_class}", "{name}" }
                                    // Enable indicator
                                    span {
                                        class: if fx.enabled {
                                            "w-1.5 h-1.5 rounded-full bg-emerald-400 flex-shrink-0"
                                        } else {
                                            "w-1.5 h-1.5 rounded-full bg-zinc-700 flex-shrink-0"
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

// ─────────────────────────────────────────────────────────────────────────────
// Capture Dialog (modal overlay)
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct CaptureDialogProps {
    label: String,
    placeholder: String,
    value: String,
    on_input: EventHandler<String>,
    on_submit: EventHandler<String>,
    on_cancel: EventHandler<()>,
}

#[component]
fn CaptureDialog(props: CaptureDialogProps) -> Element {
    let value_for_key = props.value.clone();
    let value_for_btn = props.value.clone();
    let is_empty = props.value.trim().is_empty();
    let placeholder = props.placeholder.clone();
    let input_value = props.value.clone();

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center bg-black/60",
            onclick: move |_| props.on_cancel.call(()),
            // Modal card — stop click propagation so clicking inside doesn't close
            div {
                class: "bg-zinc-900 border border-zinc-700/60 rounded-xl shadow-2xl shadow-black/40 \
                        w-full max-w-md mx-4 overflow-hidden",
                onclick: move |evt| evt.stop_propagation(),
                // Header
                div { class: "px-5 py-4 border-b border-zinc-800/60",
                    h3 { class: "text-sm font-semibold text-zinc-100", "{props.label}" }
                }
                // Body
                div { class: "px-5 py-4",
                    input {
                        class: "w-full bg-zinc-800/80 border border-zinc-700/50 rounded-lg px-3.5 py-2.5 text-sm text-zinc-200 \
                                outline-none focus:border-orange-500/50 focus:ring-1 focus:ring-orange-500/20 \
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
                        class: "px-4 py-2 rounded-lg text-xs font-semibold bg-orange-500 text-white \
                                hover:bg-orange-400 transition-colors disabled:opacity-30 disabled:cursor-not-allowed",
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
