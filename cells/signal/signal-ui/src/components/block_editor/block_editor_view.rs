//! Block Editor View — three-column layout for editing individual DSP blocks.
//!
//! Left: Block type selector (EQ, Boost, Compressor, Drive, Amp, Cab)
//! Center: Preset list + snapshot slots for the selected block type
//! Right: FxChainTree showing current REAPER track for capture source selection

use super::capture;
use super::library::*;
use crate::prelude::*;
use signal_control::daw_bridge::{self, DawParamChange};
use tracing::{debug, warn};
use uuid::Uuid;

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
// Main Component
// ─────────────────────────────────────────────────────────────────────────────

#[component]
pub fn BlockEditorView() -> Element {
    let block_types = use_signal(predefined_block_types);
    let selected_type = *SELECTED_BLOCK_TYPE.read();
    let selected_preset_id = *SELECTED_BLOCK_PRESET.read();
    let library = BLOCK_LIBRARY.read();
    let status = BLOCK_EDITOR_STATUS.read().clone();

    // Clone data out of signal guard so closures can be 'static
    let type_presets: Vec<BlockPresetSlot> = selected_type
        .map(|bt| {
            library
                .iter()
                .filter(|p| p.block_type == bt)
                .cloned()
                .collect()
        })
        .unwrap_or_default();

    let selected_preset: Option<BlockPresetSlot> =
        selected_preset_id.and_then(|id| library.iter().find(|p| p.id == id).cloned());
    let selected_snapshot_count = selected_preset
        .as_ref()
        .map(|p| p.snapshots.len())
        .unwrap_or(0);

    // Dialog state for naming new presets/snapshots
    let mut show_capture_preset_dialog = use_signal(|| false);
    let mut capture_preset_name = use_signal(String::new);
    let mut show_capture_snapshot_dialog = use_signal(|| false);
    let mut capture_snapshot_name = use_signal(String::new);

    // DAW connection state
    let mut daw_connected = use_signal(|| false);
    let mut daw_fx_list = use_signal(Vec::<daw_control::Fx>::new);

    // Poll DAW for FX list (for selecting capture source)
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
    let preset_count = type_presets.len();

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card overflow-hidden",
            // ── Main content ─────────────────────────────────────
            div { class: "flex-1 flex min-h-0 overflow-hidden",

                // ── Left: Block Type Selector ────────────────────
                div { class: "w-52 flex-shrink-0 border-r border-border flex flex-col min-h-0 bg-zinc-950/30",
                    div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                        h3 { class: "text-[10px] font-semibold text-muted-foreground uppercase tracking-wider",
                            "Block Types"
                        }
                    }
                    div { class: "flex-1 overflow-y-auto px-1.5 py-1.5",
                        for def in block_types.read().iter() {
                            {
                                let bt = def.block_type;
                                let display_name = def.display_name;
                                let color = def.color;
                                let description = def.description;
                                let icon_char = &def.display_name[..1];
                                let is_active = selected_type == Some(bt);
                                let preset_count_for_type = library.iter().filter(|p| p.block_type == bt).count();
                                rsx! {
                                    button {
                                        key: "{display_name}",
                                        class: if is_active {
                                            "w-full flex items-center gap-3 px-3 py-2.5 rounded-lg text-left transition-all \
                                             bg-primary/15 border border-primary/30 text-foreground"
                                        } else {
                                            "w-full flex items-center gap-3 px-3 py-2.5 rounded-lg text-left transition-all \
                                             hover:bg-accent/30 border border-transparent text-muted-foreground hover:text-foreground"
                                        },
                                        onclick: move |_| {
                                            *SELECTED_BLOCK_TYPE.write() = Some(bt);
                                            *SELECTED_BLOCK_PRESET.write() = None;
                                            *SELECTED_BLOCK_SNAPSHOT.write() = None;
                                            *BLOCK_EDITOR_STATUS.write() = format!("Selected: {}", display_name);
                                        },
                                        // Icon circle
                                        div {
                                            class: "w-8 h-8 rounded-lg flex items-center justify-center flex-shrink-0 \
                                                    {color} bg-current/10 text-sm font-bold",
                                            style: "background-color: color-mix(in srgb, currentColor 12%, transparent);",
                                            "{icon_char}"
                                        }
                                        // Name + description
                                        div { class: "flex-1 min-w-0",
                                            div { class: "flex items-center justify-between",
                                                span { class: "text-xs font-medium truncate", "{display_name}" }
                                                if preset_count_for_type > 0 {
                                                    span { class: "text-[9px] text-muted-foreground bg-muted px-1.5 rounded-full flex-shrink-0",
                                                        "{preset_count_for_type}"
                                                    }
                                                }
                                            }
                                            p { class: "text-[9px] text-muted-foreground/70 truncate mt-0.5",
                                                "{description}"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // ── Center: Presets + Snapshots ──────────────────
                div { class: "flex-1 flex flex-col min-h-0 min-w-0 overflow-hidden",

                    if selected_type.is_none() {
                        // No type selected — show prompt
                        div { class: "flex-1 flex items-center justify-center",
                            div { class: "text-center",
                                p { class: "text-sm font-medium text-muted-foreground mb-1",
                                    "Select a Block Type"
                                }
                                p { class: "text-xs text-muted-foreground/60",
                                    "Choose a block type from the left panel to view and manage its presets"
                                }
                            }
                        }
                    } else {
                        // ── Preset List (top half) ───────────────
                        div { class: "flex-1 flex flex-col min-h-0 border-b border-border",
                            // Header
                            div { class: "px-3 py-2 border-b border-border flex items-center justify-between flex-shrink-0",
                                div { class: "flex items-center gap-2",
                                    h3 { class: "text-[10px] font-semibold text-muted-foreground uppercase tracking-wider",
                                        "Presets"
                                    }
                                    span { class: "text-[9px] text-muted-foreground",
                                        "{preset_count}"
                                    }
                                }
                                button {
                                    class: "flex items-center gap-1 px-2 py-1 rounded text-[10px] font-medium \
                                            bg-primary/20 text-primary border border-primary/30 \
                                            hover:bg-primary/30 transition-colors disabled:opacity-30",
                                    disabled: !is_connected,
                                    title: "Capture current FX state as a new block preset",
                                    onclick: move |_| {
                                        capture_preset_name.set(String::new());
                                        show_capture_preset_dialog.set(true);
                                    },
                                    "+ Capture Preset"
                                }
                            }

                            // Capture preset dialog (inline)
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
                                            let Some(chain) = get_current_fx_chain().await else {
                                                *BLOCK_EDITOR_STATUS.write() = "No track available".into();
                                                return;
                                            };
                                            // Get all FX — user picks first one for now
                                            // TODO: let user pick specific FX from the FxChainTree
                                            // Get selected FX GUIDs from the selector
                                            let selected_guids: Vec<String> = SELECTED_FX_GUIDS.read().iter().cloned().collect();
                                            if selected_guids.is_empty() {
                                                *BLOCK_EDITOR_STATUS.write() = "Select FX in the right panel first".into();
                                                return;
                                            }

                                            debug!("Capture preset: {} FX selected: {:?}", selected_guids.len(), selected_guids);

                                            // Get plugin names for display
                                            let fx_list = chain.all().await.unwrap_or_default();
                                            let plugin_names: Vec<String> = selected_guids
                                                .iter()
                                                .filter_map(|g| fx_list.iter().find(|f| f.guid == *g).map(|f| f.plugin_name.clone()))
                                                .collect();
                                            let plugin_name = if plugin_names.len() == 1 {
                                                plugin_names[0].clone()
                                            } else {
                                                format!("{} FX", plugin_names.len())
                                            };

                                            // Capture RfxChain text for all selected FX
                                            let chunk = match capture::capture_selected_fx_rfxchain(&chain, &selected_guids).await {
                                                Ok(rfx_text) => {
                                                    debug!("Captured RfxChain block ({} bytes, {} FX)", rfx_text.len(), selected_guids.len());
                                                    Some(rfx_text)
                                                }
                                                Err(e) => {
                                                    warn!("RfxChain capture failed: {e}");
                                                    *BLOCK_EDITOR_STATUS.write() = format!("Capture failed: {e}");
                                                    None
                                                }
                                            };

                                            // Capture initial parameter snapshot for all selected FX
                                            let snap = match signal_control::daw_bridge::capture_parameter_snapshot(&chain, &name).await {
                                                Ok(full) => {
                                                    let guid_set: std::collections::HashSet<&str> = selected_guids.iter().map(|s| s.as_str()).collect();
                                                    let filtered_states: Vec<_> = full.fx_states.into_iter()
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

                                            let preset_id = Uuid::new_v4();
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
                                                source_fx_guids: selected_guids.clone(),
                                                chunk_data: chunk,
                                                snapshots: vec![initial_snapshot],
                                                created_at: "Just now".to_string(),
                                            };

                                            BLOCK_LIBRARY.write().push(preset);
                                            *SELECTED_BLOCK_PRESET.write() = Some(preset_id);
                                            *SELECTED_BLOCK_SNAPSHOT.write() = Some(snap_id);

                                            debug!("Captured block preset '{}' ({})", name, plugin_name);
                                            *BLOCK_EDITOR_STATUS.write() = format!(
                                                "Captured '{}' from {}", name, plugin_name
                                            );
                                        });
                                    },
                                    on_cancel: move |_| show_capture_preset_dialog.set(false),
                                }
                            }

                            // Preset list (scrollable)
                            div { class: "flex-1 overflow-y-auto min-h-0",
                                if type_presets.is_empty() {
                                    div { class: "flex items-center justify-center h-full px-4",
                                        div { class: "text-center",
                                            p { class: "text-xs text-muted-foreground mb-1",
                                                "No presets yet"
                                            }
                                            p { class: "text-[10px] text-muted-foreground/60",
                                                "Set up a plugin in REAPER, then click \"+ Capture Preset\" to save it"
                                            }
                                        }
                                    }
                                } else {
                                    div { class: "px-1.5 py-1",
                                        for preset in type_presets.iter() {
                                            {
                                                let pid = preset.id;
                                                let is_selected = selected_preset_id == Some(pid);
                                                let snap_count = preset.snapshots.len();
                                                rsx! {
                                                    button {
                                                        key: "{pid}",
                                                        class: if is_selected {
                                                            "w-full flex items-center gap-3 px-3 py-2 rounded-lg text-left transition-all \
                                                             bg-accent border border-accent-foreground/10"
                                                        } else {
                                                            "w-full flex items-center gap-3 px-3 py-2 rounded-lg text-left transition-all \
                                                             hover:bg-accent/40 border border-transparent"
                                                        },
                                                        onclick: move |_| {
                                                            *SELECTED_BLOCK_PRESET.write() = Some(pid);
                                                            *SELECTED_BLOCK_SNAPSHOT.write() = None;
                                                        },
                                                        div { class: "flex-1 min-w-0",
                                                            div { class: "flex items-center gap-2",
                                                                span { class: "text-xs font-medium text-foreground truncate",
                                                                    "{preset.name}"
                                                                }
                                                            }
                                                            if let Some(ref plugin) = preset.plugin_name {
                                                                p { class: "text-[9px] text-muted-foreground truncate mt-0.5",
                                                                    "{plugin}"
                                                                }
                                                            }
                                                        }
                                                        // Snapshot count badge
                                                        span { class: "text-[9px] text-muted-foreground bg-muted px-1.5 py-0.5 rounded-full flex-shrink-0",
                                                            "{snap_count} snap"
                                                        }
                                                        // Created timestamp
                                                        span { class: "text-[9px] text-muted-foreground/60 flex-shrink-0",
                                                            "{preset.created_at}"
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // ── Snapshot Slots (bottom half) ─────────
                        div { class: "flex-1 flex flex-col min-h-0",
                            // Header
                            div { class: "px-3 py-2 border-b border-border flex items-center justify-between flex-shrink-0",
                                div { class: "flex items-center gap-2",
                                    h3 { class: "text-[10px] font-semibold text-muted-foreground uppercase tracking-wider",
                                        "Snapshots"
                                    }
                                    if let Some(ref p) = selected_preset {
                                        span { class: "text-[9px] text-muted-foreground",
                                            "{p.snapshots.len()}"
                                        }
                                    }
                                }
                                button {
                                    class: "flex items-center gap-1 px-2 py-1 rounded text-[10px] font-medium \
                                            bg-blue-900/30 text-blue-300 border border-blue-800/30 \
                                            hover:bg-blue-900/50 transition-colors disabled:opacity-30",
                                    disabled: !is_connected || selected_preset_id.is_none(),
                                    title: "Capture current parameter values as a new snapshot",
                                    onclick: move |_| {
                                        capture_snapshot_name.set(format!("Snap {}", selected_snapshot_count + 1));
                                        show_capture_snapshot_dialog.set(true);
                                    },
                                    "+ Capture Snapshot"
                                }
                            }

                            // Capture snapshot dialog (inline)
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
                                            let Some(chain) = get_current_fx_chain().await else {
                                                *BLOCK_EDITOR_STATUS.write() = "No track available".into();
                                                return;
                                            };
                                            // Find the source FX GUIDs from the preset
                                            let fx_guids: Vec<String> = {
                                                let lib = BLOCK_LIBRARY.read();
                                                lib.iter()
                                                    .find(|p| p.id == preset_id)
                                                    .map(|p| p.source_fx_guids.clone())
                                                    .unwrap_or_default()
                                            };
                                            if fx_guids.is_empty() {
                                                *BLOCK_EDITOR_STATUS.write() = "Preset has no source FX GUIDs".into();
                                                return;
                                            }

                                            // Capture parameter snapshot for the preset's FX
                                            let snap = match signal_control::daw_bridge::capture_parameter_snapshot(&chain, &name).await {
                                                Ok(full) => {
                                                    let guid_set: std::collections::HashSet<&str> = fx_guids.iter().map(|s| s.as_str()).collect();
                                                    let filtered: Vec<_> = full.fx_states.into_iter()
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

                                            let snap_id = Uuid::new_v4();
                                            let slot = BlockSnapshotSlot {
                                                id: snap_id,
                                                name: name.clone(),
                                                parameter_snapshot: Some(snap),
                                                is_morphable: true,
                                            };

                                            // Add to the preset's snapshots
                                            let mut lib = BLOCK_LIBRARY.write();
                                            if let Some(preset) = lib.iter_mut().find(|p| p.id == preset_id) {
                                                preset.snapshots.push(slot);
                                            }
                                            drop(lib);

                                            *SELECTED_BLOCK_SNAPSHOT.write() = Some(snap_id);
                                            debug!("Captured snapshot '{}' for preset {}", name, preset_id);
                                            *BLOCK_EDITOR_STATUS.write() = format!("Captured snapshot '{}'", name);
                                        });
                                    },
                                    on_cancel: move |_| show_capture_snapshot_dialog.set(false),
                                }
                            }

                            // Snapshot grid (scrollable)
                            div { class: "flex-1 overflow-y-auto min-h-0 px-2 py-1.5",
                                if selected_preset_id.is_none() {
                                    div { class: "flex items-center justify-center h-full",
                                        p { class: "text-xs text-muted-foreground/60",
                                            "Select a preset above to view its snapshots"
                                        }
                                    }
                                } else if let Some(ref preset) = selected_preset {
                                    if preset.snapshots.is_empty() {
                                        div { class: "flex items-center justify-center h-full",
                                            p { class: "text-xs text-muted-foreground/60",
                                                "No snapshots — capture one with the button above"
                                            }
                                        }
                                    } else {
                                        {
                                            // Compute diffs against baseline (first snapshot) for each subsequent snapshot
                                            let baseline = preset.snapshots.first().and_then(|s| s.parameter_snapshot.as_ref());
                                            let snap_diffs: Vec<(BlockSnapshotSlot, Option<Vec<DawParamChange>>)> = preset
                                                .snapshots
                                                .iter()
                                                .enumerate()
                                                .map(|(idx, snap)| {
                                                    let diff = if idx == 0 {
                                                        None // baseline — no diff
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
                                                div { class: "grid grid-cols-2 gap-1.5",
                                                    for (snap, diff) in snap_diffs.iter() {
                                                        {
                                                            let sid = snap.id;
                                                            let is_snap_selected = *SELECTED_BLOCK_SNAPSHOT.read() == Some(sid);
                                                            let morph_badge = if snap.is_morphable { "" } else { " (fixed)" };
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
                                                                        "flex flex-col gap-1 px-3 py-2 rounded-lg border transition-all cursor-pointer \
                                                                         bg-blue-900/20 border-blue-500/40 text-foreground"
                                                                    } else {
                                                                        "flex flex-col gap-1 px-3 py-2 rounded-lg border transition-all cursor-pointer \
                                                                         hover:bg-accent/30 border-border text-muted-foreground hover:text-foreground"
                                                                    },
                                                                    onclick: move |_| {
                                                                        *SELECTED_BLOCK_SNAPSHOT.write() = Some(sid);
                                                                    },
                                                                    // Name + status dot
                                                                    div { class: "flex items-center justify-between",
                                                                        span { class: "text-xs font-medium truncate",
                                                                            "{snap.name}{morph_badge}"
                                                                        }
                                                                        if has_data {
                                                                            span { class: "w-2 h-2 rounded-full bg-green-500 flex-shrink-0" }
                                                                        } else {
                                                                            span { class: "w-2 h-2 rounded-full bg-zinc-600 flex-shrink-0" }
                                                                        }
                                                                    }
                                                                    // Parameter summary
                                                                    if has_data {
                                                                        div { class: "flex items-center gap-2",
                                                                            span { class: "text-[9px] text-muted-foreground",
                                                                                "{param_count} params"
                                                                            }
                                                                            if let Some(dc) = diff_count {
                                                                                if dc > 0 {
                                                                                    span { class: "text-[9px] text-amber-400 font-medium",
                                                                                        "{dc} changed"
                                                                                    }
                                                                                } else {
                                                                                    span { class: "text-[9px] text-green-400/60",
                                                                                        "identical"
                                                                                    }
                                                                                }
                                                                            } else {
                                                                                span { class: "text-[9px] text-muted-foreground/50 italic",
                                                                                    "baseline"
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                    // Top parameter changes (up to 3)
                                                                    if !top_changes.is_empty() {
                                                                        div { class: "flex flex-col gap-0",
                                                                            for change_text in top_changes.iter() {
                                                                                span { class: "text-[8px] font-mono text-muted-foreground/70 truncate",
                                                                                    "{change_text}"
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                    // Recall button
                                                                    div { class: "flex gap-1 mt-0.5",
                                                                        button {
                                                                            class: "flex-1 px-2 py-1 rounded text-[9px] font-medium \
                                                                                    bg-blue-900/30 text-blue-300 border border-blue-800/30 \
                                                                                    hover:bg-blue-900/50 transition-colors disabled:opacity-30",
                                                                            disabled: !has_data || !is_connected,
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
                                                                                                    "Recalled — {} params changed", result.changes.len()
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
                                    }
                                }
                            }
                        }
                    }
                }

                // ── Right: FX Selector ────────────────────────────
                div { class: "w-72 flex-shrink-0 border-l border-border flex flex-col min-h-0",
                    FxSelector { fx_list: daw_fx_list }
                }
            }

            // ── Bottom: Status bar ───────────────────────────────
            div { class: "px-3 py-1.5 border-t border-border flex items-center gap-2 flex-shrink-0 bg-zinc-900/40",
                span { class: "text-[10px] text-muted-foreground", "Status:" }
                span { class: "text-[10px] text-foreground font-mono truncate",
                    "{status}"
                }
                div { class: "flex-1" }
                if !is_connected {
                    span { class: "text-[9px] text-amber-400",
                        "DAW not connected"
                    }
                }
            }
        }
    }
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
        div { class: "h-full flex flex-col",
            // Header
            div { class: "px-3 py-2 border-b border-border flex items-center justify-between flex-shrink-0",
                div { class: "flex items-center gap-2",
                    h3 { class: "text-[10px] font-semibold text-muted-foreground uppercase tracking-wider",
                        "Select FX"
                    }
                    if selected_count > 0 {
                        span { class: "text-[9px] text-primary font-medium bg-primary/15 px-1.5 rounded-full",
                            "{selected_count} selected"
                        }
                    }
                }
                div { class: "flex items-center gap-1",
                    // Select All
                    button {
                        class: "text-[9px] text-muted-foreground hover:text-foreground px-1.5 py-0.5 rounded hover:bg-accent/50 transition-colors",
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
                    // Clear
                    button {
                        class: "text-[9px] text-muted-foreground hover:text-foreground px-1.5 py-0.5 rounded hover:bg-accent/50 transition-colors",
                        title: "Clear selection",
                        onclick: move |_| {
                            SELECTED_FX_GUIDS.write().clear();
                        },
                        "None"
                    }
                }
            }

            // FX list with checkboxes
            div { class: "flex-1 overflow-y-auto px-1.5 py-1",
                if fx_list.is_empty() {
                    div { class: "text-center text-muted-foreground text-xs py-8",
                        "No FX on track"
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
                                "text-foreground"
                            } else {
                                "text-muted-foreground line-through"
                            };
                            rsx! {
                                button {
                                    key: "{guid}",
                                    class: if is_checked {
                                        "w-full flex items-center gap-2 px-2 py-1.5 rounded-lg text-left transition-all \
                                         bg-primary/10 border border-primary/30"
                                    } else {
                                        "w-full flex items-center gap-2 px-2 py-1.5 rounded-lg text-left transition-all \
                                         hover:bg-accent/30 border border-transparent"
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
                                            "w-4 h-4 rounded border-2 border-primary bg-primary flex items-center justify-center flex-shrink-0"
                                        } else {
                                            "w-4 h-4 rounded border-2 border-zinc-600 flex items-center justify-center flex-shrink-0"
                                        },
                                        if is_checked {
                                            span { class: "text-[8px] text-primary-foreground font-bold", "\u{2713}" }
                                        }
                                    }
                                    // FX type badge
                                    span { class: "text-[9px] text-muted-foreground font-mono w-4 text-center flex-shrink-0",
                                        "{fx_type}"
                                    }
                                    // Name
                                    span { class: "flex-1 text-xs truncate {enabled_class}", "{name}" }
                                    // Enable indicator
                                    span {
                                        class: if fx.enabled { "w-2 h-2 rounded-full bg-green-500 flex-shrink-0" } else { "w-2 h-2 rounded-full bg-neutral-600 flex-shrink-0" },
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
// Capture Dialog (inline)
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
    // Clone value for use across multiple closures in RSX
    let value_for_key = props.value.clone();
    let value_for_btn = props.value.clone();
    let is_empty = props.value.trim().is_empty();

    rsx! {
        div { class: "px-3 py-2 border-b border-border bg-accent/20 flex items-center gap-2 flex-shrink-0",
            span { class: "text-[10px] text-muted-foreground whitespace-nowrap", "{props.label}:" }
            input {
                class: "flex-1 bg-background border border-border rounded px-2 py-1 text-xs text-foreground \
                        outline-none focus:border-primary",
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
                class: "px-2 py-1 rounded text-[10px] font-medium bg-primary/80 text-primary-foreground \
                        hover:bg-primary transition-colors disabled:opacity-30",
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
                class: "px-2 py-1 rounded text-[10px] font-medium text-muted-foreground hover:text-foreground transition-colors",
                onclick: move |_| props.on_cancel.call(()),
                "Cancel"
            }
        }
    }
}
