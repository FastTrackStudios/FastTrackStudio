//! Advanced Inspector View — raw chunk data and parameter tables for block presets/snapshots.
//!
//! Shows the literal captured data for debugging and advanced users:
//! - Parameter table: all param indices, names, and normalized values
//! - Parameter diff: side-by-side comparison between two snapshots
//! - Chunk data: raw RPP FX state chunk (base64-encoded binary)

use crate::components::block_editor::library::*;
use crate::prelude::*;
use signal_control::daw_bridge::{self, DawParamValue};
use std::collections::HashSet;
use tracing::warn;
use uuid::Uuid;

/// Sub-tab within the Advanced Inspector.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum InspectorTab {
    #[default]
    Parameters,
    ChunkData,
    SnapshotDiff,
}

#[component]
pub fn AdvancedInspectorView() -> Element {
    let mut inspector_tab = use_signal(|| InspectorTab::Parameters);
    let selected_preset_id = *SELECTED_BLOCK_PRESET.read();
    let selected_snapshot_id = *SELECTED_BLOCK_SNAPSHOT.read();
    let library = BLOCK_LIBRARY.read();

    // Find selected preset and snapshot
    let selected_preset: Option<BlockPresetSlot> =
        selected_preset_id.and_then(|id| library.iter().find(|p| p.id == id).cloned());
    let selected_snapshot: Option<BlockSnapshotSlot> = selected_preset.as_ref().and_then(|p| {
        selected_snapshot_id.and_then(|sid| p.snapshots.iter().find(|s| s.id == sid).cloned())
    });

    let active_tab = *inspector_tab.read();

    rsx! {
        div { class: "flex-1 w-full flex flex-col bg-card overflow-hidden min-h-0",
            // ── Header with context info ─────────────────────────
            div { class: "px-4 py-2 border-b border-border flex items-center gap-4 flex-shrink-0 bg-zinc-900/60",
                // Context: what's selected
                div { class: "flex items-center gap-2 flex-1 min-w-0",
                    span { class: "text-[10px] font-semibold text-muted-foreground uppercase tracking-wider",
                        "Inspector"
                    }
                    if let Some(ref preset) = selected_preset {
                        span { class: "text-[10px] text-zinc-500", "/" }
                        span { class: "text-xs text-foreground font-medium truncate",
                            "{preset.name}"
                        }
                        if let Some(ref plugin) = preset.plugin_name {
                            span { class: "text-[10px] text-muted-foreground", "({plugin})" }
                        }
                        if let Some(ref snap) = selected_snapshot {
                            span { class: "text-[10px] text-zinc-500", "/" }
                            span { class: "text-xs text-blue-300 font-medium truncate",
                                "{snap.name}"
                            }
                        }
                    } else {
                        span { class: "text-xs text-muted-foreground/60 italic",
                            "Select a block preset in the Blocks tab to inspect"
                        }
                    }
                }
                // Sub-tab buttons
                div { class: "flex items-center gap-0.5 bg-zinc-800/80 rounded-lg p-0.5",
                    InspectorTabButton { label: "Parameters", tab: InspectorTab::Parameters, active: active_tab, on_click: move |t| inspector_tab.set(t) }
                    InspectorTabButton { label: "Chunk Data", tab: InspectorTab::ChunkData, active: active_tab, on_click: move |t| inspector_tab.set(t) }
                    InspectorTabButton { label: "Snapshot Diff", tab: InspectorTab::SnapshotDiff, active: active_tab, on_click: move |t| inspector_tab.set(t) }
                }
            }

            // ── Content ──────────────────────────────────────────
            div { class: "flex-1 min-h-0 flex flex-col overflow-hidden",
                match active_tab {
                    InspectorTab::Parameters => rsx! {
                        ParameterTableView { preset: selected_preset.clone(), snapshot: selected_snapshot.clone() }
                    },
                    InspectorTab::ChunkData => rsx! {
                        ChunkDataView { preset: selected_preset.clone() }
                    },
                    InspectorTab::SnapshotDiff => rsx! {
                        SnapshotDiffView { preset: selected_preset.clone() }
                    },
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Inspector Tab Button
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct InspectorTabButtonProps {
    label: &'static str,
    tab: InspectorTab,
    active: InspectorTab,
    on_click: EventHandler<InspectorTab>,
}

#[component]
fn InspectorTabButton(props: InspectorTabButtonProps) -> Element {
    let is_active = props.tab == props.active;
    let tab = props.tab;
    rsx! {
        button {
            class: if is_active {
                "px-2.5 py-1 rounded-md text-[10px] font-medium bg-primary text-primary-foreground transition-colors"
            } else {
                "px-2.5 py-1 rounded-md text-[10px] font-medium text-zinc-400 hover:text-zinc-200 hover:bg-zinc-700/50 transition-colors"
            },
            onclick: move |_| props.on_click.call(tab),
            "{props.label}"
        }
    }
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
// Parameter Table View — Interactive, collapsible, with real-time sliders
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct ParameterTableViewProps {
    preset: Option<BlockPresetSlot>,
    snapshot: Option<BlockSnapshotSlot>,
}

#[component]
fn ParameterTableView(props: ParameterTableViewProps) -> Element {
    let mut search_filter = use_signal(String::new);
    let mut show_only_nonzero = use_signal(|| false);
    let mut collapsed_fx = use_signal(HashSet::<String>::new);

    let Some(ref snapshot) = props.snapshot else {
        return rsx! {
            div { class: "h-full flex items-center justify-center",
                div { class: "text-center px-4",
                    p { class: "text-sm text-muted-foreground mb-1", "No snapshot selected" }
                    p { class: "text-[10px] text-muted-foreground/50",
                        "Select a block preset and snapshot in the Blocks tab to view its parameters"
                    }
                }
            }
        };
    };

    let Some(ref param_snap) = snapshot.parameter_snapshot else {
        return rsx! {
            div { class: "h-full flex items-center justify-center",
                p { class: "text-sm text-muted-foreground", "Snapshot has no parameter data" }
            }
        };
    };

    // Clone data into owned values so closures can be 'static
    let fx_states = param_snap.fx_states.clone();
    let fx_count = fx_states.len();
    // Pre-collect all FX GUIDs for "Collapse All" button
    let all_fx_guids: HashSet<String> = fx_states.iter().map(|s| s.fx_guid.clone()).collect();

    let filter = search_filter.read().to_lowercase();
    let nonzero_only = *show_only_nonzero.read();

    let total_params: usize = fx_states.iter().map(|s| s.parameters.len()).sum();

    let visible_params: usize = fx_states
        .iter()
        .map(|s| {
            s.parameters
                .iter()
                .filter(|p| {
                    let matches_filter =
                        filter.is_empty() || p.param_name.to_lowercase().contains(&filter);
                    let matches_nonzero = !nonzero_only || p.value.abs() > 0.0001;
                    matches_filter && matches_nonzero
                })
                .count()
        })
        .sum();

    let is_daw_connected = daw_control::Daw::try_get().is_some();

    rsx! {
        div { class: "flex-1 flex flex-col min-h-0 overflow-hidden",
            // ── Toolbar ──────────────────────────────────────────
            div { class: "px-3 py-1.5 border-b border-border flex-shrink-0 bg-zinc-950/30 flex items-center gap-2",
                // Search
                div { class: "flex-1 flex items-center gap-1.5",
                    span { class: "text-[10px] text-muted-foreground/50", "Filter:" }
                    input {
                        class: "flex-1 bg-zinc-900 border border-border/50 rounded px-2 py-0.5 text-[10px] text-foreground \
                                outline-none focus:border-primary/50 placeholder:text-muted-foreground/30",
                        r#type: "text",
                        placeholder: "Search parameters...",
                        value: "{search_filter}",
                        oninput: move |evt| search_filter.set(evt.value().clone()),
                    }
                }
                // Toggle: non-zero only
                button {
                    class: if nonzero_only {
                        "text-[9px] px-2 py-0.5 rounded font-medium bg-primary/20 text-primary border border-primary/30"
                    } else {
                        "text-[9px] px-2 py-0.5 rounded font-medium text-muted-foreground hover:text-foreground border border-transparent hover:border-border/50"
                    },
                    title: "Show only parameters with non-zero values",
                    onclick: move |_| show_only_nonzero.set(!nonzero_only),
                    "Non-zero"
                }
                // Collapse / Expand all
                button {
                    class: "text-[9px] px-2 py-0.5 rounded text-muted-foreground hover:text-foreground hover:bg-accent/30",
                    title: "Collapse all FX sections",
                    onclick: move |_| {
                        collapsed_fx.set(all_fx_guids.clone());
                    },
                    "Collapse All"
                }
                button {
                    class: "text-[9px] px-2 py-0.5 rounded text-muted-foreground hover:text-foreground hover:bg-accent/30",
                    title: "Expand all FX sections",
                    onclick: move |_| collapsed_fx.set(HashSet::new()),
                    "Expand All"
                }
                // Summary
                div { class: "flex items-center gap-1.5 pl-2 border-l border-border/30",
                    span { class: "text-[9px] text-muted-foreground",
                        "{fx_count} FX"
                    }
                    span { class: "text-[9px] text-muted-foreground/40", "|" }
                    {
                        let params_text = if visible_params == total_params {
                            format!("{} params", total_params)
                        } else {
                            format!("{}/{}", visible_params, total_params)
                        };
                        rsx! {
                            span { class: "text-[9px] text-muted-foreground", "{params_text}" }
                        }
                    }
                }
            }

            // ── FX groups ────────────────────────────────────────
            div { class: "flex-1 min-h-0 overflow-y-auto",
                for fx_state in fx_states.iter() {
                    {
                        let fx_guid = fx_state.fx_guid.clone();
                        let fx_guid_toggle = fx_guid.clone();
                        let is_collapsed = collapsed_fx.read().contains(&fx_guid);

                        // Filter params for this FX
                        let filtered_params: Vec<&DawParamValue> = fx_state
                            .parameters
                            .iter()
                            .filter(|p| {
                                let matches_filter = filter.is_empty()
                                    || p.param_name.to_lowercase().contains(&filter);
                                let matches_nonzero = !nonzero_only || p.value.abs() > 0.0001;
                                matches_filter && matches_nonzero
                            })
                            .collect();

                        let filtered_count = filtered_params.len();
                        let total_for_fx = fx_state.parameters.len();
                        let fx_guid_display = &fx_state.fx_guid[..8.min(fx_state.fx_guid.len())];

                        rsx! {
                            div { class: "border-b border-border/40",
                                // ── FX Header (clickable to collapse) ────
                                button {
                                    class: "w-full px-3 py-2 bg-zinc-900/60 flex items-center gap-2 \
                                            sticky top-0 z-10 hover:bg-zinc-800/60 transition-colors \
                                            text-left cursor-pointer border-b border-border/20",
                                    onclick: move |_| {
                                        let mut set = collapsed_fx.write();
                                        if set.contains(&fx_guid_toggle) {
                                            set.remove(&fx_guid_toggle);
                                        } else {
                                            set.insert(fx_guid_toggle.clone());
                                        }
                                    },
                                    // Chevron
                                    span { class: "text-[10px] text-muted-foreground/60 w-3 flex-shrink-0 font-mono",
                                        if is_collapsed { "\u{25B6}" } else { "\u{25BC}" }
                                    }
                                    // Plugin name
                                    span { class: "text-[11px] font-semibold text-foreground",
                                        "{fx_state.plugin_name}"
                                    }
                                    // GUID badge
                                    span { class: "text-[8px] text-muted-foreground/40 font-mono",
                                        "{fx_guid_display}"
                                    }
                                    // Index + enable state
                                    span { class: "text-[9px] text-muted-foreground/50",
                                        "#{fx_state.fx_index}"
                                    }
                                    if fx_state.enabled {
                                        span { class: "w-1.5 h-1.5 rounded-full bg-green-500 flex-shrink-0" }
                                    } else {
                                        span { class: "w-1.5 h-1.5 rounded-full bg-red-500/60 flex-shrink-0" }
                                    }
                                    div { class: "flex-1" }
                                    // Param count
                                    {
                                        let count_text = if filtered_count == total_for_fx {
                                            format!("{}", total_for_fx)
                                        } else {
                                            format!("{}/{}", filtered_count, total_for_fx)
                                        };
                                        rsx! {
                                            span { class: "text-[9px] text-muted-foreground/50 bg-zinc-800 px-1.5 py-0.5 rounded",
                                                "{count_text}"
                                            }
                                        }
                                    }
                                }

                                // ── Parameter rows (collapsible) ─────────
                                if !is_collapsed {
                                    div { class: "divide-y divide-border/10",
                                        for param in filtered_params.iter() {
                                            {
                                                let p_idx = param.param_index;
                                                let p_name = param.param_name.clone();
                                                let p_value = param.value;
                                                let p_formatted = param.formatted.clone();
                                                let p_guid = fx_state.fx_guid.clone();
                                                let p_is_toggle = param.is_toggle;
                                                let p_step_count = param.step_count;
                                                let p_step_labels = param.step_labels.clone();
                                                rsx! {
                                                    ParameterRow {
                                                        key: "{p_guid}-{p_idx}",
                                                        fx_guid: p_guid,
                                                        param_index: p_idx,
                                                        param_name: p_name,
                                                        initial_value: p_value,
                                                        initial_formatted: p_formatted,
                                                        daw_connected: is_daw_connected,
                                                        is_toggle: p_is_toggle,
                                                        step_count: p_step_count,
                                                        step_labels: p_step_labels,
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

// ─────────────────────────────────────────────────────────────────────────────
// ParameterRow — single interactive parameter with slider
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct ParameterRowProps {
    fx_guid: String,
    param_index: u32,
    param_name: String,
    initial_value: f64,
    /// Formatted display value from the plugin (e.g., "-12.5 dB")
    initial_formatted: String,
    daw_connected: bool,
    /// Whether this is a toggle/boolean parameter
    is_toggle: bool,
    /// Number of discrete steps (None = continuous, Some(n) = dropdown)
    step_count: Option<u32>,
    /// Labels for discrete steps: (normalized_value, display_label)
    step_labels: Vec<(f64, String)>,
}

#[component]
fn ParameterRow(props: ParameterRowProps) -> Element {
    let mut local_value = use_signal(|| props.initial_value);
    let mut local_formatted = use_signal(|| props.initial_formatted.clone());
    let mut is_dragging = use_signal(|| false);

    // Reset local value when the initial_value prop changes (e.g., different snapshot selected)
    use_effect({
        let initial = props.initial_value;
        let initial_fmt = props.initial_formatted.clone();
        move || {
            if !*is_dragging.peek() {
                local_value.set(initial);
                local_formatted.set(initial_fmt.clone());
            }
        }
    });

    let value = *local_value.read();
    let formatted_display = local_formatted.read().clone();
    let fx_guid = props.fx_guid.clone();
    let param_index = props.param_index;
    let daw_connected = props.daw_connected;
    let is_discrete = props.step_count.is_some() && !props.step_labels.is_empty();

    // Determine which control to render: toggle, dropdown, or slider
    let is_toggle = props.is_toggle;

    rsx! {
        div { class: "group px-3 py-1 flex items-center gap-2 hover:bg-white/[0.02] transition-colors",
            // Index
            span { class: "text-[9px] text-muted-foreground/40 font-mono w-7 text-right flex-shrink-0",
                "{props.param_index}"
            }
            // Name
            span { class: "text-[10px] text-foreground/80 w-36 truncate flex-shrink-0 group-hover:text-foreground transition-colors",
                "{props.param_name}"
            }
            // Control — three modes: toggle checkbox, discrete dropdown, continuous slider
            if is_toggle {
                // ── Toggle: checkbox ──────────────────────────────
                div { class: "flex-1 flex items-center gap-2 min-w-0",
                    {
                        let is_on = value > 0.5;
                        let guid_toggle = fx_guid.clone();
                        rsx! {
                            button {
                                class: if is_on {
                                    "px-3 py-0.5 rounded text-[10px] font-medium bg-primary/20 text-primary border border-primary/30 transition-colors"
                                } else {
                                    "px-3 py-0.5 rounded text-[10px] font-medium text-muted-foreground bg-zinc-800/60 border border-border/30 hover:bg-zinc-700/60 transition-colors"
                                },
                                disabled: !daw_connected,
                                onclick: move |_| {
                                    let new_val = if value > 0.5 { 0.0 } else { 1.0 };
                                    local_value.set(new_val);
                                    let guid = guid_toggle.clone();
                                    let idx = param_index;
                                    spawn(async move {
                                        send_param_value(guid, idx, new_val, local_formatted).await;
                                    });
                                },
                                if is_on { "ON" } else { "OFF" }
                            }
                        }
                    }
                }
            } else if is_discrete {
                // ── Discrete: dropdown ────────────────────────────
                div { class: "flex-1 flex items-center gap-2 min-w-0",
                    {
                        let step_labels = props.step_labels.clone();
                        let guid_select = fx_guid.clone();
                        // Find current selection index (closest step to current value)
                        let current_step_idx = step_labels
                            .iter()
                            .enumerate()
                            .min_by(|(_, a), (_, b)| {
                                let da = (a.0 - value).abs();
                                let db = (b.0 - value).abs();
                                da.partial_cmp(&db).unwrap_or(std::cmp::Ordering::Equal)
                            })
                            .map(|(i, _)| i)
                            .unwrap_or(0);
                        let current_step_str = format!("{}", current_step_idx);
                        rsx! {
                            select {
                                class: "flex-1 bg-zinc-800 border border-border/50 rounded px-2 py-0.5 text-[10px] text-foreground \
                                        outline-none focus:border-primary/50 cursor-pointer appearance-none \
                                        max-w-xs",
                                disabled: !daw_connected,
                                value: current_step_str,
                                onchange: move |evt: FormEvent| {
                                    if let Ok(idx) = evt.value().parse::<usize>() {
                                        if let Some((norm, _)) = step_labels.get(idx) {
                                            let new_val = *norm;
                                            local_value.set(new_val);
                                            let guid = guid_select.clone();
                                            let pidx = param_index;
                                            spawn(async move {
                                                send_param_value(guid, pidx, new_val, local_formatted).await;
                                            });
                                        }
                                    }
                                },
                                for (i, (_norm, label)) in props.step_labels.iter().enumerate() {
                                    option {
                                        value: "{i}",
                                        "{label}"
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                // ── Continuous: slider ────────────────────────────
                {
                    let bar_width = format!("{}%", (value * 100.0).clamp(0.0, 100.0));
                    let bar_color = if value < 0.01 {
                        "bg-zinc-600"
                    } else if value > 0.95 {
                        "bg-amber-500"
                    } else {
                        "bg-blue-500"
                    };
                    let guid_slider = fx_guid.clone();
                    rsx! {
                        div { class: "flex-1 flex items-center gap-2 min-w-0",
                            div { class: "relative flex-1 h-5 flex items-center",
                                // Background track
                                div { class: "absolute inset-x-0 top-1/2 -translate-y-1/2 h-1.5 bg-zinc-800/80 rounded-full overflow-hidden",
                                    div {
                                        class: "h-full {bar_color} rounded-full transition-[width] duration-75",
                                        style: "width: {bar_width}",
                                    }
                                }
                                // Range input overlaid
                                input {
                                    class: "relative w-full h-5 appearance-none bg-transparent cursor-pointer \
                                            [&::-webkit-slider-thumb]:appearance-none \
                                            [&::-webkit-slider-thumb]:w-3 \
                                            [&::-webkit-slider-thumb]:h-3 \
                                            [&::-webkit-slider-thumb]:rounded-full \
                                            [&::-webkit-slider-thumb]:bg-foreground \
                                            [&::-webkit-slider-thumb]:border-2 \
                                            [&::-webkit-slider-thumb]:border-zinc-700 \
                                            [&::-webkit-slider-thumb]:shadow-sm \
                                            [&::-webkit-slider-thumb]:opacity-0 \
                                            [&::-webkit-slider-thumb]:group-hover:opacity-100 \
                                            [&::-webkit-slider-thumb]:transition-opacity \
                                            [&::-webkit-slider-runnable-track]:appearance-none \
                                            [&::-webkit-slider-runnable-track]:bg-transparent \
                                            [&::-webkit-slider-runnable-track]:h-5",
                                    r#type: "range",
                                    min: "0",
                                    max: "10000",
                                    step: "1",
                                    value: format!("{}", (value * 10000.0).round() as i64),
                                    disabled: !daw_connected,
                                    oninput: move |evt: FormEvent| {
                                        if let Ok(raw) = evt.value().parse::<f64>() {
                                            let normalized = (raw / 10000.0).clamp(0.0, 1.0);
                                            local_value.set(normalized);
                                            is_dragging.set(true);
                                            let guid = guid_slider.clone();
                                            let idx = param_index;
                                            spawn(async move {
                                                send_param_value(guid, idx, normalized, local_formatted).await;
                                            });
                                        }
                                    },
                                    onchange: move |_| {
                                        is_dragging.set(false);
                                    },
                                }
                            }
                        }
                    }
                }
            }
            // Value display — show formatted value from plugin, fall back to percentage
            if formatted_display.is_empty() || formatted_display.starts_with("0.") {
                {
                    let pct = (value * 100.0).round();
                    rsx! {
                        span { class: "text-[10px] font-mono text-foreground/70 w-20 text-right flex-shrink-0 tabular-nums truncate",
                            "{pct:.0}%"
                        }
                    }
                }
            } else {
                span { class: "text-[10px] font-mono text-foreground/70 w-20 text-right flex-shrink-0 tabular-nums truncate",
                    title: "{formatted_display}",
                    "{formatted_display}"
                }
            }
        }
    }
}

/// Send a parameter value to REAPER and read back the formatted display.
async fn send_param_value(
    fx_guid: String,
    param_index: u32,
    normalized: f64,
    mut local_formatted: Signal<String>,
) {
    let Some(chain) = get_current_fx_chain().await else {
        warn!("Param set: no FX chain (no track selected?)");
        return;
    };
    match chain.by_guid(&fx_guid).await {
        Ok(Some(handle)) => {
            let param = handle.param(param_index);
            if let Err(e) = param.set(normalized).await {
                warn!("Param set failed: {e}");
            } else {
                if let Ok(fmt) = param.formatted().await {
                    local_formatted.set(fmt);
                }
            }
        }
        Ok(None) => {
            warn!(
                "Param set: FX not found for GUID {}",
                &fx_guid[..8.min(fx_guid.len())]
            );
        }
        Err(e) => {
            warn!("Param set: by_guid error: {e}");
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Chunk Data View
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct ChunkDataViewProps {
    preset: Option<BlockPresetSlot>,
}

#[component]
fn ChunkDataView(props: ChunkDataViewProps) -> Element {
    let Some(ref preset) = props.preset else {
        return rsx! {
            div { class: "h-full flex items-center justify-center",
                div { class: "text-center px-4",
                    p { class: "text-sm text-muted-foreground mb-1", "No preset selected" }
                    p { class: "text-[10px] text-muted-foreground/50",
                        "Select a block preset in the Blocks tab to view its chunk data"
                    }
                }
            }
        };
    };

    let Some(ref chunk_json) = preset.chunk_data else {
        return rsx! {
            div { class: "h-full flex items-center justify-center",
                div { class: "text-center px-4",
                    p { class: "text-sm text-muted-foreground mb-1", "No chunk data" }
                    p { class: "text-[10px] text-muted-foreground/50",
                        "This preset was captured without state chunk data"
                    }
                }
            }
        };
    };

    let chunk_len = chunk_json.len();
    let chunk_display = chunk_json.clone();

    rsx! {
        div { class: "flex-1 flex flex-col min-h-0 overflow-hidden",
            // Summary bar
            div { class: "px-4 py-2 border-b border-border flex-shrink-0 bg-zinc-950/30",
                div { class: "flex items-center gap-4",
                    span { class: "text-[10px] text-muted-foreground",
                        "Preset: \"{preset.name}\""
                    }
                    span { class: "text-[10px] text-muted-foreground",
                        "{chunk_len} bytes (serialized)"
                    }
                    if !preset.source_fx_guids.is_empty() {
                        {
                            let guids_display = preset.source_fx_guids.iter()
                                .map(|g| &g[..8.min(g.len())])
                                .collect::<Vec<_>>()
                                .join(", ");
                            rsx! {
                                span { class: "text-[9px] text-muted-foreground/50 font-mono",
                                    "Source FX: {guids_display}"
                                }
                            }
                        }
                    }
                }
            }

            // Raw chunk display
            div { class: "flex-1 overflow-y-auto min-h-0 p-4",
                pre { class: "text-[10px] font-mono text-zinc-300 whitespace-pre-wrap break-all leading-relaxed \
                              bg-zinc-950 rounded-lg border border-border p-4 select-all",
                    "{chunk_display}"
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Snapshot Diff View
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct SnapshotDiffViewProps {
    preset: Option<BlockPresetSlot>,
}

#[component]
fn SnapshotDiffView(props: SnapshotDiffViewProps) -> Element {
    let mut diff_from = use_signal::<Option<Uuid>>(|| None);
    let mut diff_to = use_signal::<Option<Uuid>>(|| None);

    let Some(ref preset) = props.preset else {
        return rsx! {
            div { class: "h-full flex items-center justify-center",
                div { class: "text-center px-4",
                    p { class: "text-sm text-muted-foreground mb-1", "No preset selected" }
                    p { class: "text-[10px] text-muted-foreground/50",
                        "Select a block preset with multiple snapshots to compare"
                    }
                }
            }
        };
    };

    if preset.snapshots.len() < 2 {
        return rsx! {
            div { class: "h-full flex items-center justify-center",
                div { class: "text-center px-4",
                    p { class: "text-sm text-muted-foreground mb-1", "Need at least 2 snapshots to diff" }
                    p { class: "text-[10px] text-muted-foreground/50",
                        "Capture more snapshots in the Blocks tab, then compare them here"
                    }
                }
            }
        };
    }

    // Auto-select first two snapshots if none selected
    let from_id = diff_from.read().unwrap_or(preset.snapshots[0].id);
    let to_id = diff_to.read().unwrap_or(
        preset
            .snapshots
            .get(1)
            .map(|s| s.id)
            .unwrap_or(preset.snapshots[0].id),
    );

    let from_snap = preset.snapshots.iter().find(|s| s.id == from_id);
    let to_snap = preset.snapshots.iter().find(|s| s.id == to_id);

    // Compute diff
    let changes = match (from_snap, to_snap) {
        (Some(from), Some(to)) => match (&from.parameter_snapshot, &to.parameter_snapshot) {
            (Some(from_params), Some(to_params)) => {
                daw_bridge::diff_parameter_snapshots(from_params, to_params)
            }
            _ => vec![],
        },
        _ => vec![],
    };

    let change_count = changes.len();

    rsx! {
        div { class: "flex-1 flex flex-col min-h-0 overflow-hidden",
            // Snapshot selectors
            div { class: "px-4 py-2 border-b border-border flex-shrink-0 bg-zinc-950/30",
                div { class: "flex items-center gap-3",
                    // From selector
                    div { class: "flex items-center gap-1.5",
                        span { class: "text-[10px] text-muted-foreground font-semibold", "From:" }
                        select {
                            class: "bg-zinc-800 border border-border rounded px-2 py-1 text-[10px] text-foreground outline-none",
                            value: "{from_id}",
                            onchange: move |evt| {
                                if let Ok(id) = Uuid::parse_str(&evt.value()) {
                                    diff_from.set(Some(id));
                                }
                            },
                            for snap in preset.snapshots.iter() {
                                {
                                    let sid = snap.id;
                                    let name = snap.name.clone();
                                    rsx! {
                                        option { value: "{sid}", "{name}" }
                                    }
                                }
                            }
                        }
                    }
                    span { class: "text-muted-foreground/40", "→" }
                    // To selector
                    div { class: "flex items-center gap-1.5",
                        span { class: "text-[10px] text-muted-foreground font-semibold", "To:" }
                        select {
                            class: "bg-zinc-800 border border-border rounded px-2 py-1 text-[10px] text-foreground outline-none",
                            value: "{to_id}",
                            onchange: move |evt| {
                                if let Ok(id) = Uuid::parse_str(&evt.value()) {
                                    diff_to.set(Some(id));
                                }
                            },
                            for snap in preset.snapshots.iter() {
                                {
                                    let sid = snap.id;
                                    let name = snap.name.clone();
                                    rsx! {
                                        option { value: "{sid}", "{name}" }
                                    }
                                }
                            }
                        }
                    }
                    // Summary
                    div { class: "flex-1" }
                    if change_count > 0 {
                        {
                            let plural = if change_count != 1 { "s" } else { "" };
                            rsx! {
                                span { class: "text-[10px] text-amber-300 font-medium",
                                    "{change_count} parameter{plural} changed"
                                }
                            }
                        }
                    } else {
                        span { class: "text-[10px] text-green-400 font-medium",
                            "Snapshots are identical"
                        }
                    }
                }
            }

            // Diff table
            if changes.is_empty() {
                div { class: "flex-1 flex items-center justify-center",
                    p { class: "text-sm text-muted-foreground",
                        if from_id == to_id { "Same snapshot selected for both sides" } else { "No parameter differences detected" }
                    }
                }
            } else {
                div { class: "flex-1 overflow-y-auto min-h-0",
                    // Table header
                    div { class: "px-4 py-1.5 grid grid-cols-[1fr_3rem_5rem_5rem_5rem_1fr] gap-2 text-[9px] text-muted-foreground/60 border-b border-border/30 sticky top-0 bg-card",
                        span { "Parameter" }
                        span { "#" }
                        span { class: "text-right", "From" }
                        span { class: "text-right", "To" }
                        span { class: "text-right", "Delta" }
                        span { "Visual" }
                    }
                    // Diff rows
                    for change in changes.iter() {
                        {
                            let from_pct = (change.from_value * 100.0).round();
                            let to_pct = (change.to_value * 100.0).round();
                            let delta = change.to_value - change.from_value;
                            let delta_pct = (delta * 100.0).round();
                            let delta_sign = if delta > 0.0 { "+" } else { "" };
                            let delta_color = if delta.abs() > 0.2 {
                                "text-red-400"
                            } else if delta.abs() > 0.05 {
                                "text-amber-400"
                            } else {
                                "text-zinc-400"
                            };
                            let magnitude = change.magnitude();

                            // Visual: show from→to as two bars
                            let from_width = format!("{}%", (change.from_value * 100.0).clamp(0.0, 100.0));
                            let to_width = format!("{}%", (change.to_value * 100.0).clamp(0.0, 100.0));

                            rsx! {
                                div { class: "px-4 py-0.5 grid grid-cols-[1fr_3rem_5rem_5rem_5rem_1fr] gap-2 text-[10px] hover:bg-accent/20 transition-colors",
                                    span { class: "text-foreground truncate", "{change.param_name}" }
                                    span { class: "text-muted-foreground/50 font-mono", "{change.param_index}" }
                                    span { class: "text-right font-mono text-zinc-400", "{from_pct:.0}%" }
                                    span { class: "text-right font-mono text-foreground", "{to_pct:.0}%" }
                                    span { class: "text-right font-mono {delta_color}", "{delta_sign}{delta_pct:.0}%" }
                                    // Visual comparison bars
                                    div { class: "flex flex-col gap-0.5 justify-center",
                                        div { class: "h-1 bg-zinc-800 rounded-full overflow-hidden",
                                            div {
                                                class: "h-full bg-zinc-500 rounded-full",
                                                style: "width: {from_width}",
                                            }
                                        }
                                        div { class: "h-1 bg-zinc-800 rounded-full overflow-hidden",
                                            div {
                                                class: "h-full bg-blue-500 rounded-full",
                                                style: "width: {to_width}",
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
