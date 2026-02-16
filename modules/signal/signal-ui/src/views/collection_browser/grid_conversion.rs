//! Grid slot conversion and the `RigGridPanel` wrapper component.
//!
//! Converts domain hierarchy data (`EngineFlowData`, `ModuleChainData`,
//! `SignalChain`) into flat `Vec<GridSlot>` for the `DynamicGridView`.

use std::collections::HashMap;

use dioxus::prelude::*;
use signal::SignalChain;

use super::inspector::BlockInspectorPanel;
use super::types::{EngineFlowData, ModuleChainData};
use crate::components::dynamic_grid::{
    BlockPickerDropdown, DynamicGridView, GridConnection as DynGridConnection, GridSelection,
    GridSlot, PICKER_CELL, PICKER_CLICK_POS,
};

/// Pre-resolved block parameters keyed by `(preset_id, snapshot_id)`.
/// Built during async data fetching, passed into synchronous grid conversion.
pub(super) type ParamLookup = HashMap<(String, String), Vec<(String, f32)>>;

/// Extract parameters for a `ModuleBlock`.
///
/// 1. For `Inline { block }` sources, read parameters directly.
/// 2. For `PresetSnapshot` sources, look up in the pre-resolved map.
/// 3. For `PresetDefault` sources, look up with snapshot_id = "default".
/// 4. Apply any overrides on top.
fn extract_block_params(mb: &signal::ModuleBlock, lookup: &ParamLookup) -> Vec<(String, f32)> {
    let mut params: Vec<(String, f32)> = match mb.source() {
        signal::ModuleBlockSource::Inline { block } => block
            .parameters()
            .iter()
            .map(|p| (p.name().to_string(), p.value().get()))
            .collect(),
        signal::ModuleBlockSource::PresetSnapshot {
            preset_id,
            snapshot_id,
            ..
        } => lookup
            .get(&(preset_id.to_string(), snapshot_id.to_string()))
            .cloned()
            .unwrap_or_default(),
        signal::ModuleBlockSource::PresetDefault { preset_id, .. } => lookup
            .get(&(preset_id.to_string(), "default".to_string()))
            .cloned()
            .unwrap_or_default(),
    };
    // Apply overrides
    for ov in mb.overrides() {
        if let Some(p) = params
            .iter_mut()
            .find(|(name, _)| name == ov.parameter_id())
        {
            p.1 = ov.value().get();
        }
    }
    params
}

// region: --- Constants

/// Preferred max columns before wrapping a module to the next row band.
const SOFT_MAX_COLS: usize = 14;

/// Gap rows when a module wraps within a layer (needs space for split fan-out).
const ROW_BAND_STRIDE: usize = 2;

/// Gap rows between layers/engines (just enough for container title + breathing room).
const LAYER_GAP: usize = 1;

// endregion: --- Constants

// region: --- Converters

/// Flatten the full rig hierarchy (engines → layers → modules → blocks)
/// into a single `Vec<GridSlot>` for the interactive `DynamicGridView`.
///
/// Layout strategy (matching legacy `unified_grid_editor`):
///  - Modules flow left-to-right across the row band
///  - A module is **never split** across rows — if it won't fit in the
///    remaining columns, the entire module wraps to the next row band
///  - Row bands are separated by `ROW_BAND_STRIDE` rows (2 empty gap rows)
///  - Split nodes fan out vertically within the module's row band
pub(super) fn engines_to_grid_slots(
    engines: &[EngineFlowData],
    params: &ParamLookup,
) -> Vec<GridSlot> {
    let mut slots = Vec::new();
    let mut row: usize = 0;

    for engine in engines {
        let engine_key = engine.name.clone();

        // Two-pass layout: measure each layer, then pack them.
        // Pre-compute each layer's dimensions by laying it out into a
        // temporary slot list at origin (0,0).
        struct LayerMeasure {
            width: usize,  // max col + 1
            height: usize, // row count (accounts for split fan-out + wrapping)
        }

        let mut layer_measures: Vec<LayerMeasure> = Vec::new();
        for layer in &engine.layers {
            let mut temp_slots = Vec::new();
            let mut temp_col: usize = 0;
            let temp_row: usize = 0;
            let mut temp_base_row = temp_row;
            for mc in &layer.module_chains {
                let module_width = count_chain_width(mc.chain.nodes());
                if temp_col > 0 && temp_col + module_width > SOFT_MAX_COLS {
                    temp_col = 0;
                    temp_base_row += ROW_BAND_STRIDE;
                }
                let mut col_cursor = temp_col;
                flatten_chain_nodes(
                    mc.chain.nodes(),
                    "measure",
                    None,
                    None,
                    None,
                    &mut col_cursor,
                    temp_base_row,
                    &mut temp_slots,
                    params,
                );
                temp_col = col_cursor;
            }
            let max_col = temp_slots.iter().map(|s| s.col).max().unwrap_or(0);
            let max_row = temp_slots.iter().map(|s| s.row).max().unwrap_or(0);
            layer_measures.push(LayerMeasure {
                width: max_col + 1,
                height: max_row + 1,
            });
        }

        // Pack layers left-to-right, wrapping when a layer won't fit.
        let mut col: usize = 0;
        let mut band_start_row = row;
        let mut band_max_height: usize = 0;

        for (li, layer) in engine.layers.iter().enumerate() {
            let layer_key = format!("{}/{}", engine.name, layer.name);
            let measure = &layer_measures[li];

            // Wrap to next row band if this layer won't fit horizontally.
            if col > 0 && col + measure.width > SOFT_MAX_COLS {
                // Advance past the tallest layer in the current band.
                band_start_row += band_max_height + LAYER_GAP;
                band_max_height = 0;
                col = 0;
            }

            // Place this layer's modules starting at (col, band_start_row).
            let layer_base_row = band_start_row;
            let mut layer_col = col;
            let mut layer_row = layer_base_row;

            for mc in &layer.module_chains {
                let module_key = format!("{}/{}/{}", engine.name, layer.name, mc.name);
                let mt = mc.module_type;
                let module_width = count_chain_width(mc.chain.nodes());

                if layer_col > col && layer_col + module_width > col + SOFT_MAX_COLS {
                    layer_col = col;
                    layer_row += ROW_BAND_STRIDE;
                }

                let mut col_cursor = layer_col;
                flatten_chain_nodes(
                    mc.chain.nodes(),
                    &module_key,
                    Some(&layer_key),
                    Some(&engine_key),
                    mt,
                    &mut col_cursor,
                    layer_row,
                    &mut slots,
                    params,
                );
                layer_col = col_cursor;
            }

            // Use pre-measured height (from dry-run) for consistent packing.
            band_max_height = band_max_height.max(measure.height);

            // Advance col past this layer for the next one.
            col = col + measure.width + 1; // +1 col gap between side-by-side layers
        }

        // Advance row past this engine for the next one.
        row = band_start_row + band_max_height + LAYER_GAP;
    }

    slots
}

/// Convert a list of module chains into grid slots for `DynamicGridView`.
/// Used for Engine/Layer detail where we show the module chains without
/// the full rig hierarchy.
pub(super) fn module_chains_to_grid_slots(
    chains: &[ModuleChainData],
    params: &ParamLookup,
) -> Vec<GridSlot> {
    let mut slots = Vec::new();
    let mut col: usize = 0;
    let mut row: usize = 0;

    for mc in chains {
        let module_key = mc.name.clone();
        let mt = mc.module_type;
        let module_width = count_chain_width(mc.chain.nodes());

        if col > 0 && col + module_width > SOFT_MAX_COLS {
            col = 0;
            row += ROW_BAND_STRIDE;
        }

        let mut col_cursor = col;
        flatten_chain_nodes(
            mc.chain.nodes(),
            &module_key,
            None,
            None,
            mt,
            &mut col_cursor,
            row,
            &mut slots,
            params,
        );
        col = col_cursor;
    }
    slots
}

/// Convert a single signal chain into grid slots for `DynamicGridView`.
/// Used for Module snapshot detail.
pub(super) fn signal_chain_to_grid_slots(
    chain: &SignalChain,
    module_name: &str,
    module_type: Option<signal::ModuleType>,
    params: &ParamLookup,
) -> Vec<GridSlot> {
    let mut slots = Vec::new();
    let mut col_cursor = 0;
    flatten_chain_nodes(
        chain.nodes(),
        module_name,
        None,
        None,
        module_type,
        &mut col_cursor,
        0,
        &mut slots,
        params,
    );
    slots
}

/// Count the number of columns a chain of nodes needs (for wrapping decisions).
fn count_chain_width(nodes: &[signal::SignalNode]) -> usize {
    let mut width = 0;
    for node in nodes {
        match node {
            signal::SignalNode::Block(_) => width += 1,
            signal::SignalNode::Split { lanes } => {
                // A split's width is the max width among its lanes.
                // Empty lanes get a 1-col pass-through placeholder.
                let max_lane_width = lanes
                    .iter()
                    .map(|lane| {
                        if lane.is_empty() {
                            1
                        } else {
                            count_chain_width(lane.nodes())
                        }
                    })
                    .max()
                    .unwrap_or(0);
                width += max_lane_width;
            }
        }
    }
    width
}

/// Recursively flatten SignalNodes into GridSlots, handling splits.
fn flatten_chain_nodes(
    nodes: &[signal::SignalNode],
    module_key: &str,
    layer_key: Option<&str>,
    engine_key: Option<&str>,
    module_type: Option<signal::ModuleType>,
    col_cursor: &mut usize,
    base_row: usize,
    slots: &mut Vec<GridSlot>,
    param_lookup: &ParamLookup,
) {
    for node in nodes {
        match node {
            signal::SignalNode::Block(mb) => {
                // Extract parameters from the block source when available.
                let parameters = extract_block_params(mb, param_lookup);
                slots.push(GridSlot {
                    id: uuid::Uuid::new_v4(),
                    block_type: mb.block_type(),
                    block_preset_name: Some(mb.label().to_string()),
                    plugin_name: None,
                    col: *col_cursor,
                    row: base_row,
                    module_group: Some(module_key.to_string()),
                    module_type,
                    layer_group: layer_key.map(|s| s.to_string()),
                    engine_group: engine_key.map(|s| s.to_string()),
                    is_template: false,
                    bypassed: false,
                    is_phantom: false,
                    parameters,
                });
                *col_cursor += 1;
            }
            signal::SignalNode::Split { lanes } => {
                // Fan-out: each lane gets its own row, all starting at the same col.
                // Reorder so empty (dry/pass-through) lanes go in the middle
                // and wet lanes are at top and bottom.
                let split_start_col = *col_cursor;
                let mut max_col = split_start_col;

                let mut wet: Vec<&signal::SignalChain> = Vec::new();
                let mut dry: Vec<&signal::SignalChain> = Vec::new();
                for lane in lanes.iter() {
                    if lane.is_empty() {
                        dry.push(lane);
                    } else {
                        wet.push(lane);
                    }
                }
                // Layout order: first half of wet, then all dry, then second half of wet
                let mid = (wet.len() + 1) / 2;
                let mut ordered: Vec<&signal::SignalChain> = Vec::new();
                ordered.extend_from_slice(&wet[..mid]);
                ordered.extend_from_slice(&dry);
                ordered.extend_from_slice(&wet[mid..]);

                // Vertically center: dry lane sits at base_row, wet lanes
                // fan out above and below. For 3 lanes: offset=1, rows are
                // base_row-1 (top wet), base_row (dry), base_row+1 (bottom wet).
                let total_lanes = ordered.len();
                let vert_offset = (total_lanes.saturating_sub(1)) / 2;

                for (i, lane) in ordered.iter().enumerate() {
                    let lane_row = (base_row + i).saturating_sub(vert_offset);
                    let mut lane_col = split_start_col;
                    if lane.is_empty() {
                        // Empty lane = dry pass-through. Create a phantom
                        // slot so the module group bounding box includes
                        // this row, but it won't render a visible cell.
                        slots.push(GridSlot {
                            id: uuid::Uuid::new_v4(),
                            block_type: signal::BlockType::Send,
                            block_preset_name: None,
                            plugin_name: None,
                            col: lane_col,
                            row: lane_row,
                            module_group: Some(module_key.to_string()),
                            module_type,
                            layer_group: layer_key.map(|s| s.to_string()),
                            engine_group: engine_key.map(|s| s.to_string()),
                            is_template: false,
                            bypassed: false,
                            is_phantom: true,
                            parameters: Vec::new(),
                        });
                        lane_col += 1;
                    } else {
                        flatten_chain_nodes(
                            lane.nodes(),
                            module_key,
                            layer_key,
                            engine_key,
                            module_type,
                            &mut lane_col,
                            lane_row,
                            slots,
                            param_lookup,
                        );
                    }
                    if lane_col > max_col {
                        max_col = lane_col;
                    }
                }
                *col_cursor = max_col;
            }
        }
    }
}

// endregion: --- Converters

// region: --- RigGridPanel

#[derive(Props, Clone, PartialEq)]
pub(super) struct RigGridPanelProps {
    pub initial_slots: Vec<GridSlot>,
}

/// Stateful wrapper around `DynamicGridView` + `BlockPickerDropdown`.
///
/// Owns local signals for chain, selection, and connections so the
/// detail panel can render an interactive grid without lifting state further.
#[component]
pub(super) fn RigGridPanel(props: RigGridPanelProps) -> Element {
    let mut chain = use_signal(|| props.initial_slots.clone());
    let mut selection = use_signal(|| Option::<GridSelection>::None);
    let mut connections = use_signal(Vec::<DynGridConnection>::new);

    // Sync when the parent passes new data (e.g. user selects a different preset)
    use_effect(move || {
        chain.set(props.initial_slots.clone());
        selection.set(None);
        connections.set(Vec::new());
    });

    let picker_cell = PICKER_CELL();
    let picker_pos = PICKER_CLICK_POS();

    let current_chain = chain();
    let current_sel = selection();

    rsx! {
        div {
            class: "flex-1 min-h-0",
            DynamicGridView {
                chain: current_chain.clone(),
                selection: current_sel.clone(),
                connections: connections(),
                on_chain_change: move |new_chain: Vec<GridSlot>| {
                    chain.set(new_chain);
                },
                on_connections_change: move |new_conns: Vec<DynGridConnection>| {
                    connections.set(new_conns);
                },
                on_select: move |sel: Option<GridSelection>| {
                    selection.set(sel);
                },
            }
        }
        // Block picker rendered outside the transform context
        if let Some((col, row)) = picker_cell {
            BlockPickerDropdown {
                col: col,
                row: row,
                click_x: picker_pos.0,
                click_y: picker_pos.1,
                on_add_slot: move |slot: GridSlot| {
                    let mut current = chain();
                    current.push(slot);
                    chain.set(current);
                    *PICKER_CELL.write() = None;
                },
                on_close: move |_| {
                    *PICKER_CELL.write() = None;
                },
            }
        }
        // Inspector panel for selected block / module
        BlockInspectorPanel {
            selection: current_sel,
            chain: current_chain,
        }
    }
}

// endregion: --- RigGridPanel
