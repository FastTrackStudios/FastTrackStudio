//! Unified Grid Editor — single editing surface for blocks and modules.
//!
//! Owns local signals for composition chain, connections, and selection.
//! Renders the DynamicGridView with the block picker portal and the
//! DetailPanel below the grid for editing the selected block/module.

use super::detail_panel::DetailPanel;
use super::grid_view::{
    BlockPickerDropdown, DynamicGridView, GridConnection, PICKER_CELL, PICKER_CLICK_POS,
};
use super::module_editor_view::CompositionSlot;
use crate::prelude::*;
use crate::signals::RIG_MODULES;
use uuid::Uuid;

/// Max columns before blocks wrap to the next row.
/// Preferred max columns before wrapping. May be exceeded to keep a module intact.
const SOFT_MAX_COLS: usize = 14;

/// Gap between row bands — 1 empty row for cable routing. 2D modules expand
/// vertically above and below the baseline using vertical centering.
const ROW_BAND_STRIDE: usize = 3;

/// Convert the current RIG_MODULES into CompositionSlots for the grid view.
///
/// Rules:
/// - **A module is never split across rows.** If it won't fit in the remaining
///   columns, the entire module wraps to the next row band.
/// - Row bands are separated by `ROW_BAND_STRIDE` rows (2 empty gap rows).
/// - Linear modules lay out blocks left-to-right within a single row.
/// - 2D modules (with `grid_width`/`grid_height`) place blocks at their
///   `local_col`/`local_row` positions, vertically centered on the row band
///   so the middle row aligns with linear modules.
/// - Cables only connect blocks within the same module. Inter-module wiring
///   is handled separately by the cable resolver.
pub(crate) fn modules_to_composition_chain(
    modules: &[signal_control::module::Module],
) -> Vec<CompositionSlot> {
    let mut slots = Vec::new();
    let mut col: usize = 0;
    let mut row: usize = 0;

    for m in modules {
        let group_key = m.module_type.display_name().to_string();
        let has_2d = m.grid_width.is_some();

        // Compute this module's width (columns it needs)
        let module_width = if has_2d {
            m.grid_width.unwrap_or(1)
        } else {
            m.blocks.len().max(1)
        };

        // Wrap to next row band if the module won't fit (never split a module)
        if col > 0 && col + module_width > SOFT_MAX_COLS {
            col = 0;
            row += ROW_BAND_STRIDE;
        }

        if m.blocks.is_empty() {
            slots.push(CompositionSlot {
                id: m.id.as_uuid(),
                block_type: signal_control::block::BlockType::Custom,
                block_preset_id: None,
                block_preset_name: Some(m.name.clone()),
                plugin_name: None,
                col,
                row,
                module_group: Some(group_key.clone()),
                module_type: Some(m.module_type),
            });
            col += 1;
        } else if has_2d {
            // 2D module: blocks at local_col/local_row, vertically centered
            let gh = m.grid_height.unwrap_or(1);

            // Center the module vertically: offset so the middle row of the
            // module aligns with the row band's baseline (row 0 of the band).
            // For a 3-row module, the middle row (1) should be at `row`,
            // so the top row goes at `row - 1` (clamped to 0).
            let vert_offset = if gh > 1 { (gh - 1) / 2 } else { 0 };
            let base_row = row.saturating_sub(vert_offset);
            let base_col = col;

            for mb in m.blocks.iter() {
                let lc = mb.local_col.unwrap_or(0);
                let lr = mb.local_row.unwrap_or(0);
                slots.push(CompositionSlot {
                    id: mb.id,
                    block_type: mb.block.block_type,
                    block_preset_id: None,
                    block_preset_name: Some(mb.block.name.clone()),
                    plugin_name: mb.block.alias.clone(),
                    col: base_col + lc,
                    row: base_row + lr,
                    module_group: Some(group_key.clone()),
                    module_type: Some(m.module_type),
                });
            }

            col = base_col + module_width;
        } else {
            // Linear module: blocks flow left-to-right, all on the same row
            let base_col = col;
            for (i, mb) in m.blocks.iter().enumerate() {
                slots.push(CompositionSlot {
                    id: mb.id,
                    block_type: mb.block.block_type,
                    block_preset_id: None,
                    block_preset_name: Some(mb.block.name.clone()),
                    plugin_name: mb.block.alias.clone(),
                    col: base_col + i,
                    row,
                    module_group: Some(group_key.clone()),
                    module_type: Some(m.module_type),
                });
            }
            col = base_col + module_width;
        }
    }

    slots
}

/// Unified grid editor for the Edit tab's Grid view mode.
///
/// Reads directly from `RIG_MODULES` to build the composition chain.
/// The grid updates reactively whenever the loaded preset changes.
#[component]
pub fn UnifiedGridEditor() -> Element {
    let mut connections = use_signal(Vec::<GridConnection>::new);
    let mut selected_slot_id = use_signal(|| None::<Uuid>);
    // Local chain override: stores user-driven layout changes (drag-drop moves).
    // Cleared whenever the upstream RIG_MODULES data changes (new preset loaded).
    let mut chain_override = use_signal(|| None::<Vec<CompositionSlot>>);

    // Build chain directly from RIG_MODULES — reactive via signal read
    let modules = RIG_MODULES.read();
    let base_chain = modules_to_composition_chain(&modules);
    let module_count = modules.len();
    drop(modules); // release read guard

    // Use the local override if it exists, otherwise use the computed chain
    let chain_data = chain_override
        .read()
        .clone()
        .unwrap_or_else(|| base_chain.clone());
    let slot_count = chain_data.len();

    tracing::info!(
        "UnifiedGridEditor: rendering {} modules → {} grid slots",
        module_count,
        slot_count
    );

    let conn_data = connections.cloned();
    let sel_id = *selected_slot_id.read();

    rsx! {
        div { class: "h-full w-full flex flex-col overflow-hidden",
            // Debug: show slot count so we know if data is flowing
            if slot_count == 0 {
                div { class: "absolute top-2 left-1/2 -translate-x-1/2 z-50 px-3 py-1 rounded bg-red-900/80 text-red-200 text-xs font-mono",
                    "No grid data — {module_count} modules, 0 slots"
                }
            }

            // Grid area (fills available space)
            div { class: "flex-1 min-h-0 relative",
                DynamicGridView {
                    chain: chain_data.clone(),
                    selected_slot_id: sel_id,
                    connections: conn_data.clone(),
                    on_chain_change: move |new_chain: Vec<CompositionSlot>| {
                        chain_override.set(Some(new_chain));
                    },
                    on_connections_change: move |new_conns: Vec<GridConnection>| {
                        connections.set(new_conns);
                    },
                    on_select: move |id: Option<Uuid>| {
                        selected_slot_id.set(id);
                    },
                }
            }

            // Detail panel — 3-column layout for editing the selected slot
            {
                let selected_slot = sel_id.and_then(|id| chain_data.iter().find(|s| s.id == id).cloned());
                rsx! {
                    div { class: "h-52 flex-shrink-0 border-t border-zinc-800/50 bg-zinc-950/40",
                        DetailPanel { selected_slot: selected_slot }
                    }
                }
            }

            // Block picker portal — rendered above CSS transform stacking context
            if let Some((pc, pr)) = *PICKER_CELL.read() {
                {
                    let (click_x, click_y) = *PICKER_CLICK_POS.read();
                    rsx! {
                        BlockPickerDropdown {
                            col: pc,
                            row: pr,
                            click_x: click_x,
                            click_y: click_y,
                            on_add_slot: move |_new_slot: CompositionSlot| {
                                // Adding slots from picker not yet wired to RIG_MODULES
                                *PICKER_CELL.write() = None;
                            },
                            on_close: move |_| {
                                *PICKER_CELL.write() = None;
                            },
                        }
                    }
                }
            }
        }
    }
}
