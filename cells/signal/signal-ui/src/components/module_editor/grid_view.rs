//! Dynamic Grid View — auto-expanding 2D grid for block composition.
//!
//! Renders CompositionSlots on a CSS grid that grows as blocks are added.
//! Empty cells show a faint dashed border with hover highlight and click-to-add
//! via a searchable block type picker dropdown.
//!
//! Occupied cells have connection ports on left/right edges. Adjacent occupied
//! cells are linked by SVG Bézier cables rendered in an overlay layer.

use crate::components::block_editor::library::{
    block_type_categories, predefined_block_types, BlockTypeDefinition,
};
use crate::components::rig_grid::block_colors::block_type_color;
use crate::prelude::*;
use dioxus::html::input_data::MouseButton;
use dioxus::prelude::dioxus_elements::geometry::WheelDelta;
use std::rc::Rc;
use uuid::Uuid;

use super::module_editor_view::CompositionSlot;

// ─────────────────────────────────────────────────────────────────────────────
// Drag state
// ─────────────────────────────────────────────────────────────────────────────

/// Tracks an in-progress block drag on the grid.
#[derive(Debug, Clone, PartialEq)]
struct GridDragState {
    /// Which slot is being dragged.
    slot_id: Uuid,
    /// Original grid position before drag started.
    origin_col: usize,
    origin_row: usize,
    /// Mouse position at drag start (client coords).
    start_mouse_x: f64,
    start_mouse_y: f64,
    /// Current mouse position (client coords).
    mouse_x: f64,
    mouse_y: f64,
}

/// Tracks an in-progress wire connection from a port.
#[derive(Debug, Clone, PartialEq)]
struct GridWireDraft {
    /// Which slot the wire originates from.
    from_slot_id: Uuid,
    /// Pixel position of the source port center (in grid-local coords).
    from_pos: (f64, f64),
    /// Whether the draft started from an output (right) port.
    is_from_output: bool,
    /// Current mouse position in grid-local coords.
    mouse_pos: (f64, f64),
}

/// Tracks an in-progress module group drag on the grid.
#[derive(Debug, Clone, PartialEq)]
struct GroupDragState {
    /// Name of the module group being dragged.
    group_name: String,
    /// Mouse position at drag start (client coords).
    start_mouse_x: f64,
    start_mouse_y: f64,
    /// Current mouse position (client coords).
    mouse_x: f64,
    mouse_y: f64,
    /// Whether shift is held (insert mode — shifts blocks instead of swapping).
    shift_held: bool,
}

/// Where a dragged group would land if dropped.
#[derive(Debug, Clone, PartialEq)]
enum GroupDropTarget {
    /// Swap with another module group.
    SwapWith(String),
    /// Move by a grid-cell delta (col_delta, row_delta) — always grid-aligned.
    MoveDelta(isize, isize),
}

/// An explicit connection between two blocks.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct GridConnection {
    pub from_slot_id: Uuid,
    pub to_slot_id: Uuid,
}

/// Explicit connections between blocks. When empty, cables fall back to
/// adjacency-based auto-wiring via `resolve_cables()`.
pub(crate) static GRID_CONNECTIONS: GlobalSignal<Vec<GridConnection>> = Signal::global(Vec::new);

// ─────────────────────────────────────────────────────────────────────────────
// Block picker portal state (hoisted above the transform chain)
// ─────────────────────────────────────────────────────────────────────────────

/// Which grid cell (col, row) has the block picker open.
/// Set by DynamicGridView, rendered by the parent component (module_editor_view)
/// to escape the CSS transform stacking context.
pub(crate) static PICKER_CELL: GlobalSignal<Option<(usize, usize)>> = Signal::global(|| None);

/// Click position (client coords) for positioning the picker dropdown.
pub(crate) static PICKER_CLICK_POS: GlobalSignal<(f64, f64)> = Signal::global(|| (0.0, 0.0));

// ─────────────────────────────────────────────────────────────────────────────
// Grid sizing
// ─────────────────────────────────────────────────────────────────────────────

/// Fixed cell size in CSS px (square 1:1).
pub(crate) const CELL_SIZE: usize = 88;
/// Gap between cells in CSS px — wide enough for cables + port circles.
pub(crate) const CELL_GAP: usize = 32;
/// Diameter of connection port circles.
pub(crate) const PORT_SIZE: f64 = 10.0;

/// Minimum grid dimensions when the chain is empty.
const MIN_COLS: usize = 4;
const MIN_ROWS: usize = 1;

/// Compute the visible grid bounds from the current chain.
/// Returns (cols, rows) — always shows at least 1 extra col/row beyond content.
pub(crate) fn compute_grid_bounds(chain: &[CompositionSlot]) -> (usize, usize) {
    if chain.is_empty() {
        return (MIN_COLS, MIN_ROWS);
    }
    let max_col = chain.iter().map(|s| s.col).max().unwrap_or(0);
    let max_row = chain.iter().map(|s| s.row).max().unwrap_or(0);
    let cols = (max_col + 2).max(MIN_COLS);
    let rows = (max_row + 2).max(MIN_ROWS);
    (cols, rows)
}

/// Natural pixel width of the grid (before zoom).
pub(crate) fn grid_natural_width(cols: usize) -> usize {
    cols * CELL_SIZE + cols.saturating_sub(1) * CELL_GAP
}

/// Natural pixel height of the grid (before zoom).
pub(crate) fn grid_natural_height(rows: usize) -> usize {
    rows * CELL_SIZE + rows.saturating_sub(1) * CELL_GAP
}

// ─────────────────────────────────────────────────────────────────────────────
// Module group bounding boxes
// ─────────────────────────────────────────────────────────────────────────────

/// A resolved module group — bounding rectangle in pixel space + color info.
pub(crate) struct ModuleGroupRect {
    /// Display name for the title bar.
    pub(crate) name: String,
    /// ModuleType color (bg, fg, border).
    pub(crate) color: crate::components::rig_grid::block_colors::BlockColor,
    /// Pixel position and size of the container (in grid-natural coords).
    pub(crate) x: f64,
    pub(crate) y: f64,
    pub(crate) w: f64,
    pub(crate) h: f64,
}

/// Padding around the cell bounding box for the group container (px).
/// 25% of the gap between cells, so adjacent modules have clear separation.
pub(crate) const GROUP_PAD: f64 = CELL_GAP as f64 * 0.25;
/// Height of the title bar above the cells.
pub(crate) const GROUP_TITLE_H: f64 = 16.0;

/// Compute module group bounding rectangles from the chain.
///
/// Groups are identified by `CompositionSlot::module_group`. For each group,
/// we find the min/max col/row of its member slots, convert to pixel coords,
/// and add padding + a title bar strip above.
pub(crate) fn compute_module_groups(chain: &[CompositionSlot]) -> Vec<ModuleGroupRect> {
    use signal_control::module::ModuleType;
    use std::collections::BTreeMap;

    // Collect slots by group key, tracking bounds + module type
    struct GroupInfo {
        min_col: usize,
        max_col: usize,
        min_row: usize,
        max_row: usize,
        module_type: ModuleType,
        name: String,
    }

    let mut groups: BTreeMap<String, GroupInfo> = BTreeMap::new();

    for slot in chain {
        let Some(ref key) = slot.module_group else {
            continue;
        };
        let mt = slot.module_type.unwrap_or(ModuleType::Custom);
        groups
            .entry(key.clone())
            .and_modify(|g| {
                g.min_col = g.min_col.min(slot.col);
                g.max_col = g.max_col.max(slot.col);
                g.min_row = g.min_row.min(slot.row);
                g.max_row = g.max_row.max(slot.row);
            })
            .or_insert(GroupInfo {
                min_col: slot.col,
                max_col: slot.col,
                min_row: slot.row,
                max_row: slot.row,
                module_type: mt,
                name: key.clone(),
            });
    }

    groups
        .into_values()
        .map(|g| {
            let step = (CELL_SIZE + CELL_GAP) as f64;
            // Top-left corner of the min cell
            let cell_x = g.min_col as f64 * step;
            let cell_y = g.min_row as f64 * step;
            // Bottom-right corner of the max cell (cell origin + cell size)
            let cell_x2 = g.max_col as f64 * step + CELL_SIZE as f64;
            let cell_y2 = g.max_row as f64 * step + CELL_SIZE as f64;

            let color = module_type_color(g.module_type);

            ModuleGroupRect {
                name: g.name,
                color,
                x: cell_x - GROUP_PAD,
                y: cell_y - GROUP_PAD - GROUP_TITLE_H,
                w: (cell_x2 - cell_x) + GROUP_PAD * 2.0,
                h: (cell_y2 - cell_y) + GROUP_PAD * 2.0 + GROUP_TITLE_H,
            }
        })
        .collect()
}

// ─────────────────────────────────────────────────────────────────────────────
// Cable geometry — connection points + Bézier paths
// ─────────────────────────────────────────────────────────────────────────────

/// Pixel position of the right-edge output port center for a cell at (col, row).
pub(crate) fn output_port_pos(col: usize, row: usize) -> (f64, f64) {
    let x = (col * (CELL_SIZE + CELL_GAP) + CELL_SIZE) as f64;
    let y = (row * (CELL_SIZE + CELL_GAP)) as f64 + CELL_SIZE as f64 / 2.0;
    (x, y)
}

/// Pixel position of the left-edge input port center for a cell at (col, row).
pub(crate) fn input_port_pos(col: usize, row: usize) -> (f64, f64) {
    let x = (col * (CELL_SIZE + CELL_GAP)) as f64;
    let y = (row * (CELL_SIZE + CELL_GAP)) as f64 + CELL_SIZE as f64 / 2.0;
    (x, y)
}

/// A resolved cable between two points in pixel space.
///
/// Coordinates are in grid-natural pixel space (before pan/zoom).
pub(crate) struct Cable {
    pub(crate) from: (f64, f64),
    pub(crate) to: (f64, f64),
    pub(crate) color: String,
    /// When true, render as a straight line instead of a Bézier curve.
    pub(crate) straight: bool,
    /// When set, route through this Y coordinate with rounded corners
    /// (down/up → horizontal → up/down) instead of a direct path.
    pub(crate) route_y: Option<f64>,
}

/// A virtual I/O port on a module container edge.
pub(crate) struct ModulePort {
    pub(crate) pos: (f64, f64),
    pub(crate) color: String,
}

/// Compute module container I/O port positions for all multi-row modules.
///
/// Returns (input_ports, output_ports) — each is a list of `ModulePort`
/// positioned at the left/right edge center of the module container.
/// Single-row modules are omitted since their block ports serve the same role.
pub(crate) fn compute_module_ports(chain: &[CompositionSlot]) -> Vec<ModulePort> {
    use std::collections::BTreeMap;

    let mut ports = Vec::new();
    if chain.is_empty() {
        return ports;
    }

    // Collect module bounds (same logic as resolve_cables)
    let mut group_map: Vec<(String, usize, usize, usize, usize, String)> = Vec::new();
    let mut seen: BTreeMap<String, usize> = BTreeMap::new();

    for s in chain.iter() {
        let Some(ref g) = s.module_group else {
            continue;
        };
        let color = block_type_color(s.block_type).bg.to_string();
        if let Some(&idx) = seen.get(g) {
            let entry = &mut group_map[idx];
            entry.1 = entry.1.min(s.col);
            entry.2 = entry.2.max(s.col);
            entry.3 = entry.3.min(s.row);
            entry.4 = entry.4.max(s.row);
        } else {
            seen.insert(g.clone(), group_map.len());
            group_map.push((g.clone(), s.col, s.col, s.row, s.row, color));
        }
    }

    let step = (CELL_SIZE + CELL_GAP) as f64;
    for (_name, min_c, max_c, min_r, max_r, color) in &group_map {
        // Input port: left edge center
        let in_x = *min_c as f64 * step - GROUP_PAD;
        let top = *min_r as f64 * step;
        let bottom = *max_r as f64 * step + CELL_SIZE as f64;
        let center_y = (top + bottom) / 2.0;
        ports.push(ModulePort {
            pos: (in_x, center_y),
            color: color.clone(),
        });

        // Output port: right edge center
        let out_x = *max_c as f64 * step + CELL_SIZE as f64 + GROUP_PAD;
        ports.push(ModulePort {
            pos: (out_x, center_y),
            color: color.clone(),
        });
    }

    ports
}

/// Resolved module boundary info for wiring.
struct ModuleIO {
    /// Module group name (signal chain identity).
    name: String,
    /// All blocks on the left edge (inputs to this module), as (col, row) sorted by row.
    left_edge: Vec<(usize, usize)>,
    /// All blocks on the right edge (outputs from this module), as (col, row) sorted by row.
    right_edge: Vec<(usize, usize)>,
    /// Module's grid bounding box.
    min_col: usize,
    max_col: usize,
    min_row: usize,
    max_row: usize,
    /// Color for this module's cables.
    color: String,
}

impl ModuleIO {
    /// Virtual input point: left edge of the module container, vertically centered.
    fn input_point(&self) -> (f64, f64) {
        let step = (CELL_SIZE + CELL_GAP) as f64;
        let x = self.min_col as f64 * step - GROUP_PAD;
        let top = self.min_row as f64 * step;
        let bottom = self.max_row as f64 * step + CELL_SIZE as f64;
        (x, (top + bottom) / 2.0)
    }

    /// Virtual output point: right edge of the module container, vertically centered.
    fn output_point(&self) -> (f64, f64) {
        let step = (CELL_SIZE + CELL_GAP) as f64;
        let x = self.max_col as f64 * step + CELL_SIZE as f64 + GROUP_PAD;
        let top = self.min_row as f64 * step;
        let bottom = self.max_row as f64 * step + CELL_SIZE as f64;
        (x, (top + bottom) / 2.0)
    }

    /// True if this module has multiple rows (parallel blocks).
    fn is_multi_row(&self) -> bool {
        self.min_row != self.max_row
    }
}

/// Build the wiring for the entire signal chain.
///
/// Wiring model (matching the user's diagram):
///
/// Each **module container** has a virtual input point (left edge center) and
/// output point (right edge center). Cables are:
///
/// 1. **Intra-module**: For modules with horizontally adjacent blocks on the
///    same row, `block_output → next_block_input`.
/// 2. **Fan-out** (multi-row modules): Module input point → each left-edge
///    block's input port.
/// 3. **Fan-in** (multi-row modules): Each right-edge block's output port →
///    Module output point.
/// 4. **Inter-module**: Module N output point → Module N+1 input point.
///    One cable per consecutive module pair in signal chain order.
///
/// For single-row modules the virtual I/O points coincide with the
/// first/last block ports, so fan-out/fan-in are skipped.
pub(crate) fn resolve_cables(chain: &[CompositionSlot]) -> Vec<Cable> {
    use std::collections::BTreeMap;

    let mut cables = Vec::new();
    if chain.is_empty() {
        return cables;
    }

    // ── Build module info, preserving signal chain order ─────────
    let mut group_map: Vec<(String, usize, usize, usize, usize, String)> = Vec::new();
    let mut seen: BTreeMap<String, usize> = BTreeMap::new();

    for s in chain.iter() {
        let Some(ref g) = s.module_group else {
            continue;
        };
        let color = block_type_color(s.block_type).bg.to_string();
        if let Some(&idx) = seen.get(g) {
            let entry = &mut group_map[idx];
            entry.1 = entry.1.min(s.col);
            entry.2 = entry.2.max(s.col);
            entry.3 = entry.3.min(s.row);
            entry.4 = entry.4.max(s.row);
        } else {
            seen.insert(g.clone(), group_map.len());
            group_map.push((g.clone(), s.col, s.col, s.row, s.row, color));
        }
    }

    let modules: Vec<ModuleIO> = group_map
        .iter()
        .map(|(name, min_c, max_c, min_r, max_r, color)| {
            let mut left_edge: Vec<(usize, usize)> = chain
                .iter()
                .filter(|s| s.module_group.as_deref() == Some(name) && s.col == *min_c)
                .map(|s| (s.col, s.row))
                .collect();
            left_edge.sort_by_key(|&(_, r)| r);

            let mut right_edge: Vec<(usize, usize)> = chain
                .iter()
                .filter(|s| s.module_group.as_deref() == Some(name) && s.col == *max_c)
                .map(|s| (s.col, s.row))
                .collect();
            right_edge.sort_by_key(|&(_, r)| r);

            ModuleIO {
                name: name.clone(),
                left_edge,
                right_edge,
                min_col: *min_c,
                max_col: *max_c,
                min_row: *min_r,
                max_row: *max_r,
                color: color.clone(),
            }
        })
        .collect();

    // ── 1. Intra-module horizontal adjacency ─────────────────────
    for a in chain.iter() {
        for b in chain.iter() {
            let same_group = match (&a.module_group, &b.module_group) {
                (Some(ga), Some(gb)) => ga == gb,
                _ => false,
            };
            if !same_group {
                continue;
            }
            if a.row == b.row && b.col == a.col + 1 {
                let color = block_type_color(a.block_type).bg.to_string();
                cables.push(Cable {
                    from: output_port_pos(a.col, a.row),
                    to: input_port_pos(b.col, b.row),
                    color,
                    straight: false,
                    route_y: None,
                });
            }
        }
    }

    // ── 2. Fan-out / fan-in for multi-row modules ────────────────
    // Module input point → each left-edge block input.
    // Each right-edge block output → module output point.
    // For modules with an empty center row, add a pass-through cable.
    for m in &modules {
        if m.is_multi_row() {
            let mod_in = m.input_point();
            for &(col, row) in &m.left_edge {
                cables.push(Cable {
                    from: mod_in,
                    to: input_port_pos(col, row),
                    color: m.color.clone(),
                    straight: false,
                    route_y: None,
                });
            }

            let mod_out = m.output_point();
            for &(col, row) in &m.right_edge {
                cables.push(Cable {
                    from: output_port_pos(col, row),
                    to: mod_out,
                    color: m.color.clone(),
                    straight: false,
                    route_y: None,
                });
            }

            // Pass-through: if the center row has no blocks, draw a straight
            // cable from module input → module output (raw signal bypass lane).
            let center_row = (m.min_row + m.max_row) / 2;
            let has_center_block = chain
                .iter()
                .any(|s| s.module_group.as_deref() == Some(&m.name) && s.row == center_row);
            if !has_center_block && m.max_row - m.min_row >= 2 {
                cables.push(Cable {
                    from: mod_in,
                    to: mod_out,
                    color: m.color.clone(),
                    straight: false,
                    route_y: None,
                });
            }
        }
    }

    // ── 3. Inter-module cables (signal chain order) ──────────────
    // Same-row modules get a direct horizontal cable.
    // Cross-row modules route through a cable channel in the gap between
    // the two row bands: down from output → across the channel → up to input.
    // The channel Y hugs the bottom edge of the upper row band — just
    // below the cells/modules, in the CELL_GAP space before the next row.
    {
        let step = (CELL_SIZE + CELL_GAP) as f64;

        for pair in modules.windows(2) {
            let from_mod = &pair[0];
            let to_mod = &pair[1];
            let from_pt = from_mod.output_point();
            let to_pt = to_mod.input_point();
            let color = from_mod.color.clone();

            // Check if both modules overlap in row range (same horizontal band)
            let rows_overlap =
                from_mod.min_row <= to_mod.max_row && to_mod.min_row <= from_mod.max_row;

            if rows_overlap {
                // Same row band — direct horizontal cable
                cables.push(Cable {
                    from: from_pt,
                    to: to_pt,
                    color,
                    straight: false,
                    route_y: None,
                });
            } else {
                // Cross-row: route through the gap just below the upper row band.
                let upper_bottom_row = from_mod.max_row.min(to_mod.max_row);
                // Channel Y = bottom of that row's cell + small offset into gap
                let channel_y =
                    upper_bottom_row as f64 * step + CELL_SIZE as f64 + CELL_GAP as f64 * 0.25;

                // Emit a single routed cable with rounded corners at the bends.
                cables.push(Cable {
                    from: from_pt,
                    to: to_pt,
                    color,
                    straight: false,
                    route_y: Some(channel_y),
                });
            }
        }
    }

    cables
}

/// Resolve cables from explicit connections if present, otherwise from adjacency.
fn resolve_cables_or_connections(
    chain: &[CompositionSlot],
    connections: &[GridConnection],
) -> Vec<Cable> {
    if connections.is_empty() {
        return resolve_cables(chain);
    }
    // Build cables from explicit connections
    connections
        .iter()
        .filter_map(|conn| {
            let from = chain.iter().find(|s| s.id == conn.from_slot_id)?;
            let to = chain.iter().find(|s| s.id == conn.to_slot_id)?;
            let color = block_type_color(from.block_type).bg.to_string();
            Some(Cable {
                from: output_port_pos(from.col, from.row),
                to: input_port_pos(to.col, to.row),
                color,
                straight: false,
                route_y: None,
            })
        })
        .collect()
}

/// SVG path for a cable routed through a horizontal channel at `channel_y`.
///
/// Draws: from → vertical to channel → horizontal along channel → vertical to dest,
/// with smooth arc corners at each bend.
///
/// ```text
///   from ──╮         ╭── to
///          │         │
///          ╰─────────╯   ← channel_y
/// ```
fn routed_cable_path(from: (f64, f64), to: (f64, f64), channel_y: f64) -> String {
    let r = 10.0f64; // corner radius

    let (fx, fy) = from;
    let (tx, ty) = to;

    // Direction from `from` to channel (down = +y, up = -y)
    let dy1 = channel_y - fy;
    // Direction from channel to `to` (up = -y, down = +y)
    let dy2 = ty - channel_y;
    // Horizontal direction along channel
    let dx = tx - fx;

    // If the vertical distance to the channel is too small for arcs, just
    // draw a direct Bézier instead.
    if dy1.abs() < r * 2.0 || dy2.abs() < r * 2.0 {
        return cable_path_d(from, to);
    }

    let going_down_first = dy1 > 0.0;
    let going_right = dx > 0.0;
    let going_up_last = dy2 < 0.0;

    // Corner 1: from vertical → horizontal at (fx, channel_y)
    // Corner 2: from horizontal → vertical at (tx, channel_y)

    // SVG arc sweep-flag: 1 = clockwise, 0 = counter-clockwise
    // The sweep depends on which quadrant the turn is in.

    // Corner 1: vertical to horizontal
    let (c1_vy_end, _c1_hy_start, sweep1) = if going_down_first && going_right {
        (channel_y - r, channel_y, 0)
    } else if going_down_first && !going_right {
        (channel_y - r, channel_y, 1)
    } else if !going_down_first && going_right {
        (channel_y + r, channel_y, 1)
    } else {
        (channel_y + r, channel_y, 0)
    };

    let c1_hx = if going_right { fx + r } else { fx - r };

    // Corner 2: horizontal to vertical
    let (c2_hx_end, c2_vy_start, sweep2) = if going_right && going_up_last {
        (tx - r, channel_y - r, 0)
    } else if going_right && !going_up_last {
        (tx - r, channel_y + r, 1)
    } else if !going_right && going_up_last {
        (tx + r, channel_y - r, 1)
    } else {
        (tx + r, channel_y + r, 0)
    };

    format!(
        "M {fx},{fy} \
         L {fx},{c1_vy_end} \
         A {r},{r} 0 0 {sweep1} {c1_hx},{channel_y} \
         L {c2_hx_end},{channel_y} \
         A {r},{r} 0 0 {sweep2} {tx},{c2_vy_start} \
         L {tx},{ty}",
    )
}

/// SVG path between two ports.
///
/// For normal horizontal/vertical cables, draws a cubic Bézier.
/// For row-wrap cables (output on the right wrapping to input on the left
/// below), draws a smooth U-turn path that goes right → down → left.
pub(crate) fn cable_path_d(from: (f64, f64), to: (f64, f64)) -> String {
    let dx = to.0 - from.0;
    let dy = to.1 - from.1;
    let abs_dx = dx.abs();
    let abs_dy = dy.abs();

    // Row-wrap detection: target is below AND to the left (U-turn path).
    let is_row_wrap = dy > 40.0 && dx < -40.0;

    if is_row_wrap {
        // Routed U-turn with near-90° turns and rounded corners.
        //
        // Layout (stride-2 rows, gap row between populated rows):
        //
        //   [row 0 blocks ............... LAST] ──→ ╮  (corner 1)
        //                                           │
        //   - - - - gap row (routing channel) - - - │ -
        //                                           │
        //            ╭──────────── left ────────────╯  (corner 2)
        //            │
        //   - - - - -│- - gap row channel - - - - - - -
        //            │
        //            ╰──→ [FIRST block row 2 ........]  (corner 3 + 4)
        //
        // The cable exits right, drops below all row-0 cells, sweeps left
        // below the grid to col 0, then enters the row-2 input from the left.

        let r = 12.0; // corner rounding radius

        // Channel Y: midpoint between from and to Y positions (center of gap row)
        let channel_y = (from.1 + to.1) * 0.5;

        // Right vertical rail: in the gap to the right of the output port
        let right_x = from.0 + (CELL_GAP as f64) * 0.5;

        // Left vertical rail: in the gap to the left of the input port
        let left_x = to.0 - (CELL_GAP as f64) * 0.5;

        // Sweep direction for SVG arcs:
        //   A rx,ry rotation large-arc-flag sweep-flag x,y
        //   sweep-flag: 0 = counter-clockwise, 1 = clockwise

        format!(
            "M {fx},{fy} \
             L {c1sx},{fy} \
             A {r},{r} 0 0 1 {c1ex},{c1ey} \
             L {right_x},{c2sy} \
             A {r},{r} 0 0 1 {c2ex},{channel_y} \
             L {c3sx},{channel_y} \
             A {r},{r} 0 0 0 {c3ex},{c3ey} \
             L {left_x},{c4sy} \
             A {r},{r} 0 0 0 {c4ex},{ty} \
             L {tx},{ty}",
            fx = from.0,
            fy = from.1,
            r = r,
            // Corner 1: top-right (rightward → downward, clockwise)
            c1sx = right_x - r,
            c1ex = right_x,
            c1ey = from.1 + r,
            // Vertical down right side
            right_x = right_x,
            c2sy = channel_y - r,
            // Corner 2: bottom-right (downward → leftward, clockwise)
            c2ex = right_x - r,
            channel_y = channel_y,
            // Horizontal left across channel
            c3sx = left_x + r,
            // Corner 3: bottom-left (leftward → downward, counter-clockwise)
            c3ex = left_x,
            c3ey = channel_y + r,
            // Vertical down left side to row 2
            left_x = left_x,
            c4sy = to.1 - r,
            // Corner 4: arrive at input row (downward → rightward, counter-clockwise)
            c4ex = left_x + r,
            ty = to.1,
            tx = to.0,
        )
    } else if abs_dx >= abs_dy {
        // Primarily horizontal
        let offset = abs_dx.max(60.0) * 0.4;
        format!(
            "M {},{} C {},{} {},{} {},{}",
            from.0,
            from.1,
            from.0 + offset,
            from.1,
            to.0 - offset,
            to.1,
            to.0,
            to.1,
        )
    } else {
        // Primarily vertical
        let offset = abs_dy.max(60.0) * 0.4;
        format!(
            "M {},{} C {},{} {},{} {},{}",
            from.0,
            from.1,
            from.0,
            from.1 + offset,
            to.0,
            to.1 - offset,
            to.0,
            to.1,
        )
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Grid ↔ Node coordinate translation (future-ready)
// ─────────────────────────────────────────────────────────────────────────────

/// Convert grid position to node-style pixel coordinates for NodeView rendering.
#[allow(dead_code)]
pub fn grid_to_node_coords(col: usize, row: usize) -> (f64, f64) {
    let x = col as f64 * 180.0 + 40.0;
    let y = row as f64 * 120.0 + 40.0;
    (x, y)
}

/// Convert node pixel coordinates back to nearest grid position.
#[allow(dead_code)]
pub fn node_to_grid_coords(x: f64, y: f64) -> (usize, usize) {
    let col = ((x - 40.0) / 180.0).round().max(0.0) as usize;
    let row = ((y - 40.0) / 120.0).round().max(0.0) as usize;
    (col, row)
}

// ─────────────────────────────────────────────────────────────────────────────
// DynamicGridView
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct DynamicGridViewProps {
    /// The composition chain to render on the grid.
    pub chain: Vec<CompositionSlot>,
    /// Currently selected slot ID.
    pub selected_slot_id: Option<Uuid>,
    /// Explicit wire connections between blocks (empty = adjacency fallback).
    #[props(default)]
    pub connections: Vec<GridConnection>,
    /// Called when the chain changes (drag-move, drag-swap, block add).
    pub on_chain_change: EventHandler<Vec<CompositionSlot>>,
    /// Called when connections change (wire draft completion).
    pub on_connections_change: EventHandler<Vec<GridConnection>>,
    /// Called when the selected slot changes (block click, drag start).
    pub on_select: EventHandler<Option<Uuid>>,
    /// Called when a module group is reordered by dragging its title bar.
    /// Provides (dragged_group_name, target_group_name) for the swap.
    #[props(default)]
    pub on_group_reorder: Option<EventHandler<(String, String)>>,
}

/// Dynamic 2D grid that auto-expands as blocks are added.
///
/// Cells are always square (1:1 aspect ratio) at a fixed internal size.
/// The viewport supports interactive pan (drag background) and zoom (scroll
/// wheel, cursor-anchored). Auto-fits on mount. Adjacent occupied cells are
/// connected by SVG Bézier cables with port circles on left/right edges.
///
/// Supports drag-and-drop reordering: mousedown on a block starts a drag,
/// moving the mouse shows a ghost + drop target, mouseup moves the block.
///
/// Supports wire drafting: mousedown on an output port starts a wire draft,
/// mouseup on another block's input port creates an explicit connection.
#[component]
pub fn DynamicGridView(props: DynamicGridViewProps) -> Element {
    let (cols, rows) = compute_grid_bounds(&props.chain);
    let selected_slot_id = props.selected_slot_id;

    // ── Viewport state (pan/zoom) ────────────────────────────────
    let mut pan_x = use_signal(|| 0.0f64);
    let mut pan_y = use_signal(|| 0.0f64);
    let mut zoom = use_signal(|| 1.0f64);
    let mut viewport_left = use_signal(|| 0.0f64);
    let mut viewport_top = use_signal(|| 0.0f64);
    let mut viewport_w = use_signal(|| 0.0f64); // Start at 0 — render hidden until measured
    let mut viewport_h = use_signal(|| 0.0f64);

    // ── Pan drag state ───────────────────────────────────────────
    let mut pan_drag = use_signal(|| None::<(f64, f64, f64, f64)>); // (start_mx, start_my, start_px, start_py)

    // Picker state uses global signals (PICKER_CELL, PICKER_CLICK_POS) so
    // the parent component can render the dropdown above the CSS transform.

    // Block drag state
    let mut drag_state = use_signal(|| None::<GridDragState>);
    let mut group_drag = use_signal(|| None::<GroupDragState>);

    // Wire draft state
    let mut wire_draft = use_signal(|| None::<GridWireDraft>);
    let mut hovered_port_slot = use_signal(|| None::<(Uuid, bool)>); // (slot_id, is_input)

    // ── Mounted element for viewport measurement (pure Rust, no JS) ──
    let mut mounted_el: Signal<Option<Rc<MountedData>>> = use_signal(|| None);

    // Helper: re-measure the container element dimensions
    let update_viewport = move || {
        if let Some(el) = mounted_el.read().as_ref() {
            let el_clone = el.clone();
            spawn(async move {
                if let Ok(rect) = el_clone.get_client_rect().await {
                    let w = rect.width();
                    let h = rect.height();
                    let x = rect.origin.x;
                    let y = rect.origin.y;
                    if w > 0.0 && h > 0.0 {
                        viewport_w.set(w);
                        viewport_h.set(h);
                        viewport_left.set(x);
                        viewport_top.set(y);
                    }
                }
            });
        }
    };

    let chain = &props.chain;

    // Fixed-size grid: each cell is CELL_SIZE×CELL_SIZE with CELL_GAP spacing
    let nat_w = grid_natural_width(cols);
    let nat_h = grid_natural_height(rows);
    let col_template = format!("repeat({cols}, {CELL_SIZE}px)");
    let grid_style = format!(
        "grid-template-columns: {col_template}; gap: {CELL_GAP}px; \
         width: {nat_w}px; height: {nat_h}px;",
    );

    tracing::info!(
        "DynamicGridView: chain={} slots, grid={}x{}, nat={}x{}, viewport={}x{}, zoom={:.2}, pan=({:.0},{:.0})",
        chain.len(), cols, rows, nat_w, nat_h,
        viewport_w(), viewport_h(), zoom(), pan_x(), pan_y()
    );

    // Build a virtual chain that reflects the current drag state.
    // During a group drag, the dragged module's slots are shifted to their
    // snapped grid position so cables follow in real-time. During shift+drag,
    // the dragged module is excluded (cables bypass it).
    let drag_chain: Vec<CompositionSlot> = if let Some(ref gd) = group_drag() {
        let step = (CELL_SIZE + CELL_GAP) as f64;
        let cz = zoom();
        let dx_px = (gd.mouse_x - gd.start_mouse_x) / cz;
        let dy_px = (gd.mouse_y - gd.start_mouse_y) / cz;
        let col_delta = (dx_px / step).round() as isize;
        let row_delta = (dy_px / step).round() as isize;

        if gd.shift_held {
            // Shift+drag: exclude dragged module from chain (bypass)
            chain
                .iter()
                .filter(|s| s.module_group.as_deref() != Some(&gd.group_name))
                .cloned()
                .collect()
        } else {
            // Normal drag: move dragged module to snapped position
            chain
                .iter()
                .map(|s| {
                    if s.module_group.as_deref() == Some(&gd.group_name) {
                        let mut moved = s.clone();
                        moved.col = (moved.col as isize + col_delta).max(0) as usize;
                        moved.row = (moved.row as isize + row_delta).max(0) as usize;
                        moved
                    } else {
                        s.clone()
                    }
                })
                .collect()
        }
    } else {
        chain.to_vec()
    };

    // Resolve cables from the virtual chain (follows drag in real-time)
    let cables = resolve_cables_or_connections(&drag_chain, &props.connections);
    let module_ports = compute_module_ports(&drag_chain);

    // Compute module group bounding boxes (Layer 0 backgrounds)
    let module_groups = compute_module_groups(chain);

    // Compute hover target cell during drag
    let drag = drag_state();
    let current_zoom = zoom();
    let hover_cell: Option<(usize, usize)> = drag.as_ref().map(|d| {
        let step = (CELL_SIZE + CELL_GAP) as f64;
        let dx = (d.mouse_x - d.start_mouse_x) / current_zoom;
        let dy = (d.mouse_y - d.start_mouse_y) / current_zoom;
        let col_delta = (dx / step).round() as isize;
        let row_delta = (dy / step).round() as isize;
        let new_col = (d.origin_col as isize + col_delta).max(0) as usize;
        let new_row = (d.origin_row as isize + row_delta).max(0) as usize;
        (
            new_col.min(cols.saturating_sub(1)),
            new_row.min(rows.saturating_sub(1)),
        )
    });

    // Is the dragged block's slot being rendered at its current position?
    let dragged_slot_id = drag.as_ref().map(|d| d.slot_id);

    // Compute group drag drop target — always snapped to grid cells
    let group_drop_target: Option<GroupDropTarget> = group_drag().as_ref().map(|gd| {
        let cz = zoom();
        let dx_px = (gd.mouse_x - gd.start_mouse_x) / cz;
        let dy_px = (gd.mouse_y - gd.start_mouse_y) / cz;
        let step = (CELL_SIZE + CELL_GAP) as f64;

        // Snap to whole grid-cell offsets
        let col_delta = (dx_px / step).round() as isize;
        let row_delta = (dy_px / step).round() as isize;

        // Check if the snapped position overlaps another group (swap target)
        if let Some(dragged) = module_groups.iter().find(|g| g.name == gd.group_name) {
            let target_cx = dragged.x + dragged.w * 0.5 + col_delta as f64 * step;
            let target_cy = dragged.y + dragged.h * 0.5 + row_delta as f64 * step;

            for g in module_groups.iter() {
                if g.name != gd.group_name
                    && target_cx >= g.x
                    && target_cx <= g.x + g.w
                    && target_cy >= g.y
                    && target_cy <= g.y + g.h
                {
                    return GroupDropTarget::SwapWith(g.name.clone());
                }
            }
        }

        GroupDropTarget::MoveDelta(col_delta, row_delta)
    });

    // No container ID needed — viewport is tracked via onmounted + get_client_rect()

    // ── Compute actual content bounds from slots + module groups ─
    // The grid natural size includes empty padding rows/cols. The real
    // content extent is the bounding box of all occupied cells and their
    // module group backgrounds (which extend above cells for title bars).
    let content_bounds = {
        let mut min_x = f64::MAX;
        let mut min_y = f64::MAX;
        let mut max_x = 0.0f64;
        let mut max_y = 0.0f64;

        // Cells
        let step = (CELL_SIZE + CELL_GAP) as f64;
        for slot in chain.iter() {
            let cx = slot.col as f64 * step;
            let cy = slot.row as f64 * step;
            min_x = min_x.min(cx);
            min_y = min_y.min(cy);
            max_x = max_x.max(cx + CELL_SIZE as f64);
            max_y = max_y.max(cy + CELL_SIZE as f64);
        }

        // Module group backgrounds (extend above/below cells)
        for group in &module_groups {
            min_x = min_x.min(group.x);
            min_y = min_y.min(group.y);
            max_x = max_x.max(group.x + group.w);
            max_y = max_y.max(group.y + group.h);
        }

        if min_x == f64::MAX {
            // No content — use natural grid size
            (0.0, 0.0, nat_w as f64, nat_h as f64)
        } else {
            (min_x, min_y, max_x - min_x, max_y - min_y)
        }
    };
    let (content_offset_x, content_offset_y, content_w, content_h) = content_bounds;

    // ── Auto-fit: re-fit when viewport OR content bounds change ──
    let mut last_fit_state = use_signal(|| (0.0f64, 0.0f64, 0.0f64, 0.0f64)); // (vw, vh, content_w, content_h)
    {
        let vw = viewport_w();
        let vh = viewport_h();
        let (last_vw, last_vh, last_cw, last_ch) = last_fit_state();
        let viewport_changed = (last_vw - vw).abs() > 10.0 || (last_vh - vh).abs() > 10.0;
        let content_changed =
            (last_cw - content_w).abs() > 1.0 || (last_ch - content_h).abs() > 1.0;
        if (viewport_changed || content_changed)
            && vw > 1.0
            && vh > 1.0
            && content_w > 0.0
            && content_h > 0.0
        {
            last_fit_state.set((vw, vh, content_w, content_h));
            let padding = 20.0;
            let avail_w = vw - padding * 2.0;
            let avail_h = vh - padding * 2.0;
            let fit_zoom = (avail_w / content_w)
                .min(avail_h / content_h)
                .clamp(0.1, 3.0);
            // Center the content in the viewport
            let scaled_w = content_w * fit_zoom;
            let scaled_h = content_h * fit_zoom;
            pan_x.set((vw - scaled_w) / 2.0 - content_offset_x * fit_zoom);
            pan_y.set((vh - scaled_h) / 2.0 - content_offset_y * fit_zoom);
            zoom.set(fit_zoom);
        }
    }

    // Pre-compute port half-size for positioning
    let port_half = PORT_SIZE / 2.0;

    // Cursor style depends on state
    let is_panning = pan_drag().is_some();
    let cursor = if is_panning {
        "grabbing"
    } else if drag.is_some() || group_drag().is_some() {
        "grabbing"
    } else if wire_draft().is_some() {
        "crosshair"
    } else {
        "default"
    };

    // Content bounds for use in the Fit button closure
    let fit_content_w = content_w;
    let fit_content_h = content_h;
    let fit_content_offset_x = content_offset_x;
    let fit_content_offset_y = content_offset_y;

    rsx! {
        // Viewport container — fills available space, clips content, handles pan/zoom
        div {
            class: "relative h-full w-full overflow-hidden select-none",
            onmounted: move |evt: MountedEvent| {
                mounted_el.set(Some(evt.data()));
                update_viewport();
                // Schedule a delayed re-measurement in case the initial
                // get_client_rect returns 0 (element not yet laid out).
                spawn(async move {
                    tokio::time::sleep(std::time::Duration::from_millis(50)).await;
                    update_viewport();
                });
            },
            style: "cursor: {cursor}; \
                    background-color: #000000; \
                    background-image: radial-gradient(circle, #111 1px, transparent 1px); \
                    background-size: 20px 20px;",

            // ── Mouse: middle-click pan or left-click deselect ──
            onmousedown: move |evt| {
                update_viewport();
                if evt.trigger_button() == Some(MouseButton::Auxiliary) {
                    // Middle-click: start pan
                    evt.prevent_default();
                    pan_drag.set(Some((
                        evt.client_coordinates().x,
                        evt.client_coordinates().y,
                        pan_x(),
                        pan_y(),
                    )));
                } else if evt.trigger_button() == Some(MouseButton::Primary) {
                    // Left-click on background: deselect + close picker
                    props.on_select.call(None);
                    *PICKER_CELL.write() = None;
                }
            },

            // ── Mouse: move (pan + block drag + wire draft) ──
            onmousemove: move |evt| {
                let mx = evt.client_coordinates().x;
                let my = evt.client_coordinates().y;

                // Viewport pan
                if let Some((start_mx, start_my, start_px, start_py)) = pan_drag() {
                    if drag_state().is_none() && wire_draft().is_none() {
                        pan_x.set(start_px + (mx - start_mx));
                        pan_y.set(start_py + (my - start_my));
                    }
                }

                // Update block drag
                if let Some(mut d) = drag_state() {
                    d.mouse_x = mx;
                    d.mouse_y = my;
                    drag_state.set(Some(d));
                }

                // Update group drag
                if let Some(mut gd) = group_drag() {
                    gd.mouse_x = mx;
                    gd.mouse_y = my;
                    group_drag.set(Some(gd));
                }

                // Update wire draft mouse position
                if let Some(mut draft) = wire_draft() {
                    draft.mouse_pos = (mx, my);
                    wire_draft.set(Some(draft));
                }
            },

            onmouseup: move |_evt| {
                // Stop pan
                pan_drag.set(None);

                // Finalize block drag
                if let Some(d) = drag_state() {
                    let step = (CELL_SIZE + CELL_GAP) as f64;
                    let cz = zoom();
                    let dx = (d.mouse_x - d.start_mouse_x) / cz;
                    let dy = (d.mouse_y - d.start_mouse_y) / cz;
                    let col_delta = (dx / step).round() as isize;
                    let row_delta = (dy / step).round() as isize;
                    let new_col = (d.origin_col as isize + col_delta).max(0) as usize;
                    let new_row = (d.origin_row as isize + row_delta).max(0) as usize;
                    let target_col = new_col.min(cols.saturating_sub(1));
                    let target_row = new_row.min(rows.saturating_sub(1));

                    if target_col != d.origin_col || target_row != d.origin_row {
                        let mut new_chain = props.chain.clone();
                        let target_occupant = new_chain.iter().position(|s| s.col == target_col && s.row == target_row);
                        let dragged_idx = new_chain.iter().position(|s| s.id == d.slot_id);

                        if let Some(drag_idx) = dragged_idx {
                            if let Some(target_idx) = target_occupant {
                                let orig_col = new_chain[drag_idx].col;
                                let orig_row = new_chain[drag_idx].row;
                                new_chain[drag_idx].col = new_chain[target_idx].col;
                                new_chain[drag_idx].row = new_chain[target_idx].row;
                                new_chain[target_idx].col = orig_col;
                                new_chain[target_idx].row = orig_row;
                            } else {
                                new_chain[drag_idx].col = target_col;
                                new_chain[drag_idx].row = target_row;
                            }
                        }
                        props.on_chain_change.call(new_chain);
                    }
                    drag_state.set(None);
                }

                // Finalize group drag using computed drop target
                if let Some(gd) = group_drag() {
                    if let Some(ref target) = group_drop_target {
                        let mut new_chain = props.chain.clone();
                        match target {
                            GroupDropTarget::SwapWith(ref target_name) => {
                                // Swap: move each group to the other's origin
                                let dragged_min_col = new_chain.iter()
                                    .filter(|s| s.module_group.as_deref() == Some(&gd.group_name))
                                    .map(|s| s.col).min().unwrap_or(0);
                                let dragged_min_row = new_chain.iter()
                                    .filter(|s| s.module_group.as_deref() == Some(&gd.group_name))
                                    .map(|s| s.row).min().unwrap_or(0);
                                let target_min_col = new_chain.iter()
                                    .filter(|s| s.module_group.as_deref() == Some(target_name.as_str()))
                                    .map(|s| s.col).min().unwrap_or(0);
                                let target_min_row = new_chain.iter()
                                    .filter(|s| s.module_group.as_deref() == Some(target_name.as_str()))
                                    .map(|s| s.row).min().unwrap_or(0);

                                let dc = target_min_col as isize - dragged_min_col as isize;
                                let dr = target_min_row as isize - dragged_min_row as isize;

                                for s in new_chain.iter_mut() {
                                    if s.module_group.as_deref() == Some(&gd.group_name) {
                                        s.col = (s.col as isize + dc).max(0) as usize;
                                        s.row = (s.row as isize + dr).max(0) as usize;
                                    } else if s.module_group.as_deref() == Some(target_name.as_str()) {
                                        s.col = (s.col as isize - dc).max(0) as usize;
                                        s.row = (s.row as isize - dr).max(0) as usize;
                                    }
                                }
                                props.on_chain_change.call(new_chain);
                            }
                            GroupDropTarget::MoveDelta(dc, dr) => {
                                if *dc != 0 || *dr != 0 {
                                    for s in new_chain.iter_mut() {
                                        if s.module_group.as_deref() == Some(&gd.group_name) {
                                            s.col = (s.col as isize + dc).max(0) as usize;
                                            s.row = (s.row as isize + dr).max(0) as usize;
                                        }
                                    }
                                    props.on_chain_change.call(new_chain);
                                }
                            }
                        }
                    }
                    group_drag.set(None);
                }

                // Finalize wire draft
                if let Some(draft) = wire_draft() {
                    if let Some((target_id, is_input)) = hovered_port_slot() {
                        let new_conn = if draft.is_from_output && is_input && target_id != draft.from_slot_id {
                            Some(GridConnection {
                                from_slot_id: draft.from_slot_id,
                                to_slot_id: target_id,
                            })
                        } else if !draft.is_from_output && !is_input && target_id != draft.from_slot_id {
                            Some(GridConnection {
                                from_slot_id: target_id,
                                to_slot_id: draft.from_slot_id,
                            })
                        } else {
                            None
                        };
                        if let Some(conn) = new_conn {
                            let mut new_conns = props.connections.clone();
                            new_conns.push(conn);
                            props.on_connections_change.call(new_conns);
                        }
                    }
                    wire_draft.set(None);
                    hovered_port_slot.set(None);
                }
            },

            onmouseleave: move |_| {
                pan_drag.set(None);
                drag_state.set(None);
                group_drag.set(None);
                wire_draft.set(None);
                hovered_port_slot.set(None);
            },

            // ── Scroll: pan (normal/shift) or zoom (ctrl/pinch) ──
            onwheel: move |evt| {
                evt.prevent_default();
                update_viewport();
                let delta = evt.delta();
                // Dampen scroll sensitivity — raw pixel deltas from trackpads
                // and high-resolution wheels can be very large.
                let damp = 0.35;
                let (raw_dx, raw_dy) = match delta {
                    WheelDelta::Pixels(p) => (p.x * damp, p.y * damp),
                    WheelDelta::Lines(l) => (l.x * 16.0, l.y * 16.0),
                    WheelDelta::Pages(p) => (p.x * 160.0, p.y * 160.0),
                };

                let modifiers = evt.modifiers();
                let is_ctrl = modifiers.contains(keyboard_types::Modifiers::CONTROL)
                    || modifiers.contains(keyboard_types::Modifiers::META);
                let is_shift = modifiers.contains(keyboard_types::Modifiers::SHIFT);

                if is_ctrl {
                    // Ctrl+scroll / trackpad pinch → cursor-anchored zoom
                    let old_zoom = zoom();
                    let zoom_factor = if raw_dy < 0.0 { 1.08 } else { 1.0 / 1.08 };
                    let new_zoom = (old_zoom * zoom_factor).clamp(0.1, 3.0);

                    let local_x = evt.client_coordinates().x - viewport_left();
                    let local_y = evt.client_coordinates().y - viewport_top();
                    let canvas_x = (local_x - pan_x()) / old_zoom;
                    let canvas_y = (local_y - pan_y()) / old_zoom;
                    pan_x.set(local_x - canvas_x * new_zoom);
                    pan_y.set(local_y - canvas_y * new_zoom);
                    zoom.set(new_zoom);
                } else if is_shift {
                    // Shift+scroll → horizontal pan (swap axes: vertical
                    // wheel delta drives horizontal movement)
                    pan_x.set(pan_x() - raw_dy);
                    pan_y.set(pan_y() - raw_dx);
                } else {
                    // Normal scroll → vertical pan (+ horizontal from trackpad)
                    pan_y.set(pan_y() - raw_dy);
                    pan_x.set(pan_x() - raw_dx);
                }
            },

            // ── Canvas layer (positioned + zoomed) ───────────
            div {
                style: "position: absolute; left: {pan_x()}px; top: {pan_y()}px; \
                        transform: scale({zoom()}); transform-origin: 0 0;",

                // Inner wrapper at natural grid size.
                // `isolation: isolate` creates a stacking context so z-index
                // values of children (SVG cables, module backgrounds, grid cells)
                // are resolved relative to this container, not the page root.
                div {
                    class: "relative",
                    style: "width: {nat_w}px; height: {nat_h}px; isolation: isolate;",

                // Layer 0: SVG cables + port dots (behind everything)
                // Wrapped in a div because SVG with `overflow: visible` can
                // escape z-index ordering in WebKit. The wrapper div participates
                // in normal CSS stacking, and the SVG inside paints within it.
                div {
                    style: "position: absolute; left: 0; top: 0; width: {nat_w}px; height: {nat_h}px; \
                            z-index: 0; pointer-events: none; overflow: visible;",
                svg {
                    style: "overflow: visible;",
                    width: "{nat_w}",
                    height: "{nat_h}",
                    view_box: "0 0 {nat_w} {nat_h}",

                    for cable in cables.iter() {
                        {
                            let d = if let Some(ry) = cable.route_y {
                                routed_cable_path(cable.from, cable.to, ry)
                            } else if cable.straight {
                                format!("M {},{} L {},{}", cable.from.0, cable.from.1, cable.to.0, cable.to.1)
                            } else {
                                cable_path_d(cable.from, cable.to)
                            };
                            let stroke = cable.color.clone();
                            rsx! {
                                path {
                                    d: "{d}",
                                    fill: "none",
                                    stroke: "{stroke}",
                                    stroke_width: "2.5",
                                    stroke_opacity: "0.7",
                                    stroke_linecap: "round",
                                }
                            }
                        }
                    }

                    // Module I/O port dots (on container edges)
                    for port in module_ports.iter() {
                        {
                            let cx = port.pos.0;
                            let cy = port.pos.1;
                            let fill = port.color.clone();
                            rsx! {
                                circle {
                                    cx: "{cx}",
                                    cy: "{cy}",
                                    r: "4",
                                    fill: "{fill}",
                                    fill_opacity: "0.8",
                                    stroke: "{fill}",
                                    stroke_width: "1.5",
                                    stroke_opacity: "0.4",
                                }
                            }
                        }
                    }

                    // Wire draft line
                    if let Some(ref _draft) = wire_draft() {
                    }
                }
                } // close cable wrapper div

                // Layer 1: Module group backgrounds
                for group in module_groups.iter() {
                    {
                        let bg = format!(
                            "left: {}px; top: {}px; width: {}px; height: {}px; \
                             background-color: {}12; border: 1px solid {}30; border-radius: 10px;",
                            group.x, group.y, group.w, group.h,
                            group.color.bg, group.color.bg,
                        );
                        let title_style = format!(
                            "background-color: {}20; border-bottom: 1px solid {}25; \
                             border-radius: 10px 10px 0 0; height: {}px;",
                            group.color.bg, group.color.bg, GROUP_TITLE_H,
                        );
                        let name = group.name.clone();
                        let drag_name = group.name.clone();
                        let fg = group.color.fg;
                        let is_being_dragged = group_drag().as_ref().map_or(false, |gd| gd.group_name == name);
                        let drag_transform = if let Some(ref gd) = group_drag() {
                            if gd.group_name == name {
                                // Snap visual position to grid cells
                                let cz = zoom();
                                let dx_px = (gd.mouse_x - gd.start_mouse_x) / cz;
                                let dy_px = (gd.mouse_y - gd.start_mouse_y) / cz;
                                let step = (CELL_SIZE + CELL_GAP) as f64;
                                let snap_dx = (dx_px / step).round() * step;
                                let snap_dy = (dy_px / step).round() * step;
                                format!("transform: translate({snap_dx}px, {snap_dy}px); opacity: 0.85; z-index: 50;")
                            } else {
                                String::new()
                            }
                        } else {
                            String::new()
                        };
                        let transition = if is_being_dragged { "none" } else { "transform 0.15s ease" };
                        rsx! {
                            div {
                                key: "grp-{name}",
                                class: "absolute overflow-hidden",
                                style: "position: absolute; {bg} z-index: 1; pointer-events: none; transition: {transition}; {drag_transform}",
                                // Title bar — interactive drag handle
                                div {
                                    class: "flex items-center gap-1.5 px-2 cursor-grab active:cursor-grabbing",
                                    style: "{title_style} pointer-events: auto; z-index: 5;",
                                    onmousedown: move |evt: MouseEvent| {
                                        evt.stop_propagation();
                                        let mods = evt.modifiers();
                                        let shift = mods.contains(keyboard_types::Modifiers::SHIFT);
                                        group_drag.set(Some(GroupDragState {
                                            group_name: drag_name.clone(),
                                            start_mouse_x: evt.client_coordinates().x,
                                            start_mouse_y: evt.client_coordinates().y,
                                            mouse_x: evt.client_coordinates().x,
                                            mouse_y: evt.client_coordinates().y,
                                            shift_held: shift,
                                        }));
                                    },
                                    div {
                                        class: "w-2 h-2 rounded-full flex-shrink-0",
                                        style: "background-color: {group.color.bg};",
                                    }
                                    span {
                                        class: "text-[8px] font-semibold tracking-wide whitespace-nowrap opacity-80",
                                        style: "color: {fg};",
                                        "{name}"
                                    }
                                }
                            }
                        }
                    }
                }

                // Ghost preview for group drag target
                if let (Some(ref gd), Some(ref target)) = (group_drag(), &group_drop_target) {
                    {
                        // Show a dashed outline at the target position
                        let ghost_style = match target {
                            GroupDropTarget::SwapWith(ref target_name) => {
                                // Highlight the target group's bounding box
                                if let Some(tg) = module_groups.iter().find(|g| &g.name == target_name) {
                                    let label = if gd.shift_held { "extract" } else { "swap" };
                                    let border_color = if gd.shift_held { "#22d3ee" } else { "#60a5fa" };
                                    Some((tg.x, tg.y, tg.w, tg.h, border_color, label))
                                } else {
                                    None
                                }
                            }
                            GroupDropTarget::MoveDelta(dc, dr) => {
                                // Show ghost at the snapped grid position
                                if let Some(dragged) = module_groups.iter().find(|g| g.name == gd.group_name) {
                                    let step = (CELL_SIZE + CELL_GAP) as f64;
                                    let gx = dragged.x + *dc as f64 * step;
                                    let gy = dragged.y + *dr as f64 * step;
                                    let label = if gd.shift_held { "extract" } else { "move" };
                                    let border_color = if gd.shift_held { "#22d3ee" } else { "#60a5fa" };
                                    Some((gx, gy, dragged.w, dragged.h, border_color, label))
                                } else {
                                    None
                                }
                            }
                        };
                        if let Some((gx, gy, gw, gh, border_color, label)) = ghost_style {
                            rsx! {
                                div {
                                    class: "absolute",
                                    style: "left: {gx}px; top: {gy}px; width: {gw}px; height: {gh}px; \
                                            border: 2px dashed {border_color}; border-radius: 10px; \
                                            background-color: {border_color}10; \
                                            z-index: 45; pointer-events: none;",
                                    div {
                                        class: "absolute -top-5 left-1/2 -translate-x-1/2 px-2 py-0.5 rounded text-[9px] font-mono whitespace-nowrap",
                                        style: "background-color: {border_color}; color: #000;",
                                        "{label}"
                                    }
                                }
                            }
                        } else {
                            rsx! {}
                        }
                    }
                }

                // Layer 2: CSS Grid cells (blocks + empty cells)
                div {
                    class: "absolute inset-0 inline-grid",
                    style: "position: absolute; {grid_style} z-index: 2;",

                    for row in 0..rows {
                        for col in 0..cols {
                            {
                                let slot = chain.iter().find(|s| s.col == col && s.row == row);
                                let is_drag_target = hover_cell == Some((col, row)) && dragged_slot_id.is_some();
                                let is_being_dragged = slot.as_ref().map_or(false, |s| dragged_slot_id == Some(s.id));

                                if let Some(slot) = slot {
                                    let slot_id = slot.id;
                                    let bt = slot.block_type;
                                    let color = block_type_color(bt);
                                    let name = slot.block_preset_name.as_deref()
                                        .unwrap_or(bt.display_name());
                                    let is_selected = selected_slot_id == Some(slot_id);
                                    let slot_col = slot.col;
                                    let slot_row = slot.row;

                                    let cell_style = if is_being_dragged {
                                        // Ghost appearance for the dragged block's original position
                                        format!(
                                            "background-color: {}10; border-color: {}20; color: {}40; opacity: 0.4;",
                                            color.bg, color.bg, color.fg
                                        )
                                    } else if is_selected {
                                        format!(
                                            "background-color: {}25; border-color: {}; color: {};",
                                            color.bg, color.bg, color.fg
                                        )
                                    } else {
                                        format!(
                                            "background-color: {}15; border-color: {}40; color: {};",
                                            color.bg, color.bg, color.fg
                                        )
                                    };
                                    let dot_style = format!("background-color: {};", color.bg);

                                    // Always show ports on occupied cells for wire drafting
                                    let port_color = color.bg.to_string();

                                    // Port positions: centered vertically on cell edge
                                    let left_port_style = format!(
                                        "left: {}px; top: 50%; transform: translateY(-50%); \
                                         width: {}px; height: {}px; background-color: {};",
                                        -port_half as i32, PORT_SIZE, PORT_SIZE, port_color,
                                    );
                                    let right_port_style = format!(
                                        "right: {}px; top: 50%; transform: translateY(-50%); \
                                         width: {}px; height: {}px; background-color: {};",
                                        -port_half as i32, PORT_SIZE, PORT_SIZE, port_color,
                                    );

                                    // Is a wire draft hovering over this port?
                                    let left_port_hovered = hovered_port_slot() == Some((slot_id, true));
                                    let right_port_hovered = hovered_port_slot() == Some((slot_id, false));

                                    // If this slot's module group is being dragged, snap-translate it
                                    let slot_group_drag_tx = if let Some(ref gd) = group_drag() {
                                        if slot.module_group.as_deref() == Some(&gd.group_name) {
                                            let cz = zoom();
                                            let dx_px = (gd.mouse_x - gd.start_mouse_x) / cz;
                                            let dy_px = (gd.mouse_y - gd.start_mouse_y) / cz;
                                            let step = (CELL_SIZE + CELL_GAP) as f64;
                                            let snap_dx = (dx_px / step).round() * step;
                                            let snap_dy = (dy_px / step).round() * step;
                                            format!("transform: translate({snap_dx}px, {snap_dy}px); z-index: 50; opacity: 0.85;")
                                        } else {
                                            String::new()
                                        }
                                    } else {
                                        String::new()
                                    };

                                    rsx! {
                                        div {
                                            key: "{slot_id}",
                                            class: "relative aspect-square",
                                            style: "{slot_group_drag_tx}",
                                            // Occupied block cell — always square
                                            div {
                                                class: if is_being_dragged {
                                                    "absolute inset-0 flex flex-col items-center justify-center gap-1 \
                                                     rounded-lg border-2 border-dashed transition-all duration-100"
                                                } else {
                                                    "absolute inset-0 flex flex-col items-center justify-center gap-1 \
                                                     rounded-lg border-2 cursor-grab transition-all duration-100 \
                                                     hover:brightness-110 active:cursor-grabbing"
                                                },
                                                style: "{cell_style}",
                                                // Mousedown on block → start drag (not on ports)
                                                onmousedown: move |evt| {
                                                    evt.stop_propagation();
                                                    // Don't start drag if a picker is open
                                                    if PICKER_CELL.read().is_some() {
                                                        return;
                                                    }
                                                    props.on_select.call(Some(slot_id));
                                                    drag_state.set(Some(GridDragState {
                                                        slot_id,
                                                        origin_col: slot_col,
                                                        origin_row: slot_row,
                                                        start_mouse_x: evt.client_coordinates().x,
                                                        start_mouse_y: evt.client_coordinates().y,
                                                        mouse_x: evt.client_coordinates().x,
                                                        mouse_y: evt.client_coordinates().y,
                                                    }));
                                                    *PICKER_CELL.write() = None;
                                                },
                                                // Color dot + type label
                                                div { class: "flex items-center gap-1.5",
                                                    div {
                                                        class: "w-2.5 h-2.5 rounded-full flex-shrink-0",
                                                        style: "{dot_style}",
                                                    }
                                                    span {
                                                        class: "text-[9px] font-mono uppercase opacity-60 leading-none",
                                                        "{bt.display_name()}"
                                                    }
                                                }
                                                // Block name
                                                span {
                                                    class: "text-[11px] font-medium truncate max-w-full text-center px-1 leading-tight",
                                                    "{name}"
                                                }
                                            }
                                            // Left input port
                                            div {
                                                class: if left_port_hovered {
                                                    "absolute rounded-full border-2 border-cyan-400 z-10 cursor-crosshair shadow-[0_0_8px_rgba(34,211,238,0.6)]"
                                                } else {
                                                    "absolute rounded-full border border-white/40 z-10 cursor-crosshair hover:border-white/70 hover:shadow-[0_0_6px_rgba(255,255,255,0.3)] transition-all"
                                                },
                                                style: "{left_port_style}",
                                                onmousedown: move |evt| {
                                                    evt.stop_propagation();
                                                    // Start wire draft from input port
                                                    let pos = input_port_pos(slot_col, slot_row);
                                                    wire_draft.set(Some(GridWireDraft {
                                                        from_slot_id: slot_id,
                                                        from_pos: pos,
                                                        is_from_output: false,
                                                        mouse_pos: (evt.client_coordinates().x, evt.client_coordinates().y),
                                                    }));
                                                },
                                                onmouseenter: move |_| {
                                                    if wire_draft().is_some() {
                                                        hovered_port_slot.set(Some((slot_id, true)));
                                                    }
                                                },
                                                onmouseleave: move |_| {
                                                    if hovered_port_slot() == Some((slot_id, true)) {
                                                        hovered_port_slot.set(None);
                                                    }
                                                },
                                            }
                                            // Right output port
                                            div {
                                                class: if right_port_hovered {
                                                    "absolute rounded-full border-2 border-cyan-400 z-10 cursor-crosshair shadow-[0_0_8px_rgba(34,211,238,0.6)]"
                                                } else {
                                                    "absolute rounded-full border border-white/40 z-10 cursor-crosshair hover:border-white/70 hover:shadow-[0_0_6px_rgba(255,255,255,0.3)] transition-all"
                                                },
                                                style: "{right_port_style}",
                                                onmousedown: move |evt| {
                                                    evt.stop_propagation();
                                                    // Start wire draft from output port
                                                    let pos = output_port_pos(slot_col, slot_row);
                                                    wire_draft.set(Some(GridWireDraft {
                                                        from_slot_id: slot_id,
                                                        from_pos: pos,
                                                        is_from_output: true,
                                                        mouse_pos: (evt.client_coordinates().x, evt.client_coordinates().y),
                                                    }));
                                                },
                                                onmouseenter: move |_| {
                                                    if wire_draft().is_some() {
                                                        hovered_port_slot.set(Some((slot_id, false)));
                                                    }
                                                },
                                                onmouseleave: move |_| {
                                                    if hovered_port_slot() == Some((slot_id, false)) {
                                                        hovered_port_slot.set(None);
                                                    }
                                                },
                                            }
                                        }
                                    }
                                } else {
                                    // Empty cell — square placeholder
                                    let drop_highlight = is_drag_target;
                                    rsx! {
                                        div {
                                            key: "empty-{col}-{row}",
                                            class: "relative aspect-square",
                                            div {
                                                class: if drop_highlight {
                                                    "absolute inset-0 flex items-center justify-center \
                                                     rounded-lg border-2 border-dashed border-cyan-400/60 \
                                                     bg-cyan-400/10 transition-all duration-150"
                                                } else {
                                                    "absolute inset-0 flex items-center justify-center \
                                                     rounded-lg border border-dashed transition-all duration-150 cursor-pointer \
                                                     border-zinc-700/40 bg-zinc-800/10 hover:border-zinc-500/60 hover:bg-zinc-800/30 group"
                                                },
                                                onclick: move |evt| {
                                                    if drag_state().is_none() {
                                                        *PICKER_CELL.write() = Some((col, row));
                                                        *PICKER_CLICK_POS.write() = (
                                                            evt.client_coordinates().x,
                                                            evt.client_coordinates().y,
                                                        );
                                                    }
                                                },
                                                if drop_highlight {
                                                    span {
                                                        class: "text-cyan-400/60 text-xs font-mono",
                                                        "drop"
                                                    }
                                                } else {
                                                    span {
                                                        class: "text-zinc-600 text-sm opacity-40 group-hover:opacity-80 transition-opacity duration-150",
                                                        "+"
                                                    }
                                                }
                                            }
                                            // Picker is rendered at the top level (outside scaled grid)
                                            // to escape the CSS stacking context from transform: scale()
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Layer 3: Wire draft SVG overlay (above grid cells)
                if wire_draft().is_some() {
                    // Wire draft rendered in client-space overlay — purely visual feedback.
                }
                } // close inner natural-size div
            } // close canvas layer div

            // ── Zoom controls overlay (bottom-right) ─────────
            div {
                class: "absolute bottom-3 right-3 flex items-center gap-1.5 select-none",
                onmousedown: move |evt| evt.stop_propagation(),

                button {
                    class: "px-2.5 py-1 rounded-lg text-[10px] font-medium \
                            text-zinc-300 hover:text-white transition-colors",
                    style: "background-color: rgba(0,0,0,0.6); backdrop-filter: blur(8px);",
                    title: "Fit grid to view",
                    onclick: move |_| {
                        update_viewport(); // Re-measure before fitting
                        let padding = 20.0;
                        let avail_w = viewport_w() - padding * 2.0;
                        let avail_h = viewport_h() - padding * 2.0;
                        if fit_content_w > 0.0 && fit_content_h > 0.0 {
                            let fz = (avail_w / fit_content_w)
                                .min(avail_h / fit_content_h)
                                .clamp(0.1, 3.0);
                            let scaled_w = fit_content_w * fz;
                            let scaled_h = fit_content_h * fz;
                            pan_x.set((viewport_w() - scaled_w) / 2.0 - fit_content_offset_x * fz);
                            pan_y.set((viewport_h() - scaled_h) / 2.0 - fit_content_offset_y * fz);
                            zoom.set(fz);
                        }
                    },
                    "Fit"
                }

                button {
                    class: "px-1.5 py-1 rounded-lg text-[10px] font-medium \
                            text-zinc-300 hover:text-white transition-colors",
                    style: "background-color: rgba(0,0,0,0.6); backdrop-filter: blur(8px);",
                    onclick: move |_| {
                        zoom.set((zoom() / 1.2).clamp(0.1, 3.0));
                    },
                    "-"
                }

                div {
                    class: "px-2 py-1 rounded-lg text-[10px] font-mono text-zinc-400",
                    style: "background-color: rgba(0,0,0,0.6); backdrop-filter: blur(8px); \
                            min-width: 40px; text-align: center;",
                    "{(zoom() * 100.0) as i32}%"
                }

                button {
                    class: "px-1.5 py-1 rounded-lg text-[10px] font-medium \
                            text-zinc-300 hover:text-white transition-colors",
                    style: "background-color: rgba(0,0,0,0.6); backdrop-filter: blur(8px);",
                    onclick: move |_| {
                        zoom.set((zoom() * 1.2).clamp(0.1, 3.0));
                    },
                    "+"
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Block Picker Dropdown
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub(crate) struct BlockPickerDropdownProps {
    pub col: usize,
    pub row: usize,
    /// Click position (client coords) for fixed positioning.
    pub click_x: f64,
    pub click_y: f64,
    /// Called with the new CompositionSlot to add (parent handles chain mutation).
    pub on_add_slot: EventHandler<CompositionSlot>,
    pub on_close: EventHandler<()>,
}

// ── Picker subtab ────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PickerTab {
    All,
    Blocks,
    Modules,
}

/// Module type definitions for the picker (guitar signal chain order).
fn picker_module_types() -> Vec<(
    signal_control::module::ModuleType,
    &'static str,
    &'static str,
)> {
    use signal_control::module::ModuleType;
    vec![
        (ModuleType::Eq, "EQ", "Tone shaping"),
        (ModuleType::Dynamics, "Dynamics", "Comp, gate, limiter"),
        (ModuleType::Drive, "Drive", "Boost, OD, distortion"),
        (ModuleType::Amp, "Amp", "Amp + cabinet + room"),
        (ModuleType::PostEq, "Post EQ", "Post-amp shaping"),
        (
            ModuleType::Modulation,
            "Modulation",
            "Chorus, flanger, phaser",
        ),
        (ModuleType::Time, "Time", "Delay, reverb, freeze"),
        (ModuleType::Motion, "Motion", "Tremolo, vibrato, rotary"),
        (ModuleType::Special, "Special", "Wah, pitch, filter"),
        (ModuleType::Master, "Master", "Final output stage"),
    ]
}

/// Map module type to a display color (same mapping as module_editor_view).
fn module_type_color(
    mt: signal_control::module::ModuleType,
) -> crate::components::rig_grid::block_colors::BlockColor {
    use signal_control::block::BlockType;
    use signal_control::module::ModuleType;
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

/// Searchable dropdown for picking a block or module type to place on the grid.
///
/// Uses `position: fixed` with the click coordinates so it escapes
/// the CSS `transform: scale()` stacking context of the grid inner.
///
/// Has three subtabs: All (blocks + modules), Blocks only, Modules only.
#[component]
pub(crate) fn BlockPickerDropdown(props: BlockPickerDropdownProps) -> Element {
    let mut search = use_signal(String::new);
    let mut active_tab = use_signal(|| PickerTab::All);
    let col = props.col;
    let row = props.row;
    let tab = active_tab();

    let search_lower = search().to_lowercase();
    let all_types = predefined_block_types();
    let categories = block_type_categories();

    // Filter block types by search text
    let filtered_blocks: Vec<&BlockTypeDefinition> = all_types
        .iter()
        .filter(|def| {
            if search_lower.is_empty() {
                return true;
            }
            def.display_name.to_lowercase().contains(&search_lower)
                || def.category.to_lowercase().contains(&search_lower)
                || def.description.to_lowercase().contains(&search_lower)
        })
        .collect();

    // Filter module types by search text
    let module_types = picker_module_types();
    let filtered_modules: Vec<&(signal_control::module::ModuleType, &str, &str)> = module_types
        .iter()
        .filter(|(_, name, desc)| {
            if search_lower.is_empty() {
                return true;
            }
            name.to_lowercase().contains(&search_lower)
                || desc.to_lowercase().contains(&search_lower)
                || "module".contains(&search_lower)
        })
        .collect();

    let show_blocks = tab == PickerTab::All || tab == PickerTab::Blocks;
    let show_modules = tab == PickerTab::All || tab == PickerTab::Modules;
    let has_results = (show_blocks && !filtered_blocks.is_empty())
        || (show_modules && !filtered_modules.is_empty());

    // Stable input ID for JS focus
    let input_id = use_signal(|| format!("grid-picker-input-{}", Uuid::new_v4().as_simple()));
    let iid = input_id();

    // Position the dropdown near the click, clamped to viewport
    let left = props.click_x;
    let top = props.click_y;
    let panel_style = format!("position: fixed; left: {left}px; top: {top}px; z-index: 9999;",);

    // JS to focus the input after mount (autofocus doesn't work reliably in Dioxus WebView)
    let focus_js = format!(
        r#"(function(){{ var el = document.getElementById('{iid}'); if(el) el.focus(); }})()"#
    );

    rsx! {
        // Backdrop to catch outside clicks
        div {
            class: "fixed inset-0",
            style: "z-index: 9998;",
            onclick: move |_| props.on_close.call(()),
        }
        // Dropdown panel — fixed position at click coords
        div {
            class: "w-60 max-h-80 \
                    bg-zinc-900 border border-zinc-700/60 rounded-xl shadow-2xl shadow-black/50 \
                    flex flex-col overflow-hidden",
            style: "{panel_style}",
            onclick: move |evt| evt.stop_propagation(),
            onkeydown: move |evt| {
                if evt.key() == Key::Escape {
                    props.on_close.call(());
                }
                evt.stop_propagation();
            },
            // Subtab bar
            div { class: "flex items-center gap-0.5 px-2 pt-2 pb-1",
                {
                    let tabs = [
                        (PickerTab::All, "All"),
                        (PickerTab::Blocks, "Blocks"),
                        (PickerTab::Modules, "Modules"),
                    ];
                    rsx! {
                        for (t, label) in tabs {
                            button {
                                key: "{label}",
                                class: if tab == t {
                                    "px-2.5 py-1 rounded-md text-[10px] font-semibold text-zinc-200 bg-zinc-700/70 transition-colors"
                                } else {
                                    "px-2.5 py-1 rounded-md text-[10px] font-medium text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800/50 transition-colors"
                                },
                                onclick: move |_| active_tab.set(t),
                                "{label}"
                            }
                        }
                    }
                }
            }
            // Search input
            div { class: "px-3 py-1.5 border-b border-zinc-800/60",
                input {
                    id: "{iid}",
                    class: "w-full bg-zinc-800/80 border border-zinc-700/50 rounded-md px-2.5 py-1.5 \
                            text-[11px] text-zinc-200 outline-none focus:border-purple-500/40 \
                            placeholder:text-zinc-600 transition-all",
                    r#type: "text",
                    placeholder: if show_modules && !show_blocks { "Search modules..." } else if show_blocks && !show_modules { "Search blocks..." } else { "Search blocks & modules..." },
                    value: "{search}",
                    oninput: move |evt| search.set(evt.value().clone()),
                }
            }
            // Focus the search input after mount
            script { "{focus_js}" }
            // Results
            div { class: "flex-1 overflow-y-auto min-h-0 px-1.5 py-1.5",
                if !has_results {
                    div { class: "flex items-center justify-center py-4",
                        p { class: "text-[10px] text-zinc-600", "No results" }
                    }
                } else {
                    // Module types section
                    if show_modules && !filtered_modules.is_empty() {
                        div { class: "mb-2",
                            span {
                                class: "text-[8px] font-semibold text-zinc-600 uppercase tracking-[0.2em] px-1.5",
                                "Modules"
                            }
                            for (mt, name, desc) in filtered_modules.iter() {
                                {
                                    let mt = *mt;
                                    let name = *name;
                                    let desc = *desc;
                                    let color = module_type_color(mt);
                                    let dot_style = format!("background-color: {};", color.bg);
                                    rsx! {
                                        button {
                                            key: "mod-{name}",
                                            class: "w-full flex items-center gap-2 px-2 py-1.5 rounded-md text-left \
                                                    hover:bg-zinc-800/60 transition-all duration-100",
                                            onclick: move |_| {
                                                // Insert a module container as a Custom block
                                                // tagged with the module type name. The unified
                                                // grid editor will recognize this convention.
                                                let new_slot = CompositionSlot {
                                                    id: Uuid::new_v4(),
                                                    block_type: signal_control::block::BlockType::Custom,
                                                    block_preset_id: None,
                                                    block_preset_name: Some(format!("{} Module", name)),
                                                    plugin_name: Some(format!("module:{}", mt.display_name())),
                                                    col,
                                                    row,
                                                    module_group: None,
                                                    module_type: None,
                                                };
                                                props.on_add_slot.call(new_slot);
                                            },
                                            div {
                                                class: "w-2.5 h-2.5 rounded-sm flex-shrink-0",
                                                style: "{dot_style}",
                                            }
                                            div { class: "flex-1 min-w-0",
                                                span { class: "text-[11px] font-medium text-zinc-200 block", "{name}" }
                                                span { class: "text-[9px] text-zinc-500", "{desc}" }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    // Block types section (grouped by category)
                    if show_blocks && !filtered_blocks.is_empty() {
                        for category in categories.iter() {
                            {
                                let cat_items: Vec<&&BlockTypeDefinition> = filtered_blocks
                                    .iter()
                                    .filter(|d| d.category == *category)
                                    .collect();
                                if cat_items.is_empty() {
                                    return rsx! {};
                                }
                                rsx! {
                                    div { class: "mb-1.5",
                                        span {
                                            class: "text-[8px] font-semibold text-zinc-600 uppercase tracking-[0.2em] px-1.5",
                                            "{category}"
                                        }
                                        for def in cat_items.iter() {
                                            {
                                                let bt = def.block_type;
                                                let name = def.display_name;
                                                let desc = def.description;
                                                let color = block_type_color(bt);
                                                let dot_style = format!("background-color: {};", color.bg);
                                                rsx! {
                                                    button {
                                                        key: "{name}",
                                                        class: "w-full flex items-center gap-2 px-2 py-1.5 rounded-md text-left \
                                                                hover:bg-zinc-800/60 transition-all duration-100",
                                                        onclick: move |_| {
                                                            let new_slot = CompositionSlot {
                                                                id: Uuid::new_v4(),
                                                                block_type: bt,
                                                                block_preset_id: None,
                                                                block_preset_name: None,
                                                                plugin_name: None,
                                                                col,
                                                                row,
                                                                module_group: None,
                                                                module_type: None,
                                                            };
                                                            props.on_add_slot.call(new_slot);
                                                        },
                                                        div {
                                                            class: "w-2.5 h-2.5 rounded-full flex-shrink-0",
                                                            style: "{dot_style}",
                                                        }
                                                        div { class: "flex-1 min-w-0",
                                                            span { class: "text-[11px] font-medium text-zinc-200 block", "{name}" }
                                                            span { class: "text-[9px] text-zinc-500", "{desc}" }
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
