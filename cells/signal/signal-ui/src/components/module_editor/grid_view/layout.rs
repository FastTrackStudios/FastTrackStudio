//! Pure grid geometry functions — cell positions, module bounds, coordinate
//! conversions, collision detection. No rendering, no signals, no side effects.

use super::super::module_editor_view::CompositionSlot;
use crate::components::rig_grid::block_colors::{block_type_color, BlockColor};

// ─────────────────────────────────────────────────────────────────────────────
// Grid sizing constants
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

/// Padding around the cell bounding box for the group container (px).
/// 25% of the gap between cells, so adjacent modules have clear separation.
pub(crate) const GROUP_PAD: f64 = CELL_GAP as f64 * 0.25;
/// Height of the title bar above the cells.
pub(crate) const GROUP_TITLE_H: f64 = 16.0;

// ─────────────────────────────────────────────────────────────────────────────
// Grid bounds
// ─────────────────────────────────────────────────────────────────────────────

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
// Port positions
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

// ─────────────────────────────────────────────────────────────────────────────
// Module group bounding boxes
// ─────────────────────────────────────────────────────────────────────────────

/// A resolved module group — bounding rectangle in pixel space + color info.
pub(crate) struct ModuleGroupRect {
    /// Display name for the title bar.
    pub(crate) name: String,
    /// ModuleType color (bg, fg, border).
    pub(crate) color: BlockColor,
    /// Pixel position and size of the container (in grid-natural coords).
    pub(crate) x: f64,
    pub(crate) y: f64,
    pub(crate) w: f64,
    pub(crate) h: f64,
}

/// Compute module group bounding rectangles from the chain.
///
/// Groups are identified by `CompositionSlot::module_group`. For each group,
/// we find the min/max col/row of its member slots, convert to pixel coords,
/// and add padding + a title bar strip above.
pub(crate) fn compute_module_groups(chain: &[CompositionSlot]) -> Vec<ModuleGroupRect> {
    use signal_control::module::ModuleType;
    use std::collections::BTreeMap;

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
            let cell_x = g.min_col as f64 * step;
            let cell_y = g.min_row as f64 * step;
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
// Coordinate conversion (grid ↔ node graph)
// ─────────────────────────────────────────────────────────────────────────────

/// Convert a grid (col, row) to pixel coordinates for the node graph view.
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
// Collision detection
// ─────────────────────────────────────────────────────────────────────────────

/// Check if moving a module group by (col_delta, row_delta) would cause any of
/// its slots to overlap with slots belonging to other groups.
/// Returns `true` if the move is **valid** (no collision).
pub(crate) fn group_move_is_valid(
    chain: &[CompositionSlot],
    group_name: &str,
    col_delta: isize,
    row_delta: isize,
) -> bool {
    use std::collections::HashSet;

    let occupied: HashSet<(usize, usize)> = chain
        .iter()
        .filter(|s| s.module_group.as_deref() != Some(group_name))
        .map(|s| (s.col, s.row))
        .collect();

    for s in chain.iter() {
        if s.module_group.as_deref() != Some(group_name) {
            continue;
        }
        let new_col = s.col as isize + col_delta;
        let new_row = s.row as isize + row_delta;
        if new_col < 0 || new_row < 0 {
            continue;
        }
        if occupied.contains(&(new_col as usize, new_row as usize)) {
            return false;
        }
    }
    true
}

// ─────────────────────────────────────────────────────────────────────────────
// Module type → block color mapping
// ─────────────────────────────────────────────────────────────────────────────

/// Map a ModuleType to its display color by delegating to the block color palette.
pub(crate) fn module_type_color(mt: signal_control::module::ModuleType) -> BlockColor {
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
