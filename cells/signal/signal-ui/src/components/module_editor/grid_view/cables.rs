//! Cable resolution, SVG path generation, and CableLayer component for the grid view.
//!
//! Pure functions: chain of `CompositionSlot` → `Vec<Cable>` → SVG `<path>` data.
//! The `CableLayer` Dioxus component renders the resolved cables and module port dots.

use dioxus::prelude::*;

use super::super::module_editor_view::CompositionSlot;
use super::layout::{input_port_pos, output_port_pos, CELL_GAP, CELL_SIZE, GROUP_PAD};
use crate::components::rig_grid::block_colors::block_type_color;

// ─────────────────────────────────────────────────────────────────────────────
// Cable struct
// ─────────────────────────────────────────────────────────────────────────────

/// A resolved cable between two points in pixel space.
///
/// Coordinates are in grid-natural pixel space (before pan/zoom).
#[derive(Clone, PartialEq)]
pub(crate) struct Cable {
    pub(crate) from: (f64, f64),
    pub(crate) to: (f64, f64),
    pub(crate) color: String,
    /// When true, render as a straight line instead of a Bézier curve.
    pub(crate) straight: bool,
    /// When set, route through this Y coordinate with rounded corners
    /// (down/up → horizontal → up/down) instead of a direct path.
    pub(crate) route_y: Option<f64>,
    /// True when BOTH endpoints are bypassed — cable should be dimmed.
    pub(crate) bypassed: bool,
}

impl Cable {
    /// Standard Bézier cable between two points.
    pub(crate) fn new(from: (f64, f64), to: (f64, f64), color: String, bypassed: bool) -> Self {
        Self {
            from,
            to,
            color,
            straight: false,
            route_y: None,
            bypassed,
        }
    }

    /// Cable routed through a horizontal channel at the given Y coordinate.
    pub(crate) fn routed(
        from: (f64, f64),
        to: (f64, f64),
        color: String,
        route_y: f64,
        bypassed: bool,
    ) -> Self {
        Self {
            from,
            to,
            color,
            straight: false,
            route_y: Some(route_y),
            bypassed,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Module I/O ports (container edge dots)
// ─────────────────────────────────────────────────────────────────────────────

/// A virtual I/O port on a module container edge.
#[derive(Clone, PartialEq)]
pub(crate) struct ModulePort {
    pub(crate) pos: (f64, f64),
    pub(crate) color: String,
    pub(crate) bypassed: bool,
}

/// Compute module container I/O port positions for all multi-row modules.
///
/// Returns a list of `ModulePort` positioned at the left/right edge center
/// of the module container. Single-row modules are omitted since their block
/// ports serve the same role.
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
    for (name, min_c, max_c, min_r, max_r, color) in &group_map {
        let all_bypassed = {
            let slots: Vec<&CompositionSlot> = chain
                .iter()
                .filter(|s| s.module_group.as_deref() == Some(name))
                .collect();
            !slots.is_empty() && slots.iter().all(|s| s.bypassed)
        };

        // Input port: left edge center
        let in_x = *min_c as f64 * step - GROUP_PAD;
        let top = *min_r as f64 * step;
        let bottom = *max_r as f64 * step + CELL_SIZE as f64;
        let center_y = (top + bottom) / 2.0;
        ports.push(ModulePort {
            pos: (in_x, center_y),
            color: color.clone(),
            bypassed: all_bypassed,
        });

        // Output port: right edge center
        let out_x = *max_c as f64 * step + CELL_SIZE as f64 + GROUP_PAD;
        ports.push(ModulePort {
            pos: (out_x, center_y),
            color: color.clone(),
            bypassed: all_bypassed,
        });
    }

    ports
}

// ─────────────────────────────────────────────────────────────────────────────
// Internal module boundary info for wiring
// ─────────────────────────────────────────────────────────────────────────────

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

// ─────────────────────────────────────────────────────────────────────────────
// Cable resolution
// ─────────────────────────────────────────────────────────────────────────────

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
                cables.push(Cable::new(
                    output_port_pos(a.col, a.row),
                    input_port_pos(b.col, b.row),
                    color,
                    a.bypassed && b.bypassed,
                ));
            }
        }
    }

    // ── 2. Fan-out / fan-in for multi-row modules ────────────────
    // Module input point → each left-edge block input.
    // Each right-edge block output → module output point.
    // For modules with an empty center row, add a pass-through cable.

    // Helper: check if all blocks in a module are bypassed
    let module_all_bypassed = |name: &str| -> bool {
        let slots: Vec<&CompositionSlot> = chain
            .iter()
            .filter(|s| s.module_group.as_deref() == Some(name))
            .collect();
        !slots.is_empty() && slots.iter().all(|s| s.bypassed)
    };
    // Helper: check if a specific block at (col, row) is bypassed
    let block_bypassed_at = |col: usize, row: usize| -> bool {
        chain
            .iter()
            .find(|s| s.col == col && s.row == row)
            .map_or(false, |s| s.bypassed)
    };

    for m in &modules {
        if m.is_multi_row() {
            let mod_bypassed = module_all_bypassed(&m.name);
            let mod_in = m.input_point();
            for &(col, row) in &m.left_edge {
                cables.push(Cable::new(
                    mod_in,
                    input_port_pos(col, row),
                    m.color.clone(),
                    mod_bypassed || block_bypassed_at(col, row),
                ));
            }

            let mod_out = m.output_point();
            for &(col, row) in &m.right_edge {
                cables.push(Cable::new(
                    output_port_pos(col, row),
                    mod_out,
                    m.color.clone(),
                    mod_bypassed || block_bypassed_at(col, row),
                ));
            }

            // Pass-through: if the center row has no blocks, draw a straight
            // cable from module input → module output (raw signal bypass lane).
            let center_row = (m.min_row + m.max_row) / 2;
            let has_center_block = chain
                .iter()
                .any(|s| s.module_group.as_deref() == Some(&m.name) && s.row == center_row);
            if !has_center_block && m.max_row - m.min_row >= 2 {
                cables.push(Cable::new(mod_in, mod_out, m.color.clone(), mod_bypassed));
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
            let both_bypassed =
                module_all_bypassed(&from_mod.name) && module_all_bypassed(&to_mod.name);

            // Check if both modules overlap in row range (same horizontal band)
            let rows_overlap =
                from_mod.min_row <= to_mod.max_row && to_mod.min_row <= from_mod.max_row;

            if rows_overlap {
                // Same row band — direct horizontal cable
                cables.push(Cable::new(from_pt, to_pt, color, both_bypassed));
            } else {
                // Cross-row: route through the gap just below the upper row band.
                let upper_bottom_row = from_mod.max_row.min(to_mod.max_row);
                // Channel Y = bottom of that row's cell + small offset into gap
                let channel_y =
                    upper_bottom_row as f64 * step + CELL_SIZE as f64 + CELL_GAP as f64 * 0.25;

                cables.push(Cable::routed(
                    from_pt,
                    to_pt,
                    color,
                    channel_y,
                    both_bypassed,
                ));
            }
        }
    }

    cables
}

/// Resolve cables from explicit connections if present, otherwise from adjacency.
pub(crate) fn resolve_cables_or_connections(
    chain: &[CompositionSlot],
    connections: &[super::GridConnection],
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
            Some(Cable::new(
                output_port_pos(from.col, from.row),
                input_port_pos(to.col, to.row),
                color,
                from.bypassed && to.bypassed,
            ))
        })
        .collect()
}

// ─────────────────────────────────────────────────────────────────────────────
// SVG path generation
// ─────────────────────────────────────────────────────────────────────────────

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
pub(crate) fn routed_cable_path(from: (f64, f64), to: (f64, f64), channel_y: f64) -> String {
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
// CableLayer component
// ─────────────────────────────────────────────────────────────────────────────

/// Props for the SVG cable + port dot overlay.
#[derive(Props, Clone, PartialEq)]
pub(super) struct CableLayerProps {
    pub cables: Vec<Cable>,
    pub module_ports: Vec<ModulePort>,
    pub nat_w: f64,
    pub nat_h: f64,
}

/// SVG overlay rendering cables between block ports and module I/O port dots.
///
/// Positioned at the grid's natural size, behind all grid cells (z-index: 0).
/// `pointer-events: none` so clicks pass through to the grid layer.
#[component]
pub(super) fn CableLayer(props: CableLayerProps) -> Element {
    let nat_w = props.nat_w;
    let nat_h = props.nat_h;

    rsx! {
        div {
            style: "position: absolute; left: 0; top: 0; width: {nat_w}px; height: {nat_h}px; \
                    z-index: 0; pointer-events: none; overflow: visible;",
            svg {
                style: "overflow: visible;",
                width: "{nat_w}",
                height: "{nat_h}",
                view_box: "0 0 {nat_w} {nat_h}",

                for cable in props.cables.iter() {
                    {
                        let d = if let Some(ry) = cable.route_y {
                            routed_cable_path(cable.from, cable.to, ry)
                        } else if cable.straight {
                            format!("M {},{} L {},{}", cable.from.0, cable.from.1, cable.to.0, cable.to.1)
                        } else {
                            cable_path_d(cable.from, cable.to)
                        };
                        let stroke = cable.color.clone();
                        let opacity = if cable.bypassed { "0.15" } else { "0.7" };
                        rsx! {
                            path {
                                d: "{d}",
                                fill: "none",
                                stroke: "{stroke}",
                                stroke_width: "2.5",
                                stroke_opacity: "{opacity}",
                                stroke_linecap: "round",
                            }
                        }
                    }
                }

                // Module I/O port dots (on container edges)
                for port in props.module_ports.iter() {
                    {
                        let cx = port.pos.0;
                        let cy = port.pos.1;
                        let fill = port.color.clone();
                        let fill_op = if port.bypassed { "0.15" } else { "0.8" };
                        let stroke_op = if port.bypassed { "0.08" } else { "0.4" };
                        rsx! {
                            circle {
                                cx: "{cx}",
                                cy: "{cy}",
                                r: "4",
                                fill: "{fill}",
                                fill_opacity: "{fill_op}",
                                stroke: "{fill}",
                                stroke_width: "1.5",
                                stroke_opacity: "{stroke_op}",
                            }
                        }
                    }
                }
            }
        }
    }
}
