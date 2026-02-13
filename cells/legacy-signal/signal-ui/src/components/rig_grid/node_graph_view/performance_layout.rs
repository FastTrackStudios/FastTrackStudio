//! Performance-mode graph layout engine.
//!
//! Converts a free-form `NodeGraph` into a rack-style layout that fills the
//! viewport, similar to a Quad Cortex performance view. Modules are sorted
//! top-to-bottom, left-to-right, then packed into rows ("racks") based on
//! aspect-ratio heuristics.

use super::super::node_graph::{GraphModule, NodeGraph, NodeWidget};

// ── Public entry point ───────────────────────────────────────────────

pub(crate) fn build_performance_graph(
    original: &NodeGraph,
    viewport_width: f64,
    viewport_height: f64,
) -> NodeGraph {
    let mut graph = original.clone();
    if graph.modules.is_empty() {
        return graph;
    }

    // FastTrackStudio performance_view-style rack layout.
    let mut ordered: Vec<usize> = (0..graph.modules.len()).collect();
    ordered.sort_by(|a, b| {
        graph.modules[*a]
            .position
            .y
            .partial_cmp(&graph.modules[*b].position.y)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| {
                graph.modules[*a]
                    .position
                    .x
                    .partial_cmp(&graph.modules[*b].position.x)
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
    });

    let aspects: Vec<f64> = ordered
        .iter()
        .map(|idx| preferred_aspect_ratio(&graph.modules[*idx]))
        .collect();
    let viewport_aspect = (viewport_width / viewport_height).max(0.5);
    let racks = layout_racks(&ordered, &aspects, viewport_aspect);

    let outer_pad = 0.0;
    let col_gap = 0.0;
    let row_gap = 0.0;
    let avail_w = (viewport_width - outer_pad * 2.0).max(300.0);

    let mut y = outer_pad;
    for rack in &racks {
        let rack_h = 220.0;

        let total_aspect = rack.total_aspect_ratio.max(0.001);
        let mut x = outer_pad;
        for (slot, idx) in rack.modules.iter().enumerate() {
            let remaining = rack.modules.len().saturating_sub(slot + 1);
            let a = preferred_aspect_ratio(&graph.modules[*idx]);
            let mut w = (avail_w * (a / total_aspect)).max(150.0);
            if remaining == 0 {
                // Ensure each rack always fills to the right edge exactly.
                w = (outer_pad + avail_w - x).max(60.0);
            } else {
                // Reserve minimum width for remaining modules to avoid overflow.
                let reserve_for_rest = 60.0 * remaining as f64;
                let max_w = (outer_pad + avail_w - x - reserve_for_rest).max(60.0);
                w = w.clamp(60.0, max_w);
            }

            let module = &mut graph.modules[*idx];
            module.position.x = x;
            module.position.y = y;
            module.size.width = w;
            module.size.height = rack_h;

            layout_module_nodes_for_performance(module, false);
            x += w + col_gap;
        }

        y += rack_h + row_gap;
    }

    graph
}

// ── Rack data ────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct Rack {
    modules: Vec<usize>,
    total_aspect_ratio: f64,
}

// ── Aspect ratio heuristics ──────────────────────────────────────────

fn preferred_aspect_ratio(module: &GraphModule) -> f64 {
    if module.nodes.len() == 1 {
        let node = &module.nodes[0];
        return match node.widget {
            NodeWidget::CompressorGraph => 1.0,
            NodeWidget::EqGraph => 1.5,
            NodeWidget::Label => {
                if module.name.to_lowercase().contains("volume") {
                    0.5
                } else {
                    1.0
                }
            }
            _ => 1.0,
        };
    }

    let name = module.name.to_lowercase();
    if name.contains("amp") || name.contains("cab") {
        return 1.8;
    }
    if name.contains("special") || name.contains("drive") {
        return 2.5;
    }
    1.5
}

// ── Rack packing ─────────────────────────────────────────────────────

fn layout_racks(ordered: &[usize], aspects: &[f64], viewport_aspect: f64) -> Vec<Rack> {
    let mut racks = Vec::new();
    let mut current = Rack {
        modules: Vec::new(),
        total_aspect_ratio: 0.0,
    };

    for (i, idx) in ordered.iter().enumerate() {
        let a = aspects[i];
        let can_fit =
            current.modules.len() < 4 && current.total_aspect_ratio < viewport_aspect * 1.2;
        if can_fit || current.modules.is_empty() {
            current.modules.push(*idx);
            current.total_aspect_ratio += a;
        } else {
            racks.push(current);
            current = Rack {
                modules: vec![*idx],
                total_aspect_ratio: a,
            };
        }
    }

    if !current.modules.is_empty() {
        racks.push(current);
    }
    racks
}

// ── Per-module node layout ───────────────────────────────────────────

fn layout_module_nodes_for_performance(module: &mut GraphModule, fit_all: bool) {
    if module.nodes.is_empty() {
        return;
    }

    let content_title_h = 28.0;
    let pad = if fit_all { 2.0 } else { 4.0 };
    let content_w = (module.size.width - pad * 2.0).max(1.0);
    let content_h = (module.size.height - content_title_h - pad * 2.0).max(1.0);

    if module.nodes.len() == 1 {
        let node = &mut module.nodes[0];
        node.position.x = pad;
        node.position.y = pad;
        node.size.width = content_w;
        node.size.height = content_h;
        return;
    }

    // Match FastTrackStudio performance_view grouping by deriving logical
    // row/column anchors.
    let mut all_y_positions: Vec<f64> = module.nodes.iter().map(|n| n.position.y).collect();
    all_y_positions.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    all_y_positions.dedup_by(|a, b| (*a - *b).abs() < 10.0);

    let mut y_positions: Vec<f64> = all_y_positions
        .iter()
        .filter(|&&y| {
            module
                .nodes
                .iter()
                .filter(|n| (n.position.y - y).abs() < 10.0)
                .count()
                >= 2
        })
        .copied()
        .collect();
    if y_positions.is_empty() {
        y_positions = all_y_positions;
    }

    let mut x_positions: Vec<f64> = module.nodes.iter().map(|n| n.position.x).collect();
    x_positions.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    x_positions.dedup_by(|a, b| (*a - *b).abs() < 10.0);

    let row_count = y_positions.len().max(1);
    let col_count = x_positions.len().max(1);
    let cell_w = content_w / col_count as f64;
    let cell_h = content_h / row_count as f64;
    let cell_gap = if fit_all { 1.0 } else { 2.0 };

    for idx in 0..module.nodes.len() {
        let node_y = module.nodes[idx].position.y;
        let node_bottom = node_y + module.nodes[idx].size.height;
        let node_x = module.nodes[idx].position.x;

        let start_row = y_positions
            .iter()
            .position(|&y| node_y >= y - 10.0 && node_y <= y + 10.0)
            .or_else(|| y_positions.iter().position(|&y| node_y >= y))
            .unwrap_or(0);

        let end_row = y_positions
            .iter()
            .enumerate()
            .rev()
            .find(|(_, y)| node_bottom >= *y + 10.0)
            .map(|(i, _)| i + 1)
            .unwrap_or(start_row + 1)
            .clamp(start_row + 1, row_count);

        let start_col = x_positions
            .iter()
            .position(|&x| (node_x - x).abs() < 10.0)
            .unwrap_or(0)
            .min(col_count.saturating_sub(1));

        let span_rows = (end_row - start_row).max(1) as f64;
        let width = (cell_w - cell_gap).max(if fit_all { 36.0 } else { 52.0 });
        let height = (cell_h * span_rows - cell_gap).max(if fit_all { 28.0 } else { 36.0 });
        let x = pad + start_col as f64 * cell_w;
        let y = pad + start_row as f64 * cell_h;

        let node = &mut module.nodes[idx];
        node.position.x = x;
        node.position.y = y;
        node.size.width = width;
        node.size.height = height;
    }
}
