//! Freehand pen-stroke renderer. v1 renders existing strokes only;
//! the draw-tool itself is deferred (see the v1 limitations list in
//! the module-level docs of `whiteboard`).
//!
//! Points are stored in `CanvasNode.extras_json` as
//! `{"points": [[x, y], ...], "thickness": 2.0}`. Coordinates are
//! world-relative to `node.x`/`node.y` so the same stroke can be
//! translated by moving its parent block.

use dioxus::prelude::*;
use knowledge_proto::canvas::CanvasNode;
use serde::Deserialize;
use uuid::Uuid;

#[derive(Deserialize, Default, Debug)]
pub struct PenExtras {
    #[serde(default)]
    pub points: Vec<[f64; 2]>,
    #[serde(default = "default_thickness")]
    pub thickness: f64,
}

fn default_thickness() -> f64 {
    2.0
}

#[derive(Props, Clone, PartialEq)]
pub struct ShapePenProps {
    pub block_id: Uuid,
    pub node: CanvasNode,
    pub selected: bool,
}

#[component]
pub fn ShapePen(props: ShapePenProps) -> Element {
    let extras: PenExtras = props
        .node
        .extras_json
        .as_deref()
        .and_then(|s| serde_json::from_str(s).ok())
        .unwrap_or_default();

    if extras.points.len() < 2 {
        return rsx! { div {} };
    }
    let path = smooth_path(&extras.points);
    let stroke_class = if props.selected {
        "stroke-primary"
    } else {
        "stroke-foreground"
    };
    let style = format!(
        "left: {x}px; top: {y}px; width: {w}px; height: {h}px; overflow: visible;",
        x = props.node.x,
        y = props.node.y,
        w = props.node.w.max(1.0),
        h = props.node.h.max(1.0),
    );
    rsx! {
        svg { class: "absolute pointer-events-none", style: "{style}",
            path {
                class: "{stroke_class}",
                d: "{path}",
                fill: "none",
                stroke_width: "{extras.thickness}",
                stroke_linecap: "round",
                stroke_linejoin: "round",
            }
        }
    }
}

/// Cubic-Bezier smoothed path through a point sequence. Uses a
/// quadratic Catmull-Rom→Bezier conversion (one bezier per pair of
/// interior points).
fn smooth_path(points: &[[f64; 2]]) -> String {
    if points.is_empty() {
        return String::new();
    }
    let mut out = String::with_capacity(points.len() * 32);
    out.push_str(&format!("M {} {}", points[0][0], points[0][1]));
    if points.len() == 2 {
        out.push_str(&format!(" L {} {}", points[1][0], points[1][1]));
        return out;
    }
    for i in 1..points.len() {
        let p0 = points[(i as isize - 1).max(0) as usize];
        let p1 = points[i];
        let p_prev = points[(i as isize - 2).max(0) as usize];
        let p_next = points[(i + 1).min(points.len() - 1)];
        // Catmull-Rom → cubic Bezier control points.
        let c1 = [
            p0[0] + (p1[0] - p_prev[0]) / 6.0,
            p0[1] + (p1[1] - p_prev[1]) / 6.0,
        ];
        let c2 = [
            p1[0] - (p_next[0] - p0[0]) / 6.0,
            p1[1] - (p_next[1] - p0[1]) / 6.0,
        ];
        out.push_str(&format!(
            " C {} {} {} {} {} {}",
            c1[0], c1[1], c2[0], c2[1], p1[0], p1[1]
        ));
    }
    out
}
