//! SVG connector renderer. The polyline + endpoint data are computed
//! upstream by `coords::connector_path` / `hit::connector_path`; this
//! widget just paints the result inside the canvas's transformed SVG
//! overlay. Arrowheads, dash patterns, and label chip styling all
//! live here so the parent Whiteboard component stays small.

use dioxus::prelude::*;
use knowledge_proto::canvas::{ArrowHead, ConnectorLine, ConnectorStyle};

#[derive(Props, Clone, PartialEq)]
pub struct ConnectorProps {
    /// Polyline points in world coords.
    pub points: Vec<(f64, f64)>,
    pub style: ConnectorStyle,
    #[props(default)]
    pub label: Option<String>,
    #[props(default = false)]
    pub selected: bool,
    /// Unique key for the SVG `<marker>` ids — prevents collisions when
    /// many connectors share an SVG root.
    pub key_id: String,
}

#[component]
pub fn Connector(props: ConnectorProps) -> Element {
    if props.points.len() < 2 {
        return rsx! { g {} };
    }
    let d = polyline_to_d(&props.points);
    let dash = match props.style.line {
        ConnectorLine::Solid => "",
        ConnectorLine::Dashed => "8 4",
        ConnectorLine::Dotted => "2 4",
    };
    let stroke_class = if props.selected {
        "stroke-primary"
    } else {
        "stroke-foreground"
    };
    let start_marker_id = format!("c-start-{}", props.key_id);
    let end_marker_id = format!("c-end-{}", props.key_id);
    let start_ref = if props.style.start_arrow == ArrowHead::None {
        String::new()
    } else {
        format!("url(#{start_marker_id})")
    };
    let end_ref = if props.style.end_arrow == ArrowHead::None {
        String::new()
    } else {
        format!("url(#{end_marker_id})")
    };

    let mid = mid_point(&props.points);

    rsx! {
        g {
            defs {
                Marker { id: start_marker_id.clone(), kind: props.style.start_arrow }
                Marker { id: end_marker_id.clone(), kind: props.style.end_arrow }
            }
            path {
                class: "{stroke_class} fill-none",
                d: "{d}",
                stroke_width: "2",
                stroke_dasharray: "{dash}",
                marker_start: "{start_ref}",
                marker_end: "{end_ref}",
            }
            if let Some(label) = props.label.clone() {
                if let Some((mx, my)) = mid {
                    // Approx 6px per char, 18px tall — kept very small,
                    // the real layout will improve once we measure text.
                    foreignObject {
                        x: "{mx - (label.len() as f64 * 3.0)}",
                        y: "{my - 10.0}",
                        width: "{(label.len() as f64 * 7.0).max(40.0)}",
                        height: "20",
                        div {
                            class: "inline-flex items-center justify-center px-1.5 text-[10px] rounded border border-border bg-background text-foreground",
                            "{label}"
                        }
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct MarkerProps {
    id: String,
    kind: ArrowHead,
}

#[component]
fn Marker(props: MarkerProps) -> Element {
    let id = props.id;
    match props.kind {
        ArrowHead::None => rsx! { g {} },
        ArrowHead::Triangle => rsx! {
            marker {
                id: "{id}",
                view_box: "0 0 10 10",
                ref_x: "9",
                ref_y: "5",
                marker_width: "8",
                marker_height: "8",
                orient: "auto-start-reverse",
                path { d: "M 0 0 L 10 5 L 0 10 z", class: "fill-foreground" }
            }
        },
        ArrowHead::Diamond => rsx! {
            marker {
                id: "{id}",
                view_box: "0 0 10 10",
                ref_x: "5",
                ref_y: "5",
                marker_width: "10",
                marker_height: "10",
                orient: "auto-start-reverse",
                path { d: "M 0 5 L 5 0 L 10 5 L 5 10 z", class: "fill-foreground" }
            }
        },
        ArrowHead::Circle => rsx! {
            marker {
                id: "{id}",
                view_box: "0 0 10 10",
                ref_x: "5",
                ref_y: "5",
                marker_width: "8",
                marker_height: "8",
                orient: "auto-start-reverse",
                circle { cx: "5", cy: "5", r: "4", class: "fill-foreground" }
            }
        },
        ArrowHead::Bar => rsx! {
            marker {
                id: "{id}",
                view_box: "0 0 10 10",
                ref_x: "5",
                ref_y: "5",
                marker_width: "4",
                marker_height: "10",
                orient: "auto-start-reverse",
                rect { x: "4", y: "0", width: "2", height: "10", class: "fill-foreground" }
            }
        },
    }
}

fn polyline_to_d(pts: &[(f64, f64)]) -> String {
    let mut out = String::new();
    for (i, p) in pts.iter().enumerate() {
        if i == 0 {
            out.push_str(&format!("M {} {}", p.0, p.1));
        } else {
            out.push_str(&format!(" L {} {}", p.0, p.1));
        }
    }
    out
}

fn mid_point(pts: &[(f64, f64)]) -> Option<(f64, f64)> {
    if pts.is_empty() {
        return None;
    }
    // Find the midpoint along total polyline length.
    let mut total = 0.0;
    for w in pts.windows(2) {
        total += dist(w[0], w[1]);
    }
    let target = total / 2.0;
    let mut acc = 0.0;
    for w in pts.windows(2) {
        let d = dist(w[0], w[1]);
        if acc + d >= target {
            let t = if d == 0.0 { 0.0 } else { (target - acc) / d };
            return Some((
                w[0].0 + (w[1].0 - w[0].0) * t,
                w[0].1 + (w[1].1 - w[0].1) * t,
            ));
        }
        acc += d;
    }
    pts.last().copied()
}

fn dist(a: (f64, f64), b: (f64, f64)) -> f64 {
    ((b.0 - a.0).powi(2) + (b.1 - a.1).powi(2)).sqrt()
}
