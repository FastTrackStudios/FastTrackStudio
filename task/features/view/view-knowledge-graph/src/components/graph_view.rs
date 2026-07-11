//! `KnowledgeGraphView` — dumb SVG renderer for a built [`WikiGraph`].
//!
//! Data in via props, interaction out via `on_node_click`. The layout
//! is computed with [`crate::layout`] inside a memo (deterministic, so
//! it only recomputes when the graph / spacing / scale actually
//! change). The viewBox derives from the layout bounds, so the first
//! render always fits the whole graph (zoom-to-fit); "Reset view"
//! returns to that fit. Pan is drag-to-move; zoom is the wheel or the
//! on-canvas buttons. Label density is zoom-aware: zoomed out only the
//! biggest hubs keep labels (fading in near the threshold), zooming in
//! progressively labels smaller nodes; hover and search highlights are
//! always labeled. Hover dims everything that isn't the hovered node
//! or its neighbors; `highlighted` (e.g. search matches) takes
//! precedence over hover.
//!
//! Colors come from Tailwind palette utilities (`text-{stem}-400` +
//! `fill-current`) rather than raw hex, so the graph respects the theme
//! system. Stems are interpolated, so the project's Tailwind safelist
//! must cover the color stems in [`crate::model`] (same requirement the
//! heatmap view already relies on).

use std::collections::{HashMap, HashSet};

use dioxus::prelude::*;
use dioxus_elements::geometry::WheelDelta;
use fts_ui::lucide_dioxus::{Maximize, ZoomIn, ZoomOut};

use crate::layout::{LayoutConfig, Position, bounds, layout, node_radius};
use crate::model::{ColorMode, WikiGraph, community_text_class, kind_text_class};

/// Density score (degree ratio × 4 × zoom) above which a node's label
/// renders. At zoom 1 a node needs ≥ a quarter-ish of the biggest
/// hub's degree (scaled by [`LABEL_FADE_BAND`]) to be labeled.
const LABEL_THRESHOLD: f32 = 0.6;
/// Width of the fade-in band above [`LABEL_THRESHOLD`] — labels ramp
/// from faint to full opacity across it.
const LABEL_FADE_BAND: f32 = 0.4;

#[derive(Props, Clone, PartialEq)]
pub struct KnowledgeGraphViewProps {
    /// The built graph (see [`crate::build::build_wiki_graph`]).
    pub graph: WikiGraph,
    /// Color nodes by kind or community.
    #[props(default = ColorMode::Kind)]
    pub color_mode: ColorMode,
    /// User node-size multiplier.
    #[props(default = 1.0)]
    pub node_scale: f32,
    /// User spacing multiplier passed to the force layout.
    #[props(default = 1.0)]
    pub spacing: f32,
    /// Node ids to emphasize (e.g. search matches). Non-empty takes
    /// precedence over hover dimming.
    #[props(default)]
    pub highlighted: Vec<String>,
    /// Fired with the node id when a node is clicked.
    pub on_node_click: EventHandler<String>,
}

#[component]
pub fn KnowledgeGraphView(props: KnowledgeGraphViewProps) -> Element {
    let graph = props.graph.clone();
    let spacing = props.spacing;
    let node_scale = props.node_scale;
    let color_mode = props.color_mode;

    // Owned clones the memos take by move (use_reactive! moves its
    // captures and only re-runs when a captured dep's value changes).
    let layout_graph = props.graph.clone();
    let adj_graph = props.graph.clone();

    // Deterministic layout — only recomputes when inputs change.
    let positions: Memo<HashMap<String, Position>> =
        use_memo(use_reactive!(|(layout_graph, spacing)| {
            layout(
                &layout_graph.nodes,
                &layout_graph.edges,
                LayoutConfig {
                    spacing,
                    iterations: None,
                },
            )
        }));

    // Adjacency for hover-neighbor highlighting.
    let adjacency: Memo<HashMap<String, HashSet<String>>> =
        use_memo(use_reactive!(|(adj_graph,)| {
            let mut adj: HashMap<String, HashSet<String>> = HashMap::new();
            for e in &adj_graph.edges {
                adj.entry(e.source.clone())
                    .or_default()
                    .insert(e.target.clone());
                adj.entry(e.target.clone())
                    .or_default()
                    .insert(e.source.clone());
            }
            adj
        }));

    let mut hovered: Signal<Option<String>> = use_signal(|| None);
    let mut zoom: Signal<f32> = use_signal(|| 1.0_f32);
    let mut pan: Signal<(f32, f32)> = use_signal(|| (0.0_f32, 0.0));
    let mut drag: Signal<Option<(f64, f64)>> = use_signal(|| None);

    let pmap = positions.read().clone();
    if graph.nodes.is_empty() {
        return rsx! {
            div { class: "flex h-full w-full items-center justify-center text-sm text-muted-foreground",
                "No pages to graph yet."
            }
        };
    }

    // viewBox from layout bounds + zoom/pan. At the initial
    // `zoom = 1.0, pan = (0, 0)` this is exactly zoom-to-fit: the
    // padded bounding box of every node, centered (and
    // `preserveAspectRatio: meet` letterboxes it into the container).
    let (min_x, min_y, max_x, max_y) = bounds(&pmap);
    let raw_w = (max_x - min_x).max(1.0);
    let raw_h = (max_y - min_y).max(1.0);
    let pad = raw_w.max(raw_h) * 0.12;
    let base_w = raw_w + pad * 2.0;
    let base_h = raw_h + pad * 2.0;
    let cx = f32::midpoint(min_x, max_x);
    let cy = f32::midpoint(min_y, max_y);
    let z = (*zoom.read()).max(0.05);
    let (px, py) = *pan.read();
    let vw = base_w / z;
    let vh = base_h / z;
    let vx = cx - vw / 2.0 + px;
    let vy = cy - vh / 2.0 + py;
    let view_box = format!("{vx} {vy} {vw} {vh}");

    // Scale factors mapping the node-size model into layout units.
    let k = (1_000_000.0_f32 * spacing.max(0.2) / graph.nodes.len() as f32).sqrt();
    let radius_unit = k / 48.0;
    let edge_unit = k / 90.0;
    let pan_px_factor = vw / 800.0; // approx layout-units per screen px

    let max_links = graph.nodes.iter().map(|n| n.link_count).max().unwrap_or(1);
    let max_weight = graph.edges.iter().map(|e| e.weight).fold(1.0_f32, f32::max);

    // Active set: search highlight wins, else hovered + neighbors.
    let highlight_set: HashSet<String> = props.highlighted.iter().cloned().collect();
    let hov = hovered.read().clone();
    let adj = adjacency.read();
    let active: Option<HashSet<String>> = if !highlight_set.is_empty() {
        Some(highlight_set)
    } else if let Some(h) = &hov {
        let mut s: HashSet<String> = HashSet::new();
        s.insert(h.clone());
        if let Some(neigh) = adj.get(h) {
            s.extend(neigh.iter().cloned());
        }
        Some(s)
    } else {
        None
    };
    let has_active = active.is_some();

    rsx! {
        div { class: "relative h-full w-full overflow-hidden bg-background",
            svg {
                class: "h-full w-full cursor-grab active:cursor-grabbing select-none",
                view_box: "{view_box}",
                preserve_aspect_ratio: "xMidYMid meet",
                onmousedown: move |evt| {
                    let p = evt.client_coordinates();
                    drag.set(Some((p.x, p.y)));
                },
                onmousemove: move |evt| {
                    let current = *drag.peek();
                    if let Some((lx, ly)) = current {
                        let p = evt.client_coordinates();
                        let dx = (p.x - lx) as f32 * pan_px_factor;
                        let dy = (p.y - ly) as f32 * pan_px_factor;
                        pan.with_mut(|(px, py)| {
                            *px -= dx;
                            *py -= dy;
                        });
                        drag.set(Some((p.x, p.y)));
                    }
                },
                onmouseup: move |_| drag.set(None),
                onmouseleave: move |_| {
                    drag.set(None);
                    hovered.set(None);
                },
                onwheel: move |evt| {
                    // Scroll up (negative ΔY) zooms in, down zooms out.
                    let dy = match evt.delta() {
                        WheelDelta::Pixels(v) => v.y,
                        WheelDelta::Lines(v) => v.y * 16.0,
                        WheelDelta::Pages(v) => v.y * 400.0,
                    };
                    let factor = (-dy as f32 * 0.0015).exp();
                    zoom.with_mut(|z| *z = (*z * factor).clamp(0.2, 8.0));
                },

                // Edges first (drawn under nodes).
                for (i, edge) in graph.edges.iter().enumerate() {
                    match (pmap.get(&edge.source), pmap.get(&edge.target)) {
                        (Some(a), Some(b)) => {
                            let nw = (edge.weight / max_weight).clamp(0.0, 1.0);
                            let width = (0.5 + nw * 3.5) * edge_unit;
                            let edge_active = active
                                .as_ref()
                                .is_none_or(|s| s.contains(&edge.source) && s.contains(&edge.target));
                            let opacity = if !has_active || edge_active { 0.5 } else { 0.06 };
                            rsx! {
                                line {
                                    key: "e{i}",
                                    x1: "{a.x}", y1: "{a.y}", x2: "{b.x}", y2: "{b.y}",
                                    class: "stroke-current text-muted-foreground",
                                    stroke_width: "{width}",
                                    stroke_opacity: "{opacity}",
                                }
                            }
                        }
                        _ => rsx! {},
                    }
                }

                // Nodes.
                for node in graph.nodes.iter() {
                    match pmap.get(&node.id) {
                        Some(p) => {
                            let r = node_radius(node.link_count, max_links, graph.nodes.len(), node_scale)
                                * radius_unit;
                            let color_class = match color_mode {
                                ColorMode::Kind => kind_text_class(&node.kind),
                                ColorMode::Community => community_text_class(node.community),
                            };
                            let is_active = active.as_ref().is_none_or(|s| s.contains(&node.id));
                            let dim_class = if has_active && !is_active { "opacity-20" } else { "opacity-100" };
                            // Zoom-aware label density: a node's score is its
                            // degree relative to the biggest hub, boosted by
                            // the current zoom. Zoomed out only the top hubs
                            // pass the threshold; zooming in raises every
                            // score until the leaves label too. Hover and
                            // search highlights always label.
                            let is_hovered = hov.as_deref() == Some(node.id.as_str());
                            let is_highlight = !props.highlighted.is_empty() && is_active;
                            let density = (node.link_count.max(1) as f32 * 4.0 * z)
                                / max_links.max(1) as f32;
                            let show_label = is_active && (is_hovered || is_highlight || density >= LABEL_THRESHOLD);
                            // Fade labels in as they cross the threshold so a
                            // zoom doesn't pop a wall of text at once.
                            let label_opacity = if is_hovered || is_highlight {
                                1.0
                            } else {
                                ((density - LABEL_THRESHOLD) / LABEL_FADE_BAND).clamp(0.35, 1.0)
                            };
                            let label_size = (r * 0.9).max(edge_unit * 6.0);
                            let label_y = p.y - r - label_size * 0.3;
                            let id_click = node.id.clone();
                            let id_enter = node.id.clone();
                            let fill_class = format!("fill-current {color_class}");
                            let on_click = props.on_node_click;
                            rsx! {
                                g {
                                    key: "{node.id}",
                                    class: "cursor-pointer transition-opacity {dim_class}",
                                    onmouseenter: move |_| hovered.set(Some(id_enter.clone())),
                                    onclick: move |_| on_click.call(id_click.clone()),
                                    circle {
                                        cx: "{p.x}", cy: "{p.y}", r: "{r}",
                                        class: "{fill_class}",
                                        stroke: "white",
                                        stroke_width: "{edge_unit}",
                                        stroke_opacity: "0.4",
                                    }
                                    if show_label {
                                        text {
                                            x: "{p.x}", y: "{label_y}",
                                            text_anchor: "middle",
                                            font_size: "{label_size}",
                                            opacity: "{label_opacity}",
                                            class: "fill-current text-foreground pointer-events-none",
                                            "{node.label}"
                                        }
                                    }
                                }
                            }
                        }
                        None => rsx! {},
                    }
                }
            }

            // Zoom controls.
            div { class: "absolute right-3 top-3 flex flex-col gap-1",
                button {
                    r#type: "button",
                    class: "rounded border border-border bg-background/80 p-1.5 backdrop-blur-sm hover:bg-accent",
                    title: "Zoom in",
                    onclick: move |_| zoom.with_mut(|z| *z = (*z * 1.25).min(8.0)),
                    ZoomIn { size: 14 }
                }
                button {
                    r#type: "button",
                    class: "rounded border border-border bg-background/80 p-1.5 backdrop-blur-sm hover:bg-accent",
                    title: "Zoom out",
                    onclick: move |_| zoom.with_mut(|z| *z = (*z / 1.25).max(0.2)),
                    ZoomOut { size: 14 }
                }
                button {
                    r#type: "button",
                    class: "rounded border border-border bg-background/80 p-1.5 backdrop-blur-sm hover:bg-accent",
                    title: "Reset view",
                    onclick: move |_| {
                        zoom.set(1.0);
                        pan.set((0.0, 0.0));
                    },
                    Maximize { size: 14 }
                }
            }
        }
    }
}
