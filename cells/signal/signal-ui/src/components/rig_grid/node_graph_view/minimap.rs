//! Minimap navigation overlay for the node graph canvas.
//!
//! Renders a small overview of all modules in the bottom-left corner and
//! shows the current viewport rectangle. Clicking jumps to that position.

use super::super::block_colors::block_type_color;
use super::super::node_graph::NodeGraph;
use crate::callback_types::PanOffset;
use crate::prelude::*;

// ── Minimap Component ────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub(crate) struct MinimapProps {
    pub graph: NodeGraph,
    pub canvas_w: f64,
    pub canvas_h: f64,
    pub viewport_w: f64,
    pub viewport_h: f64,
    pub pan_x: f64,
    pub pan_y: f64,
    pub zoom: f64,
    pub on_pan: Callback<PanOffset>,
}

#[component]
pub(crate) fn Minimap(props: MinimapProps) -> Element {
    let minimap_w = 180.0;
    let minimap_h = 120.0;

    if props.canvas_w <= 0.0 || props.canvas_h <= 0.0 {
        return rsx! {};
    }

    let scale_x = minimap_w / props.canvas_w;
    let scale_y = minimap_h / props.canvas_h;
    let scale = scale_x.min(scale_y) * 0.9;

    let offset_x = (minimap_w - props.canvas_w * scale) / 2.0;
    let offset_y = (minimap_h - props.canvas_h * scale) / 2.0;

    let vp_x = offset_x + (-props.pan_x / props.zoom) * scale;
    let vp_y = offset_y + (-props.pan_y / props.zoom) * scale;
    let vp_w = (props.viewport_w / props.zoom) * scale;
    let vp_h = (props.viewport_h / props.zoom) * scale;

    let on_pan = props.on_pan.clone();
    let current_zoom = props.zoom;

    rsx! {
        div {
            class: "absolute bottom-4 left-4 rounded-lg overflow-hidden select-none",
            style: "width: {minimap_w}px; height: {minimap_h}px; \
                    background-color: rgba(0,0,0,0.75); \
                    border: 1px solid rgba(255,255,255,0.1); \
                    backdrop-filter: blur(4px);",
            onmousedown: move |evt| {
                evt.stop_propagation();
                let rect_x = evt.element_coordinates().x;
                let rect_y = evt.element_coordinates().y;
                let canvas_x = (rect_x - offset_x) / scale;
                let canvas_y = (rect_y - offset_y) / scale;
                let new_pan_x = -(canvas_x - props.viewport_w * 0.5 / current_zoom) * current_zoom;
                let new_pan_y = -(canvas_y - props.viewport_h * 0.5 / current_zoom) * current_zoom;
                on_pan.call(PanOffset { x: new_pan_x, y: new_pan_y });
            },

            for module in &props.graph.modules {
                {
                    let mx = offset_x + module.position.x * scale;
                    let my = offset_y + module.position.y * scale;
                    let mw = module.size.width * scale;
                    let mh = module.size.height * scale;
                    let color = block_type_color(module.block_type);
                    rsx! {
                        div {
                            key: "mm-{module.id}",
                            class: "absolute rounded-sm",
                            style: "left: {mx}px; top: {my}px; \
                                    width: {mw}px; height: {mh}px; \
                                    background-color: {color.bg}60; \
                                    border: 1px solid {color.bg}80;",
                        }
                    }
                }
            }

            div {
                class: "absolute rounded-sm",
                style: "left: {vp_x}px; top: {vp_y}px; \
                        width: {vp_w}px; height: {vp_h}px; \
                        border: 1.5px solid rgba(255,255,255,0.5); \
                        background-color: rgba(255,255,255,0.05);",
            }
        }
    }
}
