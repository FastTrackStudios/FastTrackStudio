//! Grid block cell component — occupied cell with block info, color dot, and connection ports.

use dioxus::prelude::*;
use uuid::Uuid;

use super::layout::PORT_SIZE;

// ─────────────────────────────────────────────────────────────────────────────
// GridBlockCell component
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub(super) struct GridBlockCellProps {
    pub slot_id: Uuid,
    pub block_type_name: String,
    pub name: String,
    /// Pre-computed inline style from `BlockVisualState::cell_style()`.
    pub cell_style: String,
    /// Pre-computed CSS class from `BlockVisualState::cell_class()`.
    pub cell_class: String,
    /// Color dot background color (block type color).
    pub dot_color: String,
    /// Port background color.
    pub port_color: String,
    /// Port opacity from `BlockVisualState::port_opacity()`.
    pub port_opacity: String,
    pub port_half: f64,
    pub left_port_hovered: bool,
    pub right_port_hovered: bool,
    /// Extra inline style for the outer wrapper (e.g., z-index during group drag).
    #[props(default)]
    pub outer_style: String,
    /// Called on mousedown on the block body (starts block drag).
    pub on_block_mousedown: EventHandler<MouseEvent>,
    /// Called on mousedown on the left (input) port.
    pub on_left_port_mousedown: EventHandler<MouseEvent>,
    /// Called on mousedown on the right (output) port.
    pub on_right_port_mousedown: EventHandler<MouseEvent>,
    /// Called when mouse enters the left port.
    pub on_left_port_enter: EventHandler<()>,
    /// Called when mouse leaves the left port.
    pub on_left_port_leave: EventHandler<()>,
    /// Called when mouse enters the right port.
    pub on_right_port_enter: EventHandler<()>,
    /// Called when mouse leaves the right port.
    pub on_right_port_leave: EventHandler<()>,
}

/// A single occupied block cell on the grid.
///
/// Renders the block's color dot, type label, preset name, and left/right
/// connection ports. All interaction is delegated to the parent via callbacks.
/// All style computation happens in the parent — this component is a pure
/// structural renderer (the "dumb view" in data-down / events-up).
#[component]
pub(super) fn GridBlockCell(props: GridBlockCellProps) -> Element {
    let dot_style = format!("background-color: {};", props.dot_color);
    let port_half = props.port_half;

    let left_port_style = format!(
        "left: {}px; top: 50%; transform: translateY(-50%); \
         width: {}px; height: {}px; background-color: {}; opacity: {};",
        -port_half as i32, PORT_SIZE, PORT_SIZE, props.port_color, props.port_opacity,
    );
    let right_port_style = format!(
        "right: {}px; top: 50%; transform: translateY(-50%); \
         width: {}px; height: {}px; background-color: {}; opacity: {};",
        -port_half as i32, PORT_SIZE, PORT_SIZE, props.port_color, props.port_opacity,
    );

    rsx! {
        div {
            key: "{props.slot_id}",
            class: "relative aspect-square",
            style: "{props.outer_style}",
            // Occupied block cell — always square
            div {
                class: "{props.cell_class}",
                style: "{props.cell_style}",
                onmousedown: move |evt| {
                    evt.stop_propagation();
                    props.on_block_mousedown.call(evt);
                },
                // Color dot + type label
                div { class: "flex items-center gap-1.5",
                    div {
                        class: "w-2.5 h-2.5 rounded-full flex-shrink-0",
                        style: "{dot_style}",
                    }
                    span {
                        class: "text-[9px] font-mono uppercase opacity-60 leading-none",
                        "{props.block_type_name}"
                    }
                }
                // Block name
                span {
                    class: "text-[11px] font-medium truncate max-w-full text-center px-1 leading-tight",
                    "{props.name}"
                }
            }
            // Left input port
            div {
                class: if props.left_port_hovered {
                    "absolute rounded-full border-2 border-cyan-400 z-10 cursor-crosshair shadow-[0_0_8px_rgba(34,211,238,0.6)]"
                } else {
                    "absolute rounded-full border border-white/40 z-10 cursor-crosshair hover:border-white/70 hover:shadow-[0_0_6px_rgba(255,255,255,0.3)]"
                },
                style: "{left_port_style}",
                onmousedown: move |evt| {
                    evt.stop_propagation();
                    props.on_left_port_mousedown.call(evt);
                },
                onmouseenter: move |_| props.on_left_port_enter.call(()),
                onmouseleave: move |_| props.on_left_port_leave.call(()),
            }
            // Right output port
            div {
                class: if props.right_port_hovered {
                    "absolute rounded-full border-2 border-cyan-400 z-10 cursor-crosshair shadow-[0_0_8px_rgba(34,211,238,0.6)]"
                } else {
                    "absolute rounded-full border border-white/40 z-10 cursor-crosshair hover:border-white/70 hover:shadow-[0_0_6px_rgba(255,255,255,0.3)]"
                },
                style: "{right_port_style}",
                onmousedown: move |evt| {
                    evt.stop_propagation();
                    props.on_right_port_mousedown.call(evt);
                },
                onmouseenter: move |_| props.on_right_port_enter.call(()),
                onmouseleave: move |_| props.on_right_port_leave.call(()),
            }
        }
    }
}
