//! Detail module view - full parameter display.
//!
//! In detail mode, each block is displayed with its full set of parameters
//! using audio-controls widgets. This provides complete editing capability
//! at the cost of more screen space.

use dioxus::prelude::*;
use crate::block::BlockType;
use crate::module::ModuleBlock;
use crate::parameter::ParameterValue;
use uuid::Uuid;

use super::block_colors::{block_type_color, block_type_style};
use super::module_compact_view::infer_block_type;

/// Props for the detail module view.
#[derive(Props, Clone, PartialEq)]
pub struct ModuleDetailViewProps {
    /// Blocks to display with full parameters.
    pub blocks: Vec<ModuleBlock>,
    /// Callback when a block's bypass is toggled.
    pub on_toggle_bypass: Callback<Uuid>,
    /// Callback when a parameter changes: (block_id, param_index, value).
    pub on_param_change: Callback<(Uuid, u32, f64)>,
}

/// Detail view showing full block parameters.
///
/// This view mode takes ~320px width and is ideal for detailed editing
/// and sound design work.
#[component]
pub fn ModuleDetailView(props: ModuleDetailViewProps) -> Element {
    if props.blocks.is_empty() {
        return rsx! {
            div { class: "text-sm text-muted-foreground text-center py-4",
                "No blocks in this module"
            }
        };
    }

    // Sort blocks by order
    let mut sorted_blocks = props.blocks.clone();
    sorted_blocks.sort_by_key(|b| b.order.get());

    rsx! {
        div { class: "flex flex-col gap-2 p-2",
            for block in sorted_blocks {
                BlockDetailCard {
                    key: "{block.id}",
                    block: block.clone(),
                    on_toggle_bypass: props.on_toggle_bypass.clone(),
                    on_param_change: props.on_param_change.clone(),
                }
            }
        }
    }
}

/// Props for a single block detail card.
#[derive(Props, Clone, PartialEq)]
struct BlockDetailCardProps {
    block: ModuleBlock,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f64)>,
}

/// A card showing a block with its full parameter set.
#[component]
fn BlockDetailCard(props: BlockDetailCardProps) -> Element {
    let block = &props.block;
    let block_id = block.block.id.as_uuid();
    let is_bypassed = block.block.bypassed;

    // Get block type for coloring
    let block_type = infer_block_type(&block.block.name);
    let color = block_type_color(block_type);

    let on_toggle = props.on_toggle_bypass.clone();

    // Container styling based on bypass state
    let container_class = if is_bypassed {
        "border border-border/50 rounded-lg overflow-hidden opacity-60"
    } else {
        "border border-border rounded-lg overflow-hidden"
    };

    rsx! {
        div { class: "{container_class}",
            // Header bar with color accent
            div {
                class: "flex items-center justify-between px-3 py-2 border-b",
                style: "background: linear-gradient(180deg, {color.bg}20 0%, {color.bg}10 100%); \
                        border-bottom-color: {color.border}40;",

                // Block name and type
                div { class: "flex items-center gap-2",
                    // Color indicator dot
                    div {
                        class: "w-3 h-3 rounded-full",
                        style: "background-color: {color.bg};",
                    }
                    span { class: "font-medium text-sm", "{block.block.name}" }
                }

                // Bypass toggle button
                button {
                    class: if is_bypassed {
                        "px-2 py-1 text-xs rounded bg-red-500/20 text-red-400 hover:bg-red-500/30"
                    } else {
                        "px-2 py-1 text-xs rounded bg-green-500/20 text-green-400 hover:bg-green-500/30"
                    },
                    onclick: move |_| on_toggle.call(block_id),
                    if is_bypassed { "Bypassed" } else { "Active" }
                }
            }

            // Parameters grid
            div { class: "p-3 bg-card",
                if block.block.parameters.is_empty() {
                    div { class: "text-xs text-muted-foreground text-center py-2",
                        "No parameters"
                    }
                } else {
                    div { class: "grid grid-cols-3 gap-3",
                        for (idx, param) in block.block.parameters.iter().enumerate() {
                            ParameterKnob {
                                key: "{idx}",
                                block_id,
                                param_index: param.index,
                                value: param.value.get(),
                                name: format!("Param {}", param.index),
                                on_change: props.on_param_change.clone(),
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Props for a single parameter knob.
#[derive(Props, Clone, PartialEq)]
struct ParameterKnobProps {
    block_id: Uuid,
    param_index: u32,
    value: f64,
    name: String,
    on_change: Callback<(Uuid, u32, f64)>,
}

/// A single parameter knob control.
#[component]
fn ParameterKnob(props: ParameterKnobProps) -> Element {
    let block_id = props.block_id;
    let param_index = props.param_index;

    let mut value = use_signal(|| props.value as f32);

    // Sync external value changes
    use_effect(move || {
        value.set(props.value as f32);
    });

    let on_change = props.on_change.clone();

    rsx! {
        div { class: "flex flex-col items-center gap-1",
            // Mini knob
            MiniKnob {
                value: value(),
                on_change: move |new_val: f32| {
                    value.set(new_val);
                    on_change.call((block_id, param_index, new_val as f64));
                },
            }

            // Label
            span { class: "text-xs text-muted-foreground text-center truncate w-14",
                "{props.name}"
            }

            // Value
            span { class: "text-xs font-mono text-center",
                "{(props.value * 100.0) as i32}%"
            }
        }
    }
}

/// Props for mini knob.
#[derive(Props, Clone, PartialEq)]
struct MiniKnobProps {
    value: f32,
    on_change: EventHandler<f32>,
}

/// A smaller knob for parameter grids.
#[component]
fn MiniKnob(props: MiniKnobProps) -> Element {
    let mut dragging = use_signal(|| false);
    let mut start_y = use_signal(|| 0.0f32);
    let mut start_value = use_signal(|| 0.0f32);

    let size = 36.0;
    let center = size / 2.0;
    let radius = 14.0;
    let stroke_width = 3.0;

    let value_angle = -135.0 + (props.value * 270.0);
    let end_angle: f32 = value_angle.to_radians();

    let pointer_length = radius - 3.0;
    let pointer_end_x = center + pointer_length * end_angle.cos();
    let pointer_end_y = center + pointer_length * end_angle.sin();

    let on_change = props.on_change.clone();

    rsx! {
        svg {
            class: "w-9 h-9 cursor-pointer",
            view_box: "0 0 {size} {size}",

            // Background track
            circle {
                cx: "{center}",
                cy: "{center}",
                r: "{radius}",
                fill: "none",
                stroke: "#374151",
                stroke_width: "{stroke_width}",
                stroke_linecap: "round",
                stroke_dasharray: "159 60",
                transform: "rotate(135 {center} {center})",
            }

            // Center circle
            circle {
                cx: "{center}",
                cy: "{center}",
                r: "{radius - 4.0}",
                fill: "#1F2937",
            }

            // Pointer
            line {
                x1: "{center}",
                y1: "{center}",
                x2: "{pointer_end_x}",
                y2: "{pointer_end_y}",
                stroke: "#3B82F6",
                stroke_width: "2",
                stroke_linecap: "round",
            }

            // Hit area
            circle {
                cx: "{center}",
                cy: "{center}",
                r: "{radius + 2.0}",
                fill: "transparent",
                onmousedown: move |e| {
                    dragging.set(true);
                    start_y.set(e.client_coordinates().y as f32);
                    start_value.set(props.value);
                },
            }
        }

        // Drag overlay
        if dragging() {
            div {
                class: "fixed inset-0 z-50",
                style: "cursor: ns-resize;",
                onmousemove: move |e| {
                    if dragging() {
                        let delta = (start_y() - e.client_coordinates().y as f32) / 150.0;
                        let new_value = (start_value() + delta).clamp(0.0, 1.0);
                        on_change.call(new_value);
                    }
                },
                onmouseup: move |_| {
                    dragging.set(false);
                },
                onmouseleave: move |_| {
                    dragging.set(false);
                },
            }
        }
    }
}

