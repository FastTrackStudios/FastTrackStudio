//! Macro module view - quick-access knob grid.
//!
//! In macro mode, only the configured `ModuleMacro` knobs are displayed
//! in a centered flex grid. This provides rapid parameter access without
//! the clutter of full block detail views.

use crate::prelude::*;
use signal_control::id::BlockId;
use signal_control::module::ModuleMacro;
use uuid::Uuid;

/// Props for the macro module view.
#[derive(Props, Clone, PartialEq)]
pub struct ModuleMacroViewProps {
    /// Macros to display as knobs.
    pub macros: Vec<ModuleMacro>,
    /// Callback when a macro value changes: (block_id, param_index, normalized_value).
    pub on_macro_change: Callback<(BlockId, u32, f64)>,
}

/// Macro view showing quick-access knobs in a grid.
///
/// This view mode takes ~192px width and is ideal for making quick
/// adjustments to the most important parameters.
#[component]
pub fn ModuleMacroView(props: ModuleMacroViewProps) -> Element {
    if props.macros.is_empty() {
        return rsx! {
            div { class: "flex flex-col items-center justify-center py-4 text-muted-foreground",
                p { class: "text-xs", "No macros configured" }
                p { class: "text-xs opacity-60", "Add macros in detail view" }
            }
        };
    }

    rsx! {
        div { class: "flex flex-wrap justify-center gap-3 p-3",
            for macro_knob in &props.macros {
                MacroKnob {
                    key: "{macro_knob.id}",
                    macro_def: macro_knob.clone(),
                    on_change: props.on_macro_change.clone(),
                }
            }
        }
    }
}

/// Props for a single macro knob.
#[derive(Props, Clone, PartialEq)]
struct MacroKnobProps {
    macro_def: ModuleMacro,
    on_change: Callback<(BlockId, u32, f64)>,
}

/// A single macro knob with label and value display.
#[component]
fn MacroKnob(props: MacroKnobProps) -> Element {
    let block_id = props.macro_def.block_id;
    let param_index = props.macro_def.param_index;
    let initial_value = props.macro_def.normalized.get() as f32;

    // Create signal for the knob value
    let mut value = use_signal(move || initial_value);

    let on_change = props.on_change.clone();

    // Truncate label for display
    let label = truncate_label(&props.macro_def.name, 8);
    let display_value = props.macro_def.display_value.clone();

    rsx! {
        div { class: "flex flex-col items-center gap-1",
            // Knob container
            div {
                class: "relative w-12 h-12 cursor-pointer",
                // SVG-based knob visualization
                KnobVisual {
                    value: value(),
                    on_change: move |new_val: f32| {
                        value.set(new_val);
                        on_change.call((block_id, param_index, new_val as f64));
                    },
                }
            }

            // Label
            span { class: "text-xs font-medium text-center truncate w-16", "{label}" }

            // Value display
            span { class: "text-xs text-muted-foreground text-center", "{display_value}" }
        }
    }
}

/// Props for the visual knob component.
#[derive(Props, Clone, PartialEq)]
struct KnobVisualProps {
    value: f32,
    on_change: EventHandler<f32>,
}

/// SVG-based knob visualization with drag interaction.
#[component]
fn KnobVisual(props: KnobVisualProps) -> Element {
    let mut dragging = use_signal(|| false);
    let mut start_y = use_signal(|| 0.0f32);
    let mut start_value = use_signal(|| 0.0f32);

    // Arc parameters
    let size = 48.0;
    let center = size / 2.0;
    let radius = 18.0;
    let stroke_width = 4.0;

    // Convert value (0-1) to angle (-135 to 135 degrees)
    let value_angle = -135.0 + (props.value * 270.0);

    // Arc path calculation
    let start_angle: f32 = -135.0_f32.to_radians();
    let end_angle: f32 = value_angle.to_radians();

    let arc_start_x = center + radius * start_angle.cos();
    let arc_start_y = center + radius * start_angle.sin();
    let arc_end_x = center + radius * end_angle.cos();
    let arc_end_y = center + radius * end_angle.sin();

    // Determine if we need large arc flag
    let large_arc = if (end_angle - start_angle).abs() > std::f32::consts::PI {
        1
    } else {
        0
    };

    // Pointer line
    let pointer_length = radius - 4.0;
    let pointer_end_x = center + pointer_length * end_angle.cos();
    let pointer_end_y = center + pointer_length * end_angle.sin();

    let on_change = props.on_change.clone();

    rsx! {
        svg {
            class: "w-full h-full",
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
                stroke_dasharray: "212 80",
                transform: "rotate(135 {center} {center})",
            }

            // Value arc
            if props.value > 0.01 {
                path {
                    d: "M {arc_start_x} {arc_start_y} A {radius} {radius} 0 {large_arc} 1 {arc_end_x} {arc_end_y}",
                    fill: "none",
                    stroke: "#3B82F6",
                    stroke_width: "{stroke_width}",
                    stroke_linecap: "round",
                }
            }

            // Center circle
            circle {
                cx: "{center}",
                cy: "{center}",
                r: "{radius - 6.0}",
                fill: "#1F2937",
            }

            // Pointer line
            line {
                x1: "{center}",
                y1: "{center}",
                x2: "{pointer_end_x}",
                y2: "{pointer_end_y}",
                stroke: "#E5E7EB",
                stroke_width: "2",
                stroke_linecap: "round",
            }

            // Invisible hit area for mouse events
            circle {
                cx: "{center}",
                cy: "{center}",
                r: "{radius + 4.0}",
                fill: "transparent",
                cursor: "pointer",
                onmousedown: move |e| {
                    dragging.set(true);
                    start_y.set(e.client_coordinates().y as f32);
                    start_value.set(props.value);
                },
            }
        }

        // Global mouse handlers when dragging
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

/// Truncate a label for display.
fn truncate_label(name: &str, max_len: usize) -> String {
    if name.len() <= max_len {
        name.to_string()
    } else {
        format!("{}…", &name[..max_len - 1])
    }
}
