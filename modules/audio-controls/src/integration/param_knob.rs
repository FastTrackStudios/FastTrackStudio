//! Parameter-bound knob widget.

use dioxus::prelude::*;
use nih_plug::prelude::ParamPtr;
use nih_plug_dioxus::context::use_param_context;

use crate::core::modulation::ModulationRange;
use crate::core::normal::Normal;
use crate::core::sensitivity::{DragSensitivity, ModifierKeys};
use crate::theming::style::{ControlState, KnobStyle};
use crate::widgets::knob::KnobVariant;

/// Knob bound to a nih-plug parameter.
///
/// This widget automatically reads the parameter value, displays modulation,
/// and handles parameter change notifications through the `ParamContext`.
///
/// # Example
///
/// ```ignore
/// use audio_controls::prelude::*;
///
/// #[component]
/// fn PluginUI(params: Arc<MyParams>) -> Element {
///     rsx! {
///         ParamKnob {
///             param_ptr: params.gain.as_ptr(),
///             variant: KnobVariant::Arc,
///             size: 64,
///         }
///     }
/// }
/// ```
#[component]
pub fn ParamKnob(
    param_ptr: ParamPtr,
    #[props(default)] variant: KnobVariant,
    #[props(default = 48)] size: u32,
    #[props(default = true)] show_label: bool,
    #[props(default = true)] show_value: bool,
    #[props(default)] class: String,
) -> Element {
    let ctx = use_param_context();
    let mut is_dragging = use_signal(|| false);
    let mut drag_start_y = use_signal(|| 0.0f32);
    let mut drag_start_value = use_signal(|| 0.0f32);
    let mut is_hovered = use_signal(|| false);

    let sensitivity = DragSensitivity::DEFAULT;

    // Read parameter state
    let normalized = unsafe { param_ptr.modulated_normalized_value() };
    let unmodulated = unsafe { param_ptr.unmodulated_normalized_value() };
    let display_value = unsafe { param_ptr.normalized_value_to_string(normalized, true) };
    let name = unsafe { param_ptr.name() };
    let default_normalized = unsafe { param_ptr.default_normalized_value() };

    // Calculate modulation range
    let modulation = ModulationRange::from_offset(
        Normal::new(unmodulated),
        normalized - unmodulated,
    );

    let state = if *is_dragging.read() {
        ControlState::Dragging
    } else if *is_hovered.read() {
        ControlState::Hovered
    } else {
        ControlState::Idle
    };

    let is_bipolar = matches!(variant, KnobVariant::ArcBipolar);
    let style = match &variant {
        KnobVariant::Custom(s) => s.clone(),
        _ => KnobStyle::css_vars(state, is_bipolar),
    };

    // Arc sweep configuration
    const SWEEP: f32 = 270.0;
    const START_ANGLE: f32 = 135.0;

    let radius: f32 = 40.0;
    let center: f32 = 50.0;

    let value_sweep = normalized * SWEEP;
    let value_arc = create_arc_path(center, center, radius, START_ANGLE, value_sweep);
    let track_arc = create_arc_path(center, center, radius, START_ANGLE, SWEEP);

    let mod_arc = if modulation.active {
        let mod_start = modulation.start * SWEEP;
        let mod_width = (modulation.end - modulation.start) * SWEEP;
        Some(create_arc_path(center, center, radius - 8.0, START_ANGLE + mod_start, mod_width))
    } else {
        None
    };

    let pointer_angle = START_ANGLE + value_sweep;
    let pointer_rad = pointer_angle.to_radians();
    let pointer_x1 = center + (radius - 15.0) * pointer_rad.cos();
    let pointer_y1 = center + (radius - 15.0) * pointer_rad.sin();
    let pointer_x2 = center + (radius - 5.0) * pointer_rad.cos();
    let pointer_y2 = center + (radius - 5.0) * pointer_rad.sin();

    let center_tick = if is_bipolar {
        let center_angle = START_ANGLE + (SWEEP / 2.0);
        let center_rad = center_angle.to_radians();
        let tx1 = center + (radius - 3.0) * center_rad.cos();
        let ty1 = center + (radius - 3.0) * center_rad.sin();
        let tx2 = center + (radius + 3.0) * center_rad.cos();
        let ty2 = center + (radius + 3.0) * center_rad.sin();
        Some((tx1, ty1, tx2, ty2))
    } else {
        None
    };

    rsx! {
        div {
            class: "param-knob-container flex flex-col items-center gap-1 {class}",

            // Label
            if show_label {
                div {
                    class: "text-xs text-muted-foreground text-center select-none",
                    "{name}"
                }
            }

            // Knob SVG
            svg {
                width: "{size}",
                height: "{size}",
                view_box: "0 0 100 100",
                class: "cursor-pointer",

                onmouseenter: move |_| {
                    is_hovered.set(true);
                },
                onmouseleave: {
                    let ctx = ctx.clone();
                    move |_| {
                        is_hovered.set(false);
                        if *is_dragging.read() {
                            is_dragging.set(false);
                            ctx.end_set_raw(param_ptr);
                        }
                    }
                },
                onmousedown: {
                    let ctx = ctx.clone();
                    move |evt: MouseEvent| {
                        is_dragging.set(true);
                        drag_start_y.set(evt.client_coordinates().y as f32);
                        drag_start_value.set(normalized);
                        ctx.begin_set_raw(param_ptr);
                    }
                },
                onmousemove: {
                    let ctx = ctx.clone();
                    move |evt: MouseEvent| {
                        if !*is_dragging.read() {
                            return;
                        }
                        let delta_y = evt.client_coordinates().y as f32 - drag_start_y();
                        let modifiers = ModifierKeys::new(evt.modifiers().shift(), evt.modifiers().ctrl(), evt.modifiers().alt());
                        let delta = sensitivity.calculate_delta(delta_y, modifiers);
                        let new_value = (drag_start_value() + delta).clamp(0.0, 1.0);
                        ctx.set_normalized_raw(param_ptr, new_value);
                    }
                },
                onmouseup: {
                    let ctx = ctx.clone();
                    move |_| {
                        if *is_dragging.read() {
                            is_dragging.set(false);
                            ctx.end_set_raw(param_ptr);
                        }
                    }
                },
                ondoubleclick: {
                    let ctx = ctx.clone();
                    move |_| {
                        ctx.begin_set_raw(param_ptr);
                        ctx.set_normalized_raw(param_ptr, default_normalized);
                        ctx.end_set_raw(param_ptr);
                    }
                },

                // Track arc
                path {
                    d: "{track_arc}",
                    fill: "none",
                    stroke: "{style.track_color}",
                    stroke_width: "{style.stroke_width}",
                    stroke_linecap: "round",
                }

                // Value arc
                if value_sweep > 0.1 {
                    path {
                        d: "{value_arc}",
                        fill: "none",
                        stroke: "{style.fill_color}",
                        stroke_width: "{style.stroke_width}",
                        stroke_linecap: "round",
                    }
                }

                // Modulation arc
                if let Some(ref m_arc) = mod_arc {
                    path {
                        d: "{m_arc}",
                        fill: "none",
                        stroke: "{style.modulation_color}",
                        stroke_width: "3",
                        stroke_linecap: "round",
                        opacity: "0.7",
                    }
                }

                // Center tick
                if let Some((tx1, ty1, tx2, ty2)) = center_tick {
                    line {
                        x1: "{tx1}",
                        y1: "{ty1}",
                        x2: "{tx2}",
                        y2: "{ty2}",
                        stroke: "{style.center_color.as_deref().unwrap_or(&style.track_color)}",
                        stroke_width: "2",
                    }
                }

                // Pointer
                line {
                    x1: "{pointer_x1}",
                    y1: "{pointer_y1}",
                    x2: "{pointer_x2}",
                    y2: "{pointer_y2}",
                    stroke: "{style.pointer_color}",
                    stroke_width: "{style.pointer_width}",
                    stroke_linecap: "round",
                }
            }

            // Value display
            if show_value {
                div {
                    class: "text-xs font-mono text-center text-muted-foreground select-none",
                    "{display_value}"
                }
            }
        }
    }
}

fn create_arc_path(cx: f32, cy: f32, r: f32, start_angle: f32, sweep: f32) -> String {
    if sweep.abs() < 0.1 {
        return String::new();
    }

    let start_rad = start_angle.to_radians();
    let end_rad = (start_angle + sweep).to_radians();

    let x1 = cx + r * start_rad.cos();
    let y1 = cy + r * start_rad.sin();
    let x2 = cx + r * end_rad.cos();
    let y2 = cy + r * end_rad.sin();

    let large_arc = if sweep.abs() > 180.0 { 1 } else { 0 };
    let sweep_flag = if sweep > 0.0 { 1 } else { 0 };

    format!("M {x1} {y1} A {r} {r} 0 {large_arc} {sweep_flag} {x2} {y2}")
}
