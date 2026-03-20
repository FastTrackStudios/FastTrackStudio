//! Rotary knob widget — SVG arc-style with drag interaction and modulation overlay.
//!
//! Ported from FastTrackStudio signal-ui, adapted for Blitz inline styles
//! and nih_plug ParamPtr binding.
//!
//! Drag capture is handled by the `DragProvider` wrapper at the editor root.
//! The knob only fires `onmousedown` to start a drag.

use crate::drag::{begin_drag, DragState};
use crate::theme::*;
use nih_plug::prelude::ParamPtr;
use nih_plug_dioxus::prelude::*;
use std::f64::consts::PI;

/// Knob display size.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum KnobSize {
    Small,
    Medium,
    Large,
}

impl Default for KnobSize {
    fn default() -> Self {
        Self::Medium
    }
}

impl KnobSize {
    pub fn diameter(self) -> u32 {
        match self {
            Self::Small => 32,
            Self::Medium => 48,
            Self::Large => 64,
        }
    }
}

// Arc geometry: 270° sweep from 135° (7 o'clock) to 405° (5 o'clock)
const START_ANGLE: f64 = 135.0;
const SWEEP: f64 = 270.0;

fn angle_for_value(v: f64) -> f64 {
    START_ANGLE + v.clamp(0.0, 1.0) * SWEEP
}

fn arc_point(cx: f64, cy: f64, r: f64, angle_deg: f64) -> (f64, f64) {
    let rad = angle_deg * PI / 180.0;
    (cx + r * rad.cos(), cy + r * rad.sin())
}

fn svg_arc(cx: f64, cy: f64, r: f64, start_deg: f64, end_deg: f64) -> String {
    let (x1, y1) = arc_point(cx, cy, r, start_deg);
    let (x2, y2) = arc_point(cx, cy, r, end_deg);
    let large = if (end_deg - start_deg).abs() > 180.0 {
        1
    } else {
        0
    };
    format!("M {x1:.1} {y1:.1} A {r:.1} {r:.1} 0 {large} 1 {x2:.1} {y2:.1}")
}

/// Drag sensitivity: pixels of vertical drag per full 0→1 sweep.
const SENSITIVITY: f64 = 150.0;

/// A rotary knob bound to a nih_plug parameter.
///
/// Requires a `DragProvider` ancestor to handle drag capture.
#[component]
pub fn Knob(
    /// The nih_plug parameter to control.
    param_ptr: ParamPtr,
    /// Display size.
    #[props(default)]
    size: KnobSize,
    /// Label shown below the knob.
    #[props(default)]
    label: Option<String>,
    /// Accent color override (e.g. "#F97316"). Falls back to theme ACCENT.
    #[props(default)]
    color: Option<String>,
    /// Optional modulation range minimum (0.0–1.0).
    #[props(default)]
    mod_min: Option<f64>,
    /// Optional modulation range maximum (0.0–1.0).
    #[props(default)]
    mod_max: Option<f64>,
    /// Whether the knob is disabled.
    #[props(default)]
    disabled: bool,
) -> Element {
    let ctx = use_param_context();
    let mut drag: Signal<DragState> = use_context();
    let mut revision = use_signal(|| 0u32);
    let _ = *revision.read();

    // Also re-render when drag is active (so value display updates)
    let _ = *drag.read();

    let normalized = unsafe { param_ptr.modulated_normalized_value() } as f64;
    let display_value = unsafe { param_ptr.normalized_value_to_string(normalized as f32, true) };
    let param_name = label.unwrap_or_else(|| unsafe { param_ptr.name() }.to_string());

    let d = size.diameter();
    let df = d as f64;
    let cx = df / 2.0;
    let cy = df / 2.0;
    let r = df / 2.0 - 4.0;
    let val = normalized.clamp(0.0, 1.0);

    // Track arc (full background)
    let track_path = svg_arc(cx, cy, r, START_ANGLE, START_ANGLE + SWEEP);

    // Value arc (filled portion)
    let end_angle = angle_for_value(val);
    let value_path = if val > 0.001 {
        svg_arc(cx, cy, r, START_ANGLE, end_angle)
    } else {
        String::new()
    };

    // Modulation overlay arc
    let mod_path = match (mod_min, mod_max) {
        (Some(lo), Some(hi)) => {
            let lo_angle = angle_for_value(lo.clamp(0.0, 1.0));
            let hi_angle = angle_for_value(hi.clamp(0.0, 1.0));
            svg_arc(cx, cy, r - 2.0, lo_angle, hi_angle)
        }
        _ => String::new(),
    };

    // Thumb indicator line
    let (tx, ty) = arc_point(cx, cy, r - 6.0, end_angle);
    let (tx2, ty2) = arc_point(cx, cy, r + 1.0, end_angle);

    let accent = color.as_deref().unwrap_or(ACCENT);
    let opacity = if disabled { "0.5" } else { "1.0" };
    let cursor = if disabled { "not-allowed" } else { "pointer" };

    rsx! {
        div {
            style: format!(
                "display:inline-flex; flex-direction:column; align-items:center; gap:4px; \
                 opacity:{opacity}; cursor:{cursor}; position:relative;"
            ),

            // SVG arc knob
            svg {
                width: "{d}",
                height: "{d}",
                view_box: "0 0 {df} {df}",

                // Track arc (background)
                path {
                    d: "{track_path}",
                    fill: "none",
                    stroke: "{BORDER}",
                    stroke_width: "3.5",
                    stroke_linecap: "round",
                }

                // Value arc (filled)
                if !value_path.is_empty() {
                    path {
                        d: "{value_path}",
                        fill: "none",
                        stroke: "{accent}",
                        stroke_width: "4",
                        stroke_linecap: "round",
                    }
                }

                // Modulation overlay
                if !mod_path.is_empty() {
                    path {
                        d: "{mod_path}",
                        fill: "none",
                        stroke: "{SIGNAL_MOD}",
                        stroke_width: "2",
                        stroke_linecap: "round",
                        opacity: "0.6",
                    }
                }

                // Thumb indicator line
                line {
                    x1: "{tx:.1}",
                    y1: "{ty:.1}",
                    x2: "{tx2:.1}",
                    y2: "{ty2:.1}",
                    stroke: "{TEXT}",
                    stroke_width: "2",
                    stroke_linecap: "round",
                }
            }

            // Invisible drag surface — only handles mousedown and doubleclick.
            // mousemove / mouseup are handled by the DragProvider at the root.
            if !disabled {
                div {
                    style: "position:absolute; inset:0; cursor:ns-resize; user-select:none;",
                    onmousedown: {
                        let ctx = ctx.clone();
                        move |evt: MouseEvent| {
                            begin_drag(
                                &mut drag,
                                &ctx,
                                param_ptr,
                                evt.client_coordinates().y,
                                SENSITIVITY,
                            );
                            revision += 1;
                        }
                    },
                    ondoubleclick: {
                        let ctx = ctx.clone();
                        move |_| {
                            let default =
                                unsafe { param_ptr.default_normalized_value() };
                            ctx.begin_set_raw(param_ptr);
                            ctx.set_normalized_raw(param_ptr, default);
                            ctx.end_set_raw(param_ptr);
                            revision += 1;
                        }
                    },
                }
            }

            // Display value
            span {
                style: format!(
                    "font-size:10px; color:{TEXT_DIM}; font-variant-numeric:tabular-nums; \
                     min-width:48px; text-align:center;"
                ),
                "{display_value}"
            }

            // Label
            span {
                style: format!(
                    "font-size:10px; color:{TEXT_DIM}; font-weight:500; \
                     min-width:48px; text-align:center;"
                ),
                "{param_name}"
            }
        }
    }
}

/// A rotary knob displaying a raw normalized value (not bound to a parameter).
///
/// Useful for visualizations or custom parameter handling.
#[component]
pub fn RawKnob(
    /// Current normalized value (0.0–1.0).
    #[props(default = 0.5)]
    value: f64,
    /// Display size.
    #[props(default)]
    size: KnobSize,
    /// Label shown below the knob.
    #[props(default)]
    label: Option<String>,
    /// Formatted display value (e.g. "50%", "-12 dB").
    #[props(default)]
    display_value: Option<String>,
    /// Accent color override.
    #[props(default)]
    color: Option<String>,
    /// Optional modulation range minimum (0.0–1.0).
    #[props(default)]
    mod_min: Option<f64>,
    /// Optional modulation range maximum (0.0–1.0).
    #[props(default)]
    mod_max: Option<f64>,
    /// Callback when value changes.
    #[props(default)]
    on_change: Option<Callback<f64>>,
    /// Whether the knob is disabled.
    #[props(default)]
    disabled: bool,
) -> Element {
    let d = size.diameter();
    let df = d as f64;
    let cx = df / 2.0;
    let cy = df / 2.0;
    let r = df / 2.0 - 4.0;
    let val = value.clamp(0.0, 1.0);

    let track_path = svg_arc(cx, cy, r, START_ANGLE, START_ANGLE + SWEEP);
    let end_angle = angle_for_value(val);
    let value_path = if val > 0.001 {
        svg_arc(cx, cy, r, START_ANGLE, end_angle)
    } else {
        String::new()
    };

    let mod_path = match (mod_min, mod_max) {
        (Some(lo), Some(hi)) => {
            let lo_angle = angle_for_value(lo.clamp(0.0, 1.0));
            let hi_angle = angle_for_value(hi.clamp(0.0, 1.0));
            svg_arc(cx, cy, r - 2.0, lo_angle, hi_angle)
        }
        _ => String::new(),
    };

    let (tx, ty) = arc_point(cx, cy, r - 6.0, end_angle);
    let (tx2, ty2) = arc_point(cx, cy, r + 1.0, end_angle);

    let accent = color.as_deref().unwrap_or(ACCENT);
    let opacity = if disabled { "0.5" } else { "1.0" };
    let cursor = if disabled { "not-allowed" } else { "pointer" };

    rsx! {
        div {
            style: format!(
                "display:inline-flex; flex-direction:column; align-items:center; gap:4px; \
                 opacity:{opacity}; cursor:{cursor};"
            ),

            svg {
                width: "{d}",
                height: "{d}",
                view_box: "0 0 {df} {df}",

                path {
                    d: "{track_path}",
                    fill: "none",
                    stroke: "{BORDER}",
                    stroke_width: "3.5",
                    stroke_linecap: "round",
                }

                if !value_path.is_empty() {
                    path {
                        d: "{value_path}",
                        fill: "none",
                        stroke: "{accent}",
                        stroke_width: "4",
                        stroke_linecap: "round",
                    }
                }

                if !mod_path.is_empty() {
                    path {
                        d: "{mod_path}",
                        fill: "none",
                        stroke: "{SIGNAL_MOD}",
                        stroke_width: "2",
                        stroke_linecap: "round",
                        opacity: "0.6",
                    }
                }

                line {
                    x1: "{tx:.1}",
                    y1: "{ty:.1}",
                    x2: "{tx2:.1}",
                    y2: "{ty2:.1}",
                    stroke: "{TEXT}",
                    stroke_width: "2",
                    stroke_linecap: "round",
                }
            }

            // Hidden range input for interaction
            if !disabled {
                input {
                    r#type: "range",
                    style: "position:absolute; inset:0; opacity:0; cursor:pointer;",
                    min: "0",
                    max: "1",
                    step: "0.005",
                    value: "{val}",
                    oninput: move |evt: FormEvent| {
                        if let Ok(v) = evt.value().parse::<f64>() {
                            if let Some(cb) = &on_change {
                                cb.call(v.clamp(0.0, 1.0));
                            }
                        }
                    },
                }
            }

            if let Some(display) = &display_value {
                span {
                    style: format!(
                        "font-size:10px; color:{TEXT_DIM}; font-variant-numeric:tabular-nums;"
                    ),
                    "{display}"
                }
            }

            if let Some(label) = &label {
                span {
                    style: format!("font-size:10px; color:{TEXT_DIM}; font-weight:500;"),
                    "{label}"
                }
            }
        }
    }
}
