//! Rotary knob — SVG arc with vertical-drag interaction and modulation overlay.
//!
//! Provider-agnostic port of `audio-gui::controls::knob::Knob`. Binds to a
//! [`ParamHandle`] instead of a `nih_plug::ParamPtr`. Requires a
//! [`crate::drag::DragProvider`] ancestor for drag capture.

use crate::drag::{DragState, begin_drag};
use crate::param::ParamHandle;
use crate::theme::*;
use dioxus::prelude::*;
use std::f64::consts::PI;

/// Display size.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum KnobSize {
    Small,
    #[default]
    Medium,
    Large,
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

const START_ANGLE: f64 = 135.0;
const SWEEP: f64 = 270.0;
/// Pixels of vertical drag per full 0→1 sweep.
const SENSITIVITY: f64 = 150.0;

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

/// A rotary knob bound to a [`ParamHandle`].
#[component]
pub fn Knob(
    /// The parameter this knob drives.
    handle: ParamHandle,
    #[props(default)] size: KnobSize,
    /// Override the parameter's name when rendering the label.
    #[props(default)]
    label: Option<String>,
    /// Accent color override (e.g. `"#F97316"`).
    #[props(default)]
    color: Option<String>,
    /// Modulation range minimum (0.0–1.0). Drawn as an overlay arc.
    #[props(default)]
    mod_min: Option<f64>,
    #[props(default)] mod_max: Option<f64>,
    #[props(default)] disabled: bool,
) -> Element {
    let mut drag: Signal<DragState> = use_context();
    let mut editing = use_signal(|| false);

    // Re-render while a drag is active so the value display tracks the cursor.
    let _ = drag.read().move_count;

    let normalized = handle.normalized() as f64;
    let display_value = handle.display_value();
    let param_name = label.unwrap_or_else(|| handle.name());
    let is_editing = *editing.read();

    let d = size.diameter();
    let df = d as f64;
    let cx = df / 2.0;
    let cy = df / 2.0;
    let r = df / 2.0 - 4.0;
    let val = normalized.clamp(0.0, 1.0);

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
                 opacity:{opacity}; cursor:{cursor}; position:relative;"
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

            if !disabled {
                div {
                    style: "position:absolute; inset:0; cursor:ns-resize; user-select:none;",
                    onmousedown: {
                        let handle = handle.clone();
                        move |evt: MouseEvent| {
                            begin_drag(
                                &mut drag,
                                handle.clone(),
                                evt.client_coordinates().y,
                                SENSITIVITY,
                            );
                        }
                    },
                    ondoubleclick: move |_| { editing.set(true); },
                }
            }

            if is_editing {
                input {
                    r#type: "text",
                    style: format!(
                        "font-size:10px; color:{TEXT}; background:{SURFACE}; \
                         border:1px solid {ACCENT}; border-radius:3px; \
                         min-width:48px; width:56px; text-align:center; \
                         padding:1px 2px; outline:none;"
                    ),
                    value: "{display_value}",
                    onkeydown: move |evt: KeyboardEvent| {
                        if evt.key() == Key::Enter || evt.key() == Key::Escape {
                            editing.set(false);
                        }
                    },
                    onchange: {
                        let handle = handle.clone();
                        move |evt: FormEvent| {
                            let text = evt.value();
                            if let Some(n) = handle.string_to_normalized(&text) {
                                handle.begin_edit();
                                handle.set_normalized(n);
                                handle.end_edit();
                            }
                            editing.set(false);
                        }
                    },
                    onfocusout: move |_| { editing.set(false); },
                }
            } else {
                span {
                    style: format!(
                        "font-size:10px; color:{TEXT_DIM}; font-variant-numeric:tabular-nums; \
                         min-width:48px; text-align:center; cursor:text;"
                    ),
                    ondoubleclick: move |_| {
                        if !disabled { editing.set(true); }
                    },
                    "{display_value}"
                }
            }

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

/// Knob displaying a raw normalized value, not bound to a parameter system.
/// Useful for visualizations or custom edit handling.
#[component]
pub fn RawKnob(
    #[props(default = 0.5)] value: f64,
    #[props(default)] size: KnobSize,
    #[props(default)] label: Option<String>,
    #[props(default)] display_value: Option<String>,
    #[props(default)] color: Option<String>,
    #[props(default)] mod_min: Option<f64>,
    #[props(default)] mod_max: Option<f64>,
    #[props(default)] on_change: Option<Callback<f64>>,
    #[props(default)] disabled: bool,
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
                width: "{d}", height: "{d}", view_box: "0 0 {df} {df}",
                path { d: "{track_path}", fill: "none", stroke: "{BORDER}", stroke_width: "3.5", stroke_linecap: "round" }
                if !value_path.is_empty() {
                    path { d: "{value_path}", fill: "none", stroke: "{accent}", stroke_width: "4", stroke_linecap: "round" }
                }
                if !mod_path.is_empty() {
                    path { d: "{mod_path}", fill: "none", stroke: "{SIGNAL_MOD}", stroke_width: "2", stroke_linecap: "round", opacity: "0.6" }
                }
                line { x1: "{tx:.1}", y1: "{ty:.1}", x2: "{tx2:.1}", y2: "{ty2:.1}", stroke: "{TEXT}", stroke_width: "2", stroke_linecap: "round" }
            }

            if !disabled {
                input {
                    r#type: "range",
                    style: "position:absolute; inset:0; opacity:0; cursor:pointer;",
                    min: "0", max: "1", step: "0.005",
                    value: "{val}",
                    oninput: move |evt: FormEvent| {
                        if let Ok(v) = evt.value().parse::<f64>() {
                            if let Some(cb) = &on_change { cb.call(v.clamp(0.0, 1.0)); }
                        }
                    },
                }
            }

            if let Some(display) = &display_value {
                span {
                    style: format!("font-size:10px; color:{TEXT_DIM}; font-variant-numeric:tabular-nums;"),
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
