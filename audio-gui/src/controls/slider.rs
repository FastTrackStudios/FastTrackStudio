//! Parameter slider — inline-styled, Blitz-compatible.
//!
//! Two variants:
//! - `ParamSlider`: bound to a nih_plug `ParamPtr`, displays name + value
//! - `Slider`: raw normalized value with callback

use crate::drag::{begin_drag, DragState};
use crate::theme::*;
use nih_plug::prelude::ParamPtr;
use nih_plug_dioxus::prelude::*;

/// Slider orientation.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum SliderOrientation {
    #[default]
    Horizontal,
    Vertical,
}

/// Parameter slider bound to a nih_plug parameter.
///
/// Displays parameter name, a horizontal fill bar, and current value.
/// Drag vertically to adjust, double-click to reset.
#[component]
pub fn ParamSlider(
    /// The nih_plug parameter to control.
    param_ptr: ParamPtr,
    /// Label override (defaults to param name).
    #[props(default)]
    label: Option<String>,
    /// Slider height in pixels.
    #[props(default = 24.0)]
    height: f32,
) -> Element {
    let ctx = use_param_context();
    let mut drag: Signal<DragState> = use_context();
    let mut revision = use_signal(|| 0u32);
    let _ = *revision.read();

    // Re-render when drag is active (so value display updates)
    let _ = *drag.read();

    let normalized = unsafe { param_ptr.modulated_normalized_value() };
    let display_value = unsafe { param_ptr.normalized_value_to_string(normalized, true) };
    let name = label.unwrap_or_else(|| unsafe { param_ptr.name() }.to_string());

    let fill_width = format!("{}%", normalized * 100.0);

    /// Drag sensitivity: pixels of vertical drag per full 0→1 sweep.
    const SENSITIVITY: f64 = 150.0;

    rsx! {
        div {
            style: "display:flex; flex-direction:column; gap:2px; min-width:80px; flex:1;",

            div {
                style: format!(
                    "font-size:10px; color:{TEXT_DIM}; text-transform:uppercase; \
                     letter-spacing:0.3px;"
                ),
                "{name}"
            }

            div {
                style: format!(
                    "height:{height}px; background:{SURFACE}; border-radius:4px; \
                     position:relative; overflow:hidden; cursor:ns-resize; \
                     border:1px solid {BORDER}; user-select:none;"
                ),
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
                        let default = unsafe { param_ptr.default_normalized_value() };
                        ctx.begin_set_raw(param_ptr);
                        ctx.set_normalized_raw(param_ptr, default);
                        ctx.end_set_raw(param_ptr);
                        revision += 1;
                    }
                },

                // Fill bar
                div {
                    style: format!(
                        "position:absolute; left:0; top:0; bottom:0; width:{fill_width}; \
                         background:{ACCENT}; opacity:0.6; pointer-events:none;"
                    ),
                }

                // Centered value text
                div {
                    style: format!(
                        "position:absolute; left:0; right:0; top:0; bottom:0; \
                         display:flex; align-items:center; justify-content:center; \
                         font-size:11px; color:{TEXT}; pointer-events:none; \
                         font-variant-numeric:tabular-nums;"
                    ),
                    "{display_value}"
                }
            }
        }
    }
}

/// A raw styled slider with normalized value and callback.
///
/// For use outside of nih_plug parameter binding.
#[component]
pub fn Slider(
    /// Current normalized value (0.0–1.0).
    #[props(default = 0.5)]
    value: f64,
    /// Minimum value.
    #[props(default = 0.0)]
    min: f64,
    /// Maximum value.
    #[props(default = 1.0)]
    max: f64,
    /// Step increment. Use 0.0 for continuous.
    #[props(default = 0.01)]
    step: f64,
    /// Orientation.
    #[props(default)]
    orientation: SliderOrientation,
    /// Whether the slider is disabled.
    #[props(default)]
    disabled: bool,
    /// Callback when value changes.
    #[props(default)]
    on_change: Option<Callback<f64>>,
) -> Element {
    let range = max - min;
    let pct = if range > 0.0 {
        ((value - min) / range * 100.0).clamp(0.0, 100.0)
    } else {
        0.0
    };

    let is_vertical = orientation == SliderOrientation::Vertical;
    let opacity = if disabled { "0.5" } else { "1.0" };

    let container_style = if is_vertical {
        format!(
            "position:relative; width:8px; height:100%; display:flex; \
             align-items:center; justify-content:center; opacity:{opacity};"
        )
    } else {
        format!(
            "position:relative; height:8px; width:100%; display:flex; \
             align-items:center; opacity:{opacity};"
        )
    };

    let track_style = if is_vertical {
        format!(
            "position:relative; width:8px; flex:1; overflow:hidden; \
             border-radius:4px; background:{BORDER};"
        )
    } else {
        format!(
            "position:relative; height:8px; width:100%; flex:1; overflow:hidden; \
             border-radius:4px; background:{BORDER};"
        )
    };

    let fill_style = if is_vertical {
        format!(
            "height:{pct}%; width:100%; position:absolute; bottom:0; \
             background:{ACCENT};"
        )
    } else {
        format!("width:{pct}%; height:100%; background:{ACCENT};")
    };

    let thumb_style = if is_vertical {
        format!(
            "position:absolute; left:50%; bottom:calc({pct}% - 7px); \
             transform:translateX(-50%); width:14px; height:14px; \
             border-radius:7px; border:2px solid {ACCENT}; background:{BG};"
        )
    } else {
        format!(
            "position:absolute; top:50%; left:calc({pct}% - 7px); \
             transform:translateY(-50%); width:14px; height:14px; \
             border-radius:7px; border:2px solid {ACCENT}; background:{BG};"
        )
    };

    rsx! {
        div {
            style: container_style,

            div {
                style: track_style,
                div { style: fill_style }
                div { style: thumb_style }
            }

            if !disabled {
                input {
                    r#type: "range",
                    style: "position:absolute; inset:0; opacity:0; cursor:pointer;",
                    min: min.to_string(),
                    max: max.to_string(),
                    step: step.to_string(),
                    value: value.to_string(),
                    oninput: move |evt: FormEvent| {
                        if let Ok(val) = evt.value().parse::<f64>() {
                            if let Some(cb) = &on_change {
                                cb.call(val);
                            }
                        }
                    },
                }
            }
        }
    }
}
