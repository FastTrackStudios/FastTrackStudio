//! Toggle switch — bound to a nih_plug BoolParam via ParamPtr.

use crate::theme::*;
use nih_plug::prelude::ParamPtr;
use nih_plug_dioxus::prelude::*;

/// Toggle switch bound to a boolean nih_plug parameter.
///
/// Uses a local revision signal to force Dioxus re-renders on click,
/// since `param_ptr.modulated_normalized_value()` is not reactive.
#[component]
pub fn Toggle(param_ptr: ParamPtr, #[props(default)] label: Option<&'static str>) -> Element {
    let ctx = use_param_context();
    let mut revision = use_signal(|| 0u32);
    let _ = *revision.read();

    let normalized = unsafe { param_ptr.modulated_normalized_value() };
    let on = normalized > 0.5;

    let track_bg = if on { ACCENT } else { TOGGLE_OFF };
    let thumb_x = if on { "18px" } else { "2px" };

    rsx! {
        div {
            style: "display:flex; align-items:center; gap:6px; cursor:pointer;",
            onclick: {
                let ctx = ctx.clone();
                move |_| {
                    ctx.begin_set_raw(param_ptr);
                    ctx.set_normalized_raw(param_ptr, if on { 0.0 } else { 1.0 });
                    ctx.end_set_raw(param_ptr);
                    revision += 1;
                }
            },
            div {
                style: format!(
                    "width:36px; height:20px; border-radius:10px; position:relative; \
                     background:{track_bg}; transition:background 0.15s;"
                ),
                div {
                    style: format!(
                        "width:16px; height:16px; border-radius:8px; background:#fff; \
                         position:absolute; top:2px; left:{thumb_x}; \
                         transition:left 0.15s;"
                    ),
                }
            }
            if let Some(lbl) = label {
                span {
                    style: format!("font-size:12px; color:{};", if on { TEXT } else { TEXT_DIM }),
                    "{lbl}"
                }
            }
        }
    }
}
