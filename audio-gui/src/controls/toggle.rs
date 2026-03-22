//! Toggle switch — bound to a nih_plug BoolParam via ParamPtr.
//!
//! LED-style illumination with soft glow when active.

use crate::theme;
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
    let track_shadow = if on {
        format!(
            "{SUBTLE}, 0 0 8px {GLOW}",
            SUBTLE = theme::SHADOW_SUBTLE,
            GLOW = theme::ACCENT_GLOW,
        )
    } else {
        format!("{INSET}", INSET = theme::SHADOW_INSET,)
    };

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
                     background:{track_bg}; transition:{TRANS}; \
                     box-shadow:{track_shadow};",
                    TRANS = theme::TRANSITION_FAST,
                ),
                // Thumb with 3D lighting
                div {
                    style: format!(
                        "width:16px; height:16px; border-radius:8px; \
                         background:linear-gradient(145deg, #ffffff, #e0e0e0); \
                         position:absolute; top:2px; left:{thumb_x}; \
                         transition:left 0.15s; \
                         box-shadow:{SHADOW};",
                        SHADOW = theme::SHADOW_SUBTLE,
                    ),
                }
            }
            if let Some(lbl) = label {
                span {
                    style: format!(
                        "font-size:{FSIZE}; color:{c};",
                        FSIZE = theme::FONT_SIZE_VALUE,
                        c = if on { TEXT } else { TEXT_DIM },
                    ),
                    "{lbl}"
                }
            }
        }
    }
}
