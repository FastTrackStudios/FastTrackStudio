//! Parameter-bound slider widget.

use dioxus::prelude::*;
use nih_plug::prelude::ParamPtr;
use nih_plug_dioxus::context::use_param_context;

use crate::core::modulation::ModulationRange;
use crate::core::normal::Normal;
use crate::core::sensitivity::ModifierKeys;
use crate::theming::style::{ControlState, SliderStyle};
use crate::widgets::hslider::SliderVariant;

/// Horizontal slider bound to a nih-plug parameter.
///
/// # Example
///
/// ```ignore
/// use audio_controls::prelude::*;
///
/// #[component]
/// fn PluginUI(params: Arc<MyParams>) -> Element {
///     rsx! {
///         ParamSlider {
///             param_ptr: params.gain.as_ptr(),
///             width: 200,
///         }
///     }
/// }
/// ```
#[component]
pub fn ParamSlider(
    param_ptr: ParamPtr,
    #[props(default)] variant: SliderVariant,
    #[props(default = 120)] width: u32,
    #[props(default = 24)] height: u32,
    #[props(default = true)] show_label: bool,
    #[props(default = true)] show_value: bool,
    #[props(default)] class: String,
) -> Element {
    let ctx = use_param_context();
    let mut is_dragging = use_signal(|| false);
    let mut drag_start_y = use_signal(|| 0.0f32);
    let mut drag_start_value = use_signal(|| 0.0f32);
    let mut is_hovered = use_signal(|| false);

    // Read parameter state
    let normalized = unsafe { param_ptr.modulated_normalized_value() };
    let unmodulated = unsafe { param_ptr.unmodulated_normalized_value() };
    let display_value = unsafe { param_ptr.normalized_value_to_string(normalized, true) };
    let name = unsafe { param_ptr.name() };
    let default_normalized = unsafe { param_ptr.default_normalized_value() };

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

    let is_bipolar = matches!(variant, SliderVariant::Bipolar);
    let style = match &variant {
        SliderVariant::Custom(s) => s.clone(),
        _ => SliderStyle::css_vars(state),
    };

    let fill_percent = normalized * 100.0;

    let mod_style = if modulation.active {
        let left = modulation.start * 100.0;
        let width = (modulation.end - modulation.start) * 100.0;
        Some(format!("left: {left}%; width: {width}%;"))
    } else {
        None
    };

    rsx! {
        div {
            class: "param-slider-container flex flex-col gap-1 {class}",

            // Label row
            if show_label || show_value {
                div {
                    class: "flex justify-between items-center text-xs",
                    if show_label {
                        span {
                            class: "text-muted-foreground select-none",
                            "{name}"
                        }
                    }
                    if show_value {
                        span {
                            class: "font-mono text-muted-foreground select-none",
                            "{display_value}"
                        }
                    }
                }
            }

            // Track
            div {
                class: "relative rounded-full",
                style: "width: {width}px; height: {height}px; background: {style.track_color};",

                // Modulation range
                if let Some(ref m_style) = mod_style {
                    div {
                        class: "absolute top-0 h-full rounded-full opacity-50",
                        style: "background: {style.modulation_color}; {m_style}",
                    }
                }

                // Fill
                if is_bipolar {
                    if normalized >= 0.5 {
                        div {
                            class: "absolute top-0 h-full rounded-r-full",
                            style: "background: {style.fill_color}; left: 50%; width: {(normalized - 0.5) * 100.0}%;",
                        }
                    } else {
                        div {
                            class: "absolute top-0 h-full rounded-l-full",
                            style: "background: {style.fill_color}; left: {fill_percent}%; width: {(0.5 - normalized) * 100.0}%;",
                        }
                    }
                    div {
                        class: "absolute top-0 h-full",
                        style: "left: 50%; width: 2px; background: {style.track_color}; transform: translateX(-50%);",
                    }
                } else {
                    div {
                        class: "absolute top-0 left-0 h-full rounded-full",
                        style: "background: {style.fill_color}; width: {fill_percent}%;",
                    }
                }

                // Thumb
                div {
                    class: "absolute top-1/2 -translate-y-1/2 rounded-full border-2 border-background shadow-sm",
                    style: "width: {style.thumb_size}px; height: {style.thumb_size}px; background: {style.thumb_color}; left: calc({fill_percent}% - {style.thumb_size / 2.0}px);",
                }

                // Interaction overlay
                div {
                    class: "absolute inset-0 cursor-pointer",

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
                            // Use vertical drag for precision
                            let delta_y = drag_start_y() - evt.client_coordinates().y as f32;
                            let modifiers = ModifierKeys::new(evt.modifiers().shift(), evt.modifiers().ctrl(), evt.modifiers().alt());
                            let sensitivity = if modifiers.fine_control() { 0.001 } else { 0.01 };
                            let delta = delta_y * sensitivity;
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
                }
            }
        }
    }
}
