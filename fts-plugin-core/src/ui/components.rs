//! Reusable Blitz-compatible UI components for FTS plugins.
//!
//! All components use inline styles — Blitz doesn't have full Tailwind CSS
//! coverage for external component libraries, so inline styles are the
//! reliable path for plugin GUIs.

use super::theme::*;
use nih_plug::prelude::ParamPtr;
use nih_plug_dioxus::prelude::*;

/// Toggle switch bound to a BoolParam via ParamPtr.
///
/// Uses a local revision signal to force Dioxus re-renders on click,
/// since `param_ptr.modulated_normalized_value()` is not reactive.
#[component]
pub fn Toggle(param_ptr: ParamPtr, label: Option<&'static str>) -> Element {
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

/// Card-style section wrapper with uppercase title.
#[component]
pub fn Section(title: &'static str, children: Element) -> Element {
    rsx! {
        div {
            style: "background:{CARD_BG}; border-radius:6px; padding:10px 12px; margin-bottom:8px;",
            div {
                style: "font-size:11px; font-weight:600; text-transform:uppercase; \
                        letter-spacing:0.5px; color:{TEXT_DIM}; margin-bottom:6px;",
                "{title}"
            }
            {children}
        }
    }
}

/// Pill-style segment button for mutually exclusive selections.
///
/// Typically used in a row for enum params (click sounds, modes, etc.).
/// The parent is responsible for tracking the selected index and calling
/// `ParamContext` to set the param value.
#[component]
pub fn SegmentButton(label: &'static str, selected: bool, on_click: EventHandler<()>) -> Element {
    let bg = if selected { ACCENT } else { "transparent" };
    let border = if selected { ACCENT } else { BORDER };
    let color = if selected { "#fff" } else { TEXT_DIM };

    rsx! {
        div {
            style: format!(
                "padding:4px 8px; border-radius:4px; font-size:11px; font-weight:500; \
                 cursor:pointer; border:1px solid {border}; background:{bg}; color:{color};"
            ),
            onclick: move |_| on_click.call(()),
            "{label}"
        }
    }
}

/// Full-width action button (e.g., "Generate Guide MIDI").
#[component]
pub fn ActionButton(label: &'static str, on_click: EventHandler<()>) -> Element {
    rsx! {
        div {
            style: format!(
                "padding:8px 0; cursor:pointer; text-align:center; \
                 background:{ACCENT}; color:#fff; border-radius:6px; \
                 font-size:13px; font-weight:600; margin-top:4px;"
            ),
            onclick: move |_| on_click.call(()),
            "{label}"
        }
    }
}

/// Header bar with plugin title + transport info.
#[component]
pub fn Header(
    title: &'static str,
    tempo: f32,
    time_sig_num: i32,
    time_sig_den: i32,
    is_playing: bool,
) -> Element {
    rsx! {
        div {
            style: "display:flex; justify-content:space-between; align-items:center; \
                    margin-bottom:10px; padding-bottom:8px; border-bottom:1px solid {BORDER};",
            div { style: "font-size:18px; font-weight:700;", "{title}" }
            div {
                style: "display:flex; gap:12px; align-items:center; font-size:12px; color:{TEXT_DIM};",
                span { "{tempo:.0} BPM" }
                span { "{time_sig_num}/{time_sig_den}" }
                span {
                    style: format!("padding:2px 8px; border-radius:4px; font-size:11px; \
                                   background:{}; color:#fff;",
                                   if is_playing { GREEN } else { "#555" }),
                    if is_playing { "PLAY" } else { "STOP" }
                }
            }
        }
    }
}

/// Status bar showing beat position and optional window size.
#[component]
pub fn StatusBar(beat_position: f32) -> Element {
    rsx! {
        div {
            style: "padding-top:6px; margin-top:8px; border-top:1px solid {BORDER}; \
                    font-size:11px; color:{TEXT_DIM};",
            {
                let size_info = if let Some(state) = try_use_context::<std::sync::Arc<DioxusState>>() {
                    let (w, h) = state.size();
                    format!("Beat: {beat_position:.2} | {w}x{h}")
                } else {
                    format!("Beat: {beat_position:.2}")
                };
                rsx! { "{size_info}" }
            }
        }
    }
}
