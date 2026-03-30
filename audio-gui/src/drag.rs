//! Global drag capture for Blitz-based plugin UIs.
//!
//! Blitz dispatches mouse events to whichever element is under the cursor (no
//! pointer capture). This module provides a shared drag context so that knobs
//! and sliders can start a drag on mousedown, and the root `DragProvider`
//! wrapper handles mousemove / mouseup at the top level regardless of which
//! element the cursor is over.

use nih_plug::prelude::ParamPtr;
use nih_plug_dioxus::prelude::*;

/// Fine-adjustment multiplier when Shift is held during drag.
const FINE_MULTIPLIER: f64 = 5.0;

/// Shared state for a parameter drag in progress.
#[derive(Clone, Copy, Default)]
pub struct DragState {
    pub active: bool,
    pub param_ptr: Option<ParamPtr>,
    pub start_value: f64,
    pub start_y: f64,
    pub sensitivity: f64,
    /// Whether shift was held on the last mousemove (for re-anchoring).
    pub last_shift: bool,
    /// Incremented on each mousemove so subscribers (knobs) re-render.
    pub move_count: u64,
}

/// Begin a drag. Call from a knob/slider's `onmousedown`.
pub fn begin_drag(
    drag: &mut Signal<DragState>,
    ctx: &ParamContext,
    param_ptr: ParamPtr,
    start_y: f64,
    sensitivity: f64,
) {
    let normalized = unsafe { param_ptr.modulated_normalized_value() } as f64;
    ctx.begin_set_raw(param_ptr);
    drag.set(DragState {
        active: true,
        param_ptr: Some(param_ptr),
        start_value: normalized,
        start_y,
        sensitivity,
        last_shift: false,
        move_count: 0,
    });
}

/// Wrapper component that captures mouse events for parameter drags.
///
/// Place this around your entire editor UI. It provides a `Signal<DragState>`
/// context that knobs and sliders use to initiate drags.
#[component]
pub fn DragProvider(children: Element) -> Element {
    let mut drag = use_signal(DragState::default);
    let mut revision = use_signal(|| 0u32);
    let ctx = use_param_context();

    // Provide drag state to child components
    use_context_provider(|| drag);

    let _ = *revision.read();

    rsx! {
        div {
            style: "width:100vw; height:100vh;",

            onmousemove: {
                let ctx = ctx.clone();
                move |evt: MouseEvent| {
                    let state = *drag.read();
                    if state.active {
                        if let Some(param_ptr) = state.param_ptr {
                            let shift_held = evt.modifiers().shift();
                            let cur_y = evt.client_coordinates().y;

                            // Re-anchor when shift state changes to avoid jump
                            let (anchor_value, anchor_y) = if shift_held != state.last_shift {
                                // Read current param value as new anchor
                                let current =
                                    unsafe { param_ptr.modulated_normalized_value() } as f64;
                                (current, cur_y)
                            } else {
                                (state.start_value, state.start_y)
                            };

                            let sens = if shift_held {
                                state.sensitivity * FINE_MULTIPLIER
                            } else {
                                state.sensitivity
                            };
                            let delta = (anchor_y - cur_y) / sens;
                            let new_val = (anchor_value + delta).clamp(0.0, 1.0) as f32;
                            ctx.set_normalized_raw(param_ptr, new_val);

                            let mut s = state;
                            s.start_value = anchor_value;
                            s.start_y = anchor_y;
                            s.last_shift = shift_held;
                            s.move_count += 1;
                            drag.set(s);
                        }
                    }
                }
            },

            onmouseup: {
                let ctx = ctx.clone();
                move |_| {
                    let state = *drag.read();
                    if state.active {
                        if let Some(param_ptr) = state.param_ptr {
                            ctx.end_set_raw(param_ptr);
                        }
                        drag.set(DragState::default());
                    }
                }
            },

            {children}
        }
    }
}
