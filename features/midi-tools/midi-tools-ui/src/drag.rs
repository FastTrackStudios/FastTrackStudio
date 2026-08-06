//! Sliders and the bar editor.
//!
//! The sliders wrap `dioxus_primitives::slider` rather than hand-rolling
//! pointer arithmetic. The first cut of this file did hand-roll it, and
//! it flickered: a hand-rolled slider that feeds its *clamped* value back
//! in as its displayed value oscillates whenever the pointer sits between
//! two steps, and one that measures its track only at mount drifts as
//! soon as the panel scrolls. The primitive already solves both — it
//! keeps unclamped "granular" thumb state through a drag, re-measures the
//! track on `pointerdown` and on resize, and picks the active thumb once
//! at drag start instead of per move (which is what made the two-thumb
//! range slider swap its thumbs mid-drag).
//!
//! What stays local is the *styling*. The panel has to render identically
//! standalone, as a plugin, and inside REAPER through Blitz, so nothing
//! here depends on a stylesheet: every layout-critical value is an inline
//! `style="..."`, per the signal UI rules. That's also why this doesn't
//! use `fts_ui::components::Slider`, which is the same primitive dressed
//! in Tailwind classes and collapses to nothing without them.

use dioxus::prelude::*;
use dioxus_primitives::slider::{
    RangeSlider as PrimitiveRangeSlider, Slider as PrimitiveSlider, SliderRange, SliderThumb,
    SliderTrack,
};

/// Thumb diameter. The slider's row is sized to it so a thumb that
/// overhangs the 6px track doesn't get clipped by the row above.
const THUMB: f64 = 14.0;

/// Written out rather than interpolated from [`TRACK_H`] / [`THUMB`]:
/// these are `const`s so they can be handed straight to the primitive's
/// `style` attribute, and formatting a const string needs a macro crate
/// this panel has no other reason to depend on. Keep them in step by
/// hand — they only change when the slider is redesigned.
const TRACK_STYLE: &str = "position:relative; height:6px; width:100%; border-radius:3px; background:var(--muted, #2a2a2a); overflow:visible;";

/// The thumb is a `<button>`, so its chrome is reset explicitly — a
/// default button background and border would show through.
const THUMB_STYLE: &str = "position:absolute; top:50%; width:14px; height:14px; margin-left:-7px; margin-top:-7px; padding:0; border-radius:50%; background:var(--foreground, #e8e8e8); border:1px solid var(--border, #444); cursor:pointer; touch-action:none;";

const RANGE_STYLE: &str = "position:absolute; top:0; height:6px; border-radius:3px; background:var(--primary, #d2691e);";

/// A horizontal slider over an arbitrary numeric range.
///
/// `on_change` fires continuously through the drag — every engine here is
/// cheap and pure, so live feedback costs nothing and a velocity tool
/// that only updates on release is unusable.
#[component]
pub fn Slider(
    value: f64,
    min: f64,
    max: f64,
    #[props(default = 0.01)] step: f64,
    #[props(default = 120.0)] width: f64,
    on_change: EventHandler<f64>,
) -> Element {
    rsx! {
        div {
            style: "width:{width}px; flex:none; display:flex; align-items:center; height:{THUMB}px;",
            PrimitiveSlider {
                value: Some(value),
                min,
                max,
                step,
                label: None::<String>,
                on_value_change: move |v: f64| on_change.call(v),
                style: "position:relative; display:flex; align-items:center; width:100%; touch-action:none; user-select:none;",
                SliderTrack { style: TRACK_STYLE,
                    SliderRange { style: RANGE_STYLE }
                    SliderThumb { style: THUMB_STYLE }
                }
            }
        }
    }
}

/// A two-thumb range slider — MVelocity's RANGE control.
#[component]
pub fn RangeSlider(
    low: f64,
    high: f64,
    min: f64,
    max: f64,
    #[props(default = 1.0)] step: f64,
    #[props(default = 150.0)] width: f64,
    on_change: EventHandler<(f64, f64)>,
) -> Element {
    rsx! {
        div {
            style: "width:{width}px; flex:none; display:flex; align-items:center; height:{THUMB}px;",
            PrimitiveRangeSlider {
                value: Some(low..high),
                min,
                max,
                step,
                label: None::<String>,
                on_value_change: move |r: std::ops::Range<f64>| on_change.call((r.start, r.end)),
                style: "position:relative; display:flex; align-items:center; width:100%; touch-action:none; user-select:none;",
                SliderTrack { style: TRACK_STYLE,
                    SliderRange { style: RANGE_STYLE }
                    SliderThumb { index: 0, style: THUMB_STYLE }
                    SliderThumb { index: 1, style: THUMB_STYLE }
                }
            }
        }
    }
}

/// A column of vertical bars you can draw on by dragging across them.
///
/// MVelocity's step-velocity slider bank, and how a pattern should be
/// edited: the bars *are* the pattern, drawn at the same scale as the
/// velocities they set, and dragging across several sets them all in one
/// gesture the way a drum machine's velocity lane does.
///
/// Hand-rolled because no primitive covers "N independent values, drawn
/// across" — but it takes the primitive's two lessons: the box is
/// re-measured at the start of every gesture rather than only at mount,
/// and leaving the box does not cancel the drag.
#[component]
pub fn BarEditor(
    /// Bar heights, in `1..=max`.
    values: Vec<u8>,
    #[props(default = 127)] max: u8,
    #[props(default = 88.0)] height: f64,
    on_change: EventHandler<(usize, u8)>,
) -> Element {
    let mut dragging = use_signal(|| false);
    let mut rect = use_signal(|| (0.0_f64, 0.0_f64, 0.0_f64, 0.0_f64));
    // Held so the box can be re-measured mid-gesture. Measuring only at
    // mount is what makes a hand-rolled widget drift once anything above
    // it grows or the panel scrolls.
    let mut mounted: Signal<Option<std::rc::Rc<MountedData>>> = use_signal(|| None);
    let count = values.len().max(1);

    // Which bar the pointer is over, and how high up it sits.
    let hit = move |cx: f64, cy: f64| -> Option<(usize, u8)> {
        let (x, y, w, h) = rect();
        if w <= 0.0 || h <= 0.0 {
            return None;
        }
        let i = (((cx - x) / w) * count as f64).floor().clamp(0.0, (count - 1) as f64) as usize;
        // Inverted: the top of the box is the highest velocity.
        let frac = 1.0 - ((cy - y) / h).clamp(0.0, 1.0);
        Some((i, (frac * f64::from(max)).round().max(1.0) as u8))
    };

    let measure = move || async move {
        let el = mounted();
        if let Some(el) = el
            && let Ok(r) = el.get_client_rect().await
        {
            rect.set((r.origin.x, r.origin.y, r.size.width, r.size.height));
        }
    };

    rsx! {
        div {
            style: "position:relative; display:flex; align-items:flex-end; gap:2px; height:{height}px; padding:3px; border-radius:4px; background:var(--muted, #1e1e1e); border:1px solid var(--border, #333); cursor:crosshair; touch-action:none; user-select:none;",
            onmounted: move |e| async move {
                mounted.set(Some(e.data()));
                measure().await;
            },
            onresize: move |_| async move { measure().await },
            onpointerdown: move |e| async move {
                dragging.set(true);
                // Re-measure before acting on the very first coordinate,
                // so a gesture can never be applied against a stale box.
                measure().await;
                let c = e.data().client_coordinates();
                if let Some((i, v)) = hit(c.x, c.y) {
                    on_change.call((i, v));
                }
            },
            onpointermove: move |e| {
                if dragging() {
                    let c = e.data().client_coordinates();
                    if let Some((i, v)) = hit(c.x, c.y) {
                        on_change.call((i, v));
                    }
                }
            },
            onpointerup: move |_| dragging.set(false),
            // Deliberately no `onpointerleave` handler: cancelling the
            // drag on leave means a fast stroke that strays a pixel above
            // the box drops the rest of the gesture, which is most of what
            // made this widget feel unreliable. The gesture ends when the
            // pointer is released, wherever that happens.

            for (i, v) in values.iter().copied().enumerate() {
                div {
                    key: "{i}",
                    style: "flex:1; min-width:6px; height:{f64::from(v) / f64::from(max) * 100.0}%; border-radius:2px 2px 0 0; background:var(--primary, #d2691e); pointer-events:none;",
                }
            }
        }
    }
}
