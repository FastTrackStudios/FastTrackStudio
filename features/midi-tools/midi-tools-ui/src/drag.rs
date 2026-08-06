//! Sliders, built from pointer events and inline styles.
//!
//! Not `<input type=range>` and not a component-library slider: this
//! panel has to render identically standalone, as a plugin, and embedded
//! in REAPER, and the REAPER path goes through Blitz, which does not
//! give you native form-control chrome or reliable external CSS. So the
//! track and the thumb are `div`s with explicit pixel styles, and the
//! interaction is arithmetic on pointer coordinates.
//!
//! Everything here takes its geometry as explicit style values rather
//! than Tailwind classes, per the signal UI rules — classes are additive
//! polish, and a slider that collapses to zero height without a
//! stylesheet is not a slider.

use dioxus::prelude::*;

/// Pixel geometry shared by the horizontal sliders, so the four sections
/// line up without each one hard-coding its own numbers.
pub const TRACK_H: f64 = 6.0;
pub const THUMB: f64 = 14.0;

/// Fraction of the way along `element` the pointer sits, 0.0..=1.0.
///
/// Uses the element's own bounding rect rather than the event's offset
/// coordinates: offsets are reported relative to whatever child is under
/// the cursor, so once the pointer crosses onto the thumb the value
/// jumps. Measuring against the track is what makes a drag continuous.
fn fraction_along(rect_x: f64, rect_w: f64, client_x: f64) -> f64 {
    if rect_w <= 0.0 {
        return 0.0;
    }
    ((client_x - rect_x) / rect_w).clamp(0.0, 1.0)
}

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
    /// Colour of the filled portion of the track.
    #[props(default = "var(--primary, #d2691e)".to_string())]
    fill: String,
    #[props(default = 120.0)] width: f64,
    on_change: EventHandler<f64>,
) -> Element {
    let span = (max - min).max(f64::EPSILON);
    let position = ((value - min) / span).clamp(0.0, 1.0);
    let mut dragging = use_signal(|| false);
    // The track's page geometry, captured on press. Re-measuring per
    // move would be a layout read on every pointer event.
    let mut rect = use_signal(|| (0.0_f64, 0.0_f64));

    let emit = move |frac: f64| on_change.call(min + frac * span);

    rsx! {
        div {
            style: "position:relative; width:{width}px; height:{THUMB}px; display:flex; align-items:center; cursor:pointer; touch-action:none; flex:none;",
            onmounted: move |e| async move {
                if let Ok(r) = e.get_client_rect().await {
                    rect.set((r.origin.x, r.size.width));
                }
            },
            onpointerdown: move |e| {
                dragging.set(true);
                let (x, w) = rect();
                emit(fraction_along(x, w, e.data().client_coordinates().x));
            },
            onpointermove: move |e| {
                if dragging() {
                    let (x, w) = rect();
                    emit(fraction_along(x, w, e.data().client_coordinates().x));
                }
            },
            onpointerup: move |_| dragging.set(false),
            // Releasing outside the track must end the drag too, or the
            // thumb follows the cursor around the panel afterwards.
            onpointerleave: move |_| dragging.set(false),

            // Track.
            div { style: "position:absolute; left:0; right:0; height:{TRACK_H}px; border-radius:3px; background:var(--muted, #2a2a2a);" }
            // Fill.
            div { style: "position:absolute; left:0; width:{position * 100.0}%; height:{TRACK_H}px; border-radius:3px; background:{fill};" }
            // Thumb.
            div {
                style: "position:absolute; left:calc({position * 100.0}% - {THUMB / 2.0}px); width:{THUMB}px; height:{THUMB}px; border-radius:50%; background:var(--foreground, #e8e8e8); border:1px solid var(--border, #444); pointer-events:none;",
            }
        }
    }
}

/// A two-thumb range slider — MVelocity's RANGE control.
///
/// Grabs whichever thumb is nearer the press, then lets that thumb push
/// past the other rather than pinning it. Crossing the thumbs is a
/// natural way to say "I want the window over there", and
/// `velocity::Range::new` reorders the bounds for exactly this reason.
#[component]
pub fn RangeSlider(
    low: f64,
    high: f64,
    min: f64,
    max: f64,
    #[props(default = 150.0)] width: f64,
    on_change: EventHandler<(f64, f64)>,
) -> Element {
    let span = (max - min).max(f64::EPSILON);
    let lo_pos = ((low - min) / span).clamp(0.0, 1.0);
    let hi_pos = ((high - min) / span).clamp(0.0, 1.0);

    // Which thumb the current drag owns. `None` between drags.
    let mut held = use_signal(|| Option::<bool>::None);
    let mut rect = use_signal(|| (0.0_f64, 0.0_f64));

    let apply = move |frac: f64, low_thumb: bool| {
        let v = min + frac * span;
        if low_thumb {
            on_change.call((v, high));
        } else {
            on_change.call((low, v));
        }
    };

    rsx! {
        div {
            style: "position:relative; width:{width}px; height:{THUMB}px; display:flex; align-items:center; cursor:pointer; touch-action:none; flex:none;",
            onmounted: move |e| async move {
                if let Ok(r) = e.get_client_rect().await {
                    rect.set((r.origin.x, r.size.width));
                }
            },
            onpointerdown: move |e| {
                let (x, w) = rect();
                let frac = fraction_along(x, w, e.data().client_coordinates().x);
                let low_thumb = (frac - lo_pos).abs() <= (frac - hi_pos).abs();
                held.set(Some(low_thumb));
                apply(frac, low_thumb);
            },
            onpointermove: move |e| {
                if let Some(low_thumb) = held() {
                    let (x, w) = rect();
                    apply(fraction_along(x, w, e.data().client_coordinates().x), low_thumb);
                }
            },
            onpointerup: move |_| held.set(None),
            onpointerleave: move |_| held.set(None),

            div { style: "position:absolute; left:0; right:0; height:{TRACK_H}px; border-radius:3px; background:var(--muted, #2a2a2a);" }
            div {
                style: "position:absolute; left:{lo_pos.min(hi_pos) * 100.0}%; width:{(hi_pos - lo_pos).abs() * 100.0}%; height:{TRACK_H}px; border-radius:3px; background:var(--primary, #d2691e);",
            }
            for pos in [lo_pos, hi_pos] {
                div {
                    style: "position:absolute; left:calc({pos * 100.0}% - {THUMB / 2.0}px); width:{THUMB}px; height:{THUMB}px; border-radius:50%; background:var(--foreground, #e8e8e8); border:1px solid var(--border, #444); pointer-events:none;",
                }
            }
        }
    }
}

/// A column of vertical bars you can draw on by dragging across them.
///
/// This is MVelocity's step-velocity slider bank, and it's how a pattern
/// should be edited: the bars *are* the pattern, drawn at the same scale
/// as the velocities they set, and dragging across several sets them all
/// in one gesture the way a drum machine's velocity lane does.
#[component]
pub fn BarEditor(
    /// Bar heights, in `0..=max`.
    values: Vec<u8>,
    #[props(default = 127)] max: u8,
    #[props(default = 88.0)] height: f64,
    on_change: EventHandler<(usize, u8)>,
) -> Element {
    let mut dragging = use_signal(|| false);
    let mut rect = use_signal(|| (0.0_f64, 0.0_f64, 0.0_f64, 0.0_f64));
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

    rsx! {
        div {
            style: "position:relative; display:flex; align-items:flex-end; gap:2px; height:{height}px; padding:3px; border-radius:4px; background:var(--muted, #1e1e1e); border:1px solid var(--border, #333); cursor:crosshair; touch-action:none;",
            onmounted: move |e| async move {
                if let Ok(r) = e.get_client_rect().await {
                    rect.set((r.origin.x, r.origin.y, r.size.width, r.size.height));
                }
            },
            onpointerdown: move |e| {
                dragging.set(true);
                let c = e.data().client_coordinates();
                if let Some((i, v)) = hit(c.x, c.y) { on_change.call((i, v)); }
            },
            onpointermove: move |e| {
                if dragging() {
                    let c = e.data().client_coordinates();
                    if let Some((i, v)) = hit(c.x, c.y) { on_change.call((i, v)); }
                }
            },
            onpointerup: move |_| dragging.set(false),
            onpointerleave: move |_| dragging.set(false),

            for (i, v) in values.iter().copied().enumerate() {
                div {
                    key: "{i}",
                    style: "flex:1; min-width:6px; height:{f64::from(v) / f64::from(max) * 100.0}%; border-radius:2px 2px 0 0; background:var(--primary, #d2691e); pointer-events:none;",
                }
            }
        }
    }
}
