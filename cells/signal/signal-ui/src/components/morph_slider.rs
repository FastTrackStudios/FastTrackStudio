//! Morph slider component — A/B snapshot morphing with assignment buttons.
//!
//! Provides a horizontal slider for smoothly morphing between two snapshots:
//!
//! ```text
//! ┌──────────────────────────────────────────────────────┐
//! │  [A: Verse  ▾]  A───────────[●]───────────B  [B: Chorus ▾]  │
//! │                    ⚠ Structural differences               │
//! └──────────────────────────────────────────────────────┘
//! ```

use crate::prelude::*;
use signal_control::PresetSnapshotInfo;
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// MorphSlider
// ─────────────────────────────────────────────────────────────────────────────

/// Props for the morph slider component.
#[derive(Props, Clone, PartialEq)]
pub struct MorphSliderProps {
    /// Current morph position [0.0, 1.0].
    pub position: f64,
    /// Callback when the user drags the slider.
    pub on_position_change: Callback<f64>,
    /// Currently assigned snapshot A (if any).
    pub snapshot_a: Option<PresetSnapshotInfo>,
    /// Currently assigned snapshot B (if any).
    pub snapshot_b: Option<PresetSnapshotInfo>,
    /// Available snapshots to choose from.
    pub available_snapshots: Vec<PresetSnapshotInfo>,
    /// Callback when user assigns snapshot A.
    pub on_assign_a: Callback<Uuid>,
    /// Callback when user assigns snapshot B.
    pub on_assign_b: Callback<Uuid>,
    /// Whether structural differences prevent morphing.
    #[props(default = false)]
    pub has_structural_warning: bool,
}

/// A horizontal morph slider with A/B snapshot assignment buttons.
///
/// The slider interpolates between snapshot A (left, position=0.0) and
/// snapshot B (right, position=1.0). Assignment dropdowns let the user
/// pick which snapshots are loaded into each slot.
#[component]
pub fn MorphSlider(props: MorphSliderProps) -> Element {
    let mut dropdown_a_open = use_signal(|| false);
    let mut dropdown_b_open = use_signal(|| false);

    // Snapshot label helpers
    let label_a = props
        .snapshot_a
        .as_ref()
        .map(|s| s.name.clone())
        .unwrap_or_else(|| "—".to_string());
    let label_b = props
        .snapshot_b
        .as_ref()
        .map(|s| s.name.clone())
        .unwrap_or_else(|| "—".to_string());

    let position_pct = (props.position * 100.0).round();
    let has_warning = props.has_structural_warning;

    rsx! {
        div { class: "flex flex-col gap-1",
            // Main row: [A button] — slider — [B button]
            div { class: "flex items-center gap-3",
                // Snapshot A assignment button
                div { class: "relative",
                    button {
                        class: "flex items-center gap-1 px-2.5 py-1 rounded-md text-xs font-medium \
                                bg-blue-900/40 text-blue-300 border border-blue-800/50 \
                                hover:bg-blue-800/50 transition-colors min-w-[80px] justify-between",
                        onclick: move |_| {
                            *dropdown_a_open.write() = !dropdown_a_open();
                            *dropdown_b_open.write() = false;
                        },
                        span { "A: {label_a}" }
                        span { class: "text-[10px] opacity-60", "▾" }
                    }
                    // Dropdown for A
                    if dropdown_a_open() {
                        {snapshot_dropdown(
                            &props.available_snapshots,
                            props.snapshot_a.as_ref().map(|s| s.id),
                            &props.on_assign_a,
                            &mut dropdown_a_open,
                        )}
                    }
                }

                // Slider track
                div { class: "flex-1 flex items-center gap-2",
                    span { class: "text-[10px] font-bold text-blue-400 select-none", "A" }
                    div { class: "flex-1 relative h-6 flex items-center",
                        // Track background
                        div { class: "absolute inset-x-0 h-1 bg-zinc-700 rounded-full" }
                        // Filled portion (A→thumb)
                        div {
                            class: "absolute left-0 h-1 bg-gradient-to-r from-blue-500 to-orange-500 rounded-full",
                            style: "width: {position_pct}%",
                        }
                        // Range input (invisible but captures interaction)
                        input {
                            r#type: "range",
                            class: "absolute inset-0 w-full h-full opacity-0 cursor-pointer z-10",
                            min: "0",
                            max: "1000",
                            value: "{(props.position * 1000.0).round() as i64}",
                            oninput: move |evt| {
                                if let Ok(val) = evt.value().parse::<f64>() {
                                    props.on_position_change.call(val / 1000.0);
                                }
                            },
                        }
                        // Thumb indicator
                        div {
                            class: "absolute w-4 h-4 rounded-full bg-white border-2 border-zinc-400 \
                                    shadow-md pointer-events-none transform -translate-x-1/2",
                            style: "left: {position_pct}%",
                        }
                    }
                    span { class: "text-[10px] font-bold text-orange-400 select-none", "B" }
                }

                // Snapshot B assignment button
                div { class: "relative",
                    button {
                        class: "flex items-center gap-1 px-2.5 py-1 rounded-md text-xs font-medium \
                                bg-orange-900/40 text-orange-300 border border-orange-800/50 \
                                hover:bg-orange-800/50 transition-colors min-w-[80px] justify-between",
                        onclick: move |_| {
                            *dropdown_b_open.write() = !dropdown_b_open();
                            *dropdown_a_open.write() = false;
                        },
                        span { "B: {label_b}" }
                        span { class: "text-[10px] opacity-60", "▾" }
                    }
                    // Dropdown for B
                    if dropdown_b_open() {
                        {snapshot_dropdown(
                            &props.available_snapshots,
                            props.snapshot_b.as_ref().map(|s| s.id),
                            &props.on_assign_b,
                            &mut dropdown_b_open,
                        )}
                    }
                }
            }

            // Warning row (structural differences)
            if has_warning {
                div { class: "flex items-center gap-1.5 px-2 py-1 rounded bg-amber-900/30 \
                              border border-amber-800/40 text-amber-400 text-[11px]",
                    // Warning icon
                    svg {
                        class: "w-3.5 h-3.5 flex-shrink-0",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "2",
                        view_box: "0 0 24 24",
                        path {
                            stroke_linecap: "round",
                            stroke_linejoin: "round",
                            d: "M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 \
                                1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 \
                                16c-.77 1.333.192 3 1.732 3z",
                        }
                    }
                    span { "Structural differences — snapshots use different patches. Morphing disabled, use crossfade." }
                }
            }
        }
    }
}

/// Render a dropdown of available snapshots for assignment.
fn snapshot_dropdown(
    available: &[PresetSnapshotInfo],
    current_id: Option<Uuid>,
    on_select: &Callback<Uuid>,
    open_signal: &mut Signal<bool>,
) -> Element {
    let on_select = on_select.clone();
    let mut open_signal = *open_signal;

    rsx! {
        div { class: "absolute top-full left-0 mt-1 z-50 min-w-[140px] py-1 \
                      bg-zinc-800 border border-zinc-700 rounded-lg shadow-xl",
            if available.is_empty() {
                div { class: "px-3 py-2 text-xs text-zinc-500 italic", "No scenes available" }
            }
            for snap in available.iter() {
                {
                    let snap_id = snap.id;
                    let is_current = current_id == Some(snap_id);
                    let name = snap.name.clone();
                    rsx! {
                        button {
                            key: "{snap_id}",
                            class: if is_current {
                                "w-full text-left px-3 py-1.5 text-xs bg-zinc-700 text-white"
                            } else {
                                "w-full text-left px-3 py-1.5 text-xs text-zinc-300 hover:bg-zinc-700 transition-colors"
                            },
                            onclick: move |_| {
                                on_select.call(snap_id);
                                *open_signal.write() = false;
                            },
                            "{name}"
                        }
                    }
                }
            }
        }
    }
}
