//! A/B Comparison panel — toggle, morph slider, and snapshot assignment.
//!
//! Provides a compact toolbar strip for comparing two snapshots:
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────┐
//! │  [A/B]  [A: Verse ▾]  A━━━━━━━[●]━━━━━━━B  [B: Chorus ▾]  [⇄] [✓] [✗]  │
//! └─────────────────────────────────────────────────────────────────────┘
//! ```
//!
//! When inactive, only the `[A/B]` toggle button is visible. Activating
//! comparison mode expands the full toolbar with morph slider, snapshot
//! assignment dropdowns, swap, accept, and revert controls.

use crate::components::morph_slider::SnapshotRef;
use crate::prelude::*;
use crate::signals::{
    RIG_AB_ACTIVE, RIG_CURRENT_PRESET, RIG_MORPH_POSITION, RIG_MORPH_SNAPSHOT_A,
    RIG_MORPH_SNAPSHOT_B,
};
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// ABComparisonPanel
// ─────────────────────────────────────────────────────────────────────────────

/// A/B comparison toolbar strip.
///
/// When `RIG_AB_ACTIVE` is false, renders a compact toggle button.
/// When active, expands to show snapshot assignment, morph slider,
/// and action buttons (swap, accept, revert).
#[component]
pub fn ABComparisonPanel() -> Element {
    let ab_active = RIG_AB_ACTIVE();
    let morph_pos = RIG_MORPH_POSITION();
    let snapshot_a_id = RIG_MORPH_SNAPSHOT_A();
    let snapshot_b_id = RIG_MORPH_SNAPSHOT_B();

    // Available snapshots — will be populated from DB queries in a future iteration.
    // RigPresetInfo no longer carries inline snapshot data.
    let available_snapshots: Vec<SnapshotRef> = Vec::new();

    // Resolve snapshot info from IDs
    let snapshot_a_info =
        snapshot_a_id.and_then(|id| available_snapshots.iter().find(|s| s.id == id).cloned());
    let snapshot_b_info =
        snapshot_b_id.and_then(|id| available_snapshots.iter().find(|s| s.id == id).cloned());

    // Collapsed state — just the toggle button
    if !ab_active {
        return rsx! {
            button {
                class: "flex items-center gap-1.5 px-3 py-1.5 rounded-lg text-xs font-medium \
                        bg-zinc-800/80 text-zinc-400 border border-zinc-700/50 \
                        hover:bg-zinc-700/80 hover:text-zinc-200 transition-colors",
                onclick: move |_| {
                    *RIG_AB_ACTIVE.write() = true;
                },
                // A/B icon
                span { class: "font-bold text-[11px]", "A/B" }
            }
        };
    }

    // Expanded state — full toolbar
    rsx! {
        div {
            class: "flex items-center gap-2 px-3 py-1.5 rounded-lg \
                    bg-zinc-800/80 border border-zinc-700/50",

            // A/B toggle (active state)
            button {
                class: "flex items-center gap-1 px-2 py-1 rounded text-xs font-bold \
                        bg-indigo-600 text-white hover:bg-indigo-500 transition-colors",
                onclick: move |_| {
                    *RIG_AB_ACTIVE.write() = false;
                },
                "A/B"
            }

            // Snapshot A dropdown
            {snapshot_assignment_button(
                "A",
                snapshot_a_info,
                &available_snapshots,
                "blue",
                move |id: Uuid| *RIG_MORPH_SNAPSHOT_A.write() = Some(id),
            )}

            // Morph slider
            {ab_morph_slider(morph_pos)}

            // Snapshot B dropdown
            {snapshot_assignment_button(
                "B",
                snapshot_b_info,
                &available_snapshots,
                "orange",
                move |id: Uuid| *RIG_MORPH_SNAPSHOT_B.write() = Some(id),
            )}

            // Separator
            div { class: "w-px h-5 bg-zinc-700" }

            // Swap button
            button {
                class: "px-1.5 py-1 rounded text-xs text-zinc-400 \
                        hover:bg-zinc-700 hover:text-zinc-200 transition-colors",
                title: "Swap A and B",
                onclick: move |_| {
                    let a = RIG_MORPH_SNAPSHOT_A();
                    let b = RIG_MORPH_SNAPSHOT_B();
                    *RIG_MORPH_SNAPSHOT_A.write() = b;
                    *RIG_MORPH_SNAPSHOT_B.write() = a;
                    // Invert morph position so the audible result stays the same
                    *RIG_MORPH_POSITION.write() = 1.0 - RIG_MORPH_POSITION();
                },
                // Swap icon (⇄)
                svg {
                    class: "w-4 h-4",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    view_box: "0 0 24 24",
                    path {
                        stroke_linecap: "round",
                        stroke_linejoin: "round",
                        d: "M7 16V4m0 0L3 8m4-4l4 4m6 0v12m0 0l4-4m-4 4l-4-4",
                    }
                }
            }

            // Accept button — apply current morph as permanent state
            button {
                class: "px-1.5 py-1 rounded text-xs text-emerald-400 \
                        hover:bg-emerald-900/40 hover:text-emerald-300 transition-colors",
                title: "Accept current mix",
                onclick: move |_| {
                    // Keep the current morph position applied, just exit comparison mode
                    *RIG_AB_ACTIVE.write() = false;
                },
                // Checkmark icon
                svg {
                    class: "w-4 h-4",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    view_box: "0 0 24 24",
                    path {
                        stroke_linecap: "round",
                        stroke_linejoin: "round",
                        d: "M5 13l4 4L19 7",
                    }
                }
            }

            // Revert button — return to A and exit
            button {
                class: "px-1.5 py-1 rounded text-xs text-red-400 \
                        hover:bg-red-900/40 hover:text-red-300 transition-colors",
                title: "Revert to A",
                onclick: move |_| {
                    *RIG_MORPH_POSITION.write() = 0.0;
                    *RIG_AB_ACTIVE.write() = false;
                },
                // X icon
                svg {
                    class: "w-4 h-4",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    view_box: "0 0 24 24",
                    path {
                        stroke_linecap: "round",
                        stroke_linejoin: "round",
                        d: "M6 18L18 6M6 6l12 12",
                    }
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Internal helpers
// ─────────────────────────────────────────────────────────────────────────────

/// Renders a snapshot assignment dropdown button (A or B side).
fn snapshot_assignment_button(
    label: &str,
    current: Option<SnapshotRef>,
    available: &[SnapshotRef],
    accent: &str,
    on_select: impl Fn(Uuid) + Clone + 'static,
) -> Element {
    let mut dropdown_open = use_signal(|| false);

    let display_name = current
        .as_ref()
        .map(|s| s.name.clone())
        .unwrap_or_else(|| "\u{2014}".to_string()); // em dash

    let current_id = current.as_ref().map(|s| s.id);

    // Build color classes based on accent
    let (bg_class, text_class, border_class, hover_class) = match accent {
        "blue" => (
            "bg-blue-900/40",
            "text-blue-300",
            "border-blue-800/50",
            "hover:bg-blue-800/50",
        ),
        "orange" => (
            "bg-orange-900/40",
            "text-orange-300",
            "border-orange-800/50",
            "hover:bg-orange-800/50",
        ),
        _ => (
            "bg-zinc-800/40",
            "text-zinc-300",
            "border-zinc-700/50",
            "hover:bg-zinc-700/50",
        ),
    };

    let btn_class = format!(
        "flex items-center gap-1 px-2 py-1 rounded-md text-xs font-medium \
         {bg_class} {text_class} border {border_class} \
         {hover_class} transition-colors min-w-[80px] justify-between"
    );

    let available_clone = available.to_vec();

    rsx! {
        div { class: "relative",
            button {
                class: "{btn_class}",
                onclick: move |_| {
                    *dropdown_open.write() = !dropdown_open();
                },
                span { "{label}: {display_name}" }
                span { class: "text-[10px] opacity-60", "\u{25BE}" } // ▾
            }
            if dropdown_open() {
                div {
                    class: "absolute top-full left-0 mt-1 z-50 min-w-[140px] py-1 \
                            bg-zinc-800 border border-zinc-700 rounded-lg shadow-xl",
                    if available_clone.is_empty() {
                        div { class: "px-3 py-2 text-xs text-zinc-500 italic",
                            "No scenes available"
                        }
                    }
                    for snap in available_clone.iter() {
                        {
                            let snap_id = snap.id;
                            let is_current = current_id == Some(snap_id);
                            let name = snap.name.clone();
                            let on_select = on_select.clone();
                            rsx! {
                                button {
                                    key: "{snap_id}",
                                    class: if is_current {
                                        "w-full text-left px-3 py-1.5 text-xs bg-zinc-700 text-white"
                                    } else {
                                        "w-full text-left px-3 py-1.5 text-xs text-zinc-300 \
                                         hover:bg-zinc-700 transition-colors"
                                    },
                                    onclick: move |_| {
                                        on_select(snap_id);
                                        *dropdown_open.write() = false;
                                    },
                                    "{name}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Renders the compact A↔B morph slider.
fn ab_morph_slider(position: f64) -> Element {
    let position_pct = (position * 100.0).round();

    rsx! {
        div { class: "flex items-center gap-1.5 flex-1 min-w-[120px]",
            span { class: "text-[10px] font-bold text-blue-400 select-none", "A" }
            div { class: "flex-1 relative h-5 flex items-center",
                // Track
                div { class: "absolute inset-x-0 h-1 bg-zinc-700 rounded-full" }
                // Fill gradient
                div {
                    class: "absolute left-0 h-1 bg-gradient-to-r from-blue-500 to-orange-500 rounded-full",
                    style: "width: {position_pct}%",
                }
                // Hidden range input
                input {
                    r#type: "range",
                    class: "absolute inset-0 w-full h-full opacity-0 cursor-pointer z-10",
                    min: "0",
                    max: "1000",
                    value: "{(position * 1000.0).round() as i64}",
                    oninput: move |evt| {
                        if let Ok(val) = evt.value().parse::<f64>() {
                            *RIG_MORPH_POSITION.write() = val / 1000.0;
                        }
                    },
                }
                // Thumb
                div {
                    class: "absolute w-3.5 h-3.5 rounded-full bg-white border-2 border-zinc-400 \
                            shadow-md pointer-events-none transform -translate-x-1/2",
                    style: "left: {position_pct}%",
                }
            }
            span { class: "text-[10px] font-bold text-orange-400 select-none", "B" }
        }
    }
}
