//! Automation Lane UI -- horizontal timeline with event markers and transport controls.
//!
//! Provides a visual automation lane for the snapshot timeline:
//!
//! ```text
//! +---------------------------------------------------------------------+
//! |  [Rec] [Play] [Stop] [Rewind]  |  Automation: Song Intro   [Write] |
//! +---------------------------------------------------------------------+
//! |  0s    1s    2s    3s    4s    5s    6s    7s    8s    9s   10s     |
//! |  |--*--|-----|--O========>--|-----|--*--|-----|-----|-----|-----|    |
//! |     ^           ^morph           ^snap                              |
//! |  playhead                                                           |
//! +---------------------------------------------------------------------+
//! ```
//!
//! Event markers:
//! - Blue dot: `ApplySnapshot`
//! - Orange bar: `StartMorph` (width = duration)
//! - Purple dot: `SetMorphPosition`

use crate::prelude::*;
use crate::signals::{AUTOMATION_LANE, AUTOMATION_PLAYING, AUTOMATION_RECORDING};
use signal_control::automation::{AutomationAction, SnapshotAutomation};
use uuid::Uuid;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Default visible duration of the timeline in milliseconds.
const DEFAULT_VISIBLE_DURATION_MS: u64 = 30_000;

/// Minimum timeline duration (always show at least this much).
const MIN_TIMELINE_MS: u64 = 10_000;

// ---------------------------------------------------------------------------
// AutomationToolbar
// ---------------------------------------------------------------------------

/// Transport and action toolbar for the automation lane.
///
/// Provides record, play, stop, rewind, and "Write to Timeline" controls.
#[component]
pub fn AutomationToolbar() -> Element {
    let lane = AUTOMATION_LANE.read();
    let is_playing = *AUTOMATION_PLAYING.read();
    let is_recording = *AUTOMATION_RECORDING.read();
    let has_lane = lane.is_some();
    let lane_name = lane
        .as_ref()
        .map(|l| l.name.clone())
        .unwrap_or_else(|| "No Lane".to_string());
    let event_count = lane.as_ref().map(|l| l.events.len()).unwrap_or(0);

    // Style variables extracted before rsx! block
    let rec_bg = if is_recording {
        "bg-red-600 text-white border-red-500"
    } else {
        "bg-zinc-800 text-zinc-400 border-zinc-700 hover:bg-zinc-700 hover:text-zinc-300"
    };
    let play_bg = if is_playing {
        "bg-green-600 text-white border-green-500"
    } else {
        "bg-zinc-800 text-zinc-400 border-zinc-700 hover:bg-zinc-700 hover:text-zinc-300"
    };
    let stop_bg = "bg-zinc-800 text-zinc-400 border-zinc-700 hover:bg-zinc-700 hover:text-zinc-300";
    let rewind_bg =
        "bg-zinc-800 text-zinc-400 border-zinc-700 hover:bg-zinc-700 hover:text-zinc-300";
    let write_bg = "bg-blue-900/40 text-blue-300 border-blue-800/50 hover:bg-blue-800/50";

    rsx! {
        div { class: "flex items-center gap-2 px-3 py-2 border-b border-zinc-700/50 bg-zinc-900/80",
            // Transport buttons
            div { class: "flex items-center gap-1",
                // Record
                button {
                    class: "flex items-center justify-center w-7 h-7 rounded border text-xs font-bold transition-colors {rec_bg}",
                    title: "Toggle recording",
                    onclick: move |_| {
                        if !has_lane {
                            // Create a new lane on first record press
                            *AUTOMATION_LANE.write() = Some(SnapshotAutomation::new("Untitled"));
                        }
                        let new_val = !is_recording;
                        *AUTOMATION_RECORDING.write() = new_val;
                        let mut guard = AUTOMATION_LANE.write();
                        if let Some(ref mut lane) = *guard {
                            lane.is_recording = new_val;
                        }
                    },
                    // Record circle icon
                    svg {
                        class: "w-3.5 h-3.5",
                        fill: "currentColor",
                        view_box: "0 0 24 24",
                        circle { cx: "12", cy: "12", r: "8" }
                    }
                }

                // Play
                button {
                    class: "flex items-center justify-center w-7 h-7 rounded border text-xs font-bold transition-colors {play_bg}",
                    title: "Play automation",
                    disabled: !has_lane || event_count == 0,
                    onclick: move |_| {
                        *AUTOMATION_PLAYING.write() = !is_playing;
                        *AUTOMATION_RECORDING.write() = false;
                    },
                    // Play triangle icon
                    svg {
                        class: "w-3.5 h-3.5",
                        fill: "currentColor",
                        view_box: "0 0 24 24",
                        polygon { points: "8,5 19,12 8,19" }
                    }
                }

                // Stop
                button {
                    class: "flex items-center justify-center w-7 h-7 rounded border text-xs font-bold transition-colors {stop_bg}",
                    title: "Stop",
                    disabled: !is_playing && !is_recording,
                    onclick: move |_| {
                        *AUTOMATION_PLAYING.write() = false;
                        *AUTOMATION_RECORDING.write() = false;
                        let mut guard = AUTOMATION_LANE.write();
                        if let Some(ref mut lane) = *guard {
                            lane.is_recording = false;
                        }
                    },
                    // Stop square icon
                    svg {
                        class: "w-3.5 h-3.5",
                        fill: "currentColor",
                        view_box: "0 0 24 24",
                        rect { x: "6", y: "6", width: "12", height: "12" }
                    }
                }

                // Rewind
                button {
                    class: "flex items-center justify-center w-7 h-7 rounded border text-xs font-bold transition-colors {rewind_bg}",
                    title: "Rewind to start",
                    disabled: !has_lane,
                    onclick: move |_| {
                        let mut guard = AUTOMATION_LANE.write();
                        if let Some(ref mut lane) = *guard {
                            lane.rewind();
                        }
                    },
                    // Rewind icon (skip-back)
                    svg {
                        class: "w-3.5 h-3.5",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "2",
                        view_box: "0 0 24 24",
                        polygon { points: "11,19 2,12 11,5" }
                        line { x1: "22", y1: "5", x2: "22", y2: "19" }
                        polygon { points: "22,19 13,12 22,5" }
                    }
                }
            }

            // Separator
            div { class: "w-px h-5 bg-zinc-700" }

            // Lane info
            div { class: "flex-1 flex items-center gap-2 min-w-0",
                span { class: "text-xs font-medium text-zinc-300 truncate",
                    "{lane_name}"
                }
                if event_count > 0 {
                    span { class: "text-[10px] text-zinc-500",
                        "({event_count} events)"
                    }
                }
            }

            // Write to Timeline button
            button {
                class: "flex items-center gap-1.5 px-2.5 py-1 rounded-md text-xs font-medium \
                        border transition-colors {write_bg}",
                title: "Write current snapshot actions to timeline",
                disabled: !has_lane,
                onclick: move |_| {
                    // "Write to Timeline" creates a new lane if none exists,
                    // or confirms saving the current lane's events.
                    if AUTOMATION_LANE.read().is_none() {
                        *AUTOMATION_LANE.write() = Some(SnapshotAutomation::new("Untitled"));
                    }
                    // In a full implementation this would persist to the DB.
                    // For now we just ensure the lane exists and is ready.
                },
                // Pencil/write icon
                svg {
                    class: "w-3 h-3",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    view_box: "0 0 24 24",
                    path {
                        stroke_linecap: "round",
                        stroke_linejoin: "round",
                        d: "M11 4H4a2 2 0 00-2 2v14a2 2 0 002 2h14a2 2 0 002-2v-7",
                    }
                    path {
                        stroke_linecap: "round",
                        stroke_linejoin: "round",
                        d: "M18.5 2.5a2.121 2.121 0 013 3L12 15l-4 1 1-4 9.5-9.5z",
                    }
                }
                "Write to Timeline"
            }

            // Clear button
            if has_lane && event_count > 0 {
                button {
                    class: "flex items-center gap-1 px-2 py-1 rounded-md text-[11px] font-medium \
                            text-zinc-500 border border-zinc-700 hover:text-red-400 \
                            hover:border-red-800/50 transition-colors",
                    title: "Clear all events",
                    onclick: move |_| {
                        let mut guard = AUTOMATION_LANE.write();
                        if let Some(ref mut lane) = *guard {
                            lane.clear();
                        }
                    },
                    "Clear"
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// AutomationEventMarker
// ---------------------------------------------------------------------------

/// Props for a single event marker on the timeline.
#[derive(Props, Clone, PartialEq)]
struct AutomationEventMarkerProps {
    /// Event UUID.
    event_id: Uuid,
    /// Label text.
    label: String,
    /// CSS color for the marker.
    color: String,
    /// Left position as a percentage of the timeline width.
    left_pct: f64,
    /// Width as a percentage (non-zero only for morphs).
    width_pct: f64,
    /// Whether this is a morph (bar) vs. a point event (dot).
    is_morph: bool,
    /// Callback to remove this event.
    on_remove: Callback<Uuid>,
}

/// Individual event marker rendered on the timeline.
#[component]
fn AutomationEventMarker(props: AutomationEventMarkerProps) -> Element {
    let color = props.color.clone();
    let label = props.label.clone();
    let left_style = format!("left: {:.2}%", props.left_pct);

    if props.is_morph {
        // Morph events render as a horizontal bar spanning from start to end
        let width_style = format!("width: {:.2}%", props.width_pct);
        let bar_bg = format!("background: linear-gradient(90deg, {color}66, {color}33)");
        let border_color = format!("border-color: {color}88");
        rsx! {
            div {
                class: "absolute top-1 bottom-1 rounded-sm border cursor-pointer group",
                style: "{left_style}; {width_style}; {bar_bg}; {border_color}",
                title: "{label}",
                // Label inside the bar
                span {
                    class: "absolute inset-0 flex items-center justify-center text-[9px] \
                            font-medium text-white/80 truncate px-1 select-none",
                    "{label}"
                }
                // Remove button on hover
                button {
                    class: "absolute -top-1.5 -right-1.5 w-3.5 h-3.5 rounded-full bg-zinc-800 \
                            border border-zinc-600 text-zinc-400 text-[8px] leading-none \
                            flex items-center justify-center opacity-0 group-hover:opacity-100 \
                            hover:bg-red-900 hover:text-red-300 hover:border-red-700 transition-all",
                    onclick: {
                        let event_id = props.event_id;
                        let on_remove = props.on_remove.clone();
                        move |evt: Event<MouseData>| {
                            evt.stop_propagation();
                            on_remove.call(event_id);
                        }
                    },
                    "x"
                }
            }
        }
    } else {
        // Point events render as a small colored dot with a vertical line
        let dot_bg = format!("background-color: {color}");
        let line_bg = format!("background-color: {color}44");
        rsx! {
            div {
                class: "absolute top-0 bottom-0 flex flex-col items-center cursor-pointer group",
                style: "{left_style}; transform: translateX(-50%)",
                title: "{label}",
                // Vertical line
                div {
                    class: "w-px flex-1",
                    style: "{line_bg}",
                }
                // Dot
                div {
                    class: "w-2.5 h-2.5 rounded-full border border-zinc-600 shadow-sm \
                            flex-shrink-0 my-0.5",
                    style: "{dot_bg}",
                }
                // Vertical line (bottom half)
                div {
                    class: "w-px flex-1",
                    style: "{line_bg}",
                }
                // Label below
                span {
                    class: "absolute -bottom-3.5 text-[8px] text-zinc-500 whitespace-nowrap select-none",
                    "{label}"
                }
                // Remove button on hover
                button {
                    class: "absolute -top-1.5 -right-1 w-3.5 h-3.5 rounded-full bg-zinc-800 \
                            border border-zinc-600 text-zinc-400 text-[8px] leading-none \
                            flex items-center justify-center opacity-0 group-hover:opacity-100 \
                            hover:bg-red-900 hover:text-red-300 hover:border-red-700 transition-all",
                    onclick: {
                        let event_id = props.event_id;
                        let on_remove = props.on_remove.clone();
                        move |evt: Event<MouseData>| {
                            evt.stop_propagation();
                            on_remove.call(event_id);
                        }
                    },
                    "x"
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// AutomationTimeline
// ---------------------------------------------------------------------------

/// Horizontal timeline view showing events as markers with time ruler.
#[component]
pub fn AutomationTimeline() -> Element {
    let lane_signal = AUTOMATION_LANE.read();
    let is_playing = *AUTOMATION_PLAYING.read();

    let Some(ref lane) = *lane_signal else {
        return rsx! {
            div { class: "flex items-center justify-center h-full text-zinc-600 text-xs italic select-none",
                "No automation lane -- press Record or Write to Timeline to begin."
            }
        };
    };
    let lane: &SnapshotAutomation = lane;

    // Compute the visible timeline range
    let content_duration = lane.duration_ms().max(MIN_TIMELINE_MS);
    let total_ms = content_duration.max(DEFAULT_VISIBLE_DURATION_MS);
    let playback_pos = lane.playback_position_ms;

    // Playhead position as percentage
    let playhead_pct = if total_ms > 0 {
        (playback_pos as f64 / total_ms as f64) * 100.0
    } else {
        0.0
    };

    // Generate time ruler marks every second
    let tick_interval_ms: u64 = 1000;
    let tick_count = (total_ms / tick_interval_ms) + 1;

    // Callback to remove events
    let on_remove = Callback::new(move |event_id: Uuid| {
        let mut guard = AUTOMATION_LANE.write();
        if let Some(ref mut lane) = *guard {
            lane.remove_event(event_id);
        }
    });

    // Pre-collect event data to avoid borrow issues in rsx
    let event_markers: Vec<(Uuid, String, String, f64, f64, bool)> = lane
        .events
        .iter()
        .map(|evt| {
            let left_pct = (evt.timestamp_ms as f64 / total_ms as f64) * 100.0;
            let (width_pct, is_morph) =
                if let AutomationAction::StartMorph { duration_ms, .. } = &evt.action {
                    ((*duration_ms as f64 / total_ms as f64) * 100.0, true)
                } else {
                    (0.0, false)
                };
            (
                evt.id,
                evt.label(),
                evt.color_hint().to_string(),
                left_pct,
                width_pct,
                is_morph,
            )
        })
        .collect();

    // Pre-collect tick data
    let ticks: Vec<_> = (0..tick_count)
        .map(|i| {
            let ms = i * tick_interval_ms;
            let pct = (ms as f64 / total_ms as f64) * 100.0;
            let secs = ms / 1000;
            (pct, format!("{secs}s"))
        })
        .collect();

    let playhead_color = if is_playing { "#22c55e" } else { "#71717a" };
    let playhead_style = format!("left: {playhead_pct:.2}%; background-color: {playhead_color}");

    rsx! {
        div { class: "flex flex-col h-full",
            // Time ruler
            div { class: "relative h-5 bg-zinc-900/60 border-b border-zinc-800 flex-shrink-0 overflow-hidden",
                for (pct, label) in ticks.iter() {
                    div {
                        key: "{label}",
                        class: "absolute top-0 bottom-0 flex flex-col items-center",
                        style: "left: {pct:.2}%",
                        // Tick mark
                        div { class: "w-px h-2 bg-zinc-700 flex-shrink-0" }
                        // Label
                        span {
                            class: "text-[8px] text-zinc-600 select-none mt-0.5",
                            "{label}"
                        }
                    }
                }
            }

            // Event lane area
            div { class: "relative flex-1 bg-zinc-950/40 overflow-hidden min-h-[32px]",
                // Background grid lines (every second)
                for (pct, _label) in ticks.iter() {
                    div {
                        key: "grid-{_label}",
                        class: "absolute top-0 bottom-0 w-px bg-zinc-800/40",
                        style: "left: {pct:.2}%",
                    }
                }

                // Event markers
                for (id, label, color, left_pct, width_pct, is_morph) in event_markers.iter() {
                    AutomationEventMarker {
                        key: "{id}",
                        event_id: *id,
                        label: label.clone(),
                        color: color.clone(),
                        left_pct: *left_pct,
                        width_pct: *width_pct,
                        is_morph: *is_morph,
                        on_remove: on_remove.clone(),
                    }
                }

                // Playhead
                div {
                    class: "absolute top-0 bottom-0 w-0.5 z-10 pointer-events-none",
                    style: "{playhead_style}",
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// AutomationLane (combined component)
// ---------------------------------------------------------------------------

/// Full automation lane panel combining toolbar and timeline.
///
/// Drop this into a dock panel or the transport section to get a complete
/// snapshot automation timeline with transport controls.
#[component]
pub fn AutomationLane() -> Element {
    rsx! {
        div { class: "flex flex-col h-full w-full bg-zinc-900 border border-zinc-800 rounded-lg overflow-hidden",
            AutomationToolbar {}
            div { class: "flex-1 min-h-0",
                AutomationTimeline {}
            }
        }
    }
}

/// Standalone dock panel wrapper for the automation lane.
///
/// Initializes the rig service and renders the automation lane with
/// toolbar and timeline.
#[component]
pub fn AutomationLanePanel() -> Element {
    crate::signals::init_rig_service();

    rsx! {
        div { class: "h-full w-full",
            AutomationLane {}
        }
    }
}
