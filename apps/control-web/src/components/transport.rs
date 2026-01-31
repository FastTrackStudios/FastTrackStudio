//! Transport control components for DAW playback control

use crate::ws::use_roam_websocket;
use dioxus::prelude::*;
use serde::Deserialize;

/// Transport state from the DAW
#[derive(Debug, Clone, Default, Deserialize)]
pub struct TransportState {
    pub is_playing: bool,
    pub is_recording: bool,
    pub position_seconds: f64,
    pub tempo_bpm: f64,
}

/// Format seconds as MM:SS.mmm
fn format_time(seconds: f64) -> String {
    let total_seconds = seconds.floor() as u64;
    let minutes = total_seconds / 60;
    let secs = total_seconds % 60;
    let millis = ((seconds - seconds.floor()) * 1000.0) as u64;
    format!("{:02}:{:02}.{:03}", minutes, secs, millis)
}

// SVG Icon components
#[component]
fn IconPlay(class: Option<String>) -> Element {
    let class = class.unwrap_or_default();
    rsx! {
        svg {
            class: "{class}",
            fill: "none",
            stroke: "currentColor",
            stroke_width: "2",
            stroke_linecap: "round",
            stroke_linejoin: "round",
            view_box: "0 0 24 24",
            polygon { points: "5,3 19,12 5,21" }
        }
    }
}

#[component]
fn IconPause(class: Option<String>) -> Element {
    let class = class.unwrap_or_default();
    rsx! {
        svg {
            class: "{class}",
            fill: "none",
            stroke: "currentColor",
            stroke_width: "2",
            stroke_linecap: "round",
            stroke_linejoin: "round",
            view_box: "0 0 24 24",
            rect { x: "6", y: "4", width: "4", height: "16" }
            rect { x: "14", y: "4", width: "4", height: "16" }
        }
    }
}

#[component]
fn IconStop(class: Option<String>) -> Element {
    let class = class.unwrap_or_default();
    rsx! {
        svg {
            class: "{class}",
            fill: "none",
            stroke: "currentColor",
            stroke_width: "2",
            stroke_linecap: "round",
            stroke_linejoin: "round",
            view_box: "0 0 24 24",
            rect { x: "3", y: "3", width: "18", height: "18", rx: "2", ry: "2" }
        }
    }
}

#[component]
fn IconRecord(class: Option<String>, filled: bool) -> Element {
    let class = class.unwrap_or_default();
    let fill = if filled { "currentColor" } else { "none" };
    rsx! {
        svg {
            class: "{class}",
            fill: "{fill}",
            stroke: "currentColor",
            stroke_width: "2",
            view_box: "0 0 24 24",
            circle { cx: "12", cy: "12", r: "10" }
        }
    }
}

#[component]
fn IconSkipBack(class: Option<String>) -> Element {
    let class = class.unwrap_or_default();
    rsx! {
        svg {
            class: "{class}",
            fill: "none",
            stroke: "currentColor",
            stroke_width: "2",
            stroke_linecap: "round",
            stroke_linejoin: "round",
            view_box: "0 0 24 24",
            polygon { points: "19,20 9,12 19,4" }
            line { x1: "5", y1: "19", x2: "5", y2: "5" }
        }
    }
}

#[component]
fn IconSkipForward(class: Option<String>) -> Element {
    let class = class.unwrap_or_default();
    rsx! {
        svg {
            class: "{class}",
            fill: "none",
            stroke: "currentColor",
            stroke_width: "2",
            stroke_linecap: "round",
            stroke_linejoin: "round",
            view_box: "0 0 24 24",
            polygon { points: "5,4 15,12 5,20" }
            line { x1: "19", y1: "5", x2: "19", y2: "19" }
        }
    }
}

/// Large transport control button
#[component]
fn TransportButton(
    onclick: EventHandler<()>,
    active: bool,
    danger: bool,
    children: Element,
) -> Element {
    let base = "w-16 h-16 rounded-2xl flex items-center justify-center transition-all duration-150 active:scale-95";
    let color = if danger {
        if active {
            "bg-red-500 text-white shadow-lg shadow-red-500/30"
        } else {
            "bg-muted hover:bg-red-500/20 text-red-500"
        }
    } else if active {
        "bg-primary text-primary-foreground shadow-lg shadow-primary/30"
    } else {
        "bg-muted hover:bg-muted/80 text-foreground"
    };

    rsx! {
        button {
            class: "{base} {color}",
            onclick: move |_| onclick.call(()),
            {children}
        }
    }
}

/// Small navigation button
#[component]
fn NavButton(onclick: EventHandler<()>, children: Element) -> Element {
    rsx! {
        button {
            class: "w-12 h-12 rounded-xl bg-muted/50 hover:bg-muted flex items-center justify-center transition-colors active:scale-95",
            onclick: move |_| onclick.call(()),
            {children}
        }
    }
}

/// Main transport controls component
#[component]
pub fn TransportControls() -> Element {
    let ws = use_roam_websocket();
    let mut transport_state = use_signal(TransportState::default);

    // TODO: Set up transport state subscription via subscribe_transport
    // For now, the UI shows local state that updates optimistically on button clicks

    // Read current state
    let is_playing = transport_state.read().is_playing;
    let is_recording = transport_state.read().is_recording;
    let position = transport_state.read().position_seconds;
    let tempo = transport_state.read().tempo_bpm;

    // Transport control actions - call Transport methods
    let on_stop = {
        let ws = ws.clone();
        move |_: ()| {
            ws.call_ignore("Transport", "stop", vec![]);
            transport_state.write().is_playing = false;
        }
    };

    let on_play_pause = {
        let ws = ws.clone();
        move |_: ()| {
            if is_playing {
                ws.call_ignore("Transport", "stop", vec![]);
                transport_state.write().is_playing = false;
            } else {
                ws.call_ignore("Transport", "play", vec![]);
                transport_state.write().is_playing = true;
            }
        }
    };

    let on_record = {
        let ws = ws.clone();
        move |_: ()| {
            ws.call_ignore("Transport", "record", vec![]);
            transport_state.write().is_recording = !is_recording;
        }
    };

    let on_goto_start = {
        let ws = ws.clone();
        move |_: ()| {
            ws.call_ignore("Transport", "set_position", vec![serde_json::json!(0.0)]);
            transport_state.write().position_seconds = 0.0;
        }
    };

    let on_goto_end = {
        let ws = ws.clone();
        move |_: ()| {
            // TODO: Get project length and seek to it
            // For now, just seek to 5 minutes as placeholder
            ws.call_ignore("Transport", "set_position", vec![serde_json::json!(300.0)]);
        }
    };

    rsx! {
        div { class: "flex flex-col items-center gap-6 p-6",
            // Time display
            div { class: "text-center",
                div { class: "text-4xl font-mono font-bold tracking-wider",
                    "{format_time(position)}"
                }
                if tempo > 0.0 {
                    div { class: "text-muted-foreground text-sm mt-1",
                        "{tempo:.1} BPM"
                    }
                }
            }

            // Main transport buttons
            div { class: "flex items-center gap-4",
                // Navigation - skip back
                NavButton { onclick: on_goto_start,
                    IconSkipBack { class: "w-5 h-5".to_string() }
                }

                // Stop
                TransportButton {
                    onclick: on_stop,
                    active: !is_playing && !is_recording,
                    danger: false,
                    IconStop { class: "w-6 h-6".to_string() }
                }

                // Play/Pause - single button that toggles
                TransportButton {
                    onclick: on_play_pause,
                    active: is_playing,
                    danger: false,
                    if is_playing {
                        IconPause { class: "w-8 h-8".to_string() }
                    } else {
                        IconPlay { class: "w-8 h-8 ml-1".to_string() }
                    }
                }

                // Record
                TransportButton {
                    onclick: on_record,
                    active: is_recording,
                    danger: true,
                    IconRecord { class: "w-6 h-6".to_string(), filled: is_recording }
                }

                // Navigation - skip forward
                NavButton { onclick: on_goto_end,
                    IconSkipForward { class: "w-5 h-5".to_string() }
                }
            }

            // Status indicators
            div { class: "flex gap-4 text-sm",
                if is_playing {
                    div { class: "flex items-center gap-2 text-green-500",
                        div { class: "w-2 h-2 rounded-full bg-green-500 animate-pulse" }
                        "Playing"
                    }
                }
                if is_recording {
                    div { class: "flex items-center gap-2 text-red-500",
                        div { class: "w-2 h-2 rounded-full bg-red-500 animate-pulse" }
                        "Recording"
                    }
                }
            }
        }
    }
}

/// Compact transport bar for embedding in other views
#[component]
pub fn TransportBar() -> Element {
    let ws = use_roam_websocket();
    let mut is_playing = use_signal(|| false);

    let playing = *is_playing.read();

    let on_toggle_play = {
        let ws = ws.clone();
        move |_: dioxus::events::MouseEvent| {
            if playing {
                ws.call_ignore("Transport", "stop", vec![]);
            } else {
                ws.call_ignore("Transport", "play", vec![]);
            }
            is_playing.set(!playing);
        }
    };

    let on_stop = {
        let ws = ws.clone();
        move |_: dioxus::events::MouseEvent| {
            ws.call_ignore("Transport", "stop", vec![]);
            is_playing.set(false);
        }
    };

    rsx! {
        div { class: "flex items-center gap-2 p-2 bg-muted/50 rounded-lg",
            button {
                class: if !playing {
                    "w-10 h-10 rounded-lg flex items-center justify-center transition-colors bg-muted text-muted-foreground"
                } else {
                    "w-10 h-10 rounded-lg flex items-center justify-center transition-colors bg-muted hover:bg-muted/80"
                },
                onclick: on_stop,
                IconStop { class: "w-4 h-4".to_string() }
            }
            button {
                class: if playing {
                    "w-10 h-10 rounded-lg flex items-center justify-center transition-colors bg-primary text-primary-foreground"
                } else {
                    "w-10 h-10 rounded-lg flex items-center justify-center transition-colors bg-muted hover:bg-primary/20"
                },
                onclick: on_toggle_play,
                if playing {
                    IconPause { class: "w-5 h-5".to_string() }
                } else {
                    IconPlay { class: "w-5 h-5 ml-0.5".to_string() }
                }
            }
        }
    }
}
