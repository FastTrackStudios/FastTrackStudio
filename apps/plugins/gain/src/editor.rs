//! Dioxus UI for FTS Gain plugin.
//!
//! Uses PluginShell for consistent window chrome and size controls.

use fts_gain_core::GainMeterState;
use fts_plugin_core::prelude::*;
use lumen_blocks::components::button::{Button as LumenButton, ButtonSize, ButtonVariant};
use nih_plug_dioxus::SharedState;
use std::sync::Arc;

use crate::GainParams;

/// Plugin version for display in the shell.
const PLUGIN_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Create the Dioxus editor for the gain plugin.
pub fn create(params: Arc<GainParams>, meters: GainMeterState) -> Option<Box<dyn Editor>> {
    let ui_state = Arc::new(EditorState { params, meters });
    create_dioxus_editor_with_state(ui_state.params.editor_state.clone(), ui_state, App)
}

/// Shared state for the editor.
struct EditorState {
    params: Arc<GainParams>,
    meters: GainMeterState,
}

/// Main app component - wrapped in PluginShell.
#[component]
fn App() -> Element {
    rsx! {
        PluginShell {
            plugin_name: "FTS Gain".to_string(),
            version: Some(PLUGIN_VERSION.to_string()),
            GainContent {}
        }
    }
}

/// The actual gain plugin UI content (inside the shell).
#[component]
fn GainContent() -> Element {
    // Get the SharedState wrapper and downcast to our EditorState
    let shared = use_context::<SharedState>();
    let state = shared
        .get::<EditorState>()
        .expect("EditorState not in context");

    // Create signals for reactive UI updates
    let mut input_db = use_signal(|| -60.0f32);
    let mut output_db = use_signal(|| -60.0f32);
    let mut gain_db = use_signal(|| 0.0f32);
    let mut phase_invert = use_signal(|| false);

    // Demo state
    let mut demo_progress = use_signal(|| 45.0f32);
    let mut click_count = use_signal(|| 0i32);
    let mut switch_on = use_signal(|| false);
    let mut checkbox_on = use_signal(|| false);

    // Poll for updates at ~30fps
    let state_for_poll = state.clone();
    use_future(move || {
        let state = state_for_poll.clone();
        async move {
            loop {
                // Read current values from atomics
                let (in_db, out_db) = state.meters.to_db();
                let params = state.params.to_values();

                // Update signals
                input_db.set(in_db);
                output_db.set(out_db);
                gain_db.set(params.gain_db);
                phase_invert.set(params.phase_invert);

                // Animate demo progress
                let current = *demo_progress.read();
                demo_progress.set(if current >= 100.0 { 0.0 } else { current + 0.5 });

                // Sleep for ~33ms (30fps)
                futures_timer::Delay::new(std::time::Duration::from_millis(33)).await;
            }
        }
    });

    // Read values
    let in_db = *input_db.read();
    let out_db = *output_db.read();
    let gain = *gain_db.read();
    let phase = *phase_invert.read();
    let progress = *demo_progress.read();
    let clicks = *click_count.read();

    // Normalize for display (0-100%)
    let input_pct = ((in_db + 60.0) / 66.0 * 100.0).clamp(0.0, 100.0);
    let output_pct = ((out_db + 60.0) / 66.0 * 100.0).clamp(0.0, 100.0);

    rsx! {
        div {
            class: "w-full h-full bg-zinc-900 flex p-4 gap-4",

            // Left - Meters
            div {
                class: "flex flex-col w-28",

                div { class: "text-xs text-zinc-500 font-medium mb-2", "LEVELS" }

                div {
                    class: "flex gap-2 flex-1",

                    MeterGroup {
                        label: "IN",
                        level_pct: input_pct,
                        level_db: in_db,
                    }

                    MeterGroup {
                        label: "OUT",
                        level_pct: output_pct,
                        level_db: out_db,
                    }
                }
            }

            // Center - Gain display
            div {
                class: "flex-1 flex flex-col gap-3",

                // Main gain card
                div {
                    class: "flex-1 bg-zinc-800 rounded-xl flex flex-col justify-center items-center",

                    div { class: "text-xs text-zinc-500 font-medium", "GAIN" }

                    div {
                        class: "flex items-baseline gap-2 mt-2",
                        span {
                            class: "text-5xl text-white font-light",
                            "{gain:+.1}"
                        }
                        span { class: "text-xl text-zinc-500", "dB" }
                    }

                    // Gain bar
                    div {
                        class: "w-48 h-2 bg-zinc-700 rounded-full mt-4 overflow-hidden",
                        div {
                            class: if gain > 0.0 { "h-full bg-orange-500" } else { "h-full bg-blue-500" },
                            style: format!("width: {}%;", ((gain + 60.0) / 84.0 * 100.0).clamp(0.0, 100.0)),
                        }
                    }

                    div {
                        class: "flex justify-between w-48 mt-1 text-xs text-zinc-600",
                        span { "-60" }
                        span { "0" }
                        span { "+24" }
                    }
                }

                // Phase invert row
                div {
                    class: "bg-zinc-800 rounded-lg p-3 flex items-center justify-between",

                    div {
                        class: "flex items-center gap-2",
                        span { class: "text-zinc-400 text-sm", "Phase Invert" }
                        span {
                            class: if phase { "text-blue-400 font-bold" } else { "text-zinc-600 font-bold" },
                            "Ø"
                        }
                    }

                    SimpleSwitch { on: phase }
                }
            }

            // Right - Component demos
            div {
                class: "w-40 flex flex-col gap-3",

                div { class: "text-xs text-zinc-500 font-medium", "DEMO" }

                // Buttons
                div {
                    class: "bg-zinc-800 rounded-lg p-3",

                    div { class: "text-xs text-zinc-500 mb-2", "Simple Buttons" }

                    div {
                        class: "flex flex-col gap-2",

                        SimpleButton {
                            label: "Primary",
                            variant: "primary",
                            on_click: move |_| {
                                let c = *click_count.read();
                                click_count.set(c + 1);
                            },
                        }

                        SimpleButton {
                            label: "Secondary",
                            variant: "secondary",
                            on_click: |_| {},
                        }
                    }

                    div { class: "text-xs text-zinc-500 mt-2", "Clicks: {clicks}" }
                }

                // Lumen Blocks Buttons
                div {
                    class: "bg-zinc-800 rounded-lg p-3",

                    div { class: "text-xs text-zinc-500 mb-2", "Lumen Buttons" }

                    div {
                        class: "flex flex-col gap-2",

                        LumenButton {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            full_width: true,
                            on_click: move |_| {
                                let c = *click_count.read();
                                click_count.set(c + 1);
                            },
                            "Primary"
                        }

                        LumenButton {
                            variant: ButtonVariant::Secondary,
                            size: ButtonSize::Small,
                            full_width: true,
                            "Secondary"
                        }

                        LumenButton {
                            variant: ButtonVariant::Outline,
                            size: ButtonSize::Small,
                            full_width: true,
                            "Outline"
                        }

                        LumenButton {
                            variant: ButtonVariant::Ghost,
                            size: ButtonSize::Small,
                            full_width: true,
                            "Ghost"
                        }

                        LumenButton {
                            variant: ButtonVariant::Destructive,
                            size: ButtonSize::Small,
                            full_width: true,
                            "Destructive"
                        }
                    }
                }

                // Progress bars
                div {
                    class: "bg-zinc-800 rounded-lg p-3",

                    div { class: "text-xs text-zinc-500 mb-2", "Progress" }

                    div {
                        class: "flex flex-col gap-2",

                        SimpleProgress { value: progress, color: "blue" }
                        SimpleProgress { value: progress, color: "green" }
                        SimpleProgress { value: progress, color: "orange" }
                    }
                }

                // Toggles
                div {
                    class: "bg-zinc-800 rounded-lg p-3",

                    div { class: "text-xs text-zinc-500 mb-2", "Toggles" }

                    div {
                        class: "flex flex-col gap-3",

                        div {
                            class: "flex items-center justify-between",
                            span { class: "text-zinc-400 text-sm", "Switch" }
                            SimpleSwitch {
                                on: *switch_on.read(),
                                on_click: move |_| {
                                    let v = *switch_on.read();
                                    switch_on.set(!v);
                                },
                            }
                        }

                        div {
                            class: "flex items-center gap-2",
                            SimpleCheckbox {
                                checked: *checkbox_on.read(),
                                on_click: move |_| {
                                    let v = *checkbox_on.read();
                                    checkbox_on.set(!v);
                                },
                            }
                            span { class: "text-zinc-400 text-sm", "Checkbox" }
                        }
                    }
                }
            }
        }
    }
}

// ============================================================================
// Simple UI Components
// ============================================================================

/// Meter group with label
#[component]
fn MeterGroup(label: &'static str, level_pct: f32, level_db: f32) -> Element {
    rsx! {
        div {
            class: "flex flex-col items-center flex-1",

            div { class: "text-xs text-zinc-500 font-medium mb-1", "{label}" }

            div {
                class: "flex gap-1 flex-1 w-full",
                MeterBar { level_pct: level_pct }
                MeterBar { level_pct: level_pct }
            }

            div {
                class: "text-xs text-zinc-500 font-mono mt-1",
                "{level_db:.0}"
            }
        }
    }
}

/// Single meter bar
#[component]
fn MeterBar(level_pct: f32) -> Element {
    let green = level_pct.min(70.0);
    let yellow = (level_pct - 70.0).max(0.0).min(20.0);
    let red = (level_pct - 90.0).max(0.0);

    rsx! {
        div {
            class: "flex-1 bg-zinc-700 rounded-sm overflow-hidden flex flex-col-reverse min-h-32",

            // Green segment
            div {
                class: "w-full bg-green-500",
                style: format!("height: {}%;", green),
            }

            // Yellow segment
            if yellow > 0.0 {
                div {
                    class: "w-full bg-yellow-500",
                    style: format!("height: {}%;", yellow),
                }
            }

            // Red segment
            if red > 0.0 {
                div {
                    class: "w-full bg-red-500",
                    style: format!("height: {}%;", red),
                }
            }
        }
    }
}

/// Simple switch toggle
#[component]
fn SimpleSwitch(
    on: bool,
    #[props(default = None)] on_click: Option<EventHandler<MouseEvent>>,
) -> Element {
    let track_class = if on {
        "w-10 h-5 bg-blue-600 rounded-full relative cursor-pointer"
    } else {
        "w-10 h-5 bg-zinc-600 rounded-full relative cursor-pointer"
    };

    let knob_style = if on { "right: 2px;" } else { "left: 2px;" };

    rsx! {
        div {
            class: "{track_class}",
            onclick: move |e| {
                if let Some(handler) = &on_click {
                    handler.call(e);
                }
            },

            div {
                class: "absolute top-0.5 w-4 h-4 bg-white rounded-full",
                style: "{knob_style}",
            }
        }
    }
}

/// Simple checkbox
#[component]
fn SimpleCheckbox(
    checked: bool,
    #[props(default = None)] on_click: Option<EventHandler<MouseEvent>>,
) -> Element {
    let box_class = if checked {
        "w-5 h-5 bg-blue-600 rounded flex items-center justify-center cursor-pointer"
    } else {
        "w-5 h-5 bg-zinc-700 border border-zinc-600 rounded flex items-center justify-center cursor-pointer"
    };

    rsx! {
        div {
            class: "{box_class}",
            onclick: move |e| {
                if let Some(handler) = &on_click {
                    handler.call(e);
                }
            },

            if checked {
                span { class: "text-white text-xs font-bold", "✓" }
            }
        }
    }
}

/// Simple button
#[component]
fn SimpleButton(
    label: &'static str,
    #[props(default = "primary")] variant: &'static str,
    on_click: EventHandler<MouseEvent>,
) -> Element {
    let class = match variant {
        "primary" => {
            "w-full px-3 py-1.5 bg-blue-600 hover:bg-blue-700 text-white text-sm font-medium rounded cursor-pointer"
        }
        "secondary" => {
            "w-full px-3 py-1.5 bg-zinc-700 hover:bg-zinc-600 text-white text-sm font-medium rounded cursor-pointer"
        }
        "danger" => {
            "w-full px-3 py-1.5 bg-red-600 hover:bg-red-700 text-white text-sm font-medium rounded cursor-pointer"
        }
        _ => "w-full px-3 py-1.5 bg-zinc-700 text-white text-sm font-medium rounded cursor-pointer",
    };

    rsx! {
        button {
            class: "{class}",
            onclick: move |e| on_click.call(e),
            "{label}"
        }
    }
}

/// Simple progress bar
#[component]
fn SimpleProgress(value: f32, #[props(default = "blue")] color: &'static str) -> Element {
    let bar_class = match color {
        "blue" => "h-full bg-blue-500 rounded-full",
        "green" => "h-full bg-green-500 rounded-full",
        "orange" => "h-full bg-orange-500 rounded-full",
        "red" => "h-full bg-red-500 rounded-full",
        _ => "h-full bg-blue-500 rounded-full",
    };

    rsx! {
        div {
            class: "w-full h-2 bg-zinc-700 rounded-full overflow-hidden",

            div {
                class: "{bar_class}",
                style: format!("width: {}%;", value.clamp(0.0, 100.0)),
            }
        }
    }
}
