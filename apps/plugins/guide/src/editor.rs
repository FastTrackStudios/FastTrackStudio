//! Dioxus-based editor for FTS Guide plugin

use atomic_float::AtomicF32;
use fts_plugin_core::prelude::*;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};

use crate::GuideParams;

/// Shared UI state for the plugin editor
#[derive(Clone)]
pub struct GuideUiState {
    pub params: Arc<GuideParams>,
    pub transport_tempo: Arc<AtomicF32>,
    pub transport_time_sig_numerator: Arc<AtomicI32>,
    pub transport_time_sig_denominator: Arc<AtomicI32>,
    pub transport_bar_number: Arc<AtomicI32>,
    pub transport_beat_position: Arc<AtomicF32>,
    pub transport_playing: Arc<AtomicBool>,
}

/// Create the plugin editor
pub fn create(
    params: Arc<GuideParams>,
    transport_tempo: Arc<AtomicF32>,
    transport_time_sig_numerator: Arc<AtomicI32>,
    transport_time_sig_denominator: Arc<AtomicI32>,
    transport_bar_number: Arc<AtomicI32>,
    transport_beat_position: Arc<AtomicF32>,
    transport_playing: Arc<AtomicBool>,
) -> Option<Box<dyn Editor>> {
    let ui_state = Arc::new(GuideUiState {
        params,
        transport_tempo,
        transport_time_sig_numerator,
        transport_time_sig_denominator,
        transport_bar_number,
        transport_beat_position,
        transport_playing,
    });

    create_dioxus_editor_with_state(ui_state.params.editor_state.clone(), ui_state, App)
}

/// Main app component
fn App() -> Element {
    let shared = use_context::<SharedState>();
    let ui_state = shared
        .get::<GuideUiState>()
        .expect("GuideUiState not in context");

    // Read transport values
    let tempo = ui_state.transport_tempo.load(Ordering::Relaxed);
    let time_sig_num = ui_state
        .transport_time_sig_numerator
        .load(Ordering::Relaxed);
    let time_sig_den = ui_state
        .transport_time_sig_denominator
        .load(Ordering::Relaxed);
    let _bar_number = ui_state.transport_bar_number.load(Ordering::Relaxed);
    let beat_position = ui_state.transport_beat_position.load(Ordering::Relaxed);
    let is_playing = ui_state.transport_playing.load(Ordering::Relaxed);

    // Read params
    let params = &ui_state.params;

    rsx! {
        div {
            class: "guide-editor",
            style: "
                display: flex;
                flex-direction: column;
                padding: 20px;
                background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);
                color: white;
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                height: 100%;
                box-sizing: border-box;
            ",

            // Header
            div {
                class: "header",
                style: "
                    display: flex;
                    justify-content: space-between;
                    align-items: center;
                    margin-bottom: 20px;
                    padding-bottom: 10px;
                    border-bottom: 1px solid rgba(255, 255, 255, 0.1);
                ",
                h1 {
                    style: "margin: 0; font-size: 24px; font-weight: 600;",
                    "FTS Guide"
                }
                div {
                    class: "transport-info",
                    style: "
                        display: flex;
                        gap: 15px;
                        font-size: 14px;
                        opacity: 0.8;
                    ",
                    span { "{tempo:.1} BPM" }
                    span { "{time_sig_num}/{time_sig_den}" }
                    span {
                        style: if is_playing { "color: #4ade80;" } else { "color: #ef4444;" },
                        if is_playing { "▶ Playing" } else { "⏸ Stopped" }
                    }
                }
            }

            // Controls section
            div {
                class: "controls",
                style: "
                    display: grid;
                    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
                    gap: 20px;
                ",

                // Click options
                ControlSection {
                    title: "Click",
                    children: rsx! {
                        ToggleControl {
                            label: "Beat",
                            enabled: params.enable_beat.value(),
                        }
                        ToggleControl {
                            label: "Eighth Notes",
                            enabled: params.enable_eighth.value(),
                        }
                        ToggleControl {
                            label: "Sixteenth Notes",
                            enabled: params.enable_sixteenth.value(),
                        }
                        ToggleControl {
                            label: "Triplets",
                            enabled: params.enable_triplet.value(),
                        }
                        ToggleControl {
                            label: "Measure Accent",
                            enabled: params.enable_measure_accent.value(),
                        }
                    }
                }

                // Count options
                ControlSection {
                    title: "Count-In",
                    children: rsx! {
                        ToggleControl {
                            label: "Enable Count",
                            enabled: params.enable_count.value(),
                        }
                        ToggleControl {
                            label: "Offset by One",
                            enabled: params.offset_count_by_one.value(),
                        }
                        ToggleControl {
                            label: "Extend SONGEND",
                            enabled: params.extend_songend_count.value(),
                        }
                        ToggleControl {
                            label: "Full Count (Odd Time)",
                            enabled: params.full_count_odd_time.value(),
                        }
                    }
                }

                // Guide options
                ControlSection {
                    title: "Guide",
                    children: rsx! {
                        ToggleControl {
                            label: "Enable Guide",
                            enabled: params.enable_guide.value(),
                        }
                        ToggleControl {
                            label: "Replace Beat 1",
                            enabled: params.guide_replace_beat1.value(),
                        }
                    }
                }
            }

            // Status bar
            div {
                class: "status-bar",
                style: "
                    margin-top: auto;
                    padding-top: 15px;
                    border-top: 1px solid rgba(255, 255, 255, 0.1);
                    font-size: 12px;
                    opacity: 0.6;
                ",
                "Beat: {beat_position:.2}"
            }
        }
    }
}

/// Control section wrapper
#[component]
fn ControlSection(title: &'static str, children: Element) -> Element {
    rsx! {
        div {
            class: "control-section",
            style: "
                background: rgba(255, 255, 255, 0.05);
                border-radius: 8px;
                padding: 15px;
            ",
            h3 {
                style: "
                    margin: 0 0 12px 0;
                    font-size: 14px;
                    font-weight: 500;
                    text-transform: uppercase;
                    letter-spacing: 0.5px;
                    opacity: 0.7;
                ",
                "{title}"
            }
            div {
                style: "display: flex; flex-direction: column; gap: 8px;",
                {children}
            }
        }
    }
}

/// Toggle control display
#[component]
fn ToggleControl(label: &'static str, enabled: bool) -> Element {
    rsx! {
        div {
            class: "toggle-control",
            style: "
                display: flex;
                align-items: center;
                gap: 10px;
            ",
            div {
                class: "indicator",
                style: format!(
                    "
                    width: 10px;
                    height: 10px;
                    border-radius: 50%;
                    background: {};
                    ",
                    if enabled { "#4ade80" } else { "#374151" }
                ),
            }
            span {
                style: format!(
                    "font-size: 13px; {}",
                    if enabled { "" } else { "opacity: 0.5;" }
                ),
                "{label}"
            }
        }
    }
}
