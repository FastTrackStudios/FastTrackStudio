//! Focus mode dialog for nodes.
//!
//! Shows an expanded view of a node with all advanced parameters.

use dioxus::prelude::*;
use uuid::Uuid;

use super::node_graph::Node;

// Import audio-controls widgets
use audio_controls::widgets::{CompressorGraph, CompressorMode, CompressorParams, DbRange, EqBand, EqBandShape, EqGraph};

/// Props for the node focus dialog.
#[derive(Props, Clone, PartialEq)]
pub struct NodeFocusDialogProps {
    /// The node to display in focus mode.
    pub node: Option<Node>,
    /// Callback to close the dialog.
    pub on_close: Callback<()>,
}

/// Full-screen focus dialog for a node with all parameters.
#[component]
pub fn NodeFocusDialog(props: NodeFocusDialogProps) -> Element {
    let Some(node) = props.node else {
        return rsx! {};
    };

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center bg-black/80 backdrop-blur-sm",
            onclick: move |_| props.on_close.call(()),

            // Dialog container
            div {
                class: "bg-zinc-900 rounded-xl border border-zinc-700 shadow-2xl w-[90vw] h-[85vh] \
                        flex flex-col overflow-hidden animate-in fade-in zoom-in duration-200",
                onclick: |e| e.stop_propagation(),

                // Header
                div { class: "flex items-center justify-between px-6 py-4 border-b border-zinc-800 bg-zinc-850",
                    div {
                        h2 { class: "text-xl font-semibold text-zinc-200", "{node.name}" }
                        p { class: "text-sm text-zinc-500 mt-0.5", "Advanced Parameters" }
                    }
                    button {
                        class: "p-2 rounded-lg hover:bg-zinc-800 text-zinc-400 hover:text-zinc-200 transition-colors",
                        onclick: move |_| props.on_close.call(()),
                        // Close icon
                        svg {
                            class: "w-6 h-6",
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

                // Content area
                div { class: "flex-1 overflow-y-auto p-6",
                    NodeFocusContent { node: node.clone() }
                }

                // Footer
                div { class: "px-6 py-4 border-t border-zinc-800 bg-zinc-850 flex items-center justify-between",
                    div { class: "flex items-center gap-2",
                        // Bypass toggle
                        button {
                            class: if node.bypassed {
                                "px-4 py-2 rounded-lg bg-red-600 hover:bg-red-500 text-white text-sm font-medium transition-colors"
                            } else {
                                "px-4 py-2 rounded-lg bg-green-600 hover:bg-green-500 text-white text-sm font-medium transition-colors"
                            },
                            if node.bypassed { "Bypassed" } else { "Active" }
                        }
                    }
                    button {
                        class: "px-6 py-2 rounded-lg bg-zinc-700 hover:bg-zinc-600 text-white text-sm font-medium transition-colors",
                        onclick: move |_| props.on_close.call(()),
                        "Close"
                    }
                }
            }
        }
    }
}

/// Props for node focus content.
#[derive(Props, Clone, PartialEq)]
struct NodeFocusContentProps {
    node: Node,
}

/// Content area showing all parameters for the node.
#[component]
fn NodeFocusContent(props: NodeFocusContentProps) -> Element {
    let node = &props.node;

    rsx! {
        div { class: "space-y-6",
            // Widget visualization (large)
            div { class: "bg-zinc-800/50 rounded-lg p-6 border border-zinc-700",
                div { class: "text-sm font-medium text-zinc-400 mb-4", "Visualization" }
                div { class: "h-64",
                    // Render appropriate widget
                    match node.widget {
                        super::node_graph::NodeWidget::EqGraph => rsx! {
                            NodeFocusEq {}
                        },
                        super::node_graph::NodeWidget::CompressorGraph => rsx! {
                            NodeFocusCompressor {}
                        },
                        _ => rsx! {
                            div { class: "flex items-center justify-center h-full text-zinc-500",
                                "Widget visualization"
                            }
                        }
                    }
                }
            }

            // Parameters grid
            div { class: "grid grid-cols-3 gap-4",
                // Sample parameters (would come from the node in production)
                ParameterKnob { label: "Threshold", value: -20.0, min: -60.0, max: 0.0, unit: "dB" }
                ParameterKnob { label: "Ratio", value: 4.0, min: 1.0, max: 20.0, unit: ":1" }
                ParameterKnob { label: "Attack", value: 10.0, min: 0.1, max: 100.0, unit: "ms" }
                ParameterKnob { label: "Release", value: 100.0, min: 10.0, max: 1000.0, unit: "ms" }
                ParameterKnob { label: "Knee", value: 6.0, min: 0.0, max: 12.0, unit: "dB" }
                ParameterKnob { label: "Makeup", value: 0.0, min: 0.0, max: 30.0, unit: "dB" }
                ParameterKnob { label: "Mix", value: 100.0, min: 0.0, max: 100.0, unit: "%" }
                ParameterKnob { label: "Output", value: 0.0, min: -30.0, max: 30.0, unit: "dB" }
            }
        }
    }
}

/// Interactive EQ for focus mode.
#[component]
fn NodeFocusEq() -> Element {
    let mut bands = use_signal(|| vec![
        EqBand {
            index: 0,
            used: true,
            enabled: true,
            frequency: 100.0,
            gain: 3.0,
            q: 0.7,
            shape: EqBandShape::LowShelf,
            ..Default::default()
        },
        EqBand {
            index: 1,
            used: true,
            enabled: true,
            frequency: 800.0,
            gain: -2.0,
            q: 1.5,
            shape: EqBandShape::Bell,
            ..Default::default()
        },
        EqBand {
            index: 2,
            used: true,
            enabled: true,
            frequency: 4000.0,
            gain: 2.5,
            q: 1.0,
            shape: EqBandShape::HighShelf,
            ..Default::default()
        },
    ]);

    rsx! {
        EqGraph {
            bands: bands,
            db_range: 24.0,
            show_grid: true,
            fill_curve: true,
            disabled: false,
            on_band_change: move |(idx, band): (usize, EqBand)| {
                bands.write()[idx] = band;
            },
            on_band_add: move |band: EqBand| {
                let mut all_bands = bands();
                all_bands.push(band);
                bands.set(all_bands);
            },
            on_band_remove: move |idx: usize| {
                let mut all_bands = bands();
                all_bands.remove(idx);
                bands.set(all_bands);
            },
        }
    }
}

/// Interactive compressor for focus mode.
#[component]
fn NodeFocusCompressor() -> Element {
    let params = CompressorParams {
        threshold: -20.0,
        ratio: 4.0,
        attack: 10.0,
        release: 100.0,
        knee: 6.0,
        makeup: 0.0,
        ..Default::default()
    };

    rsx! {
        CompressorGraph {
            params: params,
            db_range: DbRange::default(),
            show_grid: true,
        }
    }
}

/// Props for parameter knob.
#[derive(Props, Clone, PartialEq)]
struct ParameterKnobProps {
    label: String,
    value: f32,
    min: f32,
    max: f32,
    unit: String,
}

/// Parameter knob display.
#[component]
fn ParameterKnob(props: ParameterKnobProps) -> Element {
    rsx! {
        div { class: "bg-zinc-800/50 rounded-lg p-4 border border-zinc-700",
            div { class: "text-xs font-medium text-zinc-400 mb-2", "{props.label}" }
            div { class: "flex items-baseline gap-1",
                span { class: "text-2xl font-semibold text-zinc-200", "{props.value:.1}" }
                span { class: "text-sm text-zinc-500", "{props.unit}" }
            }
            // Slider
            input {
                r#type: "range",
                class: "w-full mt-2",
                min: "{props.min}",
                max: "{props.max}",
                step: "0.1",
                value: "{props.value}",
            }
        }
    }
}
