//! Parameter editor modal for editing node parameters.
//!
//! Double-click a node on the canvas to open this modal. Provides:
//! - Knob controls for continuous parameters (audio-controls `Knob` widget)
//! - Slider controls for stepped parameters (audio-controls `HSlider` widget)
//! - Toggle switches for boolean parameters
//! - Dropdown select for choice parameters
//! - Close via Escape key or clicking the backdrop

use crate::callback_types::SetParameter;
use crate::hooks::rig_actions::use_rig_actions;
use crate::prelude::*;
use crate::signals::RIG_NODE_GRAPH;
use audio_controls::widgets::{HSlider, Knob, SliderVariant};
use signal_proto::normalized::NormalizedF64;
use uuid::Uuid;

use super::block_colors::block_type_color;
use super::node_graph::{Node, NodeParameter, ParameterType};

// region: --- Props

/// Props for the parameter editor modal.
#[derive(Props, Clone, PartialEq)]
pub struct ParameterEditorModalProps {
    /// Whether the modal is open.
    pub is_open: bool,
    /// ID of the node being edited.
    pub node_id: Uuid,
    /// Callback to close the modal.
    pub on_close: Callback<()>,
}

// endregion: --- Props

// region: --- Component

/// Parameter editor modal.
///
/// Renders a full-screen overlay with parameter controls for the selected node.
/// Controls are chosen based on `ParameterType`:
/// - `Continuous` → Knob
/// - `Stepped` → HSlider (stepped variant)
/// - `Toggle` → toggle switch
/// - `Choice` → dropdown
#[component]
pub fn ParameterEditorModal(props: ParameterEditorModalProps) -> Element {
    if !props.is_open {
        return rsx! {};
    }

    let actions = use_rig_actions();
    let on_close = props.on_close.clone();
    let on_close_backdrop = props.on_close.clone();
    let on_close_btn = props.on_close.clone();
    let node_id = props.node_id;

    // Read current node data from the global graph
    let graph = RIG_NODE_GRAPH.read();
    let Some(node) = graph.find_node(node_id) else {
        return rsx! {};
    };
    let node = node.clone();
    drop(graph);

    let color = block_type_color(node.block_type);
    let node_name = node.name.clone();
    let params = node.parameters.clone();
    let has_params = !params.is_empty();

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center bg-black/70 backdrop-blur-sm",
            tabindex: "0",
            onclick: move |_| on_close_backdrop.call(()),
            onkeydown: move |evt| {
                if evt.key() == Key::Escape {
                    on_close.call(());
                }
            },

            // Modal container
            div {
                class: "bg-zinc-900 rounded-xl border border-zinc-700 shadow-2xl w-[640px] max-h-[85vh] \
                        flex flex-col overflow-hidden",
                onclick: |e| e.stop_propagation(),

                // Header
                div {
                    class: "flex items-center justify-between px-5 py-4 border-b border-zinc-800",
                    div { class: "flex items-center gap-3",
                        // Color dot
                        div {
                            class: "w-3 h-3 rounded-full flex-shrink-0",
                            style: "background-color: {color.bg};",
                        }
                        h2 { class: "text-lg font-semibold text-zinc-200", "{node_name}" }
                        span {
                            class: "px-2 py-0.5 rounded text-[10px] font-medium uppercase tracking-wider",
                            style: "background-color: {color.bg}30; color: {color.bg};",
                            "{node.block_type:?}"
                        }
                    }
                    button {
                        class: "p-1 rounded hover:bg-zinc-800 text-zinc-400 hover:text-zinc-200 transition-colors",
                        onclick: move |_| on_close_btn.call(()),
                        svg {
                            class: "w-5 h-5",
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

                // Content
                div { class: "flex-1 overflow-y-auto p-5",
                    if !has_params {
                        div { class: "flex items-center justify-center h-32 text-zinc-500",
                            "This node has no editable parameters."
                        }
                    } else {
                        // Parameter grid: knobs for continuous, other controls mixed in
                        div { class: "grid grid-cols-3 gap-5",
                            for (idx, param) in params.iter().enumerate() {
                                ParameterControl {
                                    key: "{param.id}",
                                    node_id: node_id,
                                    param_index: idx as u32,
                                    param: param.clone(),
                                    set_parameter: actions.set_parameter.clone(),
                                }
                            }
                        }
                    }
                }

                // Footer hint
                div { class: "px-5 py-3 border-t border-zinc-800 text-xs text-zinc-500",
                    "Press Esc to close"
                }
            }
        }
    }
}

// endregion: --- Component

// region: --- ParameterControl

#[derive(Props, Clone, PartialEq)]
struct ParameterControlProps {
    node_id: Uuid,
    param_index: u32,
    param: NodeParameter,
    set_parameter: Callback<SetParameter>,
}

/// Routes to the appropriate control widget based on `ParameterType`.
#[component]
fn ParameterControl(props: ParameterControlProps) -> Element {
    match &props.param.param_type {
        ParameterType::Continuous => rsx! {
            ContinuousControl {
                node_id: props.node_id,
                param_index: props.param_index,
                param: props.param.clone(),
                set_parameter: props.set_parameter.clone(),
            }
        },
        ParameterType::Stepped => rsx! {
            SteppedControl {
                node_id: props.node_id,
                param_index: props.param_index,
                param: props.param.clone(),
                set_parameter: props.set_parameter.clone(),
            }
        },
        ParameterType::Toggle => rsx! {
            ToggleControl {
                node_id: props.node_id,
                param_index: props.param_index,
                param: props.param.clone(),
                set_parameter: props.set_parameter.clone(),
            }
        },
        ParameterType::Choice(choices) => rsx! {
            ChoiceControl {
                node_id: props.node_id,
                param_index: props.param_index,
                param: props.param.clone(),
                choices: choices.clone(),
                set_parameter: props.set_parameter.clone(),
            }
        },
    }
}

// endregion: --- ParameterControl

// region: --- Continuous (Knob)

#[derive(Props, Clone, PartialEq)]
struct ContinuousControlProps {
    node_id: Uuid,
    param_index: u32,
    param: NodeParameter,
    set_parameter: Callback<SetParameter>,
}

#[component]
fn ContinuousControl(props: ContinuousControlProps) -> Element {
    let initial = props.param.value.get() as f32;
    let mut value_sig = use_signal(move || initial);
    let display = props.param.display_value();
    let name = props.param.name.clone();
    let node_id = props.node_id;
    let param_index = props.param_index;
    let param_id = props.param.id.clone();
    let set_parameter = props.set_parameter.clone();

    rsx! {
        div { class: "flex flex-col items-center gap-2",
            Knob {
                value: value_sig,
                size: 56,
                label: name.clone(),
                value_display: display,
                on_change: move |new_val: f32| {
                    value_sig.set(new_val);
                    let clamped = (new_val as f64).clamp(0.0, 1.0);
                    RIG_NODE_GRAPH.write().find_node_mut(node_id).map(|n| {
                        if let Some(p) = n.parameters.iter_mut().find(|p| p.id == param_id) {
                            p.value = NormalizedF64::new(clamped);
                        }
                    });
                    set_parameter.call(SetParameter { node_id, param_index, value: clamped });
                },
            }
        }
    }
}

// endregion: --- Continuous (Knob)

// region: --- Stepped (HSlider)

#[derive(Props, Clone, PartialEq)]
struct SteppedControlProps {
    node_id: Uuid,
    param_index: u32,
    param: NodeParameter,
    set_parameter: Callback<SetParameter>,
}

#[component]
fn SteppedControl(props: SteppedControlProps) -> Element {
    let initial = props.param.value.get() as f32;
    let mut value_sig = use_signal(move || initial);
    let display = props.param.display_value();
    let name = props.param.name.clone();
    let node_id = props.node_id;
    let param_index = props.param_index;
    let param_id = props.param.id.clone();
    let set_parameter = props.set_parameter.clone();
    let steps = (props.param.max - props.param.min).round() as u32;

    rsx! {
        div { class: "flex flex-col gap-1.5 col-span-2",
            span { class: "text-xs text-zinc-400 font-medium", "{name}" }
            HSlider {
                value: value_sig,
                width: 200,
                height: 24,
                variant: SliderVariant::Stepped(steps.max(2)),
                value_display: display,
                on_change: move |new_val: f32| {
                    value_sig.set(new_val);
                    let clamped = (new_val as f64).clamp(0.0, 1.0);
                    RIG_NODE_GRAPH.write().find_node_mut(node_id).map(|n| {
                        if let Some(p) = n.parameters.iter_mut().find(|p| p.id == param_id) {
                            p.value = NormalizedF64::new(clamped);
                        }
                    });
                    set_parameter.call(SetParameter { node_id, param_index, value: clamped });
                },
            }
        }
    }
}

// endregion: --- Stepped (HSlider)

// region: --- Toggle

#[derive(Props, Clone, PartialEq)]
struct ToggleControlProps {
    node_id: Uuid,
    param_index: u32,
    param: NodeParameter,
    set_parameter: Callback<SetParameter>,
}

#[component]
fn ToggleControl(props: ToggleControlProps) -> Element {
    let is_on = props.param.value.get() >= 0.5;
    let name = props.param.name.clone();
    let node_id = props.node_id;
    let param_index = props.param_index;
    let param_id = props.param.id.clone();
    let set_parameter = props.set_parameter.clone();

    let (track_class, knob_style) = if is_on {
        (
            "w-9 h-5 rounded-full transition-colors bg-green-500 cursor-pointer",
            "position: absolute; top: 2px; left: 18px; width: 16px; height: 16px; \
             background: white; border-radius: 9999px; transition: left 0.15s ease;",
        )
    } else {
        (
            "w-9 h-5 rounded-full transition-colors bg-zinc-600 cursor-pointer",
            "position: absolute; top: 2px; left: 2px; width: 16px; height: 16px; \
             background: white; border-radius: 9999px; transition: left 0.15s ease;",
        )
    };

    rsx! {
        div { class: "flex flex-col items-center gap-2",
            span { class: "text-xs text-zinc-400 font-medium", "{name}" }
            div {
                class: "relative {track_class}",
                onclick: move |_| {
                    let new_val = if is_on { 0.0 } else { 1.0 };
                    RIG_NODE_GRAPH.write().find_node_mut(node_id).map(|n| {
                        if let Some(p) = n.parameters.iter_mut().find(|p| p.id == param_id) {
                            p.value = NormalizedF64::new(new_val);
                        }
                    });
                    set_parameter.call(SetParameter { node_id, param_index, value: new_val });
                },
                div { style: knob_style }
            }
            span { class: "text-[10px] text-zinc-500",
                if is_on { "On" } else { "Off" }
            }
        }
    }
}

// endregion: --- Toggle

// region: --- Choice (Dropdown)

#[derive(Props, Clone, PartialEq)]
struct ChoiceControlProps {
    node_id: Uuid,
    param_index: u32,
    param: NodeParameter,
    choices: Vec<String>,
    set_parameter: Callback<SetParameter>,
}

#[component]
fn ChoiceControl(props: ChoiceControlProps) -> Element {
    let name = props.param.name.clone();
    let node_id = props.node_id;
    let param_index = props.param_index;
    let param_id = props.param.id.clone();
    let set_parameter = props.set_parameter.clone();
    let choices = props.choices.clone();
    let choice_count = choices.len();

    let current_idx = if choice_count > 1 {
        (props.param.value.get() * (choice_count as f64 - 1.0)).round() as usize
    } else {
        0
    };

    rsx! {
        div { class: "flex flex-col gap-1.5 col-span-2",
            span { class: "text-xs text-zinc-400 font-medium", "{name}" }
            select {
                class: "px-3 py-1.5 text-sm bg-zinc-800 border border-zinc-700 rounded-lg \
                        focus:outline-none focus:ring-1 focus:ring-zinc-600 text-zinc-300 \
                        cursor-pointer appearance-none",
                onchange: move |evt: FormEvent| {
                    if let Ok(idx) = evt.value().parse::<usize>() {
                        let norm = if choice_count > 1 {
                            idx as f64 / (choice_count as f64 - 1.0)
                        } else {
                            0.0
                        };
                        RIG_NODE_GRAPH.write().find_node_mut(node_id).map(|n| {
                            if let Some(p) = n.parameters.iter_mut().find(|p| p.id == param_id) {
                                p.value = NormalizedF64::new(norm);
                            }
                        });
                        set_parameter.call(SetParameter { node_id, param_index, value: norm });
                    }
                },
                for (idx, choice) in choices.iter().enumerate() {
                    option {
                        value: "{idx}",
                        selected: idx == current_idx,
                        "{choice}"
                    }
                }
            }
        }
    }
}

// endregion: --- Choice (Dropdown)

// ── Tests ───────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::rig_grid::node_graph::*;
    use signal_control::block::BlockType;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    // -- ParameterType

    #[test]
    fn test_parameter_type_default_is_continuous() -> Result<()> {
        // -- Check
        assert_eq!(ParameterType::default(), ParameterType::Continuous);
        Ok(())
    }

    #[test]
    fn test_parameter_type_choice_stores_options() -> Result<()> {
        // -- Setup & Fixtures
        let choices = vec!["Clean".into(), "Crunch".into(), "Lead".into()];
        let pt = ParameterType::Choice(choices.clone());

        // -- Check
        if let ParameterType::Choice(c) = pt {
            assert_eq!(c, choices);
        } else {
            panic!("Expected Choice variant");
        }
        Ok(())
    }

    // -- NodeParameter extended fields

    #[test]
    fn test_node_parameter_new_has_defaults() -> Result<()> {
        // -- Setup & Fixtures
        let param = NodeParameter::new("drive", "Drive", NormalizedF64::new(0.7));

        // -- Check
        assert_eq!(param.min, 0.0);
        assert_eq!(param.max, 1.0);
        assert!(param.unit.is_empty());
        assert_eq!(param.param_type, ParameterType::Continuous);
        Ok(())
    }

    #[test]
    fn test_node_parameter_with_range() -> Result<()> {
        // -- Setup & Fixtures
        let param = NodeParameter::new("freq", "Frequency", NormalizedF64::new(0.5))
            .with_range(20.0, 20000.0)
            .with_unit("Hz");

        // -- Check
        assert_eq!(param.min, 20.0);
        assert_eq!(param.max, 20000.0);
        assert_eq!(param.unit, "Hz");
        Ok(())
    }

    #[test]
    fn test_node_parameter_with_param_type() -> Result<()> {
        // -- Setup & Fixtures
        let param = NodeParameter::new("mode", "Mode", NormalizedF64::new(0.0))
            .with_param_type(ParameterType::Choice(vec!["A".into(), "B".into()]));

        // -- Check
        assert!(matches!(param.param_type, ParameterType::Choice(_)));
        Ok(())
    }

    // -- display_value

    #[test]
    fn test_display_value_continuous() -> Result<()> {
        // -- Setup & Fixtures
        let param = NodeParameter::new("gain", "Gain", NormalizedF64::new(0.5))
            .with_range(-12.0, 12.0)
            .with_unit("dB");

        // -- Exec
        let display = param.display_value();

        // -- Check
        assert_eq!(display, "0.0 dB");
        Ok(())
    }

    #[test]
    fn test_display_value_toggle_on() -> Result<()> {
        // -- Setup & Fixtures
        let param = NodeParameter::new("bypass", "Bypass", NormalizedF64::new(1.0))
            .with_param_type(ParameterType::Toggle);

        // -- Exec
        let display = param.display_value();

        // -- Check
        assert_eq!(display, "On");
        Ok(())
    }

    #[test]
    fn test_display_value_toggle_off() -> Result<()> {
        // -- Setup & Fixtures
        let param = NodeParameter::new("bypass", "Bypass", NormalizedF64::new(0.0))
            .with_param_type(ParameterType::Toggle);

        // -- Exec
        let display = param.display_value();

        // -- Check
        assert_eq!(display, "Off");
        Ok(())
    }

    #[test]
    fn test_display_value_choice() -> Result<()> {
        // -- Setup & Fixtures
        let param = NodeParameter::new("type", "Type", NormalizedF64::new(0.5)).with_param_type(
            ParameterType::Choice(vec!["Clean".into(), "Crunch".into(), "Lead".into()]),
        );

        // -- Exec
        let display = param.display_value();

        // -- Check
        assert_eq!(display, "Crunch");
        Ok(())
    }

    #[test]
    fn test_display_value_no_unit() -> Result<()> {
        // -- Setup & Fixtures
        let param = NodeParameter::new("mix", "Mix", NormalizedF64::new(0.75));

        // -- Exec
        let display = param.display_value();

        // -- Check
        assert_eq!(display, "0.8"); // 0.0 + 0.75 * 1.0 = 0.75, formatted as 0.8
        Ok(())
    }

    // -- Backward compatibility: existing code still works

    #[test]
    fn test_backward_compat_new_still_works() -> Result<()> {
        // -- Setup & Fixtures
        // The same constructor from before still creates a valid parameter
        let param = NodeParameter::new("drive", "Drive", NormalizedF64::new(0.7));

        // -- Check
        assert_eq!(param.id, "drive");
        assert_eq!(param.name, "Drive");
        assert!((param.value.get() - 0.7).abs() < f64::EPSILON);
        Ok(())
    }
}
