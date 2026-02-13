//! Minimal UI for signal2.

use dioxus::prelude::*;
use signal::{Block, BlockType, Preset, SignalController};

#[component]
pub fn SignalSlider(controller: SignalController) -> Element {
    let mut block_type = use_signal(|| BlockType::Amp);
    let mut block = use_signal(Block::default);
    let mut presets = use_signal(Vec::<Preset>::new);
    {
        let controller = controller.clone();
        use_effect(move || {
            let controller = controller.clone();
            let selected = block_type();
            spawn(async move {
                block.set(controller.get_block(selected).await);
                presets.set(controller.list_presets(selected).await);
            });
        });
    }
    let active_block_type = block_type();
    let b: Block = block();
    let params = b.parameters().to_vec();

    rsx! {
        div { class: "max-w-2xl mx-auto p-6 space-y-8",
            h1 { class: "text-xl font-semibold mb-4", "signal2 block" }
            div { class: "flex gap-2",
                button {
                    class: if active_block_type == BlockType::Amp { "px-3 py-1 rounded border bg-zinc-200" } else { "px-3 py-1 rounded border border-zinc-300" },
                    onclick: move |_| block_type.set(BlockType::Amp),
                    "Amp"
                }
                button {
                    class: if active_block_type == BlockType::Drive { "px-3 py-1 rounded border bg-zinc-200" } else { "px-3 py-1 rounded border border-zinc-300" },
                    onclick: move |_| block_type.set(BlockType::Drive),
                    "Drive"
                }
            }

            div { class: "space-y-4",
                for (index, parameter) in params.into_iter().enumerate() {
                    {
                        let label = parameter.name().to_string();
                        let value = parameter.value().get();
                        let row_controller = controller.clone();
                        rsx! {
                            ParameterSlider {
                                key: "{parameter.id()}",
                                label,
                                value,
                                oninput: move |e: FormEvent| {
                                    if let Ok(next) = e.value().parse::<f32>() {
                                        let mut current = block();
                                        current.set_parameter_value(index, next);
                                        block.set(current.clone());
                                        let controller = row_controller.clone();
                                        let selected = active_block_type;
                                        spawn(async move {
                                            let _ = controller.set_block(selected, current).await;
                                        });
                                    }
                                },
                            }
                        }
                    }
                }
            }

            div { class: "space-y-3",
                h2 { class: "text-lg font-semibold", "Presets" }
                for preset in presets().into_iter() {
                    {
                        let preset_id = preset.id().to_string();
                        let preset_name = preset.name().to_string();
                        let snapshots = preset.snapshots().to_vec();
                        rsx! {
                            div { key: "{preset_id}", class: "rounded-md border border-zinc-300 p-3 space-y-2",
                                div { class: "flex items-center justify-between gap-3",
                                    p { class: "font-medium", "{preset_name}" }
                                    button {
                                        class: "px-2 py-1 text-xs rounded border border-zinc-400 hover:bg-zinc-100",
                                        onclick: {
                                            let controller = controller.clone();
                                            let preset_id = preset_id.clone();
                                            move |_| {
                                                let controller = controller.clone();
                                                let preset_id = preset_id.clone();
                                                let selected = active_block_type;
                                                spawn(async move {
                                                    if let Some(next_block) = controller.load_preset(selected, preset_id).await {
                                                        block.set(next_block);
                                                    }
                                                });
                                            }
                                        },
                                        "Load default"
                                    }
                                }
                                div { class: "space-y-2",
                                    for snapshot in snapshots.into_iter() {
                                        {
                                            let snapshot_id = snapshot.id().to_string();
                                            let snapshot_name = snapshot.name().to_string();
                                            let sb = snapshot.block();
                                            rsx! {
                                                button {
                                                    key: "{snapshot_id}",
                                                    class: "w-full text-left p-2 rounded border border-zinc-200 hover:bg-zinc-50",
                                                    onclick: {
                                                        let controller = controller.clone();
                                                        let preset_id = preset_id.clone();
                                                        let snapshot_id = snapshot_id.clone();
                                                        move |_| {
                                                            let controller = controller.clone();
                                                            let preset_id = preset_id.clone();
                                                            let snapshot_id = snapshot_id.clone();
                                                            let selected = active_block_type;
                                                            spawn(async move {
                                                                if let Some(next_block) = controller.load_preset_snapshot(selected, preset_id, snapshot_id).await {
                                                                    block.set(next_block);
                                                                }
                                                            });
                                                        }
                                                    },
                                                    div { class: "text-sm font-medium", "{snapshot_name}" }
                                                    p { class: "text-xs text-zinc-600",
                                                        for parameter in sb.parameters().iter() {
                                                            span { "{parameter.name()} {parameter.value().get():.2} " }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn ParameterSlider(label: String, value: f32, oninput: EventHandler<FormEvent>) -> Element {
    rsx! {
        div { class: "space-y-1",
            div { class: "text-sm font-medium", "{label}" }
            input {
                r#type: "range",
                min: "0",
                max: "1",
                step: "0.01",
                value: "{value}",
                oninput: move |e| oninput.call(e),
            }
            p { class: "text-xs text-zinc-600", "{value:.2}" }
        }
    }
}
