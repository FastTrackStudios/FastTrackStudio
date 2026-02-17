//! Signal2 block slider — the original prototype view.
//!
//! Renders block parameter sliders, block collections (presets),
//! module collections, and a semantic browser search UI.

use dioxus::prelude::*;
use signal::tagging::{BrowserHit, BrowserMode, BrowserQuery};
use signal::{Block, BlockType, ModulePreset, Preset, SignalController, Snapshot};

// region: --- Main Component

#[component]
pub fn SignalSlider(controller: SignalController) -> Element {
    let mut block_type = use_signal(|| BlockType::Amp);
    let mut block = use_signal(Block::default);
    let mut collections = use_signal(Vec::<Preset>::new);
    let mut module_collections = use_signal(Vec::<ModulePreset>::new);
    let mut active_variant_id = use_signal(|| None::<String>);
    let mut browser_query_input = use_signal(String::new);
    let mut browser_hits = use_signal(Vec::<BrowserHit>::new);
    let mut browser_entry_count = use_signal(|| 0usize);

    // Fetch block state, block collections, and module collections on type change.
    {
        let controller = controller.clone();
        use_effect(move || {
            let controller = controller.clone();
            let selected = block_type();
            spawn(async move {
                block.set(controller.get_block(selected).await);
                #[allow(deprecated)]
                collections.set(controller.list_presets(selected).await);
                #[allow(deprecated)]
                module_collections.set(controller.list_module_presets().await);
                active_variant_id.set(None);
            });
        });
    }

    // Prime browser index and initial semantic query once at mount.
    {
        let controller = controller.clone();
        use_effect(move || {
            let controller = controller.clone();
            spawn(async move {
                let index = controller.browser_index().await;
                browser_entry_count.set(index.entries().len());
                browser_hits.set(
                    controller
                        .browse(BrowserQuery {
                            mode: BrowserMode::Semantic,
                            include: vec!["tone:clean".to_string()],
                            ..BrowserQuery::default()
                        })
                        .await,
                );
            });
        });
    }

    let active_block_type = block_type();
    let b: Block = block();
    let params = b.parameters().to_vec();

    rsx! {
        div { class: "max-w-2xl mx-auto p-6 space-y-8",
            h1 { class: "text-xl font-semibold mb-4", "signal block" }

            // -- Block type selector
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

            // -- Parameter sliders
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

            // -- Block collections (presets rendered as collection/variant)
            div { class: "space-y-3",
                h2 { class: "text-lg font-semibold", "Collections" }
                for collection in collections().into_iter() {
                    {
                        let collection_id = collection.id().to_string();
                        rsx! {
                            CollectionCard {
                                key: "{collection_id}",
                                collection,
                                controller: controller.clone(),
                                block_type: active_block_type,
                                block,
                                active_variant_id,
                            }
                        }
                    }
                }
            }

            // -- Module collections (module presets rendered as collection/variant)
            if !module_collections().is_empty() {
                div { class: "space-y-3",
                    h2 { class: "text-lg font-semibold", "Module Collections" }
                    for module_collection in module_collections().into_iter() {
                        {
                            let mc_id = module_collection.id().to_string();
                            rsx! {
                                ModuleCollectionCard {
                                    key: "{mc_id}",
                                    collection: module_collection,
                                    controller: controller.clone(),
                                }
                            }
                        }
                    }
                }
            }

            // -- Semantic browser (new typed tagging system)
            div { class: "space-y-3",
                h2 { class: "text-lg font-semibold", "Browser" }
                p { class: "text-xs text-zinc-600", "Indexed entries: {browser_entry_count}" }
                div { class: "flex gap-2",
                    input {
                        class: "flex-1 border border-zinc-300 rounded px-2 py-1 text-sm",
                        placeholder: "tags e.g. tone:clean rig_type:keys",
                        value: "{browser_query_input}",
                        oninput: move |e| browser_query_input.set(e.value()),
                    }
                    button {
                        class: "px-3 py-1 rounded border border-zinc-400 hover:bg-zinc-100 text-sm",
                        onclick: {
                            let controller = controller.clone();
                            move |_| {
                                let controller = controller.clone();
                                let raw = browser_query_input();
                                let include = raw
                                    .split_whitespace()
                                    .map(str::trim)
                                    .filter(|s| !s.is_empty())
                                    .map(ToString::to_string)
                                    .collect::<Vec<_>>();
                                spawn(async move {
                                    let hits = controller
                                        .browse(BrowserQuery {
                                            mode: BrowserMode::Semantic,
                                            include,
                                            ..BrowserQuery::default()
                                        })
                                        .await;
                                    browser_hits.set(hits);
                                });
                            }
                        },
                        "Search"
                    }
                }
                div { class: "space-y-2",
                    for hit in browser_hits().into_iter().take(16) {
                        div {
                            key: "{hit.node.id}",
                            class: "border border-zinc-200 rounded p-2",
                            p { class: "text-sm font-medium", "{hit.node.id}" }
                            p { class: "text-xs text-zinc-600", "{hit.node.kind:?}  score={hit.score:.2}" }
                        }
                    }
                }
            }
        }
    }
}

// endregion: --- Main Component

// region: --- Block Collection Card

#[component]
fn CollectionCard(
    collection: Preset,
    controller: SignalController,
    block_type: BlockType,
    mut block: Signal<Block>,
    mut active_variant_id: Signal<Option<String>>,
) -> Element {
    let collection_id = collection.id().to_string();
    let collection_name = collection.name().to_string();
    let variants = collection.snapshots().to_vec();

    let default_variant_id =
        normalize_default_variant_id(&variants, &collection.default_snapshot());

    rsx! {
        div { class: "rounded-md border border-zinc-300 p-3 space-y-2",
            div { class: "flex items-center justify-between gap-3",
                p { class: "font-medium", "{collection_name}" }
                button {
                    class: "px-2 py-1 text-xs rounded border border-zinc-400 hover:bg-zinc-100",
                    onclick: {
                        let controller = controller.clone();
                        let collection_id = collection_id.clone();
                        let default_id = default_variant_id.clone();
                        move |_| {
                            let controller = controller.clone();
                            let collection_id = collection_id.clone();
                            let default_id = default_id.clone();
                            spawn(async move {
                                #[allow(deprecated)]
                                if let Some(next_block) = controller.load_preset_snapshot(block_type, collection_id.as_str(), default_id.as_str()).await {
                                    block.set(next_block);
                                    active_variant_id.set(Some(default_id));
                                }
                            });
                        }
                    },
                    "Load default variant"
                }
            }

            MetadataPlaceholder {}

            div { class: "space-y-2",
                for variant in variants.into_iter() {
                    {
                        let variant_id = variant.id().to_string();
                        let variant_name = variant.name().to_string();
                        let vb = variant.block();
                        let is_active = active_variant_id().as_deref() == Some(variant_id.as_str());
                        rsx! {
                            button {
                                key: "{variant_id}",
                                class: if is_active { "w-full text-left p-2 rounded border-2 border-zinc-400 bg-zinc-50" } else { "w-full text-left p-2 rounded border border-zinc-200 hover:bg-zinc-50" },
                                onclick: {
                                    let controller = controller.clone();
                                    let collection_id = collection_id.clone();
                                    let variant_id = variant_id.clone();
                                    move |_| {
                                        let controller = controller.clone();
                                        let collection_id = collection_id.clone();
                                        let variant_id = variant_id.clone();
                                        spawn(async move {
                                            #[allow(deprecated)]
                                            if let Some(next_block) = controller.load_preset_snapshot(block_type, collection_id.as_str(), variant_id.as_str()).await {
                                                block.set(next_block);
                                                active_variant_id.set(Some(variant_id));
                                            }
                                        });
                                    }
                                },
                                div { class: "text-sm font-medium", "{variant_name}" }
                                p { class: "text-xs text-zinc-600",
                                    for parameter in vb.parameters().iter() {
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

// endregion: --- Block Collection Card

// region: --- Module Collection Card

#[component]
fn ModuleCollectionCard(collection: ModulePreset, controller: SignalController) -> Element {
    let collection_id = collection.id().to_string();
    let collection_name = collection.name().to_string();
    let variants = collection.snapshots().to_vec();
    let default_variant_id = collection.default_snapshot().id().to_string();
    let mut loaded_variant = use_signal(|| None::<String>);

    rsx! {
        div { class: "rounded-md border border-zinc-300 p-3 space-y-2",
            div { class: "flex items-center justify-between gap-3",
                p { class: "font-medium", "{collection_name}" }
                button {
                    class: "px-2 py-1 text-xs rounded border border-zinc-400 hover:bg-zinc-100",
                    onclick: {
                        let controller = controller.clone();
                        let collection_id = collection_id.clone();
                        let default_variant_id = default_variant_id.clone();
                        move |_| {
                            let controller = controller.clone();
                            let collection_id = collection_id.clone();
                            let default_variant_id = default_variant_id.clone();
                            spawn(async move {
                                #[allow(deprecated)]
                                let _ = controller.load_module_preset_snapshot(collection_id.as_str(), default_variant_id.as_str()).await;
                                loaded_variant.set(Some(default_variant_id));
                            });
                        }
                    },
                    "Load default variant"
                }
            }

            MetadataPlaceholder {}

            div { class: "space-y-2",
                for variant in variants.into_iter() {
                    {
                        let variant_id = variant.id().to_string();
                        let variant_name = variant.name().to_string();
                        let block_count = variant.module().blocks().len();
                        let is_active = loaded_variant().as_deref() == Some(variant_id.as_str());
                        rsx! {
                            button {
                                key: "{variant_id}",
                                class: if is_active { "w-full text-left p-2 rounded border-2 border-zinc-400 bg-zinc-50" } else { "w-full text-left p-2 rounded border border-zinc-200 hover:bg-zinc-50" },
                                onclick: {
                                    let controller = controller.clone();
                                    let collection_id = collection_id.clone();
                                    let variant_id = variant_id.clone();
                                    move |_| {
                                        let controller = controller.clone();
                                        let collection_id = collection_id.clone();
                                        let variant_id = variant_id.clone();
                                        spawn(async move {
                                            #[allow(deprecated)]
                                            let _ = controller.load_module_preset_snapshot(collection_id.as_str(), variant_id.as_str()).await;
                                            loaded_variant.set(Some(variant_id));
                                        });
                                    }
                                },
                                div { class: "text-sm font-medium", "{variant_name}" }
                                p { class: "text-xs text-zinc-600",
                                    "{block_count} block(s)"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// endregion: --- Module Collection Card

// region: --- Metadata Placeholder

#[component]
fn MetadataPlaceholder() -> Element {
    rsx! {
        div { class: "text-xs text-zinc-400 space-y-0.5 border-t border-zinc-200 pt-1 mt-1",
            p { "Tags: \u{2014}" }
            p { "Description: \u{2014}" }
            p { "Notes: \u{2014}" }
        }
    }
}

// endregion: --- Metadata Placeholder

// region: --- Parameter Slider

#[component]
pub(crate) fn ParameterSlider(
    label: String,
    value: f32,
    oninput: EventHandler<FormEvent>,
) -> Element {
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

// endregion: --- Parameter Slider

// region: --- Normalization Helpers

pub(crate) fn normalize_default_variant_id(
    variants: &[Snapshot],
    explicit_default: &Snapshot,
) -> String {
    if variants.iter().any(|v| v.id() == explicit_default.id()) {
        return explicit_default.id().to_string();
    }
    if let Some(first) = variants.first() {
        return first.id().to_string();
    }
    explicit_default.id().to_string()
}

// endregion: --- Normalization Helpers
