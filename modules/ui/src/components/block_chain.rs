//! Unified block chain rendering pipeline.
//!
//! This module replaces the 16 duplicated chain/content components in `rig.rs`
//! with a single generic pipeline that handles any block data source via the
//! `RenderBlock` trait. Both `GlobalBlockInfo` and `EffectiveBlock` implement
//! this trait, enabling one set of chain + content components for all view modes.

use std::collections::HashMap;

use audio_controls::prelude::{
    BlockDefinition, BlockInstance, BlockParameter as AudioBlockParameter, BlockRegistry, BlockView,
    FormFactor, LayoutConstraints, LevelOfDetail, ParameterFormat,
};
use dioxus::prelude::*;
use uuid::Uuid;

use fts::rig::service::GlobalBlockInfo;

use super::rig::{BlockViewMode, EffectiveBlock, EffectiveParameter};

// ============================================================================
// RenderBlock Trait
// ============================================================================

/// Trait for any block-like type that can be rendered in the block chain.
///
/// Both `GlobalBlockInfo` (raw global blocks) and `EffectiveBlock` (preset
/// blocks with snapshot overrides) implement this trait, eliminating the need
/// for duplicate rendering pipelines.
pub trait RenderBlock: Clone + PartialEq + 'static {
    fn id(&self) -> Uuid;
    fn name(&self) -> &str;
    fn order(&self) -> u8;
    fn bypassed(&self) -> bool;

    /// Resolve a `BlockDefinition` for this block.
    fn resolve_definition(&self) -> BlockDefinition;

    /// Create a `BlockInstance` from the resolved definition.
    fn create_instance(&self, definition: &BlockDefinition, form_factor: FormFactor) -> BlockInstance;

    /// Build a unique key string for diffing (includes state that should trigger re-render).
    fn render_key(&self) -> String;

    /// Build param_id → index map for event routing.
    fn param_index_map(&self) -> HashMap<String, u32>;
}

// ============================================================================
// RenderBlock for GlobalBlockInfo
// ============================================================================

impl RenderBlock for GlobalBlockInfo {
    fn id(&self) -> Uuid {
        self.id
    }
    fn name(&self) -> &str {
        &self.name
    }
    fn order(&self) -> u8 {
        self.order
    }
    fn bypassed(&self) -> bool {
        self.bypassed
    }

    fn resolve_definition(&self) -> BlockDefinition {
        BlockRegistry::global().lookup_by_name(&self.name)
    }

    fn create_instance(&self, definition: &BlockDefinition, form_factor: FormFactor) -> BlockInstance {
        let mut instance = BlockInstance::from_definition(definition, self.id.to_string());
        instance.name = self.name.clone();
        instance.bypassed = self.bypassed;
        instance.form_factor = form_factor;
        instance.lod = LevelOfDetail::Compact;
        instance
    }

    fn render_key(&self) -> String {
        format!("{}", self.id)
    }

    fn param_index_map(&self) -> HashMap<String, u32> {
        // GlobalBlockInfo doesn't carry parameter info, use definition order
        let def = self.resolve_definition();
        def.parameters
            .iter()
            .enumerate()
            .map(|(i, p)| (p.id.clone(), i as u32))
            .collect()
    }
}

// ============================================================================
// RenderBlock for EffectiveBlock
// ============================================================================

impl RenderBlock for EffectiveBlock {
    fn id(&self) -> Uuid {
        self.id
    }
    fn name(&self) -> &str {
        &self.name
    }
    fn order(&self) -> u8 {
        self.order
    }
    fn bypassed(&self) -> bool {
        self.bypassed
    }

    fn resolve_definition(&self) -> BlockDefinition {
        let registry = BlockRegistry::global();
        let type_id = self.block_type.to_lowercase();

        let mut def = registry
            .get(&type_id)
            .cloned()
            .unwrap_or_else(|| registry.lookup_by_name(&self.block_type));

        def.name = self.name.clone();

        // Replace parameters with effective values
        def.parameters = self
            .parameters
            .iter()
            .map(effective_param_to_audio_param)
            .collect();

        def
    }

    fn create_instance(&self, definition: &BlockDefinition, form_factor: FormFactor) -> BlockInstance {
        let mut instance = BlockInstance::from_definition(definition, self.id.to_string());
        instance.name = self.name.clone();
        instance.bypassed = self.bypassed;
        instance.form_factor = form_factor;
        instance.lod = LevelOfDetail::Compact;

        // Apply effective parameter values
        for param in &self.parameters {
            if definition.parameters.iter().any(|p| p.id == param.id) {
                instance.set_param(&param.id, param.value as f32);
            }
        }

        instance
    }

    fn render_key(&self) -> String {
        let param_hash = self
            .parameters
            .first()
            .map(|p| (p.value * 1000.0) as i32)
            .unwrap_or(0);
        format!("{}-{}-{}", self.id, self.bypassed, param_hash)
    }

    fn param_index_map(&self) -> HashMap<String, u32> {
        self.parameters
            .iter()
            .enumerate()
            .map(|(i, p)| (p.id.clone(), i as u32))
            .collect()
    }
}

/// Convert an `EffectiveParameter` to an `AudioBlockParameter`.
fn effective_param_to_audio_param(p: &EffectiveParameter) -> AudioBlockParameter {
    let format = if p.unit == "dB" {
        ParameterFormat::Decibels {
            min: p.min_display as f32,
            max: p.max_display as f32,
        }
    } else if p.unit == "ms" {
        ParameterFormat::Milliseconds {
            min: p.min_display as f32,
            max: p.max_display as f32,
        }
    } else if p.unit == "Hz" {
        ParameterFormat::Frequency {
            min: p.min_display as f32,
            max: p.max_display as f32,
        }
    } else {
        ParameterFormat::Percent
    };

    AudioBlockParameter {
        id: p.id.clone(),
        name: p.name.clone(),
        short_name: p.name.chars().take(4).collect(),
        value: p.value as f32,
        default: p.value as f32,
        unit: p.unit.clone(),
        format,
        priority: 0,
    }
}

// ============================================================================
// Unified Chain Component
// ============================================================================

/// Props for the unified BlockChain component.
#[derive(Props, Clone, PartialEq)]
pub struct BlockChainProps<T: RenderBlock> {
    /// Blocks to render in the chain.
    pub blocks: Vec<T>,
    /// View mode determining the visual layout.
    pub view_mode: BlockViewMode,
    /// Callback when a block's bypass is toggled.
    pub on_toggle_bypass: Callback<Uuid>,
    /// Callback when a block parameter changes: (block_id, param_index, value).
    pub on_param_change: Callback<(Uuid, u32, f32)>,
}

/// Unified block chain renderer.
///
/// Renders a sorted sequence of blocks in the specified view mode.
/// Works with any type implementing `RenderBlock`.
#[component]
pub fn BlockChain<T: RenderBlock>(props: BlockChainProps<T>) -> Element {
    if props.blocks.is_empty() {
        return rsx! {
            div { class: "text-center text-muted-foreground py-8",
                "No blocks in the current rig"
            }
        };
    }

    let mut sorted = props.blocks.clone();
    sorted.sort_by_key(|b| b.order());

    match props.view_mode {
        BlockViewMode::Pedalboard => rsx! {
            div {
                class: "pedalboard flex items-end gap-4 p-6
                        bg-gradient-to-b from-zinc-800 to-zinc-900
                        rounded-xl border-4 border-zinc-700
                        shadow-inner overflow-x-auto
                        scrollbar-thin scrollbar-track-zinc-800 scrollbar-thumb-zinc-600
                        hover:scrollbar-thumb-zinc-500",
                style: "scrollbar-width: thin; scrollbar-color: #52525b #27272a;",

                for block in sorted {
                    {
                        let key_str = block.render_key();
                        rsx! {
                            BlockContent {
                                key: "{key_str}",
                                block: block.clone(),
                                form_factor: FormFactor::Pedal,
                                width: 200,
                                height: 260,
                                on_toggle_bypass: props.on_toggle_bypass.clone(),
                                on_param_change: props.on_param_change.clone(),
                            }
                        }
                    }
                }
            }
        },

        BlockViewMode::Cards => rsx! {
            div { class: "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4",
                for block in sorted {
                    {
                        let key_str = block.render_key();
                        rsx! {
                            BlockContent {
                                key: "{key_str}",
                                block: block.clone(),
                                form_factor: FormFactor::Square,
                                width: 280,
                                height: 200,
                                on_toggle_bypass: props.on_toggle_bypass.clone(),
                                on_param_change: props.on_param_change.clone(),
                            }
                        }
                    }
                }
            }
        },

        BlockViewMode::Rack1U => rsx! {
            div {
                class: "bg-zinc-950 rounded-lg border-4 border-zinc-600 p-2 overflow-x-auto",
                style: "background: linear-gradient(180deg, #1a1a1a 0%, #0d0d0d 100%);",

                div { class: "flex gap-1",
                    // Left rack rail
                    div {
                        class: "w-6 flex flex-col justify-between py-2",
                        style: "background: linear-gradient(90deg, #404040, #2a2a2a);",
                        for _i in 0..4u8 {
                            div { class: "w-3 h-3 mx-auto rounded-full bg-zinc-800 border border-zinc-600" }
                        }
                    }

                    div { class: "flex-1 flex gap-2 py-2",
                        for block in sorted {
                            {
                                let key_str = block.render_key();
                                rsx! {
                                    BlockContent {
                                        key: "{key_str}",
                                        block: block.clone(),
                                        form_factor: FormFactor::Rack1U,
                                        width: 200,
                                        height: 80,
                                        on_toggle_bypass: props.on_toggle_bypass.clone(),
                                        on_param_change: props.on_param_change.clone(),
                                    }
                                }
                            }
                        }
                    }

                    // Right rack rail
                    div {
                        class: "w-6 flex flex-col justify-between py-2",
                        style: "background: linear-gradient(90deg, #2a2a2a, #404040);",
                        for _i in 0..4u8 {
                            div { class: "w-3 h-3 mx-auto rounded-full bg-zinc-800 border border-zinc-600" }
                        }
                    }
                }
            }
        },

        BlockViewMode::Series500 => rsx! {
            div {
                class: "bg-zinc-900 rounded-lg border-2 border-zinc-700 p-3 overflow-x-auto",
                style: "background: linear-gradient(180deg, #262626 0%, #171717 100%);",

                div { class: "flex gap-1",
                    for block in sorted {
                        {
                            let key_str = block.render_key();
                            rsx! {
                                BlockChrome500 {
                                    key: "{key_str}",
                                    block: block.clone(),
                                    on_toggle_bypass: props.on_toggle_bypass.clone(),
                                    on_param_change: props.on_param_change.clone(),
                                }
                            }
                        }
                    }
                }
            }
        },
    }
}

// ============================================================================
// BlockContent - Single block rendering
// ============================================================================

/// Props for the unified block content renderer.
#[derive(Props, Clone, PartialEq)]
pub struct BlockContentProps<T: RenderBlock> {
    block: T,
    form_factor: FormFactor,
    width: u32,
    height: u32,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
}

/// Renders a single block using audio_controls::BlockView.
///
/// This replaces the 8 duplicated block content components (PedalBlock,
/// BlockCard, RackBlock, Series500Block, and their Effective* counterparts).
#[component]
pub fn BlockContent<T: RenderBlock>(props: BlockContentProps<T>) -> Element {
    let definition = props.block.resolve_definition();
    let instance = props.block.create_instance(&definition, props.form_factor);

    let block_id = props.block.id();
    let on_bypass = props.on_toggle_bypass.clone();
    let on_param = props.on_param_change.clone();

    let param_id_to_index = props.block.param_index_map();

    let constraints = LayoutConstraints::new(
        props.form_factor,
        props.width,
        props.height,
        LevelOfDetail::Compact,
    );

    rsx! {
        BlockView {
            definition,
            instance,
            constraints: Some(constraints),
            on_bypass_toggle: move |_bypassed| {
                on_bypass.call(block_id);
            },
            on_param_change: move |(param_id, value): (String, f32)| {
                if let Some(&index) = param_id_to_index.get(&param_id) {
                    on_param.call((block_id, index, value));
                }
            },
            on_macro_change: move |_value| {},
        }
    }
}

// ============================================================================
// BlockChrome500 - 500-series chrome wrapper
// ============================================================================

/// 500-series modules have a distinctive chrome wrapper.
#[derive(Props, Clone, PartialEq)]
struct BlockChrome500Props<T: RenderBlock> {
    block: T,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
}

#[component]
fn BlockChrome500<T: RenderBlock>(props: BlockChrome500Props<T>) -> Element {
    rsx! {
        div {
            class: "bg-zinc-800 rounded border border-zinc-600",
            style: "background: linear-gradient(180deg, #3f3f46 0%, #27272a 5%, #27272a 95%, #3f3f46 100%);",

            BlockContent {
                block: props.block,
                form_factor: FormFactor::Series500,
                width: 100,
                height: 320,
                on_toggle_bypass: props.on_toggle_bypass,
                on_param_change: props.on_param_change,
            }
        }
    }
}
