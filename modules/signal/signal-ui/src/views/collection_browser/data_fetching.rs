//! Async data fetching and detail resolution for the collection browser.
//!
//! Each `fetch_col*` function queries the `SignalController` for the
//! appropriate domain entities and maps them into `ColumnItem` rows.

use signal::layer::Layer;
use signal::rig::RigType;
use signal::tagging::{StructuredTag, TagCategory, TagSet};
use signal::traits::HasMetadata;
use signal::SignalController;
use signal::{
    BlockType, ModuleBlock, ModuleBlockSource, Preset, SignalChain, SignalNode, ALL_BLOCK_TYPES,
};

use super::grid_conversion::ParamLookup;
use super::types::{
    ColumnItem, DetailData, EngineFlowData, LayerFlowData, ModuleChainData, NavCategory,
};

// region: --- Column fetching

pub(super) async fn fetch_col2(
    controller: &SignalController,
    nav: NavCategory,
    rig_type: RigType,
) -> Vec<ColumnItem> {
    match nav {
        NavCategory::Presets => {
            let rigs = controller.list_rig_collections().await;
            rigs.into_iter()
                .filter(|r| r.rig_type.map_or(false, |rt| rt == rig_type))
                .map(|r| {
                    let meta = r.metadata().clone();
                    let tags = TagSet::from_tags(&meta.tags);
                    ColumnItem {
                        id: r.id.to_string(),
                        name: r.name.clone(),
                        subtitle: None,
                        badge: Some(format!("{}", r.variants.len())),
                        metadata: Some(meta),
                        structured_tags: tags,
                        detail: DetailData::default(),
                        tag: None,
                    }
                })
                .collect()
        }
        NavCategory::Engines => {
            let et = rig_type_to_engine_type(rig_type);
            let engines = controller.list_engines().await;
            engines
                .into_iter()
                .filter(|e| e.engine_type == et)
                .map(|e| {
                    let meta = e.metadata().clone();
                    let tags = TagSet::from_tags(&meta.tags);
                    ColumnItem {
                        id: e.id.to_string(),
                        name: e.name.clone(),
                        subtitle: Some(format!("{} layer(s)", e.layer_ids.len())),
                        badge: Some(format!("{}", e.variants.len())),
                        metadata: Some(meta),
                        structured_tags: tags,
                        detail: DetailData::default(),
                        tag: None,
                    }
                })
                .collect()
        }
        NavCategory::Modules => {
            // Show module types as col2 items (like Blocks shows block types).
            // Count how many presets exist per module type for the badge.
            let all_presets = controller.list_module_collections().await;
            signal::ALL_MODULE_TYPES
                .iter()
                .enumerate()
                .map(|(idx, &mt)| {
                    let count = all_presets.iter().filter(|p| p.module_type() == mt).count();
                    let mut tags = TagSet::default();
                    tags.insert(StructuredTag::new(TagCategory::Module, mt.as_str()));
                    ColumnItem {
                        id: mt.as_str().to_string(),
                        name: mt.display_name().to_string(),
                        subtitle: Some(mt.category().display_name().to_string()),
                        badge: if count > 0 {
                            Some(format!("{count}"))
                        } else {
                            None
                        },
                        metadata: None,
                        structured_tags: tags,
                        detail: DetailData::default(),
                        tag: Some(idx),
                    }
                })
                .collect()
        }
        NavCategory::Blocks => ALL_BLOCK_TYPES
            .iter()
            .enumerate()
            .map(|(idx, bt)| {
                let mut tags = TagSet::default();
                tags.insert(StructuredTag::new(TagCategory::Block, bt.as_str()));
                ColumnItem {
                    id: bt.as_str().to_string(),
                    name: bt.display_name().to_string(),
                    subtitle: Some(bt.category().display_name().to_string()),
                    badge: None,
                    metadata: None,
                    structured_tags: tags,
                    detail: DetailData::default(),
                    tag: Some(idx),
                }
            })
            .collect(),
    }
}

/// Returns (column items, block presets cache).
/// The cache is non-empty only for `NavCategory::Blocks` — it holds the raw
/// `Preset` objects so col4 can extract snapshots without re-querying.
pub(super) async fn fetch_col3(
    controller: &SignalController,
    nav: NavCategory,
    col2_id: &str,
    col2_tag: Option<usize>,
) -> (Vec<ColumnItem>, Vec<Preset>) {
    match nav {
        NavCategory::Presets => {
            let items = if let Some(rig) = controller.load_rig_collection(col2_id).await {
                let mut out = Vec::new();
                for v in &rig.variants {
                    let engines = resolve_rig_scene_engines(controller, v).await;
                    let meta = v.metadata().clone();
                    let tags = TagSet::from_tags(&meta.tags);
                    out.push(ColumnItem {
                        id: v.id.to_string(),
                        name: v.name.clone(),
                        subtitle: Some(format!("{} engine(s)", v.engine_selections.len())),
                        badge: None,
                        metadata: Some(meta),
                        structured_tags: tags,
                        detail: DetailData {
                            engines,
                            ..Default::default()
                        },
                        tag: None,
                    });
                }
                out
            } else {
                Vec::new()
            };
            (items, Vec::new())
        }
        NavCategory::Engines => {
            let items = if let Some(engine) = controller.load_engine(col2_id).await {
                let mut items = Vec::new();
                for layer_id in &engine.layer_ids {
                    if let Some(layer) = controller.load_layer(layer_id.as_str()).await {
                        let module_chains = resolve_layer_module_chains(controller, &layer).await;
                        let meta = layer.metadata().clone();
                        let tags = TagSet::from_tags(&meta.tags);
                        items.push(ColumnItem {
                            id: layer.id.to_string(),
                            name: layer.name.clone(),
                            subtitle: Some(format!("{} variant(s)", layer.variants.len())),
                            badge: None,
                            metadata: Some(meta),
                            structured_tags: tags,
                            detail: DetailData {
                                module_chains,
                                ..Default::default()
                            },
                            tag: None,
                        });
                    }
                }
                items
            } else {
                Vec::new()
            };
            (items, Vec::new())
        }
        NavCategory::Modules => {
            // col2 is a module type index — show presets for that type.
            if let Some(idx) = col2_tag {
                if let Some(&mt) = signal::ALL_MODULE_TYPES.get(idx) {
                    let all_presets = controller.list_module_collections().await;
                    let items: Vec<ColumnItem> = all_presets
                        .iter()
                        .filter(|p| p.module_type() == mt)
                        .map(|p| {
                            // Load default snapshot chain for detail preview
                            let chain = p.snapshots().first().map(|s| s.module().chain().clone());
                            ColumnItem {
                                id: p.id().to_string(),
                                name: p.name().to_string(),
                                subtitle: Some(format!("{} snapshot(s)", p.snapshots().len())),
                                badge: Some(format!("{}", p.snapshots().len())),
                                metadata: None,
                                structured_tags: TagSet::default(),
                                detail: DetailData {
                                    chain,
                                    ..Default::default()
                                },
                                tag: col2_tag,
                            }
                        })
                        .collect();
                    return (items, Vec::new());
                }
            }
            (Vec::new(), Vec::new())
        }
        NavCategory::Blocks => {
            if let Some(idx) = col2_tag {
                if let Some(&bt) = ALL_BLOCK_TYPES.get(idx) {
                    let presets = controller.list_collections(bt).await;
                    let items = presets
                        .iter()
                        .map(|p| {
                            let tags = signal::tagging::infer_tags_from_name(p.name());
                            ColumnItem {
                                id: p.id().to_string(),
                                name: p.name().to_string(),
                                subtitle: None,
                                badge: Some(format!("{}", p.snapshots().len())),
                                metadata: None,
                                structured_tags: tags,
                                detail: DetailData::default(),
                                tag: col2_tag,
                            }
                        })
                        .collect();
                    return (items, presets);
                }
            }
            (Vec::new(), Vec::new())
        }
    }
}

// endregion: --- Column fetching

// region: --- Detail resolution helpers

/// Resolve a layer's default variant refs into `ModuleChainData` for grid rendering.
///
/// Handles all three ref types:
/// - `module_refs` → full module chains from module presets
/// - `block_refs` → single-block synthetic chains (one per block)
/// - `layer_refs` → recursively resolved nested layers
async fn resolve_layer_module_chains(
    controller: &SignalController,
    layer: &Layer,
) -> Vec<ModuleChainData> {
    let variant = match layer.default_variant() {
        Some(v) => v,
        None => return Vec::new(),
    };
    // Pre-fetch all module presets to look up module types for colors.
    let all_module_presets = controller.list_module_collections().await;

    // Build a block preset lookup: preset_id → (BlockType, preset_name)
    // so we can resolve block_refs without knowing their type upfront.
    let block_preset_lookup = build_block_preset_lookup(controller).await;

    let mut out = Vec::new();

    // 1) Resolve layer_refs (recursive — nested layers)
    for lr in &variant.layer_refs {
        let layer_id_str = lr.collection_id.to_string();
        if let Some(nested_layer) = controller.load_layer(layer_id_str.as_str()).await {
            let nested = Box::pin(resolve_layer_module_chains(controller, &nested_layer)).await;
            out.extend(nested);
        }
    }

    // 2) Resolve module_refs (module presets with full signal chains)
    for mr in &variant.module_refs {
        let collection_id_str = mr.collection_id.to_string();
        let module_preset = all_module_presets
            .iter()
            .find(|p| p.id().to_string() == collection_id_str);
        let mt = module_preset.map(|p| p.module_type());
        let mc = mt
            .map(|m| m.color())
            .unwrap_or(signal::ModuleType::Drive.color());
        let module_name = module_preset
            .map(|p| p.name().to_string())
            .unwrap_or_else(|| format!("Module {}", mr.collection_id));
        let chain;
        if let Some(snapshot) = controller
            .load_module_collection_default(collection_id_str)
            .await
        {
            chain = snapshot.module().chain().clone();
        } else {
            chain = SignalChain::new(vec![]);
        }
        out.push(ModuleChainData {
            name: module_name,
            color_bg: mc.bg.to_string(),
            color_fg: mc.fg.to_string(),
            color_border: mc.border.to_string(),
            chain,
            module_type: mt,
        });
    }

    // 3) Resolve block_refs (standalone blocks → single-node chains)
    for br in &variant.block_refs {
        let preset_id_str = br.collection_id.to_string();
        let (bt, preset_name) = block_preset_lookup
            .get(&preset_id_str)
            .cloned()
            .unwrap_or((BlockType::Custom, format!("Block {}", br.collection_id)));

        let source = match &br.variant_id {
            Some(snap_id) => ModuleBlockSource::PresetSnapshot {
                preset_id: br.collection_id.clone(),
                snapshot_id: snap_id.clone(),
                saved_at_version: None,
            },
            None => ModuleBlockSource::PresetDefault {
                preset_id: br.collection_id.clone(),
                saved_at_version: None,
            },
        };
        let node = SignalNode::Block(ModuleBlock::new(
            preset_id_str.clone(),
            &preset_name,
            bt,
            source,
        ));
        let chain = SignalChain::new(vec![node]);
        let color = bt.color();
        out.push(ModuleChainData {
            name: preset_name,
            color_bg: color.bg.to_string(),
            color_fg: color.fg.to_string(),
            color_border: color.border.to_string(),
            chain,
            module_type: None,
        });
    }

    out
}

/// Build a lookup table of block preset ID → (BlockType, name).
///
/// Loads all block collections across every block type. This is cached
/// per-call since `resolve_layer_module_chains` may be called multiple
/// times for nested layers.
async fn build_block_preset_lookup(
    controller: &SignalController,
) -> std::collections::HashMap<String, (BlockType, String)> {
    let mut lookup = std::collections::HashMap::new();
    for &bt in ALL_BLOCK_TYPES {
        for preset in controller.list_collections(bt).await {
            lookup.insert(preset.id().to_string(), (bt, preset.name().to_string()));
        }
    }
    lookup
}

/// Resolve a rig scene's full hierarchy into `EngineFlowData` for grid rendering.
///
/// Walks: `RigScene.engine_selections → Engine → EngineScene.layer_selections → Layer → modules`
async fn resolve_rig_scene_engines(
    controller: &SignalController,
    scene: &signal::rig::RigScene,
) -> Vec<EngineFlowData> {
    let mut engines = Vec::new();
    for es in &scene.engine_selections {
        let engine_id_str = es.engine_id.as_str();
        let engine = match controller.load_engine(engine_id_str).await {
            Some(e) => e,
            None => continue,
        };
        // Find the selected engine variant, fall back to default
        let engine_variant = engine
            .variant(&es.variant_id)
            .or_else(|| engine.default_variant());
        let engine_variant = match engine_variant {
            Some(v) => v,
            None => continue,
        };
        let mut layers = Vec::new();
        for ls in &engine_variant.layer_selections {
            let layer_id_str = ls.layer_id.as_str();
            let layer = match controller.load_layer(layer_id_str).await {
                Some(l) => l,
                None => continue,
            };
            let module_chains = resolve_layer_module_chains(controller, &layer).await;
            layers.push(LayerFlowData {
                name: layer.name.clone(),
                module_chains,
            });
        }
        engines.push(EngineFlowData {
            name: engine.name.clone(),
            layers,
        });
    }
    engines
}

// endregion: --- Detail resolution helpers

// region: --- Parameter resolution

/// Walk all column items' detail data, collect block source references,
/// and resolve them into a parameter lookup table.
pub(super) async fn build_param_lookup(
    controller: &SignalController,
    items: &[ColumnItem],
) -> ParamLookup {
    let mut lookup = ParamLookup::new();
    for item in items {
        collect_chain_sources(&item.detail, &mut lookup, controller).await;
    }
    lookup
}

/// Collect block parameters from all chains in a DetailData tree.
async fn collect_chain_sources(
    data: &DetailData,
    lookup: &mut ParamLookup,
    controller: &SignalController,
) {
    // Walk engines → layers → module chains → chain nodes
    for engine in &data.engines {
        for layer in &engine.layers {
            for mc in &layer.module_chains {
                resolve_chain_params(&mc.chain, lookup, controller).await;
            }
        }
    }
    // Walk module_chains directly
    for mc in &data.module_chains {
        resolve_chain_params(&mc.chain, lookup, controller).await;
    }
    // Walk standalone chain
    if let Some(ref chain) = data.chain {
        resolve_chain_params(chain, lookup, controller).await;
    }
}

/// Walk a signal chain and resolve parameters for each block source.
async fn resolve_chain_params(
    chain: &SignalChain,
    lookup: &mut ParamLookup,
    controller: &SignalController,
) {
    for node in chain.nodes() {
        resolve_node_params(node, lookup, controller).await;
    }
}

async fn resolve_node_params(
    node: &signal::SignalNode,
    lookup: &mut ParamLookup,
    controller: &SignalController,
) {
    match node {
        signal::SignalNode::Block(mb) => {
            match mb.source() {
                signal::ModuleBlockSource::PresetSnapshot {
                    preset_id,
                    snapshot_id,
                    ..
                } => {
                    let key = (preset_id.to_string(), snapshot_id.to_string());
                    if !lookup.contains_key(&key) {
                        if let Some(block) = controller
                            .load_variant(mb.block_type(), preset_id.clone(), snapshot_id.clone())
                            .await
                        {
                            let params: Vec<(String, f32)> = block
                                .parameters()
                                .iter()
                                .map(|p| (p.name().to_string(), p.value().get()))
                                .collect();
                            lookup.insert(key, params);
                        }
                    }
                }
                signal::ModuleBlockSource::PresetDefault { preset_id, .. } => {
                    let key = (preset_id.to_string(), "default".to_string());
                    if !lookup.contains_key(&key) {
                        if let Some(block) = controller
                            .load_collection_default(mb.block_type(), preset_id.clone())
                            .await
                        {
                            let params: Vec<(String, f32)> = block
                                .parameters()
                                .iter()
                                .map(|p| (p.name().to_string(), p.value().get()))
                                .collect();
                            lookup.insert(key, params);
                        }
                    }
                }
                signal::ModuleBlockSource::Inline { .. } => {
                    // Inline blocks carry their params directly — handled in extract_block_params
                }
            }
        }
        signal::SignalNode::Split { lanes } => {
            for lane in lanes {
                for n in lane.nodes() {
                    Box::pin(resolve_node_params(n, lookup, controller)).await;
                }
            }
        }
    }
}

// endregion: --- Parameter resolution

// region: --- Utility

pub(super) fn rig_type_to_engine_type(rig_type: RigType) -> signal::EngineType {
    match rig_type {
        RigType::Guitar => signal::EngineType::Guitar,
        RigType::Bass => signal::EngineType::Bass,
        RigType::Keys => signal::EngineType::Keys,
        RigType::Drums | RigType::DrumReplacement => signal::EngineType::Guitar,
        RigType::Vocals => signal::EngineType::Vocal,
    }
}

// endregion: --- Utility
