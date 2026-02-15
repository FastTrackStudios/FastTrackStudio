//! Async data fetching and detail resolution for the collection browser.
//!
//! Each `fetch_col*` function queries the `SignalController` for the
//! appropriate domain entities and maps them into `ColumnItem` rows.

use signal::layer::Layer;
use signal::rig::RigType;
use signal::tagging::{StructuredTag, TagCategory, TagSet};
use signal::traits::HasMetadata;
use signal::SignalController;
use signal::{Preset, SignalChain, ALL_BLOCK_TYPES};

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
            let presets = controller.list_module_collections().await;
            presets
                .into_iter()
                .map(|p| {
                    let mut tags = TagSet::default();
                    tags.insert(StructuredTag::new(
                        TagCategory::Module,
                        p.module_type().as_str(),
                    ));
                    ColumnItem {
                        id: p.id().to_string(),
                        name: p.name().to_string(),
                        subtitle: Some(p.module_type().display_name().to_string()),
                        badge: Some(format!("{}", p.snapshots().len())),
                        metadata: None,
                        structured_tags: tags,
                        detail: DetailData::default(),
                        tag: None,
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
            let presets = controller.list_module_collections().await;
            let items = if let Some(preset) = presets.iter().find(|p| p.id().to_string() == col2_id)
            {
                let mut out = Vec::new();
                for s in preset.snapshots() {
                    let block_count = s.module().blocks().len();
                    let chain = s.module().chain().clone();
                    out.push(ColumnItem {
                        id: s.id().to_string(),
                        name: s.name().to_string(),
                        subtitle: Some(format!("{block_count} block(s)")),
                        badge: None,
                        metadata: None,
                        structured_tags: TagSet::default(),
                        detail: DetailData {
                            chain: Some(chain),
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

/// Resolve a layer's default variant module refs into `ModuleChainData` for grid rendering.
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
    let mut out = Vec::new();
    for mr in &variant.module_refs {
        let collection_id_str = mr.collection_id.to_string();
        let module_preset = all_module_presets
            .iter()
            .find(|p| p.id().to_string() == collection_id_str);
        let mt = module_preset.map(|p| p.module_type());
        let mc = mt
            .map(|m| m.color())
            .unwrap_or(signal::ModuleType::Drive.color());
        // Use the preset name (e.g. "Source", "Full Drive Stack") not the
        // snapshot name (e.g. "Default") — snapshot names are not unique across
        // modules, which causes all modules to share one group key.
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
    out
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
