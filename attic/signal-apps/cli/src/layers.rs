//! `layers` subcommand handlers — split from cli/lib.rs.

use super::*;

pub(crate) async fn cmd_layers_list(signal: &SignalController, as_json: bool) -> Result<()> {
    let layers = signal.layers().list().await?;

    if as_json {
        let arr: Vec<_> = layers
            .iter()
            .map(|l| {
                json!({
                    "id": l.id.to_string(),
                    "name": l.name,
                    "variant_count": l.variants.len(),
                })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&arr)?);
    } else {
        if layers.is_empty() {
            println!("No layers.");
            return Ok(());
        }
        println!("Layers ({}):", layers.len());
        for l in &layers {
            println!("  {} — {} ({} variants)", l.id, l.name, l.variants.len());
        }
    }
    Ok(())
}

pub(crate) async fn cmd_layers_show(signal: &SignalController, id: &str, as_json: bool) -> Result<()> {
    let layer = signal.layers().load(id.to_string()).await?;
    match layer {
        Some(l) => {
            // Load default snapshot to show block_refs, module_refs
            let snapshot = signal
                .layers()
                .load_variant(l.id.clone(), l.default_variant_id.clone())
                .await?;

            if as_json {
                let mut obj = json!({
                    "id": l.id.to_string(),
                    "name": l.name,
                    "engine_type": l.engine_type.as_str(),
                    "variants": l.variants.iter().map(|v| json!({
                        "id": v.id.to_string(),
                        "name": v.name,
                    })).collect::<Vec<_>>(),
                });
                if let Some(ref snap) = snapshot {
                    obj["block_refs"] = json!(
                        snap.block_refs
                            .iter()
                            .map(|br| json!({
                                "collection_id": br.collection_id.to_string(),
                                "variant_id": br.variant_id.as_ref().map(|v| v.to_string()),
                            }))
                            .collect::<Vec<_>>()
                    );
                    obj["module_refs"] = json!(
                        snap.module_refs
                            .iter()
                            .map(|mr| json!({
                                "collection_id": mr.collection_id.to_string(),
                                "variant_id": mr.variant_id.as_ref().map(|v| v.to_string()),
                            }))
                            .collect::<Vec<_>>()
                    );
                }
                println!("{}", serde_json::to_string_pretty(&obj)?);
            } else {
                println!("Layer: {} ({}) [{:?}]", l.name, l.id, l.engine_type);
                println!("  Variants:");
                for v in &l.variants {
                    let is_default = v.id == l.default_variant_id;
                    println!(
                        "    {} {} — {}",
                        if is_default { "*" } else { " " },
                        v.id,
                        v.name,
                    );
                }
                if let Some(snap) = snapshot {
                    if !snap.block_refs.is_empty() {
                        println!("  Block refs (default snapshot):");
                        for br in &snap.block_refs {
                            // Try to look up the preset name
                            let name = lookup_preset_name(signal, &br.collection_id).await;
                            println!("    - {} ({})", name, br.collection_id);
                        }
                    }
                    if !snap.module_refs.is_empty() {
                        println!("  Module refs (default snapshot):");
                        for mr in &snap.module_refs {
                            println!("    - {}", mr.collection_id);
                        }
                    }
                    if !snap.plugin_refs.is_empty() {
                        println!("  Plugin refs (default snapshot):");
                        for pr in &snap.plugin_refs {
                            println!("    - {:?}", pr.def);
                        }
                    }
                }
            }
        }
        None => eyre::bail!("Layer not found: {id}"),
    }
    Ok(())
}

/// Try to find a human-readable name for a block preset by checking all block types.
pub(crate) async fn lookup_preset_name(signal: &SignalController, preset_id: &signal::PresetId) -> String {
    // Try common block types
    for bt in &[
        signal::BlockType::Amp,
        signal::BlockType::Drive,
        signal::BlockType::Eq,
        signal::BlockType::Reverb,
        signal::BlockType::Delay,
        signal::BlockType::Compressor,
        signal::BlockType::Gate,
        signal::BlockType::Chorus,
        signal::BlockType::Flanger,
        signal::BlockType::Phaser,
        signal::BlockType::Trem,
        signal::BlockType::Cabinet,
        signal::BlockType::Boost,
        signal::BlockType::Saturator,
        signal::BlockType::Limiter,
        signal::BlockType::Volume,
    ] {
        if let Ok(presets) = signal.block_presets().list(*bt).await {
            if let Some(p) = presets.iter().find(|p| p.id() == preset_id) {
                return p.name().to_string();
            }
        }
    }
    preset_id.to_string()
}

pub(crate) fn parse_engine_type(s: &str) -> Result<signal::EngineType> {
    match s.to_lowercase().as_str() {
        "guitar" => Ok(signal::EngineType::Guitar),
        "bass" => Ok(signal::EngineType::Bass),
        "vocal" | "vocals" => Ok(signal::EngineType::Vocal),
        "keys" => Ok(signal::EngineType::Keys),
        "synth" => Ok(signal::EngineType::Synth),
        "organ" => Ok(signal::EngineType::Organ),
        "pad" => Ok(signal::EngineType::Pad),
        _ => eyre::bail!(
            "Unknown engine type: \"{s}\". Valid: guitar, bass, vocals, keys, synth, organ, pad"
        ),
    }
}

pub(crate) async fn cmd_layers_create(
    signal: &SignalController,
    name: &str,
    type_str: &str,
    as_json: bool,
) -> Result<()> {
    let engine_type = parse_engine_type(type_str)?;
    let layer = signal
        .layers()
        .create(name.to_string(), engine_type)
        .await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "create_layer",
                "id": layer.id.to_string(),
                "name": layer.name,
                "ok": true,
            }))?
        );
    } else {
        println!("created layer: {} ({})", layer.name, layer.id);
    }
    Ok(())
}

pub(crate) async fn cmd_layers_delete(signal: &SignalController, id: &str, as_json: bool) -> Result<()> {
    signal.layers().delete(id.to_string()).await?;
    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "delete_layer",
                "id": id,
                "ok": true,
            }))?
        );
    } else {
        println!("deleted layer: {}", id);
    }
    Ok(())
}

pub(crate) async fn cmd_layers_add_block(
    signal: &SignalController,
    layer_id: &str,
    preset_id: &str,
    variant_id: Option<&str>,
    as_json: bool,
) -> Result<()> {
    let lid = signal::layer::LayerId::from(layer_id.to_string());
    let pid = signal::PresetId::from(preset_id.to_string());

    let layer = signal
        .layers()
        .load(lid.clone())
        .await?
        .ok_or_else(|| eyre::eyre!("Layer not found: {layer_id}"))?;

    let mut snapshot = signal
        .layers()
        .load_variant(lid.clone(), layer.default_variant_id.clone())
        .await?
        .ok_or_else(|| eyre::eyre!("Default snapshot not found for layer {layer_id}"))?;

    let block_ref = if let Some(vid) = variant_id {
        signal::layer::BlockRef::new(pid.clone())
            .with_variant(signal::SnapshotId::from(vid.to_string()))
    } else {
        signal::layer::BlockRef::new(pid.clone())
    };
    snapshot.block_refs.push(block_ref);

    signal.layers().save_variant(lid.clone(), snapshot).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "add_block",
                "layer_id": layer_id,
                "preset_id": preset_id,
                "ok": true,
            }))?
        );
    } else {
        println!("added block {} to layer {}", preset_id, layer.name);
    }
    Ok(())
}

pub(crate) async fn cmd_layers_remove_block(
    signal: &SignalController,
    layer_id: &str,
    preset_id: &str,
    as_json: bool,
) -> Result<()> {
    let lid = signal::layer::LayerId::from(layer_id.to_string());
    let pid = signal::PresetId::from(preset_id.to_string());

    let layer = signal
        .layers()
        .load(lid.clone())
        .await?
        .ok_or_else(|| eyre::eyre!("Layer not found: {layer_id}"))?;

    let mut snapshot = signal
        .layers()
        .load_variant(lid.clone(), layer.default_variant_id.clone())
        .await?
        .ok_or_else(|| eyre::eyre!("Default snapshot not found for layer {layer_id}"))?;

    let before = snapshot.block_refs.len();
    snapshot.block_refs.retain(|br| br.collection_id != pid);
    let removed = before - snapshot.block_refs.len();

    if removed == 0 {
        eyre::bail!("Block {} not found in layer {}", preset_id, layer.name);
    }

    signal.layers().save_variant(lid.clone(), snapshot).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "remove_block",
                "layer_id": layer_id,
                "preset_id": preset_id,
                "ok": true,
            }))?
        );
    } else {
        println!("removed block {} from layer {}", preset_id, layer.name);
    }
    Ok(())
}

// ============================================================================
// Command Implementations — Engines
// ============================================================================

