//! `engines` subcommand handlers — split from cli/lib.rs.

use super::*;

pub(crate) async fn cmd_engines_list(signal: &SignalController, as_json: bool) -> Result<()> {
    let engines = signal.engines().list().await?;

    if as_json {
        let arr: Vec<_> = engines
            .iter()
            .map(|e| {
                json!({
                    "id": e.id.to_string(),
                    "name": e.name,
                    "variant_count": e.variants.len(),
                })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&arr)?);
    } else {
        if engines.is_empty() {
            println!("No engines.");
            return Ok(());
        }
        println!("Engines ({}):", engines.len());
        for e in &engines {
            println!("  {} — {} ({} scenes)", e.id, e.name, e.variants.len());
        }
    }
    Ok(())
}

pub(crate) async fn cmd_engines_show(signal: &SignalController, id: &str, as_json: bool) -> Result<()> {
    let engine = signal.engines().load(id.to_string()).await?;
    match engine {
        Some(e) => {
            // Resolve layer names
            let mut layer_info = Vec::new();
            for lid in &e.layer_ids {
                let name = if let Some(l) = signal.layers().load(lid.clone()).await? {
                    l.name
                } else {
                    format!("(missing: {})", lid)
                };
                layer_info.push((lid.to_string(), name));
            }

            if as_json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&json!({
                        "id": e.id.to_string(),
                        "name": e.name,
                        "engine_type": e.engine_type.as_str(),
                        "layers": layer_info.iter().map(|(id, name)| json!({
                            "id": id,
                            "name": name,
                        })).collect::<Vec<_>>(),
                        "scenes": e.variants.iter().map(|v| json!({
                            "id": v.id.to_string(),
                            "name": v.name,
                            "layer_selections": v.layer_selections.iter().map(|s| json!({
                                "layer_id": s.layer_id.to_string(),
                                "variant_id": s.variant_id.to_string(),
                            })).collect::<Vec<_>>(),
                        })).collect::<Vec<_>>(),
                    }))?
                );
            } else {
                println!("Engine: {} ({}) [{}]", e.name, e.id, e.engine_type.as_str());
                println!("  Layers:");
                for (lid, name) in &layer_info {
                    println!("    - {} ({})", name, lid);
                }
                for v in &e.variants {
                    let is_default = v.id == e.default_variant_id;
                    println!(
                        "  {} Scene: {} — {}",
                        if is_default { "*" } else { " " },
                        v.name,
                        v.id,
                    );
                    for sel in &v.layer_selections {
                        println!("      Layer {} → snapshot {}", sel.layer_id, sel.variant_id);
                    }
                }
            }
        }
        None => eyre::bail!("Engine not found: {id}"),
    }
    Ok(())
}

pub(crate) async fn cmd_engines_create(
    signal: &SignalController,
    name: &str,
    type_str: &str,
    layer_ids_str: &[String],
    as_json: bool,
) -> Result<()> {
    let engine_type = parse_engine_type(type_str)?;

    // Parse and validate layer IDs
    let mut layer_ids = Vec::new();
    let mut layer_selections = Vec::new();
    for lid_str in layer_ids_str {
        let lid = signal::layer::LayerId::from(lid_str.to_string());
        let layer = signal
            .layers()
            .load(lid.clone())
            .await?
            .ok_or_else(|| eyre::eyre!("Layer not found: {lid_str}"))?;
        layer_selections.push(signal::engine::LayerSelection::new(
            lid.clone(),
            layer.default_variant_id.clone(),
        ));
        layer_ids.push(lid);
    }

    let mut engine = signal
        .engines()
        .create(name.to_string(), engine_type, layer_ids)
        .await?;

    // Wire layer selections into the default scene
    if !layer_selections.is_empty() {
        for scene in &mut engine.variants {
            for sel in &layer_selections {
                if !scene
                    .layer_selections
                    .iter()
                    .any(|s| s.layer_id == sel.layer_id)
                {
                    scene.layer_selections.push(sel.clone());
                }
            }
        }
        engine = signal.engines().save(engine).await?;
    }

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "create_engine",
                "id": engine.id.to_string(),
                "name": engine.name,
                "ok": true,
            }))?
        );
    } else {
        println!("created engine: {} ({})", engine.name, engine.id);
    }
    Ok(())
}

pub(crate) async fn cmd_engines_delete(signal: &SignalController, id: &str, as_json: bool) -> Result<()> {
    signal.engines().delete(id.to_string()).await?;
    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "delete_engine",
                "id": id,
                "ok": true,
            }))?
        );
    } else {
        println!("deleted engine: {}", id);
    }
    Ok(())
}

pub(crate) async fn cmd_engines_add_layer(
    signal: &SignalController,
    engine_id: &str,
    layer_id: &str,
    as_json: bool,
) -> Result<()> {
    let eid = signal::engine::EngineId::from(engine_id.to_string());
    let lid = signal::layer::LayerId::from(layer_id.to_string());

    let mut engine = signal
        .engines()
        .load(eid.clone())
        .await?
        .ok_or_else(|| eyre::eyre!("Engine not found: {engine_id}"))?;
    let layer = signal
        .layers()
        .load(lid.clone())
        .await?
        .ok_or_else(|| eyre::eyre!("Layer not found: {layer_id}"))?;

    engine.layer_ids.push(lid.clone());

    let selection = signal::engine::LayerSelection::new(lid, layer.default_variant_id.clone());
    for scene in &mut engine.variants {
        scene.layer_selections.push(selection.clone());
    }

    signal.engines().save(engine).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "add_layer",
                "engine_id": engine_id,
                "layer_id": layer_id,
                "ok": true,
            }))?
        );
    } else {
        println!("added layer {} to engine {}", layer.name, engine_id);
    }
    Ok(())
}

pub(crate) async fn cmd_engines_remove_layer(
    signal: &SignalController,
    engine_id: &str,
    layer_id: &str,
    as_json: bool,
) -> Result<()> {
    let eid = signal::engine::EngineId::from(engine_id.to_string());
    let lid = signal::layer::LayerId::from(layer_id.to_string());

    let mut engine = signal
        .engines()
        .load(eid.clone())
        .await?
        .ok_or_else(|| eyre::eyre!("Engine not found: {engine_id}"))?;

    let before = engine.layer_ids.len();
    engine.layer_ids.retain(|l| *l != lid);
    if engine.layer_ids.len() == before {
        eyre::bail!("Layer {} not found in engine {}", layer_id, engine.name);
    }

    for scene in &mut engine.variants {
        scene.layer_selections.retain(|s| s.layer_id != lid);
    }

    signal.engines().save(engine).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "remove_layer",
                "engine_id": engine_id,
                "layer_id": layer_id,
                "ok": true,
            }))?
        );
    } else {
        println!("removed layer {} from engine {}", layer_id, engine_id);
    }
    Ok(())
}

// ============================================================================
// Command Implementations — Rigs
// ============================================================================

