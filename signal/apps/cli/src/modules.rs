//! `modules` subcommand handlers — split from cli/lib.rs.

use super::*;

pub(crate) async fn cmd_modules_list(signal: &SignalController, as_json: bool) -> Result<()> {
    let modules = signal.module_presets().list().await?;

    if as_json {
        let arr: Vec<_> = modules
            .iter()
            .map(|m| {
                json!({
                    "id": m.id().to_string(),
                    "name": m.name(),
                })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&arr)?);
    } else {
        if modules.is_empty() {
            println!("No module presets.");
            return Ok(());
        }
        println!("Module presets ({}):", modules.len());
        for m in &modules {
            println!("  {} — {}", m.id(), m.name());
        }
    }
    Ok(())
}

pub(crate) async fn cmd_modules_show(signal: &SignalController, id: &str, as_json: bool) -> Result<()> {
    let snapshot = signal.module_presets().load_default(id.to_string()).await?;

    match snapshot {
        Some(snap) => {
            if as_json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&json!({
                        "id": id,
                        "snapshot": format!("{snap:?}"),
                    }))?
                );
            } else {
                println!("Module preset: {}", id);
                println!("{snap:#?}");
            }
        }
        None => eyre::bail!("Module preset not found: {id}"),
    }
    Ok(())
}

pub(crate) async fn cmd_modules_create(
    signal: &SignalController,
    module_type: &str,
    name: &str,
    block_specs: &[String],
) -> Result<()> {
    let mt = parse_module_type(module_type)?;

    // Parse block specs: "block_type:preset_id" pairs → (BlockType, PresetId, label)
    let mut blocks = Vec::new();
    for spec in block_specs {
        let (bt_str, preset_id_str) = spec.split_once(':')
            .ok_or_else(|| eyre::eyre!(
                "Invalid block spec \"{spec}\". Expected format: block_type:preset_id (e.g. amp:abc123)"
            ))?;

        let bt = parse_block_type(bt_str)?;
        let preset_id = signal::PresetId::from(preset_id_str.to_string());

        // Look up label for display
        let label = signal
            .block_presets()
            .list(bt)
            .await?
            .into_iter()
            .find(|p| *p.id() == preset_id)
            .map(|p| p.name().to_string())
            .unwrap_or_else(|| preset_id_str.to_string());

        eprintln!("  [{}] {} → \"{}\"", blocks.len(), bt.display_name(), label);
        blocks.push((bt, preset_id, label));
    }

    if blocks.is_empty() {
        eyre::bail!("At least one block spec is required");
    }

    let preset = signal.module_presets().create(name, mt, blocks).await?;
    eprintln!(
        "Saved {} module preset \"{}\" ({})",
        module_type,
        name,
        preset.id()
    );
    Ok(())
}

pub(crate) async fn cmd_modules_add_variation(
    signal: &SignalController,
    preset_id_str: &str,
    name: &str,
    override_specs: &[String],
) -> Result<()> {
    let preset_id = signal::ModulePresetId::from(preset_id_str.to_string());

    // Load the module preset
    let preset = signal
        .module_presets()
        .list()
        .await?
        .into_iter()
        .find(|p| *p.id() == preset_id)
        .ok_or_else(|| eyre::eyre!("Module preset not found: {preset_id_str}"))?;

    // Get blocks from the default snapshot
    let default_snapshot = preset
        .default_variant()
        .ok_or_else(|| eyre::eyre!("Module preset has no default snapshot"))?;
    let source_blocks = default_snapshot.module().blocks();

    // Parse overrides: "block_id:param_name=value"
    let mut parsed_overrides: Vec<(String, String, f32)> = Vec::new();
    for spec in override_specs {
        let (block_id, rest) = spec.split_once(':').ok_or_else(|| {
            eyre::eyre!("Invalid override \"{spec}\". Expected format: block_id:param_name=value")
        })?;
        let (param_name, val_str) = rest.split_once('=').ok_or_else(|| {
            eyre::eyre!("Invalid override \"{spec}\". Expected format: block_id:param_name=value")
        })?;
        let value: f32 = val_str
            .parse()
            .map_err(|_| eyre::eyre!("Invalid value \"{val_str}\" in override \"{spec}\""))?;
        parsed_overrides.push((block_id.to_string(), param_name.to_string(), value));
    }

    // Rebuild blocks with overrides applied
    let rebuilt_blocks: Vec<signal::ModuleBlock> = source_blocks
        .into_iter()
        .map(|block| {
            let overrides_for_block: Vec<signal::BlockParameterOverride> = parsed_overrides
                .iter()
                .filter(|(bid, _, _)| bid == block.id())
                .map(|(_, param, val)| signal::BlockParameterOverride::new(param, *val))
                .collect();

            let mut new_block = signal::ModuleBlock::new(
                block.id(),
                block.label(),
                block.block_type(),
                block.source().clone(),
            );
            if !overrides_for_block.is_empty() {
                new_block = new_block.with_overrides(overrides_for_block);
            }
            new_block
        })
        .collect();

    let module = signal::Module::from_blocks(rebuilt_blocks);
    let snapshot = signal::ModuleSnapshot::new(signal::ModuleSnapshotId::new(), name, module);

    signal
        .module_presets()
        .add_snapshot(preset_id, snapshot)
        .await?;
    eprintln!(
        "Added variation \"{}\" to module preset {}",
        name, preset_id_str
    );
    Ok(())
}

pub(crate) async fn cmd_modules_edit_variation(
    signal: &SignalController,
    preset_id_str: &str,
    snapshot_id_str: &str,
    override_specs: &[String],
    block_specs: &[String],
) -> Result<()> {
    let preset_id = signal::ModulePresetId::from(preset_id_str.to_string());
    let snapshot_id = signal::ModuleSnapshotId::from(snapshot_id_str.to_string());

    // Load the module preset and find the target snapshot
    let preset = signal
        .module_presets()
        .list()
        .await?
        .into_iter()
        .find(|p| *p.id() == preset_id)
        .ok_or_else(|| eyre::eyre!("Module preset not found: {preset_id_str}"))?;

    let snapshot = preset
        .variants()
        .iter()
        .find(|s| *s.id() == snapshot_id)
        .ok_or_else(|| eyre::eyre!("Snapshot not found: {snapshot_id_str}"))?;

    let source_blocks = snapshot.module().blocks();

    // Parse overrides: "block_id:param_name=value"
    let mut parsed_overrides: Vec<(String, String, f32)> = Vec::new();
    for spec in override_specs {
        let (block_id, rest) = spec.split_once(':').ok_or_else(|| {
            eyre::eyre!("Invalid override \"{spec}\". Expected format: block_id:param_name=value")
        })?;
        let (param_name, val_str) = rest.split_once('=').ok_or_else(|| {
            eyre::eyre!("Invalid override \"{spec}\". Expected format: block_id:param_name=value")
        })?;
        let value: f32 = val_str
            .parse()
            .map_err(|_| eyre::eyre!("Invalid value \"{val_str}\" in override \"{spec}\""))?;
        parsed_overrides.push((block_id.to_string(), param_name.to_string(), value));
    }

    // Parse block source reassignments: "block_id:block_type:preset_id"
    let mut parsed_blocks: Vec<(String, signal::BlockType, signal::PresetId)> = Vec::new();
    for spec in block_specs {
        let parts: Vec<&str> = spec.splitn(3, ':').collect();
        if parts.len() != 3 {
            eyre::bail!(
                "Invalid block spec \"{spec}\". Expected format: block_id:block_type:preset_id"
            );
        }
        let bt = parse_block_type(parts[1])?;
        let pid = signal::PresetId::from(parts[2].to_string());
        parsed_blocks.push((parts[0].to_string(), bt, pid));
    }

    // Rebuild blocks with overrides and source reassignments applied
    let rebuilt_blocks: Vec<signal::ModuleBlock> = source_blocks
        .into_iter()
        .map(|block| {
            // Check for source reassignment
            let source = if let Some((_, _, ref pid)) =
                parsed_blocks.iter().find(|(bid, _, _)| bid == block.id())
            {
                signal::ModuleBlockSource::PresetDefault {
                    preset_id: pid.clone(),
                    saved_at_version: None,
                }
            } else {
                block.source().clone()
            };

            // Check for block type reassignment
            let block_type = if let Some((_, bt, _)) =
                parsed_blocks.iter().find(|(bid, _, _)| bid == block.id())
            {
                *bt
            } else {
                block.block_type()
            };

            // Check for overrides
            let overrides_for_block: Vec<signal::BlockParameterOverride> = parsed_overrides
                .iter()
                .filter(|(bid, _, _)| bid == block.id())
                .map(|(_, param, val)| signal::BlockParameterOverride::new(param, *val))
                .collect();

            let mut new_block =
                signal::ModuleBlock::new(block.id(), block.label(), block_type, source);
            if !overrides_for_block.is_empty() {
                new_block = new_block.with_overrides(overrides_for_block);
            }
            new_block
        })
        .collect();

    let module = signal::Module::from_blocks(rebuilt_blocks);
    signal
        .module_presets()
        .update_snapshot_module(preset_id, snapshot_id, module)
        .await?;

    eprintln!(
        "Updated variation \"{}\" on module preset {}",
        snapshot_id_str, preset_id_str
    );
    Ok(())
}

// ============================================================================
// Command Implementations — Layers
// ============================================================================

