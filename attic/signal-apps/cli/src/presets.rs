//! `presets` subcommand handlers — split from cli/lib.rs.

use super::*;

pub(crate) async fn cmd_presets_list(
    signal: &SignalController,
    block_type: &str,
    as_json: bool,
) -> Result<()> {
    let bt = parse_block_type(block_type)?;
    let presets = signal.block_presets().list(bt).await?;

    if as_json {
        let arr: Vec<_> = presets
            .iter()
            .map(|p| {
                json!({
                    "id": p.id().to_string(),
                    "name": p.name(),
                    "block_type": block_type,
                    "snapshot_count": p.snapshots().len(),
                })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&arr)?);
    } else {
        if presets.is_empty() {
            println!("No {} presets.", block_type);
            return Ok(());
        }
        println!("{} presets ({}):", block_type, presets.len());
        for p in &presets {
            println!(
                "  {} — {} ({} snapshots)",
                p.id(),
                p.name(),
                p.snapshots().len()
            );
        }
    }
    Ok(())
}

pub(crate) async fn cmd_presets_show(
    signal: &SignalController,
    block_type: &str,
    id: &str,
    as_json: bool,
) -> Result<()> {
    let bt = parse_block_type(block_type)?;
    let block = signal
        .block_presets()
        .load_default(bt, id.to_string())
        .await?;

    match block {
        Some(block) => {
            if as_json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&json!({
                        "id": id,
                        "block_type": block_type,
                        "block": format!("{block:?}"),
                    }))?
                );
            } else {
                println!("{} preset: {}", block_type, id);
                println!("{block:#?}");
            }
        }
        None => eyre::bail!("Preset not found: {id}"),
    }
    Ok(())
}

pub(crate) async fn cmd_presets_create(
    signal: &SignalController,
    block_type: &str,
    name: &str,
    as_json: bool,
) -> Result<()> {
    let bt = parse_block_type(block_type)?;
    let block = signal.blocks().get(bt).await?;
    let preset = signal
        .block_presets()
        .create(name.to_string(), bt, block)
        .await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "create_preset",
                "id": preset.id().to_string(),
                "name": preset.name(),
                "block_type": block_type,
                "ok": true,
            }))?
        );
    } else {
        println!(
            "created {} preset: {} ({})",
            block_type,
            preset.name(),
            preset.id()
        );
    }
    Ok(())
}

pub(crate) async fn cmd_presets_delete(
    signal: &SignalController,
    block_type: &str,
    id: &str,
    as_json: bool,
) -> Result<()> {
    let bt = parse_block_type(block_type)?;
    signal.block_presets().delete(bt, id.to_string()).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "delete_preset",
                "id": id,
                "block_type": block_type,
                "ok": true,
            }))?
        );
    } else {
        println!("deleted {} preset: {}", block_type, id);
    }
    Ok(())
}

// ============================================================================
// Command Implementations — Capture
// ============================================================================

pub(crate) async fn cmd_presets_capture(
    db: Option<PathBuf>,
    socket: Option<PathBuf>,
    block_type: &str,
    name: &str,
    variation: Option<&str>,
    track_arg: &str,
    fx_index: u32,
) -> Result<()> {
    let bt = parse_block_type(block_type)?;
    let signal = connect_signal(db).await?;
    let daw = daw_cli::connect(socket)
        .await
        .map_err(|e| eyre::eyre!("REAPER required for capture: {e}"))?;

    // Resolve track and FX
    let track = daw_cli::resolve_track_handle(&daw, track_arg).await?;
    let fx = track
        .fx_chain()
        .by_index(fx_index)
        .await?
        .ok_or_else(|| eyre::eyre!("No FX at index {fx_index}"))?;

    // Get plugin name, parameters, and binary state
    let info = fx.info().await?;
    let params = fx.parameters().await?;
    let state_bytes = fx
        .state_chunk()
        .await?
        .ok_or_else(|| eyre::eyre!("FX returned no state chunk"))?;

    let snap_name = variation.unwrap_or(name);

    eprintln!(
        "Capturing: \"{}\" from \"{}\" ({} params, {} bytes)",
        info.plugin_name,
        track_arg,
        params.len(),
        state_bytes.len()
    );

    // Build param tuples for the ops method
    let param_tuples: Vec<(u32, String, f32)> = params
        .iter()
        .map(|p| (p.index, p.name.clone(), p.value as f32))
        .collect();

    let preset = signal
        .block_presets()
        .create_from_capture(
            bt,
            name,
            snap_name,
            &info.plugin_name,
            &param_tuples,
            state_bytes,
        )
        .await?;

    eprintln!("Saved {} preset \"{}\" ({})", block_type, name, preset.id());
    Ok(())
}

pub(crate) async fn cmd_presets_recapture(
    db: Option<PathBuf>,
    socket: Option<PathBuf>,
    block_type: &str,
    preset_id_str: &str,
    snapshot_arg: Option<&str>,
    track_arg: &str,
    fx_index: u32,
) -> Result<()> {
    let bt = parse_block_type(block_type)?;
    let signal = connect_signal(db).await?;
    let daw = daw_cli::connect(socket)
        .await
        .map_err(|e| eyre::eyre!("REAPER required for recapture: {e}"))?;

    // Resolve track and FX
    let track = daw_cli::resolve_track_handle(&daw, track_arg).await?;
    let fx = track
        .fx_chain()
        .by_index(fx_index)
        .await?
        .ok_or_else(|| eyre::eyre!("No FX at index {fx_index}"))?;

    // Get parameters and binary state
    let info = fx.info().await?;
    let params = fx.parameters().await?;
    let state_bytes = fx
        .state_chunk()
        .await?
        .ok_or_else(|| eyre::eyre!("FX returned no state chunk"))?;

    // Find the preset
    let preset_id = signal::PresetId::from(preset_id_str.to_string());
    let preset = signal
        .block_presets()
        .list(bt)
        .await?
        .into_iter()
        .find(|p| *p.id() == preset_id)
        .ok_or_else(|| eyre::eyre!("Block preset not found: {preset_id_str}"))?;

    // Resolve snapshot ID
    let snapshot_id = match snapshot_arg {
        Some(s) => signal::SnapshotId::from(s.to_string()),
        None => preset.default_variant_id().clone(),
    };

    eprintln!(
        "Recapturing: \"{}\" from \"{}\" ({} params, {} bytes)",
        info.plugin_name,
        track_arg,
        params.len(),
        state_bytes.len()
    );

    let param_tuples: Vec<(u32, String, f32)> = params
        .iter()
        .map(|p| (p.index, p.name.clone(), p.value as f32))
        .collect();

    signal
        .block_presets()
        .update_snapshot_from_capture(bt, preset_id, snapshot_id, &param_tuples, state_bytes)
        .await?;

    eprintln!("Recaptured {} preset \"{}\"", block_type, preset.name());
    Ok(())
}

pub(crate) async fn cmd_presets_set_param(
    signal: &SignalController,
    block_type: &str,
    preset_id_str: &str,
    snapshot_arg: Option<&str>,
    assignment: &str,
) -> Result<()> {
    let bt = parse_block_type(block_type)?;

    // Parse "param_name=value"
    let (param_name, val_str) = assignment.split_once('=')
        .ok_or_else(|| eyre::eyre!(
            "Invalid assignment \"{assignment}\". Expected format: param_name=value (e.g. \"Mix=0.75\")"
        ))?;
    let value: f32 = val_str
        .parse()
        .map_err(|_| eyre::eyre!("Invalid value \"{val_str}\" in assignment \"{assignment}\""))?;

    // Find the preset
    let preset_id = signal::PresetId::from(preset_id_str.to_string());
    let preset = signal
        .block_presets()
        .list(bt)
        .await?
        .into_iter()
        .find(|p| *p.id() == preset_id)
        .ok_or_else(|| eyre::eyre!("Block preset not found: {preset_id_str}"))?;

    // Resolve snapshot ID
    let snapshot_id = match snapshot_arg {
        Some(s) => signal::SnapshotId::from(s.to_string()),
        None => preset.default_variant_id().clone(),
    };

    signal
        .block_presets()
        .update_snapshot_param_by_name(bt, preset_id, snapshot_id, param_name, value)
        .await?;

    eprintln!(
        "Set {}={} on {} preset \"{}\"",
        param_name,
        value,
        block_type,
        preset.name()
    );
    Ok(())
}

// ============================================================================
// Command Implementations — Import
// ============================================================================

pub(crate) async fn cmd_presets_import(signal: &SignalController, cmd: &ImportCommand) -> Result<()> {
    // Compute library root for file-based preset writing
    let library_root = utils::paths::library_dir();

    match cmd {
        ImportCommand::Fabfilter {
            plugin,
            all,
            dry_run,
        } => {
            let importer = signal::signal_import::fabfilter::FabFilterImporter::new();

            if *all {
                let plugins = importer.discover_plugins()?;
                if plugins.is_empty() {
                    println!("No FabFilter preset directories found.");
                    return Ok(());
                }
                println!("Discovered {} FabFilter plugins:", plugins.len());
                for p in &plugins {
                    let format = if p.is_text_format { "text" } else { "binary" };
                    println!(
                        "  {} — {} presets ({}, {})",
                        p.plugin_name,
                        p.preset_count,
                        p.block_type.display_name(),
                        format,
                    );
                }
                if *dry_run {
                    println!("\n[dry run] No changes made.");
                    return Ok(());
                }
                for p in &plugins {
                    let collection = importer.scan(&p.plugin_name)?;
                    let report = signal::signal_import::import_presets_with_library(
                        signal,
                        collection,
                        Some(&library_root),
                    )
                    .await?;
                    println!(
                        "  Imported {}: {} snapshots",
                        report.preset_name, report.snapshots_imported
                    );
                }
            } else if let Some(name) = plugin {
                let collection = importer.scan(name)?;
                if *dry_run {
                    print!("{}", signal::signal_import::dry_run_report(&collection));
                    println!("[dry run] No changes made.");
                    return Ok(());
                }
                let report = signal::signal_import::import_presets_with_library(
                    signal,
                    collection,
                    Some(&library_root),
                )
                .await?;
                println!(
                    "Imported {}: {} snapshots",
                    report.preset_name, report.snapshots_imported
                );
            } else {
                eyre::bail!("Specify --plugin <name> or --all");
            }
            Ok(())
        }
        ImportCommand::Rfxchain {
            source,
            block_type,
            name,
            dry_run,
        } => {
            let bt = parse_block_type(block_type)?;
            let collection = signal::signal_import::rfxchain::RfxChainImporter::scan(
                source,
                bt,
                name.as_deref(),
            )?;
            if *dry_run {
                print!("{}", signal::signal_import::dry_run_report(&collection));
                println!("[dry run] No changes made.");
                return Ok(());
            }
            let report = signal::signal_import::import_presets_with_library(
                signal,
                collection,
                Some(&library_root),
            )
            .await?;
            println!(
                "Imported {}: {} snapshots",
                report.preset_name, report.snapshots_imported
            );
            Ok(())
        }
    }
}

// ============================================================================
// Command Implementations — Modules
// ============================================================================

