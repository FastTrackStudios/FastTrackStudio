//! `nam` subcommand handlers — split from cli/lib.rs.

use super::*;

pub(crate) async fn cmd_nam_packs(vendor: Option<&str>, category: Option<&str>) -> Result<()> {
    let nam_root = signal::signal_nam::nam_root_from_env(&expand_tilde(DEFAULT_NAM_ROOT));
    let packs_dir = nam_root.join("packs");

    let packs = signal::signal_nam::pack::load_packs(&packs_dir)
        .map_err(|e| eyre::eyre!("Failed to load packs: {e}"))?;

    let cat_filter = category
        .map(|c| match c.to_lowercase().as_str() {
            "amp" => Ok(signal::signal_nam::PackCategory::Amp),
            "drive" => Ok(signal::signal_nam::PackCategory::Drive),
            "ir" => Ok(signal::signal_nam::PackCategory::Ir),
            "archetype" => Ok(signal::signal_nam::PackCategory::Archetype),
            _ => Err(eyre::eyre!(
                "Unknown category: {c}. Valid: amp, drive, ir, archetype"
            )),
        })
        .transpose()?;

    let filtered: Vec<_> = packs
        .into_iter()
        .filter(|p| {
            if let Some(ref v) = vendor {
                if !p.vendor.to_lowercase().contains(&v.to_lowercase()) {
                    return false;
                }
            }
            if let Some(ref c) = cat_filter {
                if p.category != *c {
                    return false;
                }
            }
            true
        })
        .collect();

    if filtered.is_empty() {
        println!("No packs found.");
        return Ok(());
    }

    println!("NAM packs ({}):", filtered.len());
    for p in &filtered {
        let file_count = p.files.len();
        let gear = p.gear_model.as_deref().unwrap_or("-");
        println!(
            "  {} — {} [{}] vendor={} files={} gear={}",
            p.id,
            p.label,
            p.category.as_str(),
            p.vendor,
            file_count,
            gear,
        );
    }
    Ok(())
}

/// Capture the full REAPER state chunk for a NAM FX instance.
///
/// Loads the NAM plugin, injects the model path into its state, then reads
/// back the complete REAPER chunk. This produces a portable, host-validated
/// state representation rather than storing raw file paths.
pub(crate) async fn nam_capture_state(fx: &daw::rpc::FxHandle, model_path: &str) -> Result<String> {
    let reaper_chunk = fx
        .state_chunk_encoded()
        .await?
        .ok_or_else(|| eyre::eyre!("FX has no default chunk"))?;
    let segments = signal::signal_nam::extract_state_base64(&reaper_chunk)
        .ok_or_else(|| eyre::eyre!("Failed to extract base64 from chunk"))?;
    let unified_b64 = signal::signal_nam::first_base64_segment(&segments);
    let mut nam_chunk = signal::signal_nam::decode_chunk(unified_b64.trim())
        .map_err(|e| eyre::eyre!("Failed to decode NAM chunk: {e}"))?;
    signal::signal_nam::rewrite_paths(&mut nam_chunk, Some(model_path), None);
    let new_b64 = signal::signal_nam::encode_chunk(&nam_chunk);
    let rebuilt = signal::signal_nam::rebuild_chunk_with_state(&reaper_chunk, &new_b64);
    fx.set_state_chunk_encoded(rebuilt)
        .await
        .map_err(|e| eyre::eyre!("Failed to set chunk: {e}"))?;
    // Read back the final state after REAPER has processed it
    fx.state_chunk_encoded()
        .await?
        .ok_or_else(|| eyre::eyre!("No state after injection"))
}

/// Filter packs by vendor/category for NAM import.
pub(crate) fn filter_nam_packs(
    packs_dir: &Path,
    vendor: Option<&str>,
    category: Option<&str>,
) -> Result<Vec<signal::signal_nam::PackDefinition>> {
    let packs = signal::signal_nam::pack::load_packs(packs_dir)
        .map_err(|e| eyre::eyre!("Failed to load packs: {e}"))?;

    let cat_filter = category
        .map(|c| match c.to_lowercase().as_str() {
            "amp" => Ok(signal::signal_nam::PackCategory::Amp),
            "drive" => Ok(signal::signal_nam::PackCategory::Drive),
            _ => Err(eyre::eyre!(
                "Unknown category for import: {c}. Valid: amp, drive"
            )),
        })
        .transpose()?;

    Ok(packs
        .into_iter()
        .filter(|p| {
            if let Some(ref v) = vendor {
                if !p.vendor.to_lowercase().contains(&v.to_lowercase()) {
                    return false;
                }
            }
            if let Some(ref c) = cat_filter {
                if p.category != *c {
                    return false;
                }
            }
            matches!(
                p.category,
                signal::signal_nam::PackCategory::Amp | signal::signal_nam::PackCategory::Drive
            )
        })
        .collect())
}

/// Collect (tone, filename) pairs from a pack definition.
pub(crate) fn collect_tone_files(pack: &signal::signal_nam::PackDefinition) -> Vec<(String, String)> {
    let is_amp = pack.category == signal::signal_nam::PackCategory::Amp;
    let mut tone_files: Vec<(String, String)> = Vec::new();

    if is_amp {
        for (filename, file_override) in &pack.files {
            if let Some(ref tone) = file_override.tone {
                tone_files.push((tone.clone(), filename.clone()));
            }
        }
        tone_files.sort_by(|a, b| tone_sort_key(&a.0).cmp(&tone_sort_key(&b.0)));
    } else {
        for (filename, file_override) in &pack.files {
            let tone = file_override
                .tone
                .clone()
                .or_else(|| pack.default_tone.clone())
                .unwrap_or_else(|| filename_to_tone(filename));
            tone_files.push((tone, filename.clone()));
        }
        tone_files.sort_by(|a, b| a.0.cmp(&b.0));
    }

    tone_files
}

pub(crate) const NAM_PLUGIN_NAME: &str = "VST3: NeuralAmpModeler (Steven Atkinson)";

/// Dry-run NAM import: prints what would be imported without REAPER or DB changes.
pub(crate) async fn cmd_nam_import_dry_run(vendor: Option<&str>, category: Option<&str>) -> Result<()> {
    let nam_root = signal::signal_nam::nam_root_from_env(&expand_tilde(DEFAULT_NAM_ROOT));
    let packs_dir = nam_root.join("packs");
    let filtered = filter_nam_packs(&packs_dir, vendor, category)?;

    if filtered.is_empty() {
        println!("No importable packs found.");
        return Ok(());
    }

    let mut total_presets = 0;
    let mut total_snapshots = 0;

    for pack in &filtered {
        let tone_files = collect_tone_files(pack);
        if tone_files.is_empty() {
            continue;
        }

        let is_amp = pack.category == signal::signal_nam::PackCategory::Amp;
        let category_prefix = if is_amp { "nam-amp" } else { "nam-drive" };
        let preset_id = signal::seed_id(&format!("{}-{}", category_prefix, pack.id));
        let gear_model = pack.gear_model.as_deref().unwrap_or(&pack.label);
        let preset_name = format!("{} [NAM]", gear_model);
        let snap_count = tone_files.len();

        println!(
            "  [dry run] {} — {} ({} snapshots: {})",
            preset_id,
            preset_name,
            snap_count,
            tone_files
                .iter()
                .map(|(t, _)| capitalize(t))
                .collect::<Vec<_>>()
                .join(", "),
        );

        total_presets += 1;
        total_snapshots += snap_count;
    }

    println!(
        "\n[dry run] Would import {} presets ({} total snapshots). No changes made.",
        total_presets, total_snapshots,
    );

    Ok(())
}

/// Live NAM import: loads each model in REAPER, captures real state chunks.
pub(crate) async fn cmd_nam_import(
    signal: &SignalController,
    daw: &Daw,
    vendor: Option<&str>,
    category: Option<&str>,
) -> Result<()> {
    let nam_root = signal::signal_nam::nam_root_from_env(&expand_tilde(DEFAULT_NAM_ROOT));
    let packs_dir = nam_root.join("packs");
    let filtered = filter_nam_packs(&packs_dir, vendor, category)?;

    if filtered.is_empty() {
        println!("No importable packs found.");
        return Ok(());
    }

    // Create a scratch track for loading NAM instances
    let project = daw.current_project().await?;
    let scratch_track = project.tracks().add("__nam_import__", None).await?;

    let mut total_presets = 0;
    let mut total_snapshots = 0;

    for pack in &filtered {
        let tone_files = collect_tone_files(pack);
        if tone_files.is_empty() {
            continue;
        }

        let is_amp = pack.category == signal::signal_nam::PackCategory::Amp;
        let category_prefix = if is_amp { "nam-amp" } else { "nam-drive" };
        let block_type = if is_amp {
            signal::BlockType::Amp
        } else {
            signal::BlockType::Drive
        };

        let preset_id = signal::seed_id(&format!("{}-{}", category_prefix, pack.id));
        let gear_model = pack.gear_model.as_deref().unwrap_or(&pack.label);
        let preset_name = format!("{} [NAM]", gear_model);

        // Build snapshots by loading each tone in REAPER
        let mut snapshots: Vec<signal::Snapshot> = Vec::new();

        for (tone, filename) in &tone_files {
            let snap_id = signal::seed_id(&format!("{}-{}-{}", category_prefix, pack.id, tone));
            let path = resolve_nam_path(&nam_root, pack, filename);

            let path_str = match path {
                Some(p) => p,
                None => {
                    eprintln!(
                        "  warning: {} not found, skipping tone '{}'",
                        filename, tone
                    );
                    continue;
                }
            };

            // Add NAM FX, capture state, then remove
            let block = signal::Block::from_parameters(nam_block_params());
            let snapshot = match async {
                let fx = scratch_track
                    .fx_chain()
                    .add(NAM_PLUGIN_NAME)
                    .await
                    .map_err(|e| eyre::eyre!("Failed to add NAM FX: {e}"))?;

                let chunk_text = nam_capture_state(&fx, &path_str).await?;
                let state_data = chunk_text.into_bytes();

                fx.remove()
                    .await
                    .map_err(|e| eyre::eyre!("Failed to remove FX: {e}"))?;

                Ok::<_, eyre::Report>(
                    signal::Snapshot::new(
                        signal::SnapshotId::from(snap_id.to_string()),
                        capitalize(tone),
                        block,
                    )
                    .with_state_data(state_data),
                )
            }
            .await
            {
                Ok(s) => s,
                Err(e) => {
                    eprintln!(
                        "  warning: failed to capture '{}' ({}): {}",
                        tone, filename, e
                    );
                    continue;
                }
            };

            snapshots.push(snapshot);
        }

        if snapshots.is_empty() {
            eprintln!(
                "  skipping {} — no tones captured successfully",
                preset_name
            );
            continue;
        }

        let default_snapshot = snapshots.remove(0);
        let snap_count = 1 + snapshots.len();

        let metadata =
            signal::metadata::Metadata::new().with_tag(format!("source:{}", NAM_PLUGIN_NAME));

        let preset = signal::Preset::new(
            signal::PresetId::from(preset_id.to_string()),
            preset_name.clone(),
            block_type,
            default_snapshot,
            snapshots,
        )
        .with_metadata(metadata);

        signal.block_presets().save(preset).await?;
        println!(
            "  imported: {} — {} ({} snapshots)",
            preset_id, preset_name, snap_count,
        );

        total_presets += 1;
        total_snapshots += snap_count;
    }

    // Clean up scratch track
    project
        .tracks()
        .remove(daw::service::TrackRef::Guid(
            scratch_track.guid().to_string(),
        ))
        .await?;

    println!(
        "\nImported {} presets ({} total snapshots).",
        total_presets, total_snapshots,
    );

    Ok(())
}

/// Resolve a NAM file path: {nam_root}/{category_dir}/{pack_directory}/{filename}
pub(crate) fn resolve_nam_path(
    nam_root: &Path,
    pack: &signal::signal_nam::PackDefinition,
    filename: &str,
) -> Option<String> {
    let dir = pack.directory.as_deref().unwrap_or(&pack.id);
    let path = nam_root
        .join(pack.category.directory())
        .join(dir)
        .join(filename);
    if path.exists() {
        Some(path.to_string_lossy().to_string())
    } else {
        None
    }
}

/// Default NAM block parameters.
pub(crate) fn nam_block_params() -> Vec<signal::BlockParameter> {
    vec![
        signal::BlockParameter::new("INPUT_LEVEL", "Input Level", 0.5),
        signal::BlockParameter::new("OUTPUT_LEVEL", "Output Level", 0.5),
        signal::BlockParameter::new("NOISE_GATE_THRESHOLD", "Noise Gate Threshold", 0.0),
        signal::BlockParameter::new("NOISE_GATE_ACTIVE", "Noise Gate Active", 0.0),
    ]
}

/// Tone sort key for ordering: clean first, then crunch, drive, lead, overdrive.
pub(crate) fn tone_sort_key(tone: &str) -> u8 {
    match tone.to_lowercase().as_str() {
        "clean" => 0,
        "crunch" => 1,
        "drive" => 2,
        "lead" => 3,
        "overdrive" => 4,
        _ => 5,
    }
}

/// Capitalize first letter of a string.
pub(crate) fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().to_string() + chars.as_str(),
    }
}

/// Extract a tone-like label from a filename.
pub(crate) fn filename_to_tone(filename: &str) -> String {
    let stem = filename.rsplit('.').nth(1).unwrap_or(filename);
    // Remove common prefixes like "ML PEAV Block" etc.
    stem.split_whitespace()
        .last()
        .unwrap_or(stem)
        .to_lowercase()
}

// ============================================================================
// Command Implementations — Profiles
// ============================================================================

