//! Compressor curve capture and comparison.
//!
//! Uses the Delta Expose principle: a carrier tone pulsing between two gain levels
//! is processed through a compressor. Per-sample gain reduction is computed as
//! `20*log10(|output|/|input|)`, yielding the full compression curve at each
//! test frequency.
//!
//! Storage format: one `.bin` file per scenario containing all frequencies.
//! Layout: [num_freqs: u32][samples_per_freq: u32][u8 × num_freqs × samples_per_freq]
//! Data is frequency-major. Values quantized: u8 maps linearly over GR_MIN..GR_MAX dB.

use std::io::{Read, Write};
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use anyhow::{Context, Result};
use fts_analyzer::host::{LoadedPlugin, ParamInfo};
use fts_analyzer::signal::{self, Waveform};

/// A parameter remapping: reference param ID → test plugin param name.
#[derive(Debug, Clone)]
pub struct ParamRemap {
    pub from_id: u32,
    pub to_name: String,
}

/// Parse `--param-remap FROM_ID=TO_NAME` strings.
pub fn parse_param_remaps(args: &[String]) -> Result<Vec<ParamRemap>> {
    args.iter()
        .map(|s| {
            let (id_str, name) = s
                .split_once('=')
                .context("param-remap must be FROM_ID=TO_NAME")?;
            Ok(ParamRemap {
                from_id: id_str.parse().context("invalid FROM_ID")?,
                to_name: name.to_string(),
            })
        })
        .collect()
}

/// Resolved remap: maps a reference param ID to a target CLAP param ID.
pub struct ResolvedRemap {
    pub from_id: u32,
    pub to_id: u32,
    pub to_name: String,
    pub to_min: f64,
    pub to_max: f64,
}

/// Resolve remaps against the test plugin's parameter list.
pub fn resolve_remaps(remaps: &[ParamRemap], params: &[ParamInfo]) -> Result<Vec<ResolvedRemap>> {
    remaps
        .iter()
        .map(|r| {
            let info = params
                .iter()
                .find(|p| p.name.eq_ignore_ascii_case(&r.to_name))
                .with_context(|| {
                    let available: Vec<_> = params
                        .iter()
                        .map(|p| format!("  {} (id={})", p.name, p.id))
                        .collect();
                    format!(
                        "param-remap: no parameter named '{}' in test plugin.\nAvailable:\n{}",
                        r.to_name,
                        available.join("\n")
                    )
                })?;
            Ok(ResolvedRemap {
                from_id: r.from_id,
                to_id: info.id,
                to_name: r.to_name.clone(),
                to_min: info.min,
                to_max: info.max,
            })
        })
        .collect()
}

/// Parsed values extracted from a scenario name.
///
/// Supports formats like:
///   "atk-24ms_rel-50ms"           → attack_ms=24, release_ms=50
///   "thr--18db_atk-10ms_rel-100ms" → threshold_db=-18, attack_ms=10, release_ms=100
struct ScenarioValues {
    attack_ms: Option<f64>,
    release_ms: Option<f64>,
    threshold_db: Option<f64>,
}

/// Parse labeled values from a scenario name.
fn parse_scenario_values(name: &str) -> ScenarioValues {
    let mut vals = ScenarioValues {
        attack_ms: None,
        release_ms: None,
        threshold_db: None,
    };

    for part in name.split('_') {
        if let Some(rest) = part.strip_prefix("atk-") {
            if let Some(val_str) = rest.strip_suffix("ms") {
                vals.attack_ms = val_str.parse::<f64>().ok();
            }
        } else if let Some(rest) = part.strip_prefix("rel-") {
            if let Some(val_str) = rest.strip_suffix("ms") {
                vals.release_ms = val_str.parse::<f64>().ok();
            }
        } else if let Some(rest) = part.strip_prefix("thr-")
            && let Some(val_str) = rest.strip_suffix("db")
        {
            vals.threshold_db = val_str.parse::<f64>().ok();
        }
    }

    vals
}

/// Apply remaps to a scenario's params: replace reference param IDs with
/// test plugin IDs and substitute parsed values from the scenario name.
fn apply_remaps(scenario: &Scenario, resolved: &[ResolvedRemap]) -> Vec<(u32, f64)> {
    let vals = parse_scenario_values(&scenario.name);

    let mut result: Vec<(u32, f64)> = Vec::new();

    for &(id, val) in &scenario.params {
        if let Some(remap) = resolved.iter().find(|r| r.from_id == id) {
            // Try to extract the actual value from the scenario name.
            // Match by target param name to the appropriate parsed value.
            let parsed_val = match remap.to_name.to_ascii_lowercase().as_str() {
                "attack" => vals.attack_ms,
                "release" => vals.release_ms,
                "threshold" => vals.threshold_db,
                _ => None,
            };

            if let Some(v) = parsed_val {
                result.push((remap.to_id, v));
            } else {
                // Fallback: scale normalized value to target range
                let plain = remap.to_min + val * (remap.to_max - remap.to_min);
                result.push((remap.to_id, plain));
            }
        } else {
            // Non-remapped param — pass through as-is
            result.push((id, val));
        }
    }

    result
}

/// Public wrapper for use by compressor_audio module.
pub fn resolve_remaps_pub(
    remaps: &[ParamRemap],
    params: &[ParamInfo],
) -> Result<Vec<ResolvedRemap>> {
    resolve_remaps(remaps, params)
}

/// Public wrapper for use by compressor_audio module.
pub fn apply_remaps_pub(scenario: &Scenario, resolved: &[ResolvedRemap]) -> Vec<(u32, f64)> {
    apply_remaps(scenario, resolved)
}

/// A test scenario: a named set of parameter overrides (e.g. "fast-attack-fast-release").
#[derive(Debug, Clone)]
pub struct Scenario {
    pub name: String,
    pub params: Vec<(u32, f64)>,
}

// Quantization range for u8 storage
const GR_MIN_DB: f32 = -48.0;
const GR_MAX_DB: f32 = 6.0;

fn gr_to_u8(db: f32) -> u8 {
    let clamped = db.clamp(GR_MIN_DB, GR_MAX_DB);
    ((clamped - GR_MIN_DB) / (GR_MAX_DB - GR_MIN_DB) * 255.0) as u8
}

fn u8_to_gr(val: u8) -> f32 {
    GR_MIN_DB + (val as f32 / 255.0) * (GR_MAX_DB - GR_MIN_DB)
}

/// Musically-spaced test frequencies (Hz).
/// Dense at the low end where compressor behavior varies most,
/// progressively sparser toward the top.
pub const TEST_FREQUENCIES: &[f32] = &[
    // Sub bass — every 10 Hz
    20.0, 30.0, 40.0, 50.0, 60.0, 80.0, 100.0, // Bass — every 20-40 Hz
    120.0, 140.0, 160.0, 180.0, 200.0, 240.0, 280.0, 320.0, // Low mids — bigger steps
    400.0, 480.0, 560.0, 640.0, 800.0, // Mids
    1000.0, 1250.0, 1500.0, 2000.0, 2500.0, // Upper mids
    3000.0, 4000.0, 5000.0, 6000.0, // Highs
    8000.0, 10000.0, 12000.0, 16000.0, 20000.0,
];

/// Parse scenarios from a JSON file.
///
/// Expected format:
/// ```json
/// {
///   "scenarios": [
///     { "name": "fast-fast", "params": { "7": 0.01, "8": 0.05 } },
///     { "name": "default",   "params": {} }
///   ]
/// }
/// ```
pub fn parse_scenarios(path: &Path) -> Result<Vec<Scenario>> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read scenario file: {}", path.display()))?;
    let json: serde_json::Value = serde_json::from_str(&content)?;

    let scenarios = json["scenarios"]
        .as_array()
        .context("expected 'scenarios' array in config")?;

    scenarios
        .iter()
        .map(|s| {
            let name = s["name"]
                .as_str()
                .context("scenario missing 'name'")?
                .to_string();
            let params_obj = s["params"]
                .as_object()
                .context("scenario missing 'params'")?;
            let params: Vec<(u32, f64)> = params_obj
                .iter()
                .map(|(k, v)| {
                    let id: u32 = k
                        .parse()
                        .with_context(|| format!("invalid param id '{}'", k))?;
                    let val = v
                        .as_f64()
                        .with_context(|| format!("invalid value for param {}", k))?;
                    Ok((id, val))
                })
                .collect::<Result<_>>()?;
            Ok(Scenario { name, params })
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Capture
// ---------------------------------------------------------------------------

/// Capture compressor behavior across scenarios × frequencies.
///
/// Parallelizes across scenarios — each thread loads its own plugin instance
/// and processes all frequencies for one scenario. Results stored as binary.
pub fn run_capture(
    plugin_path: &str,
    output_dir: &Path,
    sample_rate: f64,
    block_size: u32,
    gain_high_db: f32,
    gain_low_db: f32,
    time_high_ms: f32,
    time_low_ms: f32,
    waveform: Waveform,
    duration: f32,
    frequencies: &[f32],
    base_params: &[(u32, f64)],
    scenarios: &[Scenario],
    threads: Option<usize>,
) -> Result<()> {
    std::fs::create_dir_all(output_dir)?;

    // Probe latency from a throwaway instance
    eprintln!("Loading plugin: {}", plugin_path);
    let mut probe = LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)?;
    let latency = probe.latency() as usize;
    drop(probe);

    // Each plugin instance spawns a full Wine process via yabridge.
    // Loading is serialized via mutex; processing runs in parallel.
    let num_threads = threads.unwrap_or_else(|| {
        std::thread::available_parallelism()
            .map(|n| n.get().min(8))
            .unwrap_or(4)
    });

    eprintln!("Latency: {} samples", latency);
    eprintln!(
        "Signal: gain_high={:.1} dB, gain_low={:.1} dB, time_high={:.0}ms, time_low={:.0}ms, {:?}",
        gain_high_db, gain_low_db, time_high_ms, time_low_ms, waveform,
    );
    eprintln!(
        "Duration: {:.1}s, Sample rate: {} Hz",
        duration, sample_rate
    );
    eprintln!(
        "{} frequencies (log-spaced {:.0}–{:.0} Hz), {} scenario(s), {} threads\n",
        frequencies.len(),
        frequencies.first().unwrap_or(&0.0),
        frequencies.last().unwrap_or(&0.0),
        scenarios.len(),
        num_threads,
    );

    let sr = sample_rate as f32;
    let total_samples = (sr * duration) as usize;
    let downsample_step = (sample_rate / 1000.0).round() as usize;

    // Write top-level metadata
    let meta = serde_json::json!({
        "plugin_path": plugin_path,
        "sample_rate": sample_rate,
        "block_size": block_size,
        "gain_high_db": gain_high_db,
        "gain_low_db": gain_low_db,
        "time_high_ms": time_high_ms,
        "time_low_ms": time_low_ms,
        "waveform": format!("{:?}", waveform),
        "duration": duration,
        "frequencies": frequencies,
        "latency_samples": latency,
        "downsample_step": downsample_step,
        "ms_per_row": downsample_step as f64 / sample_rate * 1000.0,
        "scenarios": scenarios.iter().map(|s| serde_json::json!({
            "name": &s.name,
            "params": s.params.iter().map(|(id, v)| serde_json::json!({ "id": id, "value": v })).collect::<Vec<_>>(),
        })).collect::<Vec<_>>(),
        "base_params": base_params.iter().map(|(id, v)| serde_json::json!({ "id": id, "value": v })).collect::<Vec<_>>(),
    });
    std::fs::write(
        output_dir.join("metadata.json"),
        serde_json::to_string_pretty(&meta)?,
    )?;

    // Shared progress counter
    let completed_scenarios = Arc::new(AtomicUsize::new(0));
    let total_scenarios = scenarios.len();

    // Mutex to serialize plugin loading — Wine/yabridge crashes if multiple
    // instances are loaded simultaneously, but runs fine once loaded.
    let load_lock = Arc::new(Mutex::new(()));

    // Distribute scenarios across threads in chunks
    let chunk_size = scenarios.len().div_ceil(num_threads);
    let chunks: Vec<&[Scenario]> = scenarios.chunks(chunk_size).collect();

    eprintln!("Launching {} worker threads...\n", chunks.len());

    let errors: Vec<String> = std::thread::scope(|s| {
        let handles: Vec<_> = chunks
            .into_iter()
            .map(|chunk| {
                let load_lock = Arc::clone(&load_lock);
                let completed = Arc::clone(&completed_scenarios);
                s.spawn(move || {
                    // Serialize plugin loading across threads
                    let mut plugin = {
                        let _guard = load_lock.lock().unwrap();
                        eprintln!("  Loading plugin instance for {} scenarios...", chunk.len());
                        match LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size) {
                            Ok(p) => p,
                            Err(e) => {
                                return chunk
                                    .iter()
                                    .map(|sc| format!("{}: failed to load plugin: {}", sc.name, e))
                                    .collect::<Vec<_>>();
                            }
                        }
                    };
                    // _guard dropped here — next thread can load

                    let mut thread_errors = Vec::new();
                    for scenario in chunk {
                        // Skip scenarios that already have a complete .bin file (resume support)
                        let bin_path = output_dir.join(format!("{}.bin", scenario.name));
                        if bin_path.exists() {
                            let n =
                                completed.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
                            eprintln!(
                                "[{:4}/{:4}] {} (skipped)",
                                n, total_scenarios, scenario.name
                            );
                            continue;
                        }
                        let result = run_scenario_on_instance(
                            &mut plugin,
                            output_dir,
                            gain_high_db,
                            gain_low_db,
                            time_high_ms,
                            time_low_ms,
                            waveform,
                            sample_rate as f32,
                            frequencies,
                            base_params,
                            scenario,
                            latency,
                            total_samples,
                            downsample_step,
                        );

                        let done = completed.fetch_add(1, Ordering::Relaxed) + 1;
                        match result {
                            Ok(_) => {
                                eprintln!("[{:>4}/{}] {} ", done, total_scenarios, scenario.name,);
                            }
                            Err(e) => {
                                eprintln!(
                                    "[{:>4}/{}] FAIL {} — {}",
                                    done, total_scenarios, scenario.name, e,
                                );
                                thread_errors.push(format!("{}: {}", scenario.name, e));
                            }
                        }
                    }
                    thread_errors
                })
            })
            .collect();

        handles
            .into_iter()
            .flat_map(|h| h.join().unwrap_or_default())
            .collect()
    });

    if !errors.is_empty() {
        eprintln!("\n{} scenario(s) failed:", errors.len());
        for e in &errors {
            eprintln!("  {}", e);
        }
        anyhow::bail!("{} scenario(s) failed", errors.len());
    }

    // Also write a single CSV with all data for human readability
    eprintln!("\nWriting CSV export...");
    write_all_csv(output_dir, frequencies, scenarios)?;

    let total_size = dir_size(output_dir);
    eprintln!(
        "Results saved to {} ({:.1} MB)",
        output_dir.display(),
        total_size as f64 / 1_048_576.0,
    );
    Ok(())
}

/// Process a single scenario using an already-loaded plugin instance.
/// Writes one binary file containing GR data for all frequencies.
fn run_scenario_on_instance(
    plugin: &mut LoadedPlugin,
    output_dir: &Path,
    gain_high_db: f32,
    gain_low_db: f32,
    time_high_ms: f32,
    time_low_ms: f32,
    waveform: Waveform,
    sr: f32,
    frequencies: &[f32],
    base_params: &[(u32, f64)],
    scenario: &Scenario,
    latency: usize,
    total_samples: usize,
    downsample_step: usize,
) -> Result<()> {
    // Merge base params with scenario-specific params
    let mut merged_params = base_params.to_vec();
    for &(id, val) in &scenario.params {
        if let Some(existing) = merged_params.iter_mut().find(|(eid, _)| *eid == id) {
            existing.1 = val;
        } else {
            merged_params.push((id, val));
        }
    }

    let mut all_gr: Vec<Vec<f32>> = Vec::with_capacity(frequencies.len());

    for &freq_hz in frequencies {
        let input = signal::pulse_tone(
            freq_hz,
            gain_high_db,
            gain_low_db,
            time_high_ms,
            time_low_ms,
            waveform,
            sr,
            total_samples + latency,
        );

        let output = plugin.process(&input, &merged_params)?;

        let gr = compute_gain_reduction_downsampled(
            &input[..total_samples],
            &output[latency..latency + total_samples],
            downsample_step,
        );

        all_gr.push(gr);
    }

    let bin_path = output_dir.join(format!("{}.bin", scenario.name));
    write_scenario_bin(&bin_path, &all_gr)?;

    Ok(())
}

// ---------------------------------------------------------------------------
// Compare
// ---------------------------------------------------------------------------

/// Compare a plugin's compressor behavior against a saved reference.
pub fn run_compare(
    plugin_path: &str,
    reference_dir: &Path,
    output_dir: &Path,
    sample_rate: f64,
    block_size: u32,
    base_params: &[(u32, f64)],
    tolerance_db: f32,
    remaps: &[ParamRemap],
    filters: &[String],
    limit: Option<usize>,
    freq_filters: &[f32],
) -> Result<()> {
    std::fs::create_dir_all(output_dir)?;

    // Load reference metadata
    let meta_path = reference_dir.join("metadata.json");
    let meta: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(&meta_path)
            .with_context(|| format!("no metadata.json in {}", reference_dir.display()))?,
    )?;

    let gain_high_db = meta["gain_high_db"].as_f64().unwrap_or(-6.0) as f32;
    let gain_low_db = meta["gain_low_db"].as_f64().unwrap_or(-20.0) as f32;
    let time_high_ms = meta["time_high_ms"].as_f64().unwrap_or(240.0) as f32;
    let time_low_ms = meta["time_low_ms"].as_f64().unwrap_or(240.0) as f32;
    let duration = meta["duration"].as_f64().unwrap_or(2.0) as f32;
    let downsample_step = meta["downsample_step"].as_u64().unwrap_or(48) as usize;
    let waveform_str = meta["waveform"].as_str().unwrap_or("Sine");
    let waveform = match waveform_str {
        "Square" => Waveform::Square,
        "Saw" => Waveform::Saw,
        // "Noise" waveform variant not available; fall through to Sine
        "Noise" => Waveform::Sine,
        _ => Waveform::Sine,
    };
    let frequencies: Vec<f32> = meta["frequencies"]
        .as_array()
        .map(|a| {
            a.iter()
                .filter_map(|v| v.as_f64().map(|f| f as f32))
                .collect()
        })
        .unwrap_or_default();

    // Load scenario definitions
    let scenario_metas = meta["scenarios"]
        .as_array()
        .context("no scenarios in metadata")?;

    let scenarios: Vec<Scenario> = scenario_metas
        .iter()
        .map(|s| {
            let name = s["name"].as_str().unwrap_or("default").to_string();
            let params: Vec<(u32, f64)> = s["params"]
                .as_array()
                .map(|arr| {
                    arr.iter()
                        .filter_map(|p| Some((p["id"].as_u64()? as u32, p["value"].as_f64()?)))
                        .collect()
                })
                .unwrap_or_default();
            Scenario { name, params }
        })
        .collect();

    // Apply scenario filters
    let mut scenarios = scenarios;
    let filtered_scenarios;
    if !filters.is_empty() {
        filtered_scenarios = scenarios
            .into_iter()
            .filter(|s| filters.iter().any(|f| s.name.contains(f.as_str())))
            .collect::<Vec<_>>();
        scenarios = filtered_scenarios;
    }
    if let Some(max) = limit {
        let truncated = scenarios.into_iter().take(max).collect::<Vec<_>>();
        scenarios = truncated;
    }

    eprintln!("Reference: {}", reference_dir.display());
    eprintln!("Loading plugin: {}", plugin_path);
    let mut plugin = LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)?;
    let latency = plugin.latency() as usize;

    // Resolve param remaps against the test plugin's parameter list
    let resolved_remaps = if !remaps.is_empty() {
        let plugin_params = plugin.params();
        let resolved = resolve_remaps(remaps, &plugin_params)?;
        for r in &resolved {
            eprintln!(
                "  Remap: ref param {} → {} (id={}, range={:.2}..{:.2})",
                r.from_id, r.to_name, r.to_id, r.to_min, r.to_max
            );
        }
        resolved
    } else {
        Vec::new()
    };

    eprintln!(
        "Signal: gain_high={:.1} dB, gain_low={:.1} dB, time_high={:.0}ms, time_low={:.0}ms",
        gain_high_db, gain_low_db, time_high_ms, time_low_ms,
    );
    eprintln!(
        "{} frequencies, {} scenario(s), tolerance: {:.2} dB\n",
        frequencies.len(),
        scenarios.len(),
        tolerance_db,
    );

    let sr = sample_rate as f32;
    let total_samples = (sr * duration) as usize;

    let mut overall_failures = 0;
    let mut overall_worst_rms = 0.0f32;

    // Write comparison metadata
    let compare_meta = serde_json::json!({
        "reference": reference_dir.to_string_lossy(),
        "plugin": plugin_path,
        "tolerance_db": tolerance_db,
    });
    std::fs::write(
        output_dir.join("metadata.json"),
        serde_json::to_string_pretty(&compare_meta)?,
    )?;

    for scenario in &scenarios {
        let ref_bin_path = reference_dir.join(format!("{}.bin", scenario.name));

        if !ref_bin_path.exists() {
            eprintln!(
                "  Warning: reference scenario '{}' not found, skipping",
                scenario.name
            );
            continue;
        }

        let ref_data = read_scenario_bin(&ref_bin_path)
            .with_context(|| format!("failed to read {}", ref_bin_path.display()))?;

        // Apply remaps to get test plugin params from scenario
        let remapped_params = if !resolved_remaps.is_empty() {
            apply_remaps(scenario, &resolved_remaps)
        } else {
            scenario.params.clone()
        };

        // Merge base params with (remapped) scenario params
        let mut merged_params = base_params.to_vec();
        for (id, val) in &remapped_params {
            if let Some(existing) = merged_params.iter_mut().find(|(eid, _)| eid == id) {
                existing.1 = *val;
            } else {
                merged_params.push((*id, *val));
            }
        }

        eprintln!("--- Scenario: {} ---", scenario.name);
        if !resolved_remaps.is_empty() {
            for (id, val) in &remapped_params {
                eprintln!("  param {}={:.4}", id, val);
            }
        }

        let mut scenario_failures = 0;
        let mut scenario_worst_rms = 0.0f32;
        let mut test_all_gr: Vec<Vec<f32>> = Vec::with_capacity(frequencies.len());

        for (i, &freq_hz) in frequencies.iter().enumerate() {
            // Skip frequencies not in the filter (if any filters specified)
            if !freq_filters.is_empty() && !freq_filters.iter().any(|&f| (f - freq_hz).abs() < 0.5)
            {
                continue;
            }
            let ref_gr = ref_data
                .get(i)
                .context("reference data missing frequency")?;

            let input = signal::pulse_tone(
                freq_hz,
                gain_high_db,
                gain_low_db,
                time_high_ms,
                time_low_ms,
                waveform,
                sr,
                total_samples + latency,
            );

            let output = plugin.process(&input, &merged_params)?;
            let test_gr = compute_gain_reduction_downsampled(
                &input[..total_samples],
                &output[latency..latency + total_samples],
                downsample_step,
            );

            let cmp = compare_gr_curves(ref_gr, &test_gr);
            let pass = cmp.rms_diff_db <= tolerance_db;

            if !pass {
                scenario_failures += 1;
            }
            if cmp.rms_diff_db > scenario_worst_rms {
                scenario_worst_rms = cmp.rms_diff_db;
            }

            // Print failures + every 20th + first + last
            let print = !pass || i == 0 || i == frequencies.len() - 1 || (i + 1) % 20 == 0;
            if print {
                let status = if pass { "PASS" } else { "FAIL" };
                eprintln!(
                    "  [{:>4}/{}] {:>8.1} Hz  {}  rms={:.3} dB  max={:.3} dB",
                    i + 1,
                    frequencies.len(),
                    freq_hz,
                    status,
                    cmp.rms_diff_db,
                    cmp.max_diff_db,
                );
            }

            test_all_gr.push(test_gr);
        }

        // Save test results as binary too
        let out_bin_path = output_dir.join(format!("{}.bin", scenario.name));
        write_scenario_bin(&out_bin_path, &test_all_gr)?;

        overall_failures += scenario_failures;
        if scenario_worst_rms > overall_worst_rms {
            overall_worst_rms = scenario_worst_rms;
        }

        let tested_freqs = if freq_filters.is_empty() {
            frequencies.len()
        } else {
            frequencies
                .iter()
                .filter(|&&f| freq_filters.iter().any(|&ff| (ff - f).abs() < 0.5))
                .count()
        };
        let passed = tested_freqs - scenario_failures;
        eprintln!(
            "  -> {}/{} passed (worst RMS diff: {:.3} dB)\n",
            passed, tested_freqs, scenario_worst_rms,
        );
    }

    // Summary
    let active_freqs = if freq_filters.is_empty() {
        frequencies.len()
    } else {
        frequencies
            .iter()
            .filter(|&&f| freq_filters.iter().any(|&ff| (ff - f).abs() < 0.5))
            .count()
    };
    let total_tests = active_freqs * scenarios.len();
    eprintln!("========================================================");
    eprintln!(
        "  Total: {}/{} passed across {} scenario(s)",
        total_tests - overall_failures,
        total_tests,
        scenarios.len()
    );
    eprintln!("  Worst RMS GR diff: {:.3} dB", overall_worst_rms);
    eprintln!("  Tolerance: {:.2} dB", tolerance_db);
    eprintln!("========================================================");

    if overall_failures > 0 {
        anyhow::bail!(
            "{}/{} tests exceeded tolerance of {} dB",
            overall_failures,
            total_tests,
            tolerance_db,
        );
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Binary format: one file per scenario, all frequencies packed together
// ---------------------------------------------------------------------------
//
// Layout:
//   [num_freqs: u32 LE]
//   [samples_per_freq: u32 LE]
//   [f32 LE × num_freqs × samples_per_freq]  (frequency-major)

/// Write all frequencies' GR data for one scenario into a single binary file.
/// Values are quantized to u8 over GR_MIN_DB..GR_MAX_DB range (~0.21 dB resolution).
pub fn write_scenario_bin(path: &Path, freq_data: &[Vec<f32>]) -> Result<()> {
    let num_freqs = freq_data.len() as u32;
    let samples_per_freq = freq_data.first().map(|v| v.len()).unwrap_or(0) as u32;

    let mut f = std::io::BufWriter::new(std::fs::File::create(path)?);
    f.write_all(&num_freqs.to_le_bytes())?;
    f.write_all(&samples_per_freq.to_le_bytes())?;

    for gr in freq_data {
        let quantized: Vec<u8> = gr.iter().map(|&v| gr_to_u8(v)).collect();
        f.write_all(&quantized)?;
    }

    f.flush()?;
    Ok(())
}

/// Read all frequencies' GR data for one scenario from a binary file.
/// Dequantizes u8 back to f32 dB values.
fn read_scenario_bin(path: &Path) -> Result<Vec<Vec<f32>>> {
    let mut f = std::io::BufReader::new(std::fs::File::open(path)?);

    let mut header = [0u8; 8];
    f.read_exact(&mut header)?;
    let num_freqs = u32::from_le_bytes([header[0], header[1], header[2], header[3]]) as usize;
    let samples_per_freq =
        u32::from_le_bytes([header[4], header[5], header[6], header[7]]) as usize;

    let mut result = Vec::with_capacity(num_freqs);

    for _ in 0..num_freqs {
        let mut buf = vec![0u8; samples_per_freq];
        f.read_exact(&mut buf)?;
        let freq_data: Vec<f32> = buf.iter().map(|&v| u8_to_gr(v)).collect();
        result.push(freq_data);
    }

    Ok(result)
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Compute per-sample gain reduction, then downsample by averaging windows.
pub fn compute_gain_reduction_downsampled(input: &[f32], output: &[f32], step: usize) -> Vec<f32> {
    let len = input.len().min(output.len());
    let mut result = Vec::with_capacity(len / step + 1);

    let mut i = 0;
    while i < len {
        let end = (i + step).min(len);
        let mut sum = 0.0f64;
        let mut count = 0u32;

        for j in i..end {
            let in_abs = input[j].abs();
            if in_abs > 1e-10 {
                let out_abs = output[j].abs();
                let gr = 20.0 * (out_abs.max(1e-10) / in_abs).log10();
                sum += gr as f64;
                count += 1;
            }
        }

        let avg_gr = if count > 0 {
            (sum / count as f64) as f32
        } else {
            0.0
        };
        result.push(avg_gr);

        i = end;
    }

    result
}

/// Compare two gain reduction curves.
pub fn compare_gr_curves(reference: &[f32], test: &[f32]) -> GrComparison {
    let len = reference.len().min(test.len());
    if len == 0 {
        return GrComparison {
            rms_diff_db: f32::NAN,
            max_diff_db: 0.0,
        };
    }

    let mut sum_sq = 0.0f64;
    let mut max_diff = 0.0f32;

    for i in 0..len {
        let diff = (test[i] - reference[i]).abs();
        sum_sq += (diff as f64) * (diff as f64);
        if diff > max_diff {
            max_diff = diff;
        }
    }

    GrComparison {
        rms_diff_db: (sum_sq / len as f64).sqrt() as f32,
        max_diff_db: max_diff,
    }
}

pub struct GrComparison {
    pub rms_diff_db: f32,
    pub max_diff_db: f32,
}

/// Write a single CSV containing all scenarios' data.
/// Columns: scenario, time_ms, <freq_hz>, <freq_hz>, ...
/// Values at 1 decimal precision (~0.1 dB).
fn write_all_csv(output_dir: &Path, frequencies: &[f32], scenarios: &[Scenario]) -> Result<()> {
    let csv_path = output_dir.join("capture.csv");
    let mut f = std::io::BufWriter::new(std::fs::File::create(&csv_path)?);

    // Header
    write!(f, "scenario,time_ms")?;
    for &freq in frequencies {
        write!(f, ",{:.0}hz", freq)?;
    }
    writeln!(f)?;

    // Data: read each scenario's binary and expand to CSV rows
    for scenario in scenarios {
        let bin_path = output_dir.join(format!("{}.bin", scenario.name));
        let data = read_scenario_bin(&bin_path)?;
        let samples_per_freq = data.first().map(|v| v.len()).unwrap_or(0);

        for t in 0..samples_per_freq {
            write!(f, "{},{}", scenario.name, t)?;
            for freq_data in &data {
                write!(f, ",{:.1}", freq_data[t])?;
            }
            writeln!(f)?;
        }
    }

    f.flush()?;
    Ok(())
}

/// Dump GR values from binary capture files for analysis.
pub fn dump_capture(
    dir: &Path,
    freq_idx: Option<usize>,
    scenario_filter: Option<&str>,
    time_start: usize,
    time_end: Option<usize>,
    step: usize,
) -> Result<()> {
    let mut bins: Vec<_> = std::fs::read_dir(dir)?
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "bin"))
        .map(|e| e.path())
        .collect();
    bins.sort();

    if let Some(filter) = scenario_filter {
        bins.retain(|p| p.file_stem().is_some_and(|s| s.to_str() == Some(filter)));
    }

    for bin_path in &bins {
        let name = bin_path.file_stem().unwrap().to_str().unwrap();
        let data = read_scenario_bin(bin_path)?;
        let num_freqs = data.len();
        let samples = if num_freqs > 0 { data[0].len() } else { 0 };
        let end = time_end.unwrap_or(samples).min(samples);

        eprintln!(
            "--- {} ({} freqs, {} samples/freq) ---",
            name, num_freqs, samples
        );

        let freq_range: Vec<usize> = if let Some(idx) = freq_idx {
            vec![idx]
        } else {
            (0..num_freqs).collect()
        };

        // Print header
        print!("time_ms");
        for &fi in &freq_range {
            if fi < TEST_FREQUENCIES.len() {
                print!(",{:.0}Hz", TEST_FREQUENCIES[fi]);
            } else {
                print!(",freq{}", fi);
            }
        }
        println!();

        // Print data
        let mut t = time_start;
        while t < end {
            print!("{}", t);
            for &fi in &freq_range {
                if fi < num_freqs {
                    print!(",{:.3}", data[fi][t]);
                } else {
                    print!(",");
                }
            }
            println!();
            t += step;
        }
    }

    Ok(())
}

/// Recursively compute total size of a directory.
fn dir_size(path: &Path) -> u64 {
    let mut size = 0;
    if let Ok(entries) = std::fs::read_dir(path) {
        for entry in entries.flatten() {
            let p = entry.path();
            if p.is_dir() {
                size += dir_size(&p);
            } else if let Ok(m) = p.metadata() {
                size += m.len();
            }
        }
    }
    size
}
