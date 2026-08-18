//! High-precision compressor audio capture and comparison.
//!
//! Unlike `compressor.rs` which stores gain-reduction as u8 (0.21 dB resolution,
//! downsampled to 1 ms), this module stores the raw f32 audio output at full
//! sample rate. This enables measuring null depth when phase-cancelling against
//! a reference plugin.
//!
//! Storage format: one `.bin` file per scenario, f32 audio, frequency-major.
//! Layout: [num_freqs: u32][samples_per_freq: u32][f32 × num_freqs × samples_per_freq]
//!
//! Comparison metrics (per frequency):
//!   - Null depth (dB):   20*log10(rms(ref - test) / rms(ref))  — phase cancellation quality
//!   - GR RMS diff (dB):  rms of per-sample GR difference, at full 48 kHz
//!   - Max sample diff:   worst single-sample |ref - test| expressed in dB relative to ref RMS

use std::io::{Read, Write};
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use anyhow::{Context, Result};
use fts_analyzer::host::LoadedPlugin;
use fts_analyzer::signal::{self, Waveform};

use crate::compressor::Scenario;

// ---------------------------------------------------------------------------
// Capture
// ---------------------------------------------------------------------------

/// Capture raw f32 audio output from a plugin at full sample rate.
///
/// Same pulse-tone signal as capture-compressor, but stores the actual audio
/// output (not GR, not downsampled) so it can be used for phase-cancellation
/// null tests.
pub fn run_capture_audio(
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

    eprintln!("Loading plugin: {}", plugin_path);
    let mut probe = LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)?;
    let latency = probe.latency() as usize;
    drop(probe);

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
        "Duration: {:.1}s, Sample rate: {} Hz, {} freqs, {} scenario(s), {} threads",
        duration,
        sample_rate,
        frequencies.len(),
        scenarios.len(),
        num_threads,
    );

    let sr = sample_rate as f32;
    let total_samples = (sr * duration) as usize;

    let file_size_mb = frequencies.len() * total_samples * 4 / 1_048_576;
    eprintln!(
        "Output: ~{} MB/scenario ({} freqs × {} samples × 4 bytes)\n",
        file_size_mb,
        frequencies.len(),
        total_samples,
    );

    // Metadata
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
        "precision": "f32",
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

    let completed_scenarios = Arc::new(AtomicUsize::new(0));
    let total_scenarios = scenarios.len();
    let load_lock = Arc::new(Mutex::new(()));

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
                    let mut plugin = {
                        let _guard = load_lock.lock().unwrap();
                        eprintln!("  Loading plugin instance for {} scenarios...", chunk.len());
                        match LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size) {
                            Ok(p) => p,
                            Err(e) => {
                                return chunk
                                    .iter()
                                    .map(|sc| format!("{}: failed to load: {}", sc.name, e))
                                    .collect::<Vec<_>>();
                            }
                        }
                    };

                    let mut thread_errors = Vec::new();
                    for scenario in chunk {
                        let result = run_audio_scenario(
                            &mut plugin,
                            output_dir,
                            gain_high_db,
                            gain_low_db,
                            time_high_ms,
                            time_low_ms,
                            waveform,
                            sr,
                            frequencies,
                            base_params,
                            scenario,
                            latency,
                            total_samples,
                        );

                        let done = completed.fetch_add(1, Ordering::Relaxed) + 1;
                        match result {
                            Ok(_) => {
                                eprintln!("[{:>4}/{}] {}", done, total_scenarios, scenario.name)
                            }
                            Err(e) => {
                                eprintln!(
                                    "[{:>4}/{}] FAIL {} — {}",
                                    done, total_scenarios, scenario.name, e
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

    let total_size = dir_size(output_dir);
    eprintln!(
        "\nResults saved to {} ({:.1} MB)",
        output_dir.display(),
        total_size as f64 / 1_048_576.0,
    );
    Ok(())
}

/// Process one scenario and write audio output.
fn run_audio_scenario(
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
) -> Result<()> {
    let mut merged_params = base_params.to_vec();
    for &(id, val) in &scenario.params {
        if let Some(existing) = merged_params.iter_mut().find(|(eid, _)| *eid == id) {
            existing.1 = val;
        } else {
            merged_params.push((id, val));
        }
    }

    let mut all_audio: Vec<Vec<f32>> = Vec::with_capacity(frequencies.len());

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

        // Latency-compensated output, trimmed to exact total_samples
        let audio = output[latency..latency + total_samples].to_vec();
        all_audio.push(audio);
    }

    let bin_path = output_dir.join(format!("{}.bin", scenario.name));
    write_audio_bin(&bin_path, &all_audio)?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Compare
// ---------------------------------------------------------------------------

/// Compare a plugin's audio output against a reference capture.
///
/// Metrics per frequency:
///   - Null depth (dB): 20*log10(rms(ref - test) / rms(ref))
///     Phase cancellation quality. -40 dB = good; -60 dB = excellent.
///   - GR RMS diff (dB): rms of per-sample GR difference at full rate
///     Direct measure of gain-reduction formula accuracy.
///   - Max sample diff: peak |ref - test| relative to ref RMS
pub fn run_compare_audio(
    plugin_path: &str,
    reference_dir: &Path,
    output_dir: &Path,
    sample_rate: f64,
    block_size: u32,
    base_params: &[(u32, f64)],
    null_depth_threshold_db: f32,
    remaps: &[crate::compressor::ParamRemap],
    filters: &[String],
    limit: Option<usize>,
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
    let duration = meta["duration"].as_f64().unwrap_or(3.0) as f32;
    let waveform_str = meta["waveform"].as_str().unwrap_or("Sine");
    let waveform = match waveform_str {
        "Square" => Waveform::Square,
        "Saw" => Waveform::Saw,
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

    let scenario_metas = meta["scenarios"]
        .as_array()
        .context("no scenarios in metadata")?;

    let mut scenarios: Vec<Scenario> = scenario_metas
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

    if !filters.is_empty() {
        scenarios.retain(|s| filters.iter().any(|f| s.name.contains(f.as_str())));
    }
    if let Some(max) = limit {
        scenarios.truncate(max);
    }

    eprintln!("Reference: {}", reference_dir.display());
    eprintln!("Loading plugin: {}", plugin_path);
    let mut plugin = LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)?;
    let latency = plugin.latency() as usize;

    // Resolve param remaps against the test plugin's parameter list
    let resolved_remaps = if !remaps.is_empty() {
        let plugin_params = plugin.params();
        let resolved = crate::compressor::resolve_remaps_pub(remaps, &plugin_params)?;
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
        "{} frequencies, {} scenario(s), null threshold: {:.0} dB\n",
        frequencies.len(),
        scenarios.len(),
        null_depth_threshold_db,
    );

    let sr = sample_rate as f32;
    let total_samples = (sr * duration) as usize;

    let mut overall_failures = 0;
    let mut overall_worst_null = f32::NEG_INFINITY;

    std::fs::write(
        output_dir.join("metadata.json"),
        serde_json::to_string_pretty(&serde_json::json!({
            "reference": reference_dir.to_string_lossy(),
            "plugin": plugin_path,
            "null_depth_threshold_db": null_depth_threshold_db,
        }))?,
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

        let ref_audio = read_audio_bin(&ref_bin_path)
            .with_context(|| format!("failed to read {}", ref_bin_path.display()))?;

        // Resolve params for this scenario (remap + merge)
        let remapped_params = if !resolved_remaps.is_empty() {
            crate::compressor::apply_remaps_pub(scenario, &resolved_remaps)
        } else {
            scenario.params.clone()
        };

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
        let mut scenario_worst_null = f32::NEG_INFINITY;
        let mut test_all_audio: Vec<Vec<f32>> = Vec::with_capacity(frequencies.len());

        for (i, &freq_hz) in frequencies.iter().enumerate() {
            let ref_freq = ref_audio
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
            let test_audio = output[latency..latency + total_samples].to_vec();

            // Compute the input slice matching the reference window
            let input_trimmed = &input[..total_samples];

            let cmp = compare_audio(ref_freq, &test_audio, input_trimmed);

            // Pass = null depth more negative than threshold (e.g., -40 dB)
            let pass = cmp.null_depth_db <= null_depth_threshold_db;
            if !pass {
                scenario_failures += 1;
            }
            // Track worst (least-negative) null depth
            if cmp.null_depth_db > scenario_worst_null {
                scenario_worst_null = cmp.null_depth_db;
            }

            let print = !pass || i == 0 || i == frequencies.len() - 1 || (i + 1) % 20 == 0;
            if print {
                let status = if pass { "PASS" } else { "FAIL" };
                eprintln!(
                    "  [{:>4}/{}] {:>8.1} Hz  {}  null={:+.1} dB  gr_rms={:.3} dB  max={:.3} dB",
                    i + 1,
                    frequencies.len(),
                    freq_hz,
                    status,
                    cmp.null_depth_db,
                    cmp.gr_rms_diff_db,
                    cmp.max_sample_diff_db,
                );
            }

            test_all_audio.push(test_audio);
        }

        // Save test audio for reference
        let out_bin_path = output_dir.join(format!("{}.bin", scenario.name));
        write_audio_bin(&out_bin_path, &test_all_audio)?;

        overall_failures += scenario_failures;
        if scenario_worst_null > overall_worst_null {
            overall_worst_null = scenario_worst_null;
        }

        let passed = frequencies.len() - scenario_failures;
        eprintln!(
            "  -> {}/{} passed (worst null: {:+.1} dB)\n",
            passed,
            frequencies.len(),
            scenario_worst_null,
        );
    }

    // Summary
    let total_tests = frequencies.len() * scenarios.len();
    eprintln!("========================================================");
    eprintln!(
        "  Total: {}/{} passed across {} scenario(s)",
        total_tests - overall_failures,
        total_tests,
        scenarios.len()
    );
    eprintln!("  Worst null depth: {:+.1} dB", overall_worst_null);
    eprintln!("  Null threshold:   {:.0} dB", null_depth_threshold_db);
    eprintln!("========================================================");

    if overall_failures > 0 {
        anyhow::bail!(
            "{}/{} tests below null threshold of {} dB",
            overall_failures,
            total_tests,
            null_depth_threshold_db,
        );
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Analysis
// ---------------------------------------------------------------------------

struct AudioComparison {
    /// Phase cancellation quality: 20*log10(rms(ref-test) / rms(ref)).
    /// -40 dB = 1% residual; -60 dB = 0.1% residual; 0 dB = no cancellation.
    null_depth_db: f32,
    /// RMS of per-sample gain-reduction difference in dB (at full sample rate).
    gr_rms_diff_db: f32,
    /// Peak |ref - test| expressed in dB relative to ref RMS.
    max_sample_diff_db: f32,
}

fn compare_audio(reference: &[f32], test: &[f32], input: &[f32]) -> AudioComparison {
    let len = reference.len().min(test.len()).min(input.len());

    if len == 0 {
        return AudioComparison {
            null_depth_db: 0.0,
            gr_rms_diff_db: 0.0,
            max_sample_diff_db: 0.0,
        };
    }

    // Null depth: rms of (ref - test) vs rms of ref
    let mut ref_sq_sum = 0.0f64;
    let mut diff_sq_sum = 0.0f64;
    let mut max_diff_abs = 0.0f32;

    // GR diff: compare per-sample gain reduction where input is above noise floor
    let mut gr_diff_sq_sum = 0.0f64;
    let mut gr_count = 0u64;

    for i in 0..len {
        let r = reference[i] as f64;
        let t = test[i] as f64;
        let diff = r - t;

        ref_sq_sum += r * r;
        diff_sq_sum += diff * diff;

        let diff_abs = diff.abs() as f32;
        if diff_abs > max_diff_abs {
            max_diff_abs = diff_abs;
        }

        // GR comparison: only where |input| is above noise floor
        let in_abs = input[i].abs();
        if in_abs > 1e-7 {
            let ref_gr = 20.0 * (reference[i].abs().max(1e-10) / in_abs).log10();
            let test_gr = 20.0 * (test[i].abs().max(1e-10) / in_abs).log10();
            let gr_diff = ref_gr - test_gr;
            gr_diff_sq_sum += (gr_diff as f64) * (gr_diff as f64);
            gr_count += 1;
        }
    }

    let ref_rms = (ref_sq_sum / len as f64).sqrt() as f32;
    let diff_rms = (diff_sq_sum / len as f64).sqrt() as f32;

    let null_depth_db = if ref_rms > 1e-12 {
        20.0 * (diff_rms / ref_rms).log10()
    } else {
        0.0
    };

    let max_sample_diff_db = if ref_rms > 1e-12 {
        20.0 * (max_diff_abs / ref_rms).log10()
    } else {
        0.0
    };

    let gr_rms_diff_db = if gr_count > 0 {
        (gr_diff_sq_sum / gr_count as f64).sqrt() as f32
    } else {
        0.0
    };

    AudioComparison {
        null_depth_db,
        gr_rms_diff_db,
        max_sample_diff_db,
    }
}

// ---------------------------------------------------------------------------
// Binary format: f32 audio, frequency-major, full sample rate
// ---------------------------------------------------------------------------
//
// Layout:
//   [num_freqs: u32 LE]
//   [samples_per_freq: u32 LE]
//   [f32 LE × num_freqs × samples_per_freq]  (frequency-major)

fn write_audio_bin(path: &Path, freq_data: &[Vec<f32>]) -> Result<()> {
    let num_freqs = freq_data.len() as u32;
    let samples_per_freq = freq_data.first().map(|v| v.len()).unwrap_or(0) as u32;

    let mut f = std::io::BufWriter::new(std::fs::File::create(path)?);
    f.write_all(&num_freqs.to_le_bytes())?;
    f.write_all(&samples_per_freq.to_le_bytes())?;

    for audio in freq_data {
        for &sample in audio {
            f.write_all(&sample.to_le_bytes())?;
        }
    }

    f.flush()?;
    Ok(())
}

fn read_audio_bin(path: &Path) -> Result<Vec<Vec<f32>>> {
    let mut f = std::io::BufReader::new(std::fs::File::open(path)?);

    let mut header = [0u8; 8];
    f.read_exact(&mut header)?;
    let num_freqs = u32::from_le_bytes([header[0], header[1], header[2], header[3]]) as usize;
    let samples_per_freq =
        u32::from_le_bytes([header[4], header[5], header[6], header[7]]) as usize;

    let mut result = Vec::with_capacity(num_freqs);
    let mut buf = vec![0u8; samples_per_freq * 4];

    for _ in 0..num_freqs {
        f.read_exact(&mut buf)?;
        let samples: Vec<f32> = buf
            .chunks_exact(4)
            .map(|b| f32::from_le_bytes([b[0], b[1], b[2], b[3]]))
            .collect();
        result.push(samples);
    }

    Ok(result)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

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
