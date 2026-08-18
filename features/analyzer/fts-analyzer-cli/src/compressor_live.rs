//! Live plugin comparison: process two plugins simultaneously and compare.
//!
//! Unlike `compressor.rs` which compares against saved reference data,
//! this module loads both a reference plugin and a test plugin, runs them
//! side-by-side with identical test signals, and compares their gain
//! reduction curves in real-time.

use std::path::Path;

use anyhow::Result;
use fts_analyzer::host::LoadedPlugin;
use fts_analyzer::signal::{self, Waveform};

use crate::compressor::{
    self, ParamRemap, compare_gr_curves, compute_gain_reduction_downsampled, write_scenario_bin,
};

/// Live comparison of reference vs test plugin.
pub fn run_compare_live(
    reference_path: &str,
    test_path: &str,
    output_dir: &Path,
    sample_rate: f64,
    block_size: u32,
    attack_ms: f64,
    release_ms: f64,
    threshold_db: Option<f64>,
    ratio: Option<f64>,
    knee_db: Option<f64>,
    tolerance_db: f32,
    remaps: &[ParamRemap],
    freq_filters: &[f32],
) -> Result<()> {
    std::fs::create_dir_all(output_dir)?;

    eprintln!("=== Live Plugin Comparison ===");
    eprintln!("Reference: {}", reference_path);
    eprintln!("Test:      {}", test_path);
    eprintln!(
        "Sample rate: {} Hz, Block size: {}",
        sample_rate, block_size
    );
    eprintln!("Attack: {} ms, Release: {} ms", attack_ms, release_ms);

    // Load both plugins
    eprintln!("\nLoading reference plugin...");
    let mut ref_plugin = LoadedPlugin::load(reference_path.as_ref(), 0, sample_rate, block_size)?;
    let ref_latency = ref_plugin.latency() as usize;
    eprintln!("  Latency: {} samples", ref_latency);

    eprintln!("Loading test plugin...");
    let mut test_plugin = LoadedPlugin::load(test_path.as_ref(), 0, sample_rate, block_size)?;
    let test_latency = test_plugin.latency() as usize;
    eprintln!("  Latency: {} samples", test_latency);

    // Resolve parameter remaps for the test plugin
    let test_params = test_plugin.params();
    let resolved_remaps = if !remaps.is_empty() {
        let resolved = compressor::resolve_remaps(remaps, &test_params)?;
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

    // Build parameter sets
    let scenario_name = format!("atk-{}ms_rel-{}ms", attack_ms, release_ms);
    let mut ref_params = Vec::new();
    let mut test_params = Vec::new();

    // Threshold (ID 0 is commonly threshold in both plugins)
    if let Some(thr) = threshold_db {
        ref_params.push((0u32, thr));
        // Try to find and apply to test plugin if available
        if let Some(p) = test_plugin
            .params()
            .iter()
            .find(|p| p.name.eq_ignore_ascii_case("threshold"))
        {
            let normalized = (thr - p.min) / (p.max - p.min);
            test_params.push((p.id, normalized.clamp(0.0, 1.0)));
        }
    }

    // Ratio (ID 1 is commonly ratio)
    if let Some(r) = ratio {
        ref_params.push((1u32, r));
        if let Some(p) = test_plugin
            .params()
            .iter()
            .find(|p| p.name.eq_ignore_ascii_case("ratio"))
        {
            let normalized = (r - p.min) / (p.max - p.min);
            test_params.push((p.id, normalized.clamp(0.0, 1.0)));
        }
    }

    // Knee (ID 2 is commonly knee)
    if let Some(k) = knee_db {
        ref_params.push((2u32, k));
        if let Some(p) = test_plugin
            .params()
            .iter()
            .find(|p| p.name.eq_ignore_ascii_case("knee"))
        {
            let normalized = (k - p.min) / (p.max - p.min);
            test_params.push((p.id, normalized.clamp(0.0, 1.0)));
        }
    }

    // Attack and Release (with remapping support)
    // Try to set via remapping first, then fallback to common IDs
    if !resolved_remaps.is_empty() {
        // Attack
        if let Some(remap) = resolved_remaps
            .iter()
            .find(|r| r.to_name.eq_ignore_ascii_case("attack"))
        {
            test_params.push((remap.to_id, attack_ms));
        }
        // Release
        if let Some(remap) = resolved_remaps
            .iter()
            .find(|r| r.to_name.eq_ignore_ascii_case("release"))
        {
            test_params.push((remap.to_id, release_ms));
        }
    }

    // Fallback: use common IDs
    ref_params.push((7u32, attack_ms)); // Common Pro-C 3 attack ID
    ref_params.push((8u32, release_ms)); // Common Pro-C 3 release ID

    if !resolved_remaps
        .iter()
        .any(|r| r.to_name.eq_ignore_ascii_case("attack"))
    {
        // Look for attack param in test plugin
        if let Some(p) = test_plugin
            .params()
            .iter()
            .find(|p| p.name.eq_ignore_ascii_case("attack"))
        {
            test_params.push((p.id, attack_ms));
        }
    }
    if !resolved_remaps
        .iter()
        .any(|r| r.to_name.eq_ignore_ascii_case("release"))
    {
        // Look for release param in test plugin
        if let Some(p) = test_plugin
            .params()
            .iter()
            .find(|p| p.name.eq_ignore_ascii_case("release"))
        {
            test_params.push((p.id, release_ms));
        }
    }

    eprintln!("\nTest frequencies: 35 musically-spaced (20 Hz - 20 kHz)");
    let frequencies = crate::compressor::TEST_FREQUENCIES.to_vec();

    if !freq_filters.is_empty() {
        eprintln!("Filtered to: {:?} Hz", freq_filters);
    }

    // Test configuration
    let gain_high_db = -6.0_f32;
    let gain_low_db = -20.0_f32;
    let time_high_ms = 240.0_f32;
    let time_low_ms = 240.0_f32;
    let duration = 2.0_f32;
    let sr = sample_rate as f32;
    let total_samples = (sr * duration) as usize;
    let downsample_step = (sample_rate / 1000.0).round() as usize;
    let waveform = Waveform::Sine;

    eprintln!(
        "Signal: gain_high={:.1} dB, gain_low={:.1} dB, time_high={:.0}ms, time_low={:.0}ms",
        gain_high_db, gain_low_db, time_high_ms, time_low_ms,
    );
    eprintln!(
        "{} frequencies, tolerance: {:.2} dB\n",
        frequencies.len(),
        tolerance_db,
    );

    let mut overall_failures = 0;
    let mut overall_worst_rms = 0.0f32;
    let mut ref_all_gr: Vec<Vec<f32>> = Vec::with_capacity(frequencies.len());
    let mut test_all_gr: Vec<Vec<f32>> = Vec::with_capacity(frequencies.len());

    eprintln!("--- Scenario: {} ---", scenario_name);
    eprintln!("Reference params: {:?}", ref_params);
    eprintln!("Test params: {:?}\n", test_params);

    for (i, &freq_hz) in frequencies.iter().enumerate() {
        // Skip frequencies not in the filter (if any filters specified)
        if !freq_filters.is_empty() && !freq_filters.iter().any(|&f| (f - freq_hz).abs() < 0.5) {
            continue;
        }

        // Generate test signal
        let input = signal::pulse_tone(
            freq_hz,
            gain_high_db,
            gain_low_db,
            time_high_ms,
            time_low_ms,
            waveform,
            sr,
            total_samples + ref_latency.max(test_latency),
        );

        // Process through reference plugin
        let ref_output = ref_plugin.process(&input, &ref_params)?;
        let ref_gr = compute_gain_reduction_downsampled(
            &input[..total_samples],
            &ref_output[ref_latency..ref_latency + total_samples],
            downsample_step,
        );

        // Process through test plugin
        let test_output = test_plugin.process(&input, &test_params)?;
        let test_gr = compute_gain_reduction_downsampled(
            &input[..total_samples],
            &test_output[test_latency..test_latency + total_samples],
            downsample_step,
        );

        // Compare
        let cmp = compare_gr_curves(&ref_gr, &test_gr);
        let pass = cmp.rms_diff_db <= tolerance_db;

        if !pass {
            overall_failures += 1;
        }
        if cmp.rms_diff_db > overall_worst_rms {
            overall_worst_rms = cmp.rms_diff_db;
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

        ref_all_gr.push(ref_gr);
        test_all_gr.push(test_gr);
    }

    // Save results as binary
    let ref_bin_path = output_dir.join(format!("{}_reference.bin", scenario_name));
    write_scenario_bin(&ref_bin_path, &ref_all_gr)?;
    let test_bin_path = output_dir.join(format!("{}_test.bin", scenario_name));
    write_scenario_bin(&test_bin_path, &test_all_gr)?;

    // Summary
    let tested_freqs = if freq_filters.is_empty() {
        frequencies.len()
    } else {
        frequencies
            .iter()
            .filter(|&&f| freq_filters.iter().any(|&ff| (ff - f).abs() < 0.5))
            .count()
    };
    let passed = tested_freqs - overall_failures;

    eprintln!(
        "  -> {}/{} passed (worst RMS diff: {:.3} dB)\n",
        passed, tested_freqs, overall_worst_rms
    );

    eprintln!("========================================================");
    eprintln!("  Total: {}/{} passed", passed, tested_freqs);
    eprintln!("  Worst RMS GR diff: {:.3} dB", overall_worst_rms);
    eprintln!("  Tolerance: {:.2} dB", tolerance_db);
    eprintln!("========================================================");

    // Write metadata
    let meta = serde_json::json!({
        "reference_plugin": reference_path,
        "test_plugin": test_path,
        "scenario": scenario_name,
        "attack_ms": attack_ms,
        "release_ms": release_ms,
        "threshold_db": threshold_db,
        "ratio": ratio,
        "knee_db": knee_db,
        "tolerance_db": tolerance_db,
        "frequencies": frequencies,
        "ref_latency_samples": ref_latency,
        "test_latency_samples": test_latency,
        "passed": passed,
        "total": tested_freqs,
        "worst_rms_diff_db": overall_worst_rms,
    });
    std::fs::write(
        output_dir.join("metadata.json"),
        serde_json::to_string_pretty(&meta)?,
    )?;

    if overall_failures > 0 {
        anyhow::bail!(
            "{}/{} tests exceeded tolerance of {} dB",
            overall_failures,
            tested_freqs,
            tolerance_db,
        );
    }

    Ok(())
}
