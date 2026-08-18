//! EQ fuzz tester: randomizes shared parameters across two EQ plugins and
//! compares their frequency response using white noise + FFT.

use anyhow::{Context, Result};
use fts_analyzer::analysis;
use fts_analyzer::host::LoadedPlugin;
use fts_analyzer::signal;

/// Shared EQ parameter that both plugins expose (with different names).
struct SharedParam {
    /// Human-readable label for logging.
    label: &'static str,
    /// Parameter name in plugin A (e.g. Pro-Q 4).
    name_a: &'static str,
    /// Parameter name in plugin B (e.g. FTS-EQ).
    name_b: &'static str,
    /// Possible display-text values to randomize from.
    /// Both plugins must accept these via text_to_value.
    text_choices: Vec<String>,
}

/// Simple xorshift64 PRNG — no extra deps needed.
struct Rng(u64);

impl Rng {
    fn new(seed: u64) -> Self {
        Self(seed.max(1))
    }

    fn next_u64(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.0 = x;
        x
    }

    fn next_f64(&mut self) -> f64 {
        (self.next_u64() >> 11) as f64 / (1u64 << 53) as f64
    }

    fn choose<'a, T>(&mut self, items: &'a [T]) -> &'a T {
        let idx = self.next_u64() as usize % items.len();
        &items[idx]
    }
}

fn build_shared_params(num_bands: usize) -> Vec<SharedParam> {
    let freq_choices: Vec<String> = [
        "20 Hz", "50 Hz", "100 Hz", "200 Hz", "500 Hz", "1000 Hz", "2000 Hz", "5000 Hz",
        "10000 Hz", "15000 Hz", "20000 Hz",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();

    let gain_choices: Vec<String> = [
        "-30.0 dB", "-18.0 dB", "-12.0 dB", "-6.0 dB", "-3.0 dB", "0.0 dB", "3.0 dB", "6.0 dB",
        "12.0 dB", "18.0 dB", "30.0 dB",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();

    let q_choices: Vec<String> = [
        "0.10", "0.25", "0.50", "0.71", "1.00", "2.00", "5.00", "10.00", "18.00",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();

    // Shape values 0–9 map the same on both plugins (stepped int param)
    let shape_choices: Vec<String> = (0..=9).map(|v| v.to_string()).collect();

    // Slope values 0–10 map the same on both plugins (stepped int param)
    let slope_choices: Vec<String> = (0..=10).map(|v| v.to_string()).collect();

    let mut params = Vec::new();

    for band_idx in 1..=num_bands {
        let a_prefix = format!("Band {band_idx}");
        let b_prefix = format!("B{band_idx}");

        // We leak these strings so we can use &'static str in the struct.
        // This is fine for a CLI tool.
        let a_freq: &'static str = Box::leak(format!("{a_prefix} Frequency").into_boxed_str());
        let b_freq: &'static str = Box::leak(format!("{b_prefix} Freq").into_boxed_str());
        let a_gain: &'static str = Box::leak(format!("{a_prefix} Gain").into_boxed_str());
        let b_gain: &'static str = Box::leak(format!("{b_prefix} Gain").into_boxed_str());
        let a_q: &'static str = Box::leak(format!("{a_prefix} Q").into_boxed_str());
        let b_q: &'static str = Box::leak(format!("{b_prefix} Q").into_boxed_str());
        let a_shape: &'static str = Box::leak(format!("{a_prefix} Shape").into_boxed_str());
        let b_shape: &'static str = Box::leak(format!("{b_prefix} Type").into_boxed_str());
        let a_slope: &'static str = Box::leak(format!("{a_prefix} Slope").into_boxed_str());
        let b_slope: &'static str = Box::leak(format!("{b_prefix} Slope").into_boxed_str());
        let label_freq: &'static str = Box::leak(format!("Band {band_idx} Freq").into_boxed_str());
        let label_gain: &'static str = Box::leak(format!("Band {band_idx} Gain").into_boxed_str());
        let label_q: &'static str = Box::leak(format!("Band {band_idx} Q").into_boxed_str());
        let label_shape: &'static str =
            Box::leak(format!("Band {band_idx} Shape").into_boxed_str());
        let label_slope: &'static str =
            Box::leak(format!("Band {band_idx} Slope").into_boxed_str());

        params.push(SharedParam {
            label: label_freq,
            name_a: a_freq,
            name_b: b_freq,
            text_choices: freq_choices.clone(),
        });
        params.push(SharedParam {
            label: label_gain,
            name_a: a_gain,
            name_b: b_gain,
            text_choices: gain_choices.clone(),
        });
        params.push(SharedParam {
            label: label_q,
            name_a: a_q,
            name_b: b_q,
            text_choices: q_choices.clone(),
        });
        params.push(SharedParam {
            label: label_shape,
            name_a: a_shape,
            name_b: b_shape,
            text_choices: shape_choices.clone(),
        });
        params.push(SharedParam {
            label: label_slope,
            name_a: a_slope,
            name_b: b_slope,
            text_choices: slope_choices.clone(),
        });
    }

    params
}

pub fn run_fuzz_eq(
    path_a: &str,
    path_b: &str,
    iterations: usize,
    sample_rate: f64,
    block_size: u32,
    duration: f32,
    tolerance_db: f32,
    seed: u64,
    num_bands: usize,
) -> Result<()> {
    let num_bands = num_bands.min(24);

    eprintln!("Loading plugin A: {}", path_a);
    let mut plugin_a = LoadedPlugin::load(path_a.as_ref(), 0, sample_rate, block_size)?;

    eprintln!("Loading plugin B: {}", path_b);
    let mut plugin_b = LoadedPlugin::load(path_b.as_ref(), 0, sample_rate, block_size)?;

    let params_a = plugin_a.params();
    let params_b = plugin_b.params();

    // Build shared parameter definitions
    let shared_params = build_shared_params(num_bands);

    // Resolve param IDs for both plugins
    struct ResolvedShared {
        label: &'static str,
        id_a: u32,
        id_b: u32,
        text_choices: Vec<String>,
    }

    let mut resolved: Vec<ResolvedShared> = Vec::new();
    for sp in &shared_params {
        let info_a = params_a.iter().find(|p| p.name == sp.name_a);
        let info_b = params_b.iter().find(|p| p.name == sp.name_b);

        match (info_a, info_b) {
            (Some(a), Some(b)) => {
                resolved.push(ResolvedShared {
                    label: sp.label,
                    id_a: a.id,
                    id_b: b.id,
                    text_choices: sp.text_choices.clone(),
                });
            }
            (None, _) => {
                eprintln!(
                    "  Warning: param '{}' not found in plugin A, skipping",
                    sp.name_a
                );
            }
            (_, None) => {
                eprintln!(
                    "  Warning: param '{}' not found in plugin B, skipping",
                    sp.name_b
                );
            }
        }
    }

    eprintln!(
        "\nFuzz test: {} shared params, {} iterations, {} bands, seed={}",
        resolved.len(),
        iterations,
        num_bands,
        seed,
    );
    eprintln!(
        "Signal: white noise, {} Hz, {}s, block={}",
        sample_rate, duration, block_size,
    );
    eprintln!(
        "Tolerance: {} dB RMS freq response difference\n",
        tolerance_db
    );

    // Also need to enable the bands on both plugins
    // Pro-Q 4 uses "Band N Enabled", FTS-EQ uses "BN On"
    let mut enable_overrides_a: Vec<(u32, f64)> = Vec::new();
    let mut enable_overrides_b: Vec<(u32, f64)> = Vec::new();
    for band_idx in 1..=num_bands {
        let a_name = format!("Band {} Enabled", band_idx);
        let b_name = format!("B{} On", band_idx);
        // Pro-Q 4 also has "Band N Used" which must be set
        let a_used_name = format!("Band {} Used", band_idx);

        if let Some(p) = params_a.iter().find(|p| p.name == a_name) {
            enable_overrides_a.push((p.id, 1.0));
        }
        if let Some(p) = params_a.iter().find(|p| p.name == a_used_name) {
            enable_overrides_a.push((p.id, 1.0));
        }
        if let Some(p) = params_b.iter().find(|p| p.name == b_name) {
            enable_overrides_b.push((p.id, 1.0));
        }
    }

    let total_samples = (sample_rate as f32 * duration) as usize;
    let mut rng = Rng::new(seed);
    let mut failures = 0;
    let mut worst_rms_diff = 0.0f32;
    let mut worst_max_diff = 0.0f32;
    let mut best_rms_diff = f32::INFINITY;
    let mut sum_rms_diff = 0.0f64;

    for iter in 0..iterations {
        // Generate fresh white noise for each iteration
        let input = signal::white_noise(total_samples, rng.next_u64());

        // Randomize parameters
        let mut overrides_a = enable_overrides_a.clone();
        let mut overrides_b = enable_overrides_b.clone();
        let mut param_log = Vec::new();

        for rp in &resolved {
            let text = rng.choose(&rp.text_choices);

            let val_a = plugin_a.text_to_value(rp.id_a, text);
            let val_b = plugin_b.text_to_value(rp.id_b, text);

            match (val_a, val_b) {
                (Some(va), Some(vb)) => {
                    overrides_a.push((rp.id_a, va));
                    overrides_b.push((rp.id_b, vb));
                    param_log.push(format!("{}={}", rp.label, text));
                }
                _ => {
                    // Skip params that can't parse this text value
                }
            }
        }

        // Process through both plugins
        let out_a = plugin_a
            .process(&input, &overrides_a)
            .context("plugin A process failed")?;
        let out_b = plugin_b
            .process(&input, &overrides_b)
            .context("plugin B process failed")?;

        // FFT-based frequency response comparison (20 Hz – 20 kHz)
        let freq_cmp =
            analysis::compare_freq_response(&input, &out_a, &out_b, sample_rate, 20.0, 20000.0);

        let rms_diff = freq_cmp.rms_diff_db;
        let max_diff = freq_cmp.max_diff_db;

        if rms_diff.is_finite() {
            sum_rms_diff += rms_diff as f64;
            if rms_diff > worst_rms_diff {
                worst_rms_diff = rms_diff;
            }
            if rms_diff < best_rms_diff {
                best_rms_diff = rms_diff;
            }
        }
        if max_diff > worst_max_diff && max_diff.is_finite() {
            worst_max_diff = max_diff;
        }

        let pass = rms_diff.is_finite() && rms_diff <= tolerance_db;
        if !pass {
            failures += 1;
        }

        let status = if pass { "PASS" } else { "FAIL" };

        // Always print failures, print passes on first/last/every 10th
        if !pass || iter == 0 || iter == iterations - 1 || (iter + 1) % 10 == 0 {
            eprintln!(
                "[{:>4}/{}] {} rms_diff={:.2} dB  max_diff={:.2} dB @ {:.0} Hz  ({} bins)",
                iter + 1,
                iterations,
                status,
                rms_diff,
                max_diff,
                freq_cmp.max_diff_freq_hz,
                freq_cmp.bins.len(),
            );
        }

        if !pass {
            // Print the params that caused the failure
            for p in &param_log {
                eprintln!("         {}", p);
            }
            // Print the top-5 worst frequency bins
            let mut worst_bins: Vec<&analysis::FreqBin> = freq_cmp.bins.iter().collect();
            worst_bins.sort_by(|a, b| {
                b.diff_db
                    .abs()
                    .partial_cmp(&a.diff_db.abs())
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
            for bin in worst_bins.iter().take(5) {
                eprintln!(
                    "         {:.0} Hz: A={:.2} dB  B={:.2} dB  delta={:+.2} dB",
                    bin.freq_hz, bin.a_db, bin.b_db, bin.diff_db,
                );
            }
        }
    }

    // Summary
    let avg_rms_diff = if iterations > failures {
        sum_rms_diff / (iterations - failures) as f64
    } else {
        f64::NAN
    };
    eprintln!("\n════════════════════════════════════════════════════");
    eprintln!(
        "  Fuzz EQ test complete: {}/{} passed",
        iterations - failures,
        iterations
    );
    eprintln!("  Freq response RMS diff — best:  {:.2} dB", best_rms_diff);
    eprintln!("  Freq response RMS diff — worst: {:.2} dB", worst_rms_diff);
    eprintln!("  Freq response RMS diff — avg:   {:.2} dB", avg_rms_diff);
    eprintln!("  Worst single-bin diff:          {:.2} dB", worst_max_diff);
    eprintln!("════════════════════════════════════════════════════");

    if failures > 0 {
        anyhow::bail!(
            "{} / {} iterations exceeded tolerance of {} dB",
            failures,
            iterations,
            tolerance_db
        );
    }

    Ok(())
}
