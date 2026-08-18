//! EQ reference capture and comparison.
//!
//! Captures Pro-Q 4's frequency response (magnitude + phase) for every filter
//! configuration that fts-eq supports, plus multi-band interaction scenarios.
//!
//! Runs at both 48 kHz and 96 kHz to capture Nyquist cramping differences.
//!
//! Storage: one binary file per scenario containing magnitude, phase, and group delay.
//! Layout: [num_bins: u32 LE][(freq_hz: f32, mag_db: f32, phase_rad: f32, gd_samples: f32) × num_bins]
//! Legacy 12-byte format (without group delay) is auto-detected and upgraded on read.

use std::io::Write;
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use anyhow::{Context, Result};
use fts_analyzer::analysis::{self, ResponseBin};
use fts_analyzer::host::LoadedPlugin;
use fts_analyzer::signal;

// ---------------------------------------------------------------------------
// Filter types (matches Pro-Q 4 Shape / FTS-EQ Type)
// ---------------------------------------------------------------------------

const FILTER_NAMES: &[&str] = &[
    "bell",             // 0
    "low_shelf",        // 1
    "low_cut",          // 2 (highpass)
    "high_shelf",       // 3
    "high_cut",         // 4 (lowpass)
    "notch",            // 5
    "bandpass",         // 6
    "tilt_shelf",       // 7
    "flat_tilt",        // 8
    "allpass",          // 9
    "bandpass_variant", // 10 (narrow bandpass)
    "band_shelf",       // 11
    "shelf_alt",        // 12
];

/// Max shape index to try during capture. Shapes that the plugin doesn't
/// accept will produce silent (0 dB) output and get skipped.
const MAX_SHAPE: u32 = 12;

fn filter_uses_gain(shape: u32) -> bool {
    matches!(shape, 0 | 1 | 3 | 7 | 8 | 11 | 12)
}

fn filter_uses_q(shape: u32) -> bool {
    matches!(shape, 0 | 1 | 2 | 3 | 4 | 5 | 6 | 9 | 10 | 11 | 12)
}

fn filter_uses_slope(shape: u32) -> bool {
    !matches!(shape, 8)
}

// ---------------------------------------------------------------------------
// Test grid
// ---------------------------------------------------------------------------

/// Test frequencies — dense near Nyquist to catch cramping.
const TEST_FREQS: &[f32] = &[
    10.0, 20.0, 50.0, 60.0, 100.0, 120.0, 200.0, 250.0, 500.0, 1000.0, 2000.0, 3000.0, 4000.0,
    5000.0, 6000.0, 8000.0, 10000.0, 12000.0, 14000.0, 16000.0, 17000.0, 18000.0, 19000.0, 20000.0,
    21000.0, 22000.0,
];

const TEST_GAINS: &[f32] = &[-12.0, -6.0, 6.0, 12.0];
const TEST_QS: &[f32] = &[0.5, 1.0, 4.0, 10.0];
const TEST_SLOPES: &[u32] = &[0, 2, 5, 8]; // 6dB, 18dB, 36dB, 72dB/oct

/// An EQ test scenario — single-band or multi-band.
#[derive(Debug, Clone)]
pub struct EqScenario {
    pub name: String,
    /// Per-band configurations.
    pub bands: Vec<BandConfig>,
}

/// Configuration for one EQ band.
#[derive(Debug, Clone)]
pub struct BandConfig {
    pub shape: u32,
    pub freq_hz: f32,
    pub gain_db: f32,
    pub q: f32,
    pub slope: u32,
}

/// Build the full scenario matrix: single-band + multi-band.
pub fn build_eq_scenarios() -> Vec<EqScenario> {
    let mut scenarios = Vec::new();

    // --- Single-band scenarios ---
    for shape in 0..=MAX_SHAPE {
        let shape_name = FILTER_NAMES[shape as usize];
        let uses_gain = filter_uses_gain(shape);
        let uses_q = filter_uses_q(shape);
        let uses_slope = filter_uses_slope(shape);

        let gains: &[f32] = if uses_gain { TEST_GAINS } else { &[0.0] };
        let qs: &[f32] = if uses_q { TEST_QS } else { &[1.0] };
        let slopes: &[u32] = if uses_slope { TEST_SLOPES } else { &[2] };

        for &freq in TEST_FREQS {
            for &gain in gains {
                for &q in qs {
                    for &slope in slopes {
                        let name = if uses_gain {
                            format!(
                                "{}_{}hz_{:+.0}db_q{}_s{}",
                                shape_name, freq as u32, gain, q, slope,
                            )
                        } else {
                            format!("{}_{}hz_q{}_s{}", shape_name, freq as u32, q, slope,)
                        };

                        scenarios.push(EqScenario {
                            name,
                            bands: vec![BandConfig {
                                shape,
                                freq_hz: freq,
                                gain_db: gain,
                                q,
                                slope,
                            }],
                        });
                    }
                }
            }
        }
    }

    // --- Multi-band interaction scenarios ---
    build_multiband_scenarios(&mut scenarios);

    scenarios
}

/// Common multi-band combinations that test interaction effects.
fn build_multiband_scenarios(scenarios: &mut Vec<EqScenario>) {
    // High shelf + low shelf (classic tonal shaping)
    for &gain in &[-6.0f32, 6.0] {
        scenarios.push(EqScenario {
            name: format!("multi_loshelf200_{:+.0}db_hishelf8k_{:+.0}db", gain, gain),
            bands: vec![
                BandConfig {
                    shape: 1,
                    freq_hz: 200.0,
                    gain_db: gain,
                    q: 1.0,
                    slope: 2,
                },
                BandConfig {
                    shape: 3,
                    freq_hz: 8000.0,
                    gain_db: gain,
                    q: 1.0,
                    slope: 2,
                },
            ],
        });
        // Opposing shelves (tilt-like)
        scenarios.push(EqScenario {
            name: format!("multi_loshelf200_{:+.0}db_hishelf8k_{:+.0}db", gain, -gain),
            bands: vec![
                BandConfig {
                    shape: 1,
                    freq_hz: 200.0,
                    gain_db: gain,
                    q: 1.0,
                    slope: 2,
                },
                BandConfig {
                    shape: 3,
                    freq_hz: 8000.0,
                    gain_db: -gain,
                    q: 1.0,
                    slope: 2,
                },
            ],
        });
    }

    // Two bells close together (interaction / comb effects)
    for &sep in &[100.0f32, 500.0, 2000.0] {
        let f1 = 1000.0;
        let f2 = f1 + sep;
        for &gain in &[-6.0f32, 6.0] {
            scenarios.push(EqScenario {
                name: format!(
                    "multi_bell{}hz_bell{}hz_{:+.0}db_q1",
                    f1 as u32, f2 as u32, gain,
                ),
                bands: vec![
                    BandConfig {
                        shape: 0,
                        freq_hz: f1,
                        gain_db: gain,
                        q: 1.0,
                        slope: 2,
                    },
                    BandConfig {
                        shape: 0,
                        freq_hz: f2,
                        gain_db: gain,
                        q: 1.0,
                        slope: 2,
                    },
                ],
            });
        }
    }

    // Three-band "smiley" and "frown" curves
    scenarios.push(EqScenario {
        name: "multi_smiley_lo+6_mid-3_hi+6".into(),
        bands: vec![
            BandConfig {
                shape: 1,
                freq_hz: 100.0,
                gain_db: 6.0,
                q: 1.0,
                slope: 2,
            },
            BandConfig {
                shape: 0,
                freq_hz: 1000.0,
                gain_db: -3.0,
                q: 1.0,
                slope: 2,
            },
            BandConfig {
                shape: 3,
                freq_hz: 10000.0,
                gain_db: 6.0,
                q: 1.0,
                slope: 2,
            },
        ],
    });
    scenarios.push(EqScenario {
        name: "multi_frown_lo-6_mid+3_hi-6".into(),
        bands: vec![
            BandConfig {
                shape: 1,
                freq_hz: 100.0,
                gain_db: -6.0,
                q: 1.0,
                slope: 2,
            },
            BandConfig {
                shape: 0,
                freq_hz: 1000.0,
                gain_db: 3.0,
                q: 1.0,
                slope: 2,
            },
            BandConfig {
                shape: 3,
                freq_hz: 10000.0,
                gain_db: -6.0,
                q: 1.0,
                slope: 2,
            },
        ],
    });

    // Low cut + high cut (bandpass via cuts)
    for &slope in &[2u32, 5, 8] {
        scenarios.push(EqScenario {
            name: format!("multi_locut80_hicut16k_s{}", slope),
            bands: vec![
                BandConfig {
                    shape: 2,
                    freq_hz: 80.0,
                    gain_db: 0.0,
                    q: 1.0,
                    slope,
                },
                BandConfig {
                    shape: 4,
                    freq_hz: 16000.0,
                    gain_db: 0.0,
                    q: 1.0,
                    slope,
                },
            ],
        });
    }

    // Notch + bell rescue (surgical correction)
    scenarios.push(EqScenario {
        name: "multi_notch500_bell500+6_q10".into(),
        bands: vec![
            BandConfig {
                shape: 5,
                freq_hz: 500.0,
                gain_db: 0.0,
                q: 10.0,
                slope: 2,
            },
            BandConfig {
                shape: 0,
                freq_hz: 500.0,
                gain_db: 6.0,
                q: 10.0,
                slope: 2,
            },
        ],
    });

    // High-frequency bell stacking (Nyquist interaction)
    for &freq in &[16000.0f32, 18000.0, 20000.0] {
        scenarios.push(EqScenario {
            name: format!("multi_2xbell{}hz_+6db_q4", freq as u32),
            bands: vec![
                BandConfig {
                    shape: 0,
                    freq_hz: freq,
                    gain_db: 6.0,
                    q: 4.0,
                    slope: 2,
                },
                BandConfig {
                    shape: 0,
                    freq_hz: freq,
                    gain_db: 6.0,
                    q: 4.0,
                    slope: 2,
                },
            ],
        });
    }
}

// ---------------------------------------------------------------------------
// Pro-Q 4 band parameter IDs (resolved at runtime)
// ---------------------------------------------------------------------------

struct ProQ4BandIds {
    shape: u32,
    freq: u32,
    gain: u32,
    q: u32,
    slope: u32,
    enabled: u32,
    used: u32,
}

fn resolve_proq4_band_ids(
    params: &[fts_analyzer::host::ParamInfo],
    band_num: usize,
) -> Result<ProQ4BandIds> {
    let find = |name: &str| -> Result<u32> {
        params
            .iter()
            .find(|p| p.name == name)
            .map(|p| p.id)
            .with_context(|| format!("missing '{}'", name))
    };

    Ok(ProQ4BandIds {
        shape: find(&format!("Band {} Shape", band_num))?,
        freq: find(&format!("Band {} Frequency", band_num))?,
        gain: find(&format!("Band {} Gain", band_num))?,
        q: find(&format!("Band {} Q", band_num))?,
        slope: find(&format!("Band {} Slope", band_num))?,
        enabled: find(&format!("Band {} Enabled", band_num))?,
        used: find(&format!("Band {} Used", band_num))?,
    })
}

// ---------------------------------------------------------------------------
// Capture
// ---------------------------------------------------------------------------

/// Sample rates to capture at.
const CAPTURE_SAMPLE_RATES: &[f64] = &[48000.0, 96000.0];

/// Capture Pro-Q 4 frequency response across all scenarios at both sample rates.
pub fn run_capture_eq(
    plugin_path: &str,
    output_dir: &Path,
    block_size: u32,
    duration: f32,
    base_params: &[(u32, f64)],
    scenarios: &[EqScenario],
) -> Result<()> {
    std::fs::create_dir_all(output_dir)?;

    // Determine max bands needed across all scenarios
    let max_bands = scenarios.iter().map(|s| s.bands.len()).max().unwrap_or(1);

    for &sample_rate in CAPTURE_SAMPLE_RATES {
        let sr_label = format!("{}k", sample_rate as u32 / 1000);
        let sr_dir = output_dir.join(&sr_label);
        std::fs::create_dir_all(&sr_dir)?;

        eprintln!("=== Capturing at {} Hz ===", sample_rate);
        eprintln!("Loading plugin: {}", plugin_path);
        let mut probe = LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)?;
        let params = probe.params();
        drop(probe);

        // Resolve band IDs for as many bands as we need
        let band_ids: Vec<ProQ4BandIds> = (1..=max_bands)
            .map(|i| resolve_proq4_band_ids(&params, i))
            .collect::<Result<_>>()?;

        let num_threads = std::thread::available_parallelism()
            .map(|n| n.get().min(8))
            .unwrap_or(4);

        let max_freq = sample_rate as f32 / 2.0;

        eprintln!(
            "{} scenarios, impulse→FFT (size {}), {} threads\n",
            scenarios.len(),
            IR_FFT_SIZE,
            num_threads,
        );
        let _ = duration;

        // Write metadata
        let meta = serde_json::json!({
            "plugin_path": plugin_path,
            "sample_rate": sample_rate,
            "block_size": block_size,
            "duration": duration,
            "fft_size": IR_FFT_SIZE,
            "measurement": "deterministic_impulse_response_fft",
            "includes_phase": true,
            "base_params": base_params.iter().map(|(id, v)| serde_json::json!({ "id": id, "value": v })).collect::<Vec<_>>(),
            "scenarios": scenarios.iter().map(|s| serde_json::json!({
                "name": &s.name,
                "bands": s.bands.iter().map(|b| serde_json::json!({
                    "shape": b.shape,
                    "freq_hz": b.freq_hz,
                    "gain_db": b.gain_db,
                    "q": b.q,
                    "slope": b.slope,
                })).collect::<Vec<_>>(),
            })).collect::<Vec<_>>(),
        });
        std::fs::write(
            sr_dir.join("metadata.json"),
            serde_json::to_string_pretty(&meta)?,
        )?;

        let completed = Arc::new(AtomicUsize::new(0));
        let total = scenarios.len();
        let load_lock = Arc::new(Mutex::new(()));

        let total_samples = (sample_rate as f32 * duration) as usize;
        let noise = signal::white_noise(total_samples, 12345);

        let chunk_size = scenarios.len().div_ceil(num_threads);
        let chunks: Vec<&[EqScenario]> = scenarios.chunks(chunk_size).collect();

        // Collect band_ids info we need to pass to threads
        let band_shape_ids: Vec<u32> = band_ids.iter().map(|b| b.shape).collect();
        let band_freq_ids: Vec<u32> = band_ids.iter().map(|b| b.freq).collect();
        let band_gain_ids: Vec<u32> = band_ids.iter().map(|b| b.gain).collect();
        let band_q_ids: Vec<u32> = band_ids.iter().map(|b| b.q).collect();
        let band_slope_ids: Vec<u32> = band_ids.iter().map(|b| b.slope).collect();
        let band_enabled_ids: Vec<u32> = band_ids.iter().map(|b| b.enabled).collect();
        let band_used_ids: Vec<u32> = band_ids.iter().map(|b| b.used).collect();

        eprintln!("Launching {} worker threads...\n", chunks.len());

        let sr_dir_ref = &sr_dir;
        let errors: Vec<String> = std::thread::scope(|s| {
            let handles: Vec<_> = chunks
                .into_iter()
                .map(|chunk| {
                    let load_lock = Arc::clone(&load_lock);
                    let completed = Arc::clone(&completed);
                    let noise = &noise;
                    let band_shape_ids = &band_shape_ids;
                    let band_freq_ids = &band_freq_ids;
                    let band_gain_ids = &band_gain_ids;
                    let band_q_ids = &band_q_ids;
                    let band_slope_ids = &band_slope_ids;
                    let band_enabled_ids = &band_enabled_ids;
                    let band_used_ids = &band_used_ids;
                    s.spawn(move || {
                        let mut plugin = {
                            let _guard = load_lock.lock().unwrap();
                            eprintln!("  Loading plugin instance for {} scenarios...", chunk.len());
                            match LoadedPlugin::load(
                                plugin_path.as_ref(),
                                0,
                                sample_rate,
                                block_size,
                            ) {
                                Ok(p) => p,
                                Err(e) => {
                                    return chunk
                                        .iter()
                                        .map(|sc| format!("{}: load failed: {}", sc.name, e))
                                        .collect::<Vec<_>>();
                                }
                            }
                        };

                        let mut thread_errors = Vec::new();
                        for scenario in chunk {
                            let result = capture_eq_scenario(
                                &mut plugin,
                                noise,
                                sr_dir_ref,
                                sample_rate,
                                max_freq,
                                scenario,
                                base_params,
                                band_shape_ids,
                                band_freq_ids,
                                band_gain_ids,
                                band_q_ids,
                                band_slope_ids,
                                band_enabled_ids,
                                band_used_ids,
                            );

                            let done = completed.fetch_add(1, Ordering::Relaxed) + 1;
                            match result {
                                Ok(_) => {
                                    if done.is_multiple_of(100) || done == total {
                                        eprintln!("[{:>5}/{}]", done, total);
                                    }
                                }
                                Err(e) => {
                                    eprintln!(
                                        "[{:>5}/{}] FAIL {} — {}",
                                        done, total, scenario.name, e
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
            for e in errors.iter().take(20) {
                eprintln!("  {}", e);
            }
            anyhow::bail!("{} scenario(s) failed at {} Hz", errors.len(), sample_rate);
        }

        eprintln!("\nWriting CSV export...");
        write_eq_csv(&sr_dir, scenarios)?;

        let total_size = dir_size(&sr_dir);
        eprintln!(
            "Results saved to {} ({:.1} MB)\n",
            sr_dir.display(),
            total_size as f64 / 1_048_576.0,
        );
    }

    Ok(())
}

/// Length of the impulse-response FFT. 4096 samples gives 11.72 Hz bin spacing
/// at 48 kHz and 5.86 Hz at 96 kHz — matches the legacy noise-stimulus resolution.
const IR_FFT_SIZE: usize = 4096;

/// Total samples processed per scenario. Must be >= plugin_latency + IR_FFT_SIZE.
/// 16384 gives a comfortable margin for plugin latencies up to ~260 ms at 48 kHz.
const IR_PROCESS_LEN: usize = 16384;

/// Capture a single EQ scenario using **impulse response + FFT** (deterministic).
///
/// Previously used white noise + Welch cross-spectrum averaging, which introduced
/// ~±0.08 dB measurement noise unrelated to Pro-Q 4's actual output. Impulse response
/// is noise-free: the FFT of the filter's impulse response IS its transfer function.
#[allow(clippy::too_many_arguments)]
fn capture_eq_scenario(
    plugin: &mut LoadedPlugin,
    _noise: &[f32],
    output_dir: &Path,
    sample_rate: f64,
    max_freq: f32,
    scenario: &EqScenario,
    base_params: &[(u32, f64)],
    shape_ids: &[u32],
    freq_ids: &[u32],
    gain_ids: &[u32],
    q_ids: &[u32],
    slope_ids: &[u32],
    enabled_ids: &[u32],
    used_ids: &[u32],
) -> Result<()> {
    let mut overrides: Vec<(u32, f64)> = base_params.to_vec();

    for (i, band) in scenario.bands.iter().enumerate() {
        overrides.push((enabled_ids[i], 1.0));
        overrides.push((used_ids[i], 1.0));
        overrides.push((shape_ids[i], band.shape as f64));
        overrides.push((slope_ids[i], band.slope as f64));

        if let Some(v) = plugin.text_to_value(freq_ids[i], &format!("{} Hz", band.freq_hz)) {
            overrides.push((freq_ids[i], v));
        } else {
            anyhow::bail!(
                "band {}: failed to resolve freq '{}' Hz",
                i + 1,
                band.freq_hz
            );
        }

        if let Some(v) = plugin.text_to_value(gain_ids[i], &format!("{:.1} dB", band.gain_db)) {
            overrides.push((gain_ids[i], v));
        } else {
            anyhow::bail!(
                "band {}: failed to resolve gain '{:.1}' dB",
                i + 1,
                band.gain_db
            );
        }

        if let Some(v) = plugin.text_to_value(q_ids[i], &format!("{:.2}", band.q)) {
            overrides.push((q_ids[i], v));
        } else {
            anyhow::bail!("band {}: failed to resolve Q '{:.2}'", i + 1, band.q);
        }
    }

    // Generate impulse: [1.0, 0, 0, ...] of length IR_PROCESS_LEN.
    let mut impulse = vec![0.0f32; IR_PROCESS_LEN];
    impulse[0] = 1.0;

    let output = plugin.process(&impulse, &overrides)?;

    // Skip plugin latency samples, then take IR_FFT_SIZE samples as the impulse response.
    let latency = plugin.latency() as usize;
    if output.len() < latency + IR_FFT_SIZE {
        anyhow::bail!(
            "plugin output too short for scenario {} (need latency {} + fft {} = {}, got {})",
            scenario.name,
            latency,
            IR_FFT_SIZE,
            latency + IR_FFT_SIZE,
            output.len()
        );
    }
    let ir: Vec<f32> = output[latency..latency + IR_FFT_SIZE].to_vec();

    // Deterministic FFT — no statistical noise.
    let tf = analysis::transfer_function_from_impulse(&ir, sample_rate, 10.0, max_freq);

    let bin_path = output_dir.join(format!("{}.bin", scenario.name));
    write_tf_bin(&bin_path, &tf)?;

    Ok(())
}

// ---------------------------------------------------------------------------
// Compare
// ---------------------------------------------------------------------------

/// Result of a single scenario comparison.
#[allow(dead_code)]
struct CompareResult {
    name: String,
    filter_type: String,
    freq_hz: f32,
    gain_db: f32,
    q: f32,
    slope: u32,
    /// Magnitude RMS difference (dB).
    mag_rms_diff: f32,
    /// Magnitude max difference (dB).
    mag_max_diff: f32,
    /// Phase RMS difference (radians).
    phase_rms_diff: f32,
    /// Phase max difference (radians).
    phase_max_diff: f32,
    /// Group delay RMS difference (samples).
    gd_rms_diff: f32,
    /// Group delay max difference (samples).
    gd_max_diff: f32,
    pass: bool,
    is_multi: bool,
}

/// Compare a plugin's EQ response against saved reference data.
pub fn run_compare_eq(
    plugin_path: &str,
    reference_dir: &Path,
    output_dir: &Path,
    sample_rate: f64,
    block_size: u32,
    duration: f32,
    base_params: &[(u32, f64)],
    tolerance_db: f32,
    filters: Option<&[&str]>,
    report_path: Option<&Path>,
) -> Result<()> {
    std::fs::create_dir_all(output_dir)?;

    // Find the right sample rate subdirectory
    let sr_label = format!("{}k", sample_rate as u32 / 1000);
    let ref_sr_dir = reference_dir.join(&sr_label);
    if !ref_sr_dir.exists() {
        anyhow::bail!(
            "no reference data for {} Hz (expected {})",
            sample_rate,
            ref_sr_dir.display(),
        );
    }

    let meta_path = ref_sr_dir.join("metadata.json");
    let meta: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(&meta_path)
            .with_context(|| format!("no metadata.json in {}", ref_sr_dir.display()))?,
    )?;

    let scenarios: Vec<EqScenario> = meta["scenarios"]
        .as_array()
        .context("no scenarios in metadata")?
        .iter()
        .map(|s| {
            let bands: Vec<BandConfig> = s["bands"]
                .as_array()
                .map(|arr| {
                    arr.iter()
                        .map(|b| BandConfig {
                            shape: b["shape"].as_u64().unwrap_or(0) as u32,
                            freq_hz: b["freq_hz"].as_f64().unwrap_or(1000.0) as f32,
                            gain_db: b["gain_db"].as_f64().unwrap_or(0.0) as f32,
                            q: b["q"].as_f64().unwrap_or(1.0) as f32,
                            slope: b["slope"].as_u64().unwrap_or(2) as u32,
                        })
                        .collect()
                })
                .unwrap_or_default();

            EqScenario {
                name: s["name"].as_str().unwrap_or("").to_string(),
                bands,
            }
        })
        .collect();

    // Apply scenario filters if provided
    let scenarios: Vec<EqScenario> = if let Some(filters) = filters {
        scenarios
            .into_iter()
            .filter(|s| filters.iter().any(|f| s.name.contains(f)))
            .collect()
    } else {
        scenarios
    };

    let max_freq = sample_rate as f32 / 2.0;

    eprintln!("Reference: {}", ref_sr_dir.display());
    eprintln!("Loading plugin: {}", plugin_path);
    let mut plugin = LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)?;
    let params = plugin.params();

    // Resolve FTS-EQ parameter IDs
    let max_bands = scenarios.iter().map(|s| s.bands.len()).max().unwrap_or(1);
    let find_param =
        |name: &str| -> Option<u32> { params.iter().find(|p| p.name == name).map(|p| p.id) };

    let mut b_type_ids = Vec::new();
    let mut b_freq_ids = Vec::new();
    let mut b_gain_ids = Vec::new();
    let mut b_q_ids = Vec::new();
    let mut b_slope_ids = Vec::new();
    let mut b_on_ids = Vec::new();

    for i in 1..=max_bands {
        b_type_ids
            .push(find_param(&format!("B{} Type", i)).context(format!("missing B{} Type", i))?);
        b_freq_ids
            .push(find_param(&format!("B{} Freq", i)).context(format!("missing B{} Freq", i))?);
        b_gain_ids
            .push(find_param(&format!("B{} Gain", i)).context(format!("missing B{} Gain", i))?);
        b_q_ids.push(find_param(&format!("B{} Q", i)).context(format!("missing B{} Q", i))?);
        b_slope_ids
            .push(find_param(&format!("B{} Slope", i)).context(format!("missing B{} Slope", i))?);
        b_on_ids.push(find_param(&format!("B{} On", i)).context(format!("missing B{} On", i))?);
    }

    let total_samples = (sample_rate as f32 * duration) as usize;
    let noise = signal::white_noise(total_samples, 12345);

    if let Some(filters) = filters {
        eprintln!(
            "{} scenarios (filtered by: {}), tolerance: {:.2} dB\n",
            scenarios.len(),
            filters.join(", "),
            tolerance_db,
        );
    } else {
        eprintln!(
            "{} scenarios, tolerance: {:.2} dB\n",
            scenarios.len(),
            tolerance_db,
        );
    }

    let mut results: Vec<CompareResult> = Vec::with_capacity(scenarios.len());

    for (i, scenario) in scenarios.iter().enumerate() {
        let ref_bin = ref_sr_dir.join(format!("{}.bin", scenario.name));
        if !ref_bin.exists() {
            eprintln!("  Warning: missing {}, skipping", scenario.name);
            continue;
        }

        let ref_tf = read_tf_bin(&ref_bin)?;

        // Set up FTS-EQ params
        let mut overrides: Vec<(u32, f64)> = base_params.to_vec();
        for (j, band) in scenario.bands.iter().enumerate() {
            overrides.push((b_on_ids[j], 1.0));
            overrides.push((b_type_ids[j], band.shape as f64));
            overrides.push((b_slope_ids[j], band.slope as f64));

            if let Some(v) = plugin.text_to_value(b_freq_ids[j], &format!("{} Hz", band.freq_hz)) {
                overrides.push((b_freq_ids[j], v));
            }
            if let Some(v) = plugin.text_to_value(b_gain_ids[j], &format!("{:.1} dB", band.gain_db))
            {
                overrides.push((b_gain_ids[j], v));
            }
            if let Some(v) = plugin.text_to_value(b_q_ids[j], &format!("{:.2}", band.q)) {
                overrides.push((b_q_ids[j], v));
            }
        }

        let output = plugin.process(&noise, &overrides)?;
        let test_tf =
            analysis::transfer_function_full(&noise, &output, sample_rate, 10.0, max_freq);

        let cmp = analysis::compare_response_full(&ref_tf, &test_tf);
        let pass = cmp.mag_rms_db <= tolerance_db;

        // Write per-frequency detail CSV for failing scenarios
        if !pass {
            let details_dir = output_dir.join("details");
            std::fs::create_dir_all(&details_dir)?;
            write_response_detail_csv(
                &details_dir.join(format!("{}.csv", scenario.name)),
                &ref_tf,
                &test_tf,
            )?;
        }

        let is_multi = scenario.bands.len() > 1;
        let band = &scenario.bands[0];
        let filter_type = if is_multi {
            "multi".to_string()
        } else {
            FILTER_NAMES
                .get(band.shape as usize)
                .unwrap_or(&"unknown")
                .to_string()
        };

        results.push(CompareResult {
            name: scenario.name.clone(),
            filter_type,
            freq_hz: band.freq_hz,
            gain_db: band.gain_db,
            q: band.q,
            slope: band.slope,
            mag_rms_diff: cmp.mag_rms_db,
            mag_max_diff: cmp.mag_max_db,
            phase_rms_diff: cmp.phase_rms_rad,
            phase_max_diff: cmp.phase_max_rad,
            gd_rms_diff: cmp.gd_rms_samples,
            gd_max_diff: cmp.gd_max_samples,
            pass,
            is_multi,
        });

        if !pass || i == 0 || i == scenarios.len() - 1 || (i + 1) % 200 == 0 {
            let status = if pass { "PASS" } else { "FAIL" };
            eprintln!(
                "[{:>5}/{}] {} mag={:.2}dB phase={:.3}rad gd={:.1}smp  {}",
                i + 1,
                scenarios.len(),
                status,
                cmp.mag_rms_db,
                cmp.phase_rms_rad,
                cmp.gd_rms_samples,
                scenario.name,
            );
        }
    }

    let total = results.len();
    let passed = results.iter().filter(|r| r.pass).count();
    let failed = total - passed;
    let worst_mag_rms = results
        .iter()
        .map(|r| r.mag_rms_diff)
        .fold(0.0f32, f32::max);
    let avg_phase_rms =
        results.iter().map(|r| r.phase_rms_diff as f64).sum::<f64>() / total.max(1) as f64;
    let avg_gd_rms =
        results.iter().map(|r| r.gd_rms_diff as f64).sum::<f64>() / total.max(1) as f64;

    eprintln!("\n========================================================");
    eprintln!(
        "  Total: {}/{} passed (mag tolerance: {:.2} dB)",
        passed, total, tolerance_db
    );
    eprintln!("  Worst mag RMS:  {:.3} dB", worst_mag_rms);
    eprintln!(
        "  Avg phase RMS:  {:.4} rad ({:.2} deg)",
        avg_phase_rms,
        avg_phase_rms.to_degrees()
    );
    eprintln!("  Avg GD RMS:     {:.2} samples", avg_gd_rms);
    eprintln!("========================================================");

    // Write CSV with all metrics
    let csv_path = output_dir.join("comparison.csv");
    write_comparison_csv(&csv_path, &results)?;
    eprintln!("  CSV written to: {}", csv_path.display());

    if let Some(path) = report_path {
        write_comparison_report(path, &results, tolerance_db, sample_rate)?;
        eprintln!("  Report written to: {}", path.display());
    }

    if failed > 0 {
        anyhow::bail!(
            "{}/{} scenarios exceeded tolerance of {} dB",
            failed,
            total,
            tolerance_db,
        );
    }

    Ok(())
}

/// Write comparison results to CSV with all metric columns.
fn write_comparison_csv(path: &Path, results: &[CompareResult]) -> Result<()> {
    use std::io::Write as IoWrite;
    let mut f = std::io::BufWriter::new(std::fs::File::create(path)?);
    writeln!(
        f,
        "scenario,filter_type,freq_hz,q,slope,pass,mag_rms_db,mag_max_db,phase_rms_rad,phase_max_rad,gd_rms_samples,gd_max_samples"
    )?;
    for r in results {
        writeln!(
            f,
            "{},{},{},{},{},{},{:.4},{:.4},{:.6},{:.6},{:.4},{:.4}",
            r.name,
            r.filter_type,
            r.freq_hz,
            r.q,
            r.slope,
            if r.pass { "true" } else { "false" },
            r.mag_rms_diff,
            r.mag_max_diff,
            r.phase_rms_diff,
            r.phase_max_diff,
            r.gd_rms_diff,
            r.gd_max_diff,
        )?;
    }
    f.flush()?;
    Ok(())
}

/// Write per-frequency response detail CSV for a single scenario.
///
/// Columns: freq_hz, ref_mag_db, test_mag_db, mag_diff_db, ref_phase_rad,
///          test_phase_rad, phase_diff_rad, ref_gd_samples, test_gd_samples
fn write_response_detail_csv(
    path: &Path,
    ref_bins: &[analysis::ResponseBin],
    test_bins: &[analysis::ResponseBin],
) -> Result<()> {
    let mut f = std::io::BufWriter::new(std::fs::File::create(path)?);
    writeln!(
        f,
        "freq_hz,ref_mag_db,test_mag_db,mag_diff_db,ref_phase_rad,test_phase_rad,phase_diff_rad,ref_gd_samples,test_gd_samples"
    )?;

    let mut ti = 0;
    for rb in ref_bins {
        // Find nearest test bin by frequency (same logic as compare_response_full)
        while ti + 1 < test_bins.len()
            && (test_bins[ti + 1].freq_hz - rb.freq_hz).abs()
                < (test_bins[ti].freq_hz - rb.freq_hz).abs()
        {
            ti += 1;
        }
        let tb = if ti < test_bins.len() {
            &test_bins[ti]
        } else {
            continue;
        };
        writeln!(
            f,
            "{:.2},{:.4},{:.4},{:.4},{:.6},{:.6},{:.6},{:.4},{:.4}",
            rb.freq_hz,
            rb.mag_db,
            tb.mag_db,
            tb.mag_db - rb.mag_db,
            rb.phase_rad,
            tb.phase_rad,
            tb.phase_rad - rb.phase_rad,
            rb.group_delay_samples,
            tb.group_delay_samples,
        )?;
    }
    f.flush()?;
    Ok(())
}

/// Slope index to dB/oct label.
fn slope_label(slope: u32) -> &'static str {
    match slope {
        0 => "6 dB/oct",
        2 => "18 dB/oct",
        5 => "36 dB/oct",
        8 => "72 dB/oct",
        _ => "?",
    }
}

/// Write a detailed markdown comparison report.
fn write_comparison_report(
    path: &Path,
    results: &[CompareResult],
    tolerance_db: f32,
    sample_rate: f64,
) -> Result<()> {
    use std::collections::BTreeMap;
    use std::fmt::Write as FmtWrite;

    let mut md = String::with_capacity(64 * 1024);

    let total = results.len();
    let passed = results.iter().filter(|r| r.pass).count();
    let _failed = total - passed;
    let worst_mag_rms = results
        .iter()
        .map(|r| r.mag_rms_diff)
        .fold(0.0f32, f32::max);
    let avg_mag_rms =
        results.iter().map(|r| r.mag_rms_diff as f64).sum::<f64>() / total.max(1) as f64;
    let avg_phase_rms =
        results.iter().map(|r| r.phase_rms_diff as f64).sum::<f64>() / total.max(1) as f64;
    let avg_gd_rms =
        results.iter().map(|r| r.gd_rms_diff as f64).sum::<f64>() / total.max(1) as f64;

    // Header
    writeln!(md, "# FTS-EQ vs Pro-Q 4 Comparison Report")?;
    writeln!(md)?;
    writeln!(md, "- **Sample Rate:** {} Hz", sample_rate as u32)?;
    writeln!(md, "- **Magnitude Tolerance:** {:.2} dB RMS", tolerance_db)?;
    writeln!(
        md,
        "- **Total:** {}/{} passed ({:.1}%)",
        passed,
        total,
        100.0 * passed as f64 / total.max(1) as f64
    )?;
    writeln!(md)?;
    writeln!(md, "### Summary Metrics")?;
    writeln!(md)?;
    writeln!(md, "| Metric | Avg RMS | Worst RMS |")?;
    writeln!(md, "|--------|---------|-----------|")?;
    writeln!(
        md,
        "| Magnitude | {:.3} dB | {:.3} dB |",
        avg_mag_rms, worst_mag_rms
    )?;
    writeln!(
        md,
        "| Phase | {:.4} rad ({:.2}°) | {:.4} rad ({:.2}°) |",
        avg_phase_rms,
        (avg_phase_rms).to_degrees(),
        results
            .iter()
            .map(|r| r.phase_rms_diff)
            .fold(0.0f32, f32::max),
        results
            .iter()
            .map(|r| r.phase_rms_diff)
            .fold(0.0f32, f32::max)
            .to_degrees()
    )?;
    writeln!(
        md,
        "| Group Delay | {:.2} smp | {:.2} smp |",
        avg_gd_rms,
        results.iter().map(|r| r.gd_rms_diff).fold(0.0f32, f32::max)
    )?;
    writeln!(md)?;

    // ── By Filter Type ──
    writeln!(md, "## By Filter Type")?;
    writeln!(md)?;
    writeln!(
        md,
        "| Filter | Pass | Fail | Total | Rate | Mag RMS | Mag Max | Phase RMS | GD RMS |"
    )?;
    writeln!(
        md,
        "|--------|------|------|-------|------|---------|---------|-----------|--------|"
    )?;

    let filter_order = [
        "bell",
        "low_shelf",
        "high_shelf",
        "low_cut",
        "high_cut",
        "notch",
        "bandpass",
        "tilt_shelf",
        "flat_tilt",
        "allpass",
        "multi",
    ];
    for ft in &filter_order {
        let group: Vec<&CompareResult> = results.iter().filter(|r| r.filter_type == *ft).collect();
        if group.is_empty() {
            continue;
        }
        let p = group.iter().filter(|r| r.pass).count();
        let f = group.len() - p;
        let avg_mag = group.iter().map(|r| r.mag_rms_diff as f64).sum::<f64>() / group.len() as f64;
        let worst_mag = group.iter().map(|r| r.mag_rms_diff).fold(0.0f32, f32::max);
        let avg_phase =
            group.iter().map(|r| r.phase_rms_diff as f64).sum::<f64>() / group.len() as f64;
        let avg_gd = group.iter().map(|r| r.gd_rms_diff as f64).sum::<f64>() / group.len() as f64;
        let rate = 100.0 * p as f64 / group.len() as f64;
        writeln!(
            md,
            "| {} | {} | {} | {} | {:.1}% | {:.3} dB | {:.3} dB | {:.4} rad | {:.2} smp |",
            ft,
            p,
            f,
            group.len(),
            rate,
            avg_mag,
            worst_mag,
            avg_phase,
            avg_gd
        )?;
    }
    writeln!(md)?;

    // ── By Filter Type x Slope ──
    writeln!(md, "## By Filter Type x Slope")?;
    writeln!(md)?;
    writeln!(
        md,
        "| Filter | Slope | Pass | Fail | Total | Rate | Mag RMS | Phase RMS | GD RMS |"
    )?;
    writeln!(
        md,
        "|--------|-------|------|------|-------|------|---------|-----------|--------|"
    )?;

    for ft in &filter_order {
        let type_results: Vec<&CompareResult> =
            results.iter().filter(|r| r.filter_type == *ft).collect();
        if type_results.is_empty() {
            continue;
        }
        let mut slopes: Vec<u32> = type_results.iter().map(|r| r.slope).collect();
        slopes.sort();
        slopes.dedup();
        for s in &slopes {
            let group: Vec<&&CompareResult> =
                type_results.iter().filter(|r| r.slope == *s).collect();
            if group.is_empty() {
                continue;
            }
            let p = group.iter().filter(|r| r.pass).count();
            let f = group.len() - p;
            let avg_mag =
                group.iter().map(|r| r.mag_rms_diff as f64).sum::<f64>() / group.len() as f64;
            let avg_phase =
                group.iter().map(|r| r.phase_rms_diff as f64).sum::<f64>() / group.len() as f64;
            let avg_gd =
                group.iter().map(|r| r.gd_rms_diff as f64).sum::<f64>() / group.len() as f64;
            let rate = 100.0 * p as f64 / group.len() as f64;
            writeln!(
                md,
                "| {} | {} | {} | {} | {} | {:.1}% | {:.3} dB | {:.4} rad | {:.2} smp |",
                ft,
                slope_label(*s),
                p,
                f,
                group.len(),
                rate,
                avg_mag,
                avg_phase,
                avg_gd
            )?;
        }
    }
    writeln!(md)?;

    // ── By Filter Type x Q ──
    writeln!(md, "## By Filter Type x Q")?;
    writeln!(md)?;
    writeln!(
        md,
        "| Filter | Q | Pass | Fail | Total | Rate | Mag RMS | Phase RMS | GD RMS |"
    )?;
    writeln!(
        md,
        "|--------|---|------|------|-------|------|---------|-----------|--------|"
    )?;

    for ft in &filter_order {
        let type_results: Vec<&CompareResult> = results
            .iter()
            .filter(|r| r.filter_type == *ft && !r.is_multi)
            .collect();
        if type_results.is_empty() {
            continue;
        }
        let mut qs: Vec<String> = type_results.iter().map(|r| format!("{:.1}", r.q)).collect();
        qs.sort_by(|a, b| {
            a.parse::<f32>()
                .unwrap_or(0.0)
                .partial_cmp(&b.parse::<f32>().unwrap_or(0.0))
                .unwrap()
        });
        qs.dedup();
        for q_str in &qs {
            let q_val: f32 = q_str.parse().unwrap_or(0.0);
            let group: Vec<&&CompareResult> = type_results
                .iter()
                .filter(|r| (r.q - q_val).abs() < 0.01)
                .collect();
            if group.is_empty() {
                continue;
            }
            let p = group.iter().filter(|r| r.pass).count();
            let f = group.len() - p;
            let avg_mag =
                group.iter().map(|r| r.mag_rms_diff as f64).sum::<f64>() / group.len() as f64;
            let avg_phase =
                group.iter().map(|r| r.phase_rms_diff as f64).sum::<f64>() / group.len() as f64;
            let avg_gd =
                group.iter().map(|r| r.gd_rms_diff as f64).sum::<f64>() / group.len() as f64;
            let rate = 100.0 * p as f64 / group.len() as f64;
            writeln!(
                md,
                "| {} | {} | {} | {} | {} | {:.1}% | {:.3} dB | {:.4} rad | {:.2} smp |",
                ft,
                q_str,
                p,
                f,
                group.len(),
                rate,
                avg_mag,
                avg_phase,
                avg_gd
            )?;
        }
    }
    writeln!(md)?;

    // ── By Frequency (all types combined) ──
    writeln!(md, "## By Frequency")?;
    writeln!(md)?;
    writeln!(
        md,
        "| Freq (Hz) | Pass | Fail | Total | Rate | Mag RMS | Phase RMS | GD RMS |"
    )?;
    writeln!(
        md,
        "|-----------|------|------|-------|------|---------|-----------|--------|"
    )?;

    let mut freq_groups: BTreeMap<String, Vec<&CompareResult>> = BTreeMap::new();
    for r in results.iter().filter(|r| !r.is_multi) {
        let key = format!("{:>8.0}", r.freq_hz);
        freq_groups.entry(key).or_default().push(r);
    }
    for (freq_key, group) in &freq_groups {
        let p = group.iter().filter(|r| r.pass).count();
        let f = group.len() - p;
        let avg_mag = group.iter().map(|r| r.mag_rms_diff as f64).sum::<f64>() / group.len() as f64;
        let avg_phase =
            group.iter().map(|r| r.phase_rms_diff as f64).sum::<f64>() / group.len() as f64;
        let avg_gd = group.iter().map(|r| r.gd_rms_diff as f64).sum::<f64>() / group.len() as f64;
        let rate = 100.0 * p as f64 / group.len() as f64;
        writeln!(
            md,
            "| {} | {} | {} | {} | {:.1}% | {:.3} dB | {:.4} rad | {:.2} smp |",
            freq_key.trim(),
            p,
            f,
            group.len(),
            rate,
            avg_mag,
            avg_phase,
            avg_gd
        )?;
    }
    writeln!(md)?;

    // ── Worst Failures ──
    let mut failures: Vec<&CompareResult> = results.iter().filter(|r| !r.pass).collect();
    failures.sort_by(|a, b| b.mag_rms_diff.partial_cmp(&a.mag_rms_diff).unwrap());

    writeln!(md, "## Failures ({} total)", failures.len())?;
    writeln!(md)?;

    if failures.len() > 100 {
        writeln!(md, "### Top 100 Worst Failures")?;
        writeln!(md)?;
    }

    writeln!(
        md,
        "| Scenario | Mag RMS | Mag Max | Phase RMS | GD RMS | Filter | Freq | Q | Slope |"
    )?;
    writeln!(
        md,
        "|----------|---------|---------|-----------|--------|--------|------|---|-------|"
    )?;

    for r in failures.iter().take(100) {
        writeln!(
            md,
            "| {} | {:.3} dB | {:.3} dB | {:.4} rad | {:.2} smp | {} | {} | {} | {} |",
            r.name,
            r.mag_rms_diff,
            r.mag_max_diff,
            r.phase_rms_diff,
            r.gd_rms_diff,
            r.filter_type,
            r.freq_hz,
            r.q,
            slope_label(r.slope)
        )?;
    }

    if failures.len() > 100 {
        writeln!(md)?;
        writeln!(md, "({} more failures not shown)", failures.len() - 100)?;
    }
    writeln!(md)?;

    // ── All Passing ──
    let mut passing: Vec<&CompareResult> = results.iter().filter(|r| r.pass).collect();
    passing.sort_by(|a, b| b.mag_rms_diff.partial_cmp(&a.mag_rms_diff).unwrap());

    writeln!(md, "## Passing ({} total)", passing.len())?;
    writeln!(md)?;
    writeln!(md, "### Closest to Threshold (top 50)")?;
    writeln!(md)?;
    writeln!(
        md,
        "| Scenario | Mag RMS | Phase RMS | GD RMS | Filter | Freq | Q | Slope |"
    )?;
    writeln!(
        md,
        "|----------|---------|-----------|--------|--------|------|---|-------|"
    )?;

    for r in passing.iter().take(50) {
        writeln!(
            md,
            "| {} | {:.3} dB | {:.4} rad | {:.2} smp | {} | {} | {} | {} |",
            r.name,
            r.mag_rms_diff,
            r.phase_rms_diff,
            r.gd_rms_diff,
            r.filter_type,
            r.freq_hz,
            r.q,
            slope_label(r.slope)
        )?;
    }
    writeln!(md)?;

    std::fs::write(path, &md)?;
    Ok(())
}

// compare_response_bins has been replaced by analysis::compare_response_full()

// ---------------------------------------------------------------------------
// Binary format: transfer function with phase and group delay
// ---------------------------------------------------------------------------
//
// Layout:
//   [num_bins: u32 LE]
//   [(freq_hz: f32 LE, mag_db: f32 LE, phase_rad: f32 LE, gd_samples: f32 LE) × num_bins]
//
// Legacy format (3 fields per bin, 12 bytes) is auto-detected by file size.

fn write_tf_bin(path: &Path, tf: &[ResponseBin]) -> Result<()> {
    let mut f = std::io::BufWriter::new(std::fs::File::create(path)?);
    f.write_all(&(tf.len() as u32).to_le_bytes())?;
    for bin in tf {
        f.write_all(&bin.freq_hz.to_le_bytes())?;
        f.write_all(&bin.mag_db.to_le_bytes())?;
        f.write_all(&bin.phase_rad.to_le_bytes())?;
        f.write_all(&bin.group_delay_samples.to_le_bytes())?;
    }
    f.flush()?;
    Ok(())
}

fn read_tf_bin(path: &Path) -> Result<Vec<ResponseBin>> {
    let data = std::fs::read(path)?;
    if data.len() < 4 {
        anyhow::bail!("bin file too small: {}", path.display());
    }
    let num_bins = u32::from_le_bytes([data[0], data[1], data[2], data[3]]) as usize;
    let payload = &data[4..];

    // Detect format: 16 bytes/bin (new, with GD) vs 12 bytes/bin (legacy, no GD)
    let has_gd = payload.len() >= num_bins * 16;

    let mut result = Vec::with_capacity(num_bins);
    if has_gd {
        for i in 0..num_bins {
            let off = i * 16;
            result.push(ResponseBin {
                freq_hz: f32::from_le_bytes([
                    payload[off],
                    payload[off + 1],
                    payload[off + 2],
                    payload[off + 3],
                ]),
                mag_db: f32::from_le_bytes([
                    payload[off + 4],
                    payload[off + 5],
                    payload[off + 6],
                    payload[off + 7],
                ]),
                phase_rad: f32::from_le_bytes([
                    payload[off + 8],
                    payload[off + 9],
                    payload[off + 10],
                    payload[off + 11],
                ]),
                group_delay_samples: f32::from_le_bytes([
                    payload[off + 12],
                    payload[off + 13],
                    payload[off + 14],
                    payload[off + 15],
                ]),
            });
        }
    } else {
        // Legacy 12-byte format — load then recompute group delay from phase
        for i in 0..num_bins {
            let off = i * 12;
            result.push(ResponseBin {
                freq_hz: f32::from_le_bytes([
                    payload[off],
                    payload[off + 1],
                    payload[off + 2],
                    payload[off + 3],
                ]),
                mag_db: f32::from_le_bytes([
                    payload[off + 4],
                    payload[off + 5],
                    payload[off + 6],
                    payload[off + 7],
                ]),
                phase_rad: f32::from_le_bytes([
                    payload[off + 8],
                    payload[off + 9],
                    payload[off + 10],
                    payload[off + 11],
                ]),
                group_delay_samples: 0.0,
            });
        }
        // Infer sample rate from bin spacing: bins are at k * (sr/fft_size),
        // so sr = freq_spacing * fft_size. With fft_size=4096 and first bin
        // freq ≈ bin_hz, we can recover it.
        if result.len() >= 2 {
            let bin_hz = result[1].freq_hz - result[0].freq_hz;
            let sr = (bin_hz * 4096.0) as f64;
            let gd = analysis::compute_group_delay(&result, sr);
            for (bin, &gd_val) in result.iter_mut().zip(gd.iter()) {
                bin.group_delay_samples = gd_val;
            }
        }
    }
    Ok(result)
}

// ---------------------------------------------------------------------------
// CSV export
// ---------------------------------------------------------------------------

fn write_eq_csv(output_dir: &Path, scenarios: &[EqScenario]) -> Result<()> {
    let csv_path = output_dir.join("capture.csv");
    let mut f = std::io::BufWriter::new(std::fs::File::create(&csv_path)?);

    if scenarios.is_empty() {
        return Ok(());
    }

    let first_bin = output_dir.join(format!("{}.bin", scenarios[0].name));
    let first_tf = read_tf_bin(&first_bin)?;
    let freqs: Vec<f32> = first_tf.iter().map(|b| b.freq_hz).collect();

    // Header: scenario info, then magnitude, phase, and group delay columns
    write!(f, "scenario,num_bands")?;
    for &freq in &freqs {
        write!(f, ",mag_{:.1}hz", freq)?;
    }
    for &freq in &freqs {
        write!(f, ",phase_{:.1}hz", freq)?;
    }
    for &freq in &freqs {
        write!(f, ",gd_{:.1}hz", freq)?;
    }
    writeln!(f)?;

    for scenario in scenarios {
        let bin_path = output_dir.join(format!("{}.bin", scenario.name));
        let tf = read_tf_bin(&bin_path)?;

        write!(f, "{},{}", scenario.name, scenario.bands.len())?;

        for bin in &tf {
            write!(f, ",{:.2}", bin.mag_db)?;
        }
        for bin in &tf {
            write!(f, ",{:.4}", bin.phase_rad)?;
        }
        for bin in &tf {
            write!(f, ",{:.2}", bin.group_delay_samples)?;
        }
        writeln!(f)?;
    }

    f.flush()?;
    Ok(())
}

/// Diagnose a single EQ scenario — prints detailed frequency-by-frequency comparison.
pub fn run_diagnose_eq(
    plugin_path: &str,
    reference_dir: &Path,
    scenario_name: &str,
    sample_rate: f64,
    block_size: u32,
    duration: f32,
) -> Result<()> {
    let sr_label = format!("{}k", sample_rate as u32 / 1000);
    let ref_sr_dir = reference_dir.join(&sr_label);
    if !ref_sr_dir.exists() {
        anyhow::bail!(
            "no reference data for {} Hz (expected {})",
            sample_rate,
            ref_sr_dir.display()
        );
    }

    let meta_path = ref_sr_dir.join("metadata.json");
    let meta: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(&meta_path)
            .with_context(|| format!("no metadata.json in {}", ref_sr_dir.display()))?,
    )?;

    let scenario = meta["scenarios"]
        .as_array()
        .context("no scenarios in metadata")?
        .iter()
        .find(|s| s["name"].as_str() == Some(scenario_name))
        .with_context(|| format!("scenario '{}' not found in metadata", scenario_name))?;

    let bands: Vec<BandConfig> = scenario["bands"]
        .as_array()
        .map(|arr| {
            arr.iter()
                .map(|b| BandConfig {
                    shape: b["shape"].as_u64().unwrap_or(0) as u32,
                    freq_hz: b["freq_hz"].as_f64().unwrap_or(1000.0) as f32,
                    gain_db: b["gain_db"].as_f64().unwrap_or(0.0) as f32,
                    q: b["q"].as_f64().unwrap_or(1.0) as f32,
                    slope: b["slope"].as_u64().unwrap_or(2) as u32,
                })
                .collect()
        })
        .unwrap_or_default();

    eprintln!("=== Diagnose: {} ===", scenario_name);
    for (i, b) in bands.iter().enumerate() {
        let name = FILTER_NAMES.get(b.shape as usize).unwrap_or(&"?");
        eprintln!(
            "  Band {}: {} freq={:.0}Hz gain={:.1}dB Q={:.2} slope={}",
            i + 1,
            name,
            b.freq_hz,
            b.gain_db,
            b.q,
            b.slope,
        );
    }

    let ref_bin_path = ref_sr_dir.join(format!("{}.bin", scenario_name));
    let ref_tf = read_tf_bin(&ref_bin_path)
        .with_context(|| format!("failed to read reference: {}", ref_bin_path.display()))?;

    eprintln!("\nLoading plugin: {}", plugin_path);
    let mut plugin = LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)?;
    let params = plugin.params();

    let find_param =
        |name: &str| -> Option<u32> { params.iter().find(|p| p.name == name).map(|p| p.id) };

    let max_bands = bands.len().max(1);
    let mut b_type_ids = Vec::new();
    let mut b_freq_ids = Vec::new();
    let mut b_gain_ids = Vec::new();
    let mut b_q_ids = Vec::new();
    let mut b_slope_ids = Vec::new();
    let mut b_on_ids = Vec::new();

    for i in 1..=max_bands {
        b_type_ids
            .push(find_param(&format!("B{} Type", i)).context(format!("missing B{} Type", i))?);
        b_freq_ids
            .push(find_param(&format!("B{} Freq", i)).context(format!("missing B{} Freq", i))?);
        b_gain_ids
            .push(find_param(&format!("B{} Gain", i)).context(format!("missing B{} Gain", i))?);
        b_q_ids.push(find_param(&format!("B{} Q", i)).context(format!("missing B{} Q", i))?);
        b_slope_ids
            .push(find_param(&format!("B{} Slope", i)).context(format!("missing B{} Slope", i))?);
        b_on_ids.push(find_param(&format!("B{} On", i)).context(format!("missing B{} On", i))?);
    }

    let mut overrides: Vec<(u32, f64)> = Vec::new();
    for (j, band) in bands.iter().enumerate() {
        overrides.push((b_on_ids[j], 1.0));
        overrides.push((b_type_ids[j], band.shape as f64));
        overrides.push((b_slope_ids[j], band.slope as f64));

        if let Some(v) = plugin.text_to_value(b_freq_ids[j], &format!("{} Hz", band.freq_hz)) {
            overrides.push((b_freq_ids[j], v));
        }
        if let Some(v) = plugin.text_to_value(b_gain_ids[j], &format!("{:.1} dB", band.gain_db)) {
            overrides.push((b_gain_ids[j], v));
        }
        if let Some(v) = plugin.text_to_value(b_q_ids[j], &format!("{:.2}", band.q)) {
            overrides.push((b_q_ids[j], v));
        }
    }

    eprintln!("\nParameter overrides:");
    for &(id, val) in &overrides {
        let name = params
            .iter()
            .find(|p| p.id == id)
            .map(|p| p.name.as_str())
            .unwrap_or("?");
        eprintln!("  {} (id={}) = {:.6}", name, id, val);
    }

    let total_samples = (sample_rate as f32 * duration) as usize;
    let noise = signal::white_noise(total_samples, 12345);
    let output = plugin.process(&noise, &overrides)?;
    let max_freq = sample_rate as f32 / 2.0;
    let test_tf = analysis::transfer_function_full(&noise, &output, sample_rate, 10.0, max_freq);

    // Group delay is already computed in the bins
    eprintln!(
        "\n{:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}",
        "freq_hz",
        "ref_dB",
        "fts_dB",
        "mag_diff",
        "ref_phase",
        "fts_phase",
        "ph_diff",
        "ref_gd",
        "fts_gd",
    );
    eprintln!("{}", "-".repeat(100));

    let mut mag_sum_sq = 0.0f64;
    let mut mag_max = 0.0f32;
    let mut mag_max_freq = 0.0f32;
    let mut phase_sum_sq = 0.0f64;
    let mut gd_sum_sq = 0.0f64;
    let mut count = 0;
    let mut test_idx = 0;

    let show_freqs: &[f32] = &[
        11.7, 20.0, 50.0, 100.0, 200.0, 500.0, 1000.0, 2000.0, 5000.0, 8000.0, 10000.0, 12000.0,
        14000.0, 16000.0, 18000.0, 20000.0, 22000.0, 23000.0,
    ];

    for ref_bin in ref_tf.iter() {
        while test_idx + 1 < test_tf.len()
            && (test_tf[test_idx + 1].freq_hz - ref_bin.freq_hz).abs()
                < (test_tf[test_idx].freq_hz - ref_bin.freq_hz).abs()
        {
            test_idx += 1;
        }

        let mag_diff = (test_tf[test_idx].mag_db - ref_bin.mag_db).abs();
        mag_sum_sq += mag_diff as f64 * mag_diff as f64;
        if mag_diff > mag_max {
            mag_max = mag_diff;
            mag_max_freq = ref_bin.freq_hz;
        }

        // Phase difference (wrapped)
        let mut phase_diff = test_tf[test_idx].phase_rad - ref_bin.phase_rad;
        if phase_diff > std::f32::consts::PI {
            phase_diff -= 2.0 * std::f32::consts::PI;
        }
        if phase_diff < -std::f32::consts::PI {
            phase_diff += 2.0 * std::f32::consts::PI;
        }
        let phase_diff_abs = phase_diff.abs();
        phase_sum_sq += phase_diff_abs as f64 * phase_diff_abs as f64;

        // Group delay difference (from pre-computed fields)
        let gd_diff = (test_tf[test_idx].group_delay_samples - ref_bin.group_delay_samples).abs();
        if gd_diff < 1000.0 {
            gd_sum_sq += gd_diff as f64 * gd_diff as f64;
        }

        count += 1;

        let show = show_freqs
            .iter()
            .any(|&f| (ref_bin.freq_hz - f).abs() < 6.0)
            || mag_diff > 5.0;

        if show {
            let marker = if mag_diff > 1.0 {
                " ***"
            } else if mag_diff > 0.5 {
                " *"
            } else {
                ""
            };
            let ref_gd_val = ref_bin.group_delay_samples;
            let test_gd_val = test_tf[test_idx].group_delay_samples;
            eprintln!(
                "{:10.1} {:10.2} {:10.2} {:10.2} {:10.4} {:10.4} {:10.4} {:10.2} {:10.2}{}",
                ref_bin.freq_hz,
                ref_bin.mag_db,
                test_tf[test_idx].mag_db,
                mag_diff,
                ref_bin.phase_rad,
                test_tf[test_idx].phase_rad,
                phase_diff,
                ref_gd_val,
                test_gd_val,
                marker,
            );
        }
    }

    let n = count.max(1) as f64;
    let mag_rms = (mag_sum_sq / n).sqrt();
    let phase_rms = (phase_sum_sq / n).sqrt();
    let gd_rms = (gd_sum_sq / n).sqrt();

    eprintln!("{}", "-".repeat(100));
    eprintln!(
        "Magnitude RMS:    {:.3} dB  (max: {:.3} dB at {:.1} Hz)",
        mag_rms, mag_max, mag_max_freq
    );
    eprintln!(
        "Phase RMS:        {:.4} rad ({:.2} deg)",
        phase_rms,
        phase_rms.to_degrees()
    );
    eprintln!("Group Delay RMS:  {:.2} samples", gd_rms);
    eprintln!("Ref bins:  {}  Test bins: {}", ref_tf.len(), test_tf.len());

    Ok(())
}

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

// ---------------------------------------------------------------------------
// Impulse response capture and comparison
// ---------------------------------------------------------------------------

/// Number of impulse response samples to capture per scenario.
/// A biquad cascade decays quickly; 512 samples is plenty even for steep filters.
const IR_LENGTH: usize = 512;

/// Binary format for impulse response: [num_samples: u32 LE][f32 LE × num_samples]
fn write_ir_bin(path: &Path, ir: &[f32]) -> Result<()> {
    let mut f = std::io::BufWriter::new(std::fs::File::create(path)?);
    f.write_all(&(ir.len() as u32).to_le_bytes())?;
    for &sample in ir {
        f.write_all(&sample.to_le_bytes())?;
    }
    f.flush()?;
    Ok(())
}

fn read_ir_bin(path: &Path) -> Result<Vec<f32>> {
    let data = std::fs::read(path)?;
    if data.len() < 4 {
        anyhow::bail!("IR bin file too small: {}", path.display());
    }
    let num_samples = u32::from_le_bytes([data[0], data[1], data[2], data[3]]) as usize;
    let payload = &data[4..];
    if payload.len() < num_samples * 4 {
        anyhow::bail!("IR bin file truncated: {}", path.display());
    }
    let mut result = Vec::with_capacity(num_samples);
    for i in 0..num_samples {
        let off = i * 4;
        result.push(f32::from_le_bytes([
            payload[off],
            payload[off + 1],
            payload[off + 2],
            payload[off + 3],
        ]));
    }
    Ok(result)
}

/// Capture impulse responses for all EQ scenarios.
pub fn run_capture_eq_impulse(
    plugin_path: &str,
    output_dir: &Path,
    block_size: u32,
    base_params: &[(u32, f64)],
    scenarios: &[EqScenario],
) -> Result<()> {
    for &sample_rate in &[48000.0, 96000.0] {
        let sr_label = format!("{}k", sample_rate as u32 / 1000);
        let sr_dir = output_dir.join(&sr_label);
        std::fs::create_dir_all(&sr_dir)?;

        eprintln!(
            "=== Capturing impulse responses at {} Hz ===",
            sample_rate as u32
        );

        let mut plugin = fts_analyzer::host::LoadedPlugin::load(
            plugin_path.as_ref(),
            0,
            sample_rate,
            block_size,
        )?;
        let params = plugin.params();

        // Find Pro-Q 4 band parameter IDs
        let band_ids = find_proq4_band_ids(&params)?;

        let impulse = signal::impulse(IR_LENGTH);

        for (i, scenario) in scenarios.iter().enumerate() {
            let mut overrides: Vec<(u32, f64)> = base_params.to_vec();
            for (j, band) in scenario.bands.iter().enumerate() {
                overrides.push((band_ids[j].enabled, 1.0));
                overrides.push((band_ids[j].used, 1.0));
                overrides.push((band_ids[j].shape, band.shape as f64));
                overrides.push((band_ids[j].slope, band.slope as f64));

                if let Some(v) =
                    plugin.text_to_value(band_ids[j].freq, &format!("{} Hz", band.freq_hz))
                {
                    overrides.push((band_ids[j].freq, v));
                }
                if let Some(v) =
                    plugin.text_to_value(band_ids[j].gain, &format!("{:.1} dB", band.gain_db))
                {
                    overrides.push((band_ids[j].gain, v));
                }
                if let Some(v) = plugin.text_to_value(band_ids[j].q, &format!("{:.2}", band.q)) {
                    overrides.push((band_ids[j].q, v));
                }
            }

            let output = plugin.process(&impulse, &overrides)?;
            let ir: Vec<f32> = output.iter().take(IR_LENGTH).copied().collect();

            let bin_path = sr_dir.join(format!("{}.ir.bin", scenario.name));
            write_ir_bin(&bin_path, &ir)?;

            if (i + 1) % 200 == 0 || i + 1 == scenarios.len() {
                eprintln!("[{:>5}/{}]", i + 1, scenarios.len());
            }
        }

        // Write metadata
        let meta = serde_json::json!({
            "plugin_path": plugin_path,
            "sample_rate": sample_rate,
            "block_size": block_size,
            "ir_length": IR_LENGTH,
            "measurement": "unit_impulse",
            "scenarios": scenarios.iter().map(|s| serde_json::json!({
                "name": &s.name,
                "bands": s.bands.iter().map(|b| serde_json::json!({
                    "shape": b.shape, "freq_hz": b.freq_hz,
                    "gain_db": b.gain_db, "q": b.q, "slope": b.slope,
                })).collect::<Vec<_>>(),
            })).collect::<Vec<_>>(),
        });
        std::fs::write(
            sr_dir.join("metadata_ir.json"),
            serde_json::to_string_pretty(&meta)?,
        )?;

        eprintln!("  {} scenarios captured\n", scenarios.len());
    }

    Ok(())
}

/// Compare impulse responses and extract biquad coefficients.
pub fn run_compare_eq_impulse(
    plugin_path: &str,
    reference_dir: &Path,
    output_dir: &Path,
    sample_rate: f64,
    block_size: u32,
    base_params: &[(u32, f64)],
    filters: Option<&[&str]>,
) -> Result<()> {
    std::fs::create_dir_all(output_dir)?;

    let sr_label = format!("{}k", sample_rate as u32 / 1000);
    let ref_sr_dir = reference_dir.join(&sr_label);
    if !ref_sr_dir.exists() {
        anyhow::bail!("no IR reference data at {}", ref_sr_dir.display());
    }

    let meta_path = ref_sr_dir.join("metadata_ir.json");
    let meta: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(&meta_path)
            .with_context(|| format!("no metadata_ir.json in {}", ref_sr_dir.display()))?,
    )?;

    let mut scenarios: Vec<EqScenario> = meta["scenarios"]
        .as_array()
        .context("no scenarios in metadata")?
        .iter()
        .map(|s| {
            let bands: Vec<BandConfig> = s["bands"]
                .as_array()
                .map(|arr| {
                    arr.iter()
                        .map(|b| BandConfig {
                            shape: b["shape"].as_u64().unwrap_or(0) as u32,
                            freq_hz: b["freq_hz"].as_f64().unwrap_or(1000.0) as f32,
                            gain_db: b["gain_db"].as_f64().unwrap_or(0.0) as f32,
                            q: b["q"].as_f64().unwrap_or(1.0) as f32,
                            slope: b["slope"].as_u64().unwrap_or(2) as u32,
                        })
                        .collect()
                })
                .unwrap_or_default();
            EqScenario {
                name: s["name"].as_str().unwrap_or("").to_string(),
                bands,
            }
        })
        .collect();

    if let Some(filters) = filters {
        scenarios.retain(|s| filters.iter().any(|f| s.name.contains(f)));
    }

    eprintln!("Loading plugin: {}", plugin_path);
    let mut plugin =
        fts_analyzer::host::LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)?;
    let params = plugin.params();

    let find_param =
        |name: &str| -> Option<u32> { params.iter().find(|p| p.name == name).map(|p| p.id) };

    let max_bands = scenarios.iter().map(|s| s.bands.len()).max().unwrap_or(1);
    let mut b_type_ids = Vec::new();
    let mut b_freq_ids = Vec::new();
    let mut b_gain_ids = Vec::new();
    let mut b_q_ids = Vec::new();
    let mut b_slope_ids = Vec::new();
    let mut b_on_ids = Vec::new();

    for i in 1..=max_bands {
        b_type_ids
            .push(find_param(&format!("B{} Type", i)).context(format!("missing B{} Type", i))?);
        b_freq_ids
            .push(find_param(&format!("B{} Freq", i)).context(format!("missing B{} Freq", i))?);
        b_gain_ids
            .push(find_param(&format!("B{} Gain", i)).context(format!("missing B{} Gain", i))?);
        b_q_ids.push(find_param(&format!("B{} Q", i)).context(format!("missing B{} Q", i))?);
        b_slope_ids
            .push(find_param(&format!("B{} Slope", i)).context(format!("missing B{} Slope", i))?);
        b_on_ids.push(find_param(&format!("B{} On", i)).context(format!("missing B{} On", i))?);
    }

    let impulse = signal::impulse(IR_LENGTH);

    eprintln!("{} scenarios\n", scenarios.len());

    // CSV for coefficient comparison
    let csv_path = output_dir.join("impulse_comparison.csv");
    let mut csv = std::io::BufWriter::new(std::fs::File::create(&csv_path)?);
    writeln!(
        csv,
        "scenario,filter_type,freq_hz,q,slope,ir_rms_diff,ir_peak_diff,ref_b0,ref_b1,ref_b2,ref_a1,ref_a2,test_b0,test_b1,test_b2,test_a1,test_a2,coeff_verified"
    )?;

    let mut total = 0;
    let mut matched = 0;

    for (i, scenario) in scenarios.iter().enumerate() {
        let ref_ir_path = ref_sr_dir.join(format!("{}.ir.bin", scenario.name));
        if !ref_ir_path.exists() {
            continue;
        }

        let ref_ir = read_ir_bin(&ref_ir_path)?;

        // Process impulse through test plugin
        let mut overrides: Vec<(u32, f64)> = base_params.to_vec();
        for (j, band) in scenario.bands.iter().enumerate() {
            overrides.push((b_on_ids[j], 1.0));
            overrides.push((b_type_ids[j], band.shape as f64));
            overrides.push((b_slope_ids[j], band.slope as f64));
            if let Some(v) = plugin.text_to_value(b_freq_ids[j], &format!("{} Hz", band.freq_hz)) {
                overrides.push((b_freq_ids[j], v));
            }
            if let Some(v) = plugin.text_to_value(b_gain_ids[j], &format!("{:.1} dB", band.gain_db))
            {
                overrides.push((b_gain_ids[j], v));
            }
            if let Some(v) = plugin.text_to_value(b_q_ids[j], &format!("{:.2}", band.q)) {
                overrides.push((b_q_ids[j], v));
            }
        }

        let output = plugin.process(&impulse, &overrides)?;
        let test_ir: Vec<f32> = output.iter().take(IR_LENGTH).copied().collect();

        let cmp = analysis::compare_impulse_response(&ref_ir, &test_ir);
        let is_single_biquad = scenario.bands.len() == 1 && scenario.bands[0].slope == 0;

        let ref_coeffs = if is_single_biquad {
            analysis::extract_biquad_coefficients(&ref_ir)
        } else {
            None
        };
        let test_coeffs = if is_single_biquad {
            analysis::extract_biquad_coefficients(&test_ir)
        } else {
            None
        };

        // Check if coefficients match within tolerance
        let coeff_match = match (&ref_coeffs, &test_coeffs) {
            (Some(r), Some(t)) => {
                (r.b0 - t.b0).abs() < 1e-6
                    && (r.b1 - t.b1).abs() < 1e-6
                    && (r.b2 - t.b2).abs() < 1e-6
                    && (r.a1 - t.a1).abs() < 1e-6
                    && (r.a2 - t.a2).abs() < 1e-6
            }
            _ => false,
        };

        let pass = cmp.rms_diff < 1e-5;
        total += 1;
        if pass {
            matched += 1;
        }

        let band = &scenario.bands[0];
        let filter_type = if scenario.bands.len() > 1 {
            "multi"
        } else {
            FILTER_NAMES.get(band.shape as usize).unwrap_or(&"?")
        };

        // Write CSV row
        let fmt_coeff = |c: &Option<analysis::BiquadCoefficients>,
                         field: fn(&analysis::BiquadCoefficients) -> f64|
         -> String {
            c.as_ref()
                .map(|c| format!("{:.10}", field(c)))
                .unwrap_or_default()
        };
        writeln!(
            csv,
            "{},{},{},{},{},{:.2e},{:.2e},{},{},{},{},{},{},{},{},{},{},{}",
            scenario.name,
            filter_type,
            band.freq_hz,
            band.q,
            band.slope,
            cmp.rms_diff,
            cmp.peak_diff,
            fmt_coeff(&ref_coeffs, |c| c.b0),
            fmt_coeff(&ref_coeffs, |c| c.b1),
            fmt_coeff(&ref_coeffs, |c| c.b2),
            fmt_coeff(&ref_coeffs, |c| c.a1),
            fmt_coeff(&ref_coeffs, |c| c.a2),
            fmt_coeff(&test_coeffs, |c| c.b0),
            fmt_coeff(&test_coeffs, |c| c.b1),
            fmt_coeff(&test_coeffs, |c| c.b2),
            fmt_coeff(&test_coeffs, |c| c.a1),
            fmt_coeff(&test_coeffs, |c| c.a2),
            coeff_match,
        )?;

        if !pass || (i + 1) % 200 == 0 || i + 1 == scenarios.len() {
            let status = if pass { "PASS" } else { "FAIL" };
            eprintln!(
                "[{:>5}/{}] {} rms={:.2e} peak={:.2e}  {}{}",
                i + 1,
                scenarios.len(),
                status,
                cmp.rms_diff,
                cmp.peak_diff,
                scenario.name,
                if is_single_biquad && ref_coeffs.is_some() {
                    let rc = ref_coeffs.as_ref().unwrap();
                    format!(
                        "  [b0={:.6} b1={:.6} b2={:.6} a1={:.6} a2={:.6}]",
                        rc.b0, rc.b1, rc.b2, rc.a1, rc.a2
                    )
                } else {
                    String::new()
                },
            );
        }
    }

    csv.flush()?;

    eprintln!("\n========================================================");
    eprintln!("  IR match: {}/{} (rms < 1e-5)", matched, total);
    eprintln!("  CSV: {}", csv_path.display());
    eprintln!("========================================================");

    Ok(())
}

// ---------------------------------------------------------------------------
// Multi-level linearity testing
// ---------------------------------------------------------------------------

/// Run compare-eq at multiple input levels and report level-dependent deviations.
pub fn run_compare_eq_multilevel(
    plugin_path: &str,
    reference_dir: &Path,
    output_dir: &Path,
    sample_rate: f64,
    block_size: u32,
    duration: f32,
    base_params: &[(u32, f64)],
    levels_db: &[f32],
    filters: Option<&[&str]>,
) -> Result<()> {
    std::fs::create_dir_all(output_dir)?;

    let sr_label = format!("{}k", sample_rate as u32 / 1000);
    let ref_sr_dir = reference_dir.join(&sr_label);
    if !ref_sr_dir.exists() {
        anyhow::bail!("no reference data at {}", ref_sr_dir.display());
    }

    let meta_path = ref_sr_dir.join("metadata.json");
    let meta: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(&meta_path)?)?;

    let mut scenarios: Vec<EqScenario> = meta["scenarios"]
        .as_array()
        .context("no scenarios")?
        .iter()
        .map(|s| {
            let bands = s["bands"]
                .as_array()
                .map(|arr| {
                    arr.iter()
                        .map(|b| BandConfig {
                            shape: b["shape"].as_u64().unwrap_or(0) as u32,
                            freq_hz: b["freq_hz"].as_f64().unwrap_or(1000.0) as f32,
                            gain_db: b["gain_db"].as_f64().unwrap_or(0.0) as f32,
                            q: b["q"].as_f64().unwrap_or(1.0) as f32,
                            slope: b["slope"].as_u64().unwrap_or(2) as u32,
                        })
                        .collect()
                })
                .unwrap_or_default();
            EqScenario {
                name: s["name"].as_str().unwrap_or("").to_string(),
                bands,
            }
        })
        .collect();

    if let Some(filters) = filters {
        scenarios.retain(|s| filters.iter().any(|f| s.name.contains(f)));
    }

    // Use a subset for linearity testing (every 10th scenario)
    let test_scenarios: Vec<&EqScenario> = scenarios.iter().step_by(10).collect();

    eprintln!("=== Multi-level linearity test ===");
    eprintln!("Levels: {:?} dBFS", levels_db);
    eprintln!(
        "Testing {} of {} scenarios\n",
        test_scenarios.len(),
        scenarios.len()
    );

    let mut plugin =
        fts_analyzer::host::LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)?;
    let params = plugin.params();

    let find_param =
        |name: &str| -> Option<u32> { params.iter().find(|p| p.name == name).map(|p| p.id) };

    let max_bands = test_scenarios
        .iter()
        .map(|s| s.bands.len())
        .max()
        .unwrap_or(1);
    let mut b_type_ids = Vec::new();
    let mut b_freq_ids = Vec::new();
    let mut b_gain_ids = Vec::new();
    let mut b_q_ids = Vec::new();
    let mut b_slope_ids = Vec::new();
    let mut b_on_ids = Vec::new();

    for i in 1..=max_bands {
        b_type_ids
            .push(find_param(&format!("B{} Type", i)).context(format!("missing B{} Type", i))?);
        b_freq_ids
            .push(find_param(&format!("B{} Freq", i)).context(format!("missing B{} Freq", i))?);
        b_gain_ids
            .push(find_param(&format!("B{} Gain", i)).context(format!("missing B{} Gain", i))?);
        b_q_ids.push(find_param(&format!("B{} Q", i)).context(format!("missing B{} Q", i))?);
        b_slope_ids
            .push(find_param(&format!("B{} Slope", i)).context(format!("missing B{} Slope", i))?);
        b_on_ids.push(find_param(&format!("B{} On", i)).context(format!("missing B{} On", i))?);
    }

    let total_samples = (sample_rate as f32 * duration) as usize;
    let max_freq = sample_rate as f32 / 2.0;

    // CSV output
    let csv_path = output_dir.join("linearity.csv");
    let mut csv = std::io::BufWriter::new(std::fs::File::create(&csv_path)?);
    writeln!(
        csv,
        "scenario,filter_type,level_db,mag_rms_db,phase_rms_rad"
    )?;

    let mut max_deviation = 0.0f32;
    let mut worst_scenario = String::new();

    for (si, scenario) in test_scenarios.iter().enumerate() {
        let mut overrides: Vec<(u32, f64)> = base_params.to_vec();
        for (j, band) in scenario.bands.iter().enumerate() {
            overrides.push((b_on_ids[j], 1.0));
            overrides.push((b_type_ids[j], band.shape as f64));
            overrides.push((b_slope_ids[j], band.slope as f64));
            if let Some(v) = plugin.text_to_value(b_freq_ids[j], &format!("{} Hz", band.freq_hz)) {
                overrides.push((b_freq_ids[j], v));
            }
            if let Some(v) = plugin.text_to_value(b_gain_ids[j], &format!("{:.1} dB", band.gain_db))
            {
                overrides.push((b_gain_ids[j], v));
            }
            if let Some(v) = plugin.text_to_value(b_q_ids[j], &format!("{:.2}", band.q)) {
                overrides.push((b_q_ids[j], v));
            }
        }

        // Measure transfer function at each level
        let mut level_responses: Vec<(f32, Vec<ResponseBin>)> = Vec::new();
        for &level_db in levels_db {
            let gain = 10.0f32.powf(level_db / 20.0);
            let noise: Vec<f32> = signal::white_noise(total_samples, 12345)
                .into_iter()
                .map(|s| s * gain)
                .collect();
            let output = plugin.process(&noise, &overrides)?;
            let tf = analysis::transfer_function_full(&noise, &output, sample_rate, 10.0, max_freq);
            level_responses.push((level_db, tf));
        }

        let linearity = analysis::check_linearity(&level_responses);
        let band = &scenario.bands[0];
        let filter_type = if scenario.bands.len() > 1 {
            "multi"
        } else {
            FILTER_NAMES.get(band.shape as usize).unwrap_or(&"?")
        };

        for lr in &linearity {
            writeln!(
                csv,
                "{},{},{},{:.6},{:.6}",
                scenario.name, filter_type, lr.level_db, lr.mag_rms_db, lr.phase_rms_rad
            )?;

            if lr.mag_rms_db > max_deviation {
                max_deviation = lr.mag_rms_db;
                worst_scenario = format!("{} @ {}dB", scenario.name, lr.level_db);
            }
        }

        if (si + 1) % 50 == 0 || si + 1 == test_scenarios.len() {
            eprintln!("[{:>5}/{}]", si + 1, test_scenarios.len());
        }
    }

    csv.flush()?;

    let is_linear = max_deviation < 0.01;
    eprintln!("\n========================================================");
    eprintln!(
        "  Linearity: {} (max deviation: {:.4} dB)",
        if is_linear { "PASS" } else { "FAIL" },
        max_deviation
    );
    if !is_linear {
        eprintln!("  Worst: {}", worst_scenario);
    }
    eprintln!("  CSV: {}", csv_path.display());
    eprintln!("========================================================");

    Ok(())
}

// ---------------------------------------------------------------------------
// Hi-res FFT comparison (configurable FFT size)
// ---------------------------------------------------------------------------

/// Compare EQ with a configurable (higher) FFT size for finer frequency resolution.
pub fn run_compare_eq_hires(
    plugin_path: &str,
    reference_dir: &Path,
    output_dir: &Path,
    sample_rate: f64,
    block_size: u32,
    duration: f32,
    fft_size: usize,
    base_params: &[(u32, f64)],
    tolerance_db: f32,
    filters: Option<&[&str]>,
) -> Result<()> {
    std::fs::create_dir_all(output_dir)?;

    let sr_label = format!("{}k", sample_rate as u32 / 1000);
    let ref_sr_dir = reference_dir.join(&sr_label);
    if !ref_sr_dir.exists() {
        anyhow::bail!("no reference data at {}", ref_sr_dir.display());
    }

    let meta_path = ref_sr_dir.join("metadata.json");
    let meta: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(&meta_path)?)?;

    let mut scenarios: Vec<EqScenario> = meta["scenarios"]
        .as_array()
        .context("no scenarios")?
        .iter()
        .map(|s| {
            let bands = s["bands"]
                .as_array()
                .map(|arr| {
                    arr.iter()
                        .map(|b| BandConfig {
                            shape: b["shape"].as_u64().unwrap_or(0) as u32,
                            freq_hz: b["freq_hz"].as_f64().unwrap_or(1000.0) as f32,
                            gain_db: b["gain_db"].as_f64().unwrap_or(0.0) as f32,
                            q: b["q"].as_f64().unwrap_or(1.0) as f32,
                            slope: b["slope"].as_u64().unwrap_or(2) as u32,
                        })
                        .collect()
                })
                .unwrap_or_default();
            EqScenario {
                name: s["name"].as_str().unwrap_or("").to_string(),
                bands,
            }
        })
        .collect();

    if let Some(filters) = filters {
        scenarios.retain(|s| filters.iter().any(|f| s.name.contains(f)));
    }

    let max_freq = sample_rate as f32 / 2.0;
    let bin_hz = sample_rate as f32 / fft_size as f32;

    eprintln!(
        "=== Hi-res comparison (FFT size: {}, bin: {:.2} Hz) ===",
        fft_size, bin_hz
    );
    eprintln!("Reference: {}", ref_sr_dir.display());

    let mut plugin =
        fts_analyzer::host::LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)?;
    let params = plugin.params();

    let find_param =
        |name: &str| -> Option<u32> { params.iter().find(|p| p.name == name).map(|p| p.id) };

    let max_bands = scenarios.iter().map(|s| s.bands.len()).max().unwrap_or(1);
    let mut b_type_ids = Vec::new();
    let mut b_freq_ids = Vec::new();
    let mut b_gain_ids = Vec::new();
    let mut b_q_ids = Vec::new();
    let mut b_slope_ids = Vec::new();
    let mut b_on_ids = Vec::new();

    for i in 1..=max_bands {
        b_type_ids
            .push(find_param(&format!("B{} Type", i)).context(format!("missing B{} Type", i))?);
        b_freq_ids
            .push(find_param(&format!("B{} Freq", i)).context(format!("missing B{} Freq", i))?);
        b_gain_ids
            .push(find_param(&format!("B{} Gain", i)).context(format!("missing B{} Gain", i))?);
        b_q_ids.push(find_param(&format!("B{} Q", i)).context(format!("missing B{} Q", i))?);
        b_slope_ids
            .push(find_param(&format!("B{} Slope", i)).context(format!("missing B{} Slope", i))?);
        b_on_ids.push(find_param(&format!("B{} On", i)).context(format!("missing B{} On", i))?);
    }

    let total_samples = (sample_rate as f32 * duration) as usize;
    let noise = signal::white_noise(total_samples, 12345);

    eprintln!(
        "{} scenarios, tolerance: {:.2} dB\n",
        scenarios.len(),
        tolerance_db
    );

    // CSV
    let csv_path = output_dir.join("hires_comparison.csv");
    let mut csv = std::io::BufWriter::new(std::fs::File::create(&csv_path)?);
    writeln!(
        csv,
        "scenario,filter_type,freq_hz,q,slope,mag_rms_db,mag_max_db,phase_rms_rad,pass"
    )?;

    let mut total = 0;
    let mut passed = 0;

    for (i, scenario) in scenarios.iter().enumerate() {
        // Load reference (standard resolution) for comparison metadata
        let ref_bin = ref_sr_dir.join(format!("{}.bin", scenario.name));
        if !ref_bin.exists() {
            continue;
        }

        // Run test plugin with hi-res FFT
        let mut overrides: Vec<(u32, f64)> = base_params.to_vec();
        for (j, band) in scenario.bands.iter().enumerate() {
            overrides.push((b_on_ids[j], 1.0));
            overrides.push((b_type_ids[j], band.shape as f64));
            overrides.push((b_slope_ids[j], band.slope as f64));
            if let Some(v) = plugin.text_to_value(b_freq_ids[j], &format!("{} Hz", band.freq_hz)) {
                overrides.push((b_freq_ids[j], v));
            }
            if let Some(v) = plugin.text_to_value(b_gain_ids[j], &format!("{:.1} dB", band.gain_db))
            {
                overrides.push((b_gain_ids[j], v));
            }
            if let Some(v) = plugin.text_to_value(b_q_ids[j], &format!("{:.2}", band.q)) {
                overrides.push((b_q_ids[j], v));
            }
        }

        let output = plugin.process(&noise, &overrides)?;

        // Use hi-res FFT for test
        let test_tf = analysis::transfer_function_with_fft_size(
            &noise,
            &output,
            sample_rate,
            10.0,
            max_freq,
            fft_size,
        );

        // Also measure reference at hi-res (re-process same noise through ref data isn't possible,
        // but we can compare test hires against standard ref to see if finer resolution reveals issues)
        let ref_tf = read_tf_bin(&ref_bin)?;
        let cmp = analysis::compare_response_full(&ref_tf, &test_tf);

        let pass = cmp.mag_rms_db <= tolerance_db;
        total += 1;
        if pass {
            passed += 1;
        }

        let band = &scenario.bands[0];
        let filter_type = if scenario.bands.len() > 1 {
            "multi"
        } else {
            FILTER_NAMES.get(band.shape as usize).unwrap_or(&"?")
        };

        writeln!(
            csv,
            "{},{},{},{},{},{:.4},{:.4},{:.6},{}",
            scenario.name,
            filter_type,
            band.freq_hz,
            band.q,
            band.slope,
            cmp.mag_rms_db,
            cmp.mag_max_db,
            cmp.phase_rms_rad,
            if pass { "true" } else { "false" }
        )?;

        if !pass || (i + 1) % 200 == 0 || i + 1 == scenarios.len() {
            let status = if pass { "PASS" } else { "FAIL" };
            eprintln!(
                "[{:>5}/{}] {} mag={:.2}dB phase={:.3}rad  {} ({}x res)",
                i + 1,
                scenarios.len(),
                status,
                cmp.mag_rms_db,
                cmp.phase_rms_rad,
                scenario.name,
                fft_size / analysis::DEFAULT_FFT_SIZE,
            );
        }
    }

    csv.flush()?;

    eprintln!("\n========================================================");
    eprintln!(
        "  Total: {}/{} passed (FFT={}, {:.2} Hz bins)",
        passed, total, fft_size, bin_hz
    );
    eprintln!("  CSV: {}", csv_path.display());
    eprintln!("========================================================");

    Ok(())
}

// ---------------------------------------------------------------------------
// Helpers for Pro-Q 4 band parameter IDs (used by capture)
// ---------------------------------------------------------------------------

fn find_proq4_band_ids(params: &[fts_analyzer::host::ParamInfo]) -> Result<Vec<ProQ4BandIds>> {
    let find = |name: &str| -> Option<u32> { params.iter().find(|p| p.name == name).map(|p| p.id) };

    let mut ids = Vec::new();
    for i in 1..=24 {
        let shape = match find(&format!("Band {} Shape", i)) {
            Some(id) => id,
            None => break, // No more bands
        };
        let freq = find(&format!("Band {} Frequency", i))
            .context(format!("missing Band {} Frequency", i))?;
        let gain = find(&format!("Band {} Gain", i)).context(format!("missing Band {} Gain", i))?;
        let q = find(&format!("Band {} Q", i)).context(format!("missing Band {} Q", i))?;
        let slope =
            find(&format!("Band {} Slope", i)).context(format!("missing Band {} Slope", i))?;
        let enabled =
            find(&format!("Band {} Enabled", i)).context(format!("missing Band {} Enabled", i))?;
        let used = find(&format!("Band {} Used", i)).context(format!("missing Band {} Used", i))?;
        ids.push(ProQ4BandIds {
            shape,
            freq,
            gain,
            q,
            slope,
            enabled,
            used,
        });
    }

    if ids.is_empty() {
        anyhow::bail!("no band parameters found (expected 'Band N Shape' etc.)");
    }

    Ok(ids)
}
