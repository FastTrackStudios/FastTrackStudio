//! Pitch shifter capture and comparison.
//!
//! Processes GuitarSet clips through a reference pitch shifter (Kuassa Efektor
//! Harmonitron, VST2 via yabridge) and a test plugin (FTS Pitch POG, CLAP),
//! then compares spectral content and RMS levels.
//!
//! Storage format: one `.bin` file per clip per scenario.
//! Layout: [num_samples: u32 LE][f32 LE × num_samples]

use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use anyhow::{Context, Result};
use fts_analyzer::analysis;
use fts_analyzer::host::LoadedPlugin;

/// Default GuitarSet directory.
pub const GUITARSET_DIR: &str = "/home/cody/Development/mir-datasets/data/guitarset";

/// Default sample rate for GuitarSet (native SR).
pub const DEFAULT_SAMPLE_RATE: f64 = 44100.0;

/// Maximum clip duration in seconds (Kuassa trial adds noise every ~40s).
pub const DEFAULT_MAX_CLIP_DURATION: f32 = 30.0;

// ---------------------------------------------------------------------------
// Scenarios
// ---------------------------------------------------------------------------

/// A pitch test scenario with named parameter configs for ref + test plugins.
#[derive(Debug, Clone)]
pub struct PitchScenario {
    pub name: String,
    /// Parameter overrides for the reference plugin (Harmonitron).
    pub ref_params: Vec<(u32, f64)>,
    /// Parameter overrides for the test plugin (FTS Pitch).
    pub test_params: Vec<(u32, f64)>,
}

/// Build the default pitch comparison scenarios.
///
/// Each scenario activates exactly ONE Harmonitron voice at full level with
/// all effects (detune, envelope, lowpass) zeroed out. This isolates the
/// pure pitch-shifted signal for clean comparison.
///
/// Harmonitron semitone mapping: normalized = (semitones + 24) / 48.
///   0.00 = -24st, 0.25 = -12st, 0.50 = 0st, 0.75 = +12st, 1.00 = +24st.
///
/// Param reference (29 total):
///   0: onBypass, 1: dryAlignOn, 2: dryLevel, 3: dryPanning
///   4-6: pitch1 Level/Pan/Semi, 7-9: pitch2, 10-12: pitch3, 13-15: pitch4
///   16-20: detune (DryIn/P1P2In/P3P4In/Cent/Level)
///   21-24: envelope (LinkOn/DryIn/Sensitivity/Attack)
///   25-27: lowpass (DryIn/Freq/Q), 28: level (output)
pub fn build_scenarios() -> Vec<PitchScenario> {
    // Common base: everything zeroed/neutral except bypass on and output at unity
    let base: Vec<(u32, f64)> = vec![
        (0, 1.0), // onBypass = on
        (1, 0.0), // dryAlignOn = off
        (2, 0.0), // dryLevel = 0 (100% wet)
        (3, 0.5), // dryPanning = center
        // All 4 voices off (level=0, pan=center, semi=0)
        (4, 0.0),
        (5, 0.5),
        (6, 0.5), // pitch1
        (7, 0.0),
        (8, 0.5),
        (9, 0.5), // pitch2
        (10, 0.0),
        (11, 0.5),
        (12, 0.5), // pitch3
        (13, 0.0),
        (14, 0.5),
        (15, 0.5), // pitch4
        // Detune off
        (16, 0.0), // detuneDryIn = off
        (17, 0.0), // detuneP1P2In = off
        (18, 0.0), // detuneP3P4In = off
        (19, 0.0), // detuneCent = 0
        (20, 0.0), // detuneLevel = 0
        // Envelope off
        (21, 0.0), // envelopeLinkOn = off
        (22, 0.0), // envelopeDryIn = off
        (23, 0.0), // envelopeSensitivity = 0
        (24, 0.0), // envelopeAttack = 0
        // Lowpass wide open
        (25, 0.0), // lowpassDryIn = off
        (26, 1.0), // lowpassFreq = max
        (27, 0.0), // lowpassQ = flat
        // Output at unity
        (28, 1.0), // level = 1.0
    ];

    // Helper: activate one voice at full level with a specific semitone
    // voice_level_id: param ID for the voice's level (4, 7, 10, or 13)
    // voice_semi_id: param ID for the voice's semitone (6, 9, 12, or 15)
    let make =
        |name: &str, voice_level_id: u32, voice_semi_id: u32, semitones: f64| -> PitchScenario {
            let normalized = (semitones + 24.0) / 48.0;
            let mut params = base.clone();
            // Override the specific voice
            for p in &mut params {
                if p.0 == voice_level_id {
                    p.1 = 1.0;
                }
                if p.0 == voice_semi_id {
                    p.1 = normalized;
                }
            }
            PitchScenario {
                name: name.to_string(),
                ref_params: params,
                test_params: vec![],
            }
        };

    vec![
        make("two_oct_down", 4, 6, -24.0), // pitch1 → -24st
        make("one_oct_down", 7, 9, -12.0), // pitch2 → -12st
        make("one_oct_up", 10, 12, 12.0),  // pitch3 → +12st
        make("two_oct_up", 13, 15, 24.0),  // pitch4 → +24st
    ]
}

/// Resolve test params for a scenario.
///
/// If the test plugin has FTS Pitch params (Algorithm, Pitch, Mix), maps the
/// scenario name to the appropriate POG settings. Otherwise, falls back to
/// using the scenario's `ref_params` directly — this allows running a
/// reference plugin against its own captures as a harness self-test.
fn resolve_test_params(
    plugin: &mut LoadedPlugin,
    scenario: &PitchScenario,
) -> Result<Vec<(u32, f64)>> {
    let params = plugin.params();

    let find = |name: &str| -> Option<u32> {
        params
            .iter()
            .find(|p| p.name.eq_ignore_ascii_case(name))
            .map(|p| p.id)
    };

    // Try FTS Pitch CLAP params first
    let algo_id = find("Algorithm");
    let pitch_id = find("Pitch");
    let mix_id = find("Mix");

    if let (Some(algo_id), Some(pitch_id), Some(mix_id)) = (algo_id, pitch_id, mix_id) {
        // FTS Pitch plugin — map scenario to POG params
        let (semitones, output_boost_db): (f64, f64) = match scenario.name.as_str() {
            // Harmonitron voices have different output gains. These boosts were
            // empirically measured by comparing POG (unity-gain calibrated)
            // against Harmonitron captures across 5 GuitarSet clips.
            // pitch1 (-24st), pitch2 (-12st), pitch3 (+12st), pitch4 (+24st)
            "two_oct_down" => (-24.0, 12.5),
            "one_oct_down" => (-12.0, 12.0),
            "one_oct_up" => (12.0, 10.0),
            "two_oct_up" => (24.0, 2.0),
            _ => {
                return Ok(scenario.test_params.clone());
            }
        };

        // nih-plug CLAP convention: IntParam "plain" values are offsets from 0
        // to step_count, where step_count = max - min.
        //   Algorithm: IntParam(0..8)  → plain = value (min=0, no offset)
        //   Pitch:     IntParam(-24..24) → plain = value - min = value + 24
        // FloatParam plain values are actual parameter values (nih-plug calls
        // preview_normalized internally).
        let pitch_plain = semitones + 24.0; // offset from min (-24)

        let mut params = vec![
            (algo_id, 8.0),          // POG algorithm (IntParam 0..8, min=0)
            (pitch_id, pitch_plain), // Pitch as CLAP plain value
            (mix_id, 1.0),           // 100% wet (FloatParam 0..1)
        ];

        // Add output gain boost if the plugin has the Output param
        if let Some(output_id) = find("Output") {
            params.push((output_id, output_boost_db));
        }

        Ok(params)
    } else {
        // Non-FTS plugin (e.g. Harmonitron self-test) — use ref_params directly
        eprintln!("    (using ref_params for non-FTS plugin)");
        Ok(scenario.ref_params.clone())
    }
}

// ---------------------------------------------------------------------------
// Clip discovery
// ---------------------------------------------------------------------------

/// A discovered GuitarSet clip.
#[derive(Debug, Clone)]
pub struct Clip {
    pub name: String,
    pub path: PathBuf,
}

/// Curated 20-clip subset covering diverse playing styles and keys.
/// Balanced across: solo/comp, different players, different keys.
const CURATED_CLIPS: &[&str] = &[
    "00_BN1-129-Eb_comp",
    "00_BN1-129-Eb_solo",
    "00_Jazz1-200-F#_comp",
    "01_BN1-129-Eb_comp",
    "01_Funk1-114-Db_solo",
    "01_Rock2-142-Bb_comp",
    "02_BN2-166-Ab_solo",
    "02_Jazz2-187-F#_comp",
    "02_SS2-107-Ab_solo",
    "03_BN3-119-G#_comp",
    "03_Funk3-98-A_solo",
    "03_Jazz3-137-Eb_comp",
    "04_BN1-129-Eb_comp",
    "04_Funk1-114-Db_solo",
    "04_Rock1-90-C#_comp",
    "05_BN2-166-Ab_solo",
    "05_Jazz2-187-F#_comp",
    "05_Rock2-142-Bb_comp",
    "05_SS1-100-C#_solo",
    "05_SS3-98-C#_comp",
];

/// Discover GuitarSet mono-mix WAV clips.
pub fn discover_clips(guitarset_dir: &Path) -> Vec<Clip> {
    let mono_dir = guitarset_dir.join("audio_mono-pickup_mix");
    if !mono_dir.exists() {
        return Vec::new();
    }

    let mut clips: Vec<Clip> = std::fs::read_dir(&mono_dir)
        .unwrap()
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| {
            p.extension().is_some_and(|e| e == "wav")
                && p.file_name()
                    .and_then(|n| n.to_str())
                    .is_some_and(|n| n.contains("_mix"))
        })
        .map(|p| {
            let name = p
                .file_stem()
                .unwrap()
                .to_string_lossy()
                .strip_suffix("_mix")
                .unwrap_or(&p.file_stem().unwrap().to_string_lossy())
                .to_string();
            Clip { name, path: p }
        })
        .collect();

    clips.sort_by(|a, b| a.name.cmp(&b.name));
    clips
}

/// Filter clips to the curated subset.
pub fn curated_subset(clips: &[Clip]) -> Vec<Clip> {
    CURATED_CLIPS
        .iter()
        .filter_map(|name| clips.iter().find(|c| c.name == *name).cloned())
        .collect()
}

/// Read a WAV file as f32 mono samples, converting to mono if needed.
pub fn read_clip(path: &Path, max_duration_s: f32, sample_rate: f64) -> Result<Vec<f32>> {
    let mut reader = hound::WavReader::open(path)
        .with_context(|| format!("failed to open WAV: {}", path.display()))?;

    let spec = reader.spec();
    let max_samples = (max_duration_s * sample_rate as f32) as usize;

    let samples: Vec<f32> = match spec.sample_format {
        hound::SampleFormat::Float => reader
            .samples::<f32>()
            .take(max_samples * spec.channels as usize)
            .filter_map(|s| s.ok())
            .collect(),
        hound::SampleFormat::Int => {
            let scale = 1.0 / (1u32 << (spec.bits_per_sample - 1)) as f32;
            reader
                .samples::<i32>()
                .take(max_samples * spec.channels as usize)
                .filter_map(|s| s.ok())
                .map(|s| s as f32 * scale)
                .collect()
        }
    };

    // Mix to mono if stereo
    if spec.channels > 1 {
        let ch = spec.channels as usize;
        let mono: Vec<f32> = samples
            .chunks(ch)
            .take(max_samples)
            .map(|frame| frame.iter().sum::<f32>() / ch as f32)
            .collect();
        Ok(mono)
    } else {
        Ok(samples.into_iter().take(max_samples).collect())
    }
}

// ---------------------------------------------------------------------------
// Binary format: one file per clip per scenario
// ---------------------------------------------------------------------------
// Layout: [num_samples: u32 LE][f32 LE × num_samples]

fn write_clip_bin(path: &Path, data: &[f32]) -> Result<()> {
    let num_samples = data.len() as u32;
    let mut f = std::io::BufWriter::new(std::fs::File::create(path)?);
    f.write_all(&num_samples.to_le_bytes())?;
    for &sample in data {
        f.write_all(&sample.to_le_bytes())?;
    }
    f.flush()?;
    Ok(())
}

fn read_clip_bin(path: &Path) -> Result<Vec<f32>> {
    let mut f = std::io::BufReader::new(std::fs::File::open(path)?);
    let mut header = [0u8; 4];
    f.read_exact(&mut header)?;
    let num_samples = u32::from_le_bytes(header) as usize;

    let mut data = vec![0f32; num_samples];
    let byte_slice =
        unsafe { std::slice::from_raw_parts_mut(data.as_mut_ptr() as *mut u8, num_samples * 4) };
    f.read_exact(byte_slice)?;
    Ok(data)
}

// ---------------------------------------------------------------------------
// Capture
// ---------------------------------------------------------------------------

/// Capture reference plugin output across scenarios × clips.
pub fn run_capture(
    plugin_path: &str,
    output_dir: &Path,
    sample_rate: f64,
    block_size: u32,
    guitarset_dir: &Path,
    max_clip_duration: f32,
    scenarios: &[PitchScenario],
    limit: Option<usize>,
    use_curated: bool,
) -> Result<()> {
    std::fs::create_dir_all(output_dir)?;

    // Discover clips
    let all_clips = discover_clips(guitarset_dir);
    if all_clips.is_empty() {
        anyhow::bail!("no GuitarSet clips found in {}", guitarset_dir.display());
    }

    let clips = if use_curated {
        let curated = curated_subset(&all_clips);
        if curated.is_empty() {
            eprintln!(
                "Warning: no curated clips found, using all {} clips",
                all_clips.len()
            );
            all_clips
        } else {
            curated
        }
    } else {
        all_clips
    };

    let clips = if let Some(max) = limit {
        clips.into_iter().take(max).collect::<Vec<_>>()
    } else {
        clips
    };

    eprintln!("GuitarSet dir: {}", guitarset_dir.display());
    eprintln!("Clips: {}", clips.len());
    eprintln!("Scenarios: {}", scenarios.len());
    eprintln!("Max clip duration: {:.1}s", max_clip_duration);
    eprintln!("Sample rate: {} Hz", sample_rate);
    eprintln!();

    // Write metadata
    let meta = serde_json::json!({
        "plugin_path": plugin_path,
        "sample_rate": sample_rate,
        "block_size": block_size,
        "max_clip_duration": max_clip_duration,
        "clips": clips.iter().map(|c| &c.name).collect::<Vec<_>>(),
        "scenarios": scenarios.iter().map(|s| serde_json::json!({
            "name": &s.name,
            "ref_params": s.ref_params.iter().map(|(id, v)| serde_json::json!({"id": id, "value": v})).collect::<Vec<_>>(),
        })).collect::<Vec<_>>(),
    });
    std::fs::write(
        output_dir.join("metadata.json"),
        serde_json::to_string_pretty(&meta)?,
    )?;

    let total = scenarios.len() * clips.len();
    let completed = Arc::new(AtomicUsize::new(0));

    // Process: for each scenario, load a fresh plugin instance and process all clips.
    // Reload between clips to reset Kuassa trial noise timer.
    let load_lock = Arc::new(Mutex::new(()));

    for scenario in scenarios {
        let scenario_dir = output_dir.join(&scenario.name);
        std::fs::create_dir_all(&scenario_dir)?;

        eprintln!("--- Scenario: {} ---", scenario.name);

        for clip in &clips {
            // Load fresh plugin instance per clip (resets noise timer)
            let mut plugin = {
                let _guard = load_lock.lock().unwrap();
                LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)
                    .with_context(|| format!("failed to load plugin for clip {}", clip.name))?
            };

            let input = read_clip(&clip.path, max_clip_duration, sample_rate)?;
            let output = plugin.process(&input, &scenario.ref_params)?;

            let bin_path = scenario_dir.join(format!("{}.bin", clip.name));
            write_clip_bin(&bin_path, &output)?;

            let done = completed.fetch_add(1, Ordering::Relaxed) + 1;
            let in_rms = analysis::rms_db(&input);
            let out_rms = analysis::rms_db(&output);
            eprintln!(
                "[{:>4}/{}] {} — in={:.1} dB, out={:.1} dB",
                done, total, clip.name, in_rms, out_rms,
            );
        }
    }

    let total_size = dir_size(output_dir);
    eprintln!(
        "\nResults saved to {} ({:.1} MB)",
        output_dir.display(),
        total_size as f64 / 1_048_576.0,
    );
    Ok(())
}

// ---------------------------------------------------------------------------
// Analysis helpers
// ---------------------------------------------------------------------------

/// Frequency bands for per-band spectral comparison.
const BANDS: &[(&str, f32, f32)] = &[
    ("sub", 20.0, 80.0),
    ("low", 80.0, 250.0),
    ("mid", 250.0, 2000.0),
    ("high", 2000.0, 6000.0),
    ("presence", 6000.0, 12000.0),
    ("air", 12000.0, 20000.0),
];

/// RMS energy in a frequency band, computed via Welch-averaged FFT.
fn band_rms_db(signal: &[f32], sample_rate: f64, lo_hz: f32, hi_hz: f32) -> f32 {
    let fft_size = analysis::DEFAULT_FFT_SIZE;
    let sr = sample_rate as f32;
    let bin_hz = sr / fft_size as f32;
    let num_bins = fft_size / 2;

    let mut planner = rustfft::FftPlanner::new();
    let fft = planner.plan_fft_forward(fft_size);

    let mut sum_band_power = 0.0f64;
    let mut num_windows = 0usize;

    let mut offset = 0;
    while offset + fft_size <= signal.len() {
        let mut buffer: Vec<rustfft::num_complex::Complex<f32>> = (0..fft_size)
            .map(|i| {
                let w = 0.5
                    * (1.0 - (2.0 * std::f32::consts::PI * i as f32 / (fft_size - 1) as f32).cos());
                rustfft::num_complex::Complex::new(signal[offset + i] * w, 0.0)
            })
            .collect();

        fft.process(&mut buffer);

        for k in 0..num_bins {
            let freq = k as f32 * bin_hz;
            if freq >= lo_hz && freq < hi_hz {
                let mag = buffer[k].norm();
                sum_band_power += (mag as f64) * (mag as f64);
            }
        }
        num_windows += 1;
        offset += fft_size;
    }

    if num_windows == 0 {
        return f32::NEG_INFINITY;
    }

    let avg_power = sum_band_power / num_windows as f64;
    let rms = avg_power.sqrt();
    if rms <= 1e-10 {
        f32::NEG_INFINITY
    } else {
        20.0 * rms.log10() as f32
    }
}

/// Per-band energy comparison between ref and test signals.
fn compare_bands(
    ref_signal: &[f32],
    test_signal: &[f32],
    sample_rate: f64,
) -> Vec<(String, f32, f32, f32)> {
    BANDS
        .iter()
        .map(|&(name, lo, hi)| {
            let ref_db = band_rms_db(ref_signal, sample_rate, lo, hi);
            let test_db = band_rms_db(test_signal, sample_rate, lo, hi);
            let diff = if ref_db.is_finite() && test_db.is_finite() {
                test_db - ref_db
            } else {
                0.0
            };
            (name.to_string(), ref_db, test_db, diff)
        })
        .collect()
}

/// Windowed RMS envelope (returns dB values at `hop_ms` intervals).
fn rms_envelope(signal: &[f32], sample_rate: f64, window_ms: f32, hop_ms: f32) -> Vec<f32> {
    let sr = sample_rate as f32;
    let window_samples = (sr * window_ms / 1000.0) as usize;
    let hop_samples = (sr * hop_ms / 1000.0) as usize;

    let mut envelope = Vec::new();
    let mut pos = 0;
    while pos + window_samples <= signal.len() {
        let rms = analysis::rms_db(&signal[pos..pos + window_samples]);
        envelope.push(rms);
        pos += hop_samples;
    }
    envelope
}

/// Envelope correlation between ref and test (Pearson r, ignoring silence).
fn envelope_correlation(ref_env: &[f32], test_env: &[f32]) -> f32 {
    let silence_threshold = -60.0;
    let pairs: Vec<(f64, f64)> = ref_env
        .iter()
        .zip(test_env.iter())
        .filter(|&(&r, &t)| r > silence_threshold && t > silence_threshold)
        .map(|(&r, &t)| (r as f64, t as f64))
        .collect();

    if pairs.len() < 3 {
        return 0.0;
    }

    let n = pairs.len() as f64;
    let mean_r = pairs.iter().map(|(r, _)| r).sum::<f64>() / n;
    let mean_t = pairs.iter().map(|(_, t)| t).sum::<f64>() / n;

    let mut cov = 0.0;
    let mut var_r = 0.0;
    let mut var_t = 0.0;
    for &(r, t) in &pairs {
        cov += (r - mean_r) * (t - mean_t);
        var_r += (r - mean_r) * (r - mean_r);
        var_t += (t - mean_t) * (t - mean_t);
    }

    let denom = (var_r * var_t).sqrt();
    if denom < 1e-10 {
        0.0
    } else {
        (cov / denom) as f32
    }
}

/// Cross-correlation latency measurement between two signals.
/// Returns (lag_samples, correlation_peak) where positive lag means test is delayed.
fn measure_latency_xcorr(
    ref_signal: &[f32],
    test_signal: &[f32],
    sample_rate: f64,
    max_lag_ms: f32,
) -> (i32, f32) {
    let max_lag = (sample_rate as f32 * max_lag_ms / 1000.0) as usize;
    let len = ref_signal.len().min(test_signal.len());
    if len < max_lag * 2 {
        return (0, 0.0);
    }

    // Use envelope-based cross-correlation (more robust for pitch-shifted signals)
    let sr = sample_rate as f32;
    let hop = (sr * 0.5 / 1000.0) as usize; // 0.5ms hop
    let win = (sr * 5.0 / 1000.0) as usize; // 5ms window

    let ref_env: Vec<f32> = (0..len / hop)
        .map(|i| {
            let start = i * hop;
            let end = (start + win).min(len);
            analysis::rms(&ref_signal[start..end])
        })
        .collect();

    let test_env: Vec<f32> = (0..len / hop)
        .map(|i| {
            let start = i * hop;
            let end = (start + win).min(len);
            analysis::rms(&test_signal[start..end])
        })
        .collect();

    let max_lag_env = max_lag / hop;
    let search_len = ref_env.len().min(test_env.len());
    if search_len < max_lag_env * 2 {
        return (0, 0.0);
    }

    let mut best_lag: i32 = 0;
    let mut best_corr: f64 = f64::NEG_INFINITY;

    for lag in -(max_lag_env as i32)..=(max_lag_env as i32) {
        let mut sum = 0.0f64;
        let mut count = 0usize;
        for i in max_lag_env..search_len - max_lag_env {
            let j = i as i32 + lag;
            if j >= 0 && (j as usize) < test_env.len() {
                sum += ref_env[i] as f64 * test_env[j as usize] as f64;
                count += 1;
            }
        }
        if count > 0 {
            let corr = sum / count as f64;
            if corr > best_corr {
                best_corr = corr;
                best_lag = lag;
            }
        }
    }

    (best_lag * hop as i32, best_corr as f32)
}

// ---------------------------------------------------------------------------
// Data export helpers
// ---------------------------------------------------------------------------

/// Compute averaged power spectrum in dB (Welch's method).
/// Returns Vec<(freq_hz, power_db)> for bins in [min_hz, max_hz].
fn power_spectrum_db(
    signal: &[f32],
    sample_rate: f64,
    min_hz: f32,
    max_hz: f32,
) -> Vec<(f32, f32)> {
    let fft_size = analysis::DEFAULT_FFT_SIZE;
    let sr = sample_rate as f32;
    let bin_hz = sr / fft_size as f32;
    let num_bins = fft_size / 2;

    let mut planner = rustfft::FftPlanner::new();
    let fft = planner.plan_fft_forward(fft_size);

    let mut sum_mag_sq = vec![0.0f64; num_bins];
    let mut num_windows = 0usize;

    let mut offset = 0;
    while offset + fft_size <= signal.len() {
        let mut buffer: Vec<rustfft::num_complex::Complex<f32>> = (0..fft_size)
            .map(|i| {
                let w = 0.5
                    * (1.0 - (2.0 * std::f32::consts::PI * i as f32 / (fft_size - 1) as f32).cos());
                rustfft::num_complex::Complex::new(signal[offset + i] * w, 0.0)
            })
            .collect();
        fft.process(&mut buffer);
        for k in 0..num_bins {
            let mag = buffer[k].norm();
            sum_mag_sq[k] += (mag as f64) * (mag as f64);
        }
        num_windows += 1;
        offset += fft_size;
    }

    if num_windows == 0 {
        return Vec::new();
    }

    let mut result = Vec::new();
    for k in 0..num_bins {
        let freq = k as f32 * bin_hz;
        if freq < min_hz || freq > max_hz {
            continue;
        }
        let avg_power = sum_mag_sq[k] / num_windows as f64;
        let db = if avg_power <= 1e-20 {
            -120.0
        } else {
            10.0 * avg_power.log10() as f32
        };
        result.push((freq, db));
    }
    result
}

/// Detect the top N spectral peaks (local maxima above threshold).
fn detect_peaks(spectrum: &[(f32, f32)], top_n: usize, min_db: f32) -> Vec<(f32, f32)> {
    let mut peaks: Vec<(f32, f32)> = Vec::new();

    for i in 1..spectrum.len().saturating_sub(1) {
        let (freq, db) = spectrum[i];
        if db < min_db {
            continue;
        }
        if db > spectrum[i - 1].1 && db > spectrum[i + 1].1 {
            peaks.push((freq, db));
        }
    }

    peaks.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    peaks.truncate(top_n);
    peaks.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
    peaks
}

/// Write per-clip spectral curve CSV (ref + test power spectrum side by side).
fn write_spectral_csv(
    path: &Path,
    ref_spectrum: &[(f32, f32)],
    test_spectrum: &[(f32, f32)],
) -> Result<()> {
    let mut f = std::io::BufWriter::new(std::fs::File::create(path)?);
    writeln!(f, "freq_hz,ref_db,test_db,diff_db")?;

    // Align by frequency (both should have same bins from same FFT size)
    let mut ti = 0;
    for &(freq, ref_db) in ref_spectrum {
        // Find nearest test bin
        while ti + 1 < test_spectrum.len()
            && (test_spectrum[ti + 1].0 - freq).abs() < (test_spectrum[ti].0 - freq).abs()
        {
            ti += 1;
        }
        let test_db = if ti < test_spectrum.len() {
            test_spectrum[ti].1
        } else {
            -120.0
        };
        writeln!(
            f,
            "{:.1},{:.2},{:.2},{:.2}",
            freq,
            ref_db,
            test_db,
            test_db - ref_db
        )?;
    }
    f.flush()?;
    Ok(())
}

/// Write per-clip envelope CSV (ref + test RMS over time).
fn write_envelope_csv(path: &Path, ref_env: &[f32], test_env: &[f32], hop_ms: f32) -> Result<()> {
    let mut f = std::io::BufWriter::new(std::fs::File::create(path)?);
    writeln!(f, "time_ms,ref_rms_db,test_rms_db,diff_db")?;

    let len = ref_env.len().min(test_env.len());
    for i in 0..len {
        let t = i as f32 * hop_ms;
        let diff = if ref_env[i].is_finite() && test_env[i].is_finite() {
            test_env[i] - ref_env[i]
        } else {
            0.0
        };
        writeln!(
            f,
            "{:.1},{:.2},{:.2},{:.2}",
            t, ref_env[i], test_env[i], diff
        )?;
    }
    f.flush()?;
    Ok(())
}

/// Write spectral peaks JSON for a clip.
fn peaks_to_json(peaks: &[(f32, f32)]) -> Vec<serde_json::Value> {
    peaks
        .iter()
        .map(|&(freq, db)| serde_json::json!({"freq_hz": freq, "db": db}))
        .collect()
}

/// Detailed per-clip comparison result.
#[derive(Debug)]
struct ClipComparison {
    clip_name: String,
    // Level
    ref_rms_db: f32,
    test_rms_db: f32,
    rms_diff_db: f32,
    ref_peak_db: f32,
    test_peak_db: f32,
    // Spectral
    spectral_rms_db: f32,
    spectral_max_db: f32,
    spectral_max_freq_hz: f32,
    // Per-band
    bands: Vec<(String, f32, f32, f32)>, // (name, ref_db, test_db, diff_db)
    // Temporal
    envelope_correlation: f32,
    // Latency
    measured_latency_samples: i32,
    xcorr_peak: f32,
}

// ---------------------------------------------------------------------------
// Compare
// ---------------------------------------------------------------------------

/// Compare a test plugin against captured reference data.
pub fn run_compare(
    plugin_path: &str,
    reference_dir: &Path,
    output_dir: &Path,
    sample_rate: f64,
    block_size: u32,
    guitarset_dir: &Path,
    max_clip_duration: f32,
    tolerance_rms_db: f32,
    tolerance_spectral_db: f32,
    scenarios: &[PitchScenario],
) -> Result<()> {
    std::fs::create_dir_all(output_dir)?;

    // Load reference metadata
    let meta_path = reference_dir.join("metadata.json");
    let meta: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(&meta_path)
            .with_context(|| format!("no metadata.json in {}", reference_dir.display()))?,
    )?;

    let ref_clips: Vec<String> = meta["clips"]
        .as_array()
        .map(|a| {
            a.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect()
        })
        .unwrap_or_default();

    // Discover clips for input audio
    let all_clips = discover_clips(guitarset_dir);
    let clips: Vec<Clip> = ref_clips
        .iter()
        .filter_map(|name| all_clips.iter().find(|c| c.name == *name).cloned())
        .collect();

    if clips.is_empty() {
        anyhow::bail!("no matching clips found between reference and GuitarSet dir");
    }

    eprintln!("Reference: {}", reference_dir.display());
    eprintln!("Plugin: {}", plugin_path);
    eprintln!("Clips: {}", clips.len());
    eprintln!(
        "Tolerance: RMS {:.1} dB, spectral {:.1} dB",
        tolerance_rms_db, tolerance_spectral_db
    );
    eprintln!();

    // Resolve params once with a temporary plugin instance
    let mut tmp_plugin = LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)?;
    let reported_latency = tmp_plugin.latency() as usize;
    eprintln!(
        "Reported latency: {} samples ({:.1} ms)",
        reported_latency,
        reported_latency as f64 / sample_rate * 1000.0
    );

    // Pre-resolve test params for all scenarios
    let mut scenario_params: Vec<Vec<(u32, f64)>> = Vec::new();
    for scenario in scenarios {
        scenario_params.push(resolve_test_params(&mut tmp_plugin, scenario)?);
    }
    drop(tmp_plugin);

    let mut all_comparisons: Vec<ClipComparison> = Vec::new();
    let mut results_json: Vec<serde_json::Value> = Vec::new();

    for (si, scenario) in scenarios.iter().enumerate() {
        let scenario_ref_dir = reference_dir.join(&scenario.name);
        if !scenario_ref_dir.exists() {
            eprintln!(
                "Warning: reference scenario '{}' not found, skipping",
                scenario.name
            );
            continue;
        }

        let test_params = &scenario_params[si];

        let scenario_out_dir = output_dir.join(&scenario.name);
        std::fs::create_dir_all(&scenario_out_dir)?;

        eprintln!("--- Scenario: {} ---", scenario.name);
        for (id, val) in test_params {
            eprintln!("  test param {}={:.2}", id, val);
        }

        for clip in &clips {
            let ref_bin_path = scenario_ref_dir.join(format!("{}.bin", clip.name));
            if !ref_bin_path.exists() {
                continue;
            }

            let ref_output = read_clip_bin(&ref_bin_path)
                .with_context(|| format!("failed to read {}", ref_bin_path.display()))?;

            // Reload plugin per clip to reset Kuassa trial noise timer
            let mut plugin =
                LoadedPlugin::load(plugin_path.as_ref(), 0, sample_rate, block_size)
                    .with_context(|| format!("failed to reload plugin for clip {}", clip.name))?;

            let input = read_clip(&clip.path, max_clip_duration, sample_rate)?;
            let test_output_raw = plugin.process(&input, test_params)?;

            // Compensate for reported latency
            let test_output = if reported_latency > 0 && test_output_raw.len() > reported_latency {
                test_output_raw[reported_latency..].to_vec()
            } else {
                test_output_raw
            };

            // Align lengths
            let len = ref_output.len().min(test_output.len());
            let ref_trimmed = &ref_output[..len];
            let test_trimmed = &test_output[..len];

            // --- Metrics ---

            // 1. RMS & peak level
            let ref_rms = analysis::rms_db(ref_trimmed);
            let test_rms = analysis::rms_db(test_trimmed);
            let rms_diff = (test_rms - ref_rms).abs();
            let ref_peak = analysis::peak_db(ref_trimmed);
            let test_peak = analysis::peak_db(test_trimmed);

            // 2. Spectral comparison (per-bin FFT transfer function diff)
            let spectral = analysis::compare_freq_response(
                &input[..len],
                ref_trimmed,
                test_trimmed,
                sample_rate,
                20.0,
                20000.0,
            );

            // 3. Full output power spectra (for curve export)
            let ref_spectrum = power_spectrum_db(ref_trimmed, sample_rate, 20.0, 20000.0);
            let test_spectrum = power_spectrum_db(test_trimmed, sample_rate, 20.0, 20000.0);

            // 4. Spectral peak detection (top 20 peaks above -40 dB)
            let ref_peaks = detect_peaks(&ref_spectrum, 20, -40.0);
            let test_peaks = detect_peaks(&test_spectrum, 20, -40.0);

            // 5. Per-band energy
            let bands = compare_bands(ref_trimmed, test_trimmed, sample_rate);

            // 6. Envelope correlation
            let hop_ms = 10.0;
            let ref_env = rms_envelope(ref_trimmed, sample_rate, 20.0, hop_ms);
            let test_env = rms_envelope(test_trimmed, sample_rate, 20.0, hop_ms);
            let env_corr = envelope_correlation(&ref_env, &test_env);

            // 7. Cross-correlation latency
            let (xcorr_lag, xcorr_peak) =
                measure_latency_xcorr(ref_trimmed, test_trimmed, sample_rate, 50.0);

            let cmp = ClipComparison {
                clip_name: clip.name.clone(),
                ref_rms_db: ref_rms,
                test_rms_db: test_rms,
                rms_diff_db: rms_diff,
                ref_peak_db: ref_peak,
                test_peak_db: test_peak,
                spectral_rms_db: spectral.rms_diff_db,
                spectral_max_db: spectral.max_diff_db,
                spectral_max_freq_hz: spectral.max_diff_freq_hz,
                bands: bands.clone(),
                envelope_correlation: env_corr,
                measured_latency_samples: xcorr_lag,
                xcorr_peak,
            };

            // Print summary line
            let pass =
                rms_diff <= tolerance_rms_db && spectral.rms_diff_db <= tolerance_spectral_db;
            let status = if pass { "PASS" } else { "FAIL" };
            eprintln!(
                "  {} {} — Δrms={:.1} dB  spec_rms={:.1} dB  spec_max={:.1} dB@{:.0}Hz  env_r={:.3}  lat={:+}smp",
                status,
                clip.name,
                rms_diff,
                spectral.rms_diff_db,
                spectral.max_diff_db,
                spectral.max_diff_freq_hz,
                env_corr,
                xcorr_lag,
            );

            // Print per-band breakdown
            for (name, ref_db, test_db, diff) in &bands {
                if ref_db.is_finite() || test_db.is_finite() {
                    eprintln!(
                        "    {:<10} ref={:>6.1} dB  test={:>6.1} dB  Δ={:>+5.1} dB",
                        name, ref_db, test_db, diff,
                    );
                }
            }

            // Print top peaks comparison
            eprintln!(
                "    ref peaks: {}",
                ref_peaks
                    .iter()
                    .map(|(f, db)| format!("{:.0}Hz({:.0})", f, db))
                    .collect::<Vec<_>>()
                    .join("  ")
            );
            eprintln!(
                "    test peaks: {}",
                test_peaks
                    .iter()
                    .map(|(f, db)| format!("{:.0}Hz({:.0})", f, db))
                    .collect::<Vec<_>>()
                    .join("  ")
            );

            // --- Data exports ---

            // Save test output binary
            let out_bin_path = scenario_out_dir.join(format!("{}.bin", clip.name));
            write_clip_bin(&out_bin_path, test_trimmed)?;

            // Save spectral curve CSV
            let spectral_csv_path = scenario_out_dir.join(format!("{}_spectrum.csv", clip.name));
            write_spectral_csv(&spectral_csv_path, &ref_spectrum, &test_spectrum)?;

            // Save envelope curve CSV
            let envelope_csv_path = scenario_out_dir.join(format!("{}_envelope.csv", clip.name));
            write_envelope_csv(&envelope_csv_path, &ref_env, &test_env, hop_ms)?;

            // Build JSON result
            let band_json: Vec<serde_json::Value> = bands
                .iter()
                .map(|(name, ref_db, test_db, diff)| {
                    serde_json::json!({
                        "band": name,
                        "ref_db": ref_db,
                        "test_db": test_db,
                        "diff_db": diff,
                    })
                })
                .collect();

            results_json.push(serde_json::json!({
                "scenario": scenario.name,
                "clip": clip.name,
                "pass": pass,
                "rms": {
                    "ref_db": ref_rms,
                    "test_db": test_rms,
                    "diff_db": rms_diff,
                },
                "peak": {
                    "ref_db": ref_peak,
                    "test_db": test_peak,
                },
                "spectral": {
                    "rms_diff_db": spectral.rms_diff_db,
                    "max_diff_db": spectral.max_diff_db,
                    "max_diff_freq_hz": spectral.max_diff_freq_hz,
                },
                "bands": band_json,
                "envelope_correlation": env_corr,
                "ref_peaks": peaks_to_json(&ref_peaks),
                "test_peaks": peaks_to_json(&test_peaks),
                "latency": {
                    "reported_samples": reported_latency,
                    "measured_samples": xcorr_lag,
                    "measured_ms": xcorr_lag as f64 / sample_rate * 1000.0,
                    "xcorr_peak": xcorr_peak,
                },
            }));

            all_comparisons.push(cmp);
        }

        eprintln!();
    }

    // Write detailed JSON results
    let compare_meta = serde_json::json!({
        "reference": reference_dir.to_string_lossy(),
        "plugin": plugin_path,
        "sample_rate": sample_rate,
        "tolerance_rms_db": tolerance_rms_db,
        "tolerance_spectral_db": tolerance_spectral_db,
        "reported_latency_samples": reported_latency,
        "results": results_json,
    });
    std::fs::write(
        output_dir.join("comparison.json"),
        serde_json::to_string_pretty(&compare_meta)?,
    )?;

    // Write CSV summary
    write_comparison_csv(output_dir, &all_comparisons)?;

    // Print aggregate summary
    let total = all_comparisons.len();
    let pass_count = all_comparisons
        .iter()
        .filter(|c| c.rms_diff_db <= tolerance_rms_db && c.spectral_rms_db <= tolerance_spectral_db)
        .count();
    let fail_count = total - pass_count;

    let avg_rms_diff = all_comparisons.iter().map(|c| c.rms_diff_db).sum::<f32>() / total as f32;
    let worst_rms_diff = all_comparisons
        .iter()
        .map(|c| c.rms_diff_db)
        .fold(0.0f32, f32::max);
    let avg_spectral = all_comparisons
        .iter()
        .map(|c| c.spectral_rms_db)
        .sum::<f32>()
        / total as f32;
    let avg_env_corr = all_comparisons
        .iter()
        .map(|c| c.envelope_correlation)
        .sum::<f32>()
        / total as f32;

    // Per-band averages
    let num_bands = BANDS.len();
    let mut band_avg_diff = vec![0.0f32; num_bands];
    let mut band_count = vec![0usize; num_bands];
    for cmp in &all_comparisons {
        for (i, (_, _, _, diff)) in cmp.bands.iter().enumerate() {
            if diff.is_finite() {
                band_avg_diff[i] += diff.abs();
                band_count[i] += 1;
            }
        }
    }

    eprintln!("========================================================");
    eprintln!("  Results: {}/{} passed", pass_count, total);
    eprintln!(
        "  RMS diff:  avg={:.2} dB  worst={:.2} dB",
        avg_rms_diff, worst_rms_diff
    );
    eprintln!("  Spectral:  avg={:.2} dB", avg_spectral);
    eprintln!("  Envelope:  avg r={:.3}", avg_env_corr);
    eprintln!(
        "  Reported latency: {} samples ({:.1} ms)",
        reported_latency,
        reported_latency as f64 / sample_rate * 1000.0
    );
    eprintln!();
    eprintln!("  Per-band avg |Δ|:");
    for (i, &(name, _, _)) in BANDS.iter().enumerate() {
        if band_count[i] > 0 {
            let avg = band_avg_diff[i] / band_count[i] as f32;
            eprintln!("    {:<10} {:.2} dB", name, avg);
        }
    }
    eprintln!("========================================================");

    if fail_count > 0 {
        anyhow::bail!(
            "{}/{} tests exceeded tolerance (rms={:.1} dB, spectral={:.1} dB)",
            fail_count,
            total,
            tolerance_rms_db,
            tolerance_spectral_db,
        );
    }

    Ok(())
}

/// Write a CSV with one row per clip comparison.
fn write_comparison_csv(output_dir: &Path, comparisons: &[ClipComparison]) -> Result<()> {
    let csv_path = output_dir.join("comparison.csv");
    let mut f = std::io::BufWriter::new(std::fs::File::create(&csv_path)?);

    // Header
    write!(
        f,
        "clip,ref_rms_db,test_rms_db,rms_diff_db,ref_peak_db,test_peak_db"
    )?;
    write!(f, ",spectral_rms_db,spectral_max_db,spectral_max_freq_hz")?;
    for &(name, _, _) in BANDS {
        write!(f, ",{}_diff_db", name)?;
    }
    write!(f, ",envelope_corr,measured_latency_smp,xcorr_peak")?;
    writeln!(f)?;

    for c in comparisons {
        write!(
            f,
            "{},{:.2},{:.2},{:.2},{:.2},{:.2}",
            c.clip_name, c.ref_rms_db, c.test_rms_db, c.rms_diff_db, c.ref_peak_db, c.test_peak_db
        )?;
        write!(
            f,
            ",{:.2},{:.2},{:.1}",
            c.spectral_rms_db, c.spectral_max_db, c.spectral_max_freq_hz
        )?;
        for (_, _, _, diff) in &c.bands {
            write!(f, ",{:.2}", diff)?;
        }
        write!(
            f,
            ",{:.4},{},{:.6}",
            c.envelope_correlation, c.measured_latency_samples, c.xcorr_peak
        )?;
        writeln!(f)?;
    }

    f.flush()?;
    eprintln!("CSV written to {}", csv_path.display());
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
