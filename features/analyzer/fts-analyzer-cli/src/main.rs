// WIP CLI: tolerate dead code and stylistic clippy lints across analysis modules.
#![allow(dead_code)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::needless_range_loop)]
#![allow(clippy::ptr_arg)]
#![allow(clippy::unnecessary_unwrap)]

use std::path::PathBuf;

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use fts_analyzer::analysis;
use fts_analyzer::config::{AnalysisConfig, MeasureConfig};
use fts_analyzer::engine::MeasurementEngine;
// NOTE: fts_analyzer::plugin (AnyPlugin) module has been removed; use host::LoadedPlugin instead.
// use fts_analyzer::plugin::{self as plugin_loader, AnyPlugin};
use fts_analyzer::host::LoadedPlugin;
use fts_analyzer::signal;

mod compressor;
mod compressor_audio;
mod compressor_live;
mod eq;
mod fuzz_eq;
// mod hammerstein; // module file does not exist
mod pitch;
// mod profiles; // module file does not exist
mod resolve_params;

#[derive(Parser)]
#[command(
    name = "fts-analyzer",
    about = "Audio plugin analysis and comparison",
    allow_negative_numbers = true
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// List plugins in a .clap bundle.
    List {
        /// Path to the .clap file.
        path: String,
    },

    /// Show all parameters of a plugin.
    Params {
        /// Path to the .clap file.
        path: String,
        /// Plugin index within the bundle (default: 0).
        #[arg(short, long, default_value_t = 0)]
        index: usize,
    },

    /// Dump a parameter's value-to-text mapping across a range.
    DumpParam {
        /// Path to the .clap file.
        path: String,
        /// Parameter name (substring match).
        #[arg(long)]
        name: String,
        /// Minimum value to test.
        #[arg(long, default_value_t = 0.0)]
        min: f64,
        /// Maximum value to test.
        #[arg(long, default_value_t = 10.0)]
        max: f64,
        /// Step size.
        #[arg(long, default_value_t = 1.0)]
        step: f64,
        /// Plugin index within the bundle.
        #[arg(short, long, default_value_t = 0)]
        index: usize,
    },

    /// Process a test signal through a plugin and write the output to a WAV.
    Process {
        /// Path to the .clap file.
        path: String,
        /// Output WAV path.
        #[arg(short, long, default_value = "output.wav")]
        output: String,
        /// Plugin index within the bundle.
        #[arg(short, long, default_value_t = 0)]
        index: usize,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Test signal type: sine, noise, step, sweep.
        #[arg(long, default_value = "sine")]
        signal: String,
        /// Signal level in dBFS.
        #[arg(long, default_value_t = -6.0, allow_hyphen_values = true)]
        level_db: f32,
        /// Duration in seconds.
        #[arg(long, default_value_t = 2.0)]
        duration: f32,
        /// Parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
        /// Raw VST2 state chunk as comma-separated values (loaded before processing).
        /// Example: "1.0,0.5,0.208,0.0,1.0,0.0,0.0,0.0" for Pitchproof at 0 semitones, 100% wet.
        #[arg(long)]
        state_chunk: Option<String>,
    },

    /// Run grid-based measurement sweep from a JSON config file.
    MeasureGrid {
        /// Path to the JSON configuration file.
        #[arg(long)]
        config: PathBuf,
        /// Output directory for CSV results.
        #[arg(long)]
        out: PathBuf,
        /// Override pluginPath in config.
        #[arg(long)]
        plugin: Option<PathBuf>,
        /// Override duration in seconds.
        #[arg(long)]
        seconds: Option<f32>,
        /// Override sample rate.
        #[arg(long)]
        samplerate: Option<f64>,
        /// Override block size.
        #[arg(long)]
        blocksize: Option<u32>,
    },

    /// Compare two plugins processing the same test signal.
    Compare {
        /// Path to the first .clap file.
        plugin_a: String,
        /// Path to the second .clap file.
        plugin_b: String,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Test signal type: sine, noise, step, sweep.
        #[arg(long, default_value = "sine")]
        signal: String,
        /// Signal level in dBFS.
        #[arg(long, default_value_t = -6.0, allow_hyphen_values = true)]
        level_db: f32,
        /// Duration in seconds.
        #[arg(long, default_value_t = 2.0)]
        duration: f32,
        /// Parameter overrides for plugin A as "id=value" pairs.
        #[arg(long)]
        param_a: Vec<String>,
        /// Parameter overrides for plugin B as "id=value" pairs.
        #[arg(long)]
        param_b: Vec<String>,
    },

    /// Run a multi-plugin analysis from a JSON analysis config.
    ///
    /// Measures each plugin with the same shared settings, writing results
    /// to per-plugin subdirectories. Prints a comparison summary at the end.
    RunAnalysis {
        /// Path to the analysis JSON config file.
        #[arg(long)]
        config: PathBuf,
        /// Base output directory. Each plugin gets a subdirectory.
        #[arg(long)]
        out: PathBuf,
    },

    /// Capture a compressor's gain reduction curves across scenarios × frequencies.
    ///
    /// Uses a pulse-tone signal (carrier alternating between two gain levels)
    /// to measure the full compression curve including attack, release, and
    /// frequency-dependent behavior. Results are saved as compact CSV files
    /// that serve as a reference for comparison.
    CaptureCompressor {
        /// Load defaults from a capture profile JSON file. CLI args override profile values.
        #[arg(long)]
        profile: Option<PathBuf>,
        /// Path to the .clap plugin file. Required unless --profile provides it.
        plugin: Option<String>,
        /// Output directory for reference data.
        #[arg(long)]
        out: Option<PathBuf>,
        /// JSON file defining test scenarios (attack/release combos etc).
        /// If omitted, runs a single "default" scenario with no extra params.
        #[arg(long)]
        scenarios: Option<PathBuf>,
        /// High gain level in dBFS.
        #[arg(long, default_value_t = -6.0, allow_hyphen_values = true)]
        gain_high: f32,
        /// Low gain level in dBFS.
        #[arg(long, default_value_t = -20.0, allow_hyphen_values = true)]
        gain_low: f32,
        /// Duration of high-gain phase in ms.
        #[arg(long, default_value_t = 240.0)]
        time_high: f32,
        /// Duration of low-gain phase in ms.
        #[arg(long, default_value_t = 240.0)]
        time_low: f32,
        /// Carrier waveform: sine, square, saw, noise.
        #[arg(long, default_value = "sine")]
        waveform: String,
        /// Test signal duration in seconds.
        #[arg(long, default_value_t = 3.0)]
        duration: f32,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Number of log-spaced test frequencies.
        #[arg(long, default_value_t = 200)]
        num_freqs: usize,
        /// Start frequency in Hz.
        #[arg(long, default_value_t = 20.0)]
        freq_start: f32,
        /// End frequency in Hz.
        #[arg(long, default_value_t = 20000.0)]
        freq_end: f32,
        /// Base parameter overrides as "id=value" pairs (applied to all scenarios).
        #[arg(long)]
        param: Vec<String>,
        /// Maximum number of scenarios to run.
        #[arg(long)]
        limit: Option<usize>,
        /// Number of worker threads (default: auto-detect).
        #[arg(long)]
        threads: Option<usize>,
    },

    /// Compare a plugin's compressor behavior against a reference for a single scenario.
    ///
    /// Tests a specific attack and release time pair against saved reference data.
    /// Outputs pass/fail per frequency with RMS and max gain reduction differences.
    CompareCompressor {
        /// Path to the .clap plugin file to test.
        plugin: String,
        /// Directory containing the reference capture.
        #[arg(long)]
        reference: PathBuf,
        /// Output directory for comparison results.
        #[arg(long)]
        out: PathBuf,
        /// Attack time in milliseconds.
        #[arg(long)]
        attack_ms: f64,
        /// Release time in milliseconds.
        #[arg(long)]
        release_ms: f64,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Maximum allowed RMS gain reduction difference in dB.
        #[arg(long, default_value_t = 1.0)]
        tolerance_db: f32,
        /// Base parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
        /// Remap reference param IDs to test plugin param names.
        ///
        /// Format: FROM_ID=TO_NAME (e.g. "7=Attack" "8=Release").
        #[arg(long)]
        param_remap: Vec<String>,
        /// Filter frequencies by value in Hz (e.g. --freq-filter 8000 --freq-filter 16000).
        /// Only matching frequencies are tested.
        #[arg(long)]
        freq_filter: Vec<f32>,
    },

    /// Compare two live plugins (reference vs test) in real-time.
    ///
    /// Loads both a reference plugin (e.g., Pro-C 3) and a test plugin,
    /// processes identical test signals through both, and compares gain reduction curves.
    /// No pre-captured reference data required.
    CompareCompressorLive {
        /// Path to the reference plugin (.clap, .vst2, .vst3).
        #[arg(long)]
        reference: String,
        /// Path to the test plugin (.clap, .vst2, .vst3).
        plugin: String,
        /// Output directory for comparison results.
        #[arg(long)]
        out: PathBuf,
        /// Attack time in milliseconds.
        #[arg(long)]
        attack_ms: f64,
        /// Release time in milliseconds.
        #[arg(long)]
        release_ms: f64,
        /// Threshold in dB (optional).
        #[arg(long)]
        threshold_db: Option<f64>,
        /// Ratio (optional).
        #[arg(long)]
        ratio: Option<f64>,
        /// Knee width in dB (optional).
        #[arg(long)]
        knee_db: Option<f64>,
        /// Maximum allowed RMS gain reduction difference in dB.
        #[arg(long, default_value_t = 1.0)]
        tolerance_db: f32,
        /// Remap reference param IDs to test plugin param names.
        /// Format: FROM_ID=TO_NAME (e.g. "7=Attack" "8=Release").
        #[arg(long)]
        param_remap: Vec<String>,
        /// Filter frequencies by value in Hz.
        #[arg(long)]
        freq_filter: Vec<f32>,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
    },

    /// Compare a plugin's compressor behavior against a reference across all scenarios.
    ///
    /// Runs all test scenarios (combinations of attack/release times) and diffs
    /// the gain reduction curves against saved reference data.
    CompareCompressorAll {
        /// Load defaults from a compare profile JSON file. CLI args override profile values.
        #[arg(long)]
        profile: Option<PathBuf>,
        /// Path to the .clap plugin file to test. Required unless --profile provides it.
        plugin: Option<String>,
        /// Directory containing the reference capture.
        #[arg(long)]
        reference: Option<PathBuf>,
        /// Output directory for comparison results.
        #[arg(long)]
        out: Option<PathBuf>,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Maximum allowed RMS gain reduction difference in dB.
        #[arg(long, default_value_t = 1.0)]
        tolerance_db: f32,
        /// Base parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
        /// Remap reference param IDs to test plugin param names.
        ///
        /// Format: FROM_ID=TO_NAME (e.g. "7=Attack" "8=Release").
        /// The actual parameter value (in ms) is parsed from scenario names
        /// instead of using the reference's normalized 0–1 value.
        #[arg(long)]
        param_remap: Vec<String>,
        /// Filter scenarios by substring match (e.g. "rel-50ms" or "atk-0.01ms").
        /// Multiple filters are OR'd together.
        #[arg(long)]
        filter: Vec<String>,
        /// Maximum number of scenarios to run.
        #[arg(long)]
        limit: Option<usize>,
        /// Filter frequencies by value in Hz (e.g. --freq-filter 8000 --freq-filter 16000).
        /// Only matching frequencies are tested.
        #[arg(long)]
        freq_filter: Vec<f32>,
    },

    /// Capture raw f32 audio from a compressor at full sample rate.
    ///
    /// Same pulse-tone signal as capture-compressor, but stores full-fidelity
    /// f32 audio output (not u8 gain reduction). Enables phase-cancellation
    /// null tests via compare-compressor-audio.
    CaptureCompressorAudio {
        /// Path to the .clap plugin file.
        plugin: String,
        /// Output directory for audio captures.
        #[arg(long)]
        out: PathBuf,
        /// JSON file defining test scenarios. If omitted, uses default scenario.
        #[arg(long)]
        scenarios: Option<PathBuf>,
        /// High gain level in dBFS.
        #[arg(long, default_value_t = -6.0, allow_hyphen_values = true)]
        gain_high: f32,
        /// Low gain level in dBFS.
        #[arg(long, default_value_t = -20.0, allow_hyphen_values = true)]
        gain_low: f32,
        /// Duration of high-gain phase in ms.
        #[arg(long, default_value_t = 240.0)]
        time_high: f32,
        /// Duration of low-gain phase in ms.
        #[arg(long, default_value_t = 240.0)]
        time_low: f32,
        /// Carrier waveform: sine, square, saw, noise.
        #[arg(long, default_value = "sine")]
        waveform: String,
        /// Test signal duration in seconds.
        #[arg(long, default_value_t = 3.0)]
        duration: f32,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Base parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
        /// Maximum number of scenarios to run.
        #[arg(long)]
        limit: Option<usize>,
        /// Number of worker threads.
        #[arg(long)]
        threads: Option<usize>,
    },

    /// Compare a compressor's audio output against a reference capture (phase-cancellation).
    ///
    /// Re-runs the pulse-tone test from a capture-compressor-audio reference and
    /// measures null depth (20*log10(rms(ref-test)/rms(ref))). -40 dB = good match.
    CompareCompressorAudio {
        /// Path to the .clap plugin file to test.
        plugin: String,
        /// Directory containing the f32 reference capture.
        #[arg(long)]
        reference: PathBuf,
        /// Output directory for comparison audio.
        #[arg(long)]
        out: PathBuf,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Null depth threshold in dB (fail if worse, i.e. less negative than this).
        #[arg(long, default_value_t = -40.0, allow_hyphen_values = true)]
        null_threshold_db: f32,
        /// Base parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
        /// Remap reference param IDs to test plugin param names.
        #[arg(long)]
        param_remap: Vec<String>,
        /// Filter scenarios by substring match.
        #[arg(long)]
        filter: Vec<String>,
        /// Maximum number of scenarios to run.
        #[arg(long)]
        limit: Option<usize>,
    },

    /// Resolve display text to normalized parameter values.
    ///
    /// Useful for finding a plugin's internal representation of human-readable
    /// values like "5 ms" or "4:1".
    ResolveParams {
        /// Path to the .clap plugin file.
        plugin: String,
        /// Parameter ID to resolve.
        #[arg(long)]
        param_id: u32,
        /// Display text values to resolve (e.g. "5 ms", "10 ms").
        #[arg(long)]
        text: Vec<String>,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
    },

    /// Fuzz-test two EQ plugins by randomizing shared parameters and comparing
    /// frequency response. Uses white noise input and FFT-based comparison.
    FuzzEq {
        /// Path to plugin A (reference, e.g. Pro-Q 4).
        plugin_a: String,
        /// Path to plugin B (test, e.g. FTS-EQ).
        plugin_b: String,
        /// Number of random iterations.
        #[arg(long, default_value_t = 100)]
        iterations: usize,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 256)]
        block_size: u32,
        /// Duration in seconds.
        #[arg(long, default_value_t = 2.0)]
        duration: f32,
        /// Maximum allowed RMS difference in dB (test fails above this).
        #[arg(long, default_value_t = -60.0, allow_hyphen_values = true)]
        tolerance_db: f32,
        /// Random seed.
        #[arg(long, default_value_t = 42)]
        seed: u64,
        /// Number of bands to randomize (1–24).
        #[arg(long, default_value_t = 3)]
        bands: usize,
    },

    /// Capture an EQ plugin's frequency response across all filter configurations.
    ///
    /// Tests every filter type × frequency × gain × Q × slope combination
    /// and saves the transfer function for each.
    CaptureEq {
        /// Path to the .clap plugin file (e.g. Pro-Q 4).
        plugin: String,
        /// Output directory for reference data.
        #[arg(long)]
        out: PathBuf,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Duration of white noise test signal in seconds.
        #[arg(long, default_value_t = 2.0)]
        duration: f32,
        /// Base parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
        /// Optional scenario name substring filter (e.g. "allpass_1000hz_q1_s2") for quick testing.
        #[arg(long)]
        filter: Option<String>,
        /// Skip scenarios whose .bin file already exists in --out (for resume after crash).
        #[arg(long)]
        skip_existing: bool,
    },

    /// Dump GR values from a binary capture file at specific frequencies/times.
    DumpCapture {
        /// Path to directory containing .bin capture files.
        #[arg(long)]
        dir: PathBuf,
        /// Frequency index to dump (e.g. 20 for 1kHz). If omitted, dumps all.
        #[arg(long)]
        freq_idx: Option<usize>,
        /// Only dump a specific scenario file name (without .bin).
        #[arg(long)]
        scenario: Option<String>,
        /// Time range start in ms (default: 0).
        #[arg(long, default_value_t = 0)]
        time_start: usize,
        /// Time range end in ms (default: full duration).
        #[arg(long)]
        time_end: Option<usize>,
        /// Step size in ms for output (default: 1).
        #[arg(long, default_value_t = 1)]
        step: usize,
    },

    /// Diagnose a single EQ scenario — prints detailed frequency-by-frequency comparison.
    DiagnoseEq {
        /// Path to the .clap plugin file to test.
        plugin: String,
        /// Directory containing the reference capture.
        #[arg(long)]
        reference: PathBuf,
        /// Scenario name (e.g. "high_cut_500hz_q1_s2").
        #[arg(long)]
        scenario: String,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Duration of white noise test signal in seconds.
        #[arg(long, default_value_t = 2.0)]
        duration: f32,
    },

    /// Compare a plugin's EQ response against saved reference data.
    CompareEq {
        /// Path to the .clap plugin file to test.
        plugin: String,
        /// Directory containing the reference capture.
        #[arg(long)]
        reference: PathBuf,
        /// Output directory for comparison results.
        #[arg(long)]
        out: PathBuf,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Duration of white noise test signal in seconds.
        #[arg(long, default_value_t = 2.0)]
        duration: f32,
        /// Maximum allowed RMS difference in dB.
        #[arg(long, default_value_t = 1.0)]
        tolerance_db: f32,
        /// Base parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
        /// Filter scenarios by substring match (e.g. "low_shelf", "bell_5000hz").
        /// Multiple filters can be specified; scenarios matching ANY filter are included.
        #[arg(long)]
        filter: Vec<String>,
        /// Write a detailed markdown report to this path.
        #[arg(long)]
        report: Option<PathBuf>,
    },

    /// Capture impulse responses for all EQ filter configurations.
    ///
    /// Sends a unit impulse through the plugin for each scenario and saves
    /// the raw impulse response. Enables sample-by-sample comparison and
    /// biquad coefficient extraction.
    CaptureEqImpulse {
        /// Path to the .clap plugin file (e.g. Pro-Q 4).
        plugin: String,
        /// Output directory for impulse response data.
        #[arg(long)]
        out: PathBuf,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Base parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
    },

    /// Compare impulse responses and extract biquad coefficients.
    ///
    /// For single-biquad scenarios (slope=0), extracts b0/b1/b2/a1/a2
    /// coefficients from both reference and test impulse responses.
    CompareEqImpulse {
        /// Path to the .clap plugin file to test.
        plugin: String,
        /// Directory containing the reference IR capture.
        #[arg(long)]
        reference: PathBuf,
        /// Output directory for comparison results.
        #[arg(long)]
        out: PathBuf,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Base parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
        /// Filter scenarios by substring match.
        #[arg(long)]
        filter: Vec<String>,
    },

    /// Test EQ linearity at multiple input levels.
    ///
    /// Runs the frequency response comparison at different input levels
    /// (default: -60, -20, 0 dBFS) to detect any nonlinear behavior
    /// like internal saturation or noise floor issues.
    LinearityEq {
        /// Path to the .clap plugin file to test.
        plugin: String,
        /// Directory containing the reference capture.
        #[arg(long)]
        reference: PathBuf,
        /// Output directory for linearity results.
        #[arg(long)]
        out: PathBuf,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Duration of white noise test signal in seconds.
        #[arg(long, default_value_t = 2.0)]
        duration: f32,
        /// Input levels to test, in dBFS (comma-separated).
        #[arg(
            long,
            default_value = "-60,-20,0",
            value_delimiter = ',',
            allow_hyphen_values = true
        )]
        levels: Vec<f32>,
        /// Base parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
        /// Filter scenarios by substring match.
        #[arg(long)]
        filter: Vec<String>,
    },

    /// Sweep a plugin parameter across a range and measure EQ accuracy.
    ///
    /// For each value in the sweep range, runs the filtered scenarios and
    /// reports the total pass count and average error. Useful for tuning
    /// internal constants exposed as hidden plugin parameters.
    SweepEq {
        /// Path to the .clap plugin file to test.
        plugin: String,
        /// Directory containing the reference capture.
        #[arg(long)]
        reference: PathBuf,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Duration of white noise test signal in seconds.
        #[arg(long, default_value_t = 0.5)]
        duration: f32,
        /// Maximum allowed RMS difference in dB.
        #[arg(long, default_value_t = 1.0)]
        tolerance_db: f32,
        /// Name of the plugin parameter to sweep (matched by substring).
        #[arg(long)]
        sweep_param: String,
        /// Minimum value for sweep.
        #[arg(long)]
        min: f64,
        /// Maximum value for sweep.
        #[arg(long)]
        max: f64,
        /// Number of steps in the sweep.
        #[arg(long, default_value_t = 50)]
        steps: usize,
        /// Base parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
        /// Filter scenarios by substring match.
        #[arg(long)]
        filter: Vec<String>,
    },

    /// Compare EQ with configurable FFT size for higher frequency resolution.
    CompareEqHires {
        /// Path to the .clap plugin file to test.
        plugin: String,
        /// Directory containing the reference capture.
        #[arg(long)]
        reference: PathBuf,
        /// Output directory for comparison results.
        #[arg(long)]
        out: PathBuf,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Duration of white noise test signal in seconds.
        #[arg(long, default_value_t = 4.0)]
        duration: f32,
        /// FFT size (higher = finer frequency resolution). Must be power of 2.
        #[arg(long, default_value_t = 16384)]
        fft_size: usize,
        /// Maximum allowed RMS difference in dB.
        #[arg(long, default_value_t = 1.0)]
        tolerance_db: f32,
        /// Base parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
        /// Filter scenarios by substring match.
        #[arg(long)]
        filter: Vec<String>,
    },

    /// Dump VST2 plugin state chunk for reverse-engineering hidden parameters.
    ///
    /// Loads the plugin, optionally sets parameters, then dumps the preset
    /// state chunk as hex + ASCII. Use --sweep to sweep a hidden parameter
    /// index and measure output pitch to decode what it does.
    DumpVst2State {
        /// Path to the VST2 plugin (.so/.dll).
        plugin: String,
        /// Parameter overrides as "index=value" pairs (normalized 0..1).
        #[arg(long)]
        param: Vec<String>,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 2048)]
        block_size: u32,
        /// Sweep a hidden state-chunk parameter index (0-based position in CSV).
        /// Processes audio at each value and prints output frequency.
        #[arg(long)]
        sweep: Option<usize>,
    },

    /// Capture a pitch shifter's output on GuitarSet clips for comparison.
    ///
    /// Processes clips through a reference plugin (e.g. Kuassa Efektor
    /// Harmonitron) and saves the output. Reloads the plugin between clips
    /// to reset any trial noise timer.
    CapturePitch {
        /// Path to the reference plugin (.so for VST2, .clap, .vst3).
        plugin: String,
        /// Output directory for reference data.
        #[arg(long)]
        out: PathBuf,
        /// Sample rate.
        #[arg(long, default_value_t = 44100.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Maximum clip duration in seconds (avoids trial noise).
        #[arg(long, default_value_t = 30.0)]
        max_clip_duration: f32,
        /// Path to GuitarSet dataset.
        #[arg(
            long,
            default_value = "/home/cody/Development/mir-datasets/data/guitarset"
        )]
        guitarset_dir: PathBuf,
        /// Maximum number of clips to process.
        #[arg(long)]
        limit: Option<usize>,
        /// Use all clips instead of the curated 20-clip subset.
        #[arg(long)]
        all_clips: bool,
    },

    /// Compare a pitch shifter against captured reference data.
    ///
    /// Loads reference captures and processes the same clips through the
    /// test plugin, then compares RMS levels and spectral content.
    ComparePitch {
        /// Path to the test plugin (.clap).
        plugin: String,
        /// Directory containing the reference capture.
        #[arg(long)]
        reference: PathBuf,
        /// Output directory for comparison results.
        #[arg(long)]
        out: PathBuf,
        /// Sample rate.
        #[arg(long, default_value_t = 44100.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Maximum clip duration in seconds.
        #[arg(long, default_value_t = 30.0)]
        max_clip_duration: f32,
        /// Path to GuitarSet dataset.
        #[arg(
            long,
            default_value = "/home/cody/Development/mir-datasets/data/guitarset"
        )]
        guitarset_dir: PathBuf,
        /// Maximum allowed RMS level difference in dB.
        #[arg(long, default_value_t = 1.0)]
        tolerance_rms_db: f32,
        /// Maximum allowed spectral difference in dB.
        #[arg(long, default_value_t = 3.0)]
        tolerance_spectral_db: f32,
    },

    /// Extract impulse response and biquad coefficients from a plugin.
    ///
    /// Loads a CLAP plugin, sets parameters, feeds a unit impulse (1.0 followed
    /// by zeros), and captures the output. Extracts biquad [b0, b1, b2, a1, a2]
    /// coefficients from the first 5 samples of the impulse response.
    ExtractIr {
        /// Path to the .clap plugin file.
        path: String,
        /// Plugin index within the bundle (default: 0).
        #[arg(short, long, default_value_t = 0)]
        index: usize,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Number of impulse response samples to capture.
        #[arg(long, default_value_t = 20)]
        length: usize,
        /// Parameter overrides as "id=value" pairs.
        #[arg(long)]
        param: Vec<String>,
    },

    /// Benchmark plugin CPU usage across multiple block sizes.
    ///
    /// Processes white noise through the plugin at each configured block size
    /// and reports mean, median, p99, max processing time, and realtime ratio.
    CpuBench {
        /// Path to the plugin file (.clap, .so, .vst3).
        plugin: String,
        /// Plugin index within bundle (default: 0).
        #[arg(short, long, default_value_t = 0)]
        index: usize,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Total signal duration to process in seconds.
        #[arg(long, default_value_t = 5.0)]
        duration: f32,
        /// Block sizes to benchmark (comma-separated).
        #[arg(
            long,
            default_value = "32,64,128,256,512,1024,2048,4096",
            value_delimiter = ','
        )]
        block_sizes: Vec<u32>,
        /// Output CSV path.
        #[arg(long, default_value = "cpu_bench.csv")]
        out: PathBuf,
    },

    /// Hammerstein system identification via Synchronized Swept Sine (SSS).
    ///
    /// Sends an exponential sweep through the plugin and deconvolves the output
    /// to extract per-harmonic frequency responses and Hammerstein kernels.
    Hammerstein {
        /// Path to the plugin file (.clap, .so, .vst3).
        plugin: String,
        /// Plugin index within bundle (default: 0).
        #[arg(short, long, default_value_t = 0)]
        index: usize,
        /// Output directory for CSV results.
        #[arg(long)]
        out: PathBuf,
        /// Start frequency of the sweep in Hz.
        #[arg(long, default_value_t = 20.0)]
        f1: f32,
        /// End frequency of the sweep in Hz.
        #[arg(long, default_value_t = 20000.0)]
        f2: f32,
        /// Sweep duration in seconds.
        #[arg(long, default_value_t = 10.0)]
        duration: f32,
        /// Maximum harmonic order to extract.
        #[arg(long, default_value_t = 7)]
        max_order: usize,
        /// Sample rate.
        #[arg(long, default_value_t = 48000.0)]
        sample_rate: f64,
        /// Block size.
        #[arg(long, default_value_t = 512)]
        block_size: u32,
        /// Length of each extracted harmonic impulse response in samples.
        #[arg(long, default_value_t = 4096)]
        ir_length: usize,
        /// Fade-in and fade-out duration in seconds.
        #[arg(long, default_value_t = 0.05)]
        fade: f32,
    },
}

fn parse_params(params: &[String]) -> Result<Vec<(u32, f64)>> {
    params
        .iter()
        .map(|s| {
            let (id, val) = s.split_once('=').context("param must be id=value")?;
            Ok((id.parse::<u32>()?, val.parse::<f64>()?))
        })
        .collect()
}

fn generate_signal(kind: &str, sample_rate: f64, duration: f32, level_db: f32) -> Vec<f32> {
    let sr = sample_rate as f32;
    let len = (sr * duration) as usize;
    match kind {
        "sine" => signal::sine_db(1000.0, sr, len, level_db),
        "noise" => {
            let gain = 10.0f32.powf(level_db / 20.0);
            signal::white_noise(len, 42)
                .into_iter()
                .map(|s| s * gain)
                .collect()
        }
        "step" => {
            let silent = (sr * 0.25) as usize; // 250ms silence then signal
            signal::step(silent, len, level_db)
        }
        "sweep" => {
            let gain = 10.0f32.powf(level_db / 20.0);
            signal::sweep(20.0, 20000.0, sr, len)
                .into_iter()
                .map(|s| s * gain)
                .collect()
        }
        other => {
            eprintln!("unknown signal type '{}', using sine", other);
            signal::sine_db(1000.0, sr, len, level_db)
        }
    }
}

fn write_wav(path: &str, data: &[f32], sample_rate: u32) -> Result<()> {
    let spec = hound::WavSpec {
        channels: 1,
        sample_rate,
        bits_per_sample: 32,
        sample_format: hound::SampleFormat::Float,
    };
    let mut writer = hound::WavWriter::create(path, spec)?;
    for &sample in data {
        writer.write_sample(sample)?;
    }
    writer.finalize()?;
    Ok(())
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::List { path } => {
            let plugins = LoadedPlugin::list_plugins(path.as_ref())?;
            for (i, name) in plugins.iter().enumerate() {
                println!("[{}] {}", i, name);
            }
        }

        Command::Params { path, index } => {
            let mut plugin = LoadedPlugin::load(path.as_ref(), index, 48000.0, 512)?;
            let params = plugin.params();
            println!(
                "{:<6} {:<40} {:>10} {:>10} {:>10}",
                "ID", "Name", "Min", "Max", "Default"
            );
            println!("{}", "-".repeat(80));
            for p in &params {
                println!(
                    "{:<6} {:<40} {:>10.2} {:>10.2} {:>10.2}",
                    p.id, p.name, p.min, p.max, p.default
                );
            }
            println!("\n{} parameters total", params.len());
            println!("Reported latency: {} samples", plugin.latency());
        }

        Command::DumpParam {
            path,
            name,
            min,
            max,
            step,
            index,
        } => {
            let mut plugin = LoadedPlugin::load(path.as_ref(), index, 48000.0, 512)?;
            let params = plugin.params();
            let matched: Vec<_> = params
                .iter()
                .filter(|p| p.name.to_lowercase().contains(&name.to_lowercase()))
                .collect();
            if matched.is_empty() {
                anyhow::bail!("No parameter matching '{}'", name);
            }
            for p in &matched {
                println!(
                    "Parameter: {} (id={}, min={}, max={}, default={})",
                    p.name, p.id, p.min, p.max, p.default
                );
                println!("{:>10} -> Text", "Value");
                println!("{}", "-".repeat(40));
                let mut v = min;
                while v <= max {
                    // value_to_text not available on LoadedPlugin
                    let text = format!("{:.4}", v);
                    println!("{:>10.4} -> {}", v, text);
                    v += step;
                }
                println!();
            }
        }

        Command::Process {
            path,
            output,
            index,
            sample_rate,
            block_size,
            signal: sig_type,
            level_db,
            duration,
            param,
            state_chunk,
        } => {
            let overrides = parse_params(&param)?;
            let input = generate_signal(&sig_type, sample_rate, duration, level_db);
            let mut plugin = LoadedPlugin::load(path.as_ref(), index, sample_rate, block_size)?;

            // Load raw state chunk if provided (VST2 only — not available without host_vst2)
            if let Some(ref _csv) = state_chunk {
                anyhow::bail!(
                    "--state-chunk requires VST2 support which is not currently available"
                );
            }

            println!("Processing {} samples through plugin...", input.len());
            let result = plugin.process(&input, &overrides)?;

            let in_peak_freq = find_peak_frequency(&input, sample_rate as f32);
            let out_peak_freq = find_peak_frequency(&result, sample_rate as f32);

            println!(
                "Input:  peak={:.1} dB  rms={:.1} dB  freq={:.1} Hz",
                analysis::peak_db(&input),
                analysis::rms_db(&input),
                in_peak_freq
            );
            println!(
                "Output: peak={:.1} dB  rms={:.1} dB  freq={:.1} Hz",
                analysis::peak_db(&result),
                analysis::rms_db(&result),
                out_peak_freq
            );
            println!(
                "Gain reduction: {:.1} dB",
                analysis::gain_reduction_db(&input, &result)
            );

            write_wav(&output, &result, sample_rate as u32)?;
            println!("Wrote {}", output);
        }

        Command::MeasureGrid {
            config,
            out,
            plugin,
            seconds,
            samplerate,
            blocksize,
        } => {
            let mut cfg = MeasureConfig::from_file(&config)
                .with_context(|| format!("failed to read config: {}", config.display()))?;

            // Apply CLI overrides
            if let Some(p) = plugin {
                cfg.plugin_path = p;
            }
            if let Some(s) = seconds {
                cfg.seconds = s;
            }
            if let Some(sr) = samplerate {
                cfg.sample_rate = sr;
            }
            if let Some(bs) = blocksize {
                cfg.block_size = bs;
            }

            let mut engine = MeasurementEngine::new(cfg);
            engine.run(&out)?;
        }

        Command::Compare {
            plugin_a,
            plugin_b,
            sample_rate,
            block_size,
            signal: sig_type,
            level_db,
            duration,
            param_a,
            param_b,
        } => {
            let overrides_a = parse_params(&param_a)?;
            let overrides_b = parse_params(&param_b)?;
            let input = generate_signal(&sig_type, sample_rate, duration, level_db);

            println!("Loading plugin A: {}", plugin_a);
            let mut a = LoadedPlugin::load(plugin_a.as_ref(), 0, sample_rate, block_size)?;

            println!("Loading plugin B: {}", plugin_b);
            let mut b = LoadedPlugin::load(plugin_b.as_ref(), 0, sample_rate, block_size)?;

            println!("Processing {} samples...", input.len());
            let out_a = a.process(&input, &overrides_a)?;
            let out_b = b.process(&input, &overrides_b)?;

            let cmp = analysis::compare(&input, &out_a, &out_b);

            println!("\n--- Comparison ---");
            println!(
                "Plugin A output:  rms={:.1} dB  GR={:.1} dB",
                cmp.a_rms_db, cmp.a_gain_reduction_db
            );
            println!(
                "Plugin B output:  rms={:.1} dB  GR={:.1} dB",
                cmp.b_rms_db, cmp.b_gain_reduction_db
            );
            println!(
                "Difference:       rms={:.1} dB  peak={:.1} dB",
                cmp.diff_rms_db, cmp.diff_peak_db
            );
        }

        Command::RunAnalysis { config, out } => {
            run_analysis(&config, &out)?;
        }

        Command::CaptureCompressor {
            profile: profile_path,
            plugin,
            out,
            scenarios: scenarios_path,
            gain_high,
            gain_low,
            time_high,
            time_low,
            waveform,
            duration,
            sample_rate,
            block_size,
            num_freqs: _,
            freq_start: _,
            freq_end: _,
            param,
            limit,
            threads,
        } => {
            // Profile support disabled (profiles module not available)
            if profile_path.is_some() {
                anyhow::bail!("--profile is not available (profiles module removed)");
            }

            let plugin_str = plugin.context("--plugin is required")?;
            let out_dir = out.context("--out is required")?;

            let merged_params = parse_params(&param)?;

            let wf = match waveform.as_str() {
                "square" => signal::Waveform::Square,
                "saw" => signal::Waveform::Saw,
                // "noise" waveform not available as Waveform variant; fall through to Sine
                "noise" => signal::Waveform::Sine,
                _ => signal::Waveform::Sine,
            };

            let frequencies = compressor::TEST_FREQUENCIES.to_vec();

            let mut scenarios = if let Some(path) = scenarios_path {
                compressor::parse_scenarios(&path)?
            } else {
                vec![compressor::Scenario {
                    name: "default".to_string(),
                    params: vec![],
                }]
            };
            if let Some(max) = limit {
                scenarios.truncate(max);
            }

            compressor::run_capture(
                &plugin_str,
                &out_dir,
                sample_rate,
                block_size,
                gain_high,
                gain_low,
                time_high,
                time_low,
                wf,
                duration,
                &frequencies,
                &merged_params,
                &scenarios,
                threads,
            )?;
        }

        Command::CompareCompressor {
            plugin,
            reference,
            out,
            attack_ms,
            release_ms,
            sample_rate,
            block_size,
            tolerance_db,
            param,
            param_remap,
            freq_filter,
        } => {
            let base_params = parse_params(&param)?;
            let remaps = compressor::parse_param_remaps(&param_remap)?;

            // Create a single scenario filter for this attack/release pair
            let scenario_filter = vec![format!(
                "atk-{}ms_rel-{}ms",
                if attack_ms.fract() == 0.0 {
                    format!("{:.0}", attack_ms)
                } else {
                    format!("{}", attack_ms)
                },
                if release_ms.fract() == 0.0 {
                    format!("{:.0}", release_ms)
                } else {
                    format!("{}", release_ms)
                }
            )];

            compressor::run_compare(
                &plugin,
                &reference,
                &out,
                sample_rate,
                block_size,
                &base_params,
                tolerance_db,
                &remaps,
                &scenario_filter,
                Some(1),
                &freq_filter,
            )?;
        }

        Command::CompareCompressorLive {
            reference,
            plugin,
            out,
            attack_ms,
            release_ms,
            threshold_db,
            ratio,
            knee_db,
            tolerance_db,
            param_remap,
            freq_filter,
            sample_rate,
            block_size,
        } => {
            let remaps = compressor::parse_param_remaps(&param_remap)?;

            compressor_live::run_compare_live(
                &reference,
                &plugin,
                &out,
                sample_rate,
                block_size,
                attack_ms,
                release_ms,
                threshold_db,
                ratio,
                knee_db,
                tolerance_db,
                &remaps,
                &freq_filter,
            )?;
        }

        Command::CompareCompressorAll {
            profile: profile_path,
            plugin,
            reference,
            out,
            sample_rate,
            block_size,
            tolerance_db,
            param,
            param_remap,
            filter,
            limit,
            freq_filter,
        } => {
            // Profile support disabled (profiles module not available)
            if profile_path.is_some() {
                anyhow::bail!("--profile is not available (profiles module removed)");
            }

            let plugin_str = plugin.context("--plugin is required")?;
            let reference_dir = reference.context("--reference is required")?;
            let out_dir = out.context("--out is required")?;

            let merged_params = parse_params(&param)?;
            let remaps = compressor::parse_param_remaps(&param_remap)?;

            compressor::run_compare(
                &plugin_str,
                &reference_dir,
                &out_dir,
                sample_rate,
                block_size,
                &merged_params,
                tolerance_db,
                &remaps,
                &filter,
                limit,
                &freq_filter,
            )?;
        }

        Command::CaptureCompressorAudio {
            plugin,
            out,
            scenarios: scenarios_path,
            gain_high,
            gain_low,
            time_high,
            time_low,
            waveform,
            duration,
            sample_rate,
            block_size,
            param,
            limit,
            threads,
        } => {
            let base_params = parse_params(&param)?;
            let wf = match waveform.as_str() {
                "square" => signal::Waveform::Square,
                "saw" => signal::Waveform::Saw,
                // "noise" waveform not available as Waveform variant; fall through to Sine
                "noise" => signal::Waveform::Sine,
                _ => signal::Waveform::Sine,
            };
            let frequencies = compressor::TEST_FREQUENCIES.to_vec();
            let mut scenarios = if let Some(path) = scenarios_path {
                compressor::parse_scenarios(&path)?
            } else {
                vec![compressor::Scenario {
                    name: "default".to_string(),
                    params: vec![],
                }]
            };
            if let Some(max) = limit {
                scenarios.truncate(max);
            }
            compressor_audio::run_capture_audio(
                &plugin,
                &out,
                sample_rate,
                block_size,
                gain_high,
                gain_low,
                time_high,
                time_low,
                wf,
                duration,
                &frequencies,
                &base_params,
                &scenarios,
                threads,
            )?;
        }

        Command::CompareCompressorAudio {
            plugin,
            reference,
            out,
            sample_rate,
            block_size,
            null_threshold_db,
            param,
            param_remap,
            filter,
            limit,
        } => {
            let base_params = parse_params(&param)?;
            let remaps = compressor::parse_param_remaps(&param_remap)?;
            compressor_audio::run_compare_audio(
                &plugin,
                &reference,
                &out,
                sample_rate,
                block_size,
                &base_params,
                null_threshold_db,
                &remaps,
                &filter,
                limit,
            )?;
        }

        Command::ResolveParams {
            plugin,
            param_id,
            text,
            sample_rate,
            block_size,
        } => {
            resolve_params::run_resolve(&plugin, param_id, &text, sample_rate, block_size)?;
        }

        Command::FuzzEq {
            plugin_a,
            plugin_b,
            iterations,
            sample_rate,
            block_size,
            duration,
            tolerance_db,
            seed,
            bands,
        } => {
            fuzz_eq::run_fuzz_eq(
                &plugin_a,
                &plugin_b,
                iterations,
                sample_rate,
                block_size,
                duration,
                tolerance_db,
                seed,
                bands,
            )?;
        }

        Command::CaptureEq {
            plugin,
            out,
            sample_rate: _,
            block_size,
            duration,
            param,
            filter,
            skip_existing,
        } => {
            let base_params = parse_params(&param)?;
            let mut scenarios = eq::build_eq_scenarios();
            if let Some(pat) = &filter {
                scenarios.retain(|s| s.name.contains(pat.as_str()));
                eprintln!("Filter '{}' matched {} scenarios", pat, scenarios.len());
            }
            if skip_existing {
                let before = scenarios.len();
                scenarios.retain(|s| {
                    for sr in ["48k", "96k"] {
                        let path = out.join(sr).join(format!("{}.bin", s.name));
                        if !path.exists() {
                            return true;
                        }
                    }
                    false
                });
                eprintln!("Skip-existing: {} → {} scenarios", before, scenarios.len());
            }
            eq::run_capture_eq(
                &plugin,
                &out,
                block_size,
                duration,
                &base_params,
                &scenarios,
            )?;
        }

        Command::DumpCapture {
            dir,
            freq_idx,
            scenario,
            time_start,
            time_end,
            step,
        } => {
            compressor::dump_capture(
                &dir,
                freq_idx,
                scenario.as_deref(),
                time_start,
                time_end,
                step,
            )?;
        }

        Command::DiagnoseEq {
            plugin,
            reference,
            scenario,
            sample_rate,
            block_size,
            duration,
        } => {
            eq::run_diagnose_eq(
                &plugin,
                &reference,
                &scenario,
                sample_rate,
                block_size,
                duration,
            )?;
        }

        Command::CompareEq {
            plugin,
            reference,
            out,
            sample_rate,
            block_size,
            duration,
            tolerance_db,
            param,
            filter,
            report,
        } => {
            let base_params = parse_params(&param)?;
            let filters: Vec<&str> = filter.iter().map(|s| s.as_str()).collect();
            eq::run_compare_eq(
                &plugin,
                &reference,
                &out,
                sample_rate,
                block_size,
                duration,
                &base_params,
                tolerance_db,
                if filters.is_empty() {
                    None
                } else {
                    Some(&filters)
                },
                report.as_deref(),
            )?;
        }

        Command::SweepEq {
            plugin: _,
            reference: _,
            sample_rate: _,
            block_size: _,
            duration: _,
            tolerance_db: _,
            sweep_param: _,
            min: _,
            max: _,
            steps: _,
            param: _,
            filter: _,
        } => {
            // eq::run_sweep_eq is not currently available
            anyhow::bail!("sweep-eq command is not currently implemented");
        }

        Command::CaptureEqImpulse {
            plugin,
            out,
            block_size,
            param,
        } => {
            let base_params = parse_params(&param)?;
            let scenarios = eq::build_eq_scenarios();
            eq::run_capture_eq_impulse(&plugin, &out, block_size, &base_params, &scenarios)?;
        }

        Command::CompareEqImpulse {
            plugin,
            reference,
            out,
            sample_rate,
            block_size,
            param,
            filter,
        } => {
            let base_params = parse_params(&param)?;
            let filters: Vec<&str> = filter.iter().map(|s| s.as_str()).collect();
            eq::run_compare_eq_impulse(
                &plugin,
                &reference,
                &out,
                sample_rate,
                block_size,
                &base_params,
                if filters.is_empty() {
                    None
                } else {
                    Some(&filters)
                },
            )?;
        }

        Command::LinearityEq {
            plugin,
            reference,
            out,
            sample_rate,
            block_size,
            duration,
            levels,
            param,
            filter,
        } => {
            let base_params = parse_params(&param)?;
            let filters: Vec<&str> = filter.iter().map(|s| s.as_str()).collect();
            eq::run_compare_eq_multilevel(
                &plugin,
                &reference,
                &out,
                sample_rate,
                block_size,
                duration,
                &base_params,
                &levels,
                if filters.is_empty() {
                    None
                } else {
                    Some(&filters)
                },
            )?;
        }

        Command::CompareEqHires {
            plugin,
            reference,
            out,
            sample_rate,
            block_size,
            duration,
            fft_size,
            tolerance_db,
            param,
            filter,
        } => {
            // Validate FFT size is power of 2
            if fft_size == 0 || (fft_size & (fft_size - 1)) != 0 {
                anyhow::bail!("--fft-size must be a power of 2 (got {})", fft_size);
            }
            let base_params = parse_params(&param)?;
            let filters: Vec<&str> = filter.iter().map(|s| s.as_str()).collect();
            eq::run_compare_eq_hires(
                &plugin,
                &reference,
                &out,
                sample_rate,
                block_size,
                duration,
                fft_size,
                &base_params,
                tolerance_db,
                if filters.is_empty() {
                    None
                } else {
                    Some(&filters)
                },
            )?;
        }

        Command::CapturePitch {
            plugin,
            out,
            sample_rate,
            block_size,
            max_clip_duration,
            guitarset_dir,
            limit,
            all_clips,
        } => {
            let scenarios = pitch::build_scenarios();
            pitch::run_capture(
                &plugin,
                &out,
                sample_rate,
                block_size,
                &guitarset_dir,
                max_clip_duration,
                &scenarios,
                limit,
                !all_clips,
            )?;
        }

        Command::ComparePitch {
            plugin,
            reference,
            out,
            sample_rate,
            block_size,
            max_clip_duration,
            guitarset_dir,
            tolerance_rms_db,
            tolerance_spectral_db,
        } => {
            let scenarios = pitch::build_scenarios();
            pitch::run_compare(
                &plugin,
                &reference,
                &out,
                sample_rate,
                block_size,
                &guitarset_dir,
                max_clip_duration,
                tolerance_rms_db,
                tolerance_spectral_db,
                &scenarios,
            )?;
        }

        Command::ExtractIr {
            path,
            index,
            sample_rate,
            block_size,
            length,
            param,
        } => {
            let overrides = parse_params(&param)?;

            println!("Loading plugin: {}", path);
            let mut plugin = fts_analyzer::host::LoadedPlugin::load(
                path.as_ref(),
                index,
                sample_rate,
                block_size,
            )?;

            // Show parameter info
            let params = plugin.params();
            println!("Plugin has {} parameters", params.len());
            for &(id, val) in &overrides {
                if let Some(p) = params.iter().find(|p| p.id == id) {
                    println!("  Setting param {} ({}) = {}", id, p.name, val);
                } else {
                    println!("  Setting param {} = {} (unknown name)", id, val);
                }
            }

            // Check latency — we need to skip latency samples at the start
            let latency = plugin.latency() as usize;
            if latency > 0 {
                println!(
                    "Plugin reports {} samples latency -- will compensate",
                    latency
                );
            }

            // Build impulse signal: 1.0 followed by zeros
            // Use enough samples to fill at least one block plus the desired IR length,
            // plus latency compensation.
            let total_samples =
                (block_size as usize * 2).max(length + block_size as usize) + latency;
            let mut impulse = vec![0.0f32; total_samples];
            impulse[0] = 1.0;

            println!(
                "\nProcessing impulse ({} samples, sr={}, block={})...",
                total_samples, sample_rate, block_size
            );
            let output = plugin.process(&impulse, &overrides)?;

            // Skip latency samples
            let output = &output[latency..];

            // Print impulse response
            let ir_len = length.min(output.len());
            println!("\n--- Impulse Response (first {} samples) ---", ir_len);
            for (i, &s) in output[..ir_len].iter().enumerate() {
                println!("  h[{:3}] = {:+.10e}", i, s);
            }

            // Extract biquad coefficients from the impulse response.
            //
            // A biquad filter has transfer function:
            //   H(z) = (b0 + b1*z^-1 + b2*z^-2) / (1 + a1*z^-1 + a2*z^-2)
            //
            // Its impulse response satisfies the difference equation:
            //   h[n] = b_n - a1*h[n-1] - a2*h[n-2]   (where b_n = 0 for n > 2)
            //
            // So:
            //   h[0] = b0
            //   h[1] = b1 - a1*h[0]
            //   h[2] = b2 - a1*h[1] - a2*h[0]
            //   h[3] =    - a1*h[2] - a2*h[1]
            //   h[4] =    - a1*h[3] - a2*h[2]
            //
            // From h[3] and h[4] we can solve for a1 and a2:
            //   | h[2]  h[1] | | a1 |   | -h[3] |
            //   | h[3]  h[2] | | a2 | = | -h[4] |
            //
            if ir_len >= 5 {
                let h = &output[..5];
                let det = h[2] * h[2] - h[3] * h[1];
                if det.abs() > 1e-20 {
                    let a1 = (-h[3] * h[2] - (-h[4]) * h[1]) / det;
                    let a2 = (h[2] * (-h[4]) - h[3] * (-h[3])) / det;
                    let b0 = h[0];
                    let b1 = h[1] + a1 * h[0];
                    let b2 = h[2] + a1 * h[1] + a2 * h[0];

                    println!("\n--- Extracted Biquad Coefficients ---");
                    println!("  b0 = {:+.15e}", b0);
                    println!("  b1 = {:+.15e}", b1);
                    println!("  b2 = {:+.15e}", b2);
                    println!("  a1 = {:+.15e}", a1);
                    println!("  a2 = {:+.15e}", a2);

                    // Verify: reconstruct h[3] and h[4] from coefficients
                    let h3_check = -a1 * h[2] - a2 * h[1];
                    let h4_check = -a1 * h[3] - a2 * h[2];
                    println!("\n--- Verification ---");
                    println!(
                        "  h[3] actual={:+.10e}  reconstructed={:+.10e}  err={:.2e}",
                        h[3],
                        h3_check,
                        (h[3] - h3_check).abs()
                    );
                    println!(
                        "  h[4] actual={:+.10e}  reconstructed={:+.10e}  err={:.2e}",
                        h[4],
                        h4_check,
                        (h[4] - h4_check).abs()
                    );

                    // Also check h[5..] if available to validate it's truly a single biquad
                    if output.len() > 5 {
                        let mut max_err = 0.0f32;
                        for n in 5..ir_len.min(output.len()) {
                            let predicted = -a1 * output[n - 1] - a2 * output[n - 2];
                            let err = (output[n] - predicted).abs();
                            if err > max_err {
                                max_err = err;
                            }
                        }
                        println!(
                            "  max reconstruction error h[5..{}]: {:.2e}",
                            ir_len, max_err
                        );
                        if max_err > 1e-4 {
                            println!(
                                "  WARNING: large reconstruction error — this may not be a single biquad"
                            );
                        }
                    }
                } else {
                    println!(
                        "\nCould not extract biquad coefficients (singular matrix, det={:.2e})",
                        det
                    );
                    println!("The filter may be flat/bypassed or higher-order.");
                }
            } else {
                println!(
                    "\nNeed at least 5 IR samples for biquad extraction (got {})",
                    ir_len
                );
            }
        }

        Command::CpuBench {
            plugin: _,
            index: _,
            sample_rate: _,
            duration: _,
            block_sizes: _,
            out: _,
        } => {
            // cpu_bench module is not currently available
            anyhow::bail!(
                "cpu-bench command is not currently implemented (cpu_bench module missing)"
            );
        }

        Command::Hammerstein {
            plugin: _,
            index: _,
            out: _,
            f1: _,
            f2: _,
            duration: _,
            max_order: _,
            sample_rate: _,
            block_size: _,
            ir_length: _,
            fade: _,
        } => {
            // hammerstein module is not currently available
            anyhow::bail!("hammerstein command is not currently implemented (module missing)");
        }

        Command::DumpVst2State {
            plugin: _,
            param: _,
            sample_rate: _,
            block_size: _,
            sweep: _,
        } => {
            // host_vst2 module is not currently available
            anyhow::bail!(
                "dump-vst2-state command is not currently implemented (host_vst2 module missing)"
            );
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// run-analysis implementation
// ---------------------------------------------------------------------------

fn run_analysis(config_path: &PathBuf, base_out: &PathBuf) -> Result<()> {
    let analysis = AnalysisConfig::from_file(config_path)
        .with_context(|| format!("failed to read analysis config: {}", config_path.display()))?;

    eprintln!("=== {} ===", analysis.name);
    eprintln!(
        "Signal: {:?} | SR: {} Hz | Duration: {}s | Block: {}",
        analysis.measurement.signal_type,
        analysis.measurement.sample_rate,
        analysis.measurement.seconds,
        analysis.measurement.block_size,
    );
    eprintln!("Analyzers: {:?}", analysis.measurement.analyzers,);
    eprintln!(
        "Gain levels: {:?}",
        if analysis.measurement.input_gain_buckets_db.is_empty() {
            vec![0.0]
        } else {
            analysis.measurement.input_gain_buckets_db.clone()
        },
    );
    eprintln!("Plugins: {}", analysis.plugins.len());
    eprintln!();

    // Sanitize plugin name for directory use
    fn dir_name(name: &str) -> String {
        name.chars()
            .map(|c| {
                if c.is_alphanumeric() || c == '-' || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect()
    }

    // Run each plugin
    let mut plugin_dirs: Vec<(String, PathBuf)> = Vec::new();

    for (i, plugin) in analysis.plugins.iter().enumerate() {
        let plugin_out = base_out.join(dir_name(&plugin.name));
        eprintln!(
            "─── [{}/{}] {} ───",
            i + 1,
            analysis.plugins.len(),
            plugin.name,
        );
        eprintln!("  Plugin: {}", plugin.path.display());
        eprintln!("  Params: {} bucket(s)", plugin.parameter_buckets.len());
        for pb in &plugin.parameter_buckets {
            use fts_analyzer::config::BucketStrategy;
            if pb.strategy == BucketStrategy::DisplayText {
                eprintln!(
                    "    {} (DisplayText): {} values {:?}",
                    pb.param_name,
                    pb.text_values.len(),
                    pb.text_values
                );
            } else {
                let vals = pb.generate_values();
                eprintln!(
                    "    {} ({:?}): {} values",
                    pb.param_name,
                    pb.strategy,
                    vals.len()
                );
            }
        }
        eprintln!("  Output: {}", plugin_out.display());

        let cfg = analysis.to_measure_config(plugin);
        let mut engine = MeasurementEngine::new(cfg);
        engine
            .run(&plugin_out)
            .with_context(|| format!("measurement failed for '{}'", plugin.name))?;

        plugin_dirs.push((plugin.name.clone(), plugin_out));
        eprintln!();
    }

    // Print summary comparison if we have RmsPeak results from multiple plugins
    if plugin_dirs.len() >= 2 {
        eprintln!("=== Summary ===");
        print_rms_comparison(&plugin_dirs)?;
    }

    eprintln!("\nAll results written to {}", base_out.display());
    Ok(())
}

/// Read grid_rms_peak.csv from each plugin's output dir and print a comparison table.
fn print_rms_comparison(plugin_dirs: &[(String, PathBuf)]) -> Result<()> {
    struct PluginSummary {
        name: String,
        runs: usize,
        avg_output_rms_db: f64,
        avg_gain_reduction_db: f64,
        avg_thd_percent: Option<f64>,
    }

    let mut summaries: Vec<PluginSummary> = Vec::new();

    for (name, dir) in plugin_dirs {
        let rms_path = dir.join("grid_rms_peak.csv");
        if !rms_path.exists() {
            continue;
        }

        let mut rdr = csv::Reader::from_path(&rms_path)?;
        let headers = rdr.headers()?.clone();

        // Find column indices
        let out_rms_col = headers.iter().position(|h| h == "output_rms_db");
        let gr_col = headers.iter().position(|h| h == "gain_reduction_db");

        let (mut sum_rms, mut sum_gr, mut count) = (0.0f64, 0.0f64, 0usize);

        for record in rdr.records() {
            let record = record?;
            if let Some(col) = out_rms_col
                && let Ok(v) = record[col].parse::<f64>()
            {
                sum_rms += v;
            }
            if let Some(col) = gr_col
                && let Ok(v) = record[col].parse::<f64>()
            {
                sum_gr += v;
            }
            count += 1;
        }

        // Also check for THD data
        let mut avg_thd: Option<f64> = None;
        let thd_path = dir.join("grid_thd.csv");
        if thd_path.exists() {
            let mut thd_rdr = csv::Reader::from_path(&thd_path)?;
            let thd_headers = thd_rdr.headers()?.clone();
            let thd_col = thd_headers.iter().position(|h| h == "thd_percent");

            if let Some(col) = thd_col {
                let (mut thd_sum, mut thd_count) = (0.0f64, 0usize);
                for record in thd_rdr.records() {
                    let record = record?;
                    if let Ok(v) = record[col].parse::<f64>() {
                        thd_sum += v;
                        thd_count += 1;
                    }
                }
                if thd_count > 0 {
                    avg_thd = Some(thd_sum / thd_count as f64);
                }
            }
        }

        if count > 0 {
            summaries.push(PluginSummary {
                name: name.clone(),
                runs: count,
                avg_output_rms_db: sum_rms / count as f64,
                avg_gain_reduction_db: sum_gr / count as f64,
                avg_thd_percent: avg_thd,
            });
        }
    }

    if summaries.is_empty() {
        return Ok(());
    }

    // Print comparison table
    let max_name = summaries
        .iter()
        .map(|s| s.name.len())
        .max()
        .unwrap_or(10)
        .max(10);

    eprintln!(
        "\n  {:<width$}  {:>6}  {:>12}  {:>8}  {:>10}",
        "Plugin",
        "Runs",
        "Avg Out RMS",
        "Avg GR",
        "Avg THD",
        width = max_name,
    );
    eprintln!("  {}", "─".repeat(max_name + 44));

    for s in &summaries {
        let thd_str = match s.avg_thd_percent {
            Some(v) => format!("{:.4}%", v),
            None => "—".to_string(),
        };
        eprintln!(
            "  {:<width$}  {:>6}  {:>10.2} dB  {:>6.2} dB  {:>10}",
            s.name,
            s.runs,
            s.avg_output_rms_db,
            s.avg_gain_reduction_db,
            thd_str,
            width = max_name,
        );
    }

    // Print deltas if exactly 2 plugins
    if summaries.len() == 2 {
        let a = &summaries[0];
        let b = &summaries[1];
        eprintln!("  {}", "─".repeat(max_name + 44));
        eprintln!(
            "  {:<width$}  {:>6}  {:>10.2} dB  {:>6.2} dB  {:>10}",
            "Delta (B-A)",
            "",
            b.avg_output_rms_db - a.avg_output_rms_db,
            b.avg_gain_reduction_db - a.avg_gain_reduction_db,
            match (a.avg_thd_percent, b.avg_thd_percent) {
                (Some(va), Some(vb)) => format!("{:.4}%", vb - va),
                _ => "—".to_string(),
            },
            width = max_name,
        );
    }

    eprintln!();
    Ok(())
}

// ---------------------------------------------------------------------------
// dump-vst2-state implementation (disabled — host_vst2 module not available)
// ---------------------------------------------------------------------------

fn find_peak_frequency(signal: &[f32], sample_rate: f32) -> f32 {
    use rustfft::{FftPlanner, num_complex::Complex};

    let n = signal.len().next_power_of_two();
    let mut planner = FftPlanner::new();
    let fft = planner.plan_fft_forward(n);

    let mut buffer: Vec<Complex<f32>> = signal
        .iter()
        .map(|&s| Complex::new(s, 0.0))
        .chain(std::iter::repeat(Complex::new(0.0, 0.0)))
        .take(n)
        .collect();

    fft.process(&mut buffer);

    // Find peak in first half (positive frequencies), skip DC
    let half = n / 2;
    let (peak_bin, _) = buffer[1..half]
        .iter()
        .enumerate()
        .max_by(|a, b| a.1.norm().partial_cmp(&b.1.norm()).unwrap())
        .unwrap();

    (peak_bin + 1) as f32 * sample_rate / n as f32
}

fn print_hex_dump(data: &[u8]) {
    for (i, chunk) in data.chunks(16).enumerate() {
        let offset = i * 16;
        print!("  {:04x}  ", offset);

        // Hex bytes
        for (j, byte) in chunk.iter().enumerate() {
            print!("{:02x} ", byte);
            if j == 7 {
                print!(" ");
            }
        }
        // Padding for short last line
        for j in chunk.len()..16 {
            print!("   ");
            if j == 7 {
                print!(" ");
            }
        }

        // ASCII
        print!(" |");
        for byte in chunk {
            if byte.is_ascii_graphic() || *byte == b' ' {
                print!("{}", *byte as char);
            } else {
                print!(".");
            }
        }
        println!("|");
    }
}
