use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Top-level measurement configuration, loaded from JSON.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MeasureConfig {
    pub plugin_path: PathBuf,
    #[serde(default = "default_sample_rate")]
    pub sample_rate: f64,
    #[serde(default = "default_seconds")]
    pub seconds: f32,
    #[serde(default = "default_block_size")]
    pub block_size: u32,
    #[serde(default = "default_signal_type")]
    pub signal_type: SignalType,
    #[serde(default = "default_sine_frequency")]
    pub sine_frequency: f32,
    #[serde(default = "default_sweep_start")]
    pub sweep_start_hz: f32,
    #[serde(default = "default_sweep_end")]
    pub sweep_end_hz: f32,
    #[serde(default)]
    pub input_gain_buckets_db: Vec<f32>,
    #[serde(default)]
    pub parameter_buckets: Vec<ParameterBucket>,
    #[serde(default)]
    pub analyzers: Vec<AnalyzerType>,
    /// Plugin index within the bundle (default: 0).
    #[serde(default)]
    pub plugin_index: usize,
    /// Number of warmup seconds to discard from the start of each run.
    #[serde(default = "default_warmup")]
    pub warmup_seconds: f32,
    /// FFT size used by LinearResponse and THD analyzers (default: 4096).
    #[serde(default = "default_fft_size")]
    pub fft_size: usize,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum SignalType {
    Sine,
    Noise,
    Sweep,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum AnalyzerType {
    RawCsv,
    RmsPeak,
    TransferCurve,
    LinearResponse,
    Thd,
    GainReduction,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ParameterBucket {
    pub param_name: String,
    pub strategy: BucketStrategy,
    #[serde(default)]
    pub min: f64,
    #[serde(default = "default_one")]
    pub max: f64,
    #[serde(default = "default_num_buckets")]
    pub num_buckets: usize,
    #[serde(default)]
    pub values: Vec<f64>,
    /// Display-text values for `DisplayText` strategy (e.g. `["1000 Hz", "6.0 dB"]`).
    /// Each string is passed to the plugin's `text_to_value` to resolve to a native value.
    #[serde(default)]
    pub text_values: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum BucketStrategy {
    Linear,
    ExplicitValues,
    Log,
    EdgeAndCenter,
    /// Values specified as display text (e.g. "1000 Hz"). Resolved per-plugin via
    /// CLAP `text_to_value`, so the same text works across plugins with different
    /// internal representations.
    DisplayText,
}

impl ParameterBucket {
    /// Generate the concrete parameter values for this bucket config.
    pub fn generate_values(&self) -> Vec<f64> {
        match self.strategy {
            BucketStrategy::Linear => {
                let n = self.num_buckets.max(2);
                (0..n)
                    .map(|i| self.min + (self.max - self.min) * i as f64 / (n - 1) as f64)
                    .collect()
            }
            BucketStrategy::ExplicitValues => self.values.clone(),
            BucketStrategy::Log => {
                let n = self.num_buckets.max(2);
                let log_min = self.min.max(1e-10).ln();
                let log_max = self.max.max(1e-10).ln();
                (0..n)
                    .map(|i| (log_min + (log_max - log_min) * i as f64 / (n - 1) as f64).exp())
                    .collect()
            }
            BucketStrategy::EdgeAndCenter => {
                let center = (self.min + self.max) / 2.0;
                vec![self.min, center, self.max]
            }
            BucketStrategy::DisplayText => {
                // DisplayText values are resolved at grid resolution time via text_to_value.
                // generate_values() shouldn't be called for this strategy.
                Vec::new()
            }
        }
    }
}

fn default_sample_rate() -> f64 {
    48000.0
}
fn default_seconds() -> f32 {
    5.0
}
fn default_block_size() -> u32 {
    256
}
fn default_signal_type() -> SignalType {
    SignalType::Sine
}
fn default_sine_frequency() -> f32 {
    1000.0
}
fn default_sweep_start() -> f32 {
    20.0
}
fn default_sweep_end() -> f32 {
    20000.0
}
fn default_warmup() -> f32 {
    0.1
}
fn default_one() -> f64 {
    1.0
}
fn default_num_buckets() -> usize {
    5
}
fn default_fft_size() -> usize {
    4096
}

impl MeasureConfig {
    pub fn from_json(json: &str) -> anyhow::Result<Self> {
        Ok(serde_json::from_str(json)?)
    }

    pub fn from_file(path: &std::path::Path) -> anyhow::Result<Self> {
        let content = std::fs::read_to_string(path)?;
        Self::from_json(&content)
    }
}

// ---------------------------------------------------------------------------
// Analysis config — multi-plugin comparison runs
// ---------------------------------------------------------------------------

/// A reusable analysis script that runs the same measurement against multiple plugins.
///
/// Each plugin entry can define its own parameter buckets (since parameter names
/// differ between plugins), while signal, sample rate, analyzers, etc. are shared.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AnalysisConfig {
    /// Human-readable name for this analysis (used in output).
    pub name: String,

    /// Shared measurement settings (signal, sample rate, analyzers, etc.).
    pub measurement: MeasurementSettings,

    /// Plugins to measure. Each gets its own output subdirectory.
    pub plugins: Vec<PluginEntry>,
}

/// Shared measurement settings (everything except plugin path and parameter buckets).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MeasurementSettings {
    #[serde(default = "default_sample_rate")]
    pub sample_rate: f64,
    #[serde(default = "default_seconds")]
    pub seconds: f32,
    #[serde(default = "default_block_size")]
    pub block_size: u32,
    #[serde(default = "default_signal_type")]
    pub signal_type: SignalType,
    #[serde(default = "default_sine_frequency")]
    pub sine_frequency: f32,
    #[serde(default = "default_sweep_start")]
    pub sweep_start_hz: f32,
    #[serde(default = "default_sweep_end")]
    pub sweep_end_hz: f32,
    #[serde(default)]
    pub input_gain_buckets_db: Vec<f32>,
    #[serde(default)]
    pub analyzers: Vec<AnalyzerType>,
    #[serde(default)]
    pub plugin_index: usize,
    #[serde(default = "default_warmup")]
    pub warmup_seconds: f32,
    #[serde(default = "default_fft_size")]
    pub fft_size: usize,
}

/// A plugin to include in the analysis run.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PluginEntry {
    /// Display name (used as output subdirectory name).
    pub name: String,
    /// Path to the plugin file (.clap or .vst3).
    pub path: PathBuf,
    /// Plugin-specific parameter buckets.
    #[serde(default)]
    pub parameter_buckets: Vec<ParameterBucket>,
    /// Override plugin index within bundle (default: use shared setting).
    #[serde(default)]
    pub plugin_index: Option<usize>,
}

impl AnalysisConfig {
    pub fn from_json(json: &str) -> anyhow::Result<Self> {
        Ok(serde_json::from_str(json)?)
    }

    pub fn from_file(path: &std::path::Path) -> anyhow::Result<Self> {
        let content = std::fs::read_to_string(path)?;
        Self::from_json(&content)
    }

    /// Convert a plugin entry into a full MeasureConfig using the shared settings.
    pub fn to_measure_config(&self, plugin: &PluginEntry) -> MeasureConfig {
        let m = &self.measurement;
        MeasureConfig {
            plugin_path: plugin.path.clone(),
            sample_rate: m.sample_rate,
            seconds: m.seconds,
            block_size: m.block_size,
            signal_type: m.signal_type,
            sine_frequency: m.sine_frequency,
            sweep_start_hz: m.sweep_start_hz,
            sweep_end_hz: m.sweep_end_hz,
            input_gain_buckets_db: m.input_gain_buckets_db.clone(),
            parameter_buckets: plugin.parameter_buckets.clone(),
            analyzers: m.analyzers.clone(),
            plugin_index: plugin.plugin_index.unwrap_or(m.plugin_index),
            warmup_seconds: m.warmup_seconds,
            fft_size: m.fft_size,
        }
    }
}
