//! Core types for plugin hub communication.

use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU32, Ordering};

/// Unique identifier for a plugin instance.
pub type PluginInstanceId = u64;

/// Unique identifier for a parameter within a plugin.
pub type ParamId = u32;

/// Generate a unique plugin instance ID.
///
/// This combines a process-unique counter with a timestamp to ensure
/// uniqueness even across plugin reloads.
pub fn generate_instance_id() -> PluginInstanceId {
    static COUNTER: AtomicU32 = AtomicU32::new(0);
    let counter = COUNTER.fetch_add(1, Ordering::Relaxed);
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs() as u32)
        .unwrap_or(0);
    ((timestamp as u64) << 32) | (counter as u64)
}

/// Type of FTS plugin.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PluginType {
    /// Simple gain plugin
    Gain,
    /// Parametric equalizer
    Eq,
    /// Compressor/dynamics
    Compressor,
    /// Limiter
    Limiter,
    /// Reverb
    Reverb,
    /// Delay
    Delay,
    /// Saturation/distortion
    Saturation,
    /// Generic/other
    Other(u32),
}

impl PluginType {
    /// Get the display name for this plugin type.
    #[must_use]
    pub fn display_name(&self) -> &'static str {
        match self {
            Self::Gain => "Gain",
            Self::Eq => "EQ",
            Self::Compressor => "Compressor",
            Self::Limiter => "Limiter",
            Self::Reverb => "Reverb",
            Self::Delay => "Delay",
            Self::Saturation => "Saturation",
            Self::Other(_) => "Plugin",
        }
    }
}

/// Information about a registered plugin instance.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PluginInfo {
    /// Unique instance ID
    pub id: PluginInstanceId,
    /// Type of plugin
    pub plugin_type: PluginType,
    /// Display name (e.g., "Gain" or user-defined)
    pub name: String,
    /// Track name from DAW (if available)
    pub track_name: Option<String>,
    /// Track index in DAW (if available)
    pub track_index: Option<u32>,
    /// Position in FX chain (if available)
    pub fx_index: Option<u32>,
}

/// Parameter metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParamInfo {
    /// Parameter ID (unique within plugin)
    pub id: ParamId,
    /// Display name
    pub name: String,
    /// Unit suffix (e.g., "dB", "ms", "%")
    pub unit: String,
    /// Minimum value
    pub min: f32,
    /// Maximum value
    pub max: f32,
    /// Default value
    pub default: f32,
    /// Current value
    pub value: f32,
    /// Whether this parameter can be automated
    pub automatable: bool,
}

/// Meter data from a plugin (real-time, lock-free).
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct MeterData {
    /// Left channel peak level (linear, 0.0-1.0+)
    pub peak_l: f32,
    /// Right channel peak level (linear, 0.0-1.0+)
    pub peak_r: f32,
    /// RMS level left (linear)
    pub rms_l: f32,
    /// RMS level right (linear)
    pub rms_r: f32,
    /// Gain reduction (for compressors, in dB, negative)
    pub gain_reduction: f32,
}

impl MeterData {
    /// Convert peak values to decibels.
    #[must_use]
    pub fn peak_db(&self) -> (f32, f32) {
        (
            20.0 * self.peak_l.max(1e-10).log10(),
            20.0 * self.peak_r.max(1e-10).log10(),
        )
    }

    /// Convert RMS values to decibels.
    #[must_use]
    pub fn rms_db(&self) -> (f32, f32) {
        (
            20.0 * self.rms_l.max(1e-10).log10(),
            20.0 * self.rms_r.max(1e-10).log10(),
        )
    }
}

/// Current state of a plugin (snapshot).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginState {
    /// Plugin info
    pub info: PluginInfo,
    /// All parameters
    pub params: Vec<ParamInfo>,
    /// Current meter data
    pub meters: MeterData,
    /// Whether the plugin is bypassed
    pub bypassed: bool,
}

/// Channel strip state (all plugins on a track).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChannelStripState {
    /// Track name
    pub track_name: String,
    /// Track index
    pub track_index: u32,
    /// Plugins in order (first to last in chain)
    pub plugins: Vec<PluginState>,
}
