//! Format-agnostic plugin abstraction.
//!
//! The renderer + Effects service talk to plugins through
//! [`PluginInstance`]. CLAP is the only format wired today; VST3 and
//! LV2 backends slot in by implementing the same trait.
//!
//! The trait is intentionally minimal — it covers what the project
//! renderer needs (prepare / process_block / param query) plus the
//! UI surface (descriptor, value↔text). Format-specific extras
//! (state chunks, view embedding, MIDI ports) live as inherent
//! methods on the concrete backends.

use std::fmt;
use std::path::Path;

/// Plugin format identifier — used by `Effects::add` to pick the
/// right backend.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PluginFormat {
    Clap,
    Vst3,
    Lv2,
    /// Synthetic placeholder (8 generic params, no DSP). Default
    /// when no real backend is available.
    Synthetic,
}

impl PluginFormat {
    /// Sniff the format from a path or identifier string.
    /// - `*.clap` → Clap
    /// - `*.vst3` → Vst3
    /// - `*.lv2` → Lv2
    /// - everything else → Synthetic
    pub fn from_path_or_name(name: &str) -> Self {
        let lower = name.to_ascii_lowercase();
        if lower.ends_with(".clap") {
            Self::Clap
        } else if lower.ends_with(".vst3") {
            Self::Vst3
        } else if lower.ends_with(".lv2") {
            Self::Lv2
        } else {
            Self::Synthetic
        }
    }
}

/// Plugin-format-neutral descriptor.
#[derive(Clone, Debug, Default)]
pub struct PluginDescriptor {
    pub id: String,
    pub name: String,
    pub vendor: String,
    pub version: String,
    pub format: PluginFormat,
}

impl Default for PluginFormat {
    fn default() -> Self {
        Self::Synthetic
    }
}

/// One parameter exposed by the plugin.
#[derive(Clone, Debug)]
pub struct PluginParamInfo {
    pub id: u32,
    pub name: String,
    pub min: f64,
    pub max: f64,
    pub default: f64,
}

/// A boxed, format-neutral plugin instance.
///
/// All methods take `&mut self` because plugin internals are
/// inherently stateful (sample rate, activation state, parameter
/// values). The renderer holds these inside a `Mutex` and acquires
/// it per audio callback.
pub trait PluginInstance: Send {
    /// Metadata about the plugin (id / name / vendor / format).
    fn descriptor(&self) -> PluginDescriptor;

    /// All parameters this plugin exposes.
    fn params(&mut self) -> Vec<PluginParamInfo>;

    /// Current value of a parameter, if the plugin can report it.
    fn param_value(&mut self, id: u32) -> Option<f64>;

    /// Format the value for display (e.g. `"-12 dB"`). `None` if the
    /// plugin doesn't provide a formatter.
    fn value_to_text(&mut self, id: u32, value: f64) -> Option<String>;

    /// Inverse of `value_to_text` — parse `"−12 dB"` back to a value.
    fn text_to_value(&mut self, id: u32, text: &str) -> Option<f64>;

    /// Reported plugin latency in samples (0 if not reported).
    fn latency(&mut self) -> u32;

    /// Activate the plugin at the given audio config. Must be called
    /// before any `process_block` calls.
    fn prepare(&mut self, sample_rate: f64, block_size: u32) -> Result<(), PluginError>;

    /// Whether `prepare` has been called and the plugin is ready.
    fn is_prepared(&self) -> bool;

    /// Render one block. `param_events` is a list of
    /// `(param_id, value)` changes scheduled at the start of the
    /// block (typical use: drive automation envelopes).
    fn process_block(
        &mut self,
        in_l: &[f32],
        in_r: &[f32],
        out_l: &mut [f32],
        out_r: &mut [f32],
        param_events: &[(u32, f64)],
    ) -> Result<(), PluginError>;

    /// Release activation. Idempotent — calling twice is fine.
    fn deactivate(&mut self);
}

/// Errors any plugin backend can raise.
#[derive(Debug)]
pub enum PluginError {
    /// Path didn't resolve or the bundle couldn't be opened.
    LoadFailed(String),
    /// `process_block` called before `prepare`.
    NotActivated,
    /// Activation failed (plugin rejected the config).
    ActivateFailed(String),
    /// Block size exceeded the prepared max.
    BlockTooLarge,
    /// Backend reported an error during processing.
    ProcessFailed(String),
    /// Backend not compiled in (e.g. `vst3` backend requested
    /// without the `vst3-host` feature).
    UnsupportedFormat(PluginFormat),
}

impl fmt::Display for PluginError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LoadFailed(s) => write!(f, "plugin load failed: {s}"),
            Self::NotActivated => write!(f, "plugin not activated"),
            Self::ActivateFailed(s) => write!(f, "plugin activation failed: {s}"),
            Self::BlockTooLarge => write!(f, "process block exceeds prepared size"),
            Self::ProcessFailed(s) => write!(f, "plugin process failed: {s}"),
            Self::UnsupportedFormat(fmt_) => {
                write!(f, "plugin format not supported in this build: {fmt_:?}")
            }
        }
    }
}

impl std::error::Error for PluginError {}

/// Format-aware instantiator. Tries to load `name_or_path` using the
/// first backend that recognizes the format. Returns
/// `PluginError::UnsupportedFormat` if no backend can handle it.
///
/// `Synthetic` returns `Ok(None)` — caller falls back to the
/// existing synthetic FX path (see `crate::fx`).
pub fn load_plugin(
    name_or_path: &str,
) -> Result<Option<Box<dyn PluginInstance>>, PluginError> {
    let format = PluginFormat::from_path_or_name(name_or_path);
    match format {
        PluginFormat::Synthetic => Ok(None),
        PluginFormat::Clap => load_clap(name_or_path),
        PluginFormat::Vst3 => Err(PluginError::UnsupportedFormat(format)),
        PluginFormat::Lv2 => Err(PluginError::UnsupportedFormat(format)),
    }
}

#[cfg(feature = "clap-host")]
fn load_clap(path: &str) -> Result<Option<Box<dyn PluginInstance>>, PluginError> {
    use crate::audio_engine::plugin_host::ClapHost;
    let host = ClapHost::default();
    let plugin = host
        .load(Path::new(path), 0)
        .map_err(|e| PluginError::LoadFailed(format!("{e:?}")))?;
    // Wrap so the instance is `Send` for storage in the shared
    // `Mutex<HashMap<..>>`. Safety: all access goes through the
    // mutex, so concurrent thread access is impossible.
    Ok(Some(Box::new(plugin.into_send())))
}

#[cfg(not(feature = "clap-host"))]
fn load_clap(_path: &str) -> Result<Option<Box<dyn PluginInstance>>, PluginError> {
    Err(PluginError::UnsupportedFormat(PluginFormat::Clap))
}

// Convenience so `Path` import on the cfg(feature) branch is used.
#[allow(dead_code)]
fn _path_import(_: &Path) {}
