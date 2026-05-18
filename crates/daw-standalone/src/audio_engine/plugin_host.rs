//! CLAP plugin host.
//!
//! Adapted from `fts-analyzer/crates/fts-analyzer/src/host.rs` —
//! same `clack-host` crate, but reshaped for realtime DAW use:
//!
//! - Plugins stay **activated** across many process calls (the
//!   analyzer activates+processes+deactivates per process call).
//! - Activation lifecycle is exposed (`prepare` / `deactivate`) so
//!   the audio callback can call `process_block` repeatedly with
//!   stable I/O buffers.
//! - Parameter events are pushed via a per-block input event buffer.
//!
//! This module owns no project state — it's a thin wrapper over
//! `clack-host`. The renderer + Effects service layer thread these
//! `LoadedClapPlugin`s into the project's FX chains separately.
//!
//! Feature-gated under `clap-host`. Native-only (`clack-host`
//! dlopen's native CLAP bundles).

#![cfg(feature = "clap-host")]
// `PluginEntry::load` is `unsafe` because it dlopens an arbitrary
// native library. The caller is responsible for trusting the path —
// in our case the user picked the .clap bundle from disk or the
// project file, so the unsafety is part of the host contract.
#![allow(unsafe_code)]

use std::ffi::CString;
use std::path::Path;

use clack_extensions::latency::*;
use clack_extensions::params::*;
use clack_host::events::event_types::ParamValueEvent;
use clack_host::prelude::*;
use clack_host::utils::Cookie;

// ── Host handlers (minimal stubs) ────────────────────────────────────

struct DawHostShared;

impl<'a> SharedHandler<'a> for DawHostShared {
    fn request_restart(&self) {}
    fn request_process(&self) {}
    fn request_callback(&self) {}
}

struct DawHostMainThread;

impl<'a> MainThreadHandler<'a> for DawHostMainThread {}

struct DawHostAudioProcessor;

impl<'a> AudioProcessorHandler<'a> for DawHostAudioProcessor {}

struct DawHost;

impl HostHandlers for DawHost {
    type Shared<'a> = DawHostShared;
    type MainThread<'a> = DawHostMainThread;
    type AudioProcessor<'a> = DawHostAudioProcessor;
}

// ── Public host handle ──────────────────────────────────────────────

/// A CLAP plugin host. Cheap — just a holder for the `HostInfo`
/// string identity. Construct once per `Standalone` (or once globally).
#[derive(Clone)]
pub struct ClapHost {
    name: String,
    vendor: String,
    url: String,
    version: String,
}

impl Default for ClapHost {
    fn default() -> Self {
        Self {
            name: "daw-standalone".into(),
            vendor: "com.fasttrackstudio.daw".into(),
            url: String::new(),
            version: env!("CARGO_PKG_VERSION").to_string(),
        }
    }
}

impl ClapHost {
    pub fn new(name: impl Into<String>, vendor: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            vendor: vendor.into(),
            url: String::new(),
            version: env!("CARGO_PKG_VERSION").to_string(),
        }
    }

    fn host_info(&self) -> Result<HostInfo, ClapHostError> {
        HostInfo::new(&self.name, &self.vendor, &self.url, &self.version)
            .map_err(|_| ClapHostError::HostInfo)
    }

    /// List every plugin descriptor inside a `.clap` bundle. Returns
    /// `(id, name)` pairs ordered as the bundle exposes them.
    pub fn list_in_bundle(
        &self,
        bundle_path: &Path,
    ) -> Result<Vec<ClapPluginDescriptor>, ClapHostError> {
        let entry =
            unsafe { PluginEntry::load(bundle_path) }.map_err(|_| ClapHostError::BundleLoad)?;
        let factory = entry.get_plugin_factory().ok_or(ClapHostError::NoFactory)?;
        let mut out = Vec::new();
        for d in factory.plugin_descriptors() {
            let id = d
                .id()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_default();
            let name = d
                .name()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_default();
            let vendor = d
                .vendor()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_default();
            let version = d
                .version()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_default();
            out.push(ClapPluginDescriptor {
                id,
                name,
                vendor,
                version,
            });
        }
        Ok(out)
    }

    /// Instantiate a plugin from a `.clap` bundle. `plugin_index`
    /// selects which descriptor inside the bundle (most bundles ship
    /// a single plugin — use 0).
    pub fn load(
        &self,
        bundle_path: &Path,
        plugin_index: usize,
    ) -> Result<LoadedClapPlugin, ClapHostError> {
        let entry =
            unsafe { PluginEntry::load(bundle_path) }.map_err(|_| ClapHostError::BundleLoad)?;
        let factory = entry.get_plugin_factory().ok_or(ClapHostError::NoFactory)?;
        let descriptor = factory
            .plugin_descriptors()
            .nth(plugin_index)
            .ok_or(ClapHostError::IndexOutOfRange)?;
        let id = descriptor.id().ok_or(ClapHostError::MissingId)?;
        let host_info = self.host_info()?;

        let instance = PluginInstance::<DawHost>::new(
            |_| DawHostShared,
            |_| DawHostMainThread,
            &entry,
            id,
            &host_info,
        )
        .map_err(|_| ClapHostError::Instantiate)?;

        Ok(LoadedClapPlugin {
            instance,
            activation: None,
            descriptor: ClapPluginDescriptor {
                id: id.to_string_lossy().into_owned(),
                name: descriptor
                    .name()
                    .map(|s| s.to_string_lossy().into_owned())
                    .unwrap_or_default(),
                vendor: descriptor
                    .vendor()
                    .map(|s| s.to_string_lossy().into_owned())
                    .unwrap_or_default(),
                version: descriptor
                    .version()
                    .map(|s| s.to_string_lossy().into_owned())
                    .unwrap_or_default(),
            },
        })
    }
}

/// Bundle-time metadata for a single CLAP plugin descriptor.
#[derive(Clone, Debug)]
pub struct ClapPluginDescriptor {
    pub id: String,
    pub name: String,
    pub vendor: String,
    pub version: String,
}

/// One CLAP parameter as the host sees it.
#[derive(Clone, Debug)]
pub struct ClapParamInfo {
    pub id: u32,
    pub name: String,
    pub min: f64,
    pub max: f64,
    pub default: f64,
}

/// An instantiated CLAP plugin. Activation is separate so the
/// plugin can be inspected (params, latency) before being used in
/// the audio thread.
pub struct LoadedClapPlugin {
    instance: PluginInstance<DawHost>,
    /// Persists the active processor + scratch buffers across
    /// `process_block` calls when prepared.
    activation: Option<ActivationGuard>,
    descriptor: ClapPluginDescriptor,
}

struct ActivationGuard {
    /// The processor as returned by `start_processing()`. Boxed
    /// because the concrete type comes from a closure inside
    /// clack-host and isn't directly nameable.
    processor: StartedPluginAudioProcessor<DawHost>,
    sample_rate: f64,
    block_size: u32,
    /// Reusable per-block scratch buffers so process_block doesn't
    /// allocate on the hot path. Length = block_size.
    scratch_l: Vec<f32>,
    scratch_r: Vec<f32>,
}

impl LoadedClapPlugin {
    pub fn descriptor(&self) -> &ClapPluginDescriptor {
        &self.descriptor
    }

    /// All parameters this plugin exposes. Safe to call before /
    /// after activation.
    pub fn params(&mut self) -> Vec<ClapParamInfo> {
        let mut handle = self.instance.plugin_handle();
        let Some(params_ext) = handle.get_extension::<PluginParams>() else {
            return Vec::new();
        };
        let count = params_ext.count(&mut handle);
        let mut out = Vec::with_capacity(count as usize);
        let mut info_buf = ParamInfoBuffer::new();
        for i in 0..count {
            if let Some(info) = params_ext.get_info(&mut handle, i, &mut info_buf) {
                out.push(ClapParamInfo {
                    id: info.id.get(),
                    name: String::from_utf8_lossy(info.name).into_owned(),
                    min: info.min_value,
                    max: info.max_value,
                    default: info.default_value,
                });
            }
        }
        out
    }

    /// Current value of a parameter. `None` if the plugin doesn't
    /// expose `clap_plugin_params` or the id is unknown.
    pub fn param_value(&mut self, param_id: u32) -> Option<f64> {
        let mut handle = self.instance.plugin_handle();
        let params_ext = handle.get_extension::<PluginParams>()?;
        params_ext.get_value(&mut handle, ClapId::from(param_id))
    }

    /// Format a parameter value as the plugin would display it
    /// (e.g. `"-12.3 dB"`). Uses CLAP `value_to_text`.
    pub fn value_to_text(&mut self, param_id: u32, value: f64) -> Option<String> {
        let mut handle = self.instance.plugin_handle();
        let params_ext = handle.get_extension::<PluginParams>()?;
        let mut buf = [0u8; 256];
        params_ext
            .value_to_text(&mut handle, ClapId::from(param_id), value, &mut buf)
            .ok()?;
        let end = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
        std::str::from_utf8(&buf[..end])
            .ok()
            .map(|s| s.to_string())
    }

    /// Parse a display string into a parameter value (inverse of
    /// `value_to_text`).
    pub fn text_to_value(&mut self, param_id: u32, text: &str) -> Option<f64> {
        let mut handle = self.instance.plugin_handle();
        let params_ext = handle.get_extension::<PluginParams>()?;
        let cstr = CString::new(text).ok()?;
        params_ext.text_to_value(&mut handle, ClapId::from(param_id), &cstr)
    }

    /// Plugin's reported latency (in samples). 0 if `clap_plugin_latency`
    /// isn't implemented.
    pub fn latency(&mut self) -> u32 {
        let mut handle = self.instance.plugin_handle();
        handle
            .get_extension::<PluginLatency>()
            .map(|ext| ext.get(&mut handle))
            .unwrap_or(0)
    }

    /// Activate the plugin at the given audio config and start its
    /// processor. Subsequent `process_block` calls produce audio
    /// without re-activating. `deactivate()` releases.
    pub fn prepare(&mut self, sample_rate: f64, block_size: u32) -> Result<(), ClapHostError> {
        if self.activation.is_some() {
            // Already prepared; treat re-prepare as a config bump
            // (drop + redo so sample rate / block size can change).
            self.deactivate();
        }
        let config = PluginAudioConfiguration {
            sample_rate,
            min_frames_count: 1,
            max_frames_count: block_size,
        };
        let activated = self
            .instance
            .activate(|_, _| DawHostAudioProcessor, config)
            .map_err(|_| ClapHostError::Activate)?;
        let processor = activated
            .start_processing()
            .map_err(|_| ClapHostError::StartProcessing)?;
        self.activation = Some(ActivationGuard {
            processor,
            sample_rate,
            block_size,
            scratch_l: vec![0.0; block_size as usize],
            scratch_r: vec![0.0; block_size as usize],
        });
        Ok(())
    }

    /// Process one block. `in_l/in_r` and `out_l/out_r` must all be
    /// the same length and ≤ the prepared `block_size`.
    /// `param_events` is `(param_id, value)` pairs scheduled at frame 0
    /// — pass `&[]` for no automation this block.
    pub fn process_block(
        &mut self,
        in_l: &[f32],
        in_r: &[f32],
        out_l: &mut [f32],
        out_r: &mut [f32],
        param_events: &[(u32, f64)],
    ) -> Result<(), ClapHostError> {
        let Some(act) = self.activation.as_mut() else {
            return Err(ClapHostError::NotActivated);
        };
        let frames = in_l.len().min(in_r.len()).min(out_l.len()).min(out_r.len());
        if frames > act.block_size as usize {
            return Err(ClapHostError::BlockTooLarge);
        }

        // Copy inputs into scratch (clack wants mutable input slices
        // even for read-only channels).
        let scratch_l_full = &mut act.scratch_l[..frames];
        let scratch_r_full = &mut act.scratch_r[..frames];
        scratch_l_full.copy_from_slice(&in_l[..frames]);
        scratch_r_full.copy_from_slice(&in_r[..frames]);

        let mut input_events = EventBuffer::new();
        for &(param_id, value) in param_events {
            input_events.push(&ParamValueEvent::new(
                0,
                ClapId::from(param_id),
                Pckn::match_all(),
                value,
                Cookie::empty(),
            ));
        }
        let mut output_events = EventBuffer::new();

        let mut input_ports = AudioPorts::with_capacity(2, 1);
        let mut output_ports = AudioPorts::with_capacity(2, 1);

        let inputs = input_ports.with_input_buffers([AudioPortBuffer {
            latency: 0,
            channels: AudioPortBufferType::f32_input_only([
                InputChannel {
                    buffer: scratch_l_full,
                    is_constant: false,
                },
                InputChannel {
                    buffer: scratch_r_full,
                    is_constant: false,
                },
            ]),
        }]);
        let mut outputs = output_ports.with_output_buffers([AudioPortBuffer {
            latency: 0,
            channels: AudioPortBufferType::f32_output_only([
                &mut out_l[..frames],
                &mut out_r[..frames],
            ]),
        }]);

        let in_evs = input_events.as_input();
        let mut out_evs = output_events.as_output();
        act.processor
            .process(&inputs, &mut outputs, &in_evs, &mut out_evs, None, None)
            .map_err(|_| ClapHostError::Process)?;
        Ok(())
    }

    pub fn is_prepared(&self) -> bool {
        self.activation.is_some()
    }

    pub fn sample_rate(&self) -> Option<f64> {
        self.activation.as_ref().map(|a| a.sample_rate)
    }

    pub fn block_size(&self) -> Option<u32> {
        self.activation.as_ref().map(|a| a.block_size)
    }

    /// Release activation. The plugin instance stays loaded; call
    /// `prepare` again to resume processing.
    pub fn deactivate(&mut self) {
        if let Some(act) = self.activation.take() {
            let stopped = act.processor.stop_processing();
            self.instance.deactivate(stopped);
        }
    }
}

impl Drop for LoadedClapPlugin {
    fn drop(&mut self) {
        self.deactivate();
    }
}

// ── PluginInstance bridge ────────────────────────────────────────────
//
// `clack_host::PluginInstance` carries a `PhantomData<*const ()>` to
// mark itself `!Send` — clack is conservative because some CLAP
// extensions (main-thread query) aren't thread-safe. Our project
// stores plugins inside an `Arc<Mutex<HashMap<...>>>` and the audio
// thread acquires the Mutex before any plugin call, so concurrent
// access is impossible by construction. The wrapper below opts back
// into `Send`, with that invariant as its safety contract.

/// Marker wrapper that asserts (via unsafe) that the wrapped value
/// is safe to send between threads as long as all access is
/// serialized externally (e.g. through a `Mutex`).
pub struct ThreadSerialized<T>(T);

impl<T> ThreadSerialized<T> {
    /// SAFETY: caller must guarantee that the wrapped value is only
    /// ever accessed from one thread at a time. In this crate that's
    /// enforced by storing instances inside `Mutex<HashMap<..>>`.
    pub unsafe fn new(value: T) -> Self {
        Self(value)
    }
}

unsafe impl<T> Send for ThreadSerialized<T> {}

impl<T> core::ops::Deref for ThreadSerialized<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}
impl<T> core::ops::DerefMut for ThreadSerialized<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

/// `LoadedClapPlugin` wrapped to be `Send` for storage in the
/// `plugin_instances` Mutex'd map. Use [`LoadedClapPlugin::into_send`]
/// to construct; `PluginInstance` is implemented for the wrapper so
/// the renderer talks to it transparently.
pub type SendableClapPlugin = ThreadSerialized<LoadedClapPlugin>;

impl LoadedClapPlugin {
    /// Wrap this instance for cross-thread storage. The plugin must
    /// only be accessed from one thread at a time afterwards (e.g.
    /// via the `Mutex<HashMap<..>>` that holds it).
    pub fn into_send(self) -> SendableClapPlugin {
        // SAFETY: caller is responsible for serializing access; we
        // document this on the public API.
        unsafe { ThreadSerialized::new(self) }
    }
}

/// `PluginInstance` is implemented only on the `Send`-wrapped form
/// (`SendableClapPlugin`) because `LoadedClapPlugin` itself is
/// `!Send`. All delegation goes through inherent methods on the
/// underlying type — no recursive trait dispatch.
impl crate::plugin::PluginInstance for SendableClapPlugin {
    fn descriptor(&self) -> crate::plugin::PluginDescriptor {
        let d = &(**self).descriptor;
        crate::plugin::PluginDescriptor {
            id: d.id.clone(),
            name: d.name.clone(),
            vendor: d.vendor.clone(),
            version: d.version.clone(),
            format: crate::plugin::PluginFormat::Clap,
        }
    }
    fn params(&mut self) -> Vec<crate::plugin::PluginParamInfo> {
        LoadedClapPlugin::params(&mut **self)
            .into_iter()
            .map(|p| crate::plugin::PluginParamInfo {
                id: p.id,
                name: p.name,
                min: p.min,
                max: p.max,
                default: p.default,
            })
            .collect()
    }
    fn param_value(&mut self, id: u32) -> Option<f64> {
        LoadedClapPlugin::param_value(&mut **self, id)
    }
    fn value_to_text(&mut self, id: u32, v: f64) -> Option<String> {
        LoadedClapPlugin::value_to_text(&mut **self, id, v)
    }
    fn text_to_value(&mut self, id: u32, t: &str) -> Option<f64> {
        LoadedClapPlugin::text_to_value(&mut **self, id, t)
    }
    fn latency(&mut self) -> u32 {
        LoadedClapPlugin::latency(&mut **self)
    }
    fn prepare(&mut self, sr: f64, bs: u32) -> Result<(), crate::plugin::PluginError> {
        LoadedClapPlugin::prepare(&mut **self, sr, bs).map_err(map_err)
    }
    fn is_prepared(&self) -> bool {
        LoadedClapPlugin::is_prepared(&**self)
    }
    fn process_block(
        &mut self,
        il: &[f32],
        ir: &[f32],
        ol: &mut [f32],
        or: &mut [f32],
        ev: &[(u32, f64)],
    ) -> Result<(), crate::plugin::PluginError> {
        LoadedClapPlugin::process_block(&mut **self, il, ir, ol, or, ev).map_err(map_err)
    }
    fn deactivate(&mut self) {
        LoadedClapPlugin::deactivate(&mut **self)
    }
}

fn map_err(e: ClapHostError) -> crate::plugin::PluginError {
    use crate::plugin::PluginError as P;
    match e {
        ClapHostError::BundleLoad => P::LoadFailed("bundle load".into()),
        ClapHostError::NoFactory => P::LoadFailed("no factory".into()),
        ClapHostError::IndexOutOfRange => P::LoadFailed("plugin index out of range".into()),
        ClapHostError::MissingId => P::LoadFailed("missing plugin id".into()),
        ClapHostError::HostInfo => P::LoadFailed("host info".into()),
        ClapHostError::Instantiate => P::LoadFailed("instantiate failed".into()),
        ClapHostError::Activate => P::ActivateFailed("activate failed".into()),
        ClapHostError::StartProcessing => P::ActivateFailed("start_processing failed".into()),
        ClapHostError::NotActivated => P::NotActivated,
        ClapHostError::BlockTooLarge => P::BlockTooLarge,
        ClapHostError::Process => P::ProcessFailed("process failed".into()),
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ClapHostError {
    #[error("failed to load .clap bundle")]
    BundleLoad,
    #[error("plugin bundle has no factory")]
    NoFactory,
    #[error("plugin index out of range")]
    IndexOutOfRange,
    #[error("plugin descriptor missing id")]
    MissingId,
    #[error("failed to create host info string set")]
    HostInfo,
    #[error("plugin instantiation failed")]
    Instantiate,
    #[error("plugin activate() failed")]
    Activate,
    #[error("plugin start_processing() failed")]
    StartProcessing,
    #[error("process() called before prepare()")]
    NotActivated,
    #[error("block exceeds prepared max_frames_count")]
    BlockTooLarge,
    #[error("plugin process() returned error")]
    Process,
}
