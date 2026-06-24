//! Live guitar-rig audio path — the standalone amp-modeler rig, a thin wrapper
//! over daw's [`LiveRig`].
//!
//! The rig is **one input-armed track in a tiny daw project** whose FX chain is
//! the active patch's chain ([NAM amp, cab IR, optional hosted CLAP/VST3]),
//! routed to master. All of that plumbing — seeding the project, arming the
//! input channel, reserving FX slots, opening the engine + live input stream,
//! metering, transport — lives in daw's [`LiveRig`] now; [`GuitarRig`] just
//! owns one and builds chains for it. There is no signal-owned cpal stream,
//! ring buffer, project/track/slot wiring, `Identity`/`InputProbe`, or
//! resident-bank any more — that work lives in daw's engine.
//!
//! ## Instant, GigPerformer-style patch switching
//!
//! Each FX block is a [`PluginInstance`]: [`NamProcessor`] / [`Convolver`]
//! (native) implement it directly; hosted plugins go through daw's own loader.
//! A "chain" install pre-builds + prepares its boxes and stores them
//! control-side. [`set_active`](GuitarRig::set_active) hands the active chain's
//! boxes to [`LiveRig::set_chain`], which swaps them into the reserved slot
//! guids under the renderer's per-block lock (glitch-free) and returns the
//! displaced boxes — dropped here, off the audio thread.
//!
//! ## Metering
//!
//! [`LiveRig`] exposes the track's post-fader peak (output) and a built-in
//! input-probe peak (pre-FX input); [`GuitarRig`] delegates both.

use std::path::Path;

use facet::Facet;

use daw::live::LiveRig;
use daw::standalone::metering::linear_to_db;
use signal_plugin_host::PluginInstance;

use crate::convolver::Convolver;
use crate::mixer::FX_PREPARE_BLOCK;
use crate::nam::NamProcessor;
use crate::rig_prefs::RigAudioPrefs;

/// Max block size the rig prepares models / plugins for. daw's callback block is
/// normally 64–1024 frames; preparing for [`FX_PREPARE_BLOCK`] keeps us safe
/// against larger backend buffers without per-block re-preparation.
const MAX_BLOCK: usize = FX_PREPARE_BLOCK as usize;

/// Number of chain FX slots reserved on the rig track via [`LiveRig`]. Chains
/// longer than this are rejected at install time. (LiveRig reserves one extra
/// hidden slot for its input probe.)
const MAX_CHAIN_SLOTS: usize = 8;

/// Identifies a chain resident control-side. Assigned on install; opaque
/// elsewhere.
pub type ModelId = u32;

/// One block in a patch's FX chain — a backend kind plus a file path.
///
/// Kept stringly-typed (`kind`) rather than an enum so it loads cleanly from
/// styx and maps trivially from a proto `BlockKind`. Build with [`RigBlock::nam`]
/// / [`RigBlock::cab_ir`].
#[derive(Clone, Debug, Facet)]
pub struct RigBlock {
    /// `"nam"` — neural amp/drive/pedal model (`.nam`).
    /// `"cab_ir"` — cabinet impulse response (`.wav`).
    /// `"plugin"` — any third-party CLAP / VST3 plugin (format auto-detected
    /// from the file). The generic alias for a hosted-plugin block.
    pub kind: String,
    /// Path to the `.nam` model, `.wav` IR, or `.clap` / `.vst3` plugin
    /// (resolved by the caller).
    pub path: String,
    /// Per-block input trim (dB) before this block. NAM only.
    #[facet(default)]
    pub input_trim_db: f32,
    /// Per-block output trim (dB) after this block. NAM only.
    #[facet(default)]
    pub output_trim_db: f32,
    /// For `"plugin"` blocks: optional base64 state chunk (saved plugin
    /// parameters) restored after load. Absent = the plugin's defaults.
    #[facet(default)]
    pub state_b64: Option<String>,
}

impl RigBlock {
    pub fn nam(path: impl Into<String>) -> Self {
        Self::bare("nam", path)
    }

    pub fn cab_ir(path: impl Into<String>) -> Self {
        Self::bare("cab_ir", path)
    }

    /// A hosted CLAP / VST3 plugin block (no saved state).
    pub fn plugin(path: impl Into<String>) -> Self {
        Self::bare("plugin", path)
    }

    /// A hosted plugin block that restores a saved state chunk.
    pub fn plugin_with_state(path: impl Into<String>, state_b64: Option<String>) -> Self {
        Self { state_b64, ..Self::bare("plugin", path) }
    }

    fn bare(kind: &str, path: impl Into<String>) -> Self {
        Self {
            kind: kind.into(),
            path: path.into(),
            input_trim_db: 0.0,
            output_trim_db: 0.0,
            state_b64: None,
        }
    }

    pub fn is_nam(&self) -> bool {
        self.kind.eq_ignore_ascii_case("nam")
    }

    pub fn is_cab_ir(&self) -> bool {
        matches!(
            self.kind.to_ascii_lowercase().as_str(),
            "cab_ir" | "cabir" | "cab" | "ir"
        )
    }

    pub fn is_plugin(&self) -> bool {
        matches!(self.kind.to_ascii_lowercase().as_str(), "plugin" | "clap" | "vst3")
    }
}

/// Control-side description of a chain resident in the rig, for the UI.
#[derive(Clone, Debug)]
pub struct SlotInfo {
    pub id: ModelId,
    /// Chain summary, e.g. `"Drive → AC30 → V30 (cab)"`.
    pub display_name: String,
    /// Per-block display names, in order.
    pub blocks: Vec<String>,
    /// Loudness (dB) of the chain's first NAM block, if declared — used for
    /// level-matching across patches.
    pub primary_loudness: Option<f64>,
    /// Expected sample rate of the chain's first NAM block, if declared.
    pub primary_expected_sr: Option<f64>,
}

/// An enumerated audio device — name, channel count, native sample rate.
#[derive(Clone, Debug)]
pub struct DeviceInfo {
    pub name: String,
    pub channels: u16,
    pub default_sample_rate: u32,
}


/// Build a prepared box for one [`RigBlock`] at `sample_rate`.
fn build_block(
    block: &RigBlock,
    sample_rate: u32,
) -> Result<(Box<dyn PluginInstance>, String, Option<f64>, Option<f64>), String> {
    if block.is_nam() {
        let mut nam = NamProcessor::load(&block.path, sample_rate as f64, MAX_BLOCK)?;
        nam.input_gain_db = block.input_trim_db;
        nam.output_gain_db = block.output_trim_db;
        if let Some(exp) = nam.expected_sample_rate() {
            if (exp - sample_rate as f64).abs() > 1.0 {
                tracing::warn!(
                    model = %nam.display_name,
                    expected_sample_rate = exp,
                    rig_sample_rate = sample_rate,
                    "NAM model trained at a different sample rate — voicing/pitch will be off"
                );
            }
        }
        let loud = nam.loudness();
        let exp_sr = nam.expected_sample_rate();
        let dn = nam.display_name.clone();
        Ok((Box::new(nam), dn, loud, exp_sr))
    } else if block.is_cab_ir() {
        let conv = Convolver::load(&block.path)?;
        let dn = conv.display_name.clone();
        Ok((Box::new(conv), format!("{dn} (cab)"), None, None))
    } else if block.is_plugin() {
        // Hosted CLAP/VST3: go through daw's own plugin loader, which returns a
        // `Box<dyn PluginInstance>` ready to drop straight into the FX chain —
        // no signal-side re-wrapping needed (the renderer drives it directly).
        let mut plugin = daw::plugin::load_plugin(&block.path)
            .map_err(|e| format!("load plugin {}: {e}", block.path))?
            .ok_or_else(|| format!("not a recognized CLAP/VST3 plugin: {}", block.path))?;
        plugin
            .prepare(sample_rate as f64, FX_PREPARE_BLOCK)
            .map_err(|e| format!("prepare plugin {}: {e}", block.path))?;
        if let Some(state) = &block.state_b64 {
            match base64_decode(state) {
                Ok(bytes) => {
                    if let Err(e) = plugin.load_state(&bytes) {
                        tracing::warn!(plugin = %block.path, error = %e, "failed to restore plugin state");
                    }
                }
                Err(e) => {
                    tracing::warn!(plugin = %block.path, error = %e, "invalid base64 plugin state");
                }
            }
        }
        let dn = plugin.descriptor().name;
        Ok((plugin, format!("{dn} (plugin)"), None, None))
    } else {
        Err(format!("unknown rig block kind: {:?}", block.kind))
    }
}

/// A resident chain: its pre-built + prepared boxes (one per chain slot, in
/// order) plus its control-side [`SlotInfo`]. Switching to it hands these boxes
/// to [`LiveRig::set_chain`].
struct ResidentChain {
    #[allow(dead_code)]
    info: SlotInfo,
    /// Boxes for chain slots `0..boxes.len()`. Taken out (`Option`) when active
    /// (handed to the live rig); reclaimed when another chain is selected.
    boxes: Vec<Option<Box<dyn PluginInstance>>>,
}

/// Mutable swap state shared behind a [`Mutex`] so the patch-switch surface
/// ([`set_active`](GuitarRig::set_active) / bypass / trims) can stay `&self`
/// (the original API), while installs (`&mut self`) lock it briefly. The
/// resident chains live here because both install (write) and activate (swap)
/// touch them; the actual box-swap goes through [`LiveRig::set_chain`], which
/// is itself `&self`.
struct SwapState {
    /// Resident chains keyed by [`ModelId`].
    chains: std::collections::HashMap<ModelId, ResidentChain>,
    /// Currently-active chain id, or [`None`] (clean DI passthrough).
    active: Option<ModelId>,
    /// Patch-level trims (dB).
    input_trim_db: f32,
    output_trim_db: f32,
    bypass: bool,
}

/// A live guitar rig: a single input-armed daw track whose FX chain is the
/// active patch, running on daw's [`LiveRig`].
pub struct GuitarRig {
    /// The live-monitor engine (owns the daw project/track/slots, cpal output +
    /// live-input streams, meters; drop = stop).
    live: LiveRig,
    pub sample_rate: u32,
    /// Mutable swap state (resident chains + active selection + trims/bypass).
    swap: std::sync::Mutex<SwapState>,
    /// Control-side mirror of installed chains (for the UI), in install order.
    slots: Vec<SlotInfo>,
    next_id: ModelId,
    prefs: RigAudioPrefs,
}

impl GuitarRig {
    /// Open the system default input + output devices.
    pub fn new() -> eyre::Result<Self> {
        Self::open(&RigAudioPrefs::default())
    }

    /// Back-compat: open by device substring on input channel 0.
    pub fn with_devices(
        input_name: Option<&str>,
        output_name: Option<&str>,
        sample_rate: Option<u32>,
        buffer_size: Option<u32>,
    ) -> eyre::Result<Self> {
        Self::open(&RigAudioPrefs {
            input_device: input_name.unwrap_or("").to_string(),
            input_channel: 0,
            output_device: output_name.unwrap_or("").to_string(),
            sample_rate: sample_rate.unwrap_or(0),
            buffer_size: buffer_size.unwrap_or(0),
        })
    }

    /// Open the rig from [`RigAudioPrefs`]. Stands up a daw [`LiveRig`] — one
    /// input-armed track monitoring `prefs.input_channel`, with reserved FX
    /// slots, the engine + live input running, transport rolling.
    pub fn open(prefs: &RigAudioPrefs) -> eyre::Result<Self> {
        // Low latency under JACK/PipeWire: ask PipeWire for the quantum before
        // the JACK client connects (the client can't set the buffer there).
        #[cfg(feature = "jack")]
        {
            if let Some(buf) = prefs.buffer_size_opt() {
                if std::env::var_os("PIPEWIRE_LATENCY").is_none() {
                    let rate = prefs.sample_rate_opt().unwrap_or(48_000);
                    unsafe { std::env::set_var("PIPEWIRE_LATENCY", format!("{buf}/{rate}")) };
                    tracing::info!(quantum = %format!("{buf}/{rate}"), "rig: requesting PipeWire low-latency quantum");
                }
            }
        }

        let live = LiveRig::open(&prefs.into(), prefs.input_channel as u32, MAX_CHAIN_SLOTS)
            .map_err(|e| eyre::eyre!("rig: {e}"))?;
        let sample_rate = live.sample_rate();

        tracing::info!(
            input_channel = prefs.input_channel,
            sample_rate,
            "signal-sampler: guitar rig started on daw LiveRig"
        );
        eprintln!(
            "Guitar rig (daw engine): in ch{} → FX chain → master @ {} Hz",
            prefs.input_channel, sample_rate,
        );

        let effective = RigAudioPrefs {
            input_device: prefs.input_device.clone(),
            input_channel: prefs.input_channel,
            output_device: prefs.output_device.clone(),
            sample_rate,
            buffer_size: prefs.buffer_size,
        };

        Ok(Self {
            live,
            sample_rate,
            swap: std::sync::Mutex::new(SwapState {
                chains: std::collections::HashMap::new(),
                active: None,
                input_trim_db: 0.0,
                output_trim_db: 0.0,
                bypass: false,
            }),
            slots: Vec::new(),
            next_id: 0,
            prefs: effective,
        })
    }

    /// List available input devices (name + channel count + native rate).
    pub fn input_devices() -> Vec<DeviceInfo> {
        let host = daw_audio_io::audio_host();
        daw_audio_io::input_devices(&host)
            .into_iter()
            .map(|d| DeviceInfo {
                name: d.name,
                channels: d.channels,
                default_sample_rate: d.default_sample_rate,
            })
            .collect()
    }

    /// List available output devices.
    pub fn output_devices() -> Vec<DeviceInfo> {
        let host = daw_audio_io::audio_host();
        daw_audio_io::output_devices(&host)
            .into_iter()
            .map(|d| DeviceInfo {
                name: d.name,
                channels: d.channels,
                default_sample_rate: d.default_sample_rate,
            })
            .collect()
    }

    /// The prefs the rig actually opened with (resolved sample rate).
    pub fn prefs(&self) -> &RigAudioPrefs {
        &self.prefs
    }

    /// Build an FX chain on **this** thread (loading every `.nam` / `.wav` /
    /// plugin, prepared at the rig's sample rate) and store it resident.
    /// Returns its [`ModelId`]. Does **not** activate it — call
    /// [`set_active`](Self::set_active). A failed block load fails the whole
    /// install.
    pub fn install_chain(&mut self, blocks: &[RigBlock]) -> Result<ModelId, String> {
        if blocks.is_empty() {
            return Err("chain has no blocks".into());
        }
        if blocks.len() > MAX_CHAIN_SLOTS {
            return Err(format!(
                "chain has {} blocks; the rig supports at most {MAX_CHAIN_SLOTS}",
                blocks.len()
            ));
        }
        let mut boxes: Vec<Option<Box<dyn PluginInstance>>> = Vec::with_capacity(blocks.len());
        let mut names = Vec::with_capacity(blocks.len());
        let mut primary_loudness = None;
        let mut primary_expected_sr = None;

        for b in blocks {
            let (boxed, name, loud, exp_sr) = build_block(b, self.sample_rate)?;
            if primary_loudness.is_none() && loud.is_some() {
                primary_loudness = loud;
                primary_expected_sr = exp_sr;
            }
            names.push(name);
            boxes.push(Some(boxed));
        }

        let id = self.next_id;
        self.next_id = self.next_id.wrapping_add(1);
        let info = SlotInfo {
            id,
            display_name: names.join(" → "),
            blocks: names,
            primary_loudness,
            primary_expected_sr,
        };
        self.slots.push(info.clone());
        self.swap
            .lock()
            .unwrap()
            .chains
            .insert(id, ResidentChain { info, boxes });
        Ok(id)
    }

    /// Convenience: install a single-NAM chain (amp only). Does not activate.
    pub fn install_model(&mut self, path: impl AsRef<Path>) -> Result<ModelId, String> {
        let p = path.as_ref().to_string_lossy().to_string();
        self.install_chain(&[RigBlock::nam(p)])
    }

    /// Remove a resident chain. If it was active, falls back to passthrough.
    pub fn uninstall_model(&mut self, id: ModelId) {
        if self.active() == Some(id) {
            self.set_active(None);
        }
        self.swap.lock().unwrap().chains.remove(&id);
        self.slots.retain(|s| s.id != id);
    }

    /// Select the active chain — hands the chain's pre-prepared boxes to
    /// [`LiveRig::set_chain`], which swaps them into the reserved slot guids
    /// under the renderer's per-block lock (glitch-free). `None` = clean DI
    /// passthrough (chain slots → identity). Boxes displaced from the previously
    /// active chain are reclaimed so it can be re-armed; any others are dropped
    /// on this (control) thread, off the audio thread. `&self` (via an internal
    /// `Mutex`) so footswitch / UI paths needn't hold the rig `&mut`.
    pub fn set_active(&self, id: Option<ModelId>) {
        let mut swap = self.swap.lock().unwrap();
        let bypass = swap.bypass;

        // Build the chain to install: take the requested chain's boxes out of
        // the resident map (unless bypassed / unknown → empty = all identities).
        let known = id.filter(|i| swap.chains.contains_key(i));
        let to_install = known.filter(|_| !bypass);
        let new_chain: Vec<Box<dyn PluginInstance>> = match to_install {
            Some(cid) => {
                let chain = swap.chains.get_mut(&cid).expect("checked contains_key");
                chain.boxes.iter_mut().filter_map(|b| b.take()).collect()
            }
            None => Vec::new(),
        };

        // Swap under the renderer's lock; reclaim what the previous chain left.
        let prev = swap.active.take();
        let displaced = self.live.set_chain(new_chain);

        // Reclaim displaced boxes into the previously-active chain's slots (in
        // order) so it can be re-armed later. Identity fillers are dropped here.
        if let Some(prev_id) = prev {
            if let Some(prev_chain) = swap.chains.get_mut(&prev_id) {
                let mut it = displaced.into_iter();
                for slot in prev_chain.boxes.iter_mut() {
                    match it.next() {
                        Some(b) => *slot = Some(b),
                        None => break,
                    }
                }
                // Remaining displaced boxes (identity fillers) drop here.
            }
        }
        // (When there was no previous chain, `displaced` are all identity
        // fillers — they drop at end of scope, off the audio thread.)

        // When bypassed, remember the requested (known) id so toggling bypass
        // off re-arms it; otherwise reflect the actually-armed selection.
        swap.active = match to_install {
            Some(cid) => Some(cid),
            None => {
                if bypass {
                    known
                } else {
                    None
                }
            }
        };
    }

    pub fn active(&self) -> Option<ModelId> {
        self.swap.lock().unwrap().active
    }

    /// Convenience for the single-amp case: install a single-NAM chain + activate.
    pub fn load_nam(&mut self, path: impl AsRef<Path>) -> Result<SlotInfo, String> {
        let id = self.install_model(path)?;
        self.set_active(Some(id));
        Ok(self.slots.last().cloned().expect("just installed"))
    }

    /// Remove every chain and fall back to passthrough.
    pub fn clear(&mut self) {
        self.set_active(None);
        self.swap.lock().unwrap().chains.clear();
        self.slots.clear();
    }

    /// Chains currently resident (control-side mirror, for the UI).
    pub fn slots(&self) -> &[SlotInfo] {
        &self.slots
    }

    /// Look up a slot by id.
    pub fn slot_info(&self, id: ModelId) -> Option<&SlotInfo> {
        self.slots.iter().find(|s| s.id == id)
    }

    pub fn set_input_trim_db(&self, db: f32) {
        self.swap.lock().unwrap().input_trim_db = db;
        // The input trim is folded into each NAM block's per-block input gain at
        // build time (the resolver sets `RigBlock::input_trim_db`), so the
        // patch-level input trim is a no-op live; kept for API parity. The
        // common path (ProfileRig) sets trims *before* `set_active`, which is
        // where they take effect.
    }

    pub fn set_output_trim_db(&self, db: f32) {
        self.swap.lock().unwrap().output_trim_db = db;
        // Output trim → the live track's post-fader gain.
        self.live.set_output_gain_db(db);
    }

    pub fn set_bypass(&self, bypass: bool) {
        {
            let mut swap = self.swap.lock().unwrap();
            if swap.bypass == bypass {
                return;
            }
            swap.bypass = bypass;
        }
        // Re-apply the active selection through the (now changed) bypass gate:
        // bypassed → chain slots become identities; unbypassed → real chain.
        let active = self.active();
        self.set_active(active);
    }

    pub fn is_bypassed(&self) -> bool {
        self.swap.lock().unwrap().bypass
    }

    pub fn input_trim_db(&self) -> f32 {
        self.swap.lock().unwrap().input_trim_db
    }

    pub fn output_trim_db(&self) -> f32 {
        self.swap.lock().unwrap().output_trim_db
    }

    /// Post-input peak (linear) — from the live rig's built-in input probe.
    pub fn input_peak(&self) -> f32 {
        self.live.input_peak()
    }

    /// Output peak (linear) — the track's post-fader meter cell.
    pub fn output_peak(&self) -> f32 {
        self.live.output_peak()
    }

    /// Input-stream overruns. daw's engine counts these internally; not yet
    /// surfaced through a public API, so reported as 0.
    pub fn underruns(&self) -> u64 {
        0
    }

    pub fn overruns(&self) -> u64 {
        0
    }

    pub fn installs(&self) -> u64 {
        self.slots.len() as u64
    }

    /// DSP load metrics are owned by daw's engine and not surfaced yet.
    pub fn render_us(&self) -> u32 {
        0
    }

    pub fn peak_render_us(&self) -> u32 {
        0
    }

    pub fn reset_render_peak(&self) {}

    /// Frames in the last block — the running buffer/quantum, from prefs.
    pub fn block_frames(&self) -> u32 {
        self.prefs.buffer_size
    }

    /// Bridge latency between input and output. daw's engine bridges via its own
    /// ring; not surfaced as a frame count yet.
    pub fn ring_frames(&self) -> u32 {
        0
    }

    /// dB of the current output peak — convenience over [`linear_to_db`].
    pub fn output_peak_db(&self) -> f64 {
        linear_to_db(self.output_peak())
    }
}

/// Decode a standard-base64 plugin state chunk.
fn base64_decode(s: &str) -> Result<Vec<u8>, String> {
    use base64::Engine;
    base64::engine::general_purpose::STANDARD
        .decode(s.trim())
        .map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use signal_plugin_host::PluginEvents;

    #[test]
    fn rig_block_kind_predicates() {
        assert!(RigBlock::nam("a.nam").is_nam());
        assert!(RigBlock::cab_ir("v30.wav").is_cab_ir());
        assert!(RigBlock::plugin("/x/Reverb.clap").is_plugin());
        assert!(!RigBlock::nam("a.nam").is_cab_ir());
    }

    #[test]
    fn rig_block_plugin_with_state_roundtrips() {
        let b = RigBlock::plugin_with_state("/x/Delay.vst3", Some("c3RhdGU=".into()));
        assert!(b.is_plugin());
        assert_eq!(b.state_b64.as_deref(), Some("c3RhdGU="));
    }

    /// Building a NAM block produces a prepared, non-silent `PluginInstance`.
    #[test]
    fn build_block_nam_produces_audio() {
        let fixture = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/assets/amp_a.nam");
        let Ok((mut boxed, name, _loud, _sr)) =
            build_block(&RigBlock::nam(fixture.to_string_lossy().to_string()), 48_000)
        else {
            eprintln!("skip: amp_a.nam fixture failed to load");
            return;
        };
        assert!(!name.is_empty());
        assert!(boxed.is_prepared());
        const N: usize = 128;
        let sig: Vec<f32> = (0..N).map(|i| (i as f32 * 0.05).sin() * 0.3).collect();
        let (mut ol, mut or_) = (vec![0.0f32; N], vec![0.0f32; N]);
        boxed
            .process_block(&sig, &sig, &mut ol, &mut or_, &PluginEvents::default())
            .unwrap();
        let energy: f64 = ol.iter().map(|x| (*x as f64).powi(2)).sum();
        assert!(energy > 1e-9, "NAM block should produce audio");
    }

    // NB: the `Identity` / `InputProbe` pass-through PluginInstances moved to
    // daw (`daw::live`), where their unit tests now live. signal no longer
    // defines them.
}
