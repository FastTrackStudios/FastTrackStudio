//! `signal-daw` — bridges signal's sampler into the daw engine as a **Block**.
//!
//! A Block is daw's internal plugin kind (an effect or generator hosted by the
//! same `PluginInstance` path as CLAP/VST3). [`SamplerBlock`] is a **generator
//! Block**: it wraps signal-sampler's offline engine, takes MIDI in (its own
//! note routing / articulation / choke live inside the sampler), and renders
//! audio out — so a kit piece can live on a daw track and be mixed/routed/
//! automated by the DAW.
//!
//! Call [`register`] once at startup; thereafter `Effects::add(.., "block:sampler")`
//! installs a `SamplerBlock` on a track. See `docs/plan/signal-on-daw.md`.

use std::path::PathBuf;

use daw_proto::fx::{Effects, FxChainContext};
use daw_proto::project::ProjectContext;
use daw_proto::{MidiMessage, ProjectInfo, Tracks};
use daw_standalone::plugin::{
    PluginDescriptor, PluginError, PluginEvents, PluginFormat, PluginInstance, PluginParamInfo,
};
use daw_standalone::sync::Standalone;
use signal_sampler::SamplerPlayer;

/// Instrument slot inside the wrapped offline player. MIDI + the rendered
/// output both key off this single id (the Block hosts one piece).
const SLOT: &str = "block";

/// Register signal's Blocks with the daw host. Idempotent; call at startup.
/// After this, `Effects::add(.., "block:sampler")` constructs a [`SamplerBlock`].
pub fn register() {
    daw_standalone::blocks::register_block("sampler", || Box::new(SamplerBlock::new()));
}

/// A built kit session: the daw project guid + the track hosting the kit.
#[derive(Debug, Clone)]
pub struct KitSession {
    pub project_guid: String,
    pub kit_track_guid: String,
}

/// Build a minimal daw session that plays signal's sampler: one project, one
/// "Kit" track, with a `block:sampler` Block on it (loads the default preset;
/// override via `SIGNAL_DEFAULT_PRESET`). MIDI sent to the track drives the
/// sampler; the daw renderer mixes it to master.
///
/// This is the P4 seam — per-piece tracks + OH/Room bus tracks + channel-mapped
/// sends layer on top once the Sampler Block emits per-mic outputs (P2).
/// Registers signal's Blocks first, so this is the only call a host needs.
pub fn build_kit_session(daw: &Standalone, name: &str) -> Option<KitSession> {
    register();
    let project_guid = daw.seed_project(ProjectInfo {
        guid: format!("signal-kit-{name}"),
        name: name.to_string(),
        path: String::new(),
    });
    let ctx = ProjectContext::Project(project_guid.clone());
    let kit_track_guid = Tracks::add(daw, ctx.clone(), name, None).ok()?;
    Effects::add(
        daw,
        ctx,
        FxChainContext::Track(kit_track_guid.clone()),
        "block:sampler",
    )?;
    Some(KitSession {
        project_guid,
        kit_track_guid,
    })
}

/// Default preset to load (override with `SIGNAL_DEFAULT_PRESET`). `None` if
/// the path doesn't exist, in which case the Block loads nothing until told.
fn default_preset() -> Option<PathBuf> {
    let p = std::env::var("SIGNAL_DEFAULT_PRESET")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            PathBuf::from(
                "/run/media/AudioHaven/Signal/Libraries/Drum Kits/\
                 GGD Modern and Massive 2/Presets/Metal Monster.signalpreset",
            )
        });
    p.exists().then_some(p)
}

/// A signal sampler hosted as a daw generator Block.
pub struct SamplerBlock {
    player: SamplerPlayer,
    prepared: bool,
    preset_path: Option<PathBuf>,
    /// Interleaved stereo render scratch (frames * 2). Resized on block-size
    /// change, never inside the hot path once stable.
    scratch: Vec<f32>,
    /// Ordered per-mic bus names (sorted mic ids) discovered at `prepare`.
    /// Each bus → one stereo channel pair on a multichannel track, so
    /// `output_channels() == 2 * bus_order.len()`. Empty until prepared.
    bus_order: Vec<String>,
}

impl SamplerBlock {
    pub fn new() -> Self {
        Self {
            // Offline player (no cpal stream); daw owns audio output. The real
            // sample rate is applied in `prepare`.
            player: SamplerPlayer::new_offline(48_000),
            prepared: false,
            preset_path: default_preset(),
            scratch: Vec::new(),
            bus_order: Vec::new(),
        }
    }

    /// Per-mic bus names (sorted), in channel-pair order. Channel `2*i` / `2*i+1`
    /// carry bus `bus_order()[i]`. Hosts use this to label/route track channels.
    pub fn bus_order(&self) -> &[String] {
        &self.bus_order
    }

    /// Deliver this block's MIDI events to the wrapped sampler.
    fn feed_midi(&self, events: &PluginEvents<'_>) {
        for ev in events.midi {
            match ev.message {
                MidiMessage::NoteOn { note, velocity, .. } if velocity > 0 => {
                    self.player.note_on(SLOT, note, velocity)
                }
                MidiMessage::NoteOn { note, .. } | MidiMessage::NoteOff { note, .. } => {
                    self.player.note_off(SLOT, note)
                }
                _ => {}
            }
        }
    }
}

impl Default for SamplerBlock {
    fn default() -> Self {
        Self::new()
    }
}

impl PluginInstance for SamplerBlock {
    fn descriptor(&self) -> PluginDescriptor {
        PluginDescriptor {
            id: "block:sampler".into(),
            name: "Signal Sampler".into(),
            vendor: "FastTrackStudio".into(),
            version: "1".into(),
            format: PluginFormat::Block,
        }
    }

    fn params(&mut self) -> Vec<PluginParamInfo> {
        Vec::new()
    }

    fn param_value(&mut self, _id: u32) -> Option<f64> {
        None
    }

    fn value_to_text(&mut self, _id: u32, _value: f64) -> Option<String> {
        None
    }

    fn text_to_value(&mut self, _id: u32, _text: &str) -> Option<f64> {
        None
    }

    fn latency(&mut self) -> u32 {
        0
    }

    fn prepare(&mut self, sample_rate: f64, _block_size: u32) -> Result<(), PluginError> {
        // Rebuild the offline player at the host's sample rate, then load the
        // preset so its engines + note routing are live.
        self.player = SamplerPlayer::new_offline(sample_rate.max(1.0) as u32);
        if let Some(p) = &self.preset_path {
            if let Err(e) = self.player.load_preset(SLOT, p) {
                tracing::warn!(err = %e, "SamplerBlock: preset load failed");
            }
        }
        // Discover the per-mic bus names (one-frame render — scratches exist
        // for every pack mic even with no audio yet), so output_channels is
        // known. Falls back to stereo (no buses) on error.
        self.bus_order = self
            .player
            .render_offline_buses(SLOT, 1)
            .map(|m| m.into_keys().collect())
            .unwrap_or_default();
        self.prepared = true;
        Ok(())
    }

    fn is_prepared(&self) -> bool {
        self.prepared
    }

    fn process_block(
        &mut self,
        _in_l: &[f32],
        _in_r: &[f32],
        out_l: &mut [f32],
        out_r: &mut [f32],
        events: &PluginEvents<'_>,
    ) -> Result<(), PluginError> {
        if !self.prepared {
            return Err(PluginError::NotActivated);
        }
        // Deliver MIDI to the sampler; it owns note routing / articulation /
        // choke / one-shot internally.
        self.feed_midi(events);

        let frames = out_l.len().min(out_r.len());
        if self.scratch.len() != frames * 2 {
            self.scratch.resize(frames * 2, 0.0);
        }
        for s in self.scratch.iter_mut() {
            *s = 0.0;
        }
        // Offline render fills the interleaved scratch; de-interleave to the
        // host's L/R. (Per-mic multichannel output lands with P2.)
        if self.player.render_offline(&mut self.scratch).is_ok() {
            for f in 0..frames {
                out_l[f] = self.scratch[f * 2];
                out_r[f] = self.scratch[f * 2 + 1];
            }
        }
        Ok(())
    }

    fn output_channels(&self) -> usize {
        (self.bus_order.len() * 2).max(2)
    }

    /// Multichannel render: each per-mic bus → one stereo channel pair, in
    /// `bus_order` (sorted mic id). The host track should have
    /// `2 * bus_order().len()` channels; channel-mapped sends then route each
    /// bus (Overhead, Room Close, …) to its mixer bus track.
    fn process_block_planar(
        &mut self,
        _inputs: &[&[f32]],
        outputs: &mut [&mut [f32]],
        events: &PluginEvents<'_>,
    ) -> Result<(), PluginError> {
        if !self.prepared {
            return Err(PluginError::NotActivated);
        }
        self.feed_midi(events);
        let frames = outputs.first().map_or(0, |o| o.len());
        for o in outputs.iter_mut() {
            for s in o.iter_mut() {
                *s = 0.0;
            }
        }
        let buses = self
            .player
            .render_offline_buses(SLOT, frames)
            .unwrap_or_default();
        for (i, name) in self.bus_order.iter().enumerate() {
            let (cl, cr) = (2 * i, 2 * i + 1);
            if cr >= outputs.len() {
                break;
            }
            if let Some(buf) = buses.get(name) {
                for f in 0..frames {
                    outputs[cl][f] = buf[f * 2];
                    outputs[cr][f] = buf[f * 2 + 1];
                }
            }
        }
        Ok(())
    }

    fn deactivate(&mut self) {
        self.prepared = false;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sampler_block_registers() {
        register();
        assert!(daw_standalone::blocks::make_block("block:sampler").is_some());
    }

    #[test]
    fn build_kit_session_creates_track_with_sampler_block() {
        let daw = Standalone::new();
        let kit = build_kit_session(&daw, "Test Kit").expect("build kit session");
        let ctx = ProjectContext::Project(kit.project_guid.clone());

        let tracks = Tracks::all(&daw, ctx.clone());
        assert!(
            tracks.iter().any(|t| t.guid == kit.kit_track_guid),
            "kit track should exist"
        );
        let fx = Effects::list(&daw, ctx, FxChainContext::Track(kit.kit_track_guid.clone()));
        assert!(
            fx.iter()
                .any(|f| f.plugin_name == "Signal Sampler" || f.name == "block:sampler"),
            "sampler Block should be on the kit track, got: {fx:?}"
        );
    }
}
