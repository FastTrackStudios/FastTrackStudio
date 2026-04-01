//! SamplerPlayer — owns the cpal output stream and drives a SamplerBank.

use std::path::Path;
use std::sync::{Arc, Mutex};

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

use crate::{InstrumentId, SamplerBank};

/// Live sample player with cpal audio output.
///
/// Owns the audio stream — drop to stop playback.
///
/// The `SamplerBank` inside is `Arc<Mutex<_>>` so callers can hold a clone
/// for MIDI routing from another thread (e.g. a midir callback).
pub struct SamplerPlayer {
    /// Shared bank — clone this arc to drive MIDI from another thread.
    pub bank: Arc<Mutex<SamplerBank>>,
    _stream: cpal::Stream,
    pub sample_rate: u32,
}

impl SamplerPlayer {
    /// Create a player using the system default output device.
    pub fn new() -> eyre::Result<Self> {
        Self::with_device(None)
    }

    /// Create a player with a specific audio device (substring name match).
    pub fn with_device(device_name: Option<&str>) -> eyre::Result<Self> {
        let host = cpal::default_host();

        let device = match device_name {
            Some(name) => host
                .output_devices()?
                .find(|d| d.name().map_or(false, |n| n.contains(name)))
                .ok_or_else(|| eyre::eyre!("audio device not found: {name}"))?,
            None => host
                .default_output_device()
                .ok_or_else(|| eyre::eyre!("no default audio output device"))?,
        };

        let config = device.default_output_config()?;
        let sample_rate = config.sample_rate();
        let channels = config.channels() as usize;

        tracing::info!(
            "signal-sampler: opening output — device={:?}, sr={sample_rate}, ch={channels}",
            device.name().unwrap_or_default(),
        );

        let bank = Arc::new(Mutex::new(SamplerBank::new(sample_rate)));
        let bank_audio = Arc::clone(&bank);

        let stream = device.build_output_stream(
            &config.into(),
            move |data: &mut [f32], _info: &cpal::OutputCallbackInfo| {
                render_block(data, channels, &bank_audio);
            },
            |err| tracing::warn!("signal-sampler: audio stream error: {err}"),
            None,
        )?;

        stream.play()?;
        tracing::info!("signal-sampler: stream started at {sample_rate} Hz");

        Ok(Self { bank, _stream: stream, sample_rate })
    }

    // ── Instrument management (convenience pass-throughs) ────────────────────

    pub fn load_instrument(
        &self,
        id: impl Into<InstrumentId>,
        spec_path: &Path,
        samples_root: Option<&Path>,
        section: impl Into<String>,
        mic: impl Into<String>,
    ) -> eyre::Result<()> {
        self.bank.lock().unwrap()
            .load_instrument(id, spec_path, samples_root, section, mic)
    }

    pub fn unload_instrument(&self, id: &str) {
        self.bank.lock().unwrap().unload_instrument(id);
    }

    pub fn set_midi_channel(&self, id: impl Into<InstrumentId>, channel: u8) {
        self.bank.lock().unwrap().set_midi_channel(id, channel);
    }

    pub fn set_muted(&self, id: &str, muted: bool) {
        self.bank.lock().unwrap().set_muted(id, muted);
    }

    // ── Direct MIDI (convenience pass-throughs) ──────────────────────────────

    pub fn note_on(&self, id: &str, note: u8, velocity: u8) {
        self.bank.lock().unwrap().note_on(id, note, velocity);
    }

    pub fn note_off(&self, id: &str, note: u8) {
        self.bank.lock().unwrap().note_off(id, note);
    }

    pub fn cc(&self, id: &str, controller: u8, value: u8) {
        self.bank.lock().unwrap().cc(id, controller, value);
    }

    /// Dispatch a raw MIDI message, routed by channel assignment.
    pub fn midi_message(&self, channel: u8, status: u8, data1: u8, data2: u8) {
        self.bank.lock().unwrap().midi_message(channel, status, data1, data2);
    }
}

// ── Audio callback ────────────────────────────────────────────────────────────

fn render_block(data: &mut [f32], channels: usize, bank: &Mutex<SamplerBank>) {
    let mut b = match bank.try_lock() {
        Ok(g) => g,
        Err(_) => {
            // Lock contention on RT thread — output silence this block.
            data.fill(0.0);
            return;
        }
    };

    if channels == 2 {
        data.fill(0.0);
        b.render(data);
    } else {
        let frames = data.len() / channels;
        let mut stereo = vec![0.0f32; frames * 2];
        b.render(&mut stereo);
        for (frame, out) in data.chunks_mut(channels).enumerate() {
            let l = stereo[frame * 2];
            let r = stereo[frame * 2 + 1];
            match out.len() {
                1 => out[0] = (l + r) * 0.5,
                _ => {
                    out[0] = l;
                    out[1] = r;
                    for extra in &mut out[2..] {
                        *extra = 0.0;
                    }
                }
            }
        }
    }
}
