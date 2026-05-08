//! SamplerPlayer — owns the cpal output stream and drives a SamplerBank.

use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Instant;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{BufferSize, StreamConfig};
use crossbeam_channel::{Receiver, Sender, bounded};

use crate::{InstrumentId, SamplerBank, engine::cache::PreloadStats};

/// Live sample player with cpal audio output.
///
/// Cheap to clone — all clones share the same underlying audio stream and bank.
/// Drop the last clone to stop playback.
#[derive(Clone)]
pub struct SamplerPlayer {
    /// Shared bank — clone this arc to drive MIDI from another thread.
    pub bank: Arc<Mutex<SamplerBank>>,
    _stream: Arc<cpal::Stream>,
    pub sample_rate: u32,
    stats: Arc<AudioStats>,
    clock_start: Arc<Instant>,
    events_tx: Sender<AudioEvent>,
}

#[derive(Debug, Clone)]
enum AudioEvent {
    NoteOn {
        id: InstrumentId,
        note: u8,
        velocity: u8,
    },
    NoteOff {
        id: InstrumentId,
        note: u8,
    },
    NoteOffVelocity {
        id: InstrumentId,
        note: u8,
        velocity: u8,
    },
    Cc {
        id: InstrumentId,
        controller: u8,
        value: u8,
    },
    Midi {
        channel: u8,
        status: u8,
        data1: u8,
        data2: u8,
    },
}

#[derive(Clone, Copy, Debug, Default)]
pub struct AudioStatsSnapshot {
    pub stream_errors: u64,
    pub callback_overruns: u64,
    pub lock_misses: u64,
    pub callbacks: u64,
    pub max_render_us: u64,
    pub last_render_us: u64,
    pub buffer_budget_us: u64,
    pub midi_messages: u64,
    pub last_midi_to_callback_us: u64,
    pub max_midi_to_callback_us: u64,
    pub last_callback_interval_us: u64,
    pub max_callback_interval_us: u64,
}

#[derive(Default)]
struct AudioStats {
    stream_errors: AtomicU64,
    callback_overruns: AtomicU64,
    lock_misses: AtomicU64,
    callbacks: AtomicU64,
    max_render_us: AtomicU64,
    last_render_us: AtomicU64,
    buffer_budget_us: AtomicU64,
    midi_messages: AtomicU64,
    last_midi_us: AtomicU64,
    last_observed_midi_us: AtomicU64,
    last_midi_to_callback_us: AtomicU64,
    max_midi_to_callback_us: AtomicU64,
    previous_callback_us: AtomicU64,
    last_callback_interval_us: AtomicU64,
    max_callback_interval_us: AtomicU64,
}

impl AudioStats {
    fn snapshot(&self) -> AudioStatsSnapshot {
        AudioStatsSnapshot {
            stream_errors: self.stream_errors.load(Ordering::Relaxed),
            callback_overruns: self.callback_overruns.load(Ordering::Relaxed),
            lock_misses: self.lock_misses.load(Ordering::Relaxed),
            callbacks: self.callbacks.load(Ordering::Relaxed),
            max_render_us: self.max_render_us.load(Ordering::Relaxed),
            last_render_us: self.last_render_us.load(Ordering::Relaxed),
            buffer_budget_us: self.buffer_budget_us.load(Ordering::Relaxed),
            midi_messages: self.midi_messages.load(Ordering::Relaxed),
            last_midi_to_callback_us: self.last_midi_to_callback_us.load(Ordering::Relaxed),
            max_midi_to_callback_us: self.max_midi_to_callback_us.load(Ordering::Relaxed),
            last_callback_interval_us: self.last_callback_interval_us.load(Ordering::Relaxed),
            max_callback_interval_us: self.max_callback_interval_us.load(Ordering::Relaxed),
        }
    }

    fn reset(&self) {
        for c in [
            &self.stream_errors,
            &self.callback_overruns,
            &self.lock_misses,
            &self.callbacks,
            &self.max_render_us,
            &self.last_render_us,
            &self.midi_messages,
            &self.last_midi_to_callback_us,
            &self.max_midi_to_callback_us,
            &self.previous_callback_us,
            &self.last_callback_interval_us,
            &self.max_callback_interval_us,
        ] {
            c.store(0, Ordering::Relaxed);
        }
    }

    fn update_max_render_us(&self, value: u64) {
        update_atomic_max(&self.max_render_us, value);
    }

    fn update_max_midi_to_callback_us(&self, value: u64) {
        update_atomic_max(&self.max_midi_to_callback_us, value);
    }

    fn update_max_callback_interval_us(&self, value: u64) {
        update_atomic_max(&self.max_callback_interval_us, value);
    }
}

fn update_atomic_max(target: &AtomicU64, value: u64) {
    let mut current = target.load(Ordering::Relaxed);
    while value > current {
        match target.compare_exchange_weak(current, value, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(_) => break,
            Err(next) => current = next,
        }
    }
}

impl SamplerPlayer {
    /// Create a player using the system default output device.
    pub fn new() -> eyre::Result<Self> {
        Self::with_device(None)
    }

    /// Create a player with a specific audio device (substring name match).
    pub fn with_device(device_name: Option<&str>) -> eyre::Result<Self> {
        Self::with_device_config(device_name, 48_000, Some(256))
    }

    /// Create a player with an explicit sample rate and optional fixed buffer size.
    pub fn with_device_config(
        device_name: Option<&str>,
        sample_rate: u32,
        buffer_size: Option<u32>,
    ) -> eyre::Result<Self> {
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

        let default_config = device.default_output_config()?;
        let default_channels = default_config.channels();
        let channels = default_channels.min(2).max(1);
        let channels_usize = channels as usize;
        let config = StreamConfig {
            channels,
            sample_rate,
            buffer_size: buffer_size
                .map(BufferSize::Fixed)
                .unwrap_or(BufferSize::Default),
        };
        let expected_latency_ms = buffer_size
            .map(|frames| frames as f64 * 1000.0 / sample_rate as f64)
            .unwrap_or_default();
        let buffer_budget_us = buffer_size
            .map(|frames| frames as u64 * 1_000_000 / sample_rate as u64)
            .unwrap_or(0);

        tracing::info!(
            "signal-sampler: opening output — device={:?}, requested_sr={sample_rate}, requested_ch={channels}, default_sr={}, default_ch={}, buffer={:?}, expected_buffer_ms={expected_latency_ms:.2}",
            device.name().unwrap_or_default(),
            default_config.sample_rate(),
            default_channels,
            config.buffer_size,
        );
        eprintln!(
            "Audio output: device={:?}, sample_rate={sample_rate}, channels={channels}, buffer={:?}, expected_buffer_ms={expected_latency_ms:.2}",
            device.name().unwrap_or_default(),
            config.buffer_size,
        );

        let bank = Arc::new(Mutex::new(SamplerBank::new(sample_rate)));
        let bank_audio = Arc::clone(&bank);
        let (events_tx, events_rx) = bounded::<AudioEvent>(4096);
        let stats = Arc::new(AudioStats::default());
        let clock_start = Arc::new(Instant::now());
        stats
            .buffer_budget_us
            .store(buffer_budget_us, Ordering::Relaxed);
        let stats_audio = Arc::clone(&stats);
        let stats_errors = Arc::clone(&stats);
        let clock_audio = Arc::clone(&clock_start);

        let stream = device.build_output_stream(
            &config,
            move |data: &mut [f32], _info: &cpal::OutputCallbackInfo| {
                render_block(
                    data,
                    channels_usize,
                    &bank_audio,
                    &events_rx,
                    &stats_audio,
                    &clock_audio,
                );
            },
            move |err| {
                stats_errors.stream_errors.fetch_add(1, Ordering::Relaxed);
                tracing::warn!("signal-sampler: audio stream error: {err}");
            },
            None,
        )?;

        stream.play()?;
        tracing::info!("signal-sampler: stream started at {sample_rate} Hz");

        Ok(Self {
            bank,
            _stream: Arc::new(stream),
            sample_rate,
            stats,
            clock_start,
            events_tx,
        })
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
        self.bank
            .lock()
            .unwrap()
            .load_instrument(id, spec_path, samples_root, section, mic)
    }

    /// Load a `.signalpack` directly.
    ///
    /// The pack's embedded styx supplies the [`crate::LibrarySpec`]; samples
    /// decode straight from the pack body — no on-disk audio required.
    /// This is the recommended entry point for zone-mapped libraries and
    /// (in the simplified groove mode) Stylus RMX-style loops.
    pub fn load_pack(&self, id: impl Into<InstrumentId>, pack_path: &Path) -> eyre::Result<()> {
        self.bank.lock().unwrap().load_pack(id, pack_path)
    }

    /// Load a `.signalblock` file. The block references one `.signalpack`
    /// plus block-level params (gain, pan, transpose). Background preload
    /// streams in immediately.
    pub fn load_block(&self, id: impl Into<InstrumentId>, block_path: &Path) -> eyre::Result<()> {
        self.bank.lock().unwrap().load_block(id, block_path)
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

    pub fn preload_instrument(&self, id: &str) -> eyre::Result<PreloadStats> {
        self.bank.lock().unwrap().preload_instrument(id)
    }

    // ── Direct MIDI (convenience pass-throughs) ──────────────────────────────

    pub fn note_on(&self, id: &str, note: u8, velocity: u8) {
        self.enqueue(AudioEvent::NoteOn {
            id: id.into(),
            note,
            velocity,
        });
    }

    pub fn note_off(&self, id: &str, note: u8) {
        self.enqueue(AudioEvent::NoteOff {
            id: id.into(),
            note,
        });
    }

    pub fn note_off_with_velocity(&self, id: &str, note: u8, velocity: u8) {
        self.enqueue(AudioEvent::NoteOffVelocity {
            id: id.into(),
            note,
            velocity,
        });
    }

    pub fn cc(&self, id: &str, controller: u8, value: u8) {
        self.enqueue(AudioEvent::Cc {
            id: id.into(),
            controller,
            value,
        });
    }

    /// Dispatch a raw MIDI message, routed by channel assignment.
    pub fn midi_message(&self, channel: u8, status: u8, data1: u8, data2: u8) {
        self.enqueue(AudioEvent::Midi {
            channel,
            status,
            data1,
            data2,
        });
    }

    fn enqueue(&self, event: AudioEvent) {
        if self.events_tx.try_send(event).is_err() {
            tracing::warn!("signal-sampler: audio event queue full; dropping event");
        }
        self.stats.midi_messages.fetch_add(1, Ordering::Relaxed);
        self.stats.last_midi_us.store(
            self.clock_start.elapsed().as_micros() as u64,
            Ordering::Relaxed,
        );
    }

    pub fn audio_stats(&self) -> AudioStatsSnapshot {
        self.stats.snapshot()
    }

    /// `(loaded, total)` background-preload progress for an instrument, or
    /// `(0, 0)` if not loaded. Cheap; meant for per-frame UI updates.
    pub fn preload_progress(&self, id: &str) -> (usize, usize) {
        self.bank.lock().unwrap().preload_progress(id)
    }

    /// Reset all rolling audio counters (callbacks, render-time peaks, lock
    /// misses, stream errors, …) to zero. Intended for tests and for
    /// clearing the lock-contention noise that's expected during the brief
    /// window after `load_pack` while preload is decoding.
    pub fn reset_audio_stats(&self) {
        self.stats.reset();
    }
}

// ── Audio callback ────────────────────────────────────────────────────────────

fn render_block(
    data: &mut [f32],
    channels: usize,
    bank: &Mutex<SamplerBank>,
    events_rx: &Receiver<AudioEvent>,
    stats: &AudioStats,
    clock_start: &Instant,
) {
    let start = Instant::now();
    let callback_us = clock_start.elapsed().as_micros() as u64;
    let previous_callback_us = stats
        .previous_callback_us
        .swap(callback_us, Ordering::Relaxed);
    if previous_callback_us > 0 {
        let interval_us = callback_us.saturating_sub(previous_callback_us);
        stats
            .last_callback_interval_us
            .store(interval_us, Ordering::Relaxed);
        stats.update_max_callback_interval_us(interval_us);
    }

    let last_midi_us = stats.last_midi_us.load(Ordering::Relaxed);
    let observed_midi_us = stats.last_observed_midi_us.load(Ordering::Relaxed);
    if last_midi_us > observed_midi_us {
        stats
            .last_observed_midi_us
            .store(last_midi_us, Ordering::Relaxed);
        let midi_to_callback_us = callback_us.saturating_sub(last_midi_us);
        stats
            .last_midi_to_callback_us
            .store(midi_to_callback_us, Ordering::Relaxed);
        stats.update_max_midi_to_callback_us(midi_to_callback_us);
    }

    stats.callbacks.fetch_add(1, Ordering::Relaxed);

    let mut b = match bank.try_lock() {
        Ok(g) => g,
        Err(_) => {
            stats.lock_misses.fetch_add(1, Ordering::Relaxed);
            data.fill(0.0);
            return;
        }
    };
    for event in events_rx.try_iter() {
        apply_audio_event(&mut b, event);
    }

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

    let render_us = start.elapsed().as_micros() as u64;
    stats.last_render_us.store(render_us, Ordering::Relaxed);
    stats.update_max_render_us(render_us);
    let budget_us = stats.buffer_budget_us.load(Ordering::Relaxed);
    if budget_us > 0 && render_us > budget_us {
        stats.callback_overruns.fetch_add(1, Ordering::Relaxed);
    }
}

fn apply_audio_event(bank: &mut SamplerBank, event: AudioEvent) {
    match event {
        AudioEvent::NoteOn { id, note, velocity } => bank.note_on(&id, note, velocity),
        AudioEvent::NoteOff { id, note } => bank.note_off(&id, note),
        AudioEvent::NoteOffVelocity { id, note, velocity } => {
            bank.note_off_with_velocity(&id, note, velocity)
        }
        AudioEvent::Cc {
            id,
            controller,
            value,
        } => bank.cc(&id, controller, value),
        AudioEvent::Midi {
            channel,
            status,
            data1,
            data2,
        } => bank.midi_message(channel, status, data1, data2),
    }
}
