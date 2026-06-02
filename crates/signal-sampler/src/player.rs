//! SamplerPlayer — owns the cpal output stream and drives a SamplerBank.

use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Instant;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{BufferSize, StreamConfig};
use crossbeam_channel::{Receiver, Sender, bounded};

use crate::{
    InstrumentId, SamplerBank,
    bank::PreloadProfile,
    engine::cache::{EvictStats, PreloadStats},
};

fn output_device_name(device: &cpal::Device) -> String {
    device
        .description()
        .map(|description| description.name().to_string())
        .unwrap_or_default()
}

/// Live sample player with cpal audio output.
///
/// Cheap to clone — all clones share the same underlying audio stream and bank.
/// Drop the last clone to stop playback.
#[derive(Clone)]
pub struct SamplerPlayer {
    /// Shared bank — clone this arc to drive MIDI from another thread.
    pub bank: Arc<Mutex<SamplerBank>>,
    _stream: Option<Arc<cpal::Stream>>,
    pub sample_rate: u32,
    stats: Arc<AudioStats>,
    clock_start: Arc<Instant>,
    events_tx: Sender<AudioEvent>,
    events_rx: Receiver<AudioEvent>,
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

#[derive(Clone, Debug, Default)]
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
    pub dropped_events: u64,
    pub pending_events: usize,
    pub stolen_voices: usize,
    pub cache_misses: usize,
    pub sample_misses: usize,
    pub loaded_sample_bytes: usize,
    pub cache_budget_bytes: Option<usize>,
    pub cache_over_budget_bytes: usize,
    pub recent_cache_misses: Vec<String>,
    pub recent_sample_misses: Vec<String>,
    pub resize_events: u64,
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
    dropped_events: AtomicU64,
    resize_events: AtomicU64,
}

impl AudioStats {
    fn snapshot(
        &self,
        pending_events: usize,
        stolen_voices: usize,
        cache_misses: usize,
        sample_misses: usize,
        loaded_sample_bytes: usize,
        cache_budget_bytes: Option<usize>,
        cache_over_budget_bytes: usize,
        recent_cache_misses: Vec<String>,
        recent_sample_misses: Vec<String>,
    ) -> AudioStatsSnapshot {
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
            dropped_events: self.dropped_events.load(Ordering::Relaxed),
            pending_events,
            stolen_voices,
            cache_misses,
            sample_misses,
            loaded_sample_bytes,
            cache_budget_bytes,
            cache_over_budget_bytes,
            recent_cache_misses,
            recent_sample_misses,
            resize_events: self.resize_events.load(Ordering::Relaxed),
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
            &self.dropped_events,
            &self.resize_events,
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
    /// Sample rate is taken from the device's default config — passing
    /// `None` uses the device's native rate (typically 44.1 or 48 kHz),
    /// which avoids the silent-stream pitfall when the host's audio
    /// server (PipeWire / CoreAudio) refuses rate-mismatched streams.
    pub fn with_device(device_name: Option<&str>) -> eyre::Result<Self> {
        Self::with_device_config(device_name, None, Some(256))
    }

    /// Create a player with an optional explicit sample rate. `None` =
    /// use the device's native rate (recommended). `buffer_size = None`
    /// uses the backend's default buffer.
    pub fn with_device_config(
        device_name: Option<&str>,
        sample_rate: Option<u32>,
        buffer_size: Option<u32>,
    ) -> eyre::Result<Self> {
        Self::with_device_config_and_cache_budget(device_name, sample_rate, buffer_size, None)
    }

    /// Create a player with an optional decoded-sample cache budget.
    ///
    /// The current implementation reports budget pressure in
    /// [`AudioStatsSnapshot`]; eviction is intentionally separate so the
    /// real-time callback never loses sample data unexpectedly.
    pub fn with_device_config_and_cache_budget(
        device_name: Option<&str>,
        sample_rate: Option<u32>,
        buffer_size: Option<u32>,
        cache_budget_bytes: Option<usize>,
    ) -> eyre::Result<Self> {
        let host = cpal::default_host();

        let device = match device_name {
            Some(name) => host
                .output_devices()?
                .find(|d| output_device_name(d).contains(name))
                .ok_or_else(|| eyre::eyre!("audio device not found: {name}"))?,
            None => host
                .default_output_device()
                .ok_or_else(|| eyre::eyre!("no default audio output device"))?,
        };

        let default_config = device.default_output_config()?;
        let default_channels = default_config.channels();
        let channels = default_channels.min(2).max(1);
        let channels_usize = channels as usize;
        // Honor the device's native sample rate by default — avoids the
        // silent-stream failure mode when the audio server rejects a
        // requested-but-unsupported rate.
        let sample_rate = sample_rate.unwrap_or_else(|| default_config.sample_rate());
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
            output_device_name(&device),
            default_config.sample_rate(),
            default_channels,
            config.buffer_size,
        );
        eprintln!(
            "Audio output: device={:?}, sample_rate={sample_rate}, channels={channels}, buffer={:?}, expected_buffer_ms={expected_latency_ms:.2}",
            output_device_name(&device),
            config.buffer_size,
        );

        let bank = Arc::new(Mutex::new(SamplerBank::with_cache_budget(
            sample_rate,
            cache_budget_bytes,
        )));
        let bank_audio = Arc::clone(&bank);
        let (events_tx, events_rx) = bounded::<AudioEvent>(4096);
        let events_rx_audio = events_rx.clone();
        let stats = Arc::new(AudioStats::default());
        let clock_start = Arc::new(Instant::now());
        stats
            .buffer_budget_us
            .store(buffer_budget_us, Ordering::Relaxed);
        let stats_audio = Arc::clone(&stats);
        let stats_errors = Arc::clone(&stats);
        let clock_audio = Arc::clone(&clock_start);
        let mut channel_scratch = buffer_size
            .map(|frames| vec![0.0; frames as usize * 2])
            .unwrap_or_default();

        let stream = device.build_output_stream(
            &config,
            move |data: &mut [f32], _info: &cpal::OutputCallbackInfo| {
                render_block(
                    data,
                    channels_usize,
                    &bank_audio,
                    &events_rx_audio,
                    &stats_audio,
                    &clock_audio,
                    &mut channel_scratch,
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
            _stream: Some(Arc::new(stream)),
            sample_rate,
            stats,
            clock_start,
            events_tx,
            events_rx,
        })
    }

    /// Create a player that does not open an audio device.
    ///
    /// This is intended for integration tests and offline probes: use the
    /// normal load/MIDI APIs, then call [`render_offline`](Self::render_offline)
    /// to render into an interleaved stereo buffer.
    pub fn new_offline(sample_rate: u32) -> Self {
        Self::new_offline_with_cache_budget(sample_rate, None)
    }

    pub fn new_offline_with_cache_budget(
        sample_rate: u32,
        cache_budget_bytes: Option<usize>,
    ) -> Self {
        let (events_tx, events_rx) = bounded::<AudioEvent>(4096);
        Self {
            bank: Arc::new(Mutex::new(SamplerBank::with_cache_budget(
                sample_rate,
                cache_budget_bytes,
            ))),
            _stream: None,
            sample_rate,
            stats: Arc::new(AudioStats::default()),
            clock_start: Arc::new(Instant::now()),
            events_tx,
            events_rx,
        }
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
            .map_err(|_| eyre::eyre!("sampler bank lock poisoned"))?
            .load_instrument(id, spec_path, samples_root, section, mic)
    }

    /// Load a `.signalpack` directly.
    ///
    /// The pack's embedded styx supplies the [`crate::LibrarySpec`]; samples
    /// decode straight from the pack body — no on-disk audio required.
    /// This is the recommended entry point for zone-mapped libraries and
    /// (in the simplified groove mode) Stylus RMX-style loops.
    pub fn load_pack(&self, id: impl Into<InstrumentId>, pack_path: &Path) -> eyre::Result<()> {
        self.bank
            .lock()
            .map_err(|_| eyre::eyre!("sampler bank lock poisoned"))?
            .load_pack(id, pack_path)
    }

    /// Load a `.signalblock` file. The block references one `.signalpack`
    /// plus block-level params (gain, pan, transpose). Background preload
    /// streams in immediately.
    pub fn load_block(&self, id: impl Into<InstrumentId>, block_path: &Path) -> eyre::Result<()> {
        self.bank
            .lock()
            .map_err(|_| eyre::eyre!("sampler bank lock poisoned"))?
            .load_block(id, block_path)
    }

    /// Load a `.signalengine` — one playable Engine instance (one source
    /// pack + per-mic Layers + voice config). Background preload streams
    /// in middle-out priority.
    pub fn load_engine(&self, id: impl Into<InstrumentId>, engine_path: &Path) -> eyre::Result<()> {
        let spec = crate::engine_spec::EngineSpec::from_file(engine_path)?;
        let dir = engine_path.parent().unwrap_or(std::path::Path::new(""));
        self.bank
            .lock()
            .map_err(|_| eyre::eyre!("sampler bank lock poisoned"))?
            .load_engine_spec(id, &spec, dir)
    }

    /// Load a `.signalpreset` — full kit/rig with N Engine instances,
    /// note routing, optional Modules + master FX (latter parse-only).
    /// Returns the per-engine instrument ids registered under
    /// `<id_prefix>:<engine_id>`.
    pub fn load_preset(
        &self,
        id_prefix: &str,
        preset_path: &Path,
    ) -> eyre::Result<Vec<InstrumentId>> {
        let preset = crate::preset_spec::PresetSpec::from_file(preset_path)?;
        let dir = preset_path.parent().unwrap_or(std::path::Path::new(""));
        self.bank
            .lock()
            .map_err(|_| eyre::eyre!("sampler bank lock poisoned"))?
            .load_preset_spec(id_prefix, &preset, dir)
    }

    pub fn unload_instrument(&self, id: &str) {
        self.clear_pending_events();
        match self.bank.lock() {
            Ok(mut bank) => bank.unload_instrument(id),
            Err(_) => tracing::warn!("signal-sampler: sampler bank lock poisoned; unload skipped"),
        }
    }

    pub fn set_midi_channel(&self, id: impl Into<InstrumentId>, channel: u8) {
        match self.bank.lock() {
            Ok(mut bank) => bank.set_midi_channel(id, channel),
            Err(_) => {
                tracing::warn!("signal-sampler: sampler bank lock poisoned; MIDI channel skipped")
            }
        }
    }

    pub fn set_muted(&self, id: &str, muted: bool) {
        match self.bank.lock() {
            Ok(mut bank) => bank.set_muted(id, muted),
            Err(_) => tracing::warn!("signal-sampler: sampler bank lock poisoned; mute skipped"),
        }
    }

    pub fn preload_instrument(&self, id: &str) -> eyre::Result<PreloadStats> {
        self.bank
            .lock()
            .map_err(|_| eyre::eyre!("sampler bank lock poisoned"))?
            .preload_instrument(id)
    }

    pub fn set_preload_profile(&self, profile: PreloadProfile) {
        match self.bank.lock() {
            Ok(mut bank) => bank.set_preload_profile(profile),
            Err(_) => {
                tracing::warn!(
                    "signal-sampler: sampler bank lock poisoned; preload profile skipped"
                )
            }
        }
    }

    // ── Direct MIDI (convenience pass-throughs) ──────────────────────────────

    pub fn note_on(&self, id: &str, note: u8, velocity: u8) {
        // Just enqueue. Synchronous warm-decode used to live here, but it
        // held `bank.lock()` through a ~15 ms FLAC decode and starved the
        // audio thread (try_lock failed → output filled with zeros → clicks
        // and underruns on every key press). The bank's background preload
        // brings every sample online without blocking the audio path; until
        // a sample is decoded, the audio thread simply doesn't spawn that
        // voice and the user hears a brief silence on the first press.
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

    pub fn all_notes_off(&self, id: &str) {
        self.clear_pending_events();
        match self.bank.lock() {
            Ok(mut bank) => bank.all_notes_off(id),
            Err(_) => {
                tracing::warn!("signal-sampler: sampler bank lock poisoned; all-notes-off skipped")
            }
        }
    }

    pub fn panic(&self, id: &str) {
        self.clear_pending_events();
        match self.bank.lock() {
            Ok(mut bank) => bank.panic(id),
            Err(_) => tracing::warn!("signal-sampler: sampler bank lock poisoned; panic skipped"),
        }
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
        // Same rationale as `note_on`: don't hold `bank.lock()` here
        // (warm_midi_message_samples does synchronous FLAC decode) — it
        // would starve the audio callback. Just enqueue.
        self.enqueue(AudioEvent::Midi {
            channel,
            status,
            data1,
            data2,
        });
    }

    fn enqueue(&self, event: AudioEvent) {
        if self.events_tx.try_send(event).is_err() {
            self.stats.dropped_events.fetch_add(1, Ordering::Relaxed);
            tracing::warn!("signal-sampler: audio event queue full; dropping event");
            return;
        }
        self.stats.midi_messages.fetch_add(1, Ordering::Relaxed);
        self.stats.last_midi_us.store(
            self.clock_start.elapsed().as_micros() as u64,
            Ordering::Relaxed,
        );
    }

    pub fn audio_stats(&self) -> AudioStatsSnapshot {
        let (
            stolen_voices,
            cache_misses,
            sample_misses,
            loaded_sample_bytes,
            cache_budget_bytes,
            cache_over_budget_bytes,
            recent_cache_misses,
            recent_sample_misses,
            bank_resize_events,
        ) = self
            .bank
            .try_lock()
            .map(|bank| {
                (
                    bank.total_stolen_voices(),
                    bank.total_cache_misses(),
                    bank.total_sample_misses(),
                    bank.total_loaded_sample_bytes(),
                    bank.cache_budget_bytes(),
                    bank.cache_over_budget_bytes(),
                    bank.recent_cache_misses(),
                    bank.recent_sample_misses(),
                    bank.resize_events(),
                )
            })
            .unwrap_or((0, 0, 0, 0, None, 0, Vec::new(), Vec::new(), 0));
        let mut snapshot = self.stats.snapshot(
            self.events_rx.len(),
            stolen_voices,
            cache_misses,
            sample_misses,
            loaded_sample_bytes,
            cache_budget_bytes,
            cache_over_budget_bytes,
            recent_cache_misses,
            recent_sample_misses,
        );
        snapshot.resize_events = snapshot.resize_events.saturating_add(bank_resize_events);
        snapshot
    }

    pub fn evict_cache_over_budget(&self) -> EvictStats {
        self.bank
            .try_lock()
            .map(|bank| bank.evict_cache_over_budget())
            .unwrap_or_default()
    }

    pub fn clear_pending_events(&self) -> usize {
        self.events_rx.try_iter().count()
    }

    /// Render one offline stereo block. `output` is interleaved L/R and is
    /// cleared before rendering.
    pub fn render_offline(&self, output: &mut [f32]) -> eyre::Result<()> {
        if self._stream.is_some() {
            return Err(eyre::eyre!(
                "render_offline is only available on SamplerPlayer::new_offline"
            ));
        }
        let mut bank = self
            .bank
            .lock()
            .map_err(|_| eyre::eyre!("sampler bank lock poisoned"))?;
        for event in self.events_rx.try_iter() {
            apply_audio_event(&mut bank, event);
        }
        output.fill(0.0);
        bank.render(output);
        Ok(())
    }

    /// Offline render of a loaded **preset** into per-mic buses, keyed by mic
    /// id (interleaved stereo each, `block_frames` frames). Sums each piece's
    /// mics onto shared buses ("Overhead", "Room Close", …) per the drum mic
    /// taxonomy — the daw Sampler Block maps these onto track channels for
    /// per-mic routing. `prefix` is the preset id used at `load_preset`.
    pub fn render_offline_buses(
        &self,
        prefix: &str,
        block_frames: usize,
    ) -> eyre::Result<std::collections::BTreeMap<String, Vec<f32>>> {
        if self._stream.is_some() {
            return Err(eyre::eyre!(
                "render_offline_buses is only available on SamplerPlayer::new_offline"
            ));
        }
        let mut bank = self
            .bank
            .lock()
            .map_err(|_| eyre::eyre!("sampler bank lock poisoned"))?;
        for event in self.events_rx.try_iter() {
            apply_audio_event(&mut bank, event);
        }
        bank.render_preset_buses(prefix, block_frames)
            .ok_or_else(|| eyre::eyre!("no preset loaded under '{prefix}'"))
    }

    /// `(loaded, total)` background-preload progress for an instrument, or
    /// `(0, 0)` if not loaded. Cheap; meant for per-frame UI updates.
    pub fn preload_progress(&self, id: &str) -> (usize, usize) {
        // try_lock — never block the calling thread (typically UI redraw)
        // on the audio thread's brief render-block lock. Returning a
        // stale snapshot is fine; the UI polls again on its next frame.
        self.bank
            .try_lock()
            .map(|bank| bank.preload_progress(id))
            .unwrap_or((0, 0))
    }

    pub fn active_voices(&self, id: &str) -> usize {
        self.bank
            .try_lock()
            .map(|bank| bank.active_voices(id))
            .unwrap_or(0)
    }

    pub fn stolen_voices(&self, id: &str) -> usize {
        self.bank
            .try_lock()
            .map(|bank| bank.stolen_voices(id))
            .unwrap_or(0)
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
    channel_scratch: &mut Vec<f32>,
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
        let scratch_len = frames * 2;
        if channel_scratch.len() != scratch_len {
            channel_scratch.resize(scratch_len, 0.0);
            stats.resize_events.fetch_add(1, Ordering::Relaxed);
            tracing::warn!(
                frames,
                channels,
                "signal-sampler: resized non-stereo output scratch"
            );
        }
        channel_scratch.fill(0.0);
        b.render(channel_scratch);
        for (frame, out) in data.chunks_mut(channels).enumerate() {
            let l = channel_scratch[frame * 2];
            let r = channel_scratch[frame * 2 + 1];
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

#[cfg(test)]
mod tests {
    use super::SamplerPlayer;

    #[test]
    fn offline_player_renders_without_audio_device() {
        let player = SamplerPlayer::new_offline(48_000);
        player.note_on("missing", 60, 100);

        let mut block = vec![1.0; 128 * 2];
        player.render_offline(&mut block).expect("offline render");

        assert!(block.iter().all(|sample| *sample == 0.0));
        assert_eq!(player.audio_stats().midi_messages, 1);
    }

    #[test]
    fn all_notes_off_drains_pending_audio_events() {
        let player = SamplerPlayer::new_offline(48_000);
        player.note_on("missing", 60, 100);
        assert_eq!(player.audio_stats().pending_events, 1);

        player.all_notes_off("missing");

        assert_eq!(player.audio_stats().pending_events, 0);
        assert_eq!(player.audio_stats().midi_messages, 1);
    }
}
