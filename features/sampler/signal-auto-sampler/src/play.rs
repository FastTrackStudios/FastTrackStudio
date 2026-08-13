//! Play a pack live from a MIDI keyboard.
//!
//! The sound now comes from the computer, not the instrument: the Kronos (or
//! any controller) sends notes in, the sampler engine renders the pack, and the
//! audio leaves through whichever interface output is chosen. Useful for
//! judging a fresh pack the way you'd actually play it — loops, velocity
//! layers, and key ranges all get exercised at once.

use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Instant;

use cpal::traits::{DeviceTrait, StreamTrait};
use daw_audio_io::{AudioIoPrefs, audio_host, device_name, open_output};
use eyre::{Result, WrapErr, eyre};
use midicore_midir::{MidiInput, input_ports};
use midicore_proto::{MidiEvent, PortSelector};
use signal_sampler::block::SamplerBlock;

use std::path::Path;

/// Live latency measurements, written by the audio callback and read by the
/// main thread.
///
/// Every field is an atomic and nothing here logs from the callback — a
/// `tracing` call in the audio thread allocates and takes locks, which would
/// itself cause the dropouts we are trying to measure.
#[derive(Default)]
pub struct PlayStats {
    /// Frames the device actually hands us per callback. The requested buffer
    /// size is only a request — CoreAudio may clamp it, and the difference
    /// between asking for 128 and getting 512 is 8 ms of latency.
    pub block_frames: AtomicUsize,
    /// How far ahead of the callback the audio will actually play, in
    /// microseconds — cpal's `playback - callback` timestamp delta. This is the
    /// output latency proper: buffering plus the driver's safety offset.
    pub output_latency_us: AtomicU64,
    /// Worst observed age of a MIDI event when it was applied: the delay
    /// between the event arriving from the OS and the block that acts on it.
    pub midi_age_max_us: AtomicU64,
    /// Total age, for a running mean.
    pub midi_age_sum_us: AtomicU64,
    pub midi_events: AtomicU64,
    pub callbacks: AtomicU64,
    /// Voices sounding, and the engine's ceiling. If these meet, further notes
    /// steal from the ones already playing — which sounds exactly like keys
    /// not responding.
    pub active_voices: AtomicUsize,
    pub max_voices: AtomicUsize,
    /// Cumulative voices stolen. Any non-zero value here while playing normally
    /// means notes are being cut short to make room.
    pub stolen_voices: AtomicUsize,
    /// Note-ons the engine was asked to play, so a missing note can be traced
    /// to either "MIDI never arrived" or "MIDI arrived and no voice resulted".
    pub note_ons: AtomicU64,
}

impl PlayStats {
    fn observe_max(field: &AtomicU64, value: u64) {
        let _ = field.fetch_update(Ordering::AcqRel, Ordering::Acquire, |cur| {
            (value > cur).then_some(value)
        });
    }

    /// Mean MIDI application delay in milliseconds.
    pub fn midi_age_mean_ms(&self) -> f64 {
        let n = self.midi_events.load(Ordering::Acquire);
        if n == 0 {
            return 0.0;
        }
        self.midi_age_sum_us.load(Ordering::Acquire) as f64 / n as f64 / 1000.0
    }

    /// Everything between a key going down and its sound leaving the interface,
    /// as far as we can observe it: how long a MIDI event waits to be applied,
    /// plus how far ahead the audio is scheduled.
    pub fn estimated_total_ms(&self, sample_rate: u32) -> f64 {
        let block = self.block_frames.load(Ordering::Acquire) as f64;
        let block_ms = block * 1000.0 / sample_rate as f64;
        let out_ms = self.output_latency_us.load(Ordering::Acquire) as f64 / 1000.0;
        // A key pressed at a uniformly random moment waits half a block on
        // average before the next callback picks it up.
        block_ms / 2.0 + out_ms
    }
}

/// Where the notes come from and the sound goes.
#[derive(Debug, Clone)]
pub struct PlayRoute {
    /// MIDI input port substring (e.g. `KRONOS`). Empty takes the default.
    pub midi_port: String,
    /// Only respond to this channel, 1-16. `None` listens on all channels.
    pub midi_channel: Option<u8>,
    /// Output device substring. Empty takes the system default.
    pub output_device: String,
    /// 1-based output channel for the left signal.
    pub left_output: u16,
    /// 1-based output channel for the right signal.
    pub right_output: u16,
    pub sample_rate: u32,
    /// Requested buffer size in frames. Smaller is lower latency.
    pub buffer_size: u32,
}

/// Load `pack` and play it from the keyboard until `stop` is signalled.
///
/// Blocks; the audio and MIDI threads do the work.
pub fn run(pack: &Path, route: &PlayRoute, stop: impl Fn() -> bool) -> Result<()> {
    let host = audio_host();
    let prefs = AudioIoPrefs {
        output_device: route.output_device.clone(),
        sample_rate: route.sample_rate,
        buffer_size: route.buffer_size,
        ..Default::default()
    };
    let out = open_output(&host, &prefs).map_err(|e| eyre!("open output device: {e}"))?;
    let out_name = device_name(&out.device);
    let channels = out.channels as usize;

    let left = route.left_output.saturating_sub(1) as usize;
    let right = route.right_output.saturating_sub(1) as usize;
    if left >= channels || right >= channels {
        return Err(eyre!(
            "device '{out_name}' opened {channels} channel(s), but outputs {}/{} \
             were requested",
            route.left_output,
            route.right_output,
        ));
    }

    // The engine must render at the rate the device actually opened at, not the
    // rate we asked for — a device that refused 48k would otherwise be fed
    // samples pitched wrong.
    let sample_rate = out.sample_rate;
    let mut block = SamplerBlock::from_pack(pack, sample_rate)
        .map_err(|e| eyre!("load pack {}: {e}", pack.display()))?;

    // Decode every zone BEFORE the stream starts.
    //
    // This is not an optimisation, it is a correctness fix. The engine drops a
    // note whose zone is not yet cached — it returns without spawning a voice
    // rather than blocking — so playing a freshly-opened pack silently loses
    // notes until the background preload catches up. The notes that do sound
    // arrive late, because the FLAC decode happens inside the audio callback
    // and overruns its deadline.
    //
    // Paying for it up front costs a few seconds of load and makes every key
    // respond identically from the first press.
    let stats = block.preload_samples();
    tracing::info!(?stats, "preloaded pack — every zone resident");

    // Queue between midir's callback thread and the audio callback. Both ends
    // are short critical sections over a handful of events per block. Each
    // event carries its arrival time so the audio thread can measure how long
    // it waited.
    let queue: Arc<Mutex<VecDeque<(Instant, MidiEvent)>>> = Arc::new(Mutex::new(VecDeque::new()));
    let stats = Arc::new(PlayStats::default());

    let selector = if route.midi_port.is_empty() {
        PortSelector::Default
    } else {
        PortSelector::NameContains(route.midi_port.clone())
    };
    let sink_queue = Arc::clone(&queue);
    let want_channel = route.midi_channel;
    let _midi = MidiInput::open(selector, move |timed| {
        // Filter on the MIDI thread so the audio callback only ever sees
        // events it will act on.
        if let Some(ch) = want_channel {
            let event_channel = match &timed.event {
                MidiEvent::NoteOn { channel, .. }
                | MidiEvent::NoteOff { channel, .. }
                | MidiEvent::ControlChange { channel, .. } => Some(channel.number()),
                _ => None,
            };
            if let Some(c) = event_channel
                && c != ch
            {
                return;
            }
        }
        if let Ok(mut q) = sink_queue.lock() {
            q.push_back((Instant::now(), timed.event.clone()));
        }
    })
    .wrap_err_with(|| {
        let available = input_ports();
        if available.is_empty() {
            format!("no MIDI input ports found (looking for {:?})", route.midi_port)
        } else {
            format!(
                "no MIDI input matching {:?} — available: {}",
                route.midi_port,
                available.join(", ")
            )
        }
    })?;

    tracing::info!(
        pack = %pack.display(),
        output = %out_name,
        channels,
        sample_rate,
        midi = %if route.midi_port.is_empty() { "<default>".into() } else { route.midi_port.clone() },
        "play: ready"
    );

    // Interleaved stereo scratch the engine renders into, then scattered onto
    // the device's channel pair.
    let mut scratch: Vec<f32> = Vec::new();
    let render_queue = Arc::clone(&queue);
    let cb_stats = Arc::clone(&stats);

    let stream = out
        .device
        .build_output_stream(
            out.config.clone(),
            move |data: &mut [f32], info: &cpal::OutputCallbackInfo| {
                data.fill(0.0);
                let frames = data.len() / channels;
                let now = Instant::now();

                // What the device is really giving us, versus what we asked
                // for. These are the numbers that explain perceived latency.
                cb_stats.block_frames.store(frames, Ordering::Release);
                cb_stats.callbacks.fetch_add(1, Ordering::AcqRel);
                let ts = info.timestamp();
                // Saturating: a driver whose playback stamp is not strictly
                // ahead of the callback reports 0 rather than wrapping.
                let ahead = ts.playback.duration_since(ts.callback);
                cb_stats
                    .output_latency_us
                    .store(ahead.as_micros() as u64, Ordering::Release);

                // Apply everything that arrived since the last block. Timing
                // within the block is not preserved — at typical buffer sizes
                // that is well under the threshold of noticeability, and it
                // keeps the callback allocation-free.
                if let Ok(mut q) = render_queue.try_lock() {
                    for (arrived, event) in q.drain(..) {
                        let age_us = now.saturating_duration_since(arrived).as_micros() as u64;
                        cb_stats.midi_events.fetch_add(1, Ordering::AcqRel);
                        cb_stats.midi_age_sum_us.fetch_add(age_us, Ordering::AcqRel);
                        PlayStats::observe_max(&cb_stats.midi_age_max_us, age_us);
                        match event {
                            MidiEvent::NoteOn { key, velocity, .. } => {
                                // Running-status keyboards send note-on with
                                // velocity 0 for note-off.
                                if velocity.get() == 0 {
                                    block.note_off(key.get());
                                } else {
                                    cb_stats.note_ons.fetch_add(1, Ordering::AcqRel);
                                    block.note_on(key.get(), velocity.get());
                                }
                            }
                            MidiEvent::NoteOff { key, .. } => block.note_off(key.get()),
                            MidiEvent::ControlChange {
                                controller, value, ..
                            } => block.cc_line(Default::default(), controller.get(), value.get()),
                            _ => {}
                        }
                    }
                }

                if scratch.len() != frames * 2 {
                    scratch.resize(frames * 2, 0.0);
                }
                scratch.fill(0.0);
                block.render(&mut scratch);

                cb_stats
                    .active_voices
                    .store(block.active_voices(), Ordering::Release);
                cb_stats
                    .max_voices
                    .store(block.max_voices(), Ordering::Release);
                cb_stats
                    .stolen_voices
                    .store(block.stolen_voices(), Ordering::Release);

                for f in 0..frames {
                    data[f * channels + left] = scratch[f * 2];
                    data[f * channels + right] = scratch[f * 2 + 1];
                }
            },
            |e| tracing::error!("playback stream error: {e}"),
            None,
        )
        .map_err(|e| eyre!("build output stream: {e}"))?;

    stream.play().map_err(|e| eyre!("start playback: {e}"))?;

    // Let a few callbacks land so the numbers below are real measurements
    // rather than zeros.
    std::thread::sleep(std::time::Duration::from_millis(200));
    let granted = stats.block_frames.load(Ordering::Acquire);
    let out_us = stats.output_latency_us.load(Ordering::Acquire);
    println!(
        "buffer: asked {} frames, device gave {granted} ({:.2} ms at {sample_rate} Hz)",
        route.buffer_size,
        granted as f64 * 1000.0 / sample_rate as f64,
    );
    if granted > route.buffer_size as usize {
        println!(
            "  NOTE: the device refused the requested buffer — that alone is \
             {:.1} ms of extra latency",
            (granted as f64 - route.buffer_size as f64) * 1000.0 / sample_rate as f64
        );
    }
    println!(
        "output latency: {:.2} ms (driver reports audio playing this far after the callback)",
        out_us as f64 / 1000.0
    );
    println!(
        "estimated key-to-sound: {:.2} ms\n",
        stats.estimated_total_ms(sample_rate)
    );

    let mut last_report = Instant::now();
    while !stop() {
        std::thread::sleep(std::time::Duration::from_millis(100));

        // A live line while playing: MIDI delay only becomes measurable once
        // keys are actually pressed.
        if last_report.elapsed() >= std::time::Duration::from_secs(2) {
            last_report = Instant::now();
            let n = stats.midi_events.load(Ordering::Acquire);
            if n > 0 {
                let stolen = stats.stolen_voices.load(Ordering::Acquire);
                println!(
                    "  midi {n} ({} note-on) | apply delay mean {:.2} ms worst {:.2} ms | \
                     voices {}/{}{} | total ~{:.2} ms",
                    stats.note_ons.load(Ordering::Acquire),
                    stats.midi_age_mean_ms(),
                    stats.midi_age_max_us.load(Ordering::Acquire) as f64 / 1000.0,
                    stats.active_voices.load(Ordering::Acquire),
                    stats.max_voices.load(Ordering::Acquire),
                    if stolen > 0 {
                        format!(" STOLEN {stolen}")
                    } else {
                        String::new()
                    },
                    stats.estimated_total_ms(sample_rate),
                );
            }
        }
    }
    Ok(())
}
