//! The audio side: one long-lived input stream, armed and disarmed around each
//! note.
//!
//! The stream stays open for the whole run rather than being rebuilt per note.
//! Opening a CoreAudio stream costs tens of milliseconds and can renegotiate the
//! device's rate — doing that between every note would both slow the run and
//! risk a different sample rate partway through a pack.

use std::sync::atomic::{AtomicBool, AtomicU32, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use cpal::traits::{DeviceTrait, StreamTrait};
use daw_audio_io::{AudioIoPrefs, audio_host, device_name, open_input};
use eyre::{Result, eyre};

use crate::config::AudioRoute;

/// A recorded stereo take, deinterleaved.
#[derive(Debug, Default, Clone)]
pub struct Take {
    pub left: Vec<f32>,
    pub right: Vec<f32>,
}

impl Take {
    pub fn frames(&self) -> usize {
        self.left.len().min(self.right.len())
    }

    pub fn is_empty(&self) -> bool {
        self.frames() == 0
    }

    /// Largest absolute sample across both channels.
    pub fn peak(&self) -> f32 {
        self.left
            .iter()
            .chain(self.right.iter())
            .fold(0.0f32, |m, s| m.max(s.abs()))
    }

    /// Drop the first `frames` frames — used to remove the MIDI-to-audio
    /// round-trip delay measured during calibration.
    pub fn trim_start(&mut self, frames: usize) {
        let frames = frames.min(self.frames());
        self.left.drain(..frames);
        self.right.drain(..frames);
    }

    /// Keep at most `frames` frames.
    pub fn truncate(&mut self, frames: usize) {
        self.left.truncate(frames);
        self.right.truncate(frames);
    }

    /// Drop the trailing silence, keeping `guard` frames of it.
    ///
    /// The guard matters: cutting the instant the level crosses the threshold
    /// clips the very end of a decay, which is audible as an abrupt stop. A
    /// short run-out lets [`fade_out`](Self::fade_out) land on real silence.
    pub fn trim_end(&mut self, threshold: f32, guard: usize) {
        let last_loud = (0..self.frames())
            .rev()
            .find(|&i| self.left[i].abs() >= threshold || self.right[i].abs() >= threshold);
        let keep = match last_loud {
            Some(i) => (i + 1 + guard).min(self.frames()),
            // Nothing ever crossed the threshold — leave it for the caller to
            // reject rather than silently producing a zero-length sample.
            None => return,
        };
        self.truncate(keep);
    }

    /// Ramp the last `frames` frames down to zero.
    ///
    /// Any sample cut before its decay finished ends on a non-zero value, and a
    /// step to zero is a click. This costs a few milliseconds of tail and makes
    /// the truncation inaudible.
    pub fn fade_out(&mut self, frames: usize) {
        let total = self.frames();
        let n = frames.min(total);
        if n == 0 {
            return;
        }
        // Ramp across `n - 1` steps so the ramp starts at exactly 1.0 (no step
        // where the fade begins) and lands on exactly 0.0 at the final frame.
        // Dividing by `n` instead would leave the last frame at 1/n — still a
        // step to silence, which is the click this exists to prevent.
        let last = n.saturating_sub(1);
        for i in 0..n {
            let gain = if last == 0 {
                0.0
            } else {
                (last - i) as f32 / last as f32
            };
            self.left[total - n + i] *= gain;
            self.right[total - n + i] *= gain;
        }
    }
}

/// Shared between the audio callback and the sampling loop.
///
/// `armed` and `overruns` are atomics rather than fields inside the mutex so the
/// callback can decide whether it cares *before* trying to lock, and can record
/// a dropped block even when it can't get the lock. The mutex itself is only
/// contended between takes (the consumer locks while the instrument is silent),
/// so the callback's `try_lock` succeeds in practice — but a failure must be
/// counted, not swallowed, or a corrupt sample would reach the pack.
#[derive(Default)]
struct Shared {
    armed: AtomicBool,
    overruns: AtomicUsize,
    take: Mutex<Take>,
    /// Loudest sample seen since the last read, as `f32::to_bits`.
    ///
    /// Deliberately an atomic rather than something read off `take`: the
    /// sampling loop polls the level every few milliseconds to decide when a
    /// note has decayed, and locking the buffer that often would collide with
    /// the callback's `try_lock` and register phantom overruns. The callback
    /// updates this once per block, not per sample.
    window_peak: AtomicU32,
}

impl Shared {
    fn observe_peak(&self, peak: f32) {
        let bits = peak.to_bits();
        let _ = self
            .window_peak
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |cur| {
                (f32::from_bits(cur) < peak).then_some(bits)
            });
    }
}

/// An open capture stream tapping one stereo pair of an input device.
pub struct Capture {
    /// Held to keep the stream alive; dropping it stops capture.
    _stream: cpal::Stream,
    shared: Arc<Shared>,
    pub sample_rate: u32,
    pub device_name: String,
    /// Total channels the device stream opened.
    pub channels: usize,
}

impl Capture {
    /// Open `route`'s device and start the stream running (disarmed).
    pub fn open(route: &AudioRoute) -> Result<Self> {
        let host = audio_host();
        let prefs = AudioIoPrefs {
            input_device: route.device.clone(),
            sample_rate: route.sample_rate,
            want_input: true,
            ..Default::default()
        };

        // `open_input` knows the rule that matters here: on CoreAudio a pro
        // interface must have its *full* channel count opened before a high
        // channel index (like input 5) is reachable in the interleaved stream.
        let opened = open_input(&host, &prefs, route.sample_rate, route.max_index())
            .map_err(|e| eyre!("open input device: {e}"))?;

        let device_name = device_name(&opened.device);
        let channels = opened.channels;
        let left = route.left_index();
        let right = route.right_index();

        if left >= channels || right >= channels {
            return Err(eyre!(
                "device '{device_name}' opened {channels} channel(s), but inputs \
                 {}/{} were requested — the interface may not expose that pair",
                route.left_input,
                route.right_input,
            ));
        }

        let shared = Arc::new(Shared::default());
        let sample_format = opened
            .device
            .default_input_config()
            .map_err(|e| eyre!("query input config for '{device_name}': {e}"))?
            .sample_format();

        let stream = build_stream(
            &opened.device,
            &opened.config,
            sample_format,
            channels,
            left,
            right,
            Arc::clone(&shared),
        )?;
        stream
            .play()
            .map_err(|e| eyre!("start capture stream: {e}"))?;

        Ok(Self {
            _stream: stream,
            shared,
            sample_rate: opened.config.sample_rate,
            device_name,
            channels,
        })
    }

    /// Start accumulating, discarding anything captured earlier.
    pub fn arm(&self) {
        {
            let mut take = self.shared.take.lock().expect("capture buffer poisoned");
            *take = Take::default();
        }
        self.shared.overruns.store(0, Ordering::Release);
        self.shared.window_peak.store(0, Ordering::Release);
        self.shared.armed.store(true, Ordering::Release);
    }

    /// Loudest sample since this was last called, then reset.
    ///
    /// Polling this on an interval gives the peak level over that interval —
    /// which is how the sampling loop decides a note has finished decaying,
    /// without ever locking the capture buffer.
    pub fn take_window_peak(&self) -> f32 {
        f32::from_bits(self.shared.window_peak.swap(0, Ordering::AcqRel))
    }

    /// Stop accumulating and take what was recorded.
    pub fn finish(&self) -> Result<Take> {
        self.shared.armed.store(false, Ordering::Release);
        let dropped = self.shared.overruns.load(Ordering::Acquire);
        if dropped > 0 {
            return Err(eyre!(
                "capture dropped {dropped} frame(s) — the take is incomplete"
            ));
        }
        let mut take = self.shared.take.lock().expect("capture buffer poisoned");
        Ok(std::mem::take(&mut take))
    }

    /// Record `frames` worth of whatever is currently at the inputs — used to
    /// measure the noise floor before sampling starts.
    pub fn record_frames(&self, frames: usize) -> Result<Take> {
        self.arm();
        let secs = frames as f64 / self.sample_rate as f64;
        std::thread::sleep(std::time::Duration::from_secs_f64(secs));
        self.finish()
    }
}

/// Build the input stream for whichever sample format the device speaks.
fn build_stream(
    device: &cpal::Device,
    config: &cpal::StreamConfig,
    format: cpal::SampleFormat,
    channels: usize,
    left: usize,
    right: usize,
    shared: Arc<Shared>,
) -> Result<cpal::Stream> {
    let on_error = |e| tracing::error!("capture stream error: {e}");

    macro_rules! stream {
        ($sample:ty, $to_f32:expr) => {{
            let shared = Arc::clone(&shared);
            device.build_input_stream(
                config.clone(),
                move |data: &[$sample], _: &cpal::InputCallbackInfo| {
                    if !shared.armed.load(Ordering::Acquire) {
                        return;
                    }
                    let frames = data.len() / channels;
                    let Ok(mut take) = shared.take.try_lock() else {
                        shared.overruns.fetch_add(frames, Ordering::AcqRel);
                        return;
                    };
                    let to_f32 = $to_f32;
                    let mut block_peak = 0.0f32;
                    for frame in data.chunks_exact(channels) {
                        let (l, r) = (to_f32(frame[left]), to_f32(frame[right]));
                        block_peak = block_peak.max(l.abs()).max(r.abs());
                        take.left.push(l);
                        take.right.push(r);
                    }
                    // One atomic update per block, not per sample.
                    shared.observe_peak(block_peak);
                },
                on_error,
                None,
            )
        }};
    }

    let stream = match format {
        cpal::SampleFormat::F32 => stream!(f32, |v: f32| v),
        cpal::SampleFormat::I32 => stream!(i32, |v: i32| v as f32 / i32::MAX as f32),
        cpal::SampleFormat::I16 => stream!(i16, |v: i16| v as f32 / i16::MAX as f32),
        other => {
            return Err(eyre!(
                "unsupported capture sample format {other:?} — expected f32, i32, or i16"
            ));
        }
    };

    stream.map_err(|e| eyre!("build capture stream: {e}"))
}
