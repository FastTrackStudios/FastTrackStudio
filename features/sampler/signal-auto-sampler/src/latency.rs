//! Measuring the MIDI-to-audio round trip, once, before sampling starts.
//!
//! Between `note_on` returning and the first sample of that note reaching the
//! capture buffer sits: the OS MIDI stack, the DIN/USB hop, the instrument's own
//! note-on latency, its converters, the interface, and one capture buffer. That
//! total is easily tens of milliseconds and is *not* knowable in advance.
//!
//! Left uncorrected it becomes a variable slug of silence at the head of every
//! sample, which is audible as a sloppy, uneven attack across the keyboard.
//!
//! We measure it once with a deliberately percussive strike, then trim every
//! subsequent take by the same amount. Per-sample onset detection would seem
//! more precise but is actively worse: a slow pad has no detectable transient,
//! so detection would fail exactly where the note is quietest and trim a
//! different amount for every note.

use std::time::Duration;

use eyre::{Result, eyre};

use crate::capture::{Capture, Take};
use crate::config::Timing;
use crate::midi::Instrument;

/// How far above the measured noise floor a sample must rise to count as the
/// note starting. 4× (about 12 dB) clears converter hiss and hum without
/// waiting for a soft attack to reach full level.
const ONSET_FACTOR: f32 = 4.0;

/// Absolute floor for the onset threshold, for interfaces quiet enough that
/// `noise_floor * ONSET_FACTOR` would trigger on dither.
const MIN_ONSET_THRESHOLD: f32 = 1.0e-4;

/// The measured round trip.
#[derive(Debug, Clone, Copy)]
pub struct Latency {
    /// Frames between note-on and first audible sample.
    pub frames: usize,
    /// The noise floor measured with the instrument silent (peak, linear).
    pub noise_floor: f32,
    /// The threshold that was used to detect the onset.
    pub threshold: f32,
}

impl Latency {
    pub fn millis(&self, sample_rate: u32) -> f64 {
        self.frames as f64 * 1000.0 / sample_rate as f64
    }
}

/// Above this, "silence" is not silence — something is making sound. -40 dBFS
/// is far louder than any converter's noise but well below a played note.
const NOISE_FLOOR_SANITY: f32 = 0.01;

/// How many separate windows the floor is measured over.
const NOISE_FLOOR_WINDOWS: usize = 5;

/// Peak level with nothing playing.
///
/// Measured as the **quietest** of several short windows rather than one long
/// one. A single window is only valid if the instrument happens to be silent
/// for its whole duration — a note still ringing, or someone touching the
/// keyboard, poisons it. And the failure is nasty rather than obvious: an
/// inflated floor raises the onset threshold above anything the calibration
/// note can reach, so the run aborts claiming the instrument is unpatched or
/// muted. Taking the minimum survives intermittent sound.
pub fn measure_noise_floor(capture: &Capture, millis: u32) -> Result<f32> {
    let per_window = (millis as usize / NOISE_FLOOR_WINDOWS).max(1) as u32;
    let frames = (capture.sample_rate as u64 * per_window as u64 / 1000) as usize;

    let mut quietest = f32::INFINITY;
    let mut loudest: f32 = 0.0;
    for _ in 0..NOISE_FLOOR_WINDOWS {
        let take = capture.record_frames(frames)?;
        if take.is_empty() {
            return Err(eyre!(
                "no audio arrived from '{}' — check that the device is streaming \
                 and the requested inputs exist",
                capture.device_name
            ));
        }
        let peak = take.peak();
        quietest = quietest.min(peak);
        loudest = loudest.max(peak);
    }

    if quietest > NOISE_FLOOR_SANITY {
        return Err(eyre!(
            "the instrument is making sound — measured {quietest:.4} during every \
             one of {NOISE_FLOOR_WINDOWS} silent windows (peak {loudest:.4}). Let \
             it go quiet, stop any held notes or arpeggiator, and try again"
        ));
    }
    if loudest > NOISE_FLOOR_SANITY {
        tracing::warn!(
            quietest = format!("{quietest:.6}"),
            loudest = format!("{loudest:.4}"),
            "noise floor: sound during measurement, using the quietest window"
        );
    }
    Ok(quietest)
}

/// First frame whose level reaches `threshold`, in either channel.
pub fn find_onset(take: &Take, threshold: f32) -> Option<usize> {
    (0..take.frames())
        .find(|&i| take.left[i].abs() >= threshold || take.right[i].abs() >= threshold)
}

/// Strike one note and time how long its audio takes to come back.
///
/// `note`/`velocity` should be a loud, percussive strike — the point is a sharp
/// edge to detect, not a musically representative sample.
pub fn calibrate(
    instrument: &mut Instrument,
    capture: &Capture,
    note: u8,
    velocity: u8,
    timing: &Timing,
    noise_floor: f32,
) -> Result<Latency> {
    let threshold = (noise_floor * ONSET_FACTOR).max(MIN_ONSET_THRESHOLD);

    capture.arm();
    instrument.note_on(note, velocity)?;
    // A calibration strike only needs enough audio to contain the attack.
    let listen_ms = timing.note_length_ms.min(1500).max(250);
    std::thread::sleep(Duration::from_millis(listen_ms as u64));
    instrument.note_off(note)?;
    let take = capture.finish()?;

    std::thread::sleep(Duration::from_millis(timing.settle_ms as u64));

    let Some(frames) = find_onset(&take, threshold) else {
        // Report what actually arrived — "nothing at all" and "something, but
        // under the threshold" have completely different causes.
        let heard = take.peak();
        return Err(eyre!(
            "calibration note never reached {threshold:.6} (loudest was {heard:.6}, \
             noise floor {noise_floor:.6}) — {}",
            if heard <= f32::EPSILON {
                "no audio at all arrived. Check the instrument is patched to the \
                 capture inputs, is not muted, and responds on the chosen MIDI channel"
            } else {
                "audio arrived but too quietly. Turn the instrument up, or check \
                 the chosen inputs carry it rather than a neighbouring pair"
            }
        ));
    };
    Ok(Latency {
        frames,
        noise_floor,
        threshold,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn take_with_onset_at(index: usize, level: f32) -> Take {
        let mut left = vec![0.0; 100];
        let mut right = vec![0.0; 100];
        for i in index..100 {
            left[i] = level;
            right[i] = level;
        }
        Take { left, right }
    }

    #[test]
    fn onset_is_the_first_frame_over_threshold() {
        let take = take_with_onset_at(20, 0.5);
        assert_eq!(find_onset(&take, 0.1), Some(20));
    }

    #[test]
    fn silence_has_no_onset() {
        let take = take_with_onset_at(20, 0.001);
        assert_eq!(find_onset(&take, 0.1), None);
    }

    #[test]
    fn onset_is_found_when_only_one_channel_sounds() {
        let mut take = take_with_onset_at(30, 0.8);
        take.right = vec![0.0; 100];
        assert_eq!(find_onset(&take, 0.1), Some(30));
    }

    #[test]
    fn trimming_by_the_measured_latency_puts_the_attack_first() {
        let mut take = take_with_onset_at(20, 0.5);
        take.trim_start(20);
        assert_eq!(find_onset(&take, 0.1), Some(0));
    }

    #[test]
    fn latency_converts_to_millis() {
        let l = Latency {
            frames: 480,
            noise_floor: 0.0,
            threshold: 0.0,
        };
        assert!((l.millis(48_000) - 10.0).abs() < 1e-9);
    }
}
