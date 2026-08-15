//! The run loop: walk the grid, strike each cell, keep the audio.

use std::path::PathBuf;
use std::time::Duration;

use eyre::{Result, WrapErr};

use crate::capture::Capture;
use crate::config::{AutoSampleConfig, Timing};
use crate::grid::{Cell, cells, note_name};
use crate::latency::{Latency, calibrate, measure_noise_floor};
use crate::midi::Instrument;
use crate::pack::{self, Recorded};
use crate::wav;

/// Total listening time used to establish a noise floor, split across several
/// windows (see [`measure_noise_floor`]).
const NOISE_FLOOR_MS: u32 = 500;

/// How often the level is checked while a note is sounding.
const POLL_MS: u32 = 25;

/// Silence kept after the last audible frame, so a decay isn't cut the instant
/// it crosses the threshold.
const TAIL_GUARD_MS: u32 = 50;

/// Length of the fade applied to the very end of every sample.
const FADE_OUT_MS: u32 = 10;

/// Absolute level the input must fall below before the next note is struck.
///
/// -66 dBFS. This is deliberately absolute rather than relative to the previous
/// note: what matters is whether the room/effect tail is still audible *at all*
/// when the next capture arms, not how loud the note that caused it happened to
/// be. Anything still above this lands in the head of the next sample as a
/// foreign pitch — a reverb tail from a different note, which is audible as
/// frequencies that are not in the patch.
const QUIET_THRESHOLD: f32 = 0.0005;

/// Longest to wait for the instrument to go quiet between notes.
const QUIET_TIMEOUT_MS: u32 = 6000;

/// What a completed run produced.
#[derive(Debug)]
pub struct RunReport {
    pub recorded: Vec<Recorded>,
    /// Cells that produced no usable audio, with the reason.
    pub skipped: Vec<(Cell, String)>,
    pub latency: Latency,
    pub sample_rate: u32,
    pub styx_path: PathBuf,
    pub pack_path: Option<PathBuf>,
    /// Loudest peak seen across the run — over 0.99 means the instrument was
    /// clipping the interface.
    pub peak: f32,
}

impl RunReport {
    /// Whether any capture clipped.
    pub fn clipped(&self) -> bool {
        self.peak >= 0.99
    }
}

/// Run a complete auto-sampling job.
pub fn run(config: &AutoSampleConfig) -> Result<RunReport> {
    std::fs::create_dir_all(&config.out_dir)
        .wrap_err_with(|| format!("create {}", config.out_dir.display()))?;

    let capture = Capture::open(&config.audio)?;
    tracing::info!(
        device = %capture.device_name,
        channels = capture.channels,
        sample_rate = capture.sample_rate,
        inputs = format!("{}/{}", config.audio.left_input, config.audio.right_input),
        "capture open"
    );

    let mut instrument = Instrument::open(&config.midi.port, config.midi.channel)?;

    // 1. How quiet is quiet, with the instrument idle? Silence it first and let
    //    any ringing voice decay — measuring while a note is still sounding
    //    inflates the floor and aborts the whole run at calibration.
    // `panic` rather than `silence`: it also clears a latched sustain pedal and
    // cuts release tails, on every channel. A run that starts against a ringing
    // instrument measures a poisoned noise floor and dies at calibration.
    instrument.panic()?;
    std::thread::sleep(Duration::from_millis(config.timing.settle_ms as u64));
    let noise_floor = measure_noise_floor(&capture, NOISE_FLOOR_MS)?;

    // 2. How long does a note take to come back? Strike hard at the middle of
    //    the sampled range — a note the instrument certainly maps.
    let cal_note = config.grid.low_note / 2 + config.grid.high_note / 2;
    let latency = calibrate(
        &mut instrument,
        &capture,
        cal_note,
        127,
        &config.timing,
        noise_floor,
    )?;
    tracing::info!(
        latency_ms = format!("{:.1}", latency.millis(capture.sample_rate)),
        noise_floor = format!("{:.6}", latency.noise_floor),
        "round-trip calibrated"
    );

    // The calibration strike is loud and has the same long tail as everything
    // else. Without this wait its reverb lands in the head of the FIRST sample,
    // at a pitch unrelated to that sample's note.
    wait_until_quiet(&capture, config.timing.settle_ms, QUIET_TIMEOUT_MS);

    // 2b. Ask the instrument how long it needs to be held to be loopable.
    //     Done here, after calibration, because the probe needs the latency
    //     trim to line its analysis up with what a real cell would record.
    let mut timing = config.timing.clone();
    if timing.probe_note_length {
        let search = config.probe_search.unwrap_or_default();
        let result = crate::probe::probe(
            &mut instrument,
            &capture,
            &timing.probe_notes,
            127,
            &timing,
            &latency,
            &config.loop_policy,
            &search,
            timing.note_length_ms,
            timing.probe_max_ms,
        )?;
        tracing::info!(
            note_length_ms = result.note_length_ms,
            score = result.score.map(|s| format!("{s:.4}")),
            gave_up = result.gave_up,
            "probed hold"
        );
        timing.note_length_ms = result.note_length_ms;
    }

    // 3. Walk the grid.
    let cells = cells(&config.grid);
    let mut recorded = Vec::with_capacity(cells.len());
    let mut skipped = Vec::new();
    let mut peak = 0.0f32;

    let guard_frames = ms_to_frames(TAIL_GUARD_MS, capture.sample_rate);
    let fade_frames = ms_to_frames(FADE_OUT_MS, capture.sample_rate);

    for (i, cell) in cells.iter().enumerate() {
        let file = format!(
            "{}_{}_{:03}_v{:03}.wav",
            sanitize(&config.name),
            note_name(cell.note),
            cell.note,
            cell.velocity
        );
        tracing::info!(
            "[{}/{}] {} vel {} → {file}",
            i + 1,
            cells.len(),
            note_name(cell.note),
            cell.velocity
        );

        capture.arm();
        instrument.note_on(cell.note, cell.velocity)?;

        // Hold — but stop early if the patch has already decayed to silence,
        // so a percussive sound doesn't record seconds of nothing.
        let (note_peak, held_ms) = wait_for_silence(
            &capture,
            &config.timing,
            timing.note_length_ms,
            latency.threshold,
            0.0,
        );

        instrument.note_off(cell.note)?;

        // Release — however long this patch's tail actually takes, capped.
        wait_for_silence(
            &capture,
            &timing,
            config.timing.max_tail_ms,
            latency.threshold,
            note_peak,
        );

        let mut take = match capture.finish() {
            Ok(t) => t,
            Err(e) => {
                skipped.push((*cell, e.to_string()));
                wait_until_quiet(&capture, config.timing.settle_ms, QUIET_TIMEOUT_MS);
                continue;
            }
        };

        // Remove the round trip from the head, the decayed silence from the
        // tail, and ramp the last few ms so a capped tail can't click.
        take.trim_start(latency.frames);
        let take_peak = take.peak();
        let silence = config.timing.silence_threshold(take_peak, latency.threshold);
        take.trim_end(silence, guard_frames);
        take.fade_out(fade_frames);

        if take_peak <= latency.threshold {
            skipped.push((
                *cell,
                format!(
                    "silent (peak {take_peak:.6} ≤ threshold {:.6})",
                    latency.threshold
                ),
            ));
            wait_until_quiet(&capture, config.timing.settle_ms, QUIET_TIMEOUT_MS);
            continue;
        }
        peak = peak.max(take_peak);

        // The note-off landed `held_ms` after note-on. Because `trim_start`
        // already removed the round-trip latency, that same figure is the
        // note-off frame *within the trimmed take* — so the sustained portion
        // is known exactly, with nothing to detect.
        let loop_points = config.loops.then(|| {
            crate::loops::choose(
                ms_to_frames(held_ms, capture.sample_rate),
                take.frames(),
                capture.sample_rate,
                &config.loop_policy,
            )
        });
        let loop_points = loop_points.flatten();

        tracing::debug!(
            secs = format!("{:.2}", take.frames() as f64 / capture.sample_rate as f64),
            peak = format!("{take_peak:.4}"),
            held_ms,
            looped = loop_points.is_some(),
            "captured"
        );

        wav::write(&config.out_dir.join(&file), &take, capture.sample_rate)?;
        recorded.push(Recorded {
            cell: *cell,
            file,
            loop_points,
            sustain_end: Some(ms_to_frames(held_ms, capture.sample_rate) as u32),
        });

        // Wait for the instrument to actually go quiet before the next strike.
        // A fixed sleep is a guess: any effect tail outlasting it is recorded
        // into the head of the next sample as a foreign pitch.
        wait_until_quiet(&capture, config.timing.settle_ms, QUIET_TIMEOUT_MS);
    }

    instrument.silence()?;

    if recorded.is_empty() {
        eyre::bail!(
            "every cell came back silent — nothing was recorded. Check that the \
             instrument is patched to inputs {}/{} and responds on MIDI channel {}",
            config.audio.left_input,
            config.audio.right_input,
            config.midi.channel,
        );
    }

    // 4. Spec, then pack.
    let styx_path = pack::write_styx(config, &recorded)?;
    let pack_path = if config.pack_path.is_some() {
        Some(pack::build(config, &styx_path, &recorded)?)
    } else {
        None
    };

    Ok(RunReport {
        recorded,
        skipped,
        latency,
        sample_rate: capture.sample_rate,
        styx_path,
        pack_path,
        peak,
    })
}

/// Wait until the sound has decayed to silence, or `limit_ms` elapses.
///
/// Returns the loudest level seen during the wait.
///
/// `reference_peak` is the level the silence threshold is measured against. In
/// the hold phase it's 0, meaning "use whatever this note turns out to be" —
/// the threshold tracks the note's own peak as it is discovered. In the release
/// phase it's the peak from the hold, so the tail is judged against how loud the
/// note actually was rather than against the (much quieter) tail itself.
///
/// The `heard_anything` gate matters: a patch with a slow attack is below the
/// threshold for its first few polls, and without the gate the note would be
/// declared finished before it had even started.
fn wait_for_silence(
    capture: &Capture,
    timing: &Timing,
    limit_ms: u32,
    floor: f32,
    reference_peak: f32,
) -> (f32, u32) {
    let poll = Duration::from_millis(POLL_MS as u64);
    let mut peak = reference_peak;
    let mut quiet_ms = 0;
    let mut elapsed = 0;
    let mut heard_anything = reference_peak > floor;

    while elapsed < limit_ms {
        std::thread::sleep(poll);
        elapsed += POLL_MS;

        let level = capture.take_window_peak();
        peak = peak.max(level);
        if level > floor {
            heard_anything = true;
        }
        if !heard_anything {
            continue;
        }

        if level < timing.silence_threshold(peak, floor) {
            quiet_ms += POLL_MS;
            if quiet_ms >= timing.silence_hold_ms {
                break;
            }
        } else {
            // Not silence after all — a tremolo trough or LFO dip. Start over.
            quiet_ms = 0;
        }
    }
    (peak, elapsed)
}

/// Block until the input has been below [`QUIET_THRESHOLD`] continuously for
/// `settle_ms`, or `limit_ms` elapses. Returns how long it waited.
///
/// This exists because a fixed settle is a guess about a decay time we do not
/// know. An effect tail that outlasts the guess is still sounding when the next
/// capture arms, and gets recorded into the head of a sample whose pitch it
/// does not belong to.
pub(crate) fn wait_until_quiet(capture: &Capture, settle_ms: u32, limit_ms: u32) -> u32 {
    let poll = Duration::from_millis(POLL_MS as u64);
    // Arm so the level monitor is fed; the audio is discarded.
    capture.arm();
    let mut quiet_ms = 0;
    let mut elapsed = 0;
    while elapsed < limit_ms && quiet_ms < settle_ms {
        std::thread::sleep(poll);
        elapsed += POLL_MS;
        if capture.take_window_peak() < QUIET_THRESHOLD {
            quiet_ms += POLL_MS;
        } else {
            quiet_ms = 0;
        }
    }
    let _ = capture.finish();
    if quiet_ms < settle_ms {
        tracing::warn!(
            waited_ms = elapsed,
            "instrument never went fully quiet — the next sample may capture a tail"
        );
    }
    elapsed
}

fn ms_to_frames(ms: u32, sample_rate: u32) -> usize {
    (sample_rate as u64 * ms as u64 / 1000) as usize
}

/// Make a name safe to embed in a filename.
fn sanitize(name: &str) -> String {
    name.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '-' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn names_with_spaces_and_punctuation_become_filename_safe() {
        assert_eq!(sanitize("Mart's Awesome Synth"), "Mart_s_Awesome_Synth");
        assert_eq!(sanitize("Kronos/Strings"), "Kronos_Strings");
        assert_eq!(sanitize("well-behaved"), "well-behaved");
    }

    #[test]
    fn frame_conversion_matches_the_sample_rate() {
        assert_eq!(ms_to_frames(1000, 48_000), 48_000);
        assert_eq!(ms_to_frames(500, 44_100), 22_050);
        assert_eq!(ms_to_frames(0, 48_000), 0);
    }
}
