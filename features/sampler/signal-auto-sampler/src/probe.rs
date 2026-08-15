//! Working out how long to hold a note, per patch, by asking the instrument.
//!
//! `note_length` is the one setting that cannot be adapted while recording. The
//! adaptive tail already handles the *end* of a note — it stops when the sound
//! decays — but the hold has to be decided before the note is struck, and it
//! determines how much sustained material exists to loop.
//!
//! Get it wrong in either direction and it costs:
//!
//! - **Too short** for an evolving pad: nothing repeats inside the recorded
//!   sustain, so no loop clears the threshold and held notes stop dead. A 3 s
//!   hold leaves ~2.35 s to loop in after the attack skip and release guard,
//!   which is less than one modulation cycle on a slow pad.
//! - **Too long** for a piano: minutes of silence recorded across a whole run,
//!   for nothing.
//!
//! So measure it. Hold a few notes for the maximum, then ask the *recording*
//! the question that matters: what is the shortest hold that still contains a
//! loop good enough to use? That is a property of the patch, and it is exactly
//! what the sampler needs to know.
//!
//! Probing happens before sampling rather than during it because the analysis
//! needs the finished audio. Reading the capture buffer mid-note would contend
//! with the audio callback's `try_lock`, which counts a miss as an overrun and
//! would discard otherwise good takes.

use std::time::Duration;

use eyre::Result;

use crate::capture::Capture;
use crate::config::Timing;
use crate::latency::Latency;
use crate::loopfind;
use crate::loops::LoopPolicy;
use crate::midi::Instrument;
use crate::reloop::SearchRange;

/// Granularity of the hold search, in milliseconds. Finer than this is not
/// worth the analysis time — the answer feeds a recording, not a calculation.
const STEP_MS: u32 = 500;

/// What a probe concluded.
#[derive(Debug, Clone, Copy)]
pub struct ProbeResult {
    /// Hold to use, in milliseconds.
    pub note_length_ms: u32,
    /// Seam score achieved at that hold, if one cleared the threshold.
    pub score: Option<f32>,
    /// True when no hold up to the maximum produced an acceptable loop, so the
    /// maximum was used. The patch may simply not be loopable — a piano, a
    /// plucked string — which is a legitimate answer, not a failure.
    pub gave_up: bool,
}

/// Shortest hold whose sustained portion contains a loop scoring at least
/// `min_score`.
///
/// `mono` is the probe recording with the round-trip latency already trimmed,
/// so frame 0 is the note's first audible sample.
pub fn shortest_useful_hold(
    mono: &[f32],
    sample_rate: u32,
    min_ms: u32,
    max_ms: u32,
    policy: &LoopPolicy,
    search: &SearchRange,
) -> ProbeResult {
    let ms = |v: u32| (sample_rate as u64 * v as u64 / 1000) as usize;

    let mut hold = min_ms;
    while hold <= max_ms {
        // What a real cell at this hold would have to work with.
        let sustain_end = ms(hold).min(mono.len());
        let Some(loop_end) = sustain_end.checked_sub(ms(policy.release_guard_ms)) else {
            hold += STEP_MS;
            continue;
        };
        let Some(available) = loop_end.checked_sub(ms(policy.attack_skip_ms)) else {
            hold += STEP_MS;
            continue;
        };
        let min_len = ms(search.min_len_ms);
        if available >= min_len {
            let max_len = ms(search.max_len_ms).min(available);
            // Judged by the SAME criterion the loop search will later apply.
            // The probe's job is "what hold does the search need?", so scoring
            // it differently makes it answer a question nobody asked: measured
            // on real audio, a stricter bar here declared defeat on patches the
            // search then looped happily (Sirus Piano-Wurlitzer, 459 loops
            // shape-only vs 145 level-weighted).
            if let Some(found) = loopfind::best_loop_weighted(
                mono,
                loop_end,
                min_len,
                max_len,
                ms(search.window_ms),
                search.level_weight,
            )
                && found.score >= search.min_score
            {
                return ProbeResult {
                    note_length_ms: hold,
                    score: Some(found.score),
                    gave_up: false,
                };
            }
        }
        hold += STEP_MS;
    }

    // Nothing loops at any hold. Fall back to the configured length, NOT the
    // maximum: since the probe scores by the same bar as the search, "no hold
    // works" means the extra recording time buys nothing — it would just make
    // every note 4x longer to produce a patch that still cannot loop.
    ProbeResult {
        note_length_ms: min_ms,
        score: None,
        gave_up: true,
    }
}

/// Strike `notes` for `max_ms` each and return the hold the patch needs.
///
/// Several notes are probed and the **longest** requirement wins. A split patch
/// can be a fast piano low down and a slow pad up top; sizing from one note
/// would starve the other half of the keyboard.
pub fn probe(
    instrument: &mut Instrument,
    capture: &Capture,
    notes: &[u8],
    velocity: u8,
    timing: &Timing,
    latency: &Latency,
    policy: &LoopPolicy,
    search: &SearchRange,
    min_ms: u32,
    max_ms: u32,
) -> Result<ProbeResult> {
    let mut worst = ProbeResult {
        note_length_ms: min_ms,
        score: None,
        gave_up: false,
    };

    for &note in notes {
        capture.arm();
        instrument.note_on(note, velocity)?;
        std::thread::sleep(Duration::from_millis(max_ms as u64));
        instrument.note_off(note)?;
        // Only the held portion is needed; the release is not looped.
        std::thread::sleep(Duration::from_millis(100));
        let mut take = capture.finish()?;
        take.trim_start(latency.frames);

        let mono = loopfind::to_mono(&take.left, &take.right);
        let result = shortest_useful_hold(&mono, capture.sample_rate, min_ms, max_ms, policy, search);
        tracing::debug!(
            note,
            hold_ms = result.note_length_ms,
            score = result.score.map(|s| format!("{s:.4}")),
            gave_up = result.gave_up,
            "probe"
        );

        // Longest requirement wins; a note that never loops does not force the
        // maximum on its own, but it does if nothing else needs longer.
        if result.note_length_ms > worst.note_length_ms || (result.gave_up && !worst.gave_up) {
            worst = result;
        }

        crate::session::wait_until_quiet(capture, timing.settle_ms, 6000);
    }
    Ok(worst)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f32::consts::TAU;

    const SR: u32 = 48_000;

    fn policy() -> LoopPolicy {
        LoopPolicy::default()
    }
    fn search() -> SearchRange {
        SearchRange {
            min_len_ms: 400,
            max_len_ms: 2500,
            window_ms: 80,
            min_score: 0.97,
            level_weight: 0.0,
        }
    }

    /// Level-aware variant. Cross-correlation is scale-invariant, so shape
    /// alone cannot tell a tremolo caught mid-cycle from a steady tone; this
    /// weighting can. It is not the default because measured on real material
    /// it rejects far more than it catches — see `seam_score`.
    fn level_aware_search() -> SearchRange {
        SearchRange {
            level_weight: 1.0,
            ..search()
        }
    }

    /// Steady tone — loopable almost immediately.
    fn steady(secs: f32) -> Vec<f32> {
        let n = (SR as f32 * secs) as usize;
        (0..n)
            .map(|i| (TAU * 220.0 * i as f32 / SR as f32).sin())
            .collect()
    }

    /// Tone with a slow LFO: nothing repeats until a full LFO cycle has passed.
    fn evolving(secs: f32, lfo_hz: f32) -> Vec<f32> {
        let n = (SR as f32 * secs) as usize;
        (0..n)
            .map(|i| {
                let t = i as f32 / SR as f32;
                (TAU * 220.0 * t).sin() * (1.0 + 0.9 * (TAU * lfo_hz * t).sin())
            })
            .collect()
    }

    #[test]
    fn a_steady_tone_needs_only_a_short_hold() {
        let r = shortest_useful_hold(&steady(12.0), SR, 1500, 12_000, &policy(), &search());
        assert!(!r.gave_up);
        assert!(
            r.note_length_ms <= 2500,
            "steady tone should not need {} ms",
            r.note_length_ms
        );
    }

    #[test]
    fn a_slowly_evolving_pad_needs_a_longer_hold() {
        // 0.5 Hz LFO = a 2 s cycle. A loop must span whole cycles, so the hold
        // has to cover the attack skip, guard, and at least one full cycle.
        let s = level_aware_search();
        let steady_hold =
            shortest_useful_hold(&steady(12.0), SR, 1500, 12_000, &policy(), &s).note_length_ms;
        let pad = shortest_useful_hold(&evolving(12.0, 0.5), SR, 1500, 12_000, &policy(), &s);
        assert!(
            pad.note_length_ms > steady_hold,
            "pad needed {} ms, steady tone needed {steady_hold} ms — the probe is not \
             distinguishing them",
            pad.note_length_ms
        );
    }

    #[test]
    fn the_probe_reports_giving_up_rather_than_inventing_a_loop() {
        // Deterministic noise never repeats, so no hold produces a good loop.
        let mut seed = 7u32;
        let noise: Vec<f32> = (0..SR as usize * 8)
            .map(|_| {
                seed = seed.wrapping_mul(1664525).wrapping_add(1013904223);
                (seed >> 8) as f32 / 8_388_608.0 - 1.0
            })
            .collect();
        let r = shortest_useful_hold(&noise, SR, 1500, 6000, &policy(), &search());
        assert!(r.gave_up, "should not claim a loop it cannot make");
        assert_eq!(
            r.note_length_ms, 1500,
            "gives up to the CONFIGURED length, not the maximum — recording 4x \
             longer cannot rescue material that does not loop"
        );
        assert!(r.score.is_none());
    }

    #[test]
    fn the_hold_it_picks_actually_contains_an_acceptable_loop() {
        // The whole point: re-running the same check at the chosen hold must
        // succeed, or the sampler would record material it cannot loop.
        let audio = evolving(12.0, 0.5);
        let s = level_aware_search();
        let r = shortest_useful_hold(&audio, SR, 1500, 12_000, &policy(), &s);
        assert!(!r.gave_up);
        let verify =
            shortest_useful_hold(&audio, SR, r.note_length_ms, r.note_length_ms, &policy(), &s);
        assert!(!verify.gave_up, "chosen hold does not actually loop");
        assert!(verify.score.unwrap() >= search().min_score);
    }

    #[test]
    fn a_short_recording_cannot_be_asked_for_a_long_hold() {
        // 2 s of audio, asked for up to 12 s: must not read past the buffer.
        let r = shortest_useful_hold(&steady(2.0), SR, 1500, 12_000, &policy(), &search());
        assert!(r.note_length_ms <= 12_000);
    }
}
