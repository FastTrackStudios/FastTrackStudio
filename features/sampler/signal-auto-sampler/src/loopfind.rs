//! Finding the most seamless loop by cross-correlation.
//!
//! Snapping the loop to a whole number of cycles of the fundamental only helps
//! if the fundamental is the only thing repeating. Real patches have chorus,
//! detune, vibrato and LFOs — slower periodicities that are unrelated to pitch.
//! A loop aligned to the fundamental but cutting an LFO mid-sweep still jumps,
//! and you hear it every time it wraps.
//!
//! So rather than assume what repeats, measure it. For a candidate loop length
//! `L`, the wrap is seamless when the audio approaching `loop_end` looks like
//! the audio approaching `loop_start = loop_end - L`: then the join continues
//! the waveform, and the crossfade blends two aligned copies instead of two
//! phase-offset ones.
//!
//! ```text
//!      ...──────[ window B ]──────────────────[ window A ]──►
//!               ^ loop_start                  ^ loop_end
//!               └──────────── L ──────────────┘
//!   seamless when B and A match
//! ```
//!
//! That similarity is the **normalized cross-correlation** of the two windows,
//! which is scale-invariant — it scores shape, not loudness, so a decaying note
//! is not penalised for being quieter later.
//!
//! Searching every candidate at full rate is far too slow (tens of thousands of
//! lags × a window of thousands of samples, per sample, times 180 samples), so
//! this runs coarse-to-fine: decimate, find the peak, then refine at full rate
//! around it.

/// A loop the search settled on.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Found {
    /// Loop length in frames.
    pub len: usize,
    /// Seam quality, -1..=1: waveform shape (cross-correlation) multiplied by
    /// level continuity. Above ~0.95 is an inaudible join; below ~0.7 ticks.
    pub score: f32,
}

/// Decimation factor for the coarse pass.
const COARSE: usize = 16;

/// Sum to mono. Correlation is about waveform shape, and a stereo patch's two
/// channels share it; summing halves the work and is more robust than picking
/// one channel, which could be the quieter side of a wide chorus.
pub fn to_mono(left: &[f32], right: &[f32]) -> Vec<f32> {
    left.iter()
        .zip(right.iter())
        .map(|(l, r)| (l + r) * 0.5)
        .collect()
}

/// Cheap decimation by averaging blocks of `factor`.
///
/// Averaging rather than picking every Nth sample: it is a crude lowpass, so
/// the coarse pass correlates the signal's envelope and low partials instead of
/// aliased high-frequency content.
fn decimate(x: &[f32], factor: usize) -> Vec<f32> {
    x.chunks(factor)
        .map(|c| c.iter().sum::<f32>() / c.len() as f32)
        .collect()
}

/// Normalized cross-correlation of `a` and `b`, in -1..=1.
///
/// Zero when either window is silent, so a silent region can never score as a
/// perfect match — which it otherwise would, being trivially self-similar.
fn ncc(a: &[f32], b: &[f32]) -> f32 {
    let mut dot = 0.0f64;
    let mut na = 0.0f64;
    let mut nb = 0.0f64;
    for (x, y) in a.iter().zip(b.iter()) {
        dot += (*x as f64) * (*y as f64);
        na += (*x as f64) * (*x as f64);
        nb += (*y as f64) * (*y as f64);
    }
    let denom = (na * nb).sqrt();
    if denom <= f64::EPSILON {
        0.0
    } else {
        (dot / denom) as f32
    }
}

/// How closely two windows match in *level*, 1.0 = identical, 0.0 = one silent.
///
/// [`ncc`] is scale-invariant by design — it scores waveform shape, so a
/// decaying note is not penalised for being quieter later. But that makes it
/// blind to exactly the artefact that ruins a loop on modulated material: if
/// the loop is not a whole number of tremolo or LFO cycles, the two sides sit
/// at different points in the modulation and the wrap *thumps*, even though the
/// waveform shape matches perfectly within a short window.
///
/// Scoring level alongside shape restores that sensitivity without giving up
/// the decay tolerance, because both sides are measured over the same span.
/// Level differences at or above this ratio (≈1 dB) are treated as a perfect
/// match.
///
/// A tolerance is essential, not a fudge. Every sustained note decays a little
/// across a loop, so a strict ratio would penalise *all* looped material and
/// reject the gentle, inaudible level drift that scale-invariance exists to
/// tolerate. Measured: without this band a decaying piano dropped from 188
/// acceptable loops to 36, having previously sounded correct.
///
/// The artefact worth catching is a modulation jump, which is far larger — a
/// tremolo cut mid-cycle lands nearer 0.5.
const LEVEL_TOLERANCE: f32 = 0.9;

fn level_match(a: &[f32], b: &[f32]) -> f32 {
    let rms = |w: &[f32]| -> f64 {
        if w.is_empty() {
            return 0.0;
        }
        (w.iter().map(|v| (*v as f64) * (*v as f64)).sum::<f64>() / w.len() as f64).sqrt()
    };
    let (ra, rb) = (rms(a), rms(b));
    let hi = ra.max(rb);
    if hi <= f64::EPSILON {
        return 0.0;
    }
    let ratio = (ra.min(rb) / hi) as f32;
    (ratio / LEVEL_TOLERANCE).min(1.0)
}

/// Seam score: waveform shape, optionally weighted by level continuity.
///
/// `level_weight` 0 scores shape alone; 1 multiplies in [`level_match`].
///
/// These answer different questions and the default differs by caller:
///
/// - **Loop search** uses 0. Shape alone is what a listener judges a seam by,
///   and it was validated by ear on a real pack. Adding a level term rejected
///   two thirds of loops that sounded correct, because a sustained note always
///   decays a little across a loop.
/// - **The note-length probe** uses 1. It is asking whether the patch has
///   settled into something that *repeats*, and a tremolo cut mid-cycle is
///   exactly what it must not mistake for a steady tone — which shape alone,
///   being scale-invariant, cannot see.
fn seam_score(a: &[f32], b: &[f32], level_weight: f32) -> f32 {
    let shape = ncc(a, b);
    if shape <= 0.0 || level_weight <= 0.0 {
        return shape;
    }
    let lvl = level_match(a, b);
    shape * (1.0 - level_weight + level_weight * lvl)
}

/// Search for the loop length that joins most seamlessly.
///
/// `end` is the fixed loop end; the search varies the start. Lengths are
/// searched in `min_len..=max_len`, and `window` frames on each side are
/// compared. Returns `None` if the audio is too short for the smallest
/// candidate.
pub fn best_loop(
    mono: &[f32],
    end: usize,
    min_len: usize,
    max_len: usize,
    window: usize,
) -> Option<Found> {
    best_loop_weighted(mono, end, min_len, max_len, window, 0.0)
}

/// [`best_loop`] with an explicit level weighting — see [`seam_score`].
pub fn best_loop_weighted(
    mono: &[f32],
    end: usize,
    min_len: usize,
    max_len: usize,
    window: usize,
    level_weight: f32,
) -> Option<Found> {
    let end = end.min(mono.len());
    // Every candidate needs `len + window` frames of history before `end`.
    let max_len = max_len.min(end.saturating_sub(window));
    if min_len > max_len || min_len == 0 {
        return None;
    }

    // ── Coarse pass ─────────────────────────────────────────────────────────
    let d = decimate(&mono[..end], COARSE);
    let d_end = d.len();
    let d_window = (window / COARSE).max(8);
    let d_min = (min_len / COARSE).max(1);
    let d_max = (max_len / COARSE).min(d_end.saturating_sub(d_window + 1));
    if d_min > d_max {
        return None;
    }

    let a_lo = d_end.checked_sub(d_window)?;
    let a = &d[a_lo..d_end];

    let mut best_lag = d_min;
    let mut best_score = f32::MIN;
    for lag in d_min..=d_max {
        let b_hi = d_end.saturating_sub(lag);
        let Some(b_lo) = b_hi.checked_sub(d_window) else {
            continue;
        };
        let s = seam_score(a, &d[b_lo..b_hi], level_weight);
        if s > best_score {
            best_score = s;
            best_lag = lag;
        }
    }

    // ── Fine pass ───────────────────────────────────────────────────────────
    // The coarse peak is accurate to within one decimated step; refine at full
    // rate across that neighbourhood. Without this the loop can sit up to 16
    // frames off, which is several degrees of phase at high pitches.
    let centre = best_lag * COARSE;
    let lo = centre.saturating_sub(COARSE * 2).max(min_len);
    let hi = (centre + COARSE * 2).min(max_len);

    let a_lo = end.checked_sub(window)?;
    let a = &mono[a_lo..end];

    let mut found = Found {
        len: centre.clamp(min_len, max_len),
        score: 0.0,
    };
    let mut best = f32::MIN;
    for len in lo..=hi {
        let b_hi = end.saturating_sub(len);
        let Some(b_lo) = b_hi.checked_sub(window) else {
            continue;
        };
        let s = seam_score(a, &mono[b_lo..b_hi], level_weight);
        if s > best {
            best = s;
            found = Found { len, score: s };
        }
    }
    (best > f32::MIN).then_some(found)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f32::consts::TAU;

    const SR: usize = 48_000;

    fn tone(freq: f32, n: usize) -> Vec<f32> {
        (0..n)
            .map(|i| (TAU * freq * i as f32 / SR as f32).sin())
            .collect()
    }

    #[test]
    fn ncc_is_one_for_identical_windows_and_scale_invariant() {
        let a = tone(440.0, 1000);
        let b: Vec<f32> = a.iter().map(|v| v * 0.25).collect();
        assert!((ncc(&a, &a) - 1.0).abs() < 1e-5);
        assert!(
            (ncc(&a, &b) - 1.0).abs() < 1e-5,
            "quieter copy must still score 1 — shape, not level"
        );
    }

    #[test]
    fn level_mismatch_is_penalised_even_when_the_shape_matches() {
        // Same waveform, half the level: NCC alone calls this perfect, which is
        // how a loop that cuts a tremolo mid-cycle used to score 1.0.
        let a = tone(440.0, 2000);
        let quiet: Vec<f32> = a.iter().map(|v| v * 0.5).collect();
        assert!((ncc(&a, &quiet) - 1.0).abs() < 1e-5, "shape still matches");
        // 0.5 ratio / 0.9 tolerance = 0.556 — well penalised.
        assert!(
            (level_match(&a, &quiet) - 0.5 / 0.9).abs() < 1e-3,
            "level should score ~0.56, got {}",
            level_match(&a, &quiet)
        );
        assert!(
            seam_score(&a, &quiet, 1.0) < 0.6,
            "combined score must reflect the level jump: {}",
            seam_score(&a, &quiet, 1.0)
        );
    }

    #[test]
    fn gentle_decay_across_a_loop_is_not_penalised() {
        // A sustained note quietly decaying is the normal case; a strict ratio
        // would reject every loop on such material.
        let a = tone(440.0, 2000);
        let slightly_quieter: Vec<f32> = a.iter().map(|v| v * 0.95).collect();
        assert!(
            (level_match(&a, &slightly_quieter) - 1.0).abs() < 1e-6,
            "5% decay must score a clean 1.0"
        );
        assert!(seam_score(&a, &slightly_quieter, 1.0) > 0.99);
    }

    #[test]
    fn an_identical_seam_still_scores_one() {
        let a = tone(440.0, 2000);
        assert!((seam_score(&a, &a, 1.0) - 1.0).abs() < 1e-5);
    }

    #[test]
    fn ncc_is_zero_against_silence() {
        let a = tone(440.0, 500);
        let silent = vec![0.0f32; 500];
        assert_eq!(ncc(&a, &silent), 0.0, "silence must not look like a match");
    }

    #[test]
    fn finds_a_whole_number_of_cycles_for_a_pure_tone() {
        // 200 Hz at 48k = 240 frames per cycle.
        let x = tone(200.0, SR);
        let f = best_loop(&x, SR - 1, 4_800, 12_000, 2_048).expect("should find a loop");
        let cycles = f.len as f32 / 240.0;
        assert!(
            (cycles - cycles.round()).abs() < 0.02,
            "len {} is {cycles} cycles",
            f.len
        );
        assert!(f.score > 0.99, "a pure tone should join perfectly: {}", f.score);
    }

    #[test]
    fn prefers_the_modulation_period_over_the_pitch_period() {
        // 200 Hz carrier with a 4 Hz tremolo. Pitch period is 240 frames, but a
        // seamless loop must also be a whole number of 12000-frame tremolo
        // cycles — this is the case pitch-snapping alone gets wrong.
        let x: Vec<f32> = (0..SR * 2)
            .map(|i| {
                let t = i as f32 / SR as f32;
                (TAU * 200.0 * t).sin() * (1.0 + 0.8 * (TAU * 4.0 * t).sin())
            })
            .collect();
        let f = best_loop(&x, SR * 2 - 1, 4_800, 36_000, 4_096).expect("should find a loop");
        let tremolo_cycles = f.len as f32 / 12_000.0;
        assert!(
            (tremolo_cycles - tremolo_cycles.round()).abs() < 0.05,
            "len {} is {tremolo_cycles} tremolo cycles — the search ignored the modulation",
            f.len
        );
        assert!(f.score > 0.9, "score {}", f.score);
    }

    #[test]
    fn returns_none_when_the_audio_is_too_short() {
        let x = tone(200.0, 1_000);
        assert_eq!(best_loop(&x, 1_000, 4_800, 12_000, 2_048), None);
    }

    #[test]
    fn respects_the_requested_length_bounds() {
        let x = tone(200.0, SR);
        let f = best_loop(&x, SR - 1, 9_600, 14_400, 2_048).unwrap();
        assert!(
            f.len >= 9_600 && f.len <= 14_400,
            "len {} outside bounds",
            f.len
        );
    }

    #[test]
    fn a_noisy_signal_scores_worse_than_a_periodic_one() {
        // Deterministic pseudo-noise: nothing repeats, so no loop should look
        // seamless. This is what tells a caller "leave this one unlooped".
        let mut seed = 12345u32;
        let noise: Vec<f32> = (0..SR)
            .map(|_| {
                seed = seed.wrapping_mul(1664525).wrapping_add(1013904223);
                (seed >> 8) as f32 / 8_388_608.0 - 1.0
            })
            .collect();
        let tonal = best_loop(&tone(200.0, SR), SR - 1, 4_800, 12_000, 2_048).unwrap();
        let noisy = best_loop(&noise, SR - 1, 4_800, 12_000, 2_048).unwrap();
        assert!(
            noisy.score < tonal.score,
            "noise {} should score below tone {}",
            noisy.score,
            tonal.score
        );
    }
}
