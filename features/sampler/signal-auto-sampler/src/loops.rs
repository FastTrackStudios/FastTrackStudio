//! Choosing the sustain loop for a recorded note.
//!
//! Without a loop, a held key plays the recording once and stops — a pad dies
//! after however long it was sampled. The engine loops `loop_start..loop_end`
//! for as long as the note is held, so the loop has to sit in the part of the
//! recording where the instrument was steady.
//!
//! We know exactly where that is, for free. The sampler decided when to send
//! note-off, so the sustained portion is `0..release_start` by construction —
//! no onset detection, no heuristics about where the release begins. That is
//! the whole advantage of looping material you recorded yourself.
//!
//! ```text
//!   |<- attack ->|<----------- sustained ----------->|<- release ->|
//!   0         skip                loop_start   loop_end        note-off
//!                                      |<-- looped while held -->|
//! ```
//!
//! The loop is crossfaded rather than snapped to zero crossings. A zero
//! crossing only guarantees continuity for one channel of one partial; a
//! crossfade blends the seam for the whole spectrum, which is what a rich
//! multi-oscillator patch actually needs.

/// Where a zone's sustain loop sits, in frames.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LoopPoints {
    pub start: u32,
    pub end: u32,
    pub xfade: u32,
}

/// Loop-shaping limits, all in milliseconds.
#[derive(Debug, Clone, Copy)]
pub struct LoopPolicy {
    /// Material at the head to leave out of the loop, so the attack transient
    /// never repeats.
    pub attack_skip_ms: u32,
    /// Distance to keep between the loop end and note-off, so no part of the
    /// key-up behaviour is caught inside the loop.
    pub release_guard_ms: u32,
    /// Preferred loop length. Longer loops sound less static; shorter ones are
    /// all that fit in a briefly-held note.
    pub target_len_ms: u32,
    /// Shortest acceptable loop. Below this the loop is audibly a warble, so
    /// it's better to leave the zone unlooped.
    pub min_len_ms: u32,
    /// Preferred crossfade at the seam. **Defaults to 0.**
    ///
    /// A crossfade is the standard answer to a loop click, but measurement here
    /// said otherwise: with correlation-chosen loop points the raw join is
    /// already smoother than the material's own sample-to-sample motion
    /// (0.76x), while enabling the crossfade produced an audible burst of noise
    /// in two independent samplers. Blending two near-identical copies is not
    /// free — any residual phase offset combs. Leave it off unless a specific
    /// loop needs it.
    pub xfade_ms: u32,
}

impl Default for LoopPolicy {
    fn default() -> Self {
        Self {
            attack_skip_ms: 500,
            release_guard_ms: 150,
            target_len_ms: 1000,
            min_len_ms: 250,
            xfade_ms: 0,
        }
    }
}

/// Frequency of a MIDI note in Hz. A440 = note 69.
pub fn note_hz(note: u8) -> f64 {
    440.0 * 2f64.powf((note as f64 - 69.0) / 12.0)
}

/// Round `len` down to a whole number of cycles of `note`'s fundamental.
///
/// This is the difference between a loop that is inaudible and one that ticks
/// once per cycle. A crossfade blends *amplitude* across the seam, but if the
/// loop is not a whole number of periods the two sides are at different points
/// in the waveform — so the fade sums two copies out of phase, which cancels
/// and colours rather than joining smoothly. Snapping to whole cycles makes the
/// two sides phase-aligned before the crossfade even runs.
///
/// Returns `len` unchanged if snapping would make the loop shorter than `min`.
pub fn snap_to_cycles(len: usize, note: u8, sample_rate: u32, min: usize) -> usize {
    let period = sample_rate as f64 / note_hz(note);
    if period <= 1.0 {
        return len;
    }
    let cycles = (len as f64 / period).floor();
    if cycles < 1.0 {
        return len;
    }
    let snapped = (cycles * period).round() as usize;
    if snapped >= min { snapped } else { len }
}

/// Pick the loop for one recording.
///
/// `sustain_end` is the frame where note-off takes effect; `total` is the
/// recording's full length. `root_note` lets the loop length be snapped to a
/// whole number of cycles — pass `None` to skip that.
///
/// Returns `None` when the sustained portion is too short to hold a usable loop
/// — the caller should leave that zone unlooped rather than emit a warbling one.
pub fn choose_for_note(
    sustain_end: usize,
    total: usize,
    sample_rate: u32,
    policy: &LoopPolicy,
    root_note: Option<u8>,
) -> Option<LoopPoints> {
    let mut points = choose(sustain_end, total, sample_rate, policy)?;
    if let Some(note) = root_note {
        let ms = |v: u32| (sample_rate as u64 * v as u64 / 1000) as usize;
        let len = (points.end - points.start) as usize;
        let snapped = snap_to_cycles(len, note, sample_rate, ms(policy.min_len_ms));
        // Shorten from the START, keeping `end` where it is — `end` was placed
        // relative to note-off and moving it would eat into the release guard.
        points.start = points.end - snapped as u32;
        points.xfade = points
            .xfade
            .min(snapped as u32 / 2)
            .min(points.start);
    }
    Some(points)
}

/// Pick the loop for one recording, without pitch snapping.
pub fn choose(
    sustain_end: usize,
    total: usize,
    sample_rate: u32,
    policy: &LoopPolicy,
) -> Option<LoopPoints> {
    let ms = |v: u32| (sample_rate as u64 * v as u64 / 1000) as usize;

    // Never let the loop run past the audio that actually exists, even if the
    // note-off frame says otherwise (a trimmed tail can shorten the take).
    let sustain_end = sustain_end.min(total);
    let end = sustain_end.checked_sub(ms(policy.release_guard_ms))?;
    let skip = ms(policy.attack_skip_ms);
    let available = end.checked_sub(skip)?;

    if available < ms(policy.min_len_ms) {
        return None;
    }

    let len = available.min(ms(policy.target_len_ms));
    let start = end - len;

    // The engine clamps the crossfade to the material before `loop_start` and
    // to half the loop, so compute it the same way rather than emitting a value
    // that silently gets reduced.
    let xfade = ms(policy.xfade_ms).min(len / 2).min(start);

    Some(LoopPoints {
        start: start as u32,
        end: end as u32,
        xfade: xfade as u32,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    const SR: u32 = 48_000;
    fn ms(v: u32) -> usize {
        (SR as usize) * v as usize / 1000
    }

    #[test]
    fn loop_sits_inside_the_sustained_portion() {
        // 3 s held, 5 s total (2 s of release recorded after note-off).
        let l = choose(ms(3000), ms(5000), SR, &LoopPolicy::default()).unwrap();
        assert!(l.start as usize >= ms(500), "clear of the attack");
        assert!(
            (l.end as usize) <= ms(3000) - ms(150),
            "clear of note-off, got {}",
            l.end
        );
        assert!(l.end > l.start);
    }

    #[test]
    fn loop_length_hits_the_target_when_there_is_room() {
        let l = choose(ms(3000), ms(5000), SR, &LoopPolicy::default()).unwrap();
        assert_eq!((l.end - l.start) as usize, ms(1000));
    }

    #[test]
    fn loop_shrinks_to_fit_a_short_hold() {
        // 1 s held: 500 ms attack skip + 150 ms guard leaves only 350 ms.
        let l = choose(ms(1000), ms(2000), SR, &LoopPolicy::default()).unwrap();
        assert_eq!((l.end - l.start) as usize, ms(350));
    }

    #[test]
    fn no_loop_when_the_sustain_is_too_short_to_hold_one() {
        // 700 ms held leaves 50 ms — a loop that short would warble.
        assert_eq!(choose(ms(700), ms(2000), SR, &LoopPolicy::default()), None);
        // Shorter than the attack skip alone.
        assert_eq!(choose(ms(300), ms(2000), SR, &LoopPolicy::default()), None);
        assert_eq!(choose(0, ms(2000), SR, &LoopPolicy::default()), None);
    }

    #[test]
    fn crossfade_never_exceeds_half_the_loop_or_the_head_material() {
        let l = choose(ms(1000), ms(2000), SR, &LoopPolicy::default()).unwrap();
        let len = (l.end - l.start) as usize;
        assert!(
            (l.xfade as usize) <= len / 2,
            "xfade {} > half of {len}",
            l.xfade
        );
        assert!(
            l.xfade <= l.start,
            "xfade {} needs {} frames of head material",
            l.xfade,
            l.start
        );
    }

    #[test]
    fn loop_never_runs_past_the_audio_that_exists() {
        // Note-off at 3 s but the take was trimmed to 1.5 s.
        let l = choose(ms(3000), ms(1500), SR, &LoopPolicy::default()).unwrap();
        assert!(
            (l.end as usize) <= ms(1500),
            "loop end {} past the {} frames available",
            l.end,
            ms(1500)
        );
    }

    #[test]
    fn note_frequencies_are_right() {
        assert!((note_hz(69) - 440.0).abs() < 1e-9, "A4");
        assert!((note_hz(60) - 261.6255653).abs() < 1e-6, "C4");
        assert!((note_hz(21) - 27.5).abs() < 1e-9, "A0");
    }

    #[test]
    fn snapped_loop_is_a_whole_number_of_cycles() {
        // C4 = 261.6256 Hz → 183.46 frames per cycle at 48k.
        let period = SR as f64 / note_hz(60);
        let snapped = snap_to_cycles(ms(1000), 60, SR, ms(250));
        let cycles = snapped as f64 / period;
        // A whole number of cycles almost never lands exactly on an integer
        // frame, so the achievable target is "within half a frame of a whole
        // cycle" — the residual phase error is sub-sample and inaudible.
        let tolerance_cycles = 0.5 / period;
        assert!(
            (cycles - cycles.round()).abs() < tolerance_cycles,
            "{snapped} frames is {cycles} cycles — off by more than half a frame"
        );
        assert!(snapped <= ms(1000), "snapping only ever shortens");
    }

    #[test]
    fn snapping_holds_for_notes_across_the_keyboard() {
        for note in [21u8, 36, 60, 84, 108] {
            let period = SR as f64 / note_hz(note);
            let snapped = snap_to_cycles(ms(1000), note, SR, ms(250));
            let cycles = snapped as f64 / period;
            let tolerance_cycles = 0.5 / period;
            assert!(
                (cycles - cycles.round()).abs() < tolerance_cycles,
                "note {note}: {snapped} frames is {cycles} cycles — over half a frame out"
            );
        }
    }

    #[test]
    fn snapping_is_skipped_when_it_would_shorten_below_the_minimum() {
        // A very low note has a long period; snapping a short loop could drop
        // it below the minimum, which would be worse than an unsnapped loop.
        let len = ms(260);
        let snapped = snap_to_cycles(len, 21, SR, ms(250));
        assert!(snapped >= ms(250), "got {snapped}");
    }

    #[test]
    fn snapped_loop_keeps_its_end_and_moves_only_the_start() {
        let plain = choose(ms(3000), ms(5000), SR, &LoopPolicy::default()).unwrap();
        let snapped =
            choose_for_note(ms(3000), ms(5000), SR, &LoopPolicy::default(), Some(60)).unwrap();
        assert_eq!(
            plain.end, snapped.end,
            "end is placed relative to note-off and must not move"
        );
        assert!(snapped.start >= plain.start, "loop only ever shortens");
    }

    #[test]
    fn snapped_crossfade_still_fits_the_shorter_loop() {
        let l = choose_for_note(ms(1000), ms(2000), SR, &LoopPolicy::default(), Some(21)).unwrap();
        let len = (l.end - l.start) as usize;
        assert!((l.xfade as usize) <= len / 2, "xfade {} vs len {len}", l.xfade);
        assert!(l.xfade <= l.start);
    }

    #[test]
    fn engine_accepts_the_loop_it_is_given() {
        // `Voice::with_forward_loop` ignores a loop unless end > start + 1.
        let l = choose(ms(3000), ms(5000), SR, &LoopPolicy::default()).unwrap();
        assert!(l.end > l.start + 1, "engine would discard this loop");
    }
}
