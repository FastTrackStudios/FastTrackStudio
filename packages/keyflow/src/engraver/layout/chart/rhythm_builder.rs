//! Rhythm building utilities for chart layout.
//!
//! This module provides functions for converting keyflow rhythm notation
//! to engraver Duration types and building rhythm patterns for measures.

use crate::engraver::model::DurationKind;
use crate::engraver::notation::{Duration, RhythmEntry, TupletRatio, TupletSpec};
use crate::chart::types::{Measure, RhythmElement};
use crate::chord::{ChordRhythm, LilySyntax, PushPullBase};

use super::PushSpillback;

/// Convert keyflow LilySyntax to engraver Duration.
///
/// This is a standalone function that can be used without a ChartLayoutEngine instance.
/// It applies dotted and triplet modifiers to create the final Duration.
#[must_use]
pub fn lily_syntax_to_duration(lily: LilySyntax, dotted: bool, triplet: bool) -> Duration {
    // Convert LilySyntax to DurationKind via NoteValue
    let note_value = crate::core::NoteValue::from(lily);
    let kind = DurationKind::from(note_value);

    if triplet {
        if dotted {
            // Dotted triplet
            Duration {
                kind,
                dots: 1,
                tuplet: Some(TupletRatio::triplet()),
            }
        } else {
            Duration::triplet(kind)
        }
    } else if dotted {
        Duration::dotted(kind)
    } else {
        Duration::new(kind)
    }
}

/// Check if a measure has explicit chord rhythms (Lily or Rest notation).
///
/// When chords have explicit rhythms like `r8t Ab9_8t r8t r4t F9_8t r2`,
/// we should render those rhythms instead of using slash notation.
///
/// Note: `Space` (s1, s2, etc.) does NOT count as explicit rhythm - it means
/// "fill this measure with automatic slashes".
#[must_use]
pub fn measure_has_explicit_chord_rhythm(measure: &Measure) -> bool {
    // Check rhythm_elements first (preferred - contains both chords and rests)
    if !measure.rhythm_elements.is_empty() {
        let has_real_rhythm = measure.rhythm_elements.iter().any(|elem| {
            match elem {
                RhythmElement::Chord(chord) => {
                    // Only Explicit durations count as explicit rhythm
                    // Skip pushed first chords (they spill back to previous measure)
                    let is_pushed_first = chord
                        .push_pull
                        .as_ref()
                        .map_or(false, |(is_push, _)| *is_push);
                    !is_pushed_first && chord.rhythm.has_lily_duration()
                }
                RhythmElement::Rest(_) => true, // Rests count as real rhythm
                RhythmElement::Space(_) => false, // Space triggers auto-fill
            }
        });
        if has_real_rhythm {
            return true;
        }
    }

    // Fallback: check chords for explicit rhythms
    measure.chords.iter().enumerate().any(|(idx, chord)| {
        let is_pushed_first = idx == 0
            && chord
                .push_pull
                .as_ref()
                .map_or(false, |(is_push, _)| *is_push);
        !is_pushed_first && (chord.rhythm.has_lily_duration() || chord.rhythm.is_rest())
    })
}

/// Build rhythm entries from explicit chord rhythms.
///
/// Converts chord rhythms (Lily, Rest, Space) to RhythmEntry values
/// and detects triplet groups for bracketing.
///
/// Returns (entries, total_ticks, tuplet_specs)
#[must_use]
pub fn build_rhythm_from_chord_rhythms(
    measure: &Measure,
) -> (Vec<RhythmEntry>, i32, Vec<TupletSpec>) {
    let mut entries = Vec::new();
    let mut tuplet_specs = Vec::new();
    let mut rhythm_index = 0;
    let mut triplet_group_start: Option<usize> = None;
    let mut triplet_group_ticks: i32 = 0;

    // Triplets are grouped by beat: 3 triplet eighths = 480 ticks (one quarter note)
    const TRIPLET_BEAT_TICKS: i32 = 480;

    // Use rhythm_elements if available (contains both chords and rests in order)
    if !measure.rhythm_elements.is_empty() {
        for element in &measure.rhythm_elements {
            // Note: Spaces used to trigger auto-fill, but now they're also used for
            // cross-barline chord continuation (e.g., "Abmaj9 //// | //").
            // We include spaces in the rhythm so they render as rhythm slashes.

            let (duration, is_rest, is_triplet) = extract_rhythm_parts(element);

            // Track triplet groups
            if is_triplet {
                if triplet_group_start.is_none() {
                    triplet_group_start = Some(rhythm_index);
                    triplet_group_ticks = 0;
                }
                triplet_group_ticks += duration.ticks();
            } else if let Some(start) = triplet_group_start {
                tuplet_specs.push(TupletSpec::triplet(start, rhythm_index));
                triplet_group_start = None;
                triplet_group_ticks = 0;
            }

            // Create RhythmEntry
            if is_rest {
                entries.push(RhythmEntry::Rest(duration));
            } else {
                entries.push(RhythmEntry::Note(duration));
            }
            rhythm_index += 1;

            // Close triplet group if we've hit a beat boundary
            if is_triplet && triplet_group_ticks >= TRIPLET_BEAT_TICKS {
                if let Some(start) = triplet_group_start {
                    tuplet_specs.push(TupletSpec::triplet(start, rhythm_index));
                    triplet_group_start = None;
                    triplet_group_ticks = 0;
                }
            }
        }
    } else {
        // Fallback: use chords only (no rests)
        for chord in &measure.chords {
            let (duration, is_rest, is_triplet) = extract_chord_rhythm_parts(chord);

            // Track triplet groups
            if is_triplet {
                if triplet_group_start.is_none() {
                    triplet_group_start = Some(rhythm_index);
                    triplet_group_ticks = 0;
                }
                triplet_group_ticks += duration.ticks();
            } else if let Some(start) = triplet_group_start {
                tuplet_specs.push(TupletSpec::triplet(start, rhythm_index));
                triplet_group_start = None;
                triplet_group_ticks = 0;
            }

            if is_rest {
                entries.push(RhythmEntry::Rest(duration));
            } else {
                entries.push(RhythmEntry::Note(duration));
            }
            rhythm_index += 1;

            // Close triplet group if we've hit a beat boundary
            if is_triplet && triplet_group_ticks >= TRIPLET_BEAT_TICKS {
                if let Some(start) = triplet_group_start {
                    tuplet_specs.push(TupletSpec::triplet(start, rhythm_index));
                    triplet_group_start = None;
                    triplet_group_ticks = 0;
                }
            }
        }
    }

    // Close any pending triplet group
    if let Some(start) = triplet_group_start {
        tuplet_specs.push(TupletSpec::triplet(start, rhythm_index));
    }

    let total_ticks: i32 = entries.iter().map(|e| e.duration().ticks()).sum();
    (entries, total_ticks, tuplet_specs)
}

/// Extract rhythm parts from a RhythmElement.
///
/// Returns (duration, is_rest, is_triplet)
fn extract_rhythm_parts(element: &RhythmElement) -> (Duration, bool, bool) {
    match element {
        RhythmElement::Chord(chord) => {
            if let Some((lily, dotted, triplet)) = chord.rhythm.lily_parts() {
                let dur = lily_syntax_to_duration(lily, dotted, triplet);
                (dur, false, triplet)
            } else {
                (Duration::Quarter, false, false)
            }
        }
        RhythmElement::Rest(rest) => {
            if let Some((lily, dotted, triplet)) = rest.rhythm.lily_parts() {
                let dur = lily_syntax_to_duration(lily, dotted, triplet);
                (dur, true, triplet)
            } else {
                (Duration::Quarter, true, false)
            }
        }
        RhythmElement::Space(_) => (Duration::Quarter, true, false),
    }
}

/// Extract rhythm parts from a ChordInstance.
///
/// Returns (duration, is_rest, is_triplet)
fn extract_chord_rhythm_parts(chord: &crate::ChordInstance) -> (Duration, bool, bool) {
    if let Some((lily, dotted, triplet)) = chord.rhythm.lily_parts() {
        let dur = lily_syntax_to_duration(lily, dotted, triplet);
        let is_rest = chord.rhythm.is_rest() || chord.rhythm.is_space();
        (dur, is_rest, triplet)
    } else {
        (Duration::Quarter, false, false)
    }
}

/// Build rhythm pattern with triplet support for pushed chords.
///
/// When a chord has a triplet push (pushed by 8th triplet), we render
/// that beat as [TripletQuarter, TripletEighth] with a triplet bracket,
/// where the chord symbol appears above the eighth.
///
/// Also handles spillback chords - chords from the next measure that push
/// back across the barline into this measure's last beat.
///
/// Returns (rhythm, total_ticks, tuplet_specs, spillback_chord_positions, internal_push_positions)
#[allow(clippy::type_complexity)]
#[must_use]
pub fn build_rhythm_with_triplets(
    measure: &Measure,
    num_beats: usize,
    spillbacks: Option<&[PushSpillback]>,
) -> (
    Vec<Duration>,
    i32,
    Vec<TupletSpec>,
    Vec<(usize, String)>,
    Vec<(usize, usize)>,
) {
    let mut rhythm = Vec::new();
    let mut tuplet_specs = Vec::new();
    let mut rhythm_index = 0;
    let mut spillback_chord_positions: Vec<(usize, String)> = Vec::new();
    let mut internal_push_positions: Vec<(usize, usize)> = Vec::new();

    // Build a list of beats, tracking which ones have pushed chords
    let mut beats_with_triplets: Vec<(bool, Option<usize>)> = vec![(false, None); num_beats];

    // Calculate natural beat positions for each chord
    let mut cumulative_beats = 0usize;
    for (chord_idx, chord) in measure.chords.iter().enumerate() {
        let is_triplet_push = chord.push_pull.as_ref().map_or(false, |(is_push, amount)| {
            *is_push && amount.base == PushPullBase::Triplet && amount.level == 1
        });

        let chord_duration_beats = match &chord.rhythm {
            ChordRhythm::Slashes { count, .. } => *count as usize,
            _ => 1,
        };

        if is_triplet_push && chord_idx > 0 {
            let target_beat = cumulative_beats.saturating_sub(1);
            if target_beat < num_beats {
                beats_with_triplets[target_beat] = (true, Some(chord_idx));
            }
        }

        cumulative_beats += chord_duration_beats;
    }

    // Check spillbacks from next measure
    if let Some(spills) = spillbacks {
        for spillback in spills {
            if spillback.push_base == PushPullBase::Triplet && spillback.push_level == 1 {
                let target_beat = spillback.beat_position;
                if target_beat < num_beats && !beats_with_triplets[target_beat].0 {
                    beats_with_triplets[target_beat] = (true, None);
                }
            }
        }
    }

    // Build the rhythm array
    for (beat_idx, (has_triplet, pushed_chord_idx)) in
        beats_with_triplets.iter().enumerate().take(num_beats)
    {
        if *has_triplet {
            let start_idx = rhythm_index;
            rhythm.push(Duration::TripletQuarter);
            rhythm.push(Duration::TripletEighth);

            if let Some(chord_idx) = pushed_chord_idx {
                internal_push_positions.push((*chord_idx, rhythm_index + 1));
            }

            if let Some(spills) = spillbacks {
                if let Some(spillback) = spills.iter().find(|s| s.beat_position == beat_idx) {
                    spillback_chord_positions.push((rhythm_index + 1, spillback.chord_symbol.clone()));
                }
            }

            rhythm_index += 2;
            let end_idx = rhythm_index;
            tuplet_specs.push(TupletSpec::triplet(start_idx, end_idx));
        } else {
            // Standard quarter note beat
            // Check for standard (non-triplet) spillbacks that should appear on this beat
            if let Some(spills) = spillbacks {
                if let Some(spillback) = spills.iter().find(|s| {
                    s.beat_position == beat_idx && s.push_base == PushPullBase::Standard
                }) {
                    // Record the chord position for the spillback
                    // For standard pushes, the chord symbol appears on the quarter note
                    spillback_chord_positions.push((rhythm_index, spillback.chord_symbol.clone()));
                }
            }

            rhythm.push(Duration::Quarter);
            rhythm_index += 1;
        }
    }

    let total_ticks: i32 = rhythm.iter().map(|d| d.ticks()).sum();
    (
        rhythm,
        total_ticks,
        tuplet_specs,
        spillback_chord_positions,
        internal_push_positions,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lily_syntax_to_duration_basic() {
        let dur = lily_syntax_to_duration(LilySyntax::Quarter, false, false);
        assert_eq!(dur.kind, DurationKind::Quarter);
        assert_eq!(dur.dots, 0);
        assert!(dur.tuplet.is_none());
    }

    #[test]
    fn test_lily_syntax_to_duration_dotted() {
        let dur = lily_syntax_to_duration(LilySyntax::Half, true, false);
        assert_eq!(dur.kind, DurationKind::Half);
        assert_eq!(dur.dots, 1);
        assert!(dur.tuplet.is_none());
    }

    #[test]
    fn test_lily_syntax_to_duration_triplet() {
        let dur = lily_syntax_to_duration(LilySyntax::Eighth, false, true);
        assert_eq!(dur.kind, DurationKind::Eighth);
        assert_eq!(dur.dots, 0);
        assert!(dur.tuplet.is_some());
    }

    #[test]
    fn test_lily_syntax_to_duration_all_note_values() {
        // Test all LilySyntax variants convert correctly
        assert_eq!(
            lily_syntax_to_duration(LilySyntax::Whole, false, false).kind,
            DurationKind::Whole
        );
        assert_eq!(
            lily_syntax_to_duration(LilySyntax::Half, false, false).kind,
            DurationKind::Half
        );
        assert_eq!(
            lily_syntax_to_duration(LilySyntax::Quarter, false, false).kind,
            DurationKind::Quarter
        );
        assert_eq!(
            lily_syntax_to_duration(LilySyntax::Eighth, false, false).kind,
            DurationKind::Eighth
        );
        assert_eq!(
            lily_syntax_to_duration(LilySyntax::Sixteenth, false, false).kind,
            DurationKind::Sixteenth
        );
        assert_eq!(
            lily_syntax_to_duration(LilySyntax::ThirtySecond, false, false).kind,
            DurationKind::ThirtySecond
        );
    }
}
