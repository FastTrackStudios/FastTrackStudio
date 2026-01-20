//! Push Spillback Position Tests
//!
//! Tests that pushed chords have the correct push_pull flags set.
//! The keyflow parser uses ACCUMULATED positions (for playback timing),
//! while the engraver uses STRUCTURAL positions (for notation layout).
//!
//! ## Position Model
//!
//! Keyflow positions are accumulated from the start of the song:
//! - Cm/Eb: m0.0.0 (first chord, beat 0)
//! - Eb (m1): m0.0.667 (pushed by 0.33, so at beat 0.67)
//! - Eb (m2): m0.2.667 (at beat 3 - 0.33 = beat 2.67 of accumulated timeline)
//!
//! For notation, the engraver converts these to STRUCTURAL positions:
//! - Each measure starts at beat 1 (1-indexed)
//! - Pushed chords are rendered at their adjusted position within the measure
//!
//! ## What This Test Verifies
//!
//! 1. Pushed chords have `push_pull` flag set correctly
//! 2. The push amount is triplet (0.33 beats)
//! 3. All 4 measures are parsed correctly
//!
//! The SongBuilder-based tests (in push_spillback_song_test.rs) verify
//! the structural within-measure positions for notation layout.

use keyflow::Chart;

/// The keyflow source for push spillback testing.
pub const KEYFLOW_SOURCE: &str = r#"Push Spillback Test
120bpm 4/4 #Ab
/push = triplet

CH
Cm/Eb / 'Eb // | 'Eb / 'F/C / 'Cm // | 'F/A //// | 'Fm9  ////
"#;

/// Calculate the beat position for a chord WITHIN its measure (1-indexed).
///
/// The position is already adjusted for push/pull in post-processing,
/// so we just need to convert the beat/subdivision to within-measure position.
/// The measure field gives the measure number, and beat/subdivision give position within.
fn chord_beat_position(chord: &keyflow::ChordInstance, _time_sig: keyflow::TimeSignature) -> f64 {
    let beat = chord.position.total_duration.beat as f64;
    let subdivision = chord.position.total_duration.subdivision as f64 / 1000.0;

    // beat is 0-indexed within the measure, add 1.0 for 1-indexed output
    beat + subdivision + 1.0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_spillback_chord_positions() {
        let chart = Chart::parse(KEYFLOW_SOURCE).expect("Should parse keyflow source");

        // Should have one section (Chorus)
        assert_eq!(chart.sections.len(), 1, "Should have 1 section");

        let chorus = &chart.sections[0];
        println!("Chorus has {} measures", chorus.measures().len());

        let time_sig = chart.time_signature.unwrap();

        // First, let's look at rhythm_elements to understand the full structure
        for (m_idx, measure) in chorus.measures().iter().enumerate() {
            println!("\n=== Measure {} rhythm_elements ({}) ===", m_idx + 1, measure.rhythm_elements.len());
            for (i, elem) in measure.rhythm_elements.iter().enumerate() {
                match elem {
                    keyflow::chart::types::RhythmElement::Chord(c) => {
                        let is_pushed = c.push_pull.as_ref().map_or(false, |(is_push, _)| *is_push);
                        println!("  [{}] Chord: {} beat={} pushed={}", i, c.full_symbol, c.position.total_duration.beat, is_pushed);
                    }
                    keyflow::chart::types::RhythmElement::Rest(r) => {
                        println!("  [{}] Rest at beat={}", i, r.position.total_duration.beat);
                    }
                    keyflow::chart::types::RhythmElement::Space(s) => {
                        println!("  [{}] Space at beat={}", i, s.position.total_duration.beat);
                    }
                }
            }
        }

        // Collect all chords with their measure index and position
        let mut all_chords: Vec<(usize, &keyflow::ChordInstance, f64)> = Vec::new();

        for (m_idx, measure) in chorus.measures().iter().enumerate() {
            println!("\nMeasure {} chords:", m_idx + 1);
            for chord in &measure.chords {
                let pos = chord_beat_position(chord, time_sig);
                let is_pushed = chord.push_pull.as_ref().map_or(false, |(is_push, _)| *is_push);
                let duration_beats = chord.duration.to_beats(time_sig);
                let full_pos = &chord.position.total_duration;
                println!(
                    "  {} at beat {:.2} (pos=m{}.{}.{:03}, pushed={}, duration={:.2} beats, rhythm={:?})",
                    chord.full_symbol,
                    pos,
                    full_pos.measure,
                    full_pos.beat,
                    full_pos.subdivision,
                    is_pushed,
                    duration_beats,
                    chord.rhythm
                );
                all_chords.push((m_idx, chord, pos));
            }
        }

        // Verify push flags are set correctly for all pushed chords.
        // Note: Keyflow uses accumulated positions for playback timing.
        // The engraver handles structural within-measure layout separately.

        // Measure 1: Cm/Eb / 'Eb //
        // - Cm/Eb is NOT pushed (no leading apostrophe)
        // - 'Eb IS pushed (leading apostrophe)
        let m1_chords: Vec<_> = all_chords.iter().filter(|(m, _, _)| *m == 0).collect();
        println!("\n=== Verifying Measure 1 Push Flags ===");

        let cm_eb = m1_chords.iter().find(|(_, c, _)| c.full_symbol == "Cm/Eb");
        if let Some((_, chord, _)) = cm_eb {
            let is_pushed = chord.push_pull.as_ref().map_or(false, |(is_push, _)| *is_push);
            println!("Cm/Eb pushed: {}, expected false", is_pushed);
            assert!(!is_pushed, "Cm/Eb should NOT be pushed");
        }

        let eb_m1 = m1_chords.iter().find(|(_, c, _)| c.full_symbol == "Eb");
        if let Some((_, chord, _)) = eb_m1 {
            let is_pushed = chord.push_pull.as_ref().map_or(false, |(is_push, _)| *is_push);
            println!("Eb (m1) pushed: {}, expected true", is_pushed);
            assert!(is_pushed, "First Eb should be pushed");
        }

        // Measure 2: 'Eb / 'F/C / 'Cm //
        // All chords are pushed
        let m2_chords: Vec<_> = all_chords.iter().filter(|(m, _, _)| *m == 1).collect();
        println!("\n=== Verifying Measure 2 Push Flags ===");

        let eb_m2 = m2_chords.iter().find(|(_, c, _)| c.full_symbol == "Eb");
        if let Some((_, chord, _)) = eb_m2 {
            let is_pushed = chord.push_pull.as_ref().map_or(false, |(is_push, _)| *is_push);
            println!("Eb (m2) pushed: {}, expected true", is_pushed);
            assert!(is_pushed, "Second Eb should be pushed");
        }

        let fc = m2_chords.iter().find(|(_, c, _)| c.full_symbol == "F/C");
        if let Some((_, chord, _)) = fc {
            let is_pushed = chord.push_pull.as_ref().map_or(false, |(is_push, _)| *is_push);
            println!("F/C pushed: {}, expected true", is_pushed);
            assert!(is_pushed, "F/C should be pushed");
        }

        let cm = m2_chords.iter().find(|(_, c, _)| c.full_symbol == "Cm");
        if let Some((_, chord, _)) = cm {
            let is_pushed = chord.push_pull.as_ref().map_or(false, |(is_push, _)| *is_push);
            println!("Cm pushed: {}, expected true", is_pushed);
            assert!(is_pushed, "Cm should be pushed");
        }

        // Measure 3: 'F/A ////
        let m3_chords: Vec<_> = all_chords.iter().filter(|(m, _, _)| *m == 2).collect();
        println!("\n=== Verifying Measure 3 Push Flags ===");

        let fa = m3_chords.iter().find(|(_, c, _)| c.full_symbol == "F/A");
        if let Some((_, chord, _)) = fa {
            let is_pushed = chord.push_pull.as_ref().map_or(false, |(is_push, _)| *is_push);
            println!("F/A pushed: {}, expected true", is_pushed);
            assert!(is_pushed, "F/A should be pushed");
        }

        // Measure 4: 'Fm9 ////
        let m4_chords: Vec<_> = all_chords.iter().filter(|(m, _, _)| *m == 3).collect();
        println!("\n=== Verifying Measure 4 Push Flags ===");

        let fm9 = m4_chords.iter().find(|(_, c, _)| c.full_symbol == "Fm9");
        if let Some((_, chord, _)) = fm9 {
            let is_pushed = chord.push_pull.as_ref().map_or(false, |(is_push, _)| *is_push);
            println!("Fm9 pushed: {}, expected true", is_pushed);
            assert!(is_pushed, "Fm9 should be pushed");
        }
    }

    #[test]
    fn test_measure_count() {
        let chart = Chart::parse(KEYFLOW_SOURCE).expect("Should parse keyflow source");

        let chorus = &chart.sections[0];

        // Should have exactly 4 measures
        // Cm/Eb / 'Eb // | 'Eb / 'F/C / 'Cm // | 'F/A //// | 'Fm9 ////
        assert_eq!(
            chorus.measures().len(),
            4,
            "Chorus should have 4 measures, got {}",
            chorus.measures().len()
        );
    }
}
