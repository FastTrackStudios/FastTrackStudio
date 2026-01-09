//! MIDI event integration for chord detection
//!
//! Provides utilities for converting MIDI events to chords and working with MIDI note data.

use crate::chord::{Chord, ChordDegree, ChordQuality, from_semitones};
use crate::primitives::{MusicalNote, RootNotation};
use helgoboss_midi::KeyNumber;

/// A MIDI note event with timing information
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MidiNote {
    /// MIDI pitch number (0-127)
    pub pitch: u8,
    /// Start time in PPQ (parts per quarter note)
    pub start_ppq: i64,
    /// End time in PPQ (parts per quarter note)
    pub end_ppq: i64,
    /// MIDI channel (0-15)
    pub channel: u8,
    /// Note velocity (0-127)
    pub velocity: u8,
}

impl MidiNote {
    /// Create a new MIDI note
    pub fn new(pitch: u8, start_ppq: i64, end_ppq: i64, channel: u8, velocity: u8) -> Self {
        Self {
            pitch,
            start_ppq,
            end_ppq,
            channel,
            velocity,
        }
    }

    /// Get the note name with octave (e.g., "C4", "A#3")
    pub fn note_name(&self) -> Option<MidiNoteName> {
        MidiNoteName::from_midi_pitch(self.pitch)
    }
}

/// MIDI note with note name and octave
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MidiNoteName {
    note: MusicalNote,
    octave: i32,
}

impl MidiNoteName {
    /// Create from MIDI pitch number (0-127)
    /// MIDI 0 = C-1, MIDI 1 = C#-1, MIDI 12 = C0, MIDI 60 = C4 (middle C), etc.
    pub fn from_midi_pitch(pitch: u8) -> Option<Self> {
        // Validate using helgoboss-midi
        KeyNumber::try_from(pitch).ok()?;

        let semitone = pitch % 12;
        let octave = (pitch / 12) as i32 - 1; // MIDI 0-11 is octave -1, 12-23 is octave 0, etc.
        let note = MusicalNote::from_semitone(semitone, true); // prefer sharp

        Some(Self { note, octave })
    }

    /// Get the note (without octave)
    pub fn note(&self) -> MusicalNote {
        self.note.clone()
    }

    /// Get the octave
    pub fn octave(&self) -> i32 {
        self.octave
    }
}

impl std::fmt::Display for MidiNoteName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.note, self.octave)
    }
}

/// Convert MIDI pitch to note name string
pub fn midi_pitch_to_note_name(pitch: u8) -> String {
    MidiNoteName::from_midi_pitch(pitch)
        .map(|n| n.to_string())
        .unwrap_or_else(|| format!("Invalid({})", pitch))
}

/// Detected chord with timing information
#[derive(Debug, Clone)]
pub struct DetectedChord {
    /// The detected chord
    pub chord: Chord,
    /// Start time in PPQ
    pub start_ppq: i64,
    /// End time in PPQ
    pub end_ppq: i64,
    /// Root pitch (MIDI note number)
    pub root_pitch: u8,
}

impl DetectedChord {
    /// Create a new detected chord
    pub fn new(chord: Chord, start_ppq: i64, end_ppq: i64, root_pitch: u8) -> Self {
        Self {
            chord,
            start_ppq,
            end_ppq,
            root_pitch,
        }
    }
}

/// Active note at a specific time (internal use)
#[derive(Debug, Clone)]
struct ActiveNote {
    pitch: u8,
    start_ppq: i64,
    end_ppq: i64,
}

/// Detect chords from MIDI notes using keyflow's semitone sequence analysis
///
/// Based on Lil Chordbox.lua's GetChords function:
/// - Process notes sequentially, tracking active notes
/// - Build chords when 2+ notes overlap
/// - Filter out very short chords (< min_chord_duration_ppq) to avoid arpeggiated fragments
/// - Merge consecutive identical chords
///
/// # Arguments
///
/// * `notes` - Slice of MIDI notes to analyze
/// * `min_chord_duration_ppq` - Minimum chord duration in PPQ to filter out arpeggiated fragments (e.g., 180)
///
/// # Returns
///
/// Vector of detected chords with timing information
pub fn detect_chords_from_midi_notes(
    notes: &[MidiNote],
    min_chord_duration_ppq: i64,
) -> Vec<DetectedChord> {
    if notes.is_empty() {
        return Vec::new();
    }

    // Sort notes by start time (like Lil Chordbox)
    let mut sorted_notes = notes.to_vec();
    sorted_notes.sort_by_key(|n| n.start_ppq);

    let mut chords = Vec::new();
    let mut active_notes: Vec<ActiveNote> = Vec::new();
    let mut chord_min_eppq: Option<i64> = None;

    // Process notes sequentially (like Lil Chordbox's GetChords)
    for note_info in &sorted_notes {
        let note = ActiveNote {
            pitch: note_info.pitch,
            start_ppq: note_info.start_ppq,
            end_ppq: note_info.end_ppq,
        };

        // Update chord_min_eppq (earliest end time of active notes)
        chord_min_eppq = chord_min_eppq
            .map(|min| min.min(note.end_ppq))
            .or(Some(note.end_ppq));

        // If this note starts after or at the earliest end time, process existing active notes
        if note.start_ppq >= chord_min_eppq.unwrap_or(0) {
            // Build chord from current active notes if we have 2+
            if active_notes.len() >= 2 {
                if let Some(chord) = build_chord_from_notes(
                    &active_notes,
                    chord_min_eppq.unwrap_or(0),
                    note.start_ppq,
                    min_chord_duration_ppq,
                ) {
                    chords.push(chord);
                }
            }

            // Remove notes that have ended before or at this note's start time
            // Use >= instead of > to ensure notes ending exactly when new note starts are removed
            let mut new_notes = Vec::new();
            chord_min_eppq = None;
            for active_note in &active_notes {
                if active_note.end_ppq > note.start_ppq {
                    // Note is still active - keep it
                    new_notes.push(active_note.clone());
                    chord_min_eppq = chord_min_eppq
                        .map(|min| min.min(active_note.end_ppq))
                        .or(Some(active_note.end_ppq));
                }
                // Notes ending exactly at note.start_ppq are removed (not >)
            }
            active_notes = new_notes;

            // Update chord_min_eppq with new note
            chord_min_eppq = chord_min_eppq
                .map(|min| min.min(note.end_ppq))
                .or(Some(note.end_ppq));
        } else {
            // Note starts before earliest end - build chord from current active notes
            // This happens when a new note starts while previous notes are still active
            if active_notes.len() >= 2 {
                // Build chord ending at this note's start
                if let Some(chord) = build_chord_from_notes(
                    &active_notes,
                    chord_min_eppq.unwrap_or(0),
                    note.start_ppq,
                    min_chord_duration_ppq,
                ) {
                    chords.push(chord);
                }
            }
        }

        // Add this note to active notes
        active_notes.push(note);
    }

    // Process remaining active notes at the end
    if active_notes.len() >= 2 {
        if let Some(chord) = build_chord_from_notes(
            &active_notes,
            chord_min_eppq.unwrap_or(0),
            i64::MAX,
            min_chord_duration_ppq,
        ) {
            chords.push(chord);
        }
    }

    // Merge consecutive identical chords (like Lil Chordbox)
    // But don't merge if chords start exactly when the previous one ends (separate musical events)
    let mut merged_chords: Vec<DetectedChord> = Vec::new();
    for chord in chords {
        if let Some(last_chord) = merged_chords.last_mut() {
            // Check if this chord is the same as the last one and overlapping (not just adjacent)
            // Only merge if there's actual overlap, not just adjacency
            let has_overlap = last_chord.end_ppq > chord.start_ppq;
            let is_same_chord = last_chord.root_pitch == chord.root_pitch
                && last_chord.chord.quality == chord.chord.quality
                && last_chord.chord.family == chord.chord.family;

            if is_same_chord && has_overlap {
                // Merge: extend the end time
                last_chord.end_ppq = last_chord.end_ppq.max(chord.end_ppq);
                continue;
            }
        }
        merged_chords.push(chord);
    }

    merged_chords
}

/// Build a chord from active notes (helper function)
///
/// This function tries each note as a potential root to detect inversions,
/// similar to Lil Chordbox's IdentifyChord function.
fn build_chord_from_notes(
    active_notes: &[ActiveNote],
    chord_start_ppq: i64,
    chord_end_limit: i64,
    min_chord_duration_ppq: i64,
) -> Option<DetectedChord> {
    if active_notes.len() < 2 {
        return None;
    }

    // Get pitches and sort them
    let mut pitches: Vec<u8> = active_notes.iter().map(|n| n.pitch).collect();
    pitches.sort();

    // Find the earliest start and earliest end of active notes (clamped to limit)
    // We use the minimum end time to determine when the chord ends (when the first note releases)
    let chord_start = active_notes
        .iter()
        .map(|n| n.start_ppq)
        .min()
        .unwrap_or(chord_start_ppq);
    let chord_end = active_notes
        .iter()
        .map(|n| n.end_ppq)
        .min()
        .unwrap_or(chord_end_limit)
        .min(chord_end_limit);

    // Filter out very short chords (arpeggiated fragments)
    // Lil Chordbox uses 180 ticks for arpeggiated, 240 for others
    // Note: If notes have very different end times, the chord duration might be shorter than expected
    // This is correct behavior - the chord ends when the first note releases
    let chord_duration = chord_end - chord_start;
    // Allow chords that are exactly at the minimum duration (>= instead of >)
    if chord_duration < min_chord_duration_ppq {
        return None;
    }

    // Try each pitch as a potential root (check inversions)
    // Like Lil Chordbox, we check inversions by trying each note as root
    // But prefer the simplest/most standard chord name
    let note_names = [
        "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B",
    ];

    let lowest_pitch = pitches[0];
    let mut all_chords: Vec<(Chord, u8, u32)> = Vec::new(); // (chord, root_pitch, simplicity_score)

    // Try each note as a potential root
    for &potential_root_pitch in &pitches {
        let potential_root_class = potential_root_pitch % 12;

        if let Some(chord) = try_build_chord_with_root(
            &pitches,
            potential_root_pitch,
            potential_root_class,
            &note_names,
        ) {
            // Score the chord for simplicity (lower is better)
            let score = chord_simplicity_score(&chord);
            all_chords.push((chord, potential_root_pitch, score));
        }
    }

    if all_chords.is_empty() {
        return None;
    }

    // Check if the lowest pitch forms a simple, standard chord (no alterations, standard quality)
    // If it does, heavily prefer it over inversions
    let lowest_pitch_forms_simple_chord = all_chords
        .iter()
        .find(|(chord, root_pitch, _)| {
            *root_pitch == lowest_pitch 
            && chord.alterations.is_empty() // No alterations
            && (chord.quality == ChordQuality::Major || chord.quality == ChordQuality::Minor || chord.quality == ChordQuality::Power) // Standard quality
        })
        .is_some();

    // Add penalty for inversions (root not at lowest pitch) to the score
    // Also prefer the lowest pitch of the same pitch class when multiple roots are possible
    let all_chords_with_penalty: Vec<(Chord, u8, u32)> = all_chords
        .iter()
        .map(|(chord, root_pitch, score)| {
            let mut adjusted_score = *score;
            let root_pitch_class = *root_pitch % 12;
            let lowest_pitch_class = lowest_pitch % 12;

            // If the lowest pitch forms a simple, standard chord, heavily penalize inversions
            // This ensures we prefer Bm7 (root at B2) over D7/B or Asus2/B
            // But if the lowest pitch forms a complex/altered chord, prefer simpler inversions
            if lowest_pitch_forms_simple_chord && *root_pitch != lowest_pitch {
                // Heavy penalty for inversions when lowest pitch forms a simple chord
                adjusted_score += 20; // Very heavy penalty to prefer root position
            } else if *root_pitch != lowest_pitch {
                // Normal inversion penalty when lowest pitch doesn't form a simple chord
                adjusted_score += 5;
            }

            // Also prefer the lowest pitch of the same pitch class
            // If root pitch class matches lowest, but root pitch is higher, add penalty
            if root_pitch_class == lowest_pitch_class && *root_pitch > lowest_pitch {
                adjusted_score += 3; // Penalty for higher octave of same pitch class
            }

            // Also heavily penalize suspended chords when there's a better interpretation available
            // (like a 7th chord)
            if chord.quality.is_suspended() && chord.family.is_none() {
                // Check if any other interpretation has a family (7th, etc.)
                let has_better_interpretation =
                    all_chords.iter().any(|(c, _, _)| c.family.is_some());
                if has_better_interpretation {
                    adjusted_score += 10; // Heavy penalty for suspended when 7th chord exists
                }
            }
            (chord.clone(), *root_pitch, adjusted_score)
        })
        .collect();

    // Sort by adjusted simplicity score (lower is better), then prefer root position (lowest pitch)
    let mut sorted_chords = all_chords_with_penalty;
    sorted_chords.sort_by(|a, b| {
        a.2.cmp(&b.2) // Sort by adjusted simplicity score
            .then_with(|| {
                // If scores are equal, prefer root position (lowest pitch as root)
                let a_is_root = a.1 == lowest_pitch;
                let b_is_root = b.1 == lowest_pitch;
                b_is_root.cmp(&a_is_root) // true < false, so root position comes first
            })
            .then_with(|| {
                // If still equal, prefer lower pitch as root (even if same pitch class)
                a.1.cmp(&b.1)
            })
    });

    // Use the simplest chord
    let (mut chord, root_pitch, _score) = sorted_chords.remove(0);

    // If the root is not the lowest pitch, it's an inversion - set bass note
    // But only if the bass note is actually different from the root (pitch class)
    if root_pitch != lowest_pitch {
        let root_pitch_class = root_pitch % 12;
        let bass_pitch_class = lowest_pitch % 12;

        // Only set bass if it's a different pitch class (actual inversion)
        if root_pitch_class != bass_pitch_class {
            let bass_note_name = note_names[bass_pitch_class as usize];
            if let Some(bass_note) = MusicalNote::from_string(bass_note_name) {
                chord.bass = Some(RootNotation::from_note_name(bass_note));
            }
        }
    }

    Some(DetectedChord {
        chord,
        start_ppq: chord_start,
        end_ppq: chord_end,
        root_pitch,
    })
}

/// Score a chord for simplicity (lower is better)
/// Prefers standard triads, then simple extensions, over complex altered chords
fn chord_simplicity_score(chord: &Chord) -> u32 {
    let mut score = 0u32;

    // Base quality scores (lower is better)
    match chord.quality {
        ChordQuality::Major | ChordQuality::Minor => score += 0, // Simplest
        ChordQuality::Power => score += 1,
        ChordQuality::Suspended(_) => score += 3, // Penalize suspended chords more - they're often misinterpretations
        ChordQuality::Diminished | ChordQuality::Augmented => score += 3,
    }

    // Family (7th) adds complexity
    if chord.family.is_some() {
        score += 5;
    }

    // Extensions add complexity
    if chord.extensions.ninth.is_some() {
        score += 3;
    }
    if chord.extensions.eleventh.is_some() {
        score += 3;
    }
    if chord.extensions.thirteenth.is_some() {
        score += 3;
    }

    // Additions are simpler than extensions
    score += chord.additions.len() as u32;

    // Alterations add significant complexity
    score += chord.alterations.len() as u32 * 10;

    score
}

/// Convert a vector of MIDI pitches to a semitone sequence relative to a root pitch
///
/// This function takes MIDI note pitches and converts them to semitone intervals
/// relative to a specified root pitch. It preserves octave information for extensions
/// (9th, 11th, 13th) while normalizing pitch classes for the base chord.
///
/// # Arguments
///
/// * `pitches` - Vector of MIDI pitch values (0-127)
/// * `root_pitch` - The MIDI pitch to use as the root (0)
///
/// # Returns
///
/// A sorted, deduplicated vector of semitone intervals relative to the root.
/// The root (0) is always included. Extensions are represented with their
/// compound interval values (14 for major 9th, 17 for 11th, 21 for 13th, etc.)
///
/// # Examples
///
/// ```
/// use keyflow::chord::midi::midi_pitches_to_semitone_sequence;
///
/// // C major triad: C4 (60), E4 (64), G4 (67)
/// let pitches = vec![60, 64, 67];
/// let semitones = midi_pitches_to_semitone_sequence(&pitches, 60);
/// assert_eq!(semitones, vec![0, 4, 7]);
///
/// // E major with added 4th: E2 (40), G#3 (56), A3 (57), B3 (59)
/// let pitches = vec![40, 56, 57, 59];
/// let semitones = midi_pitches_to_semitone_sequence(&pitches, 40);
/// // Should have 0 (E), 4 (G#), 5 (A), 7 (B)
/// assert!(semitones.contains(&0));
/// assert!(semitones.contains(&4));
/// assert!(semitones.contains(&5));
/// assert!(semitones.contains(&7));
/// ```
pub fn midi_pitches_to_semitone_sequence(pitches: &[u8], root_pitch: u8) -> Vec<u8> {
    let mut semitones: Vec<u8> = pitches
        .iter()
        .map(|&pitch| {
            // Calculate semitone difference from root (handling negative differences)
            let total_diff = pitch as i16 - root_pitch as i16;
            let diff = total_diff % 12;
            let diff = if diff < 0 { diff + 12 } else { diff };

            // For notes in higher octaves, preserve octave info ONLY for extensions (9th, 11th, 13th)
            // Basic chord tones (3rd, 4th, 5th, 7th) should use simple intervals, even in higher octaves
            // This prevents:
            // - A3 (minor 3rd) from being interpreted as #9 when F#2 is the root
            // - A3 (4th) from being interpreted as 11th when E2 is the root and G#3 (3rd) is in the same octave
            let octave_diff = total_diff / 12;
            if octave_diff > 0 && diff > 0 {
                // Only convert to compound intervals for actual extensions (9th, 11th, 13th)
                // Basic chord tones (3rd=3/4, 4th=5, 5th=7, 7th=10/11) should use simple intervals
                // We'll check later if 3rd and 4th are in same octave to determine add4 vs 11th
                match diff {
                    1 => 13,          // minor 9th (extension)
                    2 => 14,          // major 9th (extension)
                    3 => diff as u8,  // minor 3rd - keep as simple interval, not sharp 9th
                    4 => diff as u8,  // major 3rd - keep as simple interval, not minor 10th
                    5 => diff as u8, // perfect 4th - keep as simple interval, we'll check later if it's 11th
                    6 => 18, // sharp 11th (extension - this is always an extension, not a basic chord tone)
                    7 => diff as u8, // perfect 5th - keep as simple interval, not minor 12th
                    8 => diff as u8, // augmented 5th - keep as simple interval, not perfect 12th
                    9 => 21, // minor 13th (extension)
                    10 => diff as u8, // minor 7th - keep as simple interval, not major 13th
                    11 => diff as u8, // major 7th - keep as simple interval, not minor 14th
                    _ => diff as u8,
                }
            } else {
                // Same octave or lower - use simple interval
                diff as u8
            }
        })
        .collect();

    // Sort and deduplicate
    semitones.sort();
    semitones.dedup();

    // Ensure 0 (root) is included - from_semitones requires it
    if !semitones.contains(&0) {
        semitones.insert(0, 0);
        semitones.sort();
    }

    semitones
}

/// Try to build a chord using a specific pitch as the root
fn try_build_chord_with_root(
    pitches: &[u8],
    root_pitch: u8,
    root_pitch_class: u8,
    note_names: &[&str; 12],
) -> Option<Chord> {
    // Convert pitches to semitone sequence relative to the root
    let semitones = midi_pitches_to_semitone_sequence(pitches, root_pitch);

    // Convert root pitch to MusicalNote and RootNotation
    let note_name = note_names[root_pitch_class as usize];

    if let Some(root_note) = MusicalNote::from_string(note_name) {
        let root = RootNotation::from_note_name(root_note);

        // Use keyflow's from_semitones to detect chord
        if let Ok(mut chord) = from_semitones(&semitones, root.clone()) {
            // Get pitch classes to check for specific intervals
            let pitch_classes: std::collections::HashSet<u8> =
                semitones.iter().map(|&s| s % 12).collect();
            let has_major_third = pitch_classes.contains(&4);
            let has_fourth = pitch_classes.contains(&5);

            // Check if 3rd and 4th are in the same octave by examining actual MIDI pitches
            // Find pitches that correspond to major 3rd (pitch class 4) and 4th (pitch class 5)
            let third_pitches: Vec<u8> = pitches
                .iter()
                .filter(|&&p| (p % 12) == ((root_pitch % 12) + 4) % 12)
                .copied()
                .collect();
            let fourth_pitches: Vec<u8> = pitches
                .iter()
                .filter(|&&p| (p % 12) == ((root_pitch % 12) + 5) % 12)
                .copied()
                .collect();

            // Check if 3rd and 4th are in the same octave (within 12 semitones of each other)
            let third_and_fourth_same_octave =
                if !third_pitches.is_empty() && !fourth_pitches.is_empty() {
                    // Get the octave of the 3rd and 4th pitches (divide by 12)
                    let third_octave = third_pitches[0] / 12;
                    let fourth_octave = fourth_pitches[0] / 12;
                    third_octave == fourth_octave
                } else {
                    // Fallback: check if we have both simple intervals (same octave)
                    let has_third_simple = semitones.contains(&4);
                    let has_fourth_simple = semitones.contains(&5);
                    has_third_simple && has_fourth_simple
                };

            // Fix: If we have both a 3rd AND a 4th, it's "add4" not "sus4"
            // Sus4 means the 4th REPLACES the 3rd, but add4 means the 4th is ADDED to the 3rd
            if has_major_third && has_fourth && chord.quality.is_suspended() {
                // Convert sus4 to major with add4
                chord.quality = crate::chord::ChordQuality::Major;
            }

            // Fix: If we have root (0), 5th (7), and 2nd (2) but no 3rd, it's "D2" not "Dsus2" or "D5add2"
            // "D2" means root + 5th + 2nd (NOT a power chord, since power chord is ONLY root + 5th)
            // Check if we have root (0), 5th (7), and 2nd (2 or 14) but no 3rd
            let has_root = semitones.contains(&0);
            let has_fifth = pitch_classes.contains(&7);
            let has_second = pitch_classes.contains(&2);
            let has_third = pitch_classes.contains(&3) || pitch_classes.contains(&4);

            if has_root && has_fifth && has_second && !has_third && chord.quality.is_suspended() {
                // This is a "2" chord (root + 5th + 2nd), not a suspended chord
                // Use Power quality as base (root + 5th), but it's not a pure power chord since it has the 2nd
                chord.quality = crate::chord::ChordQuality::Power;
                // Remove the suspended quality and add the 2nd as an addition
                // Remove Ninth if present (same note as Second, just different octave)
                chord.additions.retain(|&d| d != ChordDegree::Ninth);
                if !chord.additions.contains(&ChordDegree::Second) {
                    chord.additions.push(ChordDegree::Second);
                }
            }

            // Check if this should be an "add" chord instead of an extension
            // If there's a 4th/6th/9th without a 7th, it's an "add" chord
            if chord.family.is_none() {
                // No 7th - check if we have extensions that should be additions
                if chord.extensions.ninth.is_some() {
                    chord.additions.push(ChordDegree::Ninth);
                    chord.extensions.ninth = None;
                }
                if chord.extensions.eleventh.is_some() {
                    // 11th extension: only if 4th is in a different octave from the 3rd
                    // If 17 (11th) and 16 (3rd in next octave) are both present → add4 (same octave)
                    // If 17 is present but 16 is NOT and 12 is NOT → 11th extension (keep it)
                    if third_and_fourth_same_octave {
                        // Both 3rd and 4th in same octave - convert to add4 (use Fourth, not Eleventh)
                        chord.additions.push(ChordDegree::Fourth);
                        chord.extensions.eleventh = None;
                    } else {
                        // Different octaves - convert to add11 (use Eleventh)
                        chord.additions.push(ChordDegree::Eleventh);
                        chord.extensions.eleventh = None;
                    }
                }
                if chord.extensions.thirteenth.is_some() {
                    // 13th without 7th is usually add13, but check if it's actually a 6th
                    let has_sixth = pitch_classes.contains(&9);
                    if has_sixth {
                        chord.additions.push(ChordDegree::Thirteenth);
                        chord.extensions.thirteenth = None;
                    }
                }
            }

            // Also handle case where we have a 4th pitch class but it wasn't detected as extension
            // If we have both 3rd and 4th in same octave, it should be add4
            if chord.family.is_none()
                && third_and_fourth_same_octave
                && chord.extensions.eleventh.is_none()
            {
                // We have 3rd and 4th in same octave, but no 11th extension detected
                // This means it should be add4 (use Fourth, not Eleventh)
                if !chord.additions.contains(&ChordDegree::Fourth) {
                    chord.additions.push(ChordDegree::Fourth);
                }
            }

            // Consolidate Second and Ninth - if both are present, keep only Second
            // (They're the same note in different octaves)
            if chord.additions.contains(&ChordDegree::Second)
                && chord.additions.contains(&ChordDegree::Ninth)
            {
                chord.additions.retain(|&d| d != ChordDegree::Ninth);
            }

            return Some(chord);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chord::{ChordDegree, ChordQuality};

    fn create_midi_note(pitch: u8, start_ppq: i64, end_ppq: i64) -> MidiNote {
        MidiNote::new(pitch, start_ppq, end_ppq, 0, 100)
    }

    #[test]
    fn test_midi_pitches_to_semitone_sequence() {
        // C major triad: C4 (60), E4 (64), G4 (67)
        let pitches = vec![60, 64, 67];
        let semitones = midi_pitches_to_semitone_sequence(&pitches, 60);
        assert_eq!(semitones, vec![0, 4, 7]);

        // E major with added 4th: E2 (40), G#3 (56), A3 (57), B3 (59)
        // Note: G#3 is in a higher octave, so it becomes compound interval 16 (minor 10th)
        // But we still need the pitch class 4 (major 3rd) for chord detection
        let pitches = vec![40, 56, 57, 59];
        let semitones = midi_pitches_to_semitone_sequence(&pitches, 40);
        // Should have 0 (E), 5 (A), 7 (B)
        // G# might be 4 or 16 depending on octave handling
        assert!(semitones.contains(&0));
        assert!(semitones.contains(&5) || semitones.iter().any(|&s| s % 12 == 5)); // A (4th)
        assert!(semitones.contains(&7) || semitones.iter().any(|&s| s % 12 == 7)); // B (5th)
        // G# should be present as either 4 (major 3rd) or 16 (minor 10th)
        assert!(
            semitones.iter().any(|&s| s % 12 == 4),
            "Should have G# (major 3rd), got: {:?}",
            semitones
        );
    }

    #[test]
    fn test_e_add4_chord() {
        // E2, E3, G#3, A3, B3 should be EAdd4, not E11
        // E = 40, G# = 56, A = 57, B = 59
        let notes = vec![
            create_midi_note(40, 0, 5760), // E2
            create_midi_note(52, 0, 5760), // E3
            create_midi_note(56, 0, 5760), // G#3
            create_midi_note(57, 0, 5760), // A3
            create_midi_note(59, 0, 5760), // B3
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        assert!(!chords.is_empty(), "Should detect a chord");

        let chord = &chords[0].chord;
        let chord_name = chord.to_string();

        // Debug output
        println!("Detected chord: {}", chord_name);
        println!("Root pitch: {}", chords[0].root_pitch);
        println!("Quality: {:?}", chord.quality);
        println!("Family: {:?}", chord.family);
        println!("Extensions: {:?}", chord.extensions);
        println!("Additions: {:?}", chord.additions);

        // Check what semitones were generated for the selected root
        let root_pitch = chords[0].root_pitch;
        let semitones = midi_pitches_to_semitone_sequence(&[40, 52, 56, 57, 59], root_pitch);
        println!("Root pitch: {}, Semitones: {:?}", root_pitch, semitones);
        println!("Has 16 (3rd compound): {}", semitones.contains(&16));
        println!("Has 17 (11th compound): {}", semitones.contains(&17));
        println!("Has 4 (3rd simple): {}", semitones.contains(&4));
        println!("Has 5 (4th simple): {}", semitones.contains(&5));

        // Should be exactly Eadd4 (E major with added 4th)
        assert_eq!(
            chord_name, "Eadd4",
            "Should be exactly Eadd4, not Eadd11. Additions: {:?}, Extensions: {:?}, Name: {}",
            chord.additions, chord.extensions, chord_name
        );
        assert_eq!(
            chord.quality,
            ChordQuality::Major,
            "Should be exactly Major quality, got {:?}",
            chord.quality
        );
        assert_eq!(
            chord.family, None,
            "Should have exactly no 7th family, got {:?}",
            chord.family
        );
        assert_eq!(
            chords[0].root_pitch, 40,
            "Root should be exactly E2 (40), got {}",
            chords[0].root_pitch
        );
        assert_eq!(
            chord.additions,
            vec![ChordDegree::Fourth],
            "Should have exactly [Fourth] in additions, got {:?}",
            chord.additions
        );
        assert_eq!(
            chord.extensions.eleventh, None,
            "Should have exactly no 11th extension, got {:?}",
            chord.extensions.eleventh
        );
    }

    #[test]
    fn test_d2_power_chord_with_added_second() {
        // D3, A3, D4, E4, A4 should be D2 (power chord with added 2nd), not Asus4/D
        // Pattern: root (D), 5th (A), root (D), 2nd (E), 5th (A)
        // D3 = 50, A3 = 57, D4 = 62, E4 = 64, A4 = 69
        let notes = vec![
            create_midi_note(50, 0, 5760), // D3
            create_midi_note(57, 0, 5760), // A3
            create_midi_note(62, 0, 5760), // D4
            create_midi_note(64, 0, 5760), // E4
            create_midi_note(69, 0, 5760), // A4
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        assert!(!chords.is_empty(), "Should detect a chord");

        let chord = &chords[0].chord;
        let chord_name = chord.to_string();

        // Debug output
        println!("Detected chord: {}", chord_name);
        println!("Root pitch: {}", chords[0].root_pitch);
        println!("Quality: {:?}", chord.quality);
        println!("Additions: {:?}", chord.additions);

        // Should be exactly D2 (power chord with added 2nd), not Asus4/D
        assert_eq!(
            chord_name, "D2",
            "Should be exactly D2, not Asus4/D. Root pitch: {}, Additions: {:?}, Name: {}",
            chords[0].root_pitch, chord.additions, chord_name
        );
        assert_eq!(
            chords[0].root_pitch, 50,
            "Root should be exactly D3 (50), got {}",
            chords[0].root_pitch
        );
        assert_eq!(
            chord.quality,
            ChordQuality::Power,
            "Should be exactly Power quality, got {:?}",
            chord.quality
        );
        assert_eq!(
            chord.family, None,
            "Should have exactly no 7th family, got {:?}",
            chord.family
        );
        assert_eq!(
            chord.additions,
            vec![ChordDegree::Second],
            "Should have exactly [Second] in additions, got {:?}",
            chord.additions
        );
    }

    #[test]
    fn test_b_minor_7th_not_a_sus2() {
        // B2, F#3, B3, D4, F#4, A4 should be Bm7, not Asus2/B
        // B2 = 47, F#3 = 54, B3 = 59, D4 = 62, F#4 = 66, A4 = 69
        // Pattern: root (B), 5th (F#), root (B), minor 3rd (D), 5th (F#), minor 7th (A)
        let notes = vec![
            create_midi_note(47, 0, 2880), // B2
            create_midi_note(54, 0, 2880), // F#3
            create_midi_note(59, 0, 2880), // B3
            create_midi_note(62, 0, 2880), // D4
            create_midi_note(66, 0, 2880), // F#4
            create_midi_note(69, 0, 2880), // A4
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        assert!(!chords.is_empty(), "Should detect a chord");

        let chord = &chords[0].chord;
        let chord_name = chord.to_string();

        // Debug output
        println!("Detected chord: {}", chord_name);
        println!("Root pitch: {}", chords[0].root_pitch);
        println!("Quality: {:?}", chord.quality);
        println!("Family: {:?}", chord.family);
        println!("Additions: {:?}", chord.additions);

        // Should be exactly Bm7, not Bm7#9, Asus2/B or D7/B
        // The root should be B2 (47), the lowest pitch
        assert_eq!(
            chord_name, "Bm7",
            "Should be exactly Bm7, not Bm7#9, Asus2/B or D7/B. Root pitch: {}, Quality: {:?}, Family: {:?}, Extensions: {:?}, Name: {}",
            chords[0].root_pitch, chord.quality, chord.family, chord.extensions, chord_name
        );
        assert_eq!(
            chords[0].root_pitch, 47,
            "Root should be exactly B2 (47), got {}",
            chords[0].root_pitch
        );
        assert_eq!(
            chord.quality,
            ChordQuality::Minor,
            "Should be exactly Minor quality, got {:?}",
            chord.quality
        );
        assert_eq!(
            chord.family,
            Some(crate::chord::ChordFamily::Minor7),
            "Should have exactly Minor7 family, got {:?}",
            chord.family
        );
        assert_eq!(
            chord.extensions.ninth, None,
            "Should not have 9th extension, got {:?}",
            chord.extensions.ninth
        );
    }

    #[test]
    fn test_f_sharp_minor_no_false_sharp_9() {
        // F#2, F#3, A3, C#4 should be F#m, not F#m#9
        // F#2 = 42, F#3 = 54, A3 = 57, C#4 = 61
        // Pattern: root (F#), root (F#), minor 3rd (A), 5th (C#)
        let notes = vec![
            create_midi_note(42, 0, 2880), // F#2
            create_midi_note(54, 0, 2880), // F#3
            create_midi_note(57, 0, 2880), // A3 (minor 3rd)
            create_midi_note(61, 0, 2880), // C#4 (5th)
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        assert!(!chords.is_empty(), "Should detect a chord");

        let chord = &chords[0].chord;
        let chord_name = chord.to_string();

        // Debug output
        println!("Detected chord: {}", chord_name);
        println!("Root pitch: {}", chords[0].root_pitch);
        println!("Quality: {:?}", chord.quality);
        println!("Extensions: {:?}", chord.extensions);

        // Should be exactly F#m, not F#m#9
        assert_eq!(
            chord_name, "F#m",
            "Should be exactly F#m, not F#m#9. Root pitch: {}, Quality: {:?}, Extensions: {:?}, Name: {}",
            chords[0].root_pitch, chord.quality, chord.extensions, chord_name
        );
        assert_eq!(
            chords[0].root_pitch, 42,
            "Root should be exactly F#2 (42), got {}",
            chords[0].root_pitch
        );
        assert_eq!(
            chord.quality,
            ChordQuality::Minor,
            "Should be exactly Minor quality, got {:?}",
            chord.quality
        );
        assert_eq!(
            chord.family, None,
            "Should have exactly no 7th family, got {:?}",
            chord.family
        );
        assert_eq!(
            chord.extensions.ninth, None,
            "Should have exactly no 9th extension (including #9), got {:?}",
            chord.extensions.ninth
        );
    }

    #[test]
    fn test_f_sharp_minor_7th_no_false_sharp_9() {
        // F#2, F#3, A3, C#4, E4 should be F#m7, not F#m7#9
        // F#2 = 42, F#3 = 54, A3 = 57, C#4 = 61, E4 = 64
        // Pattern: root (F#), root (F#), minor 3rd (A), 5th (C#), minor 7th (E)
        let notes = vec![
            create_midi_note(42, 0, 2880), // F#2
            create_midi_note(54, 0, 2880), // F#3
            create_midi_note(57, 0, 2880), // A3 (minor 3rd)
            create_midi_note(61, 0, 2880), // C#4 (5th)
            create_midi_note(64, 0, 2880), // E4 (minor 7th)
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        assert!(!chords.is_empty(), "Should detect a chord");

        let chord = &chords[0].chord;
        let chord_name = chord.to_string();

        // Debug output
        println!("Detected chord: {}", chord_name);
        println!("Root pitch: {}", chords[0].root_pitch);
        println!("Quality: {:?}", chord.quality);
        println!("Family: {:?}", chord.family);
        println!("Extensions: {:?}", chord.extensions);

        // Should be F#m7, not F#m7#9 - EXACT match required
        assert_eq!(
            chord_name, "F#m7",
            "Should be exactly F#m7, not F#m7#9. Root pitch: {}, Quality: {:?}, Family: {:?}, Extensions: {:?}, Name: {}",
            chords[0].root_pitch, chord.quality, chord.family, chord.extensions, chord_name
        );
        assert_eq!(
            chords[0].root_pitch, 42,
            "Root should be exactly F#2 (42), got {}",
            chords[0].root_pitch
        );
        assert_eq!(
            chord.quality,
            ChordQuality::Minor,
            "Should be exactly Minor quality, got {:?}",
            chord.quality
        );
        assert_eq!(
            chord.family,
            Some(crate::chord::ChordFamily::Minor7),
            "Should have exactly Minor7 family, got {:?}",
            chord.family
        );
        assert_eq!(
            chord.extensions.ninth, None,
            "Should not have a 9th extension (including #9), got {:?}",
            chord.extensions.ninth
        );
    }

    #[test]
    fn test_f_sharp_minor_7th_short_duration() {
        // F#2, F#3, A3, C#4, E4 should be F#m7 even with short duration
        // This tests the case where chord might be filtered out due to duration
        // F#2 = 42, F#3 = 54, A3 = 57, C#4 = 61, E4 = 64
        let notes = vec![
            create_midi_note(42, 0, 180), // F#2 - very short duration
            create_midi_note(54, 0, 180), // F#3
            create_midi_note(57, 0, 180), // A3
            create_midi_note(61, 0, 180), // C#4
            create_midi_note(64, 0, 180), // E4
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        // Should still detect the chord even with minimum duration
        assert!(
            !chords.is_empty(),
            "Should detect F#m7 even with short duration"
        );

        let chord = &chords[0].chord;
        let chord_name = chord.to_string();

        // Should be exactly F#m7
        assert_eq!(
            chord_name, "F#m7",
            "Should be exactly F#m7. Root pitch: {}, Quality: {:?}, Family: {:?}, Name: {}",
            chords[0].root_pitch, chord.quality, chord.family, chord_name
        );
        assert_eq!(
            chords[0].root_pitch, 42,
            "Root should be exactly F#2 (42), got {}",
            chords[0].root_pitch
        );
        assert_eq!(
            chord.quality,
            ChordQuality::Minor,
            "Should be exactly Minor quality, got {:?}",
            chord.quality
        );
        assert_eq!(
            chord.family,
            Some(crate::chord::ChordFamily::Minor7),
            "Should have exactly Minor7 family, got {:?}",
            chord.family
        );
    }

    #[test]
    fn test_f_sharp_minor_7th_different_end_times() {
        // F#2, F#3, A3, C#4, E4 should be F#m7 even when notes have slightly different end times
        // This tests the case where one note ends earlier, reducing the chord duration
        // F#2 = 42, F#3 = 54, A3 = 57, C#4 = 61, E4 = 64
        let notes = vec![
            create_midi_note(42, 0, 180),  // F#2 - ends early
            create_midi_note(54, 0, 2880), // F#3 - longer duration
            create_midi_note(57, 0, 2880), // A3 - longer duration
            create_midi_note(61, 0, 2880), // C#4 - longer duration
            create_midi_note(64, 0, 2880), // E4 - longer duration
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        // Should still detect the chord even when one note ends early
        // The chord duration will be 180 (minimum end time), which is exactly the minimum
        assert!(
            !chords.is_empty(),
            "Should detect F#m7 even when one note ends early. Chord duration might be shorter but should still be >= 180"
        );

        let chord = &chords[0].chord;
        let chord_name = chord.to_string();

        // Should be exactly F#m7
        assert_eq!(
            chord_name, "F#m7",
            "Should be exactly F#m7. Root pitch: {}, Quality: {:?}, Family: {:?}, Name: {}",
            chords[0].root_pitch, chord.quality, chord.family, chord_name
        );
        assert_eq!(
            chords[0].root_pitch, 42,
            "Root should be exactly F#2 (42), got {}",
            chords[0].root_pitch
        );
        assert_eq!(
            chord.quality,
            ChordQuality::Minor,
            "Should be exactly Minor quality, got {:?}",
            chord.quality
        );
        assert_eq!(
            chord.family,
            Some(crate::chord::ChordFamily::Minor7),
            "Should have exactly Minor7 family, got {:?}",
            chord.family
        );
    }

    #[test]
    fn test_f_sharp_minor_7th_staggered_start_times() {
        // F#2, F#3, A3, C#4, E4 should be F#m7 even when notes have slightly staggered start times
        // This simulates the real-world scenario where notes might not start at exactly the same time
        // F#2 = 42, F#3 = 54, A3 = 57, C#4 = 61, E4 = 64
        // Start times: 72000, 72000, 72001, 72002, 72003 (slightly staggered)
        // End times: all 74880 (same end time)
        let notes = vec![
            create_midi_note(42, 72000, 74880), // F#2 - starts first
            create_midi_note(54, 72000, 74880), // F#3 - starts at same time
            create_midi_note(57, 72001, 74880), // A3 - starts 1 tick later
            create_midi_note(61, 72002, 74880), // C#4 - starts 2 ticks later
            create_midi_note(64, 72003, 74880), // E4 - starts 3 ticks later
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        // Should detect F#m7 even with staggered start times
        assert!(
            !chords.is_empty(),
            "Should detect F#m7 with staggered start times. Got {} chords",
            chords.len()
        );

        // Find the chord that contains F#m7
        let f_sharp_m7_chord = chords.iter().find(|c| {
            c.root_pitch == 42
                && c.chord.quality == ChordQuality::Minor
                && c.chord.family == Some(crate::chord::ChordFamily::Minor7)
        });

        assert!(
            f_sharp_m7_chord.is_some(),
            "Should detect F#m7 chord. Found chords: {:?}",
            chords
                .iter()
                .map(|c| (c.root_pitch, c.chord.to_string()))
                .collect::<Vec<_>>()
        );

        let chord = f_sharp_m7_chord.unwrap();
        let chord_name = chord.chord.to_string();

        // Should be exactly F#m7
        assert_eq!(
            chord_name, "F#m7",
            "Should be exactly F#m7. Root pitch: {}, Quality: {:?}, Family: {:?}, Name: {}",
            chord.root_pitch, chord.chord.quality, chord.chord.family, chord_name
        );
        assert_eq!(
            chord.root_pitch, 42,
            "Root should be exactly F#2 (42), got {}",
            chord.root_pitch
        );
        assert_eq!(
            chord.chord.quality,
            ChordQuality::Minor,
            "Should be exactly Minor quality, got {:?}",
            chord.chord.quality
        );
        assert_eq!(
            chord.chord.family,
            Some(crate::chord::ChordFamily::Minor7),
            "Should have exactly Minor7 family, got {:?}",
            chord.chord.family
        );
    }

    #[test]
    fn test_f_sharp_minor_7th_staggered_start_and_end_times() {
        // F#2, F#3, A3, C#4, E4 should be F#m7 even when notes have both staggered start and end times
        // This is the most realistic scenario - notes don't start or end at exactly the same time
        // F#2 = 42, F#3 = 54, A3 = 57, C#4 = 61, E4 = 64
        // Start times: 72000, 72000, 72001, 72002, 72003 (slightly staggered)
        // End times: 74880, 74879, 74878, 74877, 74876 (slightly staggered, but all >= 180 ticks duration)
        let notes = vec![
            create_midi_note(42, 72000, 74880), // F#2 - starts first, ends last
            create_midi_note(54, 72000, 74879), // F#3 - starts same, ends 1 tick earlier
            create_midi_note(57, 72001, 74878), // A3 - starts 1 tick later, ends 2 ticks earlier
            create_midi_note(61, 72002, 74877), // C#4 - starts 2 ticks later, ends 3 ticks earlier
            create_midi_note(64, 72003, 74876), // E4 - starts 3 ticks later, ends 4 ticks earlier
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        // Should detect F#m7 even with staggered start and end times
        assert!(
            !chords.is_empty(),
            "Should detect F#m7 with staggered start and end times. Got {} chords",
            chords.len()
        );

        // Find the chord that contains F#m7
        let f_sharp_m7_chord = chords.iter().find(|c| {
            c.root_pitch == 42
                && c.chord.quality == ChordQuality::Minor
                && c.chord.family == Some(crate::chord::ChordFamily::Minor7)
        });

        assert!(
            f_sharp_m7_chord.is_some(),
            "Should detect F#m7 chord. Found chords: {:?}",
            chords
                .iter()
                .map(|c| (c.root_pitch, c.chord.to_string()))
                .collect::<Vec<_>>()
        );

        let chord = f_sharp_m7_chord.unwrap();
        let chord_name = chord.chord.to_string();

        // Should be exactly F#m7
        assert_eq!(
            chord_name, "F#m7",
            "Should be exactly F#m7. Root pitch: {}, Quality: {:?}, Family: {:?}, Name: {}",
            chord.root_pitch, chord.chord.quality, chord.chord.family, chord_name
        );
        assert_eq!(
            chord.root_pitch, 42,
            "Root should be exactly F#2 (42), got {}",
            chord.root_pitch
        );
        assert_eq!(
            chord.chord.quality,
            ChordQuality::Minor,
            "Should be exactly Minor quality, got {:?}",
            chord.chord.quality
        );
        assert_eq!(
            chord.chord.family,
            Some(crate::chord::ChordFamily::Minor7),
            "Should have exactly Minor7 family, got {:?}",
            chord.chord.family
        );
    }

    #[test]
    fn test_f_sharp_minor_7th_exact_reaper_scenario() {
        // This test simulates the exact scenario from REAPER where F#m7 is not detected
        // Based on the debug output: [F#2, F#3, A3, C#4, E4] at PPQ: 72000 should be F#m7
        // The issue might be that notes are being processed in a way that splits them
        // F#2 = 42, F#3 = 54, A3 = 57, C#4 = 61, E4 = 64
        // Using PPQ positions similar to the REAPER output
        let notes = vec![
            create_midi_note(42, 72000, 74880), // F#2
            create_midi_note(54, 72000, 74880), // F#3
            create_midi_note(57, 72000, 74880), // A3
            create_midi_note(61, 72000, 74880), // C#4
            create_midi_note(64, 72000, 74880), // E4
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        // Must detect exactly one F#m7 chord - this is a critical test
        assert!(
            !chords.is_empty(),
            "MUST detect F#m7. Got {} chords: {:?}",
            chords.len(),
            chords
                .iter()
                .map(|c| format!("{} (root: {})", c.chord, c.root_pitch))
                .collect::<Vec<_>>()
        );

        // Should have exactly one chord
        assert_eq!(
            chords.len(),
            1,
            "Should detect exactly one chord, got {}: {:?}",
            chords.len(),
            chords
                .iter()
                .map(|c| format!("{} (root: {})", c.chord, c.root_pitch))
                .collect::<Vec<_>>()
        );

        let chord = &chords[0];
        let chord_name = chord.chord.to_string();

        // Must be exactly F#m7 - no exceptions
        assert_eq!(
            chord_name,
            "F#m7",
            "MUST be exactly F#m7, not '{}'. Root pitch: {}, Quality: {:?}, Family: {:?}, Extensions: {:?}",
            chord_name,
            chord.root_pitch,
            chord.chord.quality,
            chord.chord.family,
            chord.chord.extensions
        );
        assert_eq!(
            chord.root_pitch, 42,
            "Root MUST be exactly F#2 (42), got {}",
            chord.root_pitch
        );
        assert_eq!(
            chord.chord.quality,
            ChordQuality::Minor,
            "Quality MUST be exactly Minor, got {:?}",
            chord.chord.quality
        );
        assert_eq!(
            chord.chord.family,
            Some(crate::chord::ChordFamily::Minor7),
            "Family MUST be exactly Minor7, got {:?}",
            chord.chord.family
        );
        assert_eq!(
            chord.chord.extensions.ninth, None,
            "MUST NOT have 9th extension, got {:?}",
            chord.chord.extensions.ninth
        );
        assert_eq!(
            chord.start_ppq, 72000,
            "Start PPQ MUST be exactly 72000, got {}",
            chord.start_ppq
        );
        assert!(
            chord.end_ppq >= 72000 + 180,
            "End PPQ MUST be at least 180 ticks after start (>= 72180), got {}",
            chord.end_ppq
        );
    }

    #[test]
    fn test_f_sharp_minor_7th_after_previous_chord() {
        // This test simulates the exact scenario where F#m7 appears after another chord
        // Based on the debug output: F#m7 at 69120 works, but at 72000 it doesn't
        // The issue might be that notes are being split when they shouldn't be
        // First chord: F#m7 at 69120, then another F#m7 at 72000
        // F#2 = 42, F#3 = 54, A3 = 57, C#4 = 61, E4 = 64
        let notes = vec![
            // First chord group (should be detected as F#m7)
            create_midi_note(42, 69120, 72000), // F#2
            create_midi_note(54, 69120, 72000), // F#3
            create_midi_note(57, 69120, 72000), // A3
            create_midi_note(61, 69120, 72000), // C#4
            create_midi_note(64, 69120, 72000), // E4
            // Second chord group (should also be detected as F#m7)
            create_midi_note(42, 72000, 74880), // F#2
            create_midi_note(54, 72000, 74880), // F#3
            create_midi_note(57, 72000, 74880), // A3
            create_midi_note(61, 72000, 74880), // C#4
            create_midi_note(64, 72000, 74880), // E4
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        // Must detect exactly two F#m7 chords
        assert!(
            !chords.is_empty(),
            "MUST detect F#m7 chords. Got {} chords",
            chords.len()
        );

        // Find all F#m7 chords
        let f_sharp_m7_chords: Vec<_> = chords
            .iter()
            .filter(|c| {
                c.root_pitch == 42
                    && c.chord.quality == ChordQuality::Minor
                    && c.chord.family == Some(crate::chord::ChordFamily::Minor7)
            })
            .collect();

        assert!(
            !f_sharp_m7_chords.is_empty(),
            "MUST detect at least one F#m7 chord. Found chords: {:?}",
            chords
                .iter()
                .map(|c| format!(
                    "{} (root: {}, start: {})",
                    c.chord, c.root_pitch, c.start_ppq
                ))
                .collect::<Vec<_>>()
        );

        // Check the second chord (at 72000) - this is the one that's failing in REAPER
        let second_chord = chords.iter().find(|c| c.start_ppq == 72000);

        assert!(
            second_chord.is_some(),
            "MUST detect F#m7 chord starting at 72000. Found chords: {:?}",
            chords
                .iter()
                .map(|c| format!(
                    "{} (root: {}, start: {})",
                    c.chord, c.root_pitch, c.start_ppq
                ))
                .collect::<Vec<_>>()
        );

        let chord = second_chord.unwrap();
        let chord_name = chord.chord.to_string();

        // Must be exactly F#m7
        assert_eq!(
            chord_name, "F#m7",
            "MUST be exactly F#m7 at 72000, not '{}'. Root pitch: {}, Quality: {:?}, Family: {:?}",
            chord_name, chord.root_pitch, chord.chord.quality, chord.chord.family
        );
        assert_eq!(
            chord.root_pitch, 42,
            "Root MUST be exactly F#2 (42) at 72000, got {}",
            chord.root_pitch
        );
        assert_eq!(
            chord.chord.quality,
            ChordQuality::Minor,
            "Quality MUST be exactly Minor at 72000, got {:?}",
            chord.chord.quality
        );
        assert_eq!(
            chord.chord.family,
            Some(crate::chord::ChordFamily::Minor7),
            "Family MUST be exactly Minor7 at 72000, got {:?}",
            chord.chord.family
        );
    }

    #[test]
    fn test_d2_chord_after_previous_chord() {
        // This test simulates the scenario where D2 appears after another chord
        // Based on the debug output: D2 at 80640 works, but at 83520 it doesn't
        // D3 = 50, A3 = 57, D4 = 62, E4 = 64, A4 = 69
        let notes = vec![
            // First chord group (should be detected as D2)
            create_midi_note(50, 80640, 83520), // D3
            create_midi_note(57, 80640, 83520), // A3
            create_midi_note(62, 80640, 83520), // D4
            create_midi_note(64, 80640, 83520), // E4
            create_midi_note(69, 80640, 83520), // A4
            // Second chord group (should also be detected as D2)
            create_midi_note(50, 83520, 86400), // D3
            create_midi_note(57, 83520, 86400), // A3
            create_midi_note(62, 83520, 86400), // D4
            create_midi_note(64, 83520, 86400), // E4
            create_midi_note(69, 83520, 86400), // A4
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        // Must detect at least one D2 chord
        assert!(
            !chords.is_empty(),
            "MUST detect D2 chords. Got {} chords",
            chords.len()
        );

        // Find all D2 chords
        let d2_chords: Vec<_> = chords
            .iter()
            .filter(|c| {
                c.root_pitch == 50
                    && c.chord.quality == ChordQuality::Power
                    && c.chord.additions.contains(&ChordDegree::Second)
            })
            .collect();

        assert!(
            !d2_chords.is_empty(),
            "MUST detect at least one D2 chord. Found chords: {:?}",
            chords
                .iter()
                .map(|c| format!(
                    "{} (root: {}, start: {})",
                    c.chord, c.root_pitch, c.start_ppq
                ))
                .collect::<Vec<_>>()
        );

        // Check the second chord (at 83520) - this is the one that's failing in REAPER
        let second_chord = chords.iter().find(|c| c.start_ppq == 83520);

        assert!(
            second_chord.is_some(),
            "MUST detect D2 chord starting at 83520. Found chords: {:?}",
            chords
                .iter()
                .map(|c| format!(
                    "{} (root: {}, start: {})",
                    c.chord, c.root_pitch, c.start_ppq
                ))
                .collect::<Vec<_>>()
        );

        let chord = second_chord.unwrap();
        let chord_name = chord.chord.to_string();

        // Must be exactly D2
        assert_eq!(
            chord_name, "D2",
            "MUST be exactly D2 at 83520, not '{}'. Root pitch: {}, Quality: {:?}, Additions: {:?}",
            chord_name, chord.root_pitch, chord.chord.quality, chord.chord.additions
        );
        assert_eq!(
            chord.root_pitch, 50,
            "Root MUST be exactly D3 (50) at 83520, got {}",
            chord.root_pitch
        );
        assert_eq!(
            chord.chord.quality,
            ChordQuality::Power,
            "Quality MUST be exactly Power at 83520, got {:?}",
            chord.chord.quality
        );
        assert_eq!(
            chord.chord.additions,
            vec![ChordDegree::Second],
            "Additions MUST be exactly [Second] at 83520, got {:?}",
            chord.chord.additions
        );
    }

    #[test]
    fn test_e_add4_with_root_in_lower_octave() {
        // E2, G#3, A3, B3 should be EAdd4, not E11
        // Even though root is E2 (pitch 40) and G#3/A3 are in octave 3,
        // they're in the same octave relative to each other, so it's add4
        // E2 = 40, G#3 = 56, A3 = 57, B3 = 59
        let notes = vec![
            create_midi_note(40, 0, 5760), // E2 (root)
            create_midi_note(56, 0, 5760), // G#3 (major 3rd, compound interval 16)
            create_midi_note(57, 0, 5760), // A3 (4th, compound interval 17)
            create_midi_note(59, 0, 5760), // B3 (5th, compound interval 19)
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        assert!(!chords.is_empty(), "Should detect a chord");

        let chord = &chords[0].chord;
        // Should be E major with added 4th
        assert_eq!(chord.quality, ChordQuality::Major, "Expected Major quality");
        assert_eq!(chord.family, None, "Expected no 7th family");

        // The chord should have the 4th (A) as an addition, not an extension
        let chord_name = chord.to_string();
        let has_add4 = chord.additions.contains(&ChordDegree::Eleventh)
            || chord_name.to_lowercase().contains("add");

        assert!(
            has_add4,
            "Should have add4/add11. Additions: {:?}, Extensions: {:?}, Name: {}",
            chord.additions, chord.extensions, chord_name
        );
        assert!(
            chord.extensions.eleventh.is_none(),
            "Should not have 11th extension"
        );
    }

    #[test]
    fn test_a_slash_csharp_inversion() {
        // C#2, C#3, E3, A3 should be A/C# (A major in first inversion)
        // C# = 37, E = 52, A = 57
        let notes = vec![
            create_midi_note(37, 0, 2880), // C#2
            create_midi_note(49, 0, 2880), // C#3
            create_midi_note(52, 0, 2880), // E3
            create_midi_note(57, 0, 2880), // A3
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        assert!(!chords.is_empty(), "Should detect a chord");

        let chord = &chords[0].chord;
        let chord_name = chord.to_string();

        // Debug output
        println!("Detected chord: {}", chord_name);
        println!("Root pitch: {}", chords[0].root_pitch);
        println!("Quality: {:?}", chord.quality);
        println!("Family: {:?}", chord.family);
        println!("Bass: {:?}", chord.bass);

        // Should be A major (not C#m#5#9b13)
        assert_eq!(
            chord.quality,
            ChordQuality::Major,
            "Should be major quality. Got: {}",
            chord_name
        );
        assert_eq!(chord.family, None, "Should not have a 7th");
        // Should have C# as bass (inversion)
        assert!(chord.bass.is_some(), "Should have bass note for inversion");

        // Should be exactly A/C# (A major in first inversion)
        let chord_name = chord.to_string();
        assert_eq!(
            chord_name, "A/C#",
            "Should be exactly A/C#, got: {}",
            chord_name
        );
        assert_eq!(
            chords[0].root_pitch, 57,
            "Root should be exactly A3 (57), got {}",
            chords[0].root_pitch
        );
        assert_eq!(
            chord.quality,
            ChordQuality::Major,
            "Should be exactly Major quality, got {:?}",
            chord.quality
        );
        assert_eq!(
            chord.family, None,
            "Should have exactly no 7th family, got {:?}",
            chord.family
        );
        assert!(chord.bass.is_some(), "Should have bass note for inversion");
    }

    #[test]
    fn test_no_false_slash_chords_same_pitch_class() {
        // E2, E3, G#3, A3, B3 should be Eadd11, NOT E/E
        // E = 40, G# = 56, A = 57, B = 59
        let notes = vec![
            create_midi_note(40, 0, 5760), // E2
            create_midi_note(52, 0, 5760), // E3
            create_midi_note(56, 0, 5760), // G#3
            create_midi_note(57, 0, 5760), // A3
            create_midi_note(59, 0, 5760), // B3
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        assert!(!chords.is_empty(), "Should detect a chord");

        let chord = &chords[0].chord;
        // Should be exactly Eadd4, no slash chord
        let chord_name = chord.to_string();
        assert_eq!(
            chord_name, "Eadd4",
            "Should be exactly Eadd4, got: {}",
            chord_name
        );
        assert_eq!(
            chord.bass, None,
            "Should have exactly no bass note, got {:?}",
            chord.bass
        );
        assert_eq!(
            chords[0].root_pitch, 40,
            "Root should be exactly E2 (40), got {}",
            chords[0].root_pitch
        );
    }

    #[test]
    fn test_f_sharp_minor_no_false_slash() {
        // F#2, F#3, A3, C#4 should be F#m, NOT F#m/F#
        // F# = 42, A = 57, C# = 61
        let notes = vec![
            create_midi_note(42, 0, 2880), // F#2
            create_midi_note(54, 0, 2880), // F#3
            create_midi_note(57, 0, 2880), // A3
            create_midi_note(61, 0, 2880), // C#4
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        assert!(!chords.is_empty(), "Should detect a chord");

        let chord = &chords[0].chord;
        let chord_name = chord.to_string();
        // Should be exactly F#m, no slash chord
        assert_eq!(
            chord_name, "F#m",
            "Should be exactly F#m, got: {}",
            chord_name
        );
        assert_eq!(
            chord.quality,
            ChordQuality::Minor,
            "Should be exactly Minor quality, got {:?}",
            chord.quality
        );
        assert_eq!(
            chord.bass, None,
            "Should have exactly no bass note, got {:?}",
            chord.bass
        );
        assert_eq!(
            chords[0].root_pitch, 42,
            "Root should be exactly F#2 (42), got {}",
            chords[0].root_pitch
        );
    }

    #[test]
    fn test_d_sus2_no_false_slash() {
        // D2, D3, E3, A3 should be Dsus2, NOT Dsus2/D
        // D = 38, E = 52, A = 57
        let notes = vec![
            create_midi_note(38, 0, 5760), // D2
            create_midi_note(50, 0, 5760), // D3
            create_midi_note(52, 0, 5760), // E3
            create_midi_note(57, 0, 5760), // A3
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        assert!(!chords.is_empty(), "Should detect a chord");

        let chord = &chords[0].chord;
        let chord_name = chord.to_string();
        // Should be exactly D2 (power chord with added 2nd), not Dsus2
        // This is root, 5th, 2nd with no 3rd, which is D2 not Dsus2
        assert_eq!(
            chord_name, "D2",
            "Should be exactly D2, got: {}",
            chord_name
        );
        assert_eq!(
            chord.quality,
            ChordQuality::Power,
            "Should be exactly Power quality, got {:?}",
            chord.quality
        );
        assert_eq!(
            chord.bass, None,
            "Should have exactly no bass note, got {:?}",
            chord.bass
        );
        assert_eq!(
            chords[0].root_pitch, 38,
            "Root should be exactly D2 (38), got {}",
            chords[0].root_pitch
        );
        assert_eq!(
            chord.additions,
            vec![ChordDegree::Second],
            "Should have exactly [Second] in additions, got {:?}",
            chord.additions
        );
    }

    #[test]
    fn test_major_triad_root_position() {
        // C4, E4, G4 should be C major
        let notes = vec![
            create_midi_note(60, 0, 4800), // C4
            create_midi_note(64, 0, 4800), // E4
            create_midi_note(67, 0, 4800), // G4
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        assert!(!chords.is_empty(), "Should detect a chord");

        let chord = &chords[0].chord;
        let chord_name = chord.to_string();
        assert_eq!(chord_name, "C", "Should be exactly C, got: {}", chord_name);
        assert_eq!(
            chord.quality,
            ChordQuality::Major,
            "Should be exactly Major quality, got {:?}",
            chord.quality
        );
        assert_eq!(
            chord.family, None,
            "Should have exactly no 7th family, got {:?}",
            chord.family
        );
        assert_eq!(
            chords[0].root_pitch, 60,
            "Root should be exactly C4 (60), got {}",
            chords[0].root_pitch
        );
        assert_eq!(chords[0].root_pitch, 60); // C should be root
    }

    #[test]
    fn test_minor_triad_inversion() {
        // E3, G3, C4 should be Cm/E (C minor in first inversion)
        // But with simplicity scoring, C might be preferred as root even if E is lowest
        let notes = vec![
            create_midi_note(52, 0, 4800), // E3
            create_midi_note(55, 0, 4800), // G3
            create_midi_note(60, 0, 4800), // C4
        ];

        let chords = detect_chords_from_midi_notes(&notes, 180);
        assert!(!chords.is_empty());

        let chord = &chords[0].chord;
        // Should be minor (either Cm or Em)
        assert!(
            chord.quality == ChordQuality::Minor || chord.quality == ChordQuality::Major,
            "Should be minor or major, got: {:?}",
            chord.quality
        );
        // If C is root, E should be bass (inversion)
        // If E is root, it's root position
        if chords[0].root_pitch == 60 {
            // C is root - should have E as bass
            assert!(chord.bass.is_some(), "Should have bass note when C is root");
        } else {
            // E is root - no bass needed
            assert_eq!(chords[0].root_pitch, 52, "If E is root, should be 52");
        }
    }
}
