//! Key and scale system for defining musical scales and keys

use crate::primitives::note::{Note, NoteLiteral, Modifier};
use crate::primitives::intervals::Interval;

/// Scale families/types - the fundamental scale patterns
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ScaleType {
    Diatonic,        // The standard 7-note pattern (Ionian, Dorian, Phrygian, etc.)
    HarmonicMinor,   // Minor with raised 7th + its modes
    MelodicMinor,    // Minor with raised 6th and 7th + its modes
    HarmonicMajor,   // Major with lowered 6th + its modes
}

/// Scale modes - these are rotations of scale families
/// Mode names are tied to the major scale pattern
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ScaleMode {
    Ionian,     // Mode 1
    Dorian,     // Mode 2
    Phrygian,   // Mode 3
    Lydian,     // Mode 4
    Mixolydian, // Mode 5
    Aeolian,    // Mode 6
    Locrian,    // Mode 7
}

/// Melodic Minor modes (own naming, not the diatonic mode names)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MelodicMinorMode {
    /// Mode 1: Melodic Minor
    MelodicMinor,
    /// Mode 2: Dorian b2 (alias: Sus b9)
    DorianFlat2,
    /// Mode 3: Lydian Augmented
    LydianAugmented,
    /// Mode 4: Lydian Dominant
    LydianDominant,
    /// Mode 5: Mixolydian b6
    MixolydianFlat6,
    /// Mode 6: Aeolian b5
    AeolianFlat5,
    /// Mode 7: Super Locrian
    SuperLocrian,
}

/// Harmonic Minor modes (own naming, not the diatonic mode names)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HarmonicMinorMode {
    /// Mode 1: Harmonic Minor
    HarmonicMinor,
    /// Mode 2: Locrian Natural 6
    LocrianNat6,
    /// Mode 3: Ionian Augmented
    IonianAugmented,
    /// Mode 4: Romanian Dorian
    RomanianDorian,
    /// Mode 5: Phrygian Dominant
    PhrygianDominant,
    /// Mode 6: Lydian #9
    LydianSharp9,
    /// Mode 7: Ultra Locrian
    UltraLocrian,
}


impl MelodicMinorMode {
    /// Rotation offset (0-6) relative to Melodic Minor base pattern
    pub fn rotation_offset(self) -> usize {
        match self {
            MelodicMinorMode::MelodicMinor => 0,
            MelodicMinorMode::DorianFlat2 => 1,
            MelodicMinorMode::LydianAugmented => 2,
            MelodicMinorMode::LydianDominant => 3,
            MelodicMinorMode::MixolydianFlat6 => 4,
            MelodicMinorMode::AeolianFlat5 => 5,
            MelodicMinorMode::SuperLocrian => 6,
        }
    }

    /// Primary name for this mode
    pub fn name(self) -> &'static str {
        match self {
            MelodicMinorMode::MelodicMinor => "Melodic Minor",
            MelodicMinorMode::DorianFlat2 => "Dorian b2",
            MelodicMinorMode::LydianAugmented => "Lydian Augmented",
            MelodicMinorMode::LydianDominant => "Lydian Dominant",
            MelodicMinorMode::MixolydianFlat6 => "Mixolydian b6",
            MelodicMinorMode::AeolianFlat5 => "Aeolian b5",
            MelodicMinorMode::SuperLocrian => "Super Locrian",
        }
    }

    /// Common aliases for each mode
    pub fn aliases(self) -> &'static [&'static str] {
        match self {
            MelodicMinorMode::MelodicMinor => &[],
            MelodicMinorMode::DorianFlat2 => &["Sus b9"],
            MelodicMinorMode::LydianAugmented => &[],
            MelodicMinorMode::LydianDominant => &["", ""],
            MelodicMinorMode::MixolydianFlat6 => &[""],
            MelodicMinorMode::AeolianFlat5 => &[],
            MelodicMinorMode::SuperLocrian => &["Altered", "Diminished Whole-Tone"],
        }
    }
}

impl HarmonicMinorMode {
    /// Rotation offset (0-6) relative to Harmonic Minor base pattern
    pub fn rotation_offset(self) -> usize {
        match self {
            HarmonicMinorMode::HarmonicMinor => 0,
            HarmonicMinorMode::LocrianNat6 => 1,
            HarmonicMinorMode::IonianAugmented => 2,
            HarmonicMinorMode::RomanianDorian => 3,
            HarmonicMinorMode::PhrygianDominant => 4,
            HarmonicMinorMode::LydianSharp9 => 5,
            HarmonicMinorMode::UltraLocrian => 6,
        }
    }

    /// Primary name for this mode
    pub fn name(self) -> &'static str {
        match self {
            HarmonicMinorMode::HarmonicMinor => "Harmonic Minor",
            HarmonicMinorMode::LocrianNat6 => "Locrian nat6",
            HarmonicMinorMode::IonianAugmented => "Ionian Augmented",
            HarmonicMinorMode::RomanianDorian => "Romanian Dorian",
            HarmonicMinorMode::PhrygianDominant => "Phrygian Dominant",
            HarmonicMinorMode::LydianSharp9 => "Lydian #9",
            HarmonicMinorMode::UltraLocrian => "Ultra Locrian",
        }
    }

    /// Common aliases for each mode
    pub fn aliases(self) -> &'static [&'static str] {
        match self {
            HarmonicMinorMode::HarmonicMinor => &[],
            HarmonicMinorMode::LocrianNat6 => &[],
            HarmonicMinorMode::IonianAugmented => &[],
            HarmonicMinorMode::RomanianDorian => &[],
            HarmonicMinorMode::PhrygianDominant => &[],
            HarmonicMinorMode::LydianSharp9 => &[],
            HarmonicMinorMode::UltraLocrian => &[],
        }
    }
}


impl ScaleType {
    /// Get the fundamental semitone pattern for this scale type
    pub fn semitone_pattern(self) -> Vec<u8> {
        match self {
            ScaleType::Diatonic => vec![0, 2, 4, 5, 7, 9, 11],
            ScaleType::HarmonicMinor => vec![0, 2, 3, 5, 7, 8, 11],
            ScaleType::MelodicMinor => vec![0, 2, 3, 5, 7, 9, 11], // A B C D E F# G#
            ScaleType::HarmonicMajor => vec![0, 2, 4, 5, 7, 8, 11],
        }
    }

    /// Get the semitone sequence for this scale type with a specific mode
    pub fn semitone_sequence_with_mode(self, mode: ScaleMode) -> Vec<u8> {
        let base_pattern = self.semitone_pattern();
        let rotation = mode.rotation_offset();
        
        // Rotate the pattern by the mode offset
        let mut rotated = Vec::new();
        for i in 0..7 {
            let idx = (i + rotation) % 7;
            let interval = base_pattern[idx];
            // Adjust intervals to start from 0
            let adjusted = if i == 0 { 
                0 
            } else { 
                (interval + 12 - base_pattern[rotation]) % 12
            };
            rotated.push(adjusted);
        }
        rotated
    }

    /// Get the semitone sequence for the Melodic Minor family with a specific melodic minor mode
    pub fn melodic_minor_semitone_sequence_with_mode(mode: MelodicMinorMode) -> Vec<u8> {
        // Melodic Minor base pattern (ascending): 0, 2, 3, 5, 7, 9, 11
        let base_pattern = ScaleType::MelodicMinor.semitone_pattern();
        let rotation = mode.rotation_offset();

        let mut rotated = Vec::new();
        for i in 0..7 {
            let idx = (i + rotation) % 7;
            let interval = base_pattern[idx];
            let adjusted = if i == 0 { 0 } else { (interval + 12 - base_pattern[rotation]) % 12 };
            rotated.push(adjusted);
        }
        rotated
    }

    /// Get the semitone sequence for the Harmonic Minor family with a specific harmonic minor mode
    pub fn harmonic_minor_semitone_sequence_with_mode(mode: HarmonicMinorMode) -> Vec<u8> {
        // Harmonic Minor base pattern: 0, 2, 3, 5, 7, 8, 11
        let base_pattern = ScaleType::HarmonicMinor.semitone_pattern();
        let rotation = mode.rotation_offset();

        let mut rotated = Vec::new();
        for i in 0..7 {
            let idx = (i + rotation) % 7;
            let interval = base_pattern[idx];
            let adjusted = if i == 0 { 0 } else { (interval + 12 - base_pattern[rotation]) % 12 };
            rotated.push(adjusted);
        }
        rotated
    }

    /// Get the name of this scale type
    pub fn name(self) -> &'static str {
        match self {
            ScaleType::Diatonic => "Diatonic",
            ScaleType::HarmonicMinor => "Harmonic Minor",
            ScaleType::MelodicMinor => "Melodic Minor",
            ScaleType::HarmonicMajor => "Harmonic Major",
        }
    }

    /// Get the interval sequence for this scale type by converting semitones to intervals
    pub fn interval_sequence_with_mode(self, mode: ScaleMode) -> Vec<Interval> {
        let semitones = self.semitone_sequence_with_mode(mode);
        semitones
            .iter()
            .filter_map(|&s| Interval::from_semitones(s))
            .collect()
    }
}

impl ScaleMode {
    /// Get the rotation offset for this mode (0-6)
    pub fn rotation_offset(self) -> usize {
        match self {
            ScaleMode::Ionian => 0,
            ScaleMode::Dorian => 1,
            ScaleMode::Phrygian => 2,
            ScaleMode::Lydian => 3,
            ScaleMode::Mixolydian => 4,
            ScaleMode::Aeolian => 5,
            ScaleMode::Locrian => 6,
        }
    }

    /// Get the name of this mode
    pub fn name(self) -> &'static str {
        match self {
            ScaleMode::Ionian => "Ionian",
            ScaleMode::Dorian => "Dorian",
            ScaleMode::Phrygian => "Phrygian",
            ScaleMode::Lydian => "Lydian",
            ScaleMode::Mixolydian => "Mixolydian",
            ScaleMode::Aeolian => "Aeolian",
            ScaleMode::Locrian => "Locrian",
        }
    }
}

/// A musical key with a root note, scale type, and mode
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct Key {
    pub root: Note,
    pub scale_type: ScaleType,
    pub mode: ScaleMode,
}

impl Key {
    /// Create a new key with a root note, scale type, and mode
    pub fn new(root: Note, scale_type: ScaleType, mode: ScaleMode) -> Self {
        Self { root, scale_type, mode }
    }

    /// Create a major key (Ionian mode of diatonic)
    pub fn major(root: Note) -> Self {
        Self::new(root, ScaleType::Diatonic, ScaleMode::Ionian)
    }

    /// Create a minor key (Aeolian mode of diatonic)
    pub fn minor(root: Note) -> Self {
        Self::new(root, ScaleType::Diatonic, ScaleMode::Aeolian)
    }

    /// Create a harmonic minor key (Mode 1: Harmonic Minor)
    pub fn harmonic_minor(root: Note) -> Self {
        Self::new_harmonic_minor_mode(root, HarmonicMinorMode::HarmonicMinor)
    }

    /// Create a harmonic minor key with a specific harmonic minor mode
    pub fn new_harmonic_minor_mode(root: Note, mode: HarmonicMinorMode) -> Self {
        // Map harmonic minor modes to diatonic modes for internal representation
        let diatonic_mode = match mode {
            HarmonicMinorMode::HarmonicMinor => ScaleMode::Aeolian,
            HarmonicMinorMode::LocrianNat6 => ScaleMode::Locrian,
            HarmonicMinorMode::IonianAugmented => ScaleMode::Ionian,
            HarmonicMinorMode::RomanianDorian => ScaleMode::Dorian,
            HarmonicMinorMode::PhrygianDominant => ScaleMode::Phrygian,
            HarmonicMinorMode::LydianSharp9 => ScaleMode::Lydian,
            HarmonicMinorMode::UltraLocrian => ScaleMode::Locrian,
        };
        Self::new(root, ScaleType::HarmonicMinor, diatonic_mode)
    }

    /// Create a melodic minor key (Mode 1: Melodic Minor)
    pub fn melodic_minor(root: Note) -> Self {
        Self::new_melodic_minor_mode(root, MelodicMinorMode::MelodicMinor)
    }

    /// Create a melodic minor key with a specific melodic minor mode
    pub fn new_melodic_minor_mode(root: Note, mode: MelodicMinorMode) -> Self {
        // Map melodic minor modes to diatonic modes for internal representation
        let diatonic_mode = match mode {
            MelodicMinorMode::MelodicMinor => ScaleMode::Ionian,
            MelodicMinorMode::DorianFlat2 => ScaleMode::Dorian,
            MelodicMinorMode::LydianAugmented => ScaleMode::Phrygian,
            MelodicMinorMode::LydianDominant => ScaleMode::Lydian,
            MelodicMinorMode::MixolydianFlat6 => ScaleMode::Mixolydian,
            MelodicMinorMode::AeolianFlat5 => ScaleMode::Aeolian,
            MelodicMinorMode::SuperLocrian => ScaleMode::Locrian,
        };
        Self::new(root, ScaleType::MelodicMinor, diatonic_mode)
    }

    /// Create a dorian key
    pub fn dorian(root: Note) -> Self {
        Self::new(root, ScaleType::Diatonic, ScaleMode::Dorian)
    }

    /// Create a phrygian key
    pub fn phrygian(root: Note) -> Self {
        Self::new(root, ScaleType::Diatonic, ScaleMode::Phrygian)
    }

    /// Create a lydian key
    pub fn lydian(root: Note) -> Self {
        Self::new(root, ScaleType::Diatonic, ScaleMode::Lydian)
    }

    /// Create a mixolydian key
    pub fn mixolydian(root: Note) -> Self {
        Self::new(root, ScaleType::Diatonic, ScaleMode::Mixolydian)
    }

    /// Create a locrian key
    pub fn locrian(root: Note) -> Self {
        Self::new(root, ScaleType::Diatonic, ScaleMode::Locrian)
    }

    // Melodic Minor Mode constructors
    /// Create a Dorian b2 key (Melodic Minor Mode 2)
    pub fn dorian_flat2(root: Note) -> Self {
        Self::new_melodic_minor_mode(root, MelodicMinorMode::DorianFlat2)
    }

    /// Create a Lydian Augmented key (Melodic Minor Mode 3)
    pub fn lydian_augmented(root: Note) -> Self {
        Self::new_melodic_minor_mode(root, MelodicMinorMode::LydianAugmented)
    }

    /// Create a Lydian Dominant key (Melodic Minor Mode 4)
    pub fn lydian_dominant(root: Note) -> Self {
        Self::new_melodic_minor_mode(root, MelodicMinorMode::LydianDominant)
    }

    /// Create a Mixolydian b6 key (Melodic Minor Mode 5)
    pub fn mixolydian_flat6(root: Note) -> Self {
        Self::new_melodic_minor_mode(root, MelodicMinorMode::MixolydianFlat6)
    }

    /// Create an Aeolian b5 key (Melodic Minor Mode 6)
    pub fn aeolian_flat5(root: Note) -> Self {
        Self::new_melodic_minor_mode(root, MelodicMinorMode::AeolianFlat5)
    }

    /// Create a Super Locrian key (Melodic Minor Mode 7)
    pub fn super_locrian(root: Note) -> Self {
        Self::new_melodic_minor_mode(root, MelodicMinorMode::SuperLocrian)
    }

    // Harmonic Minor Mode constructors
    /// Create a Locrian Natural 6 key (Harmonic Minor Mode 2)
    pub fn locrian_nat6(root: Note) -> Self {
        Self::new_harmonic_minor_mode(root, HarmonicMinorMode::LocrianNat6)
    }

    /// Create an Ionian Augmented key (Harmonic Minor Mode 3)
    pub fn ionian_augmented(root: Note) -> Self {
        Self::new_harmonic_minor_mode(root, HarmonicMinorMode::IonianAugmented)
    }

    /// Create a Romanian Dorian key (Harmonic Minor Mode 4)
    pub fn romanian_dorian(root: Note) -> Self {
        Self::new_harmonic_minor_mode(root, HarmonicMinorMode::RomanianDorian)
    }

    /// Create a Phrygian Dominant key (Harmonic Minor Mode 5)
    pub fn phrygian_dominant(root: Note) -> Self {
        Self::new_harmonic_minor_mode(root, HarmonicMinorMode::PhrygianDominant)
    }

    /// Create a Lydian #9 key (Harmonic Minor Mode 6)
    pub fn lydian_sharp9(root: Note) -> Self {
        Self::new_harmonic_minor_mode(root, HarmonicMinorMode::LydianSharp9)
    }

    /// Create an Ultra Locrian key (Harmonic Minor Mode 7)
    pub fn ultra_locrian(root: Note) -> Self {
        Self::new_harmonic_minor_mode(root, HarmonicMinorMode::UltraLocrian)
    }

    /// Get the notes of this key/scale with proper enharmonic spelling
    pub fn notes(&self) -> Vec<Note> {
        let semitones = if matches!(self.scale_type, ScaleType::MelodicMinor) {
            ScaleType::melodic_minor_semitone_sequence_with_mode(match self.mode {
                ScaleMode::Ionian => MelodicMinorMode::MelodicMinor,
                ScaleMode::Dorian => MelodicMinorMode::DorianFlat2,
                ScaleMode::Phrygian => MelodicMinorMode::LydianAugmented,
                ScaleMode::Lydian => MelodicMinorMode::LydianDominant,
                ScaleMode::Mixolydian => MelodicMinorMode::MixolydianFlat6,
                ScaleMode::Aeolian => MelodicMinorMode::AeolianFlat5,
                ScaleMode::Locrian => MelodicMinorMode::SuperLocrian,
            })
        } else if matches!(self.scale_type, ScaleType::HarmonicMinor) {
            ScaleType::harmonic_minor_semitone_sequence_with_mode(match self.mode {
                ScaleMode::Aeolian => HarmonicMinorMode::HarmonicMinor,
                ScaleMode::Locrian => HarmonicMinorMode::LocrianNat6,
                ScaleMode::Ionian => HarmonicMinorMode::IonianAugmented,
                ScaleMode::Dorian => HarmonicMinorMode::RomanianDorian,
                ScaleMode::Phrygian => HarmonicMinorMode::PhrygianDominant,
                ScaleMode::Lydian => HarmonicMinorMode::LydianSharp9,
                _ => HarmonicMinorMode::UltraLocrian, // Default for any other mode
            })
        } else {
            self.scale_type.semitone_sequence_with_mode(self.mode)
        };
        let mut notes = Vec::new();
        
        for &semitone in &semitones {
            let note = self.root.transpose_semitones(semitone);
            notes.push(note);
        }
        
        // Apply enharmonic spelling rules to avoid duplicate letter names
        self.apply_enharmonic_spelling(notes)
    }

    /// Apply enharmonic spelling rules to avoid duplicate letter names
    fn apply_enharmonic_spelling(&self, mut notes: Vec<Note>) -> Vec<Note> {
        // Determine if this is a sharp key or flat key based on the root note
        let is_sharp_key = match self.root.modifier {
            Some(Modifier::Sharp) | Some(Modifier::DSharp) => true,
            Some(Modifier::Flat) | Some(Modifier::DFlat) => false,
            None => {
                // For natural notes, determine based on the key signature
                // Sharp keys: G, D, A, E, B, F#, C#
                // Flat keys: F, Bb, Eb, Ab, Db, Gb, Cb
                match self.root.literal {
                    NoteLiteral::G | NoteLiteral::D | NoteLiteral::A | NoteLiteral::E | NoteLiteral::B => true,
                    NoteLiteral::F => false,
                    NoteLiteral::C => {
                        // C major is neutral, but we'll default to sharps for consistency
                        true
                    }
                }
            }
        };
        
        for i in 1..notes.len() {
            let prev_note = &notes[i - 1];
            let current_note = &notes[i];
            
            // If we have the same letter name, adjust the enharmonic spelling
            if prev_note.literal == current_note.literal {
                if is_sharp_key {
                    // For sharp keys, convert flats to sharps of the previous letter
                    if let Some(Modifier::Flat) = current_note.modifier {
                        let new_literal = match current_note.literal {
                            NoteLiteral::D => NoteLiteral::C,
                            NoteLiteral::E => NoteLiteral::D,
                            NoteLiteral::F => NoteLiteral::E,
                            NoteLiteral::G => NoteLiteral::F,
                            NoteLiteral::A => NoteLiteral::G,
                            NoteLiteral::B => NoteLiteral::A,
                            NoteLiteral::C => NoteLiteral::B,
                        };
                        notes[i] = Note::new(new_literal, Some(Modifier::Sharp));
                    }
                } else {
                    // For flat keys, convert sharps to flats of the next letter
                    if let Some(Modifier::Sharp) = current_note.modifier {
                        let new_literal = match current_note.literal {
                            NoteLiteral::C => NoteLiteral::D,
                            NoteLiteral::D => NoteLiteral::E,
                            NoteLiteral::E => NoteLiteral::F,
                            NoteLiteral::F => NoteLiteral::G,
                            NoteLiteral::G => NoteLiteral::A,
                            NoteLiteral::A => NoteLiteral::B,
                            NoteLiteral::B => NoteLiteral::C,
                        };
                        notes[i] = Note::new(new_literal, Some(Modifier::Flat));
                    }
                }
            }
        }
        notes
    }

    /// Get the intervals of this key/scale from the root
    pub fn intervals(&self) -> Vec<Interval> {
        if matches!(self.scale_type, ScaleType::MelodicMinor) {
            let semitones = if matches!(self.scale_type, ScaleType::MelodicMinor) {
                ScaleType::melodic_minor_semitone_sequence_with_mode(match self.mode {
                    ScaleMode::Ionian => MelodicMinorMode::MelodicMinor,
                    ScaleMode::Dorian => MelodicMinorMode::DorianFlat2,
                    ScaleMode::Phrygian => MelodicMinorMode::LydianAugmented,
                    ScaleMode::Lydian => MelodicMinorMode::LydianDominant,
                    ScaleMode::Mixolydian => MelodicMinorMode::MixolydianFlat6,
                    ScaleMode::Aeolian => MelodicMinorMode::AeolianFlat5,
                    ScaleMode::Locrian => MelodicMinorMode::SuperLocrian,
                })
            } else { vec![] };
            semitones.iter().filter_map(|&s| Interval::from_semitones(s)).collect()
        } else {
            self.scale_type.interval_sequence_with_mode(self.mode)
        }
    }

    /// Get the name of this key (e.g., "C Major", "A Minor")
    pub fn name(&self) -> String {
        match (self.scale_type, self.mode) {
            (ScaleType::Diatonic, ScaleMode::Ionian) => format!("{} Major", self.root),
            (ScaleType::Diatonic, ScaleMode::Aeolian) => format!("{} Minor", self.root),
            (ScaleType::HarmonicMinor, _) => format!("{} Harmonic Minor", self.root),
            (ScaleType::MelodicMinor, _) => format!("{} Melodic Minor", self.root),
            (_, mode) => format!("{} {} {}", self.root, self.scale_type.name(), mode.name()),
        }
    }

    /// Get the short name of this key (e.g., "C", "Am")
    pub fn short_name(&self) -> String {
        match (self.scale_type, self.mode) {
            (ScaleType::Diatonic, ScaleMode::Ionian) => self.root.to_string(),
            (ScaleType::Diatonic, ScaleMode::Aeolian) => format!("{}m", self.root),
            (ScaleType::HarmonicMinor, _) => format!("{}m", self.root),
            (ScaleType::MelodicMinor, _) => format!("{}m", self.root),
            (ScaleType::Diatonic, mode) => format!("{} {}", self.root, mode.name()),
            _ => format!("{} {}", self.root, self.scale_type.name()),
        }
    }

    /// Get the diatonic chord at a given scale degree (1-7)
    pub fn diatonic_chord(&self, degree: u8) -> Option<Vec<Note>> {
        if degree < 1 || degree > 7 {
            return None;
        }

        let notes = self.notes();
        let root_idx = (degree - 1) as usize;
        let root = notes[root_idx];

        // Build triad: root, third, fifth
        let third_idx = (root_idx + 2) % 7;
        let fifth_idx = (root_idx + 4) % 7;

        Some(vec![
            root,
            notes[third_idx],
            notes[fifth_idx],
        ])
    }

    /// Get the diatonic seventh chord at a given scale degree (1-7)
    pub fn diatonic_seventh_chord(&self, degree: u8) -> Option<Vec<Note>> {
        if degree < 1 || degree > 7 {
            return None;
        }

        let notes = self.notes();
        let root_idx = (degree - 1) as usize;
        let root = notes[root_idx];

        // Build seventh chord: root, third, fifth, seventh
        let third_idx = (root_idx + 2) % 7;
        let fifth_idx = (root_idx + 4) % 7;
        let seventh_idx = (root_idx + 6) % 7;

        Some(vec![
            root,
            notes[third_idx],
            notes[fifth_idx],
            notes[seventh_idx],
        ])
    }

    /// Get the note at a specific scale degree (1-7)
    pub fn get_scale_degree_note(&self, degree: usize) -> Option<Note> {
        if degree < 1 || degree > 7 {
            return None;
        }

        let notes = self.notes();
        notes.get(degree - 1).copied()
    }

    /// Get scale degrees relative to a base mode (e.g., Ionian)
    /// Returns a string like "1 2 3 4 5 6 7" or "1 2 3 4 5 6 b7"
    pub fn scale_degrees_relative_to(&self, base_mode: ScaleMode) -> String {
        let base_semitones = ScaleType::Diatonic.semitone_sequence_with_mode(base_mode);
        let current_semitones = self.scale_type.semitone_sequence_with_mode(self.mode);
        
        let mut degrees = Vec::new();
        for (i, &current_semitone) in current_semitones.iter().enumerate() {
            let base_semitone = base_semitones[i];
            let degree = i + 1;
            
            if current_semitone == base_semitone {
                degrees.push(degree.to_string());
            } else {
                // Calculate the difference and add appropriate accidental
                let diff = (current_semitone as i8 - base_semitone as i8 + 12) % 12;
                let accidental = match diff {
                    0 => "",
                    1 => "#",
                    2 => "##",
                    11 => "b",
                    10 => "bb",
                    _ => "", // fallback
                };
                degrees.push(format!("{}{}", accidental, degree));
            }
        }
        
        degrees.join(" ")
    }

    /// Detect chord type from semitone sequence using structural analysis
    pub fn detect_chord_from_semitones(semitones: &[u8]) -> Option<String> {
        if semitones.is_empty() {
            return None;
        }

        // Normalize to start from 0
        let root = semitones[0];
        let normalized: Vec<u8> = semitones.iter().map(|&s| (s + 12 - root) % 12).collect();
        
        // Analyze chord structure
        let has_third = normalized.contains(&4) || normalized.contains(&3);
        let has_fifth = normalized.contains(&7);
        let has_seventh = normalized.contains(&10) || normalized.contains(&11);
        let has_ninth = normalized.contains(&2) || normalized.contains(&14);
        let has_eleventh = normalized.contains(&5) || normalized.contains(&17);
        let has_thirteenth = normalized.contains(&9) || normalized.contains(&21);
        let has_sixth = normalized.contains(&9);
        
        // Check for alterations
        let has_flat_fifth = normalized.contains(&6);
        let has_sharp_fifth = normalized.contains(&8);
        let has_flat_ninth = normalized.contains(&13);
        let has_sharp_ninth = normalized.contains(&15);
        let has_sharp_eleventh = normalized.contains(&18);
        let has_flat_thirteenth = normalized.contains(&20);
        
        // Determine base chord quality - be more flexible for omitted chords
        let base_quality = if normalized.contains(&3) {
            "Minor"
        } else if normalized.contains(&4) {
            "Major"
        } else if (normalized.contains(&5) || normalized.contains(&2)) && !has_seventh {
            // Only consider suspended if we don't have a 7th (which would suggest an extended chord)
            "Sus"
        } else if normalized.contains(&6) && !normalized.contains(&7) {
            "Diminished"
        } else if normalized.contains(&8) && !normalized.contains(&7) {
            "Augmented"
        } else if has_seventh {
            // If we have a 7th but no clear 3rd, try to infer from the 7th type
            if normalized.contains(&11) {
                "Major" // Major 7th suggests major quality
            } else if normalized.contains(&10) {
                "Major" // Minor 7th could be dominant (major with minor 7th)
            } else {
                return None; // Unrecognized
            }
        } else {
            return None; // Unrecognized
        };
        
        // Special case: dominant 7th (major chord with minor 7th)
        let is_dominant = base_quality == "Major" && has_seventh && normalized.contains(&10);
        
        // Build chord name
        let mut chord_name = base_quality.to_string();
        
        // Add seventh if present
        if has_seventh {
            if is_dominant {
                chord_name = "Dominant".to_string();
                chord_name.push_str("7");
            } else if normalized.contains(&11) {
                // For major chords with major 7th, just show "Maj7" not "MajorMaj7"
                if base_quality == "Major" {
                    chord_name = "Maj7".to_string();
                } else {
                    chord_name.push_str("Maj7");
                }
            } else {
                chord_name.push_str("7");
            }
        }
        
        // Handle extensions (9th, 11th, 13th) - only add if we have a 7th
        if has_seventh {
            if has_thirteenth {
                // Replace the 7 with 13 for the highest extension
                if chord_name.ends_with("7") {
                    chord_name = chord_name.replace("7", "13");
                } else {
                    chord_name.push_str("13");
                }
            } else if has_eleventh {
                // Replace the 7 with 11 for the highest extension
                if chord_name.ends_with("7") {
                    chord_name = chord_name.replace("7", "11");
                } else {
                    chord_name.push_str("11");
                }
            } else if has_ninth {
                // Replace the 7 with 9 for the highest extension
                if chord_name.ends_with("7") {
                    chord_name = chord_name.replace("7", "9");
                } else {
                    chord_name.push_str("9");
                }
            }
        }
        
        // Handle 6th chords (no 7th)
        if has_sixth && !has_seventh {
            if base_quality == "Major" {
                chord_name = "6".to_string();
            } else {
                chord_name.push_str("6");
            }
        }
        
        // Add omission information
        let mut omissions = Vec::new();
        if has_seventh && !has_third {
            omissions.push("omit3");
        }
        if has_seventh && !has_fifth {
            omissions.push("omit5");
        }
        if has_ninth && !has_seventh {
            omissions.push("omit7");
        }
        // Check for omitted 9th in extended chords
        if has_eleventh && !has_ninth {
            omissions.push("omit9");
        }
        if has_thirteenth && !has_ninth {
            omissions.push("omit9");
        }
        
        if !omissions.is_empty() {
            chord_name.push_str(&format!("({})", omissions.join(",")));
        }
        
        // Handle 6/9 chords
        if has_sixth && has_ninth && !has_seventh {
            if base_quality == "Major" {
                chord_name = "6/9".to_string();
            } else {
                chord_name.push_str("6/9");
            }
        }
        
        // Handle suspended chords
        if base_quality == "Sus" {
            if normalized.contains(&5) {
                chord_name = "Sus4".to_string();
            } else if normalized.contains(&2) {
                chord_name = "Sus2".to_string();
            }
            if has_seventh {
                chord_name.push_str("7");
            }
        }
        
        // Add alterations
        let mut alterations = Vec::new();
        if has_flat_fifth { alterations.push("b5"); }
        if has_sharp_fifth { alterations.push("#5"); }
        if has_flat_ninth { alterations.push("b9"); }
        if has_sharp_ninth { alterations.push("#9"); }
        if has_sharp_eleventh { alterations.push("#11"); }
        if has_flat_thirteenth { alterations.push("b13"); }
        
        if !alterations.is_empty() {
            chord_name.push_str(&format!("({})", alterations.join("")));
        }
        
        Some(chord_name)
    }

    /// Detect chord type from root intervals
    pub fn detect_chord_from_root_intervals(intervals: &[Interval]) -> Option<String> {
        let semitones: Vec<u8> = intervals.iter().map(|i| i.st()).collect();
        Self::detect_chord_from_semitones(&semitones)
    }

    /// Detect chord type from sequential intervals (intervals between consecutive notes)
    pub fn detect_chord_from_sequential_intervals(intervals: &[Interval]) -> Option<String> {
        if intervals.is_empty() {
            return None;
        }

        // Convert sequential intervals to root intervals
        let mut root_intervals = vec![Interval::Unison];
        let mut current_semitone = 0;
        
        for interval in intervals {
            current_semitone = (current_semitone + interval.st()) % 12;
            if let Some(root_interval) = Interval::from_semitones(current_semitone) {
                root_intervals.push(root_interval);
            }
        }
        
        Self::detect_chord_from_root_intervals(&root_intervals)
    }

    /// Detect and normalize a chord from semitone sequence with a given root note
    /// Returns a properly formatted chord name like "CMaj7" instead of just "Maj7"
    pub fn detect_chord_name_from_semitones(root_note: Note, semitones: &[u8]) -> Option<String> {
        if let Some(chord_quality) = Self::detect_chord_from_semitones(semitones) {
            Some(format!("{}{}", root_note, chord_quality))
        } else {
            None
        }
    }

    /// Get all diatonic chords in this key up to a certain extension level
    pub fn diatonic_chords_up_to(&self, max_extension: u8) -> Vec<(u8, String, Vec<Note>)> {
        let mut chords = Vec::new();
        
        for degree in 1..=7 {
            // Get the chord notes
            let chord_notes = if max_extension >= 7 {
                self.diatonic_seventh_chord(degree).unwrap_or_else(|| {
                    self.diatonic_chord(degree).unwrap_or_default()
                })
            } else {
                self.diatonic_chord(degree).unwrap_or_default()
            };
            
            if !chord_notes.is_empty() {
                // Convert to semitones for analysis
                let root_semitone = chord_notes[0].to_semitone();
                let semitones: Vec<u8> = chord_notes.iter()
                    .map(|note| (note.to_semitone() + 12 - root_semitone) % 12)
                    .collect();
                
                // Detect chord type
                let chord_type = Self::detect_chord_from_semitones(&semitones)
                    .unwrap_or_else(|| "Unknown".to_string());
                
                chords.push((degree, chord_type, chord_notes));
            }
        }
        
        chords
    }
}

impl Key {
    /// Return the scale degree (1-7) for a given note in this key, if it belongs to the scale
    pub fn degree_of_note(&self, note: Note) -> Option<u8> {
        let notes = self.notes();
        notes.iter().position(|n| *n == note).map(|i| i as u8 + 1)
    }

    /// Return the natural diatonic seventh chord quality by degree (1-7)
    /// e.g., in C major, degree 5 -> "Dominant7"
    pub fn natural_seventh_quality_by_degree(&self, degree: u8) -> Option<String> {
        let chords = self.diatonic_chords_up_to(7);
        chords.into_iter().find(|(d, _, _)| *d == degree).map(|(_, name, _)| name)
    }

    /// Return the natural diatonic seventh chord quality for a given note in this key
    /// e.g., in C major, note G -> "Dominant7"
    pub fn natural_seventh_quality_by_note(&self, note: Note) -> Option<String> {
        let degree = self.degree_of_note(note)?;
        self.natural_seventh_quality_by_degree(degree)
    }
}

/// Auto-detect scale type and mode from a scale name string
pub fn parse_scale_name(scale_name: &str) -> Option<(ScaleType, ScaleMode)> {
    match scale_name.to_lowercase().as_str() {
        // Diatonic modes
        "ionian" | "major" => Some((ScaleType::Diatonic, ScaleMode::Ionian)),
        "dorian" => Some((ScaleType::Diatonic, ScaleMode::Dorian)),
        "phrygian" => Some((ScaleType::Diatonic, ScaleMode::Phrygian)),
        "lydian" => Some((ScaleType::Diatonic, ScaleMode::Lydian)),
        "mixolydian" => Some((ScaleType::Diatonic, ScaleMode::Mixolydian)),
        "aeolian" | "minor" => Some((ScaleType::Diatonic, ScaleMode::Aeolian)),
        "locrian" => Some((ScaleType::Diatonic, ScaleMode::Locrian)),
        
        // Melodic Minor modes - these map to the correct internal diatonic modes
        "melodic minor" => Some((ScaleType::MelodicMinor, ScaleMode::Ionian)), // Mode 1 maps to Ionian internally
        "dorian b2" | "dorian flat 2" => Some((ScaleType::MelodicMinor, ScaleMode::Dorian)), // Mode 2 maps to Dorian internally
        "lydian augmented" => Some((ScaleType::MelodicMinor, ScaleMode::Phrygian)), // Mode 3 maps to Phrygian internally
        "lydian dominant" => Some((ScaleType::MelodicMinor, ScaleMode::Lydian)), // Mode 4 maps to Lydian internally
        "mixolydian b6" | "mixolydian flat 6" => Some((ScaleType::MelodicMinor, ScaleMode::Mixolydian)), // Mode 5 maps to Mixolydian internally
        "aeolian b5" | "aeolian flat 5" => Some((ScaleType::MelodicMinor, ScaleMode::Aeolian)), // Mode 6 maps to Aeolian internally
        "super locrian" | "altered" => Some((ScaleType::MelodicMinor, ScaleMode::Locrian)), // Mode 7 maps to Locrian internally
        
        // Harmonic Minor modes - these map to the correct internal diatonic modes
        "harmonic minor" => Some((ScaleType::HarmonicMinor, ScaleMode::Aeolian)), // Mode 1 maps to Aeolian internally
        "locrian nat6" | "locrian natural 6" => Some((ScaleType::HarmonicMinor, ScaleMode::Locrian)), // Mode 2 maps to Locrian internally
        "ionian augmented" => Some((ScaleType::HarmonicMinor, ScaleMode::Ionian)), // Mode 3 maps to Ionian internally
        "romanian dorian" => Some((ScaleType::HarmonicMinor, ScaleMode::Dorian)), // Mode 4 maps to Dorian internally
        "phrygian dominant" => Some((ScaleType::HarmonicMinor, ScaleMode::Phrygian)), // Mode 5 maps to Phrygian internally
        "lydian #9" | "lydian sharp 9" => Some((ScaleType::HarmonicMinor, ScaleMode::Lydian)), // Mode 6 maps to Lydian internally
        "ultra locrian" => Some((ScaleType::HarmonicMinor, ScaleMode::Locrian)), // Mode 7 maps to Locrian internally
        
        _ => None,
    }
}

/// Create a key from a scale name string
pub fn key_from_scale_name(root: Note, scale_name: &str) -> Option<Key> {
    let (scale_type, mode) = parse_scale_name(scale_name)?;
    Some(Key::new(root, scale_type, mode))
}

/// Chord analysis result containing all the information about a chord in a key
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChordAnalysis {
    pub chord_quality: String,
    pub roman_numeral: String,
    pub scale_degree: u8,
    pub root_note: Note,
    pub chord_notes: Vec<Note>,
}

impl Key {
    /// Analyze a chord by note, key signature, and scale name
    /// Example: analyze_chord(G, C, "Ionian") -> ChordAnalysis for G7 in C major
    pub fn analyze_chord_by_note(root_note: Note, key_signature: Note, scale_name: &str) -> Option<ChordAnalysis> {
        let key = key_from_scale_name(key_signature, scale_name)?;
        let degree = key.degree_of_note(root_note)?;
        let chord_quality = key.natural_seventh_quality_by_degree(degree)?;
        let chord_notes = key.diatonic_seventh_chord(degree)?;
        
        // Convert degree to Roman numeral
        let roman_numeral = match degree {
            1 => "I",
            2 => "II", 
            3 => "III",
            4 => "IV",
            5 => "V",
            6 => "VI",
            7 => "VII",
            _ => return None,
        };
        
        Some(ChordAnalysis {
            chord_quality,
            roman_numeral: roman_numeral.to_string(),
            scale_degree: degree,
            root_note,
            chord_notes,
        })
    }
    
    /// Analyze a chord by scale degree and scale name
    /// Example: analyze_chord_by_degree(5, "Ionian") -> ChordAnalysis for degree 5 in major
    pub fn analyze_chord_by_degree(degree: u8, scale_name: &str) -> Option<ChordAnalysis> {
        if degree < 1 || degree > 7 {
            return None;
        }
        
        // Create a temporary key to get the chord quality
        let temp_key = key_from_scale_name(Note::c(), scale_name)?;
        let chord_quality = temp_key.natural_seventh_quality_by_degree(degree)?;
        let chord_notes = temp_key.diatonic_seventh_chord(degree)?;
        
        // Convert degree to Roman numeral
        let roman_numeral = match degree {
            1 => "I",
            2 => "II", 
            3 => "III",
            4 => "IV",
            5 => "V",
            6 => "VI",
            7 => "VII",
            _ => return None,
        };
        
        Some(ChordAnalysis {
            chord_quality,
            roman_numeral: roman_numeral.to_string(),
            scale_degree: degree,
            root_note: chord_notes[0],
            chord_notes,
        })
    }
    
    /// Analyze a chord by Roman numeral and scale name
    /// Example: analyze_chord_by_roman("V", "Ionian") -> ChordAnalysis for V in major
    pub fn analyze_chord_by_roman(roman: &str, scale_name: &str) -> Option<ChordAnalysis> {
        let degree = match roman.to_uppercase().as_str() {
            "I" => 1,
            "II" => 2,
            "III" => 3,
            "IV" => 4,
            "V" => 5,
            "VI" => 6,
            "VII" => 7,
            _ => return None,
        };
        
        Self::analyze_chord_by_degree(degree, scale_name)
    }
}

/// Extension trait for Note to support semitone transposition
impl Note {
    /// Transpose a note by a number of semitones with proper enharmonic spelling
    pub fn transpose_semitones(&self, semitones: u8) -> Note {
        let current_semitone = self.to_semitone();
        let new_semitone = (current_semitone + semitones) % 12;
        
        // Map semitones to proper note names with enharmonic spelling
        match new_semitone {
            0 => Note::new(NoteLiteral::C, None),
            1 => Note::new(NoteLiteral::C, Some(Modifier::Sharp)),
            2 => Note::new(NoteLiteral::D, None),
            3 => Note::new(NoteLiteral::D, Some(Modifier::Sharp)),
            4 => Note::new(NoteLiteral::E, None),
            5 => Note::new(NoteLiteral::F, None),
            6 => Note::new(NoteLiteral::F, Some(Modifier::Sharp)),
            7 => Note::new(NoteLiteral::G, None),
            8 => Note::new(NoteLiteral::G, Some(Modifier::Sharp)),
            9 => Note::new(NoteLiteral::A, None),
            10 => Note::new(NoteLiteral::A, Some(Modifier::Sharp)),
            11 => Note::new(NoteLiteral::B, None),
            _ => Note::new(NoteLiteral::C, None), // fallback
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_major_scale() {
        let c_major = Key::major(Note::c());
        let notes = c_major.notes();
        assert_eq!(notes.len(), 7);
        assert_eq!(notes[0], Note::c());
        assert_eq!(notes[1], Note::d());
        assert_eq!(notes[2], Note::e());
        assert_eq!(notes[3], Note::f());
        assert_eq!(notes[4], Note::g());
        assert_eq!(notes[5], Note::a());
        assert_eq!(notes[6], Note::b());
    }

    #[test]
    fn test_minor_scale() {
        let a_minor = Key::minor(Note::a());
        let notes = a_minor.notes();
        assert_eq!(notes.len(), 7);
        assert_eq!(notes[0], Note::a());
        assert_eq!(notes[1], Note::b());
        assert_eq!(notes[2], Note::c());
        assert_eq!(notes[3], Note::d());
        assert_eq!(notes[4], Note::e());
        assert_eq!(notes[5], Note::f());
        assert_eq!(notes[6], Note::g());
    }

    #[test]
    fn test_diatonic_chords() {
        let c_major = Key::major(Note::c());
        
        // I chord (C major)
        let i_chord = c_major.diatonic_chord(1).unwrap();
        assert_eq!(i_chord[0], Note::c());
        assert_eq!(i_chord[1], Note::e());
        assert_eq!(i_chord[2], Note::g());
        
        // V chord (G major)
        let v_chord = c_major.diatonic_chord(5).unwrap();
        assert_eq!(v_chord[0], Note::g());
        assert_eq!(v_chord[1], Note::b());
        assert_eq!(v_chord[2], Note::d());
    }

    #[test]
    fn test_key_names() {
        let c_major = Key::major(Note::c());
        assert_eq!(c_major.name(), "C Major");
        assert_eq!(c_major.short_name(), "C");
        
        let a_minor = Key::minor(Note::a());
        assert_eq!(a_minor.name(), "A Minor");
        assert_eq!(a_minor.short_name(), "Am");
    }

    #[test]
    fn test_all_modes_from_c() {
        let modes = [
            ScaleMode::Ionian,
            ScaleMode::Dorian,
            ScaleMode::Phrygian,
            ScaleMode::Lydian,
            ScaleMode::Mixolydian,
            ScaleMode::Aeolian,
            ScaleMode::Locrian,
        ];

        println!("All modes from C (Diatonic scale):");
        for mode in modes {
            let key = Key::new(Note::c(), ScaleType::Diatonic, mode);
            let notes = key.notes();
            let note_names: Vec<String> = notes.iter().map(|n| n.to_string()).collect();
            println!("{:?}: {}", mode, note_names.join(" "));
        }
    }

    #[test]
    fn test_scale_degrees_relative_to_ionian() {
        let modes = [
            ScaleMode::Ionian,
            ScaleMode::Dorian,
            ScaleMode::Phrygian,
            ScaleMode::Lydian,
            ScaleMode::Mixolydian,
            ScaleMode::Aeolian,
            ScaleMode::Locrian,
        ];

        println!("\nScale degrees relative to Ionian:");
        for mode in modes {
            let key = Key::new(Note::c(), ScaleType::Diatonic, mode);
            let degrees = key.scale_degrees_relative_to(ScaleMode::Ionian);
            println!("{:?}: {}", mode, degrees);
        }
    }

    #[test]
    fn test_scale_degrees_relative_to_aeolian() {
        let modes = [
            ScaleMode::Ionian,
            ScaleMode::Dorian,
            ScaleMode::Phrygian,
            ScaleMode::Lydian,
            ScaleMode::Mixolydian,
            ScaleMode::Aeolian,
            ScaleMode::Locrian,
        ];

        println!("\nScale degrees relative to Aeolian (natural minor):");
        for mode in modes {
            let key = Key::new(Note::c(), ScaleType::Diatonic, mode);
            let degrees = key.scale_degrees_relative_to(ScaleMode::Aeolian);
            println!("{:?}: {}", mode, degrees);
        }
    }

    #[test]
    fn test_chord_detection() {
        println!("\nChord detection from semitone sequences:");
        
        // Test various chord types
        let test_chords = vec![
            (vec![0, 4, 7], "Major triad"),
            (vec![0, 3, 7], "Minor triad"),
            (vec![0, 4, 7, 11], "Major7"),
            (vec![0, 3, 7, 10], "Minor7"),
            (vec![0, 4, 7, 10], "Dominant7"),
            (vec![0, 3, 6, 10], "Minor7b5"),
            (vec![0, 4, 7, 10, 14], "Dominant9"),
            (vec![0, 5, 7], "Sus4"),
        ];
        
        for (semitones, expected) in test_chords {
            let detected = Key::detect_chord_from_semitones(&semitones);
            println!("{:?} -> {} (expected: {})", semitones, detected.as_deref().unwrap_or("Unknown"), expected);
        }
    }

    #[test]
    fn test_diatonic_chords_in_c_major() {
        let c_major = Key::major(Note::c());
        
        println!("\nDiatonic chords in C Major:");
        let chords = c_major.diatonic_chords_up_to(7); // Up to 7th chords
        
        for (degree, chord_type, notes) in chords {
            let note_names: Vec<String> = notes.iter().map(|n| n.to_string()).collect();
            println!("Degree {}: {} ({})", degree, chord_type, note_names.join(" "));
        }
    }

    #[test]
    fn test_diatonic_chords_in_a_minor() {
        let a_minor = Key::minor(Note::a());
        
        println!("\nDiatonic chords in A Minor (Aeolian):");
        let chords = a_minor.diatonic_chords_up_to(7); // Up to 7th chords
        
        for (degree, chord_type, notes) in chords {
            let note_names: Vec<String> = notes.iter().map(|n| n.to_string()).collect();
            println!("Degree {}: {} ({})", degree, chord_type, note_names.join(" "));
        }
    }

    #[test]
    fn test_diatonic_chords_in_b_locrian() {
        let b_locrian = Key::locrian(Note::b());
        
        println!("\nDiatonic chords in B Locrian:");
        let chords = b_locrian.diatonic_chords_up_to(7); // Up to 7th chords
        
        for (degree, chord_type, notes) in chords {
            let note_names: Vec<String> = notes.iter().map(|n| n.to_string()).collect();
            println!("Degree {}: {} ({})", degree, chord_type, note_names.join(" "));
        }
    }

    #[test]
    fn test_melodic_minor_scale_generation() {
        let a_melodic_minor = Key::melodic_minor(Note::a());
        
        println!("\nA Melodic Minor Scale Analysis:");
        println!("Scale type: {:?}", a_melodic_minor.scale_type);
        println!("Mode: {:?}", a_melodic_minor.mode);
        
        let notes = a_melodic_minor.notes();
        let note_names: Vec<String> = notes.iter().map(|n| n.to_string()).collect();
        println!("Notes: {}", note_names.join(" "));
        
        // Show the semitone pattern
        let semitones = ScaleType::melodic_minor_semitone_sequence_with_mode(MelodicMinorMode::MelodicMinor);
        println!("Semitone pattern: {:?}", semitones);
        
        // Expected: A B C D E F# G# (ascending melodic minor)
        // Semitones: 0 2 3 5 7 9 11
        println!("Expected semitones: [0, 2, 3, 5, 7, 9, 11]");
    }

    #[test]
    fn test_melodic_minor_modes_from_c() {
        let root = Note::c();
        let modes = [
            MelodicMinorMode::MelodicMinor,
            MelodicMinorMode::DorianFlat2,
            MelodicMinorMode::LydianAugmented,
            MelodicMinorMode::LydianDominant,
            MelodicMinorMode::MixolydianFlat6,
            MelodicMinorMode::AeolianFlat5,
            MelodicMinorMode::SuperLocrian,
        ];

        println!("\nMelodic Minor modes from C:");
        for mode in modes {
            let key = Key::new_melodic_minor_mode(root, mode);
            let notes = key.notes();
            let note_names: Vec<String> = notes.iter().map(|n| n.to_string()).collect();
            println!("{} (aliases: {}): {}",
                mode.name(),
                mode.aliases().join(", "),
                note_names.join(" "));
        }
    }

    #[test]
    fn test_diatonic_chords_in_a_melodic_minor() {
        let a_melodic_minor = Key::melodic_minor(Note::a());
        
        println!("\nDiatonic chords in A Melodic Minor:");
        let chords = a_melodic_minor.diatonic_chords_up_to(7); // Up to 7th chords
        
        for (degree, chord_type, notes) in chords {
            let note_names: Vec<String> = notes.iter().map(|n| n.to_string()).collect();
            println!("Degree {}: {} ({})", degree, chord_type, note_names.join(" "));
        }
    }

    #[test]
    fn test_comprehensive_chord_detection() {
        println!("\n=== COMPREHENSIVE CHORD DETECTION TEST ===");
        
        // Test various chord types with different analysis methods
        let test_cases = vec![
            // Basic triads
            (vec![0, 4, 7], "Major triad"),
            (vec![0, 3, 7], "Minor triad"),
            (vec![0, 3, 6], "Diminished triad"),
            (vec![0, 4, 8], "Augmented triad"),
            
            // Seventh chords
            (vec![0, 4, 7, 11], "Major 7th"),
            (vec![0, 3, 7, 10], "Minor 7th"),
            (vec![0, 4, 7, 10], "Dominant 7th"),
            (vec![0, 3, 6, 10], "Half diminished 7th"),
            (vec![0, 3, 6, 9], "Fully diminished 7th"),
            
            // Extended chords
            (vec![0, 4, 7, 10, 14], "Dominant 9th"),
            (vec![0, 4, 7, 11, 14], "Major 9th"),
            (vec![0, 3, 7, 10, 14], "Minor 9th"),
            (vec![0, 4, 7, 10, 14, 17], "Dominant 11th"),
            (vec![0, 4, 7, 10, 14, 17, 21], "Dominant 13th"),
            
            // 6th chords
            (vec![0, 4, 7, 9], "Major 6th"),
            (vec![0, 3, 7, 9], "Minor 6th"),
            (vec![0, 4, 7, 9, 14], "6/9 chord"),
            
            // Suspended chords
            (vec![0, 5, 7], "Sus4"),
            (vec![0, 2, 7], "Sus2"),
            (vec![0, 5, 7, 10], "7Sus4"),
            
            // Altered chords
            (vec![0, 4, 7, 10, 13], "Dominant 7b9"),
            (vec![0, 4, 7, 10, 15], "Dominant 7#9"),
            (vec![0, 4, 7, 10, 18], "Dominant 7#11"),
        ];
        
        println!("Testing chord detection from semitone sequences:");
        for (semitones, expected) in test_cases {
            let detected = Key::detect_chord_from_semitones(&semitones);
            println!("{:?} -> {} (expected: {})", semitones, detected.as_deref().unwrap_or("Unknown"), expected);
        }
        
        // Test with different keys and modes
        println!("\n=== DIATONIC CHORDS IN DIFFERENT KEYS ===");
        
        let keys_to_test = vec![
            ("C Major", Key::major(Note::c())),
            ("A Minor", Key::minor(Note::a())),
            ("D Dorian", Key::dorian(Note::d())),
            ("E Phrygian", Key::phrygian(Note::e())),
        ];
        
        for (key_name, key) in keys_to_test {
            println!("\n{}:", key_name);
            let chords = key.diatonic_chords_up_to(7);
            for (degree, chord_type, notes) in chords {
                let note_names: Vec<String> = notes.iter().map(|n| n.to_string()).collect();
                println!("  Degree {}: {} ({})", degree, chord_type, note_names.join(" "));
            }
        }
    }

    #[test]
    fn test_melodic_minor_chords_from_c_all_modes() {
        let root = Note::c();
        let modes = [
            MelodicMinorMode::MelodicMinor,
            MelodicMinorMode::DorianFlat2,
            MelodicMinorMode::LydianAugmented,
            MelodicMinorMode::LydianDominant,
            MelodicMinorMode::MixolydianFlat6,
            MelodicMinorMode::AeolianFlat5,
            MelodicMinorMode::SuperLocrian,
        ];

        println!("\n=== DIATONIC SEVENTH CHORDS – MELODIC MINOR FAMILY FROM C ===");
        for mode in modes {
            let key = Key::new_melodic_minor_mode(root, mode);
            println!("\n{} (aliases: {}):", mode.name(), mode.aliases().join(", "));
            let chords = key.diatonic_chords_up_to(7);
            for (degree, chord_type, notes) in chords {
                let note_names: Vec<String> = notes.iter().map(|n| n.to_string()).collect();
                println!("  Degree {}: {} ({})", degree, chord_type, note_names.join(" "));
            }
        }
    }

    #[test]
    fn test_harmonic_minor_chords_from_c_all_modes() {
        let root = Note::c();
        let modes = [
            HarmonicMinorMode::HarmonicMinor,
            HarmonicMinorMode::LocrianNat6,
            HarmonicMinorMode::IonianAugmented,
            HarmonicMinorMode::RomanianDorian,
            HarmonicMinorMode::PhrygianDominant,
            HarmonicMinorMode::LydianSharp9,
            HarmonicMinorMode::UltraLocrian,
        ];

        println!("\n=== DIATONIC SEVENTH CHORDS – HARMONIC MINOR FAMILY FROM C ===");
        for mode in modes {
            let key = Key::new_harmonic_minor_mode(root, mode);
            println!("\n{} (aliases: {}):", mode.name(), mode.aliases().join(", "));
            let chords = key.diatonic_chords_up_to(7);
            for (degree, chord_type, notes) in chords {
                let note_names: Vec<String> = notes.iter().map(|n| n.to_string()).collect();
                println!("  Degree {}: {} ({})", degree, chord_type, note_names.join(" "));
            }
        }
    }

    #[test]
    fn test_chord_analysis_system() {
        println!("\n=== CHORD ANALYSIS SYSTEM TEST ===");
        
        // Test 1: G in C major (should be V7 - Dominant7)
        println!("\n1. G in C Ionian (should be V7 - Dominant7):");
        if let Some(analysis) = Key::analyze_chord_by_note(Note::g(), Note::c(), "Ionian") {
            let note_names: Vec<String> = analysis.chord_notes.iter().map(|n| n.to_string()).collect();
            println!("   Chord Quality: {}", analysis.chord_quality);
            println!("   Roman Numeral: {}", analysis.roman_numeral);
            println!("   Scale Degree: {}", analysis.scale_degree);
            println!("   Root Note: {}", analysis.root_note);
            println!("   Chord Notes: {}", note_names.join(" "));
        }
        
        // Test 2: Degree 5 in Ionian (should be Dominant7)
        println!("\n2. Degree 5 in Ionian (should be Dominant7):");
        if let Some(analysis) = Key::analyze_chord_by_degree(5, "Ionian") {
            let note_names: Vec<String> = analysis.chord_notes.iter().map(|n| n.to_string()).collect();
            println!("   Chord Quality: {}", analysis.chord_quality);
            println!("   Roman Numeral: {}", analysis.roman_numeral);
            println!("   Scale Degree: {}", analysis.scale_degree);
            println!("   Root Note: {}", analysis.root_note);
            println!("   Chord Notes: {}", note_names.join(" "));
        }
        
        // Test 3: Roman numeral V in Ionian (should be Dominant7)
        println!("\n3. Roman numeral V in Ionian (should be Dominant7):");
        if let Some(analysis) = Key::analyze_chord_by_roman("V", "Ionian") {
            let note_names: Vec<String> = analysis.chord_notes.iter().map(|n| n.to_string()).collect();
            println!("   Chord Quality: {}", analysis.chord_quality);
            println!("   Roman Numeral: {}", analysis.roman_numeral);
            println!("   Scale Degree: {}", analysis.scale_degree);
            println!("   Root Note: {}", analysis.root_note);
            println!("   Chord Notes: {}", note_names.join(" "));
        }
        
        // Test 4: Different scales
        println!("\n4. Testing different scales:");
        let scales = ["Dorian", "Phrygian Dominant", "Lydian Augmented", "Super Locrian"];
        for scale in scales {
            println!("\n   {} scale - Degree 1:", scale);
            if let Some(analysis) = Key::analyze_chord_by_degree(1, scale) {
                let note_names: Vec<String> = analysis.chord_notes.iter().map(|n| n.to_string()).collect();
                println!("     Chord Quality: {}", analysis.chord_quality);
                println!("     Roman Numeral: {}", analysis.roman_numeral);
                println!("     Chord Notes: {}", note_names.join(" "));
            }
        }
        
        // Test 5: Scale name parsing
        println!("\n5. Testing scale name parsing:");
        let scale_names = ["Ionian", "major", "Dorian", "melodic minor", "phrygian dominant", "super locrian"];
        for name in scale_names {
            if let Some((scale_type, mode)) = parse_scale_name(name) {
                // Show the actual mode name, not the internal ScaleMode enum
                let mode_name = match (scale_type, mode) {
                    (ScaleType::Diatonic, ScaleMode::Ionian) => "Ionian",
                    (ScaleType::Diatonic, ScaleMode::Dorian) => "Dorian",
                    (ScaleType::Diatonic, ScaleMode::Phrygian) => "Phrygian",
                    (ScaleType::Diatonic, ScaleMode::Lydian) => "Lydian",
                    (ScaleType::Diatonic, ScaleMode::Mixolydian) => "Mixolydian",
                    (ScaleType::Diatonic, ScaleMode::Aeolian) => "Aeolian",
                    (ScaleType::Diatonic, ScaleMode::Locrian) => "Locrian",
                    
                    (ScaleType::MelodicMinor, ScaleMode::Ionian) => "Melodic Minor",
                    (ScaleType::MelodicMinor, ScaleMode::Dorian) => "Dorian b2",
                    (ScaleType::MelodicMinor, ScaleMode::Phrygian) => "Lydian Augmented",
                    (ScaleType::MelodicMinor, ScaleMode::Lydian) => "Lydian Dominant",
                    (ScaleType::MelodicMinor, ScaleMode::Mixolydian) => "Mixolydian b6",
                    (ScaleType::MelodicMinor, ScaleMode::Aeolian) => "Aeolian b5",
                    (ScaleType::MelodicMinor, ScaleMode::Locrian) => "Super Locrian",
                    
                    (ScaleType::HarmonicMinor, ScaleMode::Aeolian) => "Harmonic Minor",
                    (ScaleType::HarmonicMinor, ScaleMode::Locrian) => "Locrian nat6",
                    (ScaleType::HarmonicMinor, ScaleMode::Ionian) => "Ionian Augmented",
                    (ScaleType::HarmonicMinor, ScaleMode::Dorian) => "Romanian Dorian",
                    (ScaleType::HarmonicMinor, ScaleMode::Phrygian) => "Phrygian Dominant",
                    (ScaleType::HarmonicMinor, ScaleMode::Lydian) => "Lydian #9",
                    _ => "Unknown",
                };
                println!("   '{}' -> {:?}, {}", name, scale_type, mode_name);
            } else {
                println!("   '{}' -> Not recognized", name);
            }
        }
    }

    #[test]
    fn test_chord_progressions_in_different_scales() {
        println!("\n=== CHORD PROGRESSIONS IN DIFFERENT SCALES ===");
        
        // Test progression: 1 4 6 5 in different scales
        let progression = [1, 4, 6, 5];
        let scales = ["Ionian", "Melodic Minor"];
        
        for scale_name in scales {
            println!("\n{} scale - Progression 1 4 6 5:", scale_name);
            
            for &degree in &progression {
                if let Some(analysis) = Key::analyze_chord_by_degree(degree, scale_name) {
                    let note_names: Vec<String> = analysis.chord_notes.iter().map(|n| n.to_string()).collect();
                    println!("  Degree {} ({}): {} ({})", 
                        degree, 
                        analysis.roman_numeral, 
                        analysis.chord_quality, 
                        note_names.join(" "));
                }
            }
        }
        
        // Test more complex progressions
        println!("\n=== COMPLEX PROGRESSIONS ===");
        
        let complex_progressions = [
            ("Ionian", [1, 6, 4, 5]),
            ("Dorian", [1, 4, 5, 1]),
            ("Phrygian Dominant", [1, 4, 5, 1]),
            ("Super Locrian", [1, 4, 5, 1]),
        ];
        
        for (scale_name, progression) in complex_progressions {
            println!("\n{} scale - Progression {:?}:", scale_name, progression);
            
            for &degree in &progression {
                if let Some(analysis) = Key::analyze_chord_by_degree(degree, scale_name) {
                    let note_names: Vec<String> = analysis.chord_notes.iter().map(|n| n.to_string()).collect();
                    println!("  Degree {} ({}): {} ({})", 
                        degree, 
                        analysis.roman_numeral, 
                        analysis.chord_quality, 
                        note_names.join(" "));
                }
            }
        }
        
        // Test all degrees in each scale type
        println!("\n=== ALL DEGREES IN EACH SCALE TYPE ===");
        
        let scale_types = ["Ionian", "Dorian", "Melodic Minor", "Phrygian Dominant", "Super Locrian"];
        
        for scale_name in scale_types {
            println!("\n{} scale - All degrees 1-7:", scale_name);
            
            for degree in 1..=7 {
                if let Some(analysis) = Key::analyze_chord_by_degree(degree, scale_name) {
                    let note_names: Vec<String> = analysis.chord_notes.iter().map(|n| n.to_string()).collect();
                    println!("  Degree {} ({}): {} ({})", 
                        degree, 
                        analysis.roman_numeral, 
                        analysis.chord_quality, 
                        note_names.join(" "));
                }
            }
        }
    }
}