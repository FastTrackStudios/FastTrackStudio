// Chord Memory System
//
// This module handles chord quality inference and memory based on:
// - Scale degrees (1-7) → infer quality from key signature
// - Roman numerals (I, ii, iii, etc.) → infer quality from case and key
// - Note names (C, D, E, etc.) → apply remembered quality or infer from key
//
// Key features:
// - Remembers chord qualities (e.g., "g" expands to "Gmaj7" if that was used before)
// - Section-specific memory: Each section type can have its own chord memory
// - One-time overrides: "!g7" uses G7 without affecting memory
// - Key signature awareness: Infers quality from scale degrees

use std::collections::HashMap;
use crate::chord::chord::ChordData;
use crate::key::keys::{Key, ScaleType, ScaleMode};
use crate::primitives::note::Note;
use crate::sections::SectionType;
use crate::chart::parser::{ChordInstance, RootNote};
use crate::rhythm::chord_rhythm::{ChordWithRhythm, ChordRhythm, TimingModifier};
use crate::chart::parser::MusicalDuration;
use crate::time::TimeSignature;

/// ChordMemory manages chord quality inference and remembers chord qualities across the chart
pub struct ChordMemory {
    /// Global chord memory: root -> full_symbol mapping
    pub global_memory: HashMap<String, String>,
    /// Per-section chord memory
    pub section_memory: HashMap<SectionType, HashMap<String, String>>,
    /// Current key context for inferring qualities
    pub current_key: Option<Key>,
    /// Current time signature for duration calculations
    pub time_signature: Option<TimeSignature>,
}

impl ChordMemory {
    pub fn new() -> Self {
        ChordMemory {
            global_memory: HashMap::new(),
            section_memory: HashMap::new(),
            current_key: None,
            time_signature: None,
        }
    }

    /// Set the current key context
    pub fn set_key(&mut self, key: Option<Key>) {
        self.current_key = key;
    }

    /// Set the time signature
    pub fn set_time_signature(&mut self, ts: Option<TimeSignature>) {
        self.time_signature = ts;
    }

    /// Apply default chord quality for note names, scale degrees, and Roman numerals
    /// Returns a ChordData with the proper chord structure
    pub fn apply_default_quality_for_note_name(&self, root: &str) -> Option<ChordData> {
        // Get current key signature
        let key = match self.current_key.as_ref() {
            Some(key) => key,
            None => return None,
        };
        
        // Get the scale name
        let scale_name = match (key.scale_type, key.mode) {
            (ScaleType::Diatonic, ScaleMode::Ionian) => "Ionian",
            (ScaleType::Diatonic, ScaleMode::Aeolian) => "Aeolian",
            (ScaleType::Diatonic, ScaleMode::Dorian) => "Dorian",
            (ScaleType::Diatonic, ScaleMode::Phrygian) => "Phrygian",
            (ScaleType::Diatonic, ScaleMode::Lydian) => "Lydian",
            (ScaleType::Diatonic, ScaleMode::Mixolydian) => "Mixolydian",
            (ScaleType::Diatonic, ScaleMode::Locrian) => "Locrian",
            (ScaleType::MelodicMinor, _) => "Melodic Minor",
            (ScaleType::HarmonicMinor, _) => "Harmonic Minor",
            _ => "Ionian", // Default fallback
        };
        
        // Try to analyze as different types of chord symbols
        let analysis = if let Ok(degree) = root.parse::<u8>() {
            // Scale degree (1-7)
            if degree >= 1 && degree <= 7 {
                Key::analyze_chord_by_degree(degree, scale_name)
            } else {
                None
            }
        } else if matches!(root.to_uppercase().as_str(), "I" | "II" | "III" | "IV" | "V" | "VI" | "VII") {
            // Roman numeral
            Key::analyze_chord_by_roman(root, scale_name)
        } else {
            // Note name - parse and analyze
            if let Some(root_note) = Note::from_string(root) {
                Key::analyze_chord_by_note(root_note, key.root, scale_name)
            } else {
                None
            }
        };
        
        if let Some(analysis) = analysis {
            
            // Get the root note for the chord
            let chord_root = if let Ok(degree) = root.parse::<u8>() {
                // For scale degrees, get the actual note from the key
                if let Some(note) = key.get_scale_degree_note(degree as usize) {
                    note
                } else {
                    return None;
                }
            } else if matches!(root.to_uppercase().as_str(), "I" | "II" | "III" | "IV" | "V" | "VI" | "VII") {
                // For Roman numerals, convert to scale degree first
                let degree = match root.to_uppercase().as_str() {
                    "I" => 1, "II" => 2, "III" => 3, "IV" => 4, "V" => 5, "VI" => 6, "VII" => 7,
                    _ => return None,
                };
                if let Some(note) = key.get_scale_degree_note(degree) {
                    note
                } else {
                    return None;
                }
            } else {
                // For note names, parse directly
                if let Some(note) = Note::from_string(root) {
                    note
                } else {
                    return None;
                }
            };
            
            // Create the appropriate chord based on the analysis
            let chord_data = match analysis.chord_quality.as_str() {
                "Major" => {
                    use crate::chord::chord::Chord;
                    Chord::maj().with_root(chord_root).to_data()
                },
                "Minor" => {
                    use crate::chord::chord::Chord;
                    Chord::min().with_root(chord_root).to_data()
                },
                "Dominant7" => {
                    use crate::chord::chord::Chord;
                    Chord::dom7().with_root(chord_root).to_data()
                },
                "Maj7" => {
                    use crate::chord::chord::Chord;
                    Chord::maj7().with_root(chord_root).to_data()
                },
                "Minor7" => {
                    use crate::chord::chord::Chord;
                    Chord::min7().with_root(chord_root).to_data()
                },
                "Diminished" => {
                    use crate::chord::chord::Chord;
                    Chord::dim().with_root(chord_root).to_data()
                },
                _ => {
                    // Default to major
                    use crate::chord::chord::Chord;
                    Chord::maj().with_root(chord_root).to_data()
                }
            };
            
            Some(chord_data)
        } else {
            None
        }
    }

    /// Get chord memory for a section type
    fn get_section_memory(&self, section_type: &SectionType) -> &HashMap<String, String> {
        self.section_memory.get(section_type).unwrap_or(&self.global_memory)
    }

    /// Get mutable chord memory for a section type
    fn get_section_memory_mut(&mut self, section_type: &SectionType) -> &mut HashMap<String, String> {
        self.section_memory.entry(section_type.clone()).or_insert_with(HashMap::new)
    }

    /// Transpose chord memory from one key to another
    pub fn transpose_memory(&mut self, from_key: &Key, to_key: &Key) {
        // Calculate semitone difference
        let from_semitones = from_key.root.semitones() as i32;
        let to_semitones = to_key.root.semitones() as i32;
        let mut diff = to_semitones - from_semitones;
        
        // Normalize to 0-11 range
        while diff < 0 {
            diff += 12;
        }
        while diff >= 12 {
            diff -= 12;
        }
        
        // Transpose global memory
        let mut new_global: HashMap<String, String> = HashMap::new();
        for (root, full_symbol) in &self.global_memory {
            if let Some(transposed_root) = Self::transpose_root(root, diff) {
                if let Some(transposed_symbol) = Self::transpose_symbol(full_symbol, diff) {
                    new_global.insert(transposed_root, transposed_symbol);
                }
            }
        }
        self.global_memory = new_global;
        
        // Transpose section memories
        let mut new_section_memory: HashMap<SectionType, HashMap<String, String>> = HashMap::new();
        for (stype, memory) in &self.section_memory {
            let mut new_memory: HashMap<String, String> = HashMap::new();
            for (root, full_symbol) in memory {
                if let Some(transposed_root) = Self::transpose_root(root, diff) {
                    if let Some(transposed_symbol) = Self::transpose_symbol(full_symbol, diff) {
                        new_memory.insert(transposed_root, transposed_symbol);
                    }
                }
            }
            new_section_memory.insert(stype.clone(), new_memory);
        }
        self.section_memory = new_section_memory;
    }

    /// Transpose a root note by semitones
    fn transpose_root(root: &str, semitones: i32) -> Option<String> {
        if let Some(note) = Note::from_string(root) {
            let transposed = note.transpose(semitones);
            Some(transposed.to_string())
        } else {
            // For scale degrees or Roman numerals, don't transpose
            Some(root.to_string())
        }
    }

    /// Transpose a full chord symbol by semitones
    fn transpose_symbol(symbol: &str, semitones: i32) -> Option<String> {
        // Parse the root from the symbol
        let root_end = symbol.chars().take_while(|c| {
            c.is_ascii_alphabetic() || *c == '#' || *c == 'b' || *c == '♯' || *c == '♭'
        }).count();
        
        if root_end == 0 {
            return None;
        }
        
        let root_str = &symbol[..root_end];
        let quality = &symbol[root_end..];
        
        if let Some(transposed_root) = Self::transpose_root(root_str, semitones) {
            Some(format!("{}{}", transposed_root, quality))
        } else {
            None
        }
    }
}

impl Default for ChordMemory {
    fn default() -> Self {
        Self::new()
    }
}

