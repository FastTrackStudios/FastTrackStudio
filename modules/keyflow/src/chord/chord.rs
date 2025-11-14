//! Chord definition and parsing
//!
//! Combines root notation with quality, family, and extensions to represent musical chords

use super::quality::{ChordQuality, SuspendedType};
use super::family::ChordFamily;
use super::degree::ChordDegree;
use super::alteration::Alteration;
use super::extensions::{Extensions, ExtensionQuality};
use super::duration::ChordRhythm;
use super::root;
use crate::primitives::{RootNotation, Interval, MusicalNote};
use crate::key::Key;
use crate::parsing::{Token, TokenType, ParseError};
use tracing::{debug, trace, instrument};
use std::collections::HashMap;

/// A complete chord with root, quality, family, extensions, and alterations
#[derive(Debug, Clone, PartialEq)]
pub struct Chord {
    /// The original input string that was parsed
    pub origin: String,
    
    /// The descriptor (everything after the root)
    pub descriptor: String,
    
    /// The normalized version of the chord
    pub normalized: String,
    
    /// The root note (can be note name, scale degree, or roman numeral)
    pub root: RootNotation,
    
    /// The chord quality (major, minor, diminished, augmented, suspended, power)
    pub quality: ChordQuality,
    
    /// The chord family (seventh type: maj7, dom7, m7, mM7, ø7, dim7). None = triad
    pub family: Option<ChordFamily>,
    
    /// Extensions (9th, 11th, 13th)
    pub extensions: Extensions,
    
    /// Alterations (b5, #5, b9, #9, #11, b13, etc.)
    pub alterations: Vec<Alteration>,
    
    /// Additions (add9, add11, etc.) - degrees added without implying lower extensions
    pub additions: Vec<ChordDegree>,
    
    /// Omissions (no3, no5, etc.) - degrees explicitly removed
    pub omissions: Vec<ChordDegree>,
    
    /// Bass note for slash chords (if different from root)
    pub bass: Option<RootNotation>,
    
    /// Optional rhythm/duration (None = no duration specified)
    pub duration: Option<ChordRhythm>,
    
    /// Computed intervals (real intervals from root)
    /// Key: ChordDegree, Value: Interval
    intervals: HashMap<ChordDegree, Interval>,
    
    /// Semantic degrees present in the chord
    semantic_degrees: Vec<ChordDegree>,
    
    /// Number of tokens consumed during parsing
    tokens_consumed: usize,
}

impl Chord {
    /// Create a new chord with just root and quality (triad, no extensions)
    pub fn new(root: RootNotation, quality: ChordQuality) -> Self {
        let mut chord = Self {
            origin: String::new(),
            descriptor: String::new(),
            normalized: String::new(),
            root,
            quality,
            family: None,
            extensions: Extensions::none(),
            alterations: Vec::new(),
            additions: Vec::new(),
            omissions: Vec::new(),
            bass: None,
            duration: None,
            intervals: HashMap::new(),
            semantic_degrees: Vec::new(),
            tokens_consumed: 0,
        };
        chord.compute_intervals();
        chord.normalize();
        chord
    }
    
    /// Create a new chord with duration
    pub fn with_duration(root: RootNotation, quality: ChordQuality, duration: ChordRhythm) -> Self {
        let mut chord = Self::new(root, quality);
        chord.duration = Some(duration);
        chord
    }
    
    /// Create a chord with explicit family (seventh type)
    pub fn with_family(root: RootNotation, quality: ChordQuality, family: ChordFamily) -> Self {
        let mut chord = Self {
            origin: String::new(),
            descriptor: String::new(),
            normalized: String::new(),
            root,
            quality,
            family: Some(family),
            extensions: Extensions::none(),
            alterations: Vec::new(),
            additions: Vec::new(),
            omissions: Vec::new(),
            bass: None,
            duration: None,
            intervals: HashMap::new(),
            semantic_degrees: Vec::new(),
            tokens_consumed: 0,
        };
        chord.compute_intervals();
        chord.normalize();
        chord
    }
    
    /// Compute intervals and semantic degrees from quality, family, extensions, and alterations
    pub(crate) fn compute_intervals(&mut self) {
        self.intervals.clear();
        self.semantic_degrees.clear();
        
        // Always include root
        self.intervals.insert(ChordDegree::Root, Interval::Unison);
        self.semantic_degrees.push(ChordDegree::Root);
        
        // Add intervals from quality (triad)
        for interval in self.quality.intervals() {
            let degree = ChordDegree::from_interval(interval);
            self.intervals.insert(degree, interval);
            if !self.semantic_degrees.contains(&degree) {
                self.semantic_degrees.push(degree);
            }
        }
        
        // Add seventh from family (if present)
        if let Some(family) = &self.family {
            let seventh_interval = family.seventh_interval();
            self.intervals.insert(ChordDegree::Seventh, seventh_interval);
            if !self.semantic_degrees.contains(&ChordDegree::Seventh) {
                self.semantic_degrees.push(ChordDegree::Seventh);
            }
        }
        
        // Add extensions
        for interval in self.extensions.intervals() {
            let degree = ChordDegree::from_interval(interval);
            self.intervals.insert(degree, interval);
            if !self.semantic_degrees.contains(&degree) {
                self.semantic_degrees.push(degree);
            }
        }
        
        // Apply alterations (override expected intervals)
        for alteration in &self.alterations {
            self.intervals.insert(alteration.degree, alteration.interval);
            if !self.semantic_degrees.contains(&alteration.degree) {
                self.semantic_degrees.push(alteration.degree);
            }
        }
        
        // Add additions
        for degree in &self.additions {
            if !self.intervals.contains_key(degree) {
                let interval = degree.to_expected_interval(self.quality);
                self.intervals.insert(*degree, interval);
            }
            if !self.semantic_degrees.contains(degree) {
                self.semantic_degrees.push(*degree);
            }
        }
        
        // Remove omissions
        for degree in &self.omissions {
            self.intervals.remove(degree);
            self.semantic_degrees.retain(|d| d != degree);
        }
        
        // Sort semantic degrees
        self.semantic_degrees.sort();
    }
    
    /// Get all intervals in this chord
    pub fn intervals(&self) -> Vec<Interval> {
        let mut intervals: Vec<_> = self.intervals.values().copied().collect();
        intervals.sort_by_key(|i| i.semitones());
        intervals
    }
    
    /// Get all semantic degrees in this chord
    pub fn semantic_degrees(&self) -> &[ChordDegree] {
        &self.semantic_degrees
    }
    
    /// Get the interval for a specific degree (if present)
    pub fn interval_for_degree(&self, degree: ChordDegree) -> Option<Interval> {
        self.intervals.get(&degree).copied()
    }
    
    /// Check if a degree is present in the chord
    pub fn has_degree(&self, degree: ChordDegree) -> bool {
        self.intervals.contains_key(&degree)
    }
    
    /// Add an alteration to the chord
    pub fn add_alteration(&mut self, alteration: Alteration) -> Result<(), String> {
        // Check if the degree is already present or will be added
        if !self.has_degree(alteration.degree) {
            return Err(format!(
                "Cannot alter degree {} which is not present in the chord",
                alteration.degree
            ));
        }
        
        // Check for conflicting alterations
        if self.alterations.iter().any(|a| a.degree == alteration.degree) {
            return Err(format!(
                "Degree {} is already altered",
                alteration.degree
            ));
        }
        
        self.alterations.push(alteration);
        self.compute_intervals();
        Ok(())
    }
    
    /// Add an addition (e.g., add9, add11)
    pub fn add_addition(&mut self, degree: ChordDegree) {
        if !self.additions.contains(&degree) {
            self.additions.push(degree);
            self.compute_intervals();
        }
    }
    
    /// Add an omission (e.g., no3, no5)
    pub fn add_omission(&mut self, degree: ChordDegree) {
        if !self.omissions.contains(&degree) {
            self.omissions.push(degree);
            self.compute_intervals();
        }
    }
    
    /// Set extensions
    pub fn set_extensions(&mut self, extensions: Extensions) {
        self.extensions = extensions;
        self.compute_intervals();
    }
    
    /// Set the bass note (for slash chords)
    pub fn set_bass(&mut self, bass: RootNotation) {
        self.bass = Some(bass);
    }
    
    /// Get the root note as a MusicalNote
    /// 
    /// For note names, this returns the note directly.
    /// For scale degrees and roman numerals, a Key is required to resolve them.
    /// 
    /// # Arguments
    /// * `key` - Optional key context for resolving scale degrees and roman numerals
    /// 
    /// # Returns
    /// * `Some(MusicalNote)` if the root can be resolved
    /// * `None` if the root requires a key context that wasn't provided
    pub fn root_note(&self, key: Option<&Key>) -> Option<MusicalNote> {
        self.root.resolve(key)
    }
    
    /// Get the semitone sequence for this chord
    /// 
    /// Returns a vector of semitones relative to the root, preserving octave information.
    /// This is useful for voicings where intervals can span multiple octaves.
    /// The root is always 0 (first element).
    /// 
    /// For example:
    /// - Cmaj7 = [0, 4, 7, 11] - all within first octave
    /// - C9 = [0, 4, 7, 10, 14] - ninth is in second octave
    /// - C13 = [0, 4, 7, 10, 14, 17, 21] - extends to second octave
    /// 
    /// # Example
    /// ```
    /// use keyflow::chord::from_semitones;
    /// use keyflow::primitives::{RootNotation, MusicalNote};
    /// 
    /// // Build a C9 chord from semitones
    /// let root = RootNotation::from_note_name(MusicalNote::c());
    /// // C9 = C (0), E (4), G (7), Bb (10), D (14 - second octave)
    /// let chord = from_semitones(&[0, 4, 7, 10, 14], root).unwrap();
    /// assert_eq!(chord.semitone_sequence(), vec![0, 4, 7, 10, 14]);
    /// ```
    pub fn semitone_sequence(&self) -> Vec<u8> {
        let mut semitones: Vec<u8> = self.intervals
            .values()
            .map(|interval| interval.semitones())
            .collect();
        
        // Ensure root (0) is included and sort
        if !semitones.contains(&0) {
            semitones.push(0);
        }
        semitones.sort_unstable();
        semitones
    }
    
    /// Get the pitch class set for this chord (semitones within one octave)
    /// 
    /// Returns a vector of semitones (0-11) relative to the root, sorted in ascending order.
    /// All intervals are reduced to the first octave. This is useful for analyzing
    /// chord quality regardless of voicing.
    /// 
    /// # Example
    /// ```
    /// use keyflow::chord::from_semitones;
    /// use keyflow::primitives::{RootNotation, MusicalNote};
    /// 
    /// // Build a C9 chord from semitones
    /// let root = RootNotation::from_note_name(MusicalNote::c());
    /// let chord = from_semitones(&[0, 4, 7, 10, 14], root).unwrap();
    /// // C9 with ninth in second octave reduces to: C (0), D (2), E (4), G (7), Bb (10)
    /// assert_eq!(chord.pitch_classes(), vec![0, 2, 4, 7, 10]);
    /// ```
    pub fn pitch_classes(&self) -> Vec<u8> {
        let mut semitones: Vec<u8> = self.intervals
            .values()
            .map(|interval| interval.semitones() % 12)
            .collect();
        
        // Ensure root (0) is included and sort
        if !semitones.contains(&0) {
            semitones.push(0);
        }
        semitones.sort_unstable();
        semitones.dedup(); // Remove duplicates (e.g., if both 2 and 14 are present, we get only 2)
        semitones
    }
    
    /// Get all notes in the chord as MusicalNote objects
    /// 
    /// Returns a vector of MusicalNote objects representing each tone in the chord.
    /// The notes are ordered from lowest to highest (root first).
    /// 
    /// For correct enharmonic spelling, a Key context should be provided.
    /// Without a key, notes will use sharp/flat based on the root note's preference.
    /// 
    /// # Arguments
    /// * `key` - Optional key context for proper enharmonic spelling and resolving scale degrees/roman numerals
    /// 
    /// # Returns
    /// * `Some(Vec<MusicalNote>)` if the root can be resolved
    /// * `None` if the root requires a key context that wasn't provided
    /// 
    /// # Example
    /// ```
    /// use keyflow::chord::from_semitones;
    /// use keyflow::primitives::{RootNotation, MusicalNote};
    /// 
    /// // Build a Cmaj7 chord from semitones
    /// let root = RootNotation::from_note_name(MusicalNote::c());
    /// // Cmaj7 = C (0), E (4), G (7), B (11)
    /// let chord = from_semitones(&[0, 4, 7, 11], root).unwrap();
    /// // Cmaj7 returns [C, E, G, B]
    /// let notes = chord.notes(None).unwrap();
    /// assert_eq!(notes.len(), 4);
    /// ```
    pub fn notes(&self, key: Option<&Key>) -> Option<Vec<MusicalNote>> {
        // Get the root note
        let root = self.root_note(key)?;
        
        // Build a list of (semitones, chord_degree) pairs and sort by semitones
        // Keep the full semitone value to preserve octave information for extensions
        let mut degree_semitone_pairs: Vec<(u8, ChordDegree)> = self.semantic_degrees
            .iter()
            .filter_map(|&degree| {
                self.intervals.get(&degree).map(|interval| {
                    (interval.semitones(), degree)
                })
            })
            .collect();
        
        // Sort by semitone (ascending) - this preserves octave ordering
        degree_semitone_pairs.sort_by_key(|(semitones, _)| *semitones);
        
        // Generate notes using enharmonically correct spelling based on chord degrees
        let mut notes = Vec::with_capacity(degree_semitone_pairs.len());
        
        for (semitones, chord_degree) in degree_semitone_pairs {
            // Use semantic interval to determine correct letter name
            let semantic_interval = chord_degree.semantic_interval();
            
            // Generate enharmonically correct note
            // The semitones value preserves octave (e.g., 14 for ninth, not 2)
            // but enharmonic_from_root uses % 12 internally for pitch class
            let note = MusicalNote::enharmonic_from_root(&root, semitones % 12, semantic_interval);
            notes.push(note);
        }
        
        Some(notes)
    }
    
    /// Transpose this chord to a new key
    /// 
    /// Unified algorithm that works for all transposition scenarios:
    /// 1. **Root-only transposition** (C Major → G Major): Transposes by interval
    /// 2. **Scale-type change** (C Major → C Minor): Applies enharmonic mapping
    /// 3. **Both** (C Major → G Minor): Combines both
    /// 
    /// The algorithm:
    /// 1. Get all notes in the chord
    /// 2. Transpose each note by the interval between source and target roots
    /// 3. Apply enharmonic changes based on the target scale
    ///    (e.g., E→Eb when going to C minor, A→Ab, B→Bb)
    /// 4. Recalculate chord quality from the resulting notes
    /// 
    /// # Arguments
    /// * `target_key` - The key to transpose to (provides both root and scale type)
    /// * `source_key` - Optional source key for resolving scale degrees/roman numerals
    /// 
    /// # Returns
    /// * `Some(Chord)` - The transposed chord
    /// * `None` - If the root cannot be resolved
    /// 
    /// # Examples
    /// ```
    /// use keyflow::chord::from_semitones;
    /// use keyflow::primitives::{RootNotation, MusicalNote};
    /// use keyflow::key::Key;
    /// 
    /// // Root transposition: Cmaj7 → Gmaj7
    /// let root = RootNotation::from_note_name(MusicalNote::c());
    /// // Cmaj7 = C (0), E (4), G (7), B (11)
    /// let c_maj7 = from_semitones(&[0, 4, 7, 11], root).unwrap();
    /// let g_key = Key::major(MusicalNote::g());
    /// let g_maj7 = c_maj7.transpose_to(&g_key, None).unwrap();
    /// 
    /// // Scale type change: C → Cm (C Major → C Minor)
    /// let c_root = RootNotation::from_note_name(MusicalNote::c());
    /// let c_major = from_semitones(&[0, 4, 7], c_root).unwrap();
    /// let c_key = Key::major(MusicalNote::c());
    /// let c_minor_key = Key::minor(MusicalNote::c());
    /// let c_minor_chord = c_major.transpose_to(&c_minor_key, Some(&c_key)).unwrap();
    /// ```
    pub fn transpose_to(&self, target_key: &Key, source_key: Option<&Key>) -> Option<Self> {
        // Get the current root note
        let current_root = self.root_note(source_key)?;
        let target_root = target_key.root();
        
        // Check if the scale mode changed - if so, we need to map scale degrees
        let source_scale_mode = source_key.map(|k| k.mode);
        let target_scale_mode = target_key.mode;
        let scale_mode_changed = source_scale_mode.is_some() 
            && source_scale_mode.unwrap() != target_scale_mode;
        
        // Calculate the transposition interval
        // If scale mode changed and the chord root is a scale degree in the source key,
        // we need to find what scale degree it is and map to that degree in target key
        let interval_semitones = if scale_mode_changed && source_key.is_some() {
            let src_key = source_key.unwrap();
            
            // Check if current root is a scale degree in source key
            let mut found_scale_degree = None;
            for deg in 1..=7 {
                if let Some(scale_note) = src_key.get_scale_degree(deg) {
                    if scale_note.semitone == current_root.semitone {
                        found_scale_degree = Some(deg);
                        break;
                    }
                }
            }
            
            if let Some(degree) = found_scale_degree {
                // Map to the same degree in target key
                if let Some(target_scale_note) = target_key.get_scale_degree(degree) {
                    (target_scale_note.semitone + 12 - current_root.semitone) % 12
                } else {
                    (target_root.semitone + 12 - current_root.semitone) % 12
                }
            } else {
                (target_root.semitone + 12 - current_root.semitone) % 12
            }
        } else {
            (target_root.semitone + 12 - current_root.semitone) % 12
        };
        
        // Step 1: Transpose all notes by the musical note interval
        // This changes E → Eb, but doesn't change chord quality
        let mut transposed_notes: Vec<MusicalNote> = Vec::new();
        
        for &degree in &self.semantic_degrees {
            if let Some(interval) = self.intervals.get(&degree) {
                let semantic_interval = degree.semantic_interval();
                let semitone_offset = interval.semitones();
                let new_semitone = (current_root.semitone + semitone_offset + interval_semitones) % 12;
                
                // For the root degree
                if degree == ChordDegree::Root {
                    // Calculate the transposed root semitone
                    let transposed_root_semitone = (current_root.semitone + interval_semitones) % 12;
                    
                    // Try to find this in the target scale
                    let mut found_root = false;
                    for scale_deg in 1..=7 {
                        if let Some(scale_note) = target_key.get_scale_degree(scale_deg) {
                            if scale_note.semitone == transposed_root_semitone {
                                transposed_notes.push(scale_note);
                                found_root = true;
                                break;
                            }
                        }
                    }
                    
                    if !found_root {
                        // Not in scale - use enharmonic spelling
                        let note = MusicalNote::enharmonic_from_root(&target_root, transposed_root_semitone, 1);
                        transposed_notes.push(note);
                    }
                } else {
                    // For other notes, try to find in target scale first
                    let mut found_in_scale = false;
                    for scale_deg in 1..=7 {
                        if let Some(scale_note) = target_key.get_scale_degree(scale_deg) {
                            if scale_note.semitone == new_semitone {
                                transposed_notes.push(scale_note);
                                found_in_scale = true;
                                break;
                            }
                        }
                    }
                    
                    if !found_in_scale {
                        // Not in scale - use enharmonic spelling
                        let note = MusicalNote::enharmonic_from_root(&target_root, new_semitone, semantic_interval);
                        transposed_notes.push(note);
                    }
                }
            }
        }
        
        // Step 2: If scale mode changed, apply scale transformations
        // This changes chord quality (Major → Minor, etc.)
        // We need to compare the actual ScaleMode, not just ScaleType,
        // because Ionian and Aeolian are both Diatonic but different modes
        let source_scale_mode = source_key.map(|k| k.mode);
        let target_scale_mode = target_key.mode;
        let scale_mode_changed = source_scale_mode.is_some() 
            && source_scale_mode.unwrap() != target_scale_mode;
        
        if scale_mode_changed {
            let mut scale_transformed_notes: Vec<MusicalNote> = Vec::new();
            
            // Find where the chord root is in the target scale
            let new_chord_root = &transposed_notes[0];
            let mut chord_root_scale_degree = None;
            for deg in 1..=7 {
                if let Some(scale_note) = target_key.get_scale_degree(deg) {
                    if scale_note.semitone == new_chord_root.semitone {
                        chord_root_scale_degree = Some(deg);
                        break;
                    }
                }
            }
            
            if let Some(root_deg) = chord_root_scale_degree {
                
                // For each chord tone, count up from the chord root's position in the scale
                // This includes extensions (9th, 11th, 13th) which we handle by:
                // 1. Flattening to get the scale degree (9 → 2, 11 → 4, 13 → 6)
                // 2. Transforming through the scale
                // 3. Preserving the octave offset
                
                for &degree in &self.semantic_degrees {
                    if let Some(interval) = self.intervals.get(&degree) {
                        let original_semitones = interval.semitones();
                        let octave_offset = (original_semitones / 12) * 12;
                        
                        // Use semantic_interval which already flattens (9th→2, 11th→4, 13th→6)
                        let semantic_interval = degree.semantic_interval();
                        
                        // Calculate which scale degree this chord tone should map to
                        let target_scale_deg = ((root_deg - 1) + (semantic_interval - 1)) % 7 + 1;
                        
                        if let Some(scale_note) = target_key.get_scale_degree(target_scale_deg) {
                            // Calculate the new semitone value with octave preserved
                            let new_semitone_within_octave = (scale_note.semitone + 12 - new_chord_root.semitone) % 12;
                            let new_total_semitones = new_semitone_within_octave + octave_offset;
                            
                            // Create a note with the transformed pitch but preserve the name from scale
                            let mut transformed_note = scale_note;
                            // The semitone is used for sorting and interval calculation
                            // but the name (e.g., "Ab") comes from the scale
                            transformed_note.semitone = new_total_semitones;
                            
                            scale_transformed_notes.push(transformed_note);
                        }
                    }
                }
                
                transposed_notes = scale_transformed_notes;
            }
            // If chord root not in scale, keep original transposed notes
        }
        
        // The first note is the new root
        // If the root has semitone 0 (relative), we need to find its absolute semitone
        // by looking up its name in the target key
        let root_note_for_notation = if transposed_notes[0].semitone == 0 {
            // Root is relative (semitone 0), find absolute semitone by looking up the name
            let root_name = &transposed_notes[0].name;
            let mut absolute_note = transposed_notes[0].clone();
            // Search for this note name in the target scale
            for deg in 1..=7 {
                if let Some(scale_note) = target_key.get_scale_degree(deg) {
                    if scale_note.name == *root_name {
                        absolute_note.semitone = scale_note.semitone;
                        break;
                    }
                }
            }
            absolute_note
        } else {
            transposed_notes[0].clone()
        };
        let new_root = RootNotation::from_note_name(root_note_for_notation);
        
        // Recalculate quality from the transposed notes
        let new_quality = self.calculate_quality_from_notes(&transposed_notes)?;
        
        // Recalculate family if needed
        let new_family = if scale_mode_changed && self.family.is_some() {
            // If quality changed, we need to update the family
            if new_quality != self.quality {
                match (new_quality, self.family.unwrap()) {
                    (ChordQuality::Major, ChordFamily::Minor7) => Some(ChordFamily::Dominant7),
                    (ChordQuality::Major, ChordFamily::MinorMajor7) => Some(ChordFamily::Major7),
                    (ChordQuality::Minor, ChordFamily::Major7) => Some(ChordFamily::MinorMajor7),
                    (ChordQuality::Minor, ChordFamily::Dominant7) => Some(ChordFamily::Minor7),
                    _ => self.family,
                }
            } else {
                self.family
            }
        } else {
            self.family
        };
        
        // After scale transformation, recalculate extensions to be all Natural
        // because the transformed notes are now the natural scale degrees in the target scale
        let new_extensions = if scale_mode_changed && self.extensions.has_any() {
            let mut ext = Extensions::none();
            if self.extensions.ninth.is_some() {
                ext.ninth = Some(ExtensionQuality::Natural);
            }
            if self.extensions.eleventh.is_some() {
                ext.eleventh = Some(ExtensionQuality::Natural);
            }
            if self.extensions.thirteenth.is_some() {
                ext.thirteenth = Some(ExtensionQuality::Natural);
            }
            ext
        } else {
            self.extensions.clone()
        };
        
        // Create a new chord with transposed properties
        let mut transposed = Self {
            origin: String::new(),
            descriptor: self.descriptor.clone(),
            normalized: String::new(),
            root: new_root,
            quality: new_quality,
            family: new_family,
            extensions: new_extensions,
            alterations: self.alterations.clone(),
            additions: self.additions.clone(),
            omissions: self.omissions.clone(),
            bass: if let Some(ref bass) = self.bass {
                self.transpose_root_notation(bass, interval_semitones)
            } else {
                None
            },
            duration: self.duration.clone(),
            intervals: HashMap::new(),
            semantic_degrees: Vec::new(),
            tokens_consumed: 0,
        };
        
        // If scale was transformed, build intervals from the transformed notes
        // Otherwise, compute intervals normally
        if scale_mode_changed && !transposed_notes.is_empty() {
            // Build intervals map from the transformed notes
            let new_root_semitone = transposed_notes[0].semitone;
            for (i, &degree) in self.semantic_degrees.iter().enumerate() {
                if i < transposed_notes.len() {
                    let note_semitone = transposed_notes[i].semitone;
                    // Calculate semitone difference (preserving octave information)
                    let semitone_diff = (note_semitone + 120 - new_root_semitone) % 120;
                    
                    // Map the semitone difference to the appropriate Interval variant
                    // For extensions (9th, 11th, 13th), we need to use the extension-specific variants
                    let interval = match (degree, semitone_diff % 12) {
                        // 9ths
                        (ChordDegree::Ninth, 1) => Some(Interval::FlatNinth),
                        (ChordDegree::Ninth, 2) => Some(Interval::Ninth),
                        (ChordDegree::Ninth, 3) => Some(Interval::SharpNinth),
                        // 11ths
                        (ChordDegree::Eleventh, 5) => Some(Interval::Eleventh),
                        (ChordDegree::Eleventh, 6) => Some(Interval::SharpEleventh),
                        // 13ths
                        (ChordDegree::Thirteenth, 8) => Some(Interval::FlatThirteenth),
                        (ChordDegree::Thirteenth, 9) => Some(Interval::Thirteenth),
                        // Everything else use from_semitones
                        _ => Interval::from_semitones((semitone_diff % 12) as u8),
                    };
                    
                    if let Some(interval) = interval {
                        transposed.intervals.insert(degree, interval);
                    }
                }
            }
            transposed.semantic_degrees = self.semantic_degrees.clone();
        } else {
            // Recompute intervals normally
            transposed.compute_intervals();
        }
        
        transposed.normalize();
        
        Some(transposed)
    }
    
    /// Helper: Transpose a root notation by a semitone interval
    fn transpose_root_notation(&self, root: &RootNotation, semitones: u8) -> Option<RootNotation> {
        if let Some(note) = root.resolve(None) {
            let new_semitone = (note.semitone + semitones) % 12;
            let new_note = MusicalNote::from_semitone(new_semitone, note.name.contains('#'));
            Some(RootNotation::from_note_name(new_note))
        } else {
            None
        }
    }
    /// Helper: Calculate chord quality from a set of notes (for scale-type transposition)
    /// 
    /// Examines the interval between root and third to determine quality
    fn calculate_quality_from_notes(&self, notes: &[MusicalNote]) -> Option<ChordQuality> {
        if notes.is_empty() {
            return None;
        }
        
        let root = &notes[0];
        
        // Find the third (if present)
        for note in notes.iter().skip(1) {
            let interval = (note.semitone + 12 - root.semitone) % 12;
            match interval {
                3 => return Some(ChordQuality::Minor),  // Minor third
                4 => return Some(ChordQuality::Major),  // Major third
                _ => continue,
            }
        }
        
        // No third found, keep original quality
        Some(self.quality)
    }
    
    /// Get the scale degrees of each chord tone in a given key
    /// 
    /// Returns a vector of tuples mapping each ChordDegree to its scale degree (1-7)
    /// in the given key. This is useful for understanding the chord's function within
    /// a key (e.g., "this chord has the 3rd and 7th of the key").
    /// 
    /// # Arguments
    /// * `key` - The key to analyze the chord in
    /// 
    /// # Returns
    /// * `Some(Vec<(ChordDegree, u8)>)` if the chord can be analyzed in this key
    /// * `None` if the root cannot be resolved
    /// 
    /// # Example
    /// ```
    /// use keyflow::chord::from_semitones;
    /// use keyflow::primitives::{RootNotation, MusicalNote};
    /// use keyflow::key::Key;
    /// use keyflow::chord::ChordDegree;
    /// 
    /// // Cmaj7 in C major: C=1, E=3, G=5, B=7
    /// let root = RootNotation::from_note_name(MusicalNote::c());
    /// // Cmaj7 = C (0), E (4), G (7), B (11)
    /// let chord = from_semitones(&[0, 4, 7, 11], root).unwrap();
    /// let degrees = chord.scale_degrees(&Key::major(MusicalNote::c())).unwrap();
    /// assert_eq!(degrees[0], (ChordDegree::Root, 1));
    /// assert_eq!(degrees[1], (ChordDegree::Third, 3));
    /// ```
    pub fn scale_degrees(&self, key: &Key) -> Option<Vec<(ChordDegree, u8)>> {
        let key_root = key.root();
        
        // Map each chord degree to its scale degree in the key
        let mut result = Vec::with_capacity(self.semantic_degrees.len());
        
        // Iterate through semantic_degrees (these are the chord tones we have)
        for chord_degree in &self.semantic_degrees {
            // Get the interval for this chord degree
            if let Some(interval) = self.intervals.get(chord_degree) {
                // Get the actual note for this interval
                let root_note = self.root_note(Some(key))?;
                let note_semitone = (root_note.semitone + interval.semitones()) % 12;
                
                // Calculate the semitone distance from the key root
                let interval_from_key = (note_semitone + 12 - key_root.semitone) % 12;
                
                // Map semitones to scale degrees (1-7)
                // This is approximate - in a real implementation you'd check against the actual scale
                let scale_degree = match interval_from_key {
                    0 => 1,  // Root
                    1 | 2 => 2,  // Second (major or minor)
                    3 | 4 => 3,  // Third (minor or major)
                    5 => 4,  // Fourth
                    6 | 7 => 5,  // Fifth (dim, perfect, or aug)
                    8 | 9 => 6,  // Sixth (minor or major)
                    10 | 11 => 7,  // Seventh (minor or major)
                    _ => 1,  // Fallback (shouldn't reach here)
                };
                
                result.push((*chord_degree, scale_degree));
            }
        }
        
        Some(result)
    }
    
    /// Check if the quality should be displayed for scale degree/Roman numeral roots
    /// 
    /// For scale degrees and Roman numerals, the quality is often implied by the key.
    /// We only show the quality suffix if it differs from the diatonic default.
    /// 
    /// # Arguments
    /// * `key` - Optional key context to determine diatonic quality
    /// 
    /// # Returns
    /// * `true` if quality should be shown (explicit or non-diatonic)
    /// * `false` if quality should be hidden (matches diatonic default)
    fn should_show_quality(&self, key: Option<&Key>) -> bool {
        use crate::primitives::RootFormat;
        use crate::key::scale::harmonization::{harmonize_scale, HarmonizationDepth};
        
        // Always show quality for note names
        if !matches!(self.root.original_format(), 
            RootFormat::ScaleDegree(_) | RootFormat::RomanNumeral { .. }) {
            return true;
        }
        
        // If no key context, show the quality
        let key = match key {
            Some(k) => k,
            None => return true,
        };
        
        // Get the scale degree
        let degree = match self.root.original_format() {
            RootFormat::ScaleDegree(d) => *d,
            RootFormat::RomanNumeral { degree, .. } => *degree,
            _ => return true,
        };
        
        // Harmonize the scale to get diatonic chords
        let depth = if self.family.is_some() {
            HarmonizationDepth::Sevenths
        } else {
            HarmonizationDepth::Triads
        };
        
        let diatonic_chords = harmonize_scale(&key.mode, &key.root, depth);
        
        // Get the diatonic chord for this degree (degree is 1-7, array is 0-6)
        if let Some(diatonic_chord) = diatonic_chords.get((degree - 1) as usize) {
            // Compare qualities - only show if different from diatonic
            return self.quality != diatonic_chord.quality;
        }
        
        // If we can't determine, show the quality
        true
    }
    
    /// Normalize the chord representation
    /// 
    /// Computes:
    /// - descriptor: everything after the root in standard form
    /// - normalized: full normalized chord string (root + descriptor)
    /// 
    /// Normalization rules:
    /// - Use standard quality symbols: "" for major, "m" for minor
    /// - Use "M7" or "maj7" consistently (we'll use "maj7")
    /// - Use "7" for dominant seventh
    /// - Use standard alteration/extension notation
    /// - Order: quality -> family -> extensions -> alterations -> additions -> omissions -> bass
    pub(crate) fn normalize(&mut self) {
        // Build descriptor from components
        let mut desc = String::new();
        
        // Check if this is a sixth chord (has 6th addition but no seventh family)
        let is_sixth_chord = self.additions.contains(&ChordDegree::Sixth) && self.family.is_none();
        let is_six_nine_chord = is_sixth_chord && self.additions.contains(&ChordDegree::Ninth);
        
        // Quality
        // For major sixth chords, we need to show "maj" explicitly (Cmaj6, not C6)
        // EXCEPT for 6/9 chords which stay as C6/9 (not Cmaj6/9)
        // For minor sixth chords, "m" is already shown by the quality
        if is_sixth_chord && !is_six_nine_chord && self.quality == ChordQuality::Major {
            desc.push_str("maj");
        } else {
            desc.push_str(&self.quality.to_string());
        }
        
        // Family (seventh type) and Extensions
        // Special handling for Major family with extensions: show "maj" + extension number
        if let Some(ref family) = self.family {
            if matches!(family, ChordFamily::Major7 | ChordFamily::MinorMajor7) && self.extensions.has_any() {
                // For maj9, maj11, maj13: show "maj" + highest extension
                desc.push_str("maj");
                // Show only the highest extension number
                if self.extensions.thirteenth.is_some() {
                    desc.push_str("13");
                } else if self.extensions.eleventh.is_some() {
                    desc.push_str("11");
                } else if self.extensions.ninth.is_some() {
                    desc.push_str("9");
                }
                // Show altered extensions separately
                if let Some(qual) = self.extensions.ninth {
                    if qual != ExtensionQuality::Natural {
                        match qual {
                            ExtensionQuality::Flat => desc.push_str("b9"),
                            ExtensionQuality::Sharp => desc.push_str("#9"),
                            _ => {}
                        }
                    }
                }
                if let Some(qual) = self.extensions.eleventh {
                    if qual != ExtensionQuality::Natural {
                        match qual {
                            ExtensionQuality::Flat => desc.push_str("b11"),
                            ExtensionQuality::Sharp => desc.push_str("#11"),
                            _ => {}
                        }
                    }
                }
                if let Some(qual) = self.extensions.thirteenth {
                    if qual != ExtensionQuality::Natural {
                        match qual {
                            ExtensionQuality::Flat => desc.push_str("b13"),
                            ExtensionQuality::Sharp => desc.push_str("#13"),
                            _ => {}
                        }
                    }
                }
        } else {
                // Non-major family or no extensions
                // The highest natural extension masks the seventh
                // If all extensions are altered, the seventh must be shown explicitly
                let should_show_seventh = !self.extensions.has_any() || !self.extensions.has_natural();
                if should_show_seventh {
                    desc.push_str(&family.to_string());
                }
                // Extensions
                if self.extensions.has_any() {
                    desc.push_str(&self.extensions.to_string());
                }
            }
        } else {
            // No family, just show extensions
            if self.extensions.has_any() {
                desc.push_str(&self.extensions.to_string());
            }
        }
        
        // Alterations
        for alteration in &self.alterations {
            desc.push_str(&alteration.to_string());
        }
        
        // Additions (with special handling for 6 and 6/9)
        let is_sixth_chord = self.additions.contains(&ChordDegree::Sixth) && self.family.is_none();
        let is_six_nine_chord = is_sixth_chord && self.additions.contains(&ChordDegree::Ninth);
        
        if is_six_nine_chord {
            desc.push_str("6/9");
            // Add any other additions
            for addition in &self.additions {
                if *addition != ChordDegree::Sixth && *addition != ChordDegree::Ninth {
                    desc.push_str(&format!("add{}", addition));
                }
            }
        } else {
            for addition in &self.additions {
                if *addition == ChordDegree::Sixth && is_sixth_chord {
                    desc.push_str("6");
                } else {
                    desc.push_str(&format!("add{}", addition));
                }
            }
        }
        
        // Omissions
        for omission in &self.omissions {
            desc.push_str(&format!("no{}", omission));
        }
        
        // Bass note
        if let Some(ref bass) = self.bass {
            desc.push_str(&format!("/{}", bass));
        }
        
        self.descriptor = desc.clone();
        self.normalized = format!("{}{}", self.root, desc);
    }
    
    /// Parse a chord from tokens
    /// 
    /// Expected format:
    /// - Root: C, F#, Bb (note name) OR 1, #4, b7 (scale degree) OR I, IV, vi (roman)
    /// - Quality: m, maj, dim, aug, sus4, sus2, 5, etc.
    /// - Duration (optional): _4, ////, r4, etc.
    /// 
    /// Examples:
    /// - "Cmaj" -> C major (no duration)
    /// - "F#m_4" -> F# minor quarter note
    /// - "4////" -> scale degree 4 with 4 slashes
    /// - "IVm" -> roman IV minor
    /// - "Gsus4_2." -> G suspended 4th, dotted half note
    #[instrument(level = "debug", skip(tokens), fields(token_count = tokens.len()))]
    pub fn parse(tokens: &[Token]) -> Result<Self, ParseError> {
        if tokens.is_empty() {
            return Err(ParseError::EmptyInput);
        }
        
        // Step 1: Parse the root (note name, scale degree, or roman numeral)
        trace!("Parsing root from {} tokens", tokens.len());
        let root_result = root::parse_root(tokens)?;
        let mut consumed = root_result.tokens_consumed;
        debug!("Parsed root: {:?}, consumed {} tokens", root_result.root, consumed);
        
        // Step 2: Parse the quality (if present)
        let quality = if consumed < tokens.len() {
            trace!("Parsing quality from remaining tokens");
            Self::parse_quality(&tokens[consumed..], &root_result.root)?
        } else {
            // No quality specified, default to major
            debug!("No quality tokens, defaulting to Major");
            (ChordQuality::Major, 0)
        };
        
        consumed += quality.1;
        debug!("Parsed quality: {:?}, total consumed: {}", quality.0, consumed);
        
        // Step 3: Parse family (seventh type) if present
        let family = if consumed < tokens.len() {
            trace!("Parsing family from remaining tokens");
            match ChordFamily::parse(&tokens[consumed..], quality.0) {
                Ok((fam, tokens_used)) => {
                    consumed += tokens_used;
                    debug!("Parsed family: {:?}, consumed {} additional tokens", fam, tokens_used);
                    fam
                }
                Err(_) => {
                    trace!("No family found");
                    None
                }
            }
            } else {
            None
        };
        
        // Step 4: Parse extensions (9, 11, 13) if present
        let extensions = if consumed < tokens.len() {
            trace!("Parsing extensions from remaining tokens");
            match Extensions::parse(&tokens[consumed..]) {
                Ok((ext, tokens_used)) => {
                    if ext.has_any() {
                        consumed += tokens_used;
                        debug!("Parsed extensions: {:?}, consumed {} additional tokens", ext, tokens_used);
                        ext
        } else {
                        Extensions::none()
                    }
                }
                Err(_) => {
                    trace!("No extensions found");
                    Extensions::none()
                }
            }
        } else {
            Extensions::none()
        };
        
        // If we have extensions but no explicit family, infer the family from quality
        // Extensions (9, 11, 13) imply a seventh chord
        let family = if family.is_none() && extensions.has_any() {
            let inferred = match quality.0 {
                ChordQuality::Minor => Some(ChordFamily::Minor7),
                ChordQuality::Diminished => Some(ChordFamily::HalfDiminished),
                _ => Some(ChordFamily::Dominant7), // Major, Augmented, Suspended
            };
            debug!("Inferred family from extensions: {:?}", inferred);
            inferred
            } else {
            family
        };
        
        // Step 5: Parse alterations (b5, #5, b9, #9, #11, b13) if present
        let alterations = if consumed < tokens.len() {
            trace!("Parsing alterations from remaining tokens");
            match Alteration::parse(&tokens[consumed..]) {
                Ok((alts, tokens_used)) => {
                    if !alts.is_empty() {
                        consumed += tokens_used;
                        debug!("Parsed alterations: {:?}, consumed {} additional tokens", alts, tokens_used);
                        alts
                    } else {
                        Vec::new()
                    }
                }
                Err(_) => {
                    trace!("No alterations found");
                    Vec::new()
                }
            }
        } else {
            Vec::new()
        };
        
        // Step 6: Check for sixth chord (special case - "6" without "add")
        // Sixth chords like C6, Cm6 are additions but displayed without "add"
        // Also handle 6/9 chords (C6/9, Cm6/9)
        let mut is_sixth_chord = false;
        let mut is_six_nine = false;
        if consumed < tokens.len() && family.is_none() {
            if let TokenType::Number(n) = &tokens[consumed].token_type {
                if n == "6" {
                    is_sixth_chord = true;
                    consumed += 1;
                    debug!("Found sixth chord notation at position {}", consumed);
                    
                    // Check for "/9" after the "6"
                    if consumed + 1 < tokens.len() {
                        if let TokenType::Slash = tokens[consumed].token_type {
                            if let TokenType::Number(num) = &tokens[consumed + 1].token_type {
                                if num == "9" {
                                    is_six_nine = true;
                                    consumed += 2; // Skip "/" and "9"
                                    debug!("Found 6/9 chord notation");
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // Step 7: Parse additions (add9, add11) if present
        let mut additions = if consumed < tokens.len() {
            trace!("Parsing additions from remaining tokens");
            Self::parse_additions(&tokens[consumed..])?
        } else {
            (Vec::new(), 0)
        };
        
        // If it's a sixth chord, add the sixth to additions
        if is_sixth_chord {
            additions.0.push(ChordDegree::Sixth);
        }
        
        // If it's a 6/9 chord, also add the ninth to additions
        if is_six_nine {
            additions.0.push(ChordDegree::Ninth);
        }
        
        consumed += additions.1;
        debug!("Parsed additions: {:?}, total consumed: {}", additions.0, consumed);
        
        // Step 8: Parse omissions (no3, no5) if present
        let omissions = if consumed < tokens.len() {
            trace!("Parsing omissions from remaining tokens");
            Self::parse_omissions(&tokens[consumed..])?
        } else {
            (Vec::new(), 0)
        };
        
        consumed += omissions.1;
        debug!("Parsed omissions: {:?}, total consumed: {}", omissions.0, consumed);
        
        // Step 9: Parse slash chord (bass note) if present
        let bass = if consumed < tokens.len() {
            trace!("Parsing slash chord from remaining tokens");
            Self::parse_slash_chord(&tokens[consumed..])?
            } else {
            (None, 0)
        };
        
        consumed += bass.1;
        debug!("Parsed bass: {:?}, total consumed: {}", bass.0, consumed);
        
        // Step 10: Parse the duration (if present)
        let duration = if consumed < tokens.len() {
            trace!("Attempting to parse duration from remaining tokens");
            match ChordRhythm::parse(&tokens[consumed..]) {
                Ok((rhythm, tokens_used)) => {
                    consumed += tokens_used;
                    debug!("Parsed duration: {:?}, consumed {} additional tokens", rhythm, tokens_used);
                    Some(rhythm)
                }
                Err(_) => {
                    trace!("No duration found");
                    None
                }
            }
        } else {
            trace!("No remaining tokens for duration");
            None
        };
        
        debug!("Chord parsing complete: root={:?}, quality={:?}, family={:?}, extensions={:?}, duration={:?}", 
               root_result.root, quality.0, family, extensions, duration);
        
        let mut chord = Self {
            origin: String::new(),  // Will be set from tokens if needed
            descriptor: String::new(),  // Will be computed
            normalized: String::new(),  // Will be computed in normalize()
            root: root_result.root,
            quality: quality.0,
            family,
            extensions,
            alterations: alterations.clone(),
            additions: additions.0,
            omissions: omissions.0,
            bass: bass.0,
            duration,
            intervals: HashMap::new(),
            semantic_degrees: Vec::new(),
            tokens_consumed: consumed,
        };
        
        chord.compute_intervals();
        chord.normalize();
        Ok(chord)
    }
    
    /// Parse chord quality from tokens
    /// Returns (quality, tokens_consumed)
    fn parse_quality(tokens: &[Token], root: &RootNotation) -> Result<(ChordQuality, usize), ParseError> {
        if tokens.is_empty() {
            return Ok((ChordQuality::Major, 0));
        }
        
        // Skip whitespace
        let tokens = Self::skip_whitespace(tokens);
        if tokens.is_empty() {
            return Ok((ChordQuality::Major, 0));
        }
        
        let mut consumed = 0;
        
        // Check for quality indicators
        match &tokens[0].token_type {
            // Check for "min", "maj", or just "m"/"M"
            TokenType::Letter('m') | TokenType::Letter('M') => {
                let is_upper = matches!(tokens[0].token_type, TokenType::Letter('M'));
                
                // Check for "maj7", "maj9", etc. - DON'T consume as quality, let family parser handle it
                if consumed + 2 < tokens.len() {
                    if let TokenType::Letter('a') = tokens[1].token_type {
                        if let TokenType::Letter('j') = tokens[2].token_type {
                            // Look ahead for a number (7, 9, 11, 13)
                            if consumed + 3 < tokens.len() {
                                if let TokenType::Number(_) = tokens[3].token_type {
                                    // It's "maj7", "maj9", etc. - don't consume, return Major quality with 0 tokens
                                    return Ok((ChordQuality::Major, 0));
                                }
                            }
                            // Just "maj" without a number - consume it as quality
                            return Ok((ChordQuality::Major, 3));
                        }
                    } else if !is_upper {
                        // Check for "min7", "min9", etc.
                        if let TokenType::Letter('i') = tokens[1].token_type {
                            if let TokenType::Letter('n') = tokens[2].token_type {
                                // Look ahead for a number
                                if consumed + 3 < tokens.len() {
                                    if let TokenType::Number(_) = tokens[3].token_type {
                                        // It's "min7", "min9", etc. - don't consume, return Minor quality with 0 tokens
                                        return Ok((ChordQuality::Minor, 0));
                                    }
                                }
                                // Just "min" without a number
                                return Ok((ChordQuality::Minor, 3));
                            }
                        }
                    }
                }
                
                // Just "m" or "M"
                // Note: We DO consume "m" even if followed by a number like "m9"
                // The number will be handled by family/extension parsers
                if is_upper {
                    Ok((ChordQuality::Major, 1))
                } else {
                    Ok((ChordQuality::Minor, 1))
                }
            }
            
            TokenType::Minus => {
                Ok((ChordQuality::Minor, 1))
            }
            
            TokenType::Triangle => {
                Ok((ChordQuality::Major, 1))
            }
            
            // Diminished: "dim", "o", "°"
            TokenType::Letter('d') => {
                if consumed + 2 < tokens.len() {
                    if let TokenType::Letter('i') = tokens[1].token_type {
                        if let TokenType::Letter('m') = tokens[2].token_type {
                            return Ok((ChordQuality::Diminished, 3)); // "dim"
                        }
                    }
                }
                Err(ParseError::NoValidParser {
                    context: "Expected 'dim' for diminished".to_string(),
                })
            }
            
            TokenType::Letter('o') | TokenType::Circle => {
                Ok((ChordQuality::Diminished, 1))
            }
            
            // Augmented: "aug", "+"
            TokenType::Letter('a') => {
                if consumed + 2 < tokens.len() {
                    if let TokenType::Letter('u') = tokens[1].token_type {
                        if let TokenType::Letter('g') = tokens[2].token_type {
                            return Ok((ChordQuality::Augmented, 3)); // "aug"
                        }
                    }
                }
                Err(ParseError::NoValidParser {
                    context: "Expected 'aug' for augmented".to_string(),
                })
            }
            
            TokenType::Plus => {
                Ok((ChordQuality::Augmented, 1))
            }
            
            // Suspended: "sus", "sus2", "sus4"
            TokenType::Letter('s') => {
                if consumed + 2 < tokens.len() {
                    if let TokenType::Letter('u') = tokens[1].token_type {
                        if let TokenType::Letter('s') = tokens[2].token_type {
                            consumed = 3; // "sus"
                            
                            // Check for "sus2" or "sus4"
                            if consumed < tokens.len() {
                                match &tokens[consumed].token_type {
                                    TokenType::Number(n) if n == "2" => {
                                        return Ok((ChordQuality::Suspended(SuspendedType::Second), consumed + 1));
                                    }
                                    TokenType::Number(n) if n == "4" => {
                                        return Ok((ChordQuality::Suspended(SuspendedType::Fourth), consumed + 1));
                                    }
                                    _ => {}
                                }
                            }
                            
                            // Just "sus" defaults to sus4
                            return Ok((ChordQuality::Suspended(SuspendedType::Fourth), consumed));
                        }
                    }
                }
                Err(ParseError::NoValidParser {
                    context: "Expected 'sus' for suspended".to_string(),
                })
            }
            
            // Power chord: "5"
            TokenType::Number(n) if n == "5" => {
                Ok((ChordQuality::Power, 1))
            }
            
            // No quality indicator found, infer from root if possible
            _ => {
                // For roman numerals, uppercase = major, lowercase = minor
                if let Some(case) = root.roman_case() {
                    match case {
                        crate::primitives::RomanCase::Upper => Ok((ChordQuality::Major, 0)),
                        crate::primitives::RomanCase::Lower => Ok((ChordQuality::Minor, 0)),
                    }
        } else {
                    // Default to major for other root types
                    Ok((ChordQuality::Major, 0))
                }
            }
        }
    }
    
    /// Skip leading whitespace tokens
    fn skip_whitespace(tokens: &[Token]) -> &[Token] {
        let mut i = 0;
        while i < tokens.len() && tokens[i].token_type == TokenType::Space {
            i += 1;
        }
        &tokens[i..]
    }
    
    /// Parse additions (add9, add11, add13, etc.)
    /// Returns (Vec<ChordDegree>, tokens_consumed)
    fn parse_additions(tokens: &[Token]) -> Result<(Vec<ChordDegree>, usize), ParseError> {
        if tokens.is_empty() {
            return Ok((Vec::new(), 0));
        }
        
        let mut additions = Vec::new();
        let mut consumed = 0;
        
        loop {
            if consumed >= tokens.len() {
                break;
            }
            
            // Check for "add"
            if consumed + 2 < tokens.len() {
                if let TokenType::Letter('a') = tokens[consumed].token_type {
                    if let TokenType::Letter('d') = tokens[consumed + 1].token_type {
                        if let TokenType::Letter('d') = tokens[consumed + 2].token_type {
                            consumed += 3; // "add"
                            
                            // Parse the degree number
                            if consumed < tokens.len() {
                                if let TokenType::Number(n) = &tokens[consumed].token_type {
                                    if let Some(degree) = ChordDegree::from_number(n.parse().ok().unwrap_or(0)) {
                                        additions.push(degree);
                                        consumed += 1;
                                        continue;
                                    }
                                }
                            }
                            // "add" found but no valid number, back up and stop
                            consumed -= 3;
                            break;
                        }
                    }
                }
            }
            
            // No more additions
            break;
        }
        
        Ok((additions, consumed))
    }
    
    /// Parse omissions (no3, no5, omit3, omit5, etc.)
    /// Returns (Vec<ChordDegree>, tokens_consumed)
    fn parse_omissions(tokens: &[Token]) -> Result<(Vec<ChordDegree>, usize), ParseError> {
        if tokens.is_empty() {
            return Ok((Vec::new(), 0));
        }
        
        let mut omissions = Vec::new();
        let mut consumed = 0;
        
        loop {
            if consumed >= tokens.len() {
                break;
            }
            
            // Check for "no" or "omit"
            let keyword_len = if consumed + 1 < tokens.len() {
                if let TokenType::Letter('n') = tokens[consumed].token_type {
                    if let TokenType::Letter('o') = tokens[consumed + 1].token_type {
                        Some(2) // "no"
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            };
            
            let keyword_len = if keyword_len.is_none() && consumed + 3 < tokens.len() {
                if let TokenType::Letter('o') = tokens[consumed].token_type {
                    if let TokenType::Letter('m') = tokens[consumed + 1].token_type {
                        if let TokenType::Letter('i') = tokens[consumed + 2].token_type {
                            if let TokenType::Letter('t') = tokens[consumed + 3].token_type {
                                Some(4) // "omit"
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                keyword_len
            };
            
            if let Some(len) = keyword_len {
                consumed += len;
                
                // Parse the degree number
                if consumed < tokens.len() {
                    if let TokenType::Number(n) = &tokens[consumed].token_type {
                        if let Some(degree) = ChordDegree::from_number(n.parse().ok().unwrap_or(0)) {
                            omissions.push(degree);
                            consumed += 1;
                            continue;
                        }
                    }
                }
                // Keyword found but no valid number, back up and stop
                consumed -= len;
                break;
            }
            
            // No more omissions
            break;
        }
        
        Ok((omissions, consumed))
    }
    
    /// Parse slash chord (bass note): /E, /G, etc.
    /// Returns (Option<RootNotation>, tokens_consumed)
    fn parse_slash_chord(tokens: &[Token]) -> Result<(Option<RootNotation>, usize), ParseError> {
        if tokens.is_empty() {
            return Ok((None, 0));
        }
        
        let mut consumed = 0;
        
        // Check for slash
        if consumed < tokens.len() {
            if let TokenType::Slash = tokens[consumed].token_type {
                consumed += 1;
                
                // Parse the bass note (same as parsing root)
                if consumed < tokens.len() {
                    match root::parse_root(&tokens[consumed..]) {
                        Ok(result) => {
                            consumed += result.tokens_consumed;
                            return Ok((Some(result.root), consumed));
                        }
                        Err(_) => {
                            // Slash found but no valid root, back up
                            return Ok((None, 0));
                        }
                    }
                }
            }
        }
        
        Ok((None, 0))
    }
    
    /// Get the number of tokens consumed during parsing
    pub fn tokens_consumed(&self) -> usize {
        self.tokens_consumed
    }
}

impl std::fmt::Display for Chord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Root
        write!(f, "{}", self.root)?;
        
        // Check if this is a sixth chord (has 6th addition but no seventh family)
        let is_sixth_chord = self.additions.contains(&ChordDegree::Sixth) && self.family.is_none();
        let is_six_nine_chord = is_sixth_chord && self.additions.contains(&ChordDegree::Ninth);
        
        // Quality (uses its own Display which outputs the symbol)
        // For major sixth chords, we need to show "maj" explicitly (Cmaj6, not C6)
        // EXCEPT for 6/9 chords which stay as C6/9 (not Cmaj6/9)
        // For minor sixth chords, "m" is already shown by the quality
        if is_sixth_chord && !is_six_nine_chord && self.quality == ChordQuality::Major {
            write!(f, "maj")?;
        } else {
            write!(f, "{}", self.quality)?;
        }
        
        // Family (seventh type) and Extensions
        // Special handling for Major family with extensions: show "maj" + extension number
        if let Some(family) = &self.family {
            if matches!(family, ChordFamily::Major7 | ChordFamily::MinorMajor7) && self.extensions.has_any() {
                // For maj9, maj11, maj13: show "maj" + highest extension
                write!(f, "maj")?;
                // Show only the highest extension number (e.g., "13" not "9 11 13")
                if self.extensions.thirteenth.is_some() {
                    write!(f, "13")?;
                } else if self.extensions.eleventh.is_some() {
                    write!(f, "11")?;
                } else if self.extensions.ninth.is_some() {
                    write!(f, "9")?;
                }
                // Show altered extensions separately
                if let Some(qual) = self.extensions.ninth {
                    if qual != ExtensionQuality::Natural {
                        match qual {
                            ExtensionQuality::Flat => write!(f, "b9")?,
                            ExtensionQuality::Sharp => write!(f, "#9")?,
                            _ => {}
                        }
                    }
                }
                if let Some(qual) = self.extensions.eleventh {
                    if qual != ExtensionQuality::Natural {
                        match qual {
                            ExtensionQuality::Flat => write!(f, "b11")?,
                            ExtensionQuality::Sharp => write!(f, "#11")?,
                            _ => {}
                        }
                    }
                }
                if let Some(qual) = self.extensions.thirteenth {
                    if qual != ExtensionQuality::Natural {
                        match qual {
                            ExtensionQuality::Flat => write!(f, "b13")?,
                            ExtensionQuality::Sharp => write!(f, "#13")?,
                            _ => {}
                        }
                    }
                }
        } else {
                // Non-major family or no extensions
                // The highest natural extension masks the seventh
                // If all extensions are altered, the seventh must be shown explicitly
                let should_show_seventh = !self.extensions.has_any() || !self.extensions.has_natural();
                if should_show_seventh {
                    write!(f, "{}", family)?;
                }
                // Extensions - show only the highest natural extension number
                if self.extensions.has_any() {
                    // Show highest natural extension number
                    if self.extensions.thirteenth.is_some() {
                        write!(f, "13")?;
                    } else if self.extensions.eleventh.is_some() {
                        write!(f, "11")?;
                    } else if self.extensions.ninth.is_some() {
                        write!(f, "9")?;
                    }
                    // Show altered extensions separately
                    if let Some(qual) = self.extensions.ninth {
                        if qual != ExtensionQuality::Natural {
                            match qual {
                                ExtensionQuality::Flat => write!(f, "b9")?,
                                ExtensionQuality::Sharp => write!(f, "#9")?,
                                _ => {}
                            }
                        }
                    }
                    if let Some(qual) = self.extensions.eleventh {
                        if qual != ExtensionQuality::Natural {
                            match qual {
                                ExtensionQuality::Flat => write!(f, "b11")?,
                                ExtensionQuality::Sharp => write!(f, "#11")?,
                                _ => {}
                            }
                        }
                    }
                    if let Some(qual) = self.extensions.thirteenth {
                        if qual != ExtensionQuality::Natural {
                            match qual {
                                ExtensionQuality::Flat => write!(f, "b13")?,
                                ExtensionQuality::Sharp => write!(f, "#13")?,
                                _ => {}
                            }
                        }
                    }
                }
            }
        } else {
            // No family, just show extensions (with same masking logic)
            if self.extensions.has_any() {
                // Show highest natural extension number
                if self.extensions.thirteenth.is_some() {
                    write!(f, "13")?;
                } else if self.extensions.eleventh.is_some() {
                    write!(f, "11")?;
                } else if self.extensions.ninth.is_some() {
                    write!(f, "9")?;
                }
                // Show altered extensions separately
                if let Some(qual) = self.extensions.ninth {
                    if qual != ExtensionQuality::Natural {
                        match qual {
                            ExtensionQuality::Flat => write!(f, "b9")?,
                            ExtensionQuality::Sharp => write!(f, "#9")?,
                            _ => {}
                        }
                    }
                }
                if let Some(qual) = self.extensions.eleventh {
                    if qual != ExtensionQuality::Natural {
                        match qual {
                            ExtensionQuality::Flat => write!(f, "b11")?,
                            ExtensionQuality::Sharp => write!(f, "#11")?,
                            _ => {}
                        }
                    }
                }
                if let Some(qual) = self.extensions.thirteenth {
                    if qual != ExtensionQuality::Natural {
                        match qual {
                            ExtensionQuality::Flat => write!(f, "b13")?,
                            ExtensionQuality::Sharp => write!(f, "#13")?,
                            _ => {}
                        }
                    }
                }
            }
        }
        
        // Alterations (each uses its own Display)
        for alteration in &self.alterations {
            write!(f, "{}", alteration)?;
        }
        
        // Additions - sixth chords are special (displayed as "6" not "add6")
        // 6/9 chords are even more special (displayed as "6/9" not "6add9")
        let is_sixth_chord = self.additions.contains(&ChordDegree::Sixth) && self.family.is_none();
        let is_six_nine_chord = is_sixth_chord && self.additions.contains(&ChordDegree::Ninth);
        
        // For 6/9 chords, display in specific order: 6 first, then /9
        if is_six_nine_chord {
            write!(f, "6/9")?;
            // Display any other additions
            for addition in &self.additions {
                if *addition != ChordDegree::Sixth && *addition != ChordDegree::Ninth {
                    write!(f, "add{}", addition)?;
                }
            }
        } else {
            // Normal addition display
            for addition in &self.additions {
                if *addition == ChordDegree::Sixth && is_sixth_chord {
                    // Display as "6" for sixth chords (C6, Cm6)
                    write!(f, "6")?;
                } else {
                    // Display with "add" prefix for other additions
                    write!(f, "add{}", addition)?;
                }
            }
        }
        
        // Omissions (each degree uses its own Display)
        for omission in &self.omissions {
            write!(f, "no{}", omission)?;
        }
        
        // Bass note (slash chord - uses RootNotation's Display)
        if let Some(bass) = &self.bass {
            write!(f, "/{}", bass)?;
        }
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsing::Lexer;
    use crate::chord::duration::ChordRhythm;

    #[test]
    fn test_parse_c_major() {
        let mut lexer = Lexer::new("C".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Major);
    }

    #[test]
    fn test_parse_f_sharp_minor() {
        let mut lexer = Lexer::new("F#m".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Minor);
    }

    #[test]
    fn test_parse_scale_degree() {
        let mut lexer = Lexer::new("4".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Major);
    }

    #[test]
    fn test_parse_roman_upper_case() {
        let mut lexer = Lexer::new("IV".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Major);
    }

    #[test]
    fn test_parse_roman_lower_case() {
        let mut lexer = Lexer::new("vi".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Minor);
    }

    #[test]
    fn test_parse_suspended() {
        let mut lexer = Lexer::new("Gsus4".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Suspended(SuspendedType::Fourth));
    }

    #[test]
    fn test_parse_sus2() {
        let mut lexer = Lexer::new("Dsus2".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Suspended(SuspendedType::Second));
    }

    #[test]
    fn test_parse_diminished() {
        let mut lexer = Lexer::new("Bdim".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Diminished);
    }

    #[test]
    fn test_parse_augmented() {
        let mut lexer = Lexer::new("Caug".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Augmented);
    }

    #[test]
    fn test_parse_power_chord() {
        let mut lexer = Lexer::new("E5".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Power);
    }

    #[test]
    fn test_display() {
        let mut lexer = Lexer::new("F#m".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let display = format!("{}", chord);
        assert!(display.contains("F#"));
        assert!(display.contains("m"));
    }

    #[test]
    fn test_parse_chord_with_lily_duration() {
        use crate::chord::duration::LilySyntax;
        
        let mut lexer = Lexer::new("C_4".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Major);
        assert!(chord.duration.is_some());
        if let Some(ChordRhythm::Lily { duration, .. }) = chord.duration {
            assert_eq!(duration, LilySyntax::Quarter);
        } else {
            panic!("Expected Lily duration");
        }
    }

    #[test]
    fn test_parse_chord_with_slashes() {
        let mut lexer = Lexer::new("Dm////".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Minor);
        assert!(chord.duration.is_some());
        if let Some(ChordRhythm::Slashes(count)) = chord.duration {
            assert_eq!(count, 4);
        } else {
            panic!("Expected Slashes duration");
        }
    }

    #[test]
    fn test_parse_chord_without_duration() {
        let mut lexer = Lexer::new("Gmaj".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Major);
        assert!(chord.duration.is_none());
    }

    #[test]
    fn test_parse_scale_degree_with_duration() {
        use crate::chord::duration::LilySyntax;
        
        let mut lexer = Lexer::new("4_8.".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Major);
        assert!(chord.duration.is_some());
        if let Some(ChordRhythm::Lily { duration, dotted, .. }) = chord.duration {
            assert_eq!(duration, LilySyntax::Eighth);
            assert!(dotted);
        } else {
            panic!("Expected dotted Lily duration");
        }
    }
    
    // === Chord System Tests ===

    #[test]
    fn test_simple_major_triad() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::new(root, ChordQuality::Major);
        
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, None);
        assert!(!chord.extensions.has_any());
        
        // Check intervals
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::Unison));
        assert!(intervals.contains(&Interval::MajorThird));
        assert!(intervals.contains(&Interval::PerfectFifth));
        assert_eq!(intervals.len(), 3);
        
        // Check semantic degrees
        let degrees = chord.semantic_degrees();
        assert!(degrees.contains(&ChordDegree::Root));
        assert!(degrees.contains(&ChordDegree::Third));
        assert!(degrees.contains(&ChordDegree::Fifth));
        assert_eq!(degrees.len(), 3);
    }

    #[test]
    fn test_minor_triad() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::new(root, ChordQuality::Minor);
        
        assert_eq!(chord.quality, ChordQuality::Minor);
        
        // Check intervals
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::MinorThird));
        assert!(intervals.contains(&Interval::PerfectFifth));
    }

    #[test]
    fn test_major_seventh_chord() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Major7);
        
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, Some(ChordFamily::Major7));
        
        // Check intervals
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::Unison));
        assert!(intervals.contains(&Interval::MajorThird));
        assert!(intervals.contains(&Interval::PerfectFifth));
        assert!(intervals.contains(&Interval::MajorSeventh));
        assert_eq!(intervals.len(), 4);
        
        // Check semantic degrees
        let degrees = chord.semantic_degrees();
        assert!(degrees.contains(&ChordDegree::Seventh));
        assert_eq!(degrees.len(), 4);
    }

    #[test]
    fn test_dominant_seventh_chord() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Dominant7);
        
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, Some(ChordFamily::Dominant7));
        
        // Check intervals - should have minor 7th
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::MinorSeventh));
        assert!(!intervals.contains(&Interval::MajorSeventh));
    }

    #[test]
    fn test_minor_seventh_chord() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::with_family(root, ChordQuality::Minor, ChordFamily::Minor7);
        
        assert_eq!(chord.quality, ChordQuality::Minor);
        assert_eq!(chord.family, Some(ChordFamily::Minor7));
        
        // Check intervals
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::MinorThird));
        assert!(intervals.contains(&Interval::MinorSeventh));
    }

    #[test]
    fn test_minor_major_seventh() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::with_family(root, ChordQuality::Minor, ChordFamily::MinorMajor7);
        
        assert_eq!(chord.quality, ChordQuality::Minor);
        assert_eq!(chord.family, Some(ChordFamily::MinorMajor7));
        
        // Check intervals - minor 3rd with major 7th
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::MinorThird));
        assert!(intervals.contains(&Interval::MajorSeventh));
    }

    #[test]
    fn test_half_diminished_seventh() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::with_family(root, ChordQuality::Diminished, ChordFamily::HalfDiminished);
        
        assert_eq!(chord.quality, ChordQuality::Diminished);
        assert_eq!(chord.family, Some(ChordFamily::HalfDiminished));
        
        // Check intervals
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::MinorThird));
        assert!(intervals.contains(&Interval::DiminishedFifth));
        assert!(intervals.contains(&Interval::MinorSeventh));
    }

    #[test]
    fn test_fully_diminished_seventh() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::with_family(root, ChordQuality::Diminished, ChordFamily::FullyDiminished);
        
        assert_eq!(chord.quality, ChordQuality::Diminished);
        assert_eq!(chord.family, Some(ChordFamily::FullyDiminished));
        
        // Check intervals
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::DiminishedSeventh));
    }

    #[test]
    fn test_chord_with_extensions() {
        use crate::primitives::MusicalNote;
        use crate::chord::extensions::{Extensions, ExtensionQuality};
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Dominant7);
        
        // Add ninth extension
        let extensions = Extensions::with_ninth(ExtensionQuality::Natural);
        chord.set_extensions(extensions);
        
        // Check intervals
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::Ninth));
        
        // Check semantic degrees
        let degrees = chord.semantic_degrees();
        assert!(degrees.contains(&ChordDegree::Ninth));
    }

    #[test]
    fn test_chord_with_flat_ninth() {
        use crate::primitives::MusicalNote;
        use crate::chord::extensions::{Extensions, ExtensionQuality};
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Dominant7);
        
        // Add flat ninth extension
        let extensions = Extensions::with_ninth(ExtensionQuality::Flat);
        chord.set_extensions(extensions);
        
        // Check intervals
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::FlatNinth));
    }

    #[test]
    fn test_chord_with_sharp_ninth() {
        use crate::primitives::MusicalNote;
        use crate::chord::extensions::{Extensions, ExtensionQuality};
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Dominant7);
        
        // Add sharp ninth extension
        let extensions = Extensions::with_ninth(ExtensionQuality::Sharp);
        chord.set_extensions(extensions);
        
        // Check intervals
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::SharpNinth));
    }

    #[test]
    fn test_chord_with_thirteenth() {
        use crate::primitives::MusicalNote;
        use crate::chord::extensions::{Extensions, ExtensionQuality};
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Dominant7);
        
        // Add 13th extension (implies 9th and 11th)
        let extensions = Extensions::with_thirteenth(
            ExtensionQuality::Natural,
            ExtensionQuality::Natural,
            ExtensionQuality::Natural,
        );
        chord.set_extensions(extensions);
        
        // Check intervals
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::Ninth));
        assert!(intervals.contains(&Interval::Eleventh));
        assert!(intervals.contains(&Interval::Thirteenth));
        
        // Check semantic degrees
        let degrees = chord.semantic_degrees();
        assert!(degrees.contains(&ChordDegree::Ninth));
        assert!(degrees.contains(&ChordDegree::Eleventh));
        assert!(degrees.contains(&ChordDegree::Thirteenth));
    }

    #[test]
    fn test_chord_with_alteration_flat_five() {
        use crate::primitives::MusicalNote;
        use crate::chord::alteration::Alteration;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Dominant7);
        
        // Add b5 alteration
        let result = chord.add_alteration(Alteration::flat_five());
        assert!(result.is_ok());
        
        // Check intervals - should have diminished fifth instead of perfect fifth
        assert_eq!(
            chord.interval_for_degree(ChordDegree::Fifth),
            Some(Interval::DiminishedFifth)
        );
    }

    #[test]
    fn test_chord_with_alteration_sharp_five() {
        use crate::primitives::MusicalNote;
        use crate::chord::alteration::Alteration;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Dominant7);
        
        // Add #5 alteration
        let result = chord.add_alteration(Alteration::sharp_five());
        assert!(result.is_ok());
        
        // Check intervals
        assert_eq!(
            chord.interval_for_degree(ChordDegree::Fifth),
            Some(Interval::AugmentedFifth)
        );
    }

    #[test]
    fn test_chord_with_additions() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::new(root, ChordQuality::Major);
        
        // Add ninth without implying seventh
        chord.add_addition(ChordDegree::Ninth);
        
        // Check that 9th is present but 7th is not
        assert!(chord.has_degree(ChordDegree::Ninth));
        assert!(!chord.has_degree(ChordDegree::Seventh));
    }

    #[test]
    fn test_chord_with_omissions() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::new(root, ChordQuality::Major);
        
        // Omit the third (power chord style)
        chord.add_omission(ChordDegree::Third);
        
        // Check that third is not present
        assert!(!chord.has_degree(ChordDegree::Third));
        assert!(chord.has_degree(ChordDegree::Root));
        assert!(chord.has_degree(ChordDegree::Fifth));
    }

    #[test]
    fn test_chord_with_bass_note() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::new(root, ChordQuality::Major);
        
        // Add bass note (slash chord)
        let bass = RootNotation::from_note_name(MusicalNote::g());
        chord.set_bass(bass);
        
        assert!(chord.bass.is_some());
    }

    #[test]
    fn test_complex_chord() {
        use crate::primitives::MusicalNote;
        use crate::chord::extensions::{Extensions, ExtensionQuality};
        
        // Build a C13(#11) chord
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Dominant7);
        
        // Add extensions
        let extensions = Extensions::with_thirteenth(
            ExtensionQuality::Natural,  // 9
            ExtensionQuality::Sharp,     // #11
            ExtensionQuality::Natural,   // 13
        );
        chord.set_extensions(extensions);
        
        // Verify all degrees are present
        assert!(chord.has_degree(ChordDegree::Root));
        assert!(chord.has_degree(ChordDegree::Third));
        assert!(chord.has_degree(ChordDegree::Fifth));
        assert!(chord.has_degree(ChordDegree::Seventh));
        assert!(chord.has_degree(ChordDegree::Ninth));
        assert!(chord.has_degree(ChordDegree::Eleventh));
        assert!(chord.has_degree(ChordDegree::Thirteenth));
        
        // Verify #11
        assert_eq!(
            chord.interval_for_degree(ChordDegree::Eleventh),
            Some(Interval::SharpEleventh)
        );
    }

    #[test]
    fn test_altered_dominant() {
        use crate::primitives::MusicalNote;
        use crate::chord::extensions::{Extensions, ExtensionQuality};
        use crate::chord::alteration::Alteration;
        
        // Build a C7(b9,#5,b13) - altered dominant
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Dominant7);
        
        let extensions = Extensions::with_thirteenth(
            ExtensionQuality::Flat,   // b9
            ExtensionQuality::Sharp,  // #11
            ExtensionQuality::Flat,   // b13
        );
        chord.set_extensions(extensions);
        
        // Add #5 alteration
        chord.add_alteration(Alteration::sharp_five()).unwrap();
        
        // Verify alterations
        assert_eq!(
            chord.interval_for_degree(ChordDegree::Ninth),
            Some(Interval::FlatNinth)
        );
        assert_eq!(
            chord.interval_for_degree(ChordDegree::Fifth),
            Some(Interval::AugmentedFifth)
        );
        assert_eq!(
            chord.interval_for_degree(ChordDegree::Thirteenth),
            Some(Interval::FlatThirteenth)
        );
    }

    #[test]
    fn test_display_dominant_seventh() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Dominant7);
        
        let display = format!("{}", chord);
        assert!(display.contains("C"));
        assert!(display.contains("7"));
    }

    #[test]
    fn test_display_major_seventh() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Major7);
        
        let display = format!("{}", chord);
        assert!(display.contains("C"));
        assert!(display.contains("maj7"));
    }

    #[test]
    fn test_suspended_chords_intervals() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::new(root, ChordQuality::sus4());
        
        // Sus4 should have 4th instead of 3rd
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::PerfectFourth));
        assert!(!intervals.contains(&Interval::MajorThird));
        assert!(!intervals.contains(&Interval::MinorThird));
    }

    #[test]
    fn test_power_chord_intervals() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::new(root, ChordQuality::Power);
        
        // Power chord should only have root and fifth
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::Unison));
        assert!(intervals.contains(&Interval::PerfectFifth));
        assert!(!intervals.contains(&Interval::MajorThird));
        assert!(!intervals.contains(&Interval::MinorThird));
    }

    #[test]
    fn test_augmented_triad_intervals() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::new(root, ChordQuality::Augmented);
        
        // Augmented should have augmented fifth
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::AugmentedFifth));
        assert!(!intervals.contains(&Interval::PerfectFifth));
    }

    #[test]
    fn test_diminished_triad_intervals() {
        use crate::primitives::MusicalNote;
        
        let root = RootNotation::from_note_name(MusicalNote::c());
        let chord = Chord::new(root, ChordQuality::Diminished);
        
        // Diminished should have diminished fifth
        let intervals = chord.intervals();
        assert!(intervals.contains(&Interval::DiminishedFifth));
        assert!(!intervals.contains(&Interval::PerfectFifth));
    }

    #[test]
    fn test_display_uses_component_displays() {
        use crate::primitives::MusicalNote;
        use crate::chord::extensions::{Extensions, ExtensionQuality};
        use crate::chord::alteration::Alteration;
        
        // Build a complex chord: C7#5b9
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Dominant7);
        
        // Add extensions
        let extensions = Extensions::with_ninth(ExtensionQuality::Flat);
        chord.set_extensions(extensions);
        
        // Add alteration
        chord.add_alteration(Alteration::sharp_five()).unwrap();
        
        // Display should use all component Display implementations
        let display = format!("{}", chord);
        
        // Should contain: C (root), nothing (major quality has empty symbol), 
        // 7 (dominant family), b9 (extension with quality), #5 (alteration)
        assert!(display.starts_with("C"));
        assert!(display.contains("7"));
        assert!(display.contains("b9"));
        assert!(display.contains("#5"));
    }

    #[test]
    fn test_display_with_additions_and_omissions() {
        use crate::primitives::MusicalNote;
        
        // Build: Cadd9no5
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::new(root, ChordQuality::Major);
        
        chord.add_addition(ChordDegree::Ninth);
        chord.add_omission(ChordDegree::Fifth);
        
        let display = format!("{}", chord);
        assert!(display.contains("C"));
        assert!(display.contains("add9"));
        assert!(display.contains("no5"));
    }

    #[test]
    fn test_display_with_slash_chord() {
        use crate::primitives::MusicalNote;
        
        // Build: C/E
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::new(root, ChordQuality::Major);
        
        let bass = RootNotation::from_note_name(MusicalNote::e());
        chord.set_bass(bass);
        
        let display = format!("{}", chord);
        assert!(display.starts_with("C"));
        assert!(display.contains("/E"));
    }
    
    #[test]
    fn test_parse_sixth_chord() {
        let mut lexer = Lexer::new("C6".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, None);
        assert!(chord.has_degree(ChordDegree::Sixth));
        
        // Display should show "Cmaj6" (major sixth chord)
        let display = format!("{}", chord);
        assert_eq!(display, "Cmaj6");
        
        // Normalized should also be "Cmaj6"
        assert_eq!(chord.normalized, "Cmaj6");
        assert_eq!(chord.descriptor, "maj6");
    }

    #[test]
    fn test_parse_minor_sixth_chord() {
        let mut lexer = Lexer::new("Cm6".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Minor);
        assert_eq!(chord.family, None);
        assert!(chord.has_degree(ChordDegree::Sixth));
        
        // Display should show "Cm6" not "Cmadd6"
        let display = format!("{}", chord);
        assert_eq!(display, "Cm6");
    }

    #[test]
    fn test_parse_sixth_ninth_chord() {
        // Parse using "6/9" notation
        let mut lexer = Lexer::new("C6/9".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, None);
        assert!(chord.has_degree(ChordDegree::Sixth));
        assert!(chord.has_degree(ChordDegree::Ninth));
        
        // Display should show "C6/9"
        let display = format!("{}", chord);
        assert_eq!(display, "C6/9");
    }

    #[test]
    fn test_parse_sixth_with_add_ninth() {
        // Parse using "6add9" notation  
        let mut lexer = Lexer::new("C6add9".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, None);
        assert!(chord.has_degree(ChordDegree::Sixth));
        assert!(chord.has_degree(ChordDegree::Ninth));
        
        // Display should normalize to "C6/9"
        let display = format!("{}", chord);
        assert_eq!(display, "C6/9");
    }
    
    #[test]
    fn test_parse_minor_sixth_ninth_chord() {
        let mut lexer = Lexer::new("Cm6/9".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.quality, ChordQuality::Minor);
        assert_eq!(chord.family, None);
        assert!(chord.has_degree(ChordDegree::Sixth));
        assert!(chord.has_degree(ChordDegree::Ninth));
        
        // Display should show "Cm6/9"
        let display = format!("{}", chord);
        assert_eq!(display, "Cm6/9");
    }

    #[test]
    fn test_sixth_with_seventh_uses_add() {
        use crate::primitives::MusicalNote;
        
        // C7 with added 6th should display as "C7add6" not "C76"
        let root = RootNotation::from_note_name(MusicalNote::c());
        let mut chord = Chord::with_family(root, ChordQuality::Major, ChordFamily::Dominant7);
        chord.add_addition(ChordDegree::Sixth);
        
        let display = format!("{}", chord);
        assert_eq!(display, "C7add6");
    }
    
    #[test]
    fn test_normalization_basic() {
        let mut lexer = Lexer::new("Cmaj7".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        println!("Quality: {:?}", chord.quality);
        println!("Family: {:?}", chord.family);
        println!("Normalized: {}", chord.normalized);
        println!("Descriptor: {}", chord.descriptor);
        
        assert_eq!(chord.normalized, "Cmaj7");
        assert_eq!(chord.descriptor, "maj7");
    }

    #[test]
    fn test_normalization_with_extensions() {
        let mut lexer = Lexer::new("Dm9".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.normalized, "Dm9");
        assert_eq!(chord.descriptor, "m9");
    }

    #[test]
    fn test_normalization_with_alterations() {
        let mut lexer = Lexer::new("G7#9b13".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        // Note: #9 and b13 are extensions (not alterations)
        // All extensions are altered (no natural extensions)
        // When all extensions are altered, the seventh must be shown explicitly
        // G7#9b13 stays as G7#9b13
        assert_eq!(chord.normalized, "G7#9b13");
        assert!(chord.normalized.contains("7"));
        assert!(chord.normalized.contains("#9"));
        assert!(chord.normalized.contains("b13"));
    }

    #[test]
    fn test_normalization_slash_chord() {
        let mut lexer = Lexer::new("C/E".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        assert_eq!(chord.normalized, "C/E");
        assert_eq!(chord.descriptor, "/E");
    }

    #[test]
    fn test_normalization_complex() {
        let mut lexer = Lexer::new("Dm7b5add11/F".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        // Check that all components are present in normalized form
        assert!(chord.normalized.contains("Dm"));
        assert!(chord.normalized.contains("7"));
        assert!(chord.normalized.contains("b5"));
        assert!(chord.normalized.contains("add11"));
        assert!(chord.normalized.contains("/F"));
    }

    // Tests for new methods: semitone_sequence, root_note, notes, scale_degrees

    #[test]
    fn test_semitone_sequence_major_triad() {
        let mut lexer = Lexer::new("C".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let semitones = chord.semitone_sequence();
        // C major triad: C (0), E (4), G (7)
        assert_eq!(semitones, vec![0, 4, 7]);
    }

    #[test]
    fn test_semitone_sequence_minor_seventh() {
        let mut lexer = Lexer::new("Dm7".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let semitones = chord.semitone_sequence();
        // D minor 7: D (0), F (3), A (7), C (10)
        assert_eq!(semitones, vec![0, 3, 7, 10]);
    }

    #[test]
    fn test_semitone_sequence_major_seventh() {
        let mut lexer = Lexer::new("Cmaj7".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let semitones = chord.semitone_sequence();
        // C major 7: C (0), E (4), G (7), B (11)
        assert_eq!(semitones, vec![0, 4, 7, 11]);
    }

    #[test]
    fn test_semitone_sequence_dominant_ninth() {
        let mut lexer = Lexer::new("G9".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let semitones = chord.semitone_sequence();
        // G9: G (0), B (4), D (7), F (10), A (14 - in second octave)
        assert_eq!(semitones, vec![0, 4, 7, 10, 14]);
        
        // Pitch classes should wrap to first octave
        let pitch_classes = chord.pitch_classes();
        assert_eq!(pitch_classes, vec![0, 2, 4, 7, 10]);
    }

    #[test]
    fn test_semitone_sequence_altered_chord() {
        let mut lexer = Lexer::new("C7#5".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let semitones = chord.semitone_sequence();
        // C7#5: C (0), E (4), G# (8), Bb (10)
        assert_eq!(semitones, vec![0, 4, 8, 10]);
    }

    #[test]
    fn test_semitone_sequence_sixth_chord() {
        let mut lexer = Lexer::new("C6".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let semitones = chord.semitone_sequence();
        // C6: C (0), E (4), G (7), A (9)
        assert_eq!(semitones, vec![0, 4, 7, 9]);
    }

    #[test]
    fn test_semitone_sequence_thirteenth_chord() {
        let mut lexer = Lexer::new("C13".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let semitones = chord.semitone_sequence();
        // C13: C (0), E (4), G (7), Bb (10), D (14), F (17), A (21)
        // All extensions are in second octave
        assert_eq!(semitones, vec![0, 4, 7, 10, 14, 17, 21]);
    }

    #[test]
    fn test_pitch_classes_vs_semitone_sequence() {
        let mut lexer = Lexer::new("Dm11".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        // Semitone sequence preserves octave information
        let semitones = chord.semitone_sequence();
        // Dm11: D (0), F (3), A (7), C (10), E (14), G (17)
        assert_eq!(semitones, vec![0, 3, 7, 10, 14, 17]);
        
        // Pitch classes reduces to one octave
        let pitch_classes = chord.pitch_classes();
        // D (0), E (2), F (3), G (5), A (7), C (10)
        assert_eq!(pitch_classes, vec![0, 2, 3, 5, 7, 10]);
    }

    #[test]
    fn test_root_note_from_note_name() {
        let mut lexer = Lexer::new("C".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let root = chord.root_note(None).expect("Should resolve without key");
        assert_eq!(root.name, "C");
        assert_eq!(root.semitone, 0);
    }

    #[test]
    fn test_root_note_from_note_name_sharp() {
        let mut lexer = Lexer::new("F#".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let root = chord.root_note(None).expect("Should resolve without key");
        assert_eq!(root.name, "F#");
        assert_eq!(root.semitone, 6);
    }

    #[test]
    fn test_root_note_from_note_name_flat() {
        let mut lexer = Lexer::new("Bb".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let root = chord.root_note(None).expect("Should resolve without key");
        assert_eq!(root.name, "Bb");
        assert_eq!(root.semitone, 10);
    }

    #[test]
    fn test_notes_major_triad() {
        let mut lexer = Lexer::new("C".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let notes = chord.notes(None).expect("Should resolve");
        assert_eq!(notes.len(), 3);
        assert_eq!(notes[0].name, "C");
        assert_eq!(notes[1].name, "E");
        assert_eq!(notes[2].name, "G");
    }

    #[test]
    fn test_notes_minor_seventh() {
        let mut lexer = Lexer::new("Dm7".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let notes = chord.notes(None).expect("Should resolve");
        assert_eq!(notes.len(), 4);
        assert_eq!(notes[0].name, "D");
        assert_eq!(notes[1].name, "F");
        assert_eq!(notes[2].name, "A");
        assert_eq!(notes[3].name, "C");
    }

    #[test]
    fn test_notes_major_seventh() {
        let mut lexer = Lexer::new("Cmaj7".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let notes = chord.notes(None).expect("Should resolve");
        assert_eq!(notes.len(), 4);
        assert_eq!(notes[0].name, "C");
        assert_eq!(notes[1].name, "E");
        assert_eq!(notes[2].name, "G");
        assert_eq!(notes[3].name, "B");
    }

    #[test]
    fn test_notes_with_sharp_root() {
        let mut lexer = Lexer::new("F#m".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let notes = chord.notes(None).expect("Should resolve");
        assert_eq!(notes.len(), 3);
        assert_eq!(notes[0].name, "F#");
        // F#m: F# (6), A (9), C# (1)
        assert_eq!(notes[1].name, "A");
        assert_eq!(notes[2].name, "C#");
    }

    #[test]
    fn test_notes_with_flat_root() {
        let mut lexer = Lexer::new("Bbmaj7".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let notes = chord.notes(None).expect("Should resolve");
        assert_eq!(notes.len(), 4);
        assert_eq!(notes[0].name, "Bb");
        assert_eq!(notes[1].name, "D");
        assert_eq!(notes[2].name, "F");
        assert_eq!(notes[3].name, "A");
    }

    #[test]
    fn test_notes_dominant_ninth() {
        let mut lexer = Lexer::new("G9".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let notes = chord.notes(None).expect("Should resolve");
        assert_eq!(notes.len(), 5);
        // G9 intervals: G (root=0), B (maj3=4), D (5th=7), F (min7=10), A (9th=14)
        // Sorted by semitone distance (preserving octaves): G, B, D, F, A
        assert_eq!(notes[0].name, "G");
        assert_eq!(notes[1].name, "B");
        assert_eq!(notes[2].name, "D");
        assert_eq!(notes[3].name, "F");
        assert_eq!(notes[4].name, "A");
    }

    #[test]
    fn test_scale_degrees_c_major_in_c_major() {
        let mut lexer = Lexer::new("C".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let key = Key::major(MusicalNote::c());
        let degrees = chord.scale_degrees(&key).expect("Should resolve");
        
        // C major triad in C major: C=1, E=3, G=5
        assert_eq!(degrees.len(), 3);
        assert_eq!(degrees[0].1, 1); // Root is scale degree 1
        assert_eq!(degrees[1].1, 3); // Third is scale degree 3
        assert_eq!(degrees[2].1, 5); // Fifth is scale degree 5
    }

    #[test]
    fn test_scale_degrees_dm7_in_c_major() {
        let mut lexer = Lexer::new("Dm7".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let key = Key::major(MusicalNote::c());
        let degrees = chord.scale_degrees(&key).expect("Should resolve");
        
        // Dm7 in C major: D=2, F=4, A=6, C=1
        assert_eq!(degrees.len(), 4);
        assert_eq!(degrees[0].1, 2); // D is scale degree 2
        assert_eq!(degrees[1].1, 4); // F is scale degree 4
        assert_eq!(degrees[2].1, 6); // A is scale degree 6
        assert_eq!(degrees[3].1, 1); // C is scale degree 1
    }

    #[test]
    fn test_scale_degrees_g7_in_c_major() {
        let mut lexer = Lexer::new("G7".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let key = Key::major(MusicalNote::c());
        let degrees = chord.scale_degrees(&key).expect("Should resolve");
        
        // G7 in C major: G=5, B=7, D=2, F=4
        assert_eq!(degrees.len(), 4);
        assert_eq!(degrees[0].1, 5); // G is scale degree 5
        assert_eq!(degrees[1].1, 7); // B is scale degree 7
        assert_eq!(degrees[2].1, 2); // D is scale degree 2
        assert_eq!(degrees[3].1, 4); // F is scale degree 4
    }

    #[test]
    fn test_scale_degrees_shows_chord_degree() {
        let mut lexer = Lexer::new("Cmaj7".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let key = Key::major(MusicalNote::c());
        let degrees = chord.scale_degrees(&key).expect("Should resolve");
        
        // Verify we get ChordDegree values
        assert_eq!(degrees.len(), 4);
        assert_eq!(degrees[0].0, ChordDegree::Root);
        assert_eq!(degrees[1].0, ChordDegree::Third);
        assert_eq!(degrees[2].0, ChordDegree::Fifth);
        assert_eq!(degrees[3].0, ChordDegree::Seventh);
    }

    // Transpose tests

    #[test]
    fn test_transpose_major_triad() {
        let mut lexer = Lexer::new("C".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        // Transpose C to G (same scale type)
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::major(MusicalNote::g());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        assert_eq!(transposed.quality, ChordQuality::Major);
        assert_eq!(transposed.root_note(None).unwrap().name, "G");
        assert_eq!(transposed.to_string(), "G");
    }

    #[test]
    fn test_transpose_minor_seventh() {
        let mut lexer = Lexer::new("Dm7".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        // Transpose Dm7 to Am7 (same scale type - major)
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::major(MusicalNote::a());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        assert_eq!(transposed.quality, ChordQuality::Minor);
        assert_eq!(transposed.family, Some(ChordFamily::Minor7));
        // Note: root will be A in the target key
        assert_eq!(transposed.to_string(), "Am7");
    }

    #[test]
    fn test_transpose_major_seventh() {
        let mut lexer = Lexer::new("Cmaj7".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        // Transpose Cmaj7 to F#maj7 (same scale type)
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::major(MusicalNote::from_string("F#").unwrap());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        assert_eq!(transposed.quality, ChordQuality::Major);
        assert_eq!(transposed.family, Some(ChordFamily::Major7));
        assert_eq!(transposed.root_note(None).unwrap().name, "F#");
        assert_eq!(transposed.to_string(), "F#maj7");
    }

    #[test]
    fn test_transpose_dominant_ninth() {
        let mut lexer = Lexer::new("G9".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        // Transpose G9 to D9 (same scale type)
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::major(MusicalNote::d());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        assert_eq!(transposed.quality, ChordQuality::Major);
        assert_eq!(transposed.family, Some(ChordFamily::Dominant7));
        assert!(transposed.extensions.has_any());
        assert_eq!(transposed.root_note(None).unwrap().name, "D");
        assert_eq!(transposed.to_string(), "D9");
    }

    #[test]
    fn test_transpose_altered_chord() {
        let mut lexer = Lexer::new("C7#5".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        // Transpose C7#5 to Bb7#5 (same scale type)
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::major(MusicalNote::from_string("Bb").unwrap());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        assert_eq!(transposed.quality, ChordQuality::Major);
        assert_eq!(transposed.family, Some(ChordFamily::Dominant7));
        assert_eq!(transposed.alterations.len(), 1);
        assert_eq!(transposed.root_note(None).unwrap().name, "Bb");
        assert_eq!(transposed.to_string(), "Bb7#5");
    }

    #[test]
    fn test_transpose_with_additions() {
        // Use a chord we know parses correctly
        let mut lexer = Lexer::new("Cno5".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        // Transpose Cno5 to Eno5 (same scale type)
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::major(MusicalNote::e());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        assert_eq!(transposed.quality, ChordQuality::Major);
        assert_eq!(transposed.omissions.len(), 1);
        assert!(transposed.omissions.contains(&ChordDegree::Fifth));
        assert_eq!(transposed.root_note(None).unwrap().name, "E");
        assert_eq!(transposed.to_string(), "Eno5");
    }

    #[test]
    fn test_transpose_sixth_chord() {
        let mut lexer = Lexer::new("C6".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        // Transpose C6 to A6 (same scale type)
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::major(MusicalNote::a());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        assert_eq!(transposed.quality, ChordQuality::Major);
        assert!(transposed.additions.contains(&ChordDegree::Sixth));
        assert_eq!(transposed.root_note(None).unwrap().name, "A");
        assert_eq!(transposed.to_string(), "Amaj6"); // Major sixth chords display as "maj6"
    }

    #[test]
    fn test_transpose_complex_chord() {
        let mut lexer = Lexer::new("Cmaj13".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        // Transpose Cmaj13 to Ebmaj13 (same scale type)
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::major(MusicalNote::from_string("Eb").unwrap());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        assert_eq!(transposed.quality, ChordQuality::Major);
        assert_eq!(transposed.family, Some(ChordFamily::Major7));
        assert!(transposed.extensions.has_any());
        assert_eq!(transposed.root_note(None).unwrap().name, "Eb");
        // Verify the structure is preserved
        assert_eq!(format!("{}", transposed), "Ebmaj13");
    }

    #[test]
    fn test_transpose_enharmonic_spelling() {
        let mut lexer = Lexer::new("C".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        // Transpose C to Db (same scale type)
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::major(MusicalNote::from_string("Db").unwrap());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        // Check that notes use correct enharmonic spelling for Db major
        let notes = transposed.notes(None).expect("Should resolve");
        assert_eq!(notes[0].name, "Db");
        assert_eq!(notes[1].name, "F");
        assert_eq!(notes[2].name, "Ab");
    }
    
    // New tests for scale type changes
    
    #[test]
    fn test_transpose_scale_type_c_major_to_c_minor() {
        // C major triad in C Major → Cm triad in C Minor
        let mut lexer = Lexer::new("C".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::minor(MusicalNote::c());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        assert_eq!(transposed.quality, ChordQuality::Minor);
        assert_eq!(transposed.root_note(None).unwrap().name, "C");
        assert_eq!(transposed.to_string(), "Cm");
    }

    #[test]
    fn test_transpose_scale_type_em_to_ebmaj() {
        // Em in C Major (iii chord) → Eb in C Minor (iii chord)
        let mut lexer = Lexer::new("Em".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::minor(MusicalNote::c());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        // In C minor, the iii chord is Eb major (Eb-G-Bb)
        assert_eq!(transposed.root_note(None).unwrap().name, "Eb");
        assert_eq!(transposed.quality, ChordQuality::Major);
    }
    
    #[test]
    fn test_transpose_to_harmonic_minor() {
        use crate::key::ScaleMode;
        
        // C major in C Major → C harmonic minor
        let mut lexer = Lexer::new("C".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::new(MusicalNote::c(), ScaleMode::harmonic_minor());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        // C harmonic minor has a minor third, so C → Cm
        assert_eq!(transposed.root_note(None).unwrap().name, "C");
        assert_eq!(transposed.quality, ChordQuality::Minor);
        assert_eq!(transposed.to_string(), "Cm");
    }
    
    #[test]
    fn test_transpose_both_root_and_scale_type() {
        // C major in C Major → Gm in G Minor
        let mut lexer = Lexer::new("C".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::minor(MusicalNote::g());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        assert_eq!(transposed.quality, ChordQuality::Minor);
        assert_eq!(transposed.root_note(None).unwrap().name, "G");
        assert_eq!(transposed.to_string(), "Gm");
    }
    
    #[test]
    fn test_transpose_complex_chord_with_scale_type_change() {
        use crate::key::ScaleMode;
        
        // Cmaj13 in C Major → Cm(maj13) in C Harmonic Minor
        // C Major scale: C D E F G A B
        // Cmaj13: 1=C, 3=E, 5=G, 7=B, 9=D, 11=F, 13=A
        // Notes: C E G B D F A
        //
        // C Harmonic Minor scale: C D Eb F G Ab B
        // Counting from C (degree 1):
        // 1=C, 3=Eb, 5=G, 7=B, 9=D, 11=F, 13=Ab
        // Notes: C Eb G B D F Ab
        // This is Cm(maj13) - minor triad with major 7th and extensions
        
        let mut lexer = Lexer::new("Cmaj13".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::new(MusicalNote::c(), ScaleMode::harmonic_minor());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        // Verify root stayed C
        assert_eq!(transposed.root_note(None).unwrap().name, "C");
        
        // Verify quality changed from Major to Minor (due to Eb instead of E)
        assert_eq!(transposed.quality, ChordQuality::Minor);
        
        // Verify it has major 7th family (B natural, not Bb)
        assert_eq!(transposed.family, Some(ChordFamily::MinorMajor7));
        
        // Verify extensions are present
        assert!(transposed.extensions.has_any());
        
        // Verify the actual notes (sorted by semitone distance from root, preserving octaves)
        let notes = transposed.notes(Some(&target_key)).expect("Should resolve notes");
        let note_names: Vec<&str> = notes.iter().map(|n| n.name.as_str()).collect();
        
        // Extensions are transformed by flattening to scale degree, transforming, then preserving octave
        // C harmonic minor: C D Eb F G Ab B
        // Cmaj13 (C E G B D F A) → Cm(maj13) (C Eb G B D F Ab)
        // Sorted with octaves preserved: C(0) Eb(3) G(7) B(11) D(14) F(17) Ab(20)
        assert_eq!(note_names.len(), 7);
        assert_eq!(note_names[0], "C");   // Root (0 semitones)
        assert_eq!(note_names[1], "Eb");  // Minor third (3 semitones) - E→Eb via scale
        assert_eq!(note_names[2], "G");   // Perfect fifth (7 semitones) - unchanged
        assert_eq!(note_names[3], "B");   // Major seventh (11 semitones) - unchanged
        assert_eq!(note_names[4], "D");   // Ninth (14 semitones = 2 + 12) - unchanged
        assert_eq!(note_names[5], "F");   // Eleventh (17 semitones = 5 + 12) - unchanged
        assert_eq!(note_names[6], "Ab");  // Thirteenth (20 semitones = 8 + 12) - A→Ab via scale
    }
    
    #[test]
    fn test_transpose_non_root_13th_with_root_and_scale_change() {
        use crate::key::ScaleMode;
        
        // G13 (V13) in C Major → A13 (V13) in D Harmonic Minor
        // This tests:
        // 1. Root transposition: G → A (2 semitones up, C to D)
        // 2. Scale type change: Major → Harmonic Minor
        // 3. Non-root chord in scale (V chord, scale degree 5)
        // 4. Extension transformation through the new scale
        //
        // G13 in C Major: G B D F A C E
        // - G is scale degree 5 in C Major (C D E F G A B)
        // - Chord: Root=G(0), 3rd=B(4), 5th=D(7), 7th=F(10), 9th=A(14), 11th=C(17), 13th=E(20)
        //
        // A13 in D Harmonic Minor: A C# E G Bb D F
        // - D Harmonic Minor scale: D E F G A Bb C#
        // - A is scale degree 5 in D Harmonic Minor
        // - After scale transformation (mapping through scale degrees from A):
        //   - Chord root (scale deg 1 from A) → A
        //   - 3rd (scale deg 3 from A) → C# (not C, because of harmonic minor raised 7th)
        //   - 5th (scale deg 5 from A) → E
        //   - 7th (scale deg 7 from A) → G
        //   - 9th (scale deg 2 from A, next octave) → Bb (not B)
        //   - 11th (scale deg 4 from A, next octave) → D
        //   - 13th (scale deg 6 from A, next octave) → F
        // - Chord: Root=A(0), 3rd=C#(4), 5th=E(7), 7th=G(10), 9th=Bb(13), 11th=D(17), 13th=F(20)
        
        let mut lexer = Lexer::new("G13".to_string());
        let tokens = lexer.tokenize();
        let chord = Chord::parse(&tokens).unwrap();
        
        let source_key = Key::major(MusicalNote::c());
        let target_key = Key::new(MusicalNote::d(), ScaleMode::harmonic_minor());
        let transposed = chord.transpose_to(&target_key, Some(&source_key)).expect("Should transpose");
        
        // Verify root transposed from G to A
        assert_eq!(transposed.root_note(None).unwrap().name, "A");
        
        // Verify quality is Dominant (major triad with minor 7th)
        assert_eq!(transposed.quality, ChordQuality::Major);
        assert_eq!(transposed.family, Some(ChordFamily::Dominant7));
        
        // Verify extensions are present
        assert!(transposed.extensions.has_any());
        
        // Verify the actual notes (sorted by semitone distance, preserving octaves)
        let notes = transposed.notes(Some(&target_key)).expect("Should resolve notes");
        let note_names: Vec<&str> = notes.iter().map(|n| n.name.as_str()).collect();
        
        // D Harmonic Minor: D E F G A Bb C#
        // A13 chord in D Harmonic Minor:
        // Root (A), Major 3rd (C#), Perfect 5th (E), Minor 7th (G), 9th (Bb), 11th (D), 13th (F)
        // Sorted by semitone: A(0) C#(4) E(7) G(10) Bb(13) D(17) F(20)
        assert_eq!(note_names.len(), 7);
        assert_eq!(note_names[0], "A");   // Root (0 semitones)
        assert_eq!(note_names[1], "C#");  // Major third (4 semitones)
        assert_eq!(note_names[2], "E");   // Perfect fifth (7 semitones)
        assert_eq!(note_names[3], "G");   // Minor seventh (10 semitones)
        assert_eq!(note_names[4], "Bb");  // Ninth (13 semitones = 1 + 12)
        assert_eq!(note_names[5], "D");   // Eleventh (17 semitones = 5 + 12)
        assert_eq!(note_names[6], "F");   // Thirteenth (20 semitones = 8 + 12)
        
        // Verify the chord displays correctly
        // After scale transformation, all extensions are Natural in the new scale context
        // So the highest extension (13) masks the 7th, displaying as "A13"
        assert_eq!(transposed.to_string(), "A13");
    }
}

