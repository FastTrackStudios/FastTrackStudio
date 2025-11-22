//! Chord Memory System
//!
//! Manages global and section-specific chord memory for chord quality recall

use crate::sections::SectionType;
use std::collections::HashMap;

/// Manages chord memory for quality recall
///
/// Memory hierarchy:
/// 1. One-time overrides (prefix `!`) - highest priority (handled by parser)
/// 2. Section-specific memory - middle priority
/// 3. Global memory - low priority
/// 4. Default qualities from key - lowest priority (future feature)
#[derive(Debug, Clone)]
pub struct ChordMemory {
    /// Global chord memory (root -> full symbol)
    global: HashMap<String, String>,

    /// Section-specific chord memory (section type -> (root -> full symbol))
    section_specific: HashMap<String, HashMap<String, String>>,
}

impl ChordMemory {
    pub fn new() -> Self {
        Self {
            global: HashMap::new(),
            section_specific: HashMap::new(),
        }
    }

    /// Remember a chord in global memory
    pub fn remember_global(&mut self, root: &str, full_symbol: &str) {
        self.global
            .insert(root.to_lowercase(), full_symbol.to_string());
    }

    /// Remember a chord in section-specific memory
    pub fn remember_section(&mut self, section_type: &SectionType, root: &str, full_symbol: &str) {
        let section_key = section_type.full_name();
        self.section_specific
            .entry(section_key)
            .or_insert_with(HashMap::new)
            .insert(root.to_lowercase(), full_symbol.to_string());
    }

    /// Process a chord token and return the full symbol to use
    ///
    /// This is the main API for the parser to use.
    ///
    /// Hierarchy (v1-compatible):
    /// 1. Override (`!`): Use quality but DON'T update memory
    /// 2. Has explicit quality: Remember in BOTH global AND section memory
    /// 3. Just root: Check section → Check global (and copy to section) → Infer from key (and remember)
    ///
    /// # Arguments
    /// * `root` - The root note as a string (e.g., "C", "D", "G")
    /// * `token` - The original token (e.g., "Cmaj7", "c", "d")
    /// * `parsed_symbol` - The normalized symbol from the chord parser
    /// * `section_type` - The current section type
    /// * `is_override` - Whether this is a one-time override (prefix `!`)
    /// * `current_key` - The current key for inferring default qualities (optional)
    ///
    /// # Returns
    /// The full chord symbol to use for this chord
    pub fn process_chord(
        &mut self,
        root: &str,
        token: &str,
        parsed_symbol: &str,
        section_type: &SectionType,
        is_override: bool,
        current_key: Option<&crate::key::Key>,
    ) -> String {
        // Determine if this chord has explicit quality information
        // Strip rhythm notation (/, _, ') from token before checking length
        let token_chord_part = if let Some(pos) = token.find(|c| c == '/' || c == '_' || c == '\'')
        {
            &token[..pos]
        } else {
            token
        };
        let has_quality = token_chord_part.len() > root.len();

        if is_override {
            // Override: use parsed quality but DON'T update any memory
            parsed_symbol.to_string()
        } else if has_quality {
            // Has explicit quality - remember in BOTH global AND section memory
            self.remember_global(root, parsed_symbol);
            self.remember_section(section_type, root, parsed_symbol);
            parsed_symbol.to_string()
        } else {
            // Just root - lookup hierarchy: section → global → key inference
            if let Some(section_remembered) = self.recall_section(section_type, root) {
                // Found in section memory - use it directly
                section_remembered
            } else if let Some(global_remembered) = self.recall_global(root) {
                // Found in global but not in section - copy to section memory AND use it
                self.remember_section(section_type, root, &global_remembered);
                global_remembered
            } else if let Some(key_default) = Self::infer_from_key(root, current_key) {
                // Not found anywhere - infer from key and remember in BOTH global and section
                self.remember_global(root, &key_default);
                self.remember_section(section_type, root, &key_default);
                key_default
            } else {
                // No key or couldn't infer - use parsed as-is and remember it
                self.remember_global(root, parsed_symbol);
                self.remember_section(section_type, root, parsed_symbol);
                parsed_symbol.to_string()
            }
        }
    }

    /// Infer a chord quality from the current key
    ///
    /// Returns a full chord symbol with the appropriate quality appended to the original root
    fn infer_from_key(root: &str, key: Option<&crate::key::Key>) -> Option<String> {
        use crate::key::scale::harmonization::{harmonize_scale, HarmonizationDepth};

        let key = key?;

        // Harmonize the scale (returns Vec<Chord>)
        let chords = harmonize_scale(&key.mode, &key.root, HarmonizationDepth::Triads);

        // Check if this is a scale degree (1-7)
        if let Ok(degree) = root.parse::<usize>() {
            if degree >= 1 && degree <= 7 {
                if let Some(_chord) = chords.get(degree - 1) {
                    // For scale degrees, the quality is implied by the key
                    // Don't append any quality suffix - just return the root
                    return Some(root.to_string());
                }
            }
        }

        // Check if this is a Roman numeral
        let root_upper = root.to_uppercase();
        match root_upper.as_str() {
            "I" | "II" | "III" | "IV" | "V" | "VI" | "VII" => {
                let degree = match root_upper.as_str() {
                    "I" => 1,
                    "II" => 2,
                    "III" => 3,
                    "IV" => 4,
                    "V" => 5,
                    "VI" => 6,
                    "VII" => 7,
                    _ => return None,
                };
                if let Some(_chord) = chords.get(degree - 1) {
                    // For Roman numerals, the quality is implied by the key
                    // Don't append any quality suffix - just return the root
                    // Preserve original casing of Roman numeral
                    return Some(root.to_string());
                }
            }
            _ => {}
        }

        // Try to match by note name (case-insensitive, enharmonically aware)
        for chord in &chords {
            let chord_root_name = format!("{}", chord.root).to_lowercase();
            let target_root = root.to_lowercase();

            // Direct match
            if chord_root_name == target_root {
                let quality = Self::extract_quality(&chord.normalized);
                return Some(format!("{}{}", root, quality));
            }

            // Enharmonic match (e.g., A# = Bb, Gb = F#)
            if Self::are_enharmonic(&chord_root_name, &target_root) {
                let quality = Self::extract_quality(&chord.normalized);
                return Some(format!("{}{}", root, quality));
            }
        }

        None
    }

    /// Check if two note names are enharmonic equivalents
    fn are_enharmonic(note1: &str, note2: &str) -> bool {
        // Map each note to its semitone value (0-11)
        let semitone = |n: &str| -> Option<u8> {
            let n_upper = n.to_uppercase();
            let base = match n_upper.chars().next()? {
                'C' => 0,
                'D' => 2,
                'E' => 4,
                'F' => 5,
                'G' => 7,
                'A' => 9,
                'B' => 11,
                _ => return None,
            };

            let modifier: i8 = if n_upper.contains('#') {
                1
            } else if n_upper.contains('B') && n_upper.len() > 1 {
                -1
            } else {
                0
            };

            Some(((base as i8 + modifier).rem_euclid(12)) as u8)
        };

        match (semitone(note1), semitone(note2)) {
            (Some(s1), Some(s2)) => s1 == s2,
            _ => false,
        }
    }

    /// Extract the quality portion from a normalized chord symbol
    /// E.g., "Gmaj7" -> "maj7", "Em" -> "m", "A" -> ""
    fn extract_quality(normalized: &str) -> &str {
        // Find where the note name ends (could be 1-2 chars: C, C#, Bb, etc.)
        let mut chars = normalized.chars();
        let first = chars.next();

        if first.is_none() {
            return "";
        }

        let second = chars.next();

        // If second char is 'b' or '#', quality starts at index 2
        if let Some(c) = second {
            if c == 'b' || c == '#' {
                return &normalized[2..];
            }
        }

        // Otherwise, quality starts at index 1
        &normalized[1..]
    }

    /// Recall a chord from memory (section-specific first, then global)
    pub fn recall(&self, root: &str, section_type: Option<&SectionType>) -> Option<String> {
        let root_lower = root.to_lowercase();

        // Try section-specific memory first
        if let Some(section_type) = section_type {
            let section_key = section_type.full_name();
            if let Some(section_mem) = self.section_specific.get(&section_key) {
                if let Some(symbol) = section_mem.get(&root_lower) {
                    return Some(symbol.clone());
                }
            }
        }

        // Fall back to global memory
        self.global.get(&root_lower).cloned()
    }

    /// Recall from global memory only
    pub fn recall_global(&self, root: &str) -> Option<String> {
        self.global.get(&root.to_lowercase()).cloned()
    }

    /// Recall from section-specific memory only
    pub fn recall_section(&self, section_type: &SectionType, root: &str) -> Option<String> {
        let section_key = section_type.full_name();
        self.section_specific
            .get(&section_key)
            .and_then(|section_mem| section_mem.get(&root.to_lowercase()).cloned())
    }

    /// Check if a root is in global memory
    pub fn has_global(&self, root: &str) -> bool {
        self.global.contains_key(&root.to_lowercase())
    }

    /// Check if a root is in section-specific memory
    pub fn has_section(&self, section_type: &SectionType, root: &str) -> bool {
        let section_key = section_type.full_name();
        self.section_specific
            .get(&section_key)
            .map(|section_mem| section_mem.contains_key(&root.to_lowercase()))
            .unwrap_or(false)
    }

    /// Clear all memory
    pub fn clear(&mut self) {
        self.global.clear();
        self.section_specific.clear();
    }

    /// Clear section-specific memory for a particular section type
    ///
    /// This is called when a section is explicitly redefined with new content,
    /// allowing it to build its own memory from scratch or inherit from global.
    pub fn clear_section(&mut self, section_type: &SectionType) {
        let section_key = section_type.full_name();
        self.section_specific.remove(&section_key);
    }

    // Note: Scale-derived defaults have been removed from ChordMemory.
    // Future scale-to-chord inference will live in a dedicated module.
}

impl Default for ChordMemory {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_global_memory() {
        let mut memory = ChordMemory::new();

        memory.remember_global("G", "Gmaj7");
        memory.remember_global("C", "Cmaj7");

        assert_eq!(memory.recall_global("G"), Some("Gmaj7".to_string()));
        assert_eq!(memory.recall_global("C"), Some("Cmaj7".to_string()));
        assert_eq!(memory.recall_global("D"), None);
    }

    #[test]
    fn test_section_memory() {
        let mut memory = ChordMemory::new();
        let verse = SectionType::Verse;
        let chorus = SectionType::Chorus;

        memory.remember_section(&verse, "G", "Gmaj13");
        memory.remember_section(&chorus, "G", "Gmaj6");

        assert_eq!(
            memory.recall_section(&verse, "G"),
            Some("Gmaj13".to_string())
        );
        assert_eq!(
            memory.recall_section(&chorus, "G"),
            Some("Gmaj6".to_string())
        );
    }

    #[test]
    fn test_memory_hierarchy() {
        let mut memory = ChordMemory::new();
        let verse = SectionType::Verse;

        memory.remember_global("G", "Gmaj7");
        memory.remember_section(&verse, "G", "Gmaj13");

        // Section memory takes precedence
        assert_eq!(memory.recall("G", Some(&verse)), Some("Gmaj13".to_string()));

        // Falls back to global for other sections
        assert_eq!(memory.recall("G", None), Some("Gmaj7".to_string()));
    }

    #[test]
    fn test_process_chord_with_quality() {
        let mut memory = ChordMemory::new();
        let verse = SectionType::Verse;

        // Process a chord with explicit quality
        let result = memory.process_chord("C", "Cmaj7", "Cmaj7", &verse, false, None);

        assert_eq!(result, "Cmaj7");
        assert_eq!(memory.recall_global("C"), Some("Cmaj7".to_string()));
        assert_eq!(
            memory.recall_section(&verse, "C"),
            Some("Cmaj7".to_string())
        );
    }

    #[test]
    fn test_process_chord_recall() {
        let mut memory = ChordMemory::new();
        let verse = SectionType::Verse;

        // First, define the quality
        memory.process_chord("C", "Cmaj7", "Cmaj7", &verse, false, None);

        // Then recall with just the root
        let result = memory.process_chord("C", "c", "C", &verse, false, None);

        assert_eq!(result, "Cmaj7");
    }

    #[test]
    fn test_process_chord_global_to_section_copy() {
        let mut memory = ChordMemory::new();
        let verse = SectionType::Verse;
        let chorus = SectionType::Chorus;

        // Define in verse
        memory.process_chord("C", "Cmaj7", "Cmaj7", &verse, false, None);

        // Recall in chorus (should copy from global to chorus section memory)
        let result = memory.process_chord("C", "c", "C", &chorus, false, None);

        assert_eq!(result, "Cmaj7");
        assert_eq!(
            memory.recall_section(&chorus, "C"),
            Some("Cmaj7".to_string())
        );
    }

    #[test]
    fn test_process_chord_override() {
        let mut memory = ChordMemory::new();
        let verse = SectionType::Verse;

        // Define default quality
        memory.process_chord("C", "Cmaj7", "Cmaj7", &verse, false, None);

        // Override without remembering
        let result = memory.process_chord("C", "Cmaj9", "Cmaj9", &verse, true, None);

        assert_eq!(result, "Cmaj9");
        // Memory should still have maj7
        assert_eq!(memory.recall_global("C"), Some("Cmaj7".to_string()));
    }

    #[test]
    fn test_clear_section_memory() {
        let mut memory = ChordMemory::new();
        let verse = SectionType::Verse;

        // Define chord in verse
        memory.remember_section(&verse, "C", "Cmaj7");
        assert!(memory.has_section(&verse, "C"));

        // Clear verse memory
        memory.clear_section(&verse);
        assert!(!memory.has_section(&verse, "C"));
    }

    #[test]
    fn test_section_redefinition_workflow() {
        let mut memory = ChordMemory::new();
        let verse = SectionType::Verse;

        // First verse: define Cmaj7
        memory.process_chord("C", "Cmaj7", "Cmaj7", &verse, false, None);
        assert_eq!(
            memory.recall_section(&verse, "C"),
            Some("Cmaj7".to_string())
        );

        // Clear section memory (simulating explicit redefinition)
        memory.clear_section(&verse);

        // Process just root - should get from global
        let result = memory.process_chord("C", "c", "C", &verse, false, None);
        assert_eq!(result, "Cmaj7"); // Gets from global
        assert_eq!(
            memory.recall_section(&verse, "C"),
            Some("Cmaj7".to_string())
        ); // Copied to section
    }

    #[test]
    fn test_multiple_sections_independence() {
        let mut memory = ChordMemory::new();
        let verse = SectionType::Verse;
        let chorus = SectionType::Chorus;

        // Define different qualities in different sections
        memory.process_chord("C", "Cmaj7", "Cmaj7", &verse, false, None);
        memory.clear_section(&chorus); // Chorus starts fresh
        memory.process_chord("C", "Cmaj9", "Cmaj9", &chorus, false, None);

        // Clear verse and recall - should get maj9 from global (most recent)
        memory.clear_section(&verse);
        let result = memory.process_chord("C", "c", "C", &verse, false, None);

        assert_eq!(result, "Cmaj9"); // Gets the most recent global value
    }

    #[test]
    fn test_debug_csharp_major_harmonization() {
        use crate::key::scale::harmonization::{harmonize_scale, HarmonizationDepth};
        use crate::key::Key;
        use crate::primitives::MusicalNote;

        // Create C# major key from semitone
        let c_sharp = MusicalNote::from_semitone(1, true); // C# is 1 semitone above C
        let key = Key::major(c_sharp);

        // Get harmonization
        let chords = harmonize_scale(&key.mode, &key.root, HarmonizationDepth::Triads);

        println!("\n=== C# Major Harmonization ===");
        for (i, chord) in chords.iter().enumerate() {
            println!(
                "Degree {}: root={}, normalized={}",
                i + 1,
                chord.root,
                chord.normalized
            );
        }

        // Test inferring A# (6th degree)
        let mut memory = ChordMemory::new();
        let verse = SectionType::Verse;

        let result = memory.process_chord("A#", "a#", "A#", &verse, false, Some(&key));
        println!("\nInferred A# in C# major: {}", result);
        assert_eq!(result, "A#m", "A# in C# major should be A#m (vi is minor)");
    }
}
