//! Core data types for lyrics

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Represents a complete lyrics document
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Lyrics {
    /// Unique identifier for this lyrics document
    pub id: Option<Uuid>,
    /// Song ID this lyrics document is associated with (optional)
    pub song_id: Option<Uuid>,
    /// Song name (for reference, may differ from song.name)
    pub song_name: String,
    /// List of sections in the lyrics
    pub sections: Vec<LyricSection>,
    /// Optional metadata
    pub metadata: HashMap<String, String>,
}

impl Lyrics {
    /// Create a new lyrics document
    pub fn new(song_name: String) -> Self {
        Self {
            id: None,
            song_id: None,
            song_name,
            sections: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    /// Create a new lyrics document with ID
    pub fn with_id(id: Uuid, song_name: String) -> Self {
        Self {
            id: Some(id),
            song_id: None,
            song_name,
            sections: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    /// Add a section to the lyrics
    pub fn add_section(&mut self, section: LyricSection) {
        self.sections.push(section);
    }

    /// Get a section by index
    pub fn get_section(&self, index: usize) -> Option<&LyricSection> {
        self.sections.get(index)
    }

    /// Get a mutable reference to a section by index
    pub fn get_section_mut(&mut self, index: usize) -> Option<&mut LyricSection> {
        self.sections.get_mut(index)
    }

    /// Find sections by name (case-insensitive)
    pub fn find_sections_by_name(&self, name: &str) -> Vec<&LyricSection> {
        self.sections
            .iter()
            .filter(|s| s.name.eq_ignore_ascii_case(name))
            .collect()
    }

    /// Get metadata value
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }

    /// Set metadata value
    pub fn set_metadata<K: Into<String>, V: Into<String>>(&mut self, key: K, value: V) {
        self.metadata.insert(key.into(), value.into());
    }

    /// Remove metadata value
    pub fn remove_metadata(&mut self, key: &str) -> Option<String> {
        self.metadata.remove(key)
    }
}

/// Represents a section within the lyrics (e.g., [Verse 1], [Chorus])
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LyricSection {
    /// Unique identifier for this section
    pub id: Option<Uuid>,
    /// Section name (e.g., "Verse 1", "Chorus", "Intro")
    pub name: String,
    /// Parsed section type (if recognized)
    pub section_type: Option<SectionTypeHint>,
    /// Section number (if applicable, e.g., 1 for "Verse 1")
    pub number: Option<u32>,
    /// List of lines in this section
    pub lines: Vec<LyricLine>,
    /// Optional metadata
    pub metadata: HashMap<String, String>,
}

impl LyricSection {
    /// Create a new lyric section
    pub fn new(name: String) -> Self {
        Self {
            id: None,
            name: name.clone(),
            section_type: SectionTypeHint::from_name(&name),
            number: Self::extract_number(&name),
            lines: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    /// Extract number from section name (e.g., "Verse 1" -> Some(1))
    fn extract_number(name: &str) -> Option<u32> {
        // Try to find a number at the end of the name
        let parts: Vec<&str> = name.split_whitespace().collect();
        if let Some(last) = parts.last() {
            last.parse::<u32>().ok()
        } else {
            None
        }
    }

    /// Add a line to this section
    pub fn add_line(&mut self, line: LyricLine) {
        self.lines.push(line);
    }

    /// Get metadata value
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }

    /// Set metadata value
    pub fn set_metadata<K: Into<String>, V: Into<String>>(&mut self, key: K, value: V) {
        self.metadata.insert(key.into(), value.into());
    }
}

/// Hint about the section type (for matching with setlist sections)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SectionTypeHint {
    Intro,
    Verse,
    Chorus,
    Bridge,
    Outro,
    Instrumental,
    PreChorus,
    PostChorus,
    Custom(String),
}

impl SectionTypeHint {
    /// Try to parse section type from a name
    pub fn from_name(name: &str) -> Option<Self> {
        let name_lower = name.to_lowercase();
        let name_lower = name_lower.trim();

        // Remove numbers and extra whitespace for matching
        let base_name = name_lower
            .split_whitespace()
            .filter(|s| s.parse::<u32>().is_err())
            .collect::<Vec<_>>()
            .join(" ");

        match base_name.as_str() {
            "intro" | "introduction" => Some(Self::Intro),
            "verse" | "v" => Some(Self::Verse),
            "chorus" | "ch" => Some(Self::Chorus),
            "bridge" | "br" => Some(Self::Bridge),
            "outro" | "outroduction" => Some(Self::Outro),
            "instrumental" | "inst" => Some(Self::Instrumental),
            "pre-chorus" | "prechorus" | "pre chorus" => Some(Self::PreChorus),
            "post-chorus" | "postchorus" | "post chorus" => Some(Self::PostChorus),
            _ => Some(Self::Custom(name.to_string())),
        }
    }

    /// Get the base name without number
    pub fn base_name(&self) -> String {
        match self {
            Self::Intro => "Intro".to_string(),
            Self::Verse => "Verse".to_string(),
            Self::Chorus => "Chorus".to_string(),
            Self::Bridge => "Bridge".to_string(),
            Self::Outro => "Outro".to_string(),
            Self::Instrumental => "Instrumental".to_string(),
            Self::PreChorus => "Pre-Chorus".to_string(),
            Self::PostChorus => "Post-Chorus".to_string(),
            Self::Custom(name) => name.clone(),
        }
    }
}

/// Represents a single line of lyrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LyricLine {
    /// Unique identifier for this line
    pub id: Option<Uuid>,
    /// The raw text content of the line
    pub text: String,
    /// List of parts (regular text and parenthetical)
    pub parts: Vec<LinePart>,
    /// Words in this line, broken down into syllables with timing and MIDI note assignments
    /// This is optional - if None, words can be generated on-demand from text
    /// If Some, it contains the persisted word/syllable structure with sync data
    pub words: Option<Vec<Word>>,
    /// Optional timing information (in seconds from song start) - line-level timing
    /// Note: Individual syllable timing is stored in words[].syllables[].start_time/end_time
    pub start_time: Option<f64>,
    /// Optional timing information (in seconds from song start) - line-level timing
    /// Note: Individual syllable timing is stored in words[].syllables[].start_time/end_time
    pub end_time: Option<f64>,
    /// Optional metadata
    pub metadata: HashMap<String, String>,
}

impl LyricLine {
    /// Create a new lyric line from text
    pub fn new(text: String) -> Self {
        let parts = Self::parse_parts(&text);
        Self {
            id: None,
            text: text.clone(),
            parts,
            words: None, // Words generated on-demand if not provided
            start_time: None,
            end_time: None,
            metadata: HashMap::new(),
        }
    }

    /// Create a new lyric line with words already parsed
    pub fn with_words(text: String, words: Vec<Word>) -> Self {
        let parts = Self::parse_parts(&text);
        Self {
            id: None,
            text: text.clone(),
            parts,
            words: Some(words),
            start_time: None,
            end_time: None,
            metadata: HashMap::new(),
        }
    }

    /// Get words for this line, generating them if not already stored
    pub fn get_words(&self) -> Vec<Word> {
        if let Some(ref stored_words) = self.words {
            stored_words.clone()
        } else {
            split_line_into_words(&self.regular_text())
        }
    }

    /// Set words for this line (with syllable timing and MIDI notes)
    pub fn set_words(&mut self, words: Vec<Word>) {
        self.words = Some(words);
    }

    /// Get all syllables from this line (from stored words or generated)
    pub fn get_syllables(&self) -> Vec<Syllable> {
        self.get_words()
            .iter()
            .flat_map(|word| word.syllables.clone())
            .collect()
    }

    /// Parse text into parts (handling parenthetical text)
    fn parse_parts(text: &str) -> Vec<LinePart> {
        let mut parts = Vec::new();
        let mut current_pos = 0;
        let chars: Vec<char> = text.chars().collect();

        while current_pos < chars.len() {
            if chars[current_pos] == '(' {
                // Find matching closing parenthesis
                let mut depth = 1;
                let mut end_pos = current_pos + 1;
                while end_pos < chars.len() && depth > 0 {
                    if chars[end_pos] == '(' {
                        depth += 1;
                    } else if chars[end_pos] == ')' {
                        depth -= 1;
                    }
                    if depth > 0 {
                        end_pos += 1;
                    }
                }

                if depth == 0 {
                    // Found matching parenthesis
                    let parenthetical: String = chars[current_pos + 1..end_pos].iter().collect();
                    if !parenthetical.trim().is_empty() {
                        parts.push(LinePart::Parenthetical(parenthetical.trim().to_string()));
                    }
                    current_pos = end_pos + 1;
                } else {
                    // No matching parenthesis, treat as regular text
                    parts.push(LinePart::Regular(chars[current_pos].to_string()));
                    current_pos += 1;
                }
            } else {
                // Regular text - collect until we hit a parenthesis
                let mut regular_start = current_pos;
                while current_pos < chars.len() && chars[current_pos] != '(' {
                    current_pos += 1;
                }
                let regular: String = chars[regular_start..current_pos].iter().collect();
                if !regular.trim().is_empty() {
                    parts.push(LinePart::Regular(regular.trim().to_string()));
                }
            }
        }

        // If no parts were created, add the whole text as a regular part
        if parts.is_empty() && !text.trim().is_empty() {
            parts.push(LinePart::Regular(text.trim().to_string()));
        }

        parts
    }

    /// Get the display text (without parenthetical markers)
    pub fn display_text(&self) -> String {
        self.parts
            .iter()
            .map(|p| match p {
                LinePart::Regular(text) => text.clone(),
                LinePart::Parenthetical(text) => format!("({})", text),
            })
            .collect::<Vec<_>>()
            .join(" ")
    }

    /// Get only the regular text (excluding parenthetical)
    pub fn regular_text(&self) -> String {
        self.parts
            .iter()
            .filter_map(|p| match p {
                LinePart::Regular(text) => Some(text.clone()),
                LinePart::Parenthetical(_) => None,
            })
            .collect::<Vec<_>>()
            .join(" ")
    }

    /// Get metadata value
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }

    /// Set metadata value
    pub fn set_metadata<K: Into<String>, V: Into<String>>(&mut self, key: K, value: V) {
        self.metadata.insert(key.into(), value.into());
    }
}

/// Represents a part of a lyric line (either regular text or parenthetical)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LinePart {
    /// Regular lyric text
    Regular(String),
    /// Parenthetical text (e.g., "(Woo)", "(Are you ready?)")
    Parenthetical(String),
}

/// Represents a syllable within a word (for karaoke-style display)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Syllable {
    /// The text of this syllable
    pub text: String,
    /// Optional timing information (in seconds from song start)
    pub start_time: Option<f64>,
    /// Optional timing information (in seconds from song start)
    pub end_time: Option<f64>,
    /// Optional MIDI note number
    pub midi_note: Option<u8>,
    /// Optional MIDI velocity
    pub midi_velocity: Option<u8>,
}

/// Represents a word broken down into syllables
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Word {
    /// The full word text
    pub text: String,
    /// Syllables that make up this word
    pub syllables: Vec<Syllable>,
}

impl Word {
    /// Create a word from text using syllable counting heuristics
    ///
    /// Uses regex-based heuristics to estimate the number of syllables,
    /// then applies splitting heuristics to create syllable segments.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::crate::lyrics::core::Word;
    ///
    /// let word = Word::from_text("hello");
    /// assert_eq!(word.text, "hello");
    /// assert_eq!(word.syllables.len(), 2);
    /// ```
    pub fn from_text(text: &str) -> Self {
        use crate::syllables::syllables_in_word;
        
        // Get estimated syllable count
        let expected_count = syllables_in_word(text);
        
        // If it's a single syllable word, return it as-is
        if expected_count <= 1 {
            return Self {
                text: text.to_string(),
                syllables: vec![Syllable {
                    text: text.to_string(),
                    start_time: None,
                    end_time: None,
                    midi_note: None,
                    midi_velocity: None,
                }],
            };
        }

        // Split the word into syllables using improved heuristics
        let syllables = Self::split_into_syllables(text, expected_count);

        Self {
            text: text.to_string(),
            syllables,
        }
    }

    /// Split a word into syllables using heuristics guided by expected count
    ///
    /// This uses a vowel-based splitting algorithm that attempts to match
    /// the expected syllable count from the syllable counting module.
    /// Returns actual text segments for each syllable (e.g., "hello" -> ["hel", "lo"]).
    fn split_into_syllables(text: &str, expected_count: usize) -> Vec<Syllable> {
        let chars: Vec<char> = text.chars().collect();
        if chars.is_empty() {
            return vec![Syllable {
                text: text.to_string(),
                start_time: None,
                end_time: None,
                midi_note: None,
                midi_velocity: None,
            }];
        }

        // If we only need 1 syllable, return the whole word
        if expected_count == 1 {
            return vec![Syllable {
                text: text.to_string(),
                start_time: None,
                end_time: None,
                midi_note: None,
                midi_velocity: None,
            }];
        }

        // Find all vowel positions (individual vowels)
        let vowel_positions: Vec<usize> = chars
            .iter()
            .enumerate()
            .filter_map(|(i, ch)| if Self::is_vowel(*ch) { Some(i) } else { None })
            .collect();

        if vowel_positions.is_empty() {
            // No vowels - return whole word as one syllable
            return vec![Syllable {
                text: text.to_string(),
                start_time: None,
                end_time: None,
                midi_note: None,
                midi_velocity: None,
            }];
        }

        // Group consecutive vowels
        let vowel_groups = Self::group_vowels(&vowel_positions);
        
        // Build split points based on vowel groups and expected syllable count
        let mut split_points = vec![0];
        let mut vowel_idx = 0;
        
        // Distribute vowel groups across syllables
        for syllable_num in 0..expected_count.saturating_sub(1) {
            if vowel_idx >= vowel_groups.len() {
                break;
            }
            
            // Calculate how many vowel groups should go in this syllable
            let remaining_syllables = expected_count - syllable_num;
            let remaining_vowel_groups = vowel_groups.len() - vowel_idx;
            let groups_per_syllable = (remaining_vowel_groups as f64 / remaining_syllables as f64).ceil() as usize;
            let groups_to_use = groups_per_syllable.min(remaining_vowel_groups);
            
            if groups_to_use == 0 {
                break;
            }
            
            // Get the last vowel group we're using for this syllable
            let last_group_idx = vowel_idx + groups_to_use - 1;
            let last_group = &vowel_groups[last_group_idx];
            let vowel_end = last_group[last_group.len() - 1];
            
            // Count consonants after this vowel group
            let consonants_after = Self::count_consonants(&chars, vowel_end + 1);
            
            // Determine split point
            let split_point = if consonants_after >= 2 {
                // Multiple consonants: split after first consonant
                // e.g., "hello" -> "hel-lo"
                vowel_end + 2
            } else if consonants_after == 1 {
                // Single consonant: check if we should split before or after
                // For multi-syllable words, often split before the consonant
                if syllable_num < expected_count - 1 && remaining_vowel_groups > 1 {
                    // Not last syllable and more vowels coming - split before consonant
                    vowel_end + 1
                } else {
                    // Last syllable or no more vowels - include consonant
                    vowel_end + 1
                }
            } else {
                // No consonants: end at vowel
                vowel_end + 1
            };
            
            // Make sure we're making progress
            if split_point > split_points.last().copied().unwrap_or(0) && split_point < chars.len() {
                split_points.push(split_point);
                vowel_idx = last_group_idx + 1;
            } else {
                // Can't make progress, break
                break;
            }
        }
        
        // Add final position
        split_points.push(chars.len());
        
        // Create syllables from split points
        let mut syllables = Vec::new();
        for i in 0..split_points.len().saturating_sub(1) {
            let start = split_points[i];
            let end = split_points[i + 1];
            
            if end > start && end <= chars.len() {
                let syllable_text: String = chars[start..end].iter().collect();
                
                if !syllable_text.trim().is_empty() {
                    syllables.push(Syllable {
                        text: syllable_text,
                        start_time: None,
                        end_time: None,
                        midi_note: None,
                        midi_velocity: None,
                    });
                }
            }
        }

        // If we didn't get the right number, use fallback: divide evenly at vowel boundaries
        if syllables.len() != expected_count && expected_count > 1 {
            syllables.clear();
            split_points.clear();
            split_points.push(0);
            
            // Try to split at vowel boundaries more evenly
            let vowels_per_syllable = (vowel_positions.len() as f64 / expected_count as f64).ceil() as usize;
            
            for syllable_num in 0..expected_count.saturating_sub(1) {
                let vowel_idx_for_split = (syllable_num + 1) * vowels_per_syllable;
                
                if vowel_idx_for_split < vowel_positions.len() {
                    let vowel_pos = vowel_positions[vowel_idx_for_split];
                    // Count consonants before this vowel
                    let consonants_before = Self::count_consonants_before(&chars, vowel_pos);
                    
                    let split_point = if consonants_before >= 1 {
                        vowel_pos - consonants_before
                    } else {
                        vowel_pos
                    };
                    
                    if split_point > split_points.last().copied().unwrap_or(0) && split_point < chars.len() {
                        split_points.push(split_point);
                    }
                }
            }
            
            split_points.push(chars.len());
            
            // Create syllables
            for i in 0..split_points.len().saturating_sub(1) {
                let start = split_points[i];
                let end = split_points[i + 1];
                
                if end > start && end <= chars.len() {
                    let syllable_text: String = chars[start..end].iter().collect();
                    if !syllable_text.trim().is_empty() {
                        syllables.push(Syllable {
                            text: syllable_text,
                            start_time: None,
                            end_time: None,
                            midi_note: None,
                            midi_velocity: None,
                        });
                    }
                }
            }
        }

        // Final fallback: if still wrong, just divide evenly
        if syllables.len() != expected_count && expected_count > 1 {
            syllables.clear();
            let chars_per_syllable = (chars.len() as f64 / expected_count as f64).ceil() as usize;
            
            for i in 0..expected_count {
                let start = i * chars_per_syllable;
                let end = if i == expected_count - 1 {
                    chars.len()
                } else {
                    ((i + 1) * chars_per_syllable).min(chars.len())
                };
                
                if start < chars.len() && end > start {
                    let syllable_text: String = chars[start..end].iter().collect();
                    syllables.push(Syllable {
                        text: syllable_text,
                        start_time: None,
                        end_time: None,
                        midi_note: None,
                        midi_velocity: None,
                    });
                }
            }
        }

        // Ensure we have at least one syllable
        if syllables.is_empty() {
            syllables.push(Syllable {
                text: text.to_string(),
                start_time: None,
                end_time: None,
                midi_note: None,
                midi_velocity: None,
            });
        }

        syllables
    }

    /// Count consecutive consonants before a position (going backwards)
    fn count_consonants_before(chars: &[char], pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }
        let mut count = 0;
        let mut i = pos.saturating_sub(1);
        while i < pos && !Self::is_vowel(chars[i]) {
            count += 1;
            if i == 0 {
                break;
            }
            i -= 1;
        }
        count
    }

    /// Check if a character is a vowel
    fn is_vowel(ch: char) -> bool {
        matches!(ch.to_ascii_lowercase(), 'a' | 'e' | 'i' | 'o' | 'u' | 'y')
    }

    /// Count consecutive consonants starting from a position
    fn count_consonants(chars: &[char], start: usize) -> usize {
        let mut count = 0;
        let mut i = start;
        while i < chars.len() && !Self::is_vowel(chars[i]) {
            count += 1;
            i += 1;
        }
        count
    }

    /// Group consecutive vowel positions into vowel groups
    fn group_vowels(vowel_positions: &[usize]) -> Vec<Vec<usize>> {
        if vowel_positions.is_empty() {
            return Vec::new();
        }

        let mut groups = Vec::new();
        let mut current_group = vec![vowel_positions[0]];

        for i in 1..vowel_positions.len() {
            if vowel_positions[i] == vowel_positions[i - 1] + 1 {
                // Consecutive vowels - same group (e.g., "read" has "ea")
                current_group.push(vowel_positions[i]);
            } else {
                // New vowel group
                groups.push(current_group);
                current_group = vec![vowel_positions[i]];
            }
        }
        groups.push(current_group);
        groups
    }
}

/// Split a line of text into words, each with their syllables
///
/// This function takes a line of lyrics and splits it into individual words,
/// where each word is broken down into its constituent syllables.
///
/// # Examples
///
/// ```
/// use crate::crate::lyrics::core::split_line_into_words;
///
/// let words = split_line_into_words("hello world");
/// assert_eq!(words.len(), 2);
/// assert_eq!(words[0].text, "hello");
/// assert_eq!(words[0].syllables.len(), 2);
/// assert_eq!(words[1].text, "world");
/// assert_eq!(words[1].syllables.len(), 1);
/// ```
pub fn split_line_into_words(line: &str) -> Vec<Word> {
    // Remove parenthetical text markers for word splitting
    // We'll preserve them in the word text but split on whitespace
    let trimmed = line.trim();
    
    if trimmed.is_empty() {
        return Vec::new();
    }

    // Split by whitespace and create words
    trimmed
        .split_whitespace()
        .filter(|word| !word.trim().is_empty())
        .map(|word| {
            // Remove punctuation for syllable counting, but preserve in word text
            Word::from_text(word)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_line_into_words() {
        let words = split_line_into_words("hello world");
        assert_eq!(words.len(), 2);
        assert_eq!(words[0].text, "hello");
        assert_eq!(words[1].text, "world");
    }

    #[test]
    fn test_split_line_into_words_with_punctuation() {
        let words = split_line_into_words("Drowning, fishing, dropping!");
        assert_eq!(words.len(), 3);
        assert_eq!(words[0].text, "Drowning,");
        assert_eq!(words[1].text, "fishing,");
        assert_eq!(words[2].text, "dropping!");
    }

    #[test]
    fn test_split_line_into_words_with_parenthetical() {
        let words = split_line_into_words("(Woo) Well!");
        assert_eq!(words.len(), 2);
        assert_eq!(words[0].text, "(Woo)");
        assert_eq!(words[1].text, "Well!");
    }

    #[test]
    fn test_split_line_into_words_empty() {
        let words = split_line_into_words("");
        assert!(words.is_empty());
        
        let words2 = split_line_into_words("   ");
        assert!(words2.is_empty());
    }

    #[test]
    fn test_split_line_into_words_syllables() {
        let words = split_line_into_words("hello beautiful world");
        assert_eq!(words.len(), 3);
        
        // Check that each word has syllables
        assert!(!words[0].syllables.is_empty()); // hello = 2 syllables
        assert!(!words[1].syllables.is_empty()); // beautiful = 3 syllables
        assert!(!words[2].syllables.is_empty()); // world = 1 syllable
        
        // Verify syllable counts match expected
        assert_eq!(words[0].syllables.len(), 2); // hello
        assert_eq!(words[1].syllables.len(), 3); // beautiful
        assert_eq!(words[2].syllables.len(), 1); // world
    }

    #[test]
    fn test_split_line_into_words_complex_line() {
        let line = "Drowning, fishing, dropping, screaming under the lights";
        let words = split_line_into_words(line);
        
        // Should have 7 words: Drowning, fishing, dropping, screaming, under, the, lights
        assert_eq!(words.len(), 7, "Expected 7 words, got {}", words.len());
        
        // Verify all words have syllables
        for word in &words {
            assert!(!word.syllables.is_empty(), "Word '{}' should have syllables", word.text);
        }
        
        // Check specific words
        assert_eq!(words[0].text, "Drowning,");
        assert!(words[0].syllables.len() >= 1, "Drowning should have at least 1 syllable");
        
        assert_eq!(words[3].text, "screaming");
        assert!(words[3].syllables.len() >= 1, "Screaming should have at least 1 syllable");
    }

    #[test]
    fn test_split_line_into_words_lyric_examples() {
        // Test with actual lyric lines from the example
        let test_cases = vec![
            "Drowning, fishing, dropping, screaming under the lights",
            "I'm feeling everything crashing, burning, I lost track of time",
            "I'm breathing, I'm breathing, I think I'm reading you well",
            "Well! Well!",
            "(Woo)",
            "(Are you ready?)",
        ];

        for line in test_cases {
            let words = split_line_into_words(line);
            assert!(!words.is_empty(), "Line '{}' should produce words", line);
            
            // Verify each word has syllables
            for word in &words {
                assert!(!word.syllables.is_empty(), 
                    "Word '{}' from line '{}' should have syllables", 
                    word.text, line);
            }
        }
    }

    #[test]
    fn test_syllable_text_content() {
        // Test that syllables actually contain text segments
        use crate::syllables::syllables_in_word;
        
        let test_cases: Vec<(&str, Vec<&str>)> = vec![
            ("hello", vec!["hel", "lo"]),
            ("beautiful", vec!["beau", "ti", "ful"]),
            ("drowning", vec!["drow", "ning"]),
            ("supercalifragilistic", vec!["super", "ca", "lif", "ra", "gi", "lis", "tic"]),
        ];

        for (word_text, expected_syllables) in test_cases {
            // Get the actual count from the counter
            let expected_count = syllables_in_word(word_text);
            
            // Skip words that the counter thinks have 1 syllable (they won't be split)
            if expected_count <= 1 {
                continue;
            }
            
            let word = Word::from_text(word_text);
            
            // Extract actual syllable texts
            let actual_syllables: Vec<&str> = word.syllables.iter().map(|s| s.text.as_str()).collect();
            
            // Print output syllable vec
            println!(
                "Word: '{}' -> {} syllables: {:?}",
                word_text,
                word.syllables.len(),
                actual_syllables
            );
            
            // Verify we have the right number of syllables
            assert_eq!(
                word.syllables.len(),
                expected_count,
                "Word '{}' should have {} syllables, got {}. Syllables: {:?}",
                word_text,
                expected_count,
                word.syllables.len(),
                actual_syllables
            );
            
            // Directly compare against expected syllable vec
            assert_eq!(
                actual_syllables,
                expected_syllables,
                "Word '{}' syllables don't match expected. Expected: {:?}, Got: {:?}",
                word_text,
                expected_syllables,
                actual_syllables
            );
            
            // Verify each syllable has text content
            for (i, syllable) in word.syllables.iter().enumerate() {
                assert!(
                    !syllable.text.is_empty(),
                    "Syllable {} of '{}' should have text content. Got: '{}'",
                    i,
                    word_text,
                    syllable.text
                );
            }
            
            // Verify the syllables combine to form the original word (ignoring case and punctuation)
            let combined: String = word.syllables.iter().map(|s| s.text.as_str()).collect();
            let word_clean: String = word_text.chars().filter(|c| c.is_alphanumeric()).collect();
            let combined_clean: String = combined.chars().filter(|c| c.is_alphanumeric()).collect();
            assert_eq!(
                combined_clean.to_lowercase(),
                word_clean.to_lowercase(),
                "Syllables of '{}' should combine to form the word. Got: {} (syllables: {:?})",
                word_text,
                combined,
                actual_syllables
            );
        }
    }

    #[test]
    fn test_split_line_into_words_syllable_text() {
        // Test that split_line_into_words returns words with actual syllable text
        use crate::syllables::syllables_in_word;
        
        let line = "Hello beautiful supercalifragilistic";
        let words = split_line_into_words(line);
        
        assert_eq!(words.len(), 3);
        
        // Check first word
        let word1 = &words[0];
        assert_eq!(word1.text, "Hello");
        let expected_syllables_1 = syllables_in_word("hello");
        assert_eq!(
            word1.syllables.len(),
            expected_syllables_1,
            "Hello should have {} syllables, got {}",
            expected_syllables_1,
            word1.syllables.len()
        );
        
        // Verify syllables have text
        for (i, syllable) in word1.syllables.iter().enumerate() {
            assert!(!syllable.text.is_empty(), "Syllable {} should have text: {:?}", i, syllable.text);
        }
        
        // Check second word
        let word2 = &words[1];
        assert_eq!(word2.text, "beautiful");
        let expected_syllables_2 = syllables_in_word("beautiful");
        assert_eq!(
            word2.syllables.len(),
            expected_syllables_2,
            "beautiful should have {} syllables, got {}",
            expected_syllables_2,
            word2.syllables.len()
        );
        
        // Check third word
        let word3 = &words[2];
        assert_eq!(word3.text, "supercalifragilistic");
        let expected_syllables_3 = syllables_in_word("supercalifragilistic");
        assert_eq!(
            word3.syllables.len(),
            expected_syllables_3,
            "supercalifragilistic should have {} syllables, got {}",
            expected_syllables_3,
            word3.syllables.len()
        );
        
        // Print for debugging
        println!("Word: {}", word3.text);
        for (i, syllable) in word3.syllables.iter().enumerate() {
            println!("  Syllable {}: '{}'", i, syllable.text);
        }
        
        // Verify all syllables have text content
        for word in &words {
            for syllable in &word.syllables {
                assert!(!syllable.text.is_empty(), "Syllable should have text: '{}'", syllable.text);
            }
        }
    }
}


