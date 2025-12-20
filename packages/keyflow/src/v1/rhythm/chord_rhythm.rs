// Chord Rhythm Parser
//
// Combines chord parsing with rhythm notation:
// 1. Slash notation: G //// C // (based on time signature)
// 2. Lilypond duration: Gmaj7_4 Cadd9_8. 
// 3. Push/pull: 'C (pushed), C' (delayed)
// 4. Articulations: G-> C-. (inherited from rhythm parser)
//
// The chart parser handles:
// - Variable definitions (rhythm = { ... })
// - Inline syntax (r{...}, \rhythm)

use super::rhythm::ParsedRhythm;
use crate::v1::chord::chord::ChordData;

/// Represents a slash rhythm token
#[derive(Debug, Clone, PartialEq)]
pub enum SlashToken {
    /// Single slash (one beat based on time signature)
    Slash,
    
    /// Dotted slash (for compound time like 6/8)
    DottedSlash,
}

/// Push/pull timing modification
#[derive(Debug, Clone, PartialEq)]
pub enum TimingModifier {
    /// Push back (earlier): ' before chord
    PushEighth,      // 'C
    PushSixteenth,   // ''C
    PushThirtySecond, // '''C
    
    /// Pull (delay): ' after chord
    DelayEighth,     // C'
    DelaySixteenth,  // C''
    DelayThirtySecond, // C'''
}

impl TimingModifier {
    /// Get the timing offset in beats (negative = earlier, positive = later)
    pub fn offset_in_beats(&self) -> f64 {
        match self {
            TimingModifier::PushEighth => -0.5,
            TimingModifier::PushSixteenth => -0.25,
            TimingModifier::PushThirtySecond => -0.125,
            TimingModifier::DelayEighth => 0.5,
            TimingModifier::DelaySixteenth => 0.25,
            TimingModifier::DelayThirtySecond => 0.125,
        }
    }
}

/// Rhythm notation for a chord
#[derive(Debug, Clone, PartialEq)]
pub enum ChordRhythm {
    /// Default (one bar per chord)
    Default,
    
    /// Slash notation: //// or // or /.
    Slashes(Vec<SlashToken>),
    
    /// Lilypond duration: _4, _8., etc.
    Duration(ParsedRhythm),
    
    /// Rest or space
    RestOrSpace { 
        is_rest: bool,  // true = rest (r), false = space (s)
        rhythm: ParsedRhythm,
    },
}

/// A chord with rhythm and timing information
#[derive(Debug, Clone)]
pub struct ChordWithRhythm {
    /// The chord itself (can be None for rests/spaces)
    pub chord: Option<ChordData>,
    
    /// Rhythm notation
    pub rhythm: ChordRhythm,
    
    /// Push/pull timing modifier
    pub timing_modifier: Option<TimingModifier>,
    
    /// Original input string
    pub original: String,
}

impl ChordWithRhythm {
    /// Capitalize the first character of a chord name (for case-insensitive parsing)
    fn capitalize_chord_name(chord_name: &str) -> String {
        if chord_name.is_empty() {
            return chord_name.to_string();
        }
        
        let mut result = String::new();
        result.push(chord_name.chars().next().unwrap().to_ascii_uppercase());
        if chord_name.len() > 1 {
            result.push_str(&chord_name[1..]);
        }
        result
    }
    
    /// Create a new chord with default rhythm (one bar)
    pub fn new(chord: ChordData) -> Self {
        Self {
            original: String::new(),
            chord: Some(chord),
            rhythm: ChordRhythm::Default,
            timing_modifier: None,
        }
    }
    
    /// Parse a chord with optional rhythm notation
    /// 
    /// Supports:
    /// - Basic: "Cmaj7" (default one bar)
    /// - With duration: "Cmaj7_4" (quarter note)
    /// - With slashes: "Cmaj7 ////" (4 quarter notes)
    /// - With push: "'Cmaj7" (pushed back eighth note)
    /// - With pull: "Cmaj7'" (delayed eighth note)
    /// - Rest: "r4" (quarter rest)
    /// - Space: "s2" (half note space)
    /// - No spaces: "G//// C//// D// Em//" (inline slashes)
    /// - Scale degrees: "1", "2", etc. (converted to note names for parsing)
    /// - Roman numerals: "I", "II", etc. (converted to note names for parsing)
    pub fn parse(input: &str, _time_signature: Option<(u8, u8)>) -> Result<Self, String> {
        let input = input.trim();
        
        if input.is_empty() {
            return Err("Empty chord rhythm string".to_string());
        }
        
        let mut result = ChordWithRhythm {
            chord: None,
            rhythm: ChordRhythm::Default,
            timing_modifier: None,
            original: input.to_string(),
        };
        
        // Check for rest (r followed by duration)
        if input.starts_with('r') && input.len() > 1 {
            let rhythm_part = &input[1..];
            if rhythm_part.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                let parsed_rhythm = ParsedRhythm::parse(rhythm_part)?;
                result.rhythm = ChordRhythm::RestOrSpace {
                    is_rest: true,
                    rhythm: parsed_rhythm,
                };
                return Ok(result);
            }
        }
        
        // Check for space (s followed by duration)
        if input.starts_with('s') && input.len() > 1 {
            let rhythm_part = &input[1..];
            if rhythm_part.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                let parsed_rhythm = ParsedRhythm::parse(rhythm_part)?;
                result.rhythm = ChordRhythm::RestOrSpace {
                    is_rest: false,
                    rhythm: parsed_rhythm,
                };
                return Ok(result);
            }
        }
        
        // Parse push timing (leading apostrophes)
        let mut chars = input.chars().peekable();
        let mut push_count = 0;
        while chars.peek() == Some(&'\'') {
            push_count += 1;
            chars.next();
        }
        
        if push_count > 0 {
            result.timing_modifier = Some(match push_count {
                1 => TimingModifier::PushEighth,
                2 => TimingModifier::PushSixteenth,
                3 => TimingModifier::PushThirtySecond,
                _ => return Err(format!("Too many push apostrophes (max 3): {}", push_count)),
            });
        }
        
        let remaining: String = chars.collect();
        
        // Check for Lilypond duration syntax (ChordName_duration)
        if let Some(underscore_pos) = remaining.find('_') {
            let chord_part = &remaining[..underscore_pos];
            let rhythm_part = &remaining[underscore_pos + 1..];
            
            // Parse the chord using keyflow chord parser
            let mut chord_parser = crate::parsing::chord::parser::Parser::new();
            let capitalized_chord = Self::capitalize_chord_name(chord_part);
            let parsed_chord = chord_parser.parse(&capitalized_chord)
                .map_err(|e| format!("Failed to parse chord '{}': {:?}", capitalized_chord, e))?;
            result.chord = Some(parsed_chord);
            
            // Parse the rhythm
            let parsed_rhythm = ParsedRhythm::parse(rhythm_part)
                .map_err(|e| format!("Failed to parse rhythm '{}': {}", rhythm_part, e))?;
            result.rhythm = ChordRhythm::Duration(parsed_rhythm);
            
            return Ok(result);
        }
        
        // Split by whitespace to separate chord from potential slashes
        let tokens: Vec<&str> = remaining.split_whitespace().collect();
        
        if tokens.is_empty() {
            return Err("No chord found after push notation".to_string());
        }
        
        // First token is the chord
        let chord_token = tokens[0];
        
        // Check for pull timing (trailing apostrophes on chord)
        let (chord_name, pull_count) = {
            let mut name = chord_token;
            let mut count = 0;
            while name.ends_with('\'') {
                count += 1;
                name = &name[..name.len() - 1];
            }
            (name, count)
        };
        
        if pull_count > 0 {
            result.timing_modifier = Some(match pull_count {
                1 => TimingModifier::DelayEighth,
                2 => TimingModifier::DelaySixteenth,
                3 => TimingModifier::DelayThirtySecond,
                _ => return Err(format!("Too many pull apostrophes (max 3): {}", pull_count)),
            });
        }
        
        // Check for inline slash notation (no spaces between chord and slashes)
        // Examples: "G////", "C//", "D//", "Em//"
        // Only trigger if there are no spaces in the input
        if !remaining.contains(' ') {
            let mut inline_chord_name = String::new();
            let mut slashes = Vec::new();
            let mut in_slash_mode = false;
            
            for ch in remaining.chars() {
                if ch == '/' {
                    in_slash_mode = true;
                    slashes.push(SlashToken::Slash);
                } else if ch == '.' && in_slash_mode {
                    // Convert last slash to dotted slash
                    if let Some(last) = slashes.last_mut() {
                        if *last == SlashToken::Slash {
                            *last = SlashToken::DottedSlash;
                        }
                    }
                } else if !in_slash_mode {
                    inline_chord_name.push(ch);
                }
            }
            
            if !slashes.is_empty() {
                // Parse the chord using keyflow chord parser
                let mut chord_parser = crate::parsing::chord::parser::Parser::new();
                let capitalized_chord = Self::capitalize_chord_name(&inline_chord_name);
                let parsed_chord = chord_parser.parse(&capitalized_chord)
                    .map_err(|e| format!("Failed to parse chord '{}': {:?}", capitalized_chord, e))?;
                result.chord = Some(parsed_chord);
                
                result.rhythm = ChordRhythm::Slashes(slashes);
                return Ok(result);
            }
        }
        
        // Parse the chord using keyflow chord parser
        let mut chord_parser = crate::parsing::chord::parser::Parser::new();
        let capitalized_chord = Self::capitalize_chord_name(&chord_name);
        let parsed_chord = chord_parser.parse(&capitalized_chord)
            .map_err(|e| format!("Failed to parse chord '{}': {:?}", capitalized_chord, e))?;
        result.chord = Some(parsed_chord);
        
        // Check for slash notation in remaining tokens
        if tokens.len() > 1 {
            let mut slashes = Vec::new();
            
            for token in &tokens[1..] {
                // Parse slashes: /, //, ///, ////, /., etc.
                for ch in token.chars() {
                    match ch {
                        '/' => slashes.push(SlashToken::Slash),
                        '.' => {
                            // Convert last slash to dotted slash
                            if let Some(last) = slashes.last_mut() {
                                if *last == SlashToken::Slash {
                                    *last = SlashToken::DottedSlash;
                                }
                            }
                        }
                        _ => return Err(format!("Invalid character in slash notation: '{}'", ch)),
                    }
                }
            }
            
            if !slashes.is_empty() {
                result.rhythm = ChordRhythm::Slashes(slashes);
            }
        }
        
        Ok(result)
    }
    
    /// Calculate duration in beats based on time signature
    /// 
    /// For slash notation:
    /// - 4/4: each / = 1 beat (quarter note)
    /// - 6/8: each / = 0.5 beats (eighth note), /. = 1.5 beats (dotted quarter)
    pub fn duration_in_beats(&self, time_signature: (u8, u8)) -> f64 {
        match &self.rhythm {
            ChordRhythm::Default => {
                // Default is one full bar
                let (numerator, _denominator) = time_signature;
                numerator as f64
            }
            ChordRhythm::Slashes(slashes) => {
                let (_numerator, denominator) = time_signature;
                let beat_value = 4.0 / denominator as f64;
                
                slashes.iter().map(|s| match s {
                    SlashToken::Slash => beat_value,
                    SlashToken::DottedSlash => beat_value * 1.5,
                }).sum()
            }
            ChordRhythm::Duration(rhythm) => {
                rhythm.duration_in_beats()
            }
            ChordRhythm::RestOrSpace { rhythm, .. } => {
                rhythm.duration_in_beats()
            }
        }
    }
    
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_basic_chord() {
        let cr = ChordWithRhythm::parse("Cmaj7", Some((4, 4))).unwrap();
        assert!(cr.chord.is_some());
        assert_eq!(cr.chord.as_ref().unwrap().to_display_name(), "Cmaj7");
        assert!(matches!(cr.rhythm, ChordRhythm::Default));
        assert_eq!(cr.timing_modifier, None);
    }
    
    #[test]
    fn test_parse_chord_with_duration() {
        let cr = ChordWithRhythm::parse("Gmaj7_4", Some((4, 4))).unwrap();
        assert!(cr.chord.is_some());
        assert!(matches!(cr.rhythm, ChordRhythm::Duration(_)));
    }
    
    #[test]
    fn test_parse_slash_notation() {
        let cr = ChordWithRhythm::parse("G ////", Some((4, 4))).unwrap();
        assert!(cr.chord.is_some());
        
        if let ChordRhythm::Slashes(slashes) = &cr.rhythm {
            assert_eq!(slashes.len(), 4);
            assert_eq!(slashes[0], SlashToken::Slash);
        } else {
            panic!("Expected Slashes rhythm");
        }
    }
    
    #[test]
    fn test_parse_dotted_slashes() {
        let cr = ChordWithRhythm::parse("Em /.", Some((6, 8))).unwrap();
        assert!(cr.chord.is_some());
        
        if let ChordRhythm::Slashes(slashes) = &cr.rhythm {
            assert_eq!(slashes.len(), 1);
            assert_eq!(slashes[0], SlashToken::DottedSlash);
        } else {
            panic!("Expected Slashes rhythm");
        }
    }
    
    #[test]
    fn test_parse_push_timing() {
        let cr = ChordWithRhythm::parse("'C", Some((4, 4))).unwrap();
        assert_eq!(cr.timing_modifier, Some(TimingModifier::PushEighth));
        
        let cr = ChordWithRhythm::parse("''C", Some((4, 4))).unwrap();
        assert_eq!(cr.timing_modifier, Some(TimingModifier::PushSixteenth));
        
        let cr = ChordWithRhythm::parse("'''C", Some((4, 4))).unwrap();
        assert_eq!(cr.timing_modifier, Some(TimingModifier::PushThirtySecond));
    }
    
    #[test]
    fn test_parse_pull_timing() {
        let cr = ChordWithRhythm::parse("Cmaj'", Some((4, 4))).unwrap();
        assert_eq!(cr.timing_modifier, Some(TimingModifier::DelayEighth));
        
        let cr = ChordWithRhythm::parse("Cmaj''", Some((4, 4))).unwrap();
        assert_eq!(cr.timing_modifier, Some(TimingModifier::DelaySixteenth));
        
        let cr = ChordWithRhythm::parse("Cmaj'''", Some((4, 4))).unwrap();
        assert_eq!(cr.timing_modifier, Some(TimingModifier::DelayThirtySecond));
    }
    
    #[test]
    fn test_parse_rest() {
        let cr = ChordWithRhythm::parse("r4", Some((4, 4))).unwrap();
        assert!(cr.chord.is_none());
        
        if let ChordRhythm::RestOrSpace { is_rest, .. } = cr.rhythm {
            assert!(is_rest);
        } else {
            panic!("Expected RestOrSpace");
        }
    }
    
    #[test]
    fn test_parse_space() {
        let cr = ChordWithRhythm::parse("s2", Some((4, 4))).unwrap();
        assert!(cr.chord.is_none());
        
        if let ChordRhythm::RestOrSpace { is_rest, .. } = cr.rhythm {
            assert!(!is_rest);
        } else {
            panic!("Expected RestOrSpace");
        }
    }
    
    #[test]
    fn test_duration_in_beats() {
        // Default in 4/4
        let cr = ChordWithRhythm::parse("C", Some((4, 4))).unwrap();
        assert_eq!(cr.duration_in_beats((4, 4)), 4.0);
        
        // Slashes in 4/4
        let cr = ChordWithRhythm::parse("G ////", Some((4, 4))).unwrap();
        assert_eq!(cr.duration_in_beats((4, 4)), 4.0);
        
        let cr = ChordWithRhythm::parse("Em //", Some((4, 4))).unwrap();
        assert_eq!(cr.duration_in_beats((4, 4)), 2.0);
        
        // Slashes in 6/8
        let cr = ChordWithRhythm::parse("D ///", Some((6, 8))).unwrap();
        assert_eq!(cr.duration_in_beats((6, 8)), 1.5); // 3 eighth notes
        
        let cr = ChordWithRhythm::parse("A /.", Some((6, 8))).unwrap();
        assert_eq!(cr.duration_in_beats((6, 8)), 0.75); // Dotted quarter = 1.5 eighth notes
    }
    
    #[test]
    fn test_timing_modifier_offset() {
        assert_eq!(TimingModifier::PushEighth.offset_in_beats(), -0.5);
        assert_eq!(TimingModifier::PushSixteenth.offset_in_beats(), -0.25);
        assert_eq!(TimingModifier::DelayEighth.offset_in_beats(), 0.5);
        assert_eq!(TimingModifier::DelaySixteenth.offset_in_beats(), 0.25);
    }
    
    #[test]
    fn test_parse_inline_slash_notation() {
        // Test inline slash notation without spaces
        let cr = ChordWithRhythm::parse("G////", Some((4, 4))).unwrap();
        assert!(cr.chord.is_some());
        assert_eq!(cr.chord.as_ref().unwrap().to_display_name(), "G");
        
        if let ChordRhythm::Slashes(slashes) = &cr.rhythm {
            assert_eq!(slashes.len(), 4);
            assert_eq!(slashes[0], SlashToken::Slash);
        } else {
            panic!("Expected Slashes rhythm");
        }
        
        // Test with partial chord quality
        let cr = ChordWithRhythm::parse("Em//", Some((4, 4))).unwrap();
        assert!(cr.chord.is_some());
        assert_eq!(cr.chord.as_ref().unwrap().to_display_name(), "Em");
        
        if let ChordRhythm::Slashes(slashes) = &cr.rhythm {
            assert_eq!(slashes.len(), 2);
            assert_eq!(slashes[0], SlashToken::Slash);
        } else {
            panic!("Expected Slashes rhythm");
        }
    }
    
    #[test]
    fn test_parse_duration_notation() {
        // Test underscore duration notation
        let cr = ChordWithRhythm::parse("Cmaj7_4", Some((4, 4))).unwrap();
        assert!(cr.chord.is_some());
        assert_eq!(cr.chord.as_ref().unwrap().to_display_name(), "Cmaj7");
        
        if let ChordRhythm::Duration(rhythm) = &cr.rhythm {
            assert_eq!(rhythm.duration, 4);
            assert_eq!(rhythm.dots, 0);
        } else {
            panic!("Expected Duration rhythm");
        }
        
        // Test dotted duration
        let cr = ChordWithRhythm::parse("Dmin7_8.", Some((4, 4))).unwrap();
        assert!(cr.chord.is_some());
        assert_eq!(cr.chord.as_ref().unwrap().to_display_name(), "Dm7");
        
        if let ChordRhythm::Duration(rhythm) = &cr.rhythm {
            assert_eq!(rhythm.duration, 8);
            assert_eq!(rhythm.dots, 1);
        } else {
            panic!("Expected Duration rhythm");
        }
    }
    
    #[test]
    fn test_parse_combined_timing_and_rhythm() {
        // Test push timing with duration
        let cr = ChordWithRhythm::parse("'Gmaj_4", Some((4, 4))).unwrap();
        assert!(cr.chord.is_some());
        assert_eq!(cr.timing_modifier, Some(TimingModifier::PushEighth));
        
        if let ChordRhythm::Duration(rhythm) = &cr.rhythm {
            assert_eq!(rhythm.duration, 4);
        } else {
            panic!("Expected Duration rhythm");
        }
        
        // Test pull timing with slashes - use a chord format that keyflow parser supports
        let cr = ChordWithRhythm::parse("Cmaj' ////", Some((4, 4))).unwrap();
        assert!(cr.chord.is_some());
        assert_eq!(cr.timing_modifier, Some(TimingModifier::DelayEighth));
        
        if let ChordRhythm::Slashes(slashes) = &cr.rhythm {
            assert_eq!(slashes.len(), 4);
        } else {
            panic!("Expected Slashes rhythm");
        }
    }
    
}

/// Convert a total duration in beats to optimal tied Lilypond durations
/// 
/// This function takes a total duration in beats and converts it to a list of
/// tied Lilypond note values that sum to the total duration.
/// 
/// Examples:
/// - 4.0 beats in 4/4 → [1] (whole note)
/// - 3.5 beats in 4/4 → [2, 4.] (half note tied to dotted quarter)
/// - 2.5 beats in 4/4 → [2, 8] (half note tied to eighth)
/// - 1.5 beats in 4/4 → [4, 8] (quarter tied to eighth)
pub fn beats_to_tied_durations(total_beats: f64, time_signature: (u8, u8)) -> Vec<ParsedRhythm> {
    let (numerator, denominator) = time_signature;
    let mut remaining_beats = total_beats;
    let mut durations = Vec::new();
    
    // Define note values and their beat equivalents
    let note_values = [
        (1, 4.0),    // whole note
        (2, 2.0),    // half note
        (4, 1.0),    // quarter note
        (8, 0.5),    // eighth note
        (16, 0.25),  // sixteenth note
        (32, 0.125), // thirty-second note
    ];
    
    // Dotted note values
    let dotted_note_values = [
        (2, 3.0),    // dotted half note
        (4, 1.5),    // dotted quarter note
        (8, 0.75),   // dotted eighth note
        (16, 0.375), // dotted sixteenth note
    ];
    
    while remaining_beats > 0.0 {
        let mut found = false;
        
        // First try to find exact matches with dotted notes
        for &(duration, beats) in &dotted_note_values {
            if (remaining_beats - beats).abs() < 0.001 {
                durations.push(ParsedRhythm {
                    duration,
                    dots: 1,
                    multiplier: Some(1),
                    has_tie: false,
                    articulations: vec![],
                    original: String::new(),
                });
                remaining_beats = 0.0;
                found = true;
                break;
            }
        }
        
        if found {
            continue;
        }
        
        // Try to find the largest note that fits
        for &(duration, beats) in &note_values {
            if remaining_beats >= beats {
                durations.push(ParsedRhythm {
                    duration,
                    dots: 0,
                    multiplier: Some(1),
                    has_tie: false,
                    articulations: vec![],
                    original: String::new(),
                });
                remaining_beats -= beats;
                found = true;
                break;
            }
        }
        
        if !found {
            // If we can't find a perfect fit, use the smallest note
            // This handles fractional beats
            let smallest_note = note_values.last().unwrap();
            durations.push(ParsedRhythm {
                duration: smallest_note.0,
                dots: 0,
                multiplier: Some(1),
                has_tie: false,
                articulations: vec![],
                original: String::new(),
            });
            remaining_beats -= smallest_note.1;
        }
    }
    
    durations
}

#[cfg(test)]
mod duration_tests {
    use super::*;
    
    #[test]
    fn test_beats_to_tied_durations_whole_note() {
        let durations = beats_to_tied_durations(4.0, (4, 4));
        assert_eq!(durations.len(), 1);
        assert_eq!(durations[0].duration, 1);
        assert_eq!(durations[0].dots, 0);
    }
    
    #[test]
    fn test_beats_to_tied_durations_half_plus_dotted_quarter() {
        let durations = beats_to_tied_durations(3.5, (4, 4));
        assert_eq!(durations.len(), 2);
        assert_eq!(durations[0].duration, 2); // half note
        assert_eq!(durations[0].dots, 0);
        assert_eq!(durations[1].duration, 4); // quarter note
        assert_eq!(durations[1].dots, 1);     // dotted
    }
    
    #[test]
    fn test_beats_to_tied_durations_quarter_plus_eighth() {
        let durations = beats_to_tied_durations(1.5, (4, 4));
        assert_eq!(durations.len(), 1);
        assert_eq!(durations[0].duration, 4); // quarter note
        assert_eq!(durations[0].dots, 1);     // dotted (1.5 beats total)
    }
    
    #[test]
    fn test_beats_to_tied_durations_six_eight_time() {
        let durations = beats_to_tied_durations(3.0, (6, 8));
        assert_eq!(durations.len(), 1);
        assert_eq!(durations[0].duration, 2); // half note
        assert_eq!(durations[0].dots, 1);     // dotted (3 beats total)
    }
}
