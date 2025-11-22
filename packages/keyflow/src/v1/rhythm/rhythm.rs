// Rhythm Parser
// 
// Parses Lilypond-style rhythm notation:
// - Basic durations: 1, 2, 4, 8, 16, 32, 64, 128
// - Dotted durations: 4., 8.., 16...
// - Multipliers: 1*8, 4*3
// - Ties: ~ (for connecting rhythms)
//
// Examples:
// - "4" -> Quarter note
// - "8." -> Dotted eighth note
// - "16.." -> Double-dotted sixteenth
// - "1*4" -> Whole note held for 4 bars

use std::fmt;

/// Articulation types
#[derive(Debug, Clone, PartialEq)]
pub enum Articulation {
    Accent,          // -> or \accent
    Staccato,        // -. or \staccato
    Tenuto,          // -- or \tenuto
    Marcato,         // -^ or \marcato
    Staccatissimo,   // -! or \staccatissimo
    Portato,         // -_ or \portato
    Espressivo,      // \espressivo
    Fermata,         // \fermata
}

impl Articulation {
    /// Convert to Lilypond notation
    pub fn to_lilypond(&self) -> &str {
        match self {
            Articulation::Accent => "->",
            Articulation::Staccato => "-.",
            Articulation::Tenuto => "--",
            Articulation::Marcato => "-^",
            Articulation::Staccatissimo => "-!",
            Articulation::Portato => "-_",
            Articulation::Espressivo => "\\espressivo",
            Articulation::Fermata => "\\fermata",
        }
    }
}

/// Represents a parsed rhythm value
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedRhythm {
    /// The base duration (1=whole, 2=half, 4=quarter, etc.)
    pub duration: u8,
    
    /// Number of dots (0, 1, 2, or 3)
    pub dots: u8,
    
    /// Multiplier (e.g., 1*8 means hold for 8 bars)
    pub multiplier: Option<u32>,
    
    /// Whether this rhythm has a tie (~) attached
    pub has_tie: bool,
    
    /// Articulations applied to this rhythm
    pub articulations: Vec<Articulation>,
    
    /// Original input string
    pub original: String,
}

impl ParsedRhythm {
    /// Create a new rhythm with default values
    pub fn new(duration: u8) -> Self {
        Self {
            duration,
            dots: 0,
            multiplier: None,
            has_tie: false,
            articulations: Vec::new(),
            original: String::new(),
        }
    }
    
    /// Calculate the actual duration in quarter note beats
    /// 
    /// Examples:
    /// - 4 (quarter note) = 1.0
    /// - 4. (dotted quarter) = 1.5
    /// - 8 (eighth note) = 0.5
    /// - 1*2 (whole note × 2) = 8.0
    pub fn duration_in_beats(&self) -> f64 {
        let base = 4.0 / self.duration as f64;
        
        // Apply dots (each dot adds half of the previous value)
        let mut total = base;
        let mut dot_value = base / 2.0;
        for _ in 0..self.dots {
            total += dot_value;
            dot_value /= 2.0;
        }
        
        // Apply multiplier
        if let Some(mult) = self.multiplier {
            total *= mult as f64;
        }
        
        total
    }
    
    /// Convert to MusicalDuration format (measure.beats.subdivision)
    /// 
    /// This method is time signature aware and will properly wrap around
    /// to increment measures when the duration exceeds the time signature.
    /// 
    /// Examples:
    /// - In 4/4: 4 (quarter note) = 0.1.00
    /// - In 4/4: 1 (whole note) = 0.4.00  
    /// - In 4/4: 1*2 (whole note × 2) = 1.0.00 (wraps to next measure)
    /// - In 6/8: 4 (quarter note) = 0.2.00 (quarter note = 2 beats in 6/8)
    pub fn to_musical_duration(&self, time_signature: (u8, u8)) -> crate::chart::parser::MusicalDuration {
        let (numerator, denominator) = time_signature;
        
        // Convert the duration to beats based on the time signature
        // The denominator tells us what note gets the beat:
        // - 4/4: quarter note gets the beat (denominator = 4)
        // - 6/8: eighth note gets the beat (denominator = 8)
        // - 3/4: quarter note gets the beat (denominator = 4)
        
        let quarter_note_beats = self.duration_in_beats();
        let beat_unit_beats = quarter_note_beats * (denominator as f64 / 4.0);
        
        crate::chart::parser::MusicalDuration::from_beats(beat_unit_beats, time_signature)
    }
    
    /// Parse a rhythm string
    /// 
    /// Supports:
    /// - Basic: "4", "8", "16"
    /// - Dotted: "4.", "8..", "16..."
    /// - Multiplied: "1*8", "4*3"
    /// - With tie: "4~", "8.~"
    pub fn parse(input: &str) -> Result<Self, String> {
        if input.is_empty() {
            return Err("Empty rhythm string".to_string());
        }
        
        let mut chars = input.trim().chars().peekable();
        let mut rhythm = ParsedRhythm::new(4); // Default quarter note
        rhythm.original = input.to_string();
        
        // Parse duration number
        let mut duration_str = String::new();
        while let Some(&ch) = chars.peek() {
            if ch.is_ascii_digit() {
                duration_str.push(ch);
                chars.next();
            } else {
                break;
            }
        }
        
        if duration_str.is_empty() {
            return Err(format!("Invalid rhythm: '{}' - must start with a number", input));
        }
        
        rhythm.duration = duration_str.parse::<u8>()
            .map_err(|_| format!("Invalid duration number: '{}'", duration_str))?;
        
        // Validate duration (must be power of 2: 1, 2, 4, 8, 16, 32, 64, 128)
        if !matches!(rhythm.duration, 1 | 2 | 4 | 8 | 16 | 32 | 64 | 128) {
            return Err(format!("Invalid duration: {} (must be 1, 2, 4, 8, 16, 32, 64, or 128)", rhythm.duration));
        }
        
        // Parse dots
        while let Some(&ch) = chars.peek() {
            if ch == '.' {
                rhythm.dots += 1;
                chars.next();
            } else {
                break;
            }
        }
        
        // Parse multiplier (e.g., *8)
        if let Some(&ch) = chars.peek() {
            if ch == '*' {
                chars.next();
                let mut mult_str = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_ascii_digit() {
                        mult_str.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                
                if mult_str.is_empty() {
                    return Err(format!("Invalid multiplier in rhythm: '{}'", input));
                }
                
                rhythm.multiplier = Some(mult_str.parse::<u32>()
                    .map_err(|_| format!("Invalid multiplier number: '{}'", mult_str))?);
            }
        }
        
        // Parse tie (~)
        if let Some(&ch) = chars.peek() {
            if ch == '~' {
                rhythm.has_tie = true;
                chars.next();
            }
        }
        
        // Parse articulations (shorthand or long form)
        while chars.peek().is_some() {
            let remaining: String = chars.clone().collect();
            
            // Try shorthand first (2-char patterns)
            if remaining.starts_with("->") {
                rhythm.articulations.push(Articulation::Accent);
                chars.next();
                chars.next();
            } else if remaining.starts_with("-.") {
                rhythm.articulations.push(Articulation::Staccato);
                chars.next();
                chars.next();
            } else if remaining.starts_with("--") {
                rhythm.articulations.push(Articulation::Tenuto);
                chars.next();
                chars.next();
            } else if remaining.starts_with("-^") {
                rhythm.articulations.push(Articulation::Marcato);
                chars.next();
                chars.next();
            } else if remaining.starts_with("-!") {
                rhythm.articulations.push(Articulation::Staccatissimo);
                chars.next();
                chars.next();
            } else if remaining.starts_with("-_") {
                rhythm.articulations.push(Articulation::Portato);
                chars.next();
                chars.next();
            } else if remaining.starts_with("\\espressivo") {
                rhythm.articulations.push(Articulation::Espressivo);
                for _ in 0..11 {
                    chars.next();
                }
            } else if remaining.starts_with("\\fermata") {
                rhythm.articulations.push(Articulation::Fermata);
                for _ in 0..8 {
                    chars.next();
                }
            } else {
                return Err(format!("Unexpected characters at end of rhythm: '{}'", remaining));
            }
        }
        
        Ok(rhythm)
    }
}

impl fmt::Display for ParsedRhythm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.duration)?;
        for _ in 0..self.dots {
            write!(f, ".")?;
        }
        if let Some(mult) = self.multiplier {
            write!(f, "*{}", mult)?;
        }
        if self.has_tie {
            write!(f, "~")?;
        }
        for art in &self.articulations {
            write!(f, "{}", art.to_lilypond())?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_basic_durations() {
        let r = ParsedRhythm::parse("4").unwrap();
        assert_eq!(r.duration, 4);
        assert_eq!(r.dots, 0);
        assert_eq!(r.multiplier, None);
        assert!(!r.has_tie);
        
        let r = ParsedRhythm::parse("8").unwrap();
        assert_eq!(r.duration, 8);
        
        let r = ParsedRhythm::parse("1").unwrap();
        assert_eq!(r.duration, 1);
        
        let r = ParsedRhythm::parse("16").unwrap();
        assert_eq!(r.duration, 16);
    }
    
    #[test]
    fn test_parse_dotted() {
        let r = ParsedRhythm::parse("4.").unwrap();
        assert_eq!(r.duration, 4);
        assert_eq!(r.dots, 1);
        
        let r = ParsedRhythm::parse("8..").unwrap();
        assert_eq!(r.duration, 8);
        assert_eq!(r.dots, 2);
        
        let r = ParsedRhythm::parse("16...").unwrap();
        assert_eq!(r.duration, 16);
        assert_eq!(r.dots, 3);
    }
    
    #[test]
    fn test_parse_multiplier() {
        let r = ParsedRhythm::parse("1*8").unwrap();
        assert_eq!(r.duration, 1);
        assert_eq!(r.multiplier, Some(8));
        
        let r = ParsedRhythm::parse("4*3").unwrap();
        assert_eq!(r.duration, 4);
        assert_eq!(r.multiplier, Some(3));
    }
    
    #[test]
    fn test_parse_with_tie() {
        let r = ParsedRhythm::parse("4~").unwrap();
        assert_eq!(r.duration, 4);
        assert!(r.has_tie);
        
        let r = ParsedRhythm::parse("8.~").unwrap();
        assert_eq!(r.duration, 8);
        assert_eq!(r.dots, 1);
        assert!(r.has_tie);
    }
    
    #[test]
    fn test_parse_complex() {
        let r = ParsedRhythm::parse("4.*2~").unwrap();
        assert_eq!(r.duration, 4);
        assert_eq!(r.dots, 1);
        assert_eq!(r.multiplier, Some(2));
        assert!(r.has_tie);
    }
    
    #[test]
    fn test_duration_in_beats() {
        // Quarter note = 1 beat
        let r = ParsedRhythm::parse("4").unwrap();
        assert_eq!(r.duration_in_beats(), 1.0);
        
        // Dotted quarter = 1.5 beats
        let r = ParsedRhythm::parse("4.").unwrap();
        assert_eq!(r.duration_in_beats(), 1.5);
        
        // Eighth note = 0.5 beats
        let r = ParsedRhythm::parse("8").unwrap();
        assert_eq!(r.duration_in_beats(), 0.5);
        
        // Whole note = 4 beats
        let r = ParsedRhythm::parse("1").unwrap();
        assert_eq!(r.duration_in_beats(), 4.0);
        
        // Whole note × 2 = 8 beats
        let r = ParsedRhythm::parse("1*2").unwrap();
        assert_eq!(r.duration_in_beats(), 8.0);
        
        // Half note = 2 beats
        let r = ParsedRhythm::parse("2").unwrap();
        assert_eq!(r.duration_in_beats(), 2.0);
    }
    
    #[test]
    fn test_invalid_rhythms() {
        assert!(ParsedRhythm::parse("").is_err());
        assert!(ParsedRhythm::parse("abc").is_err());
        assert!(ParsedRhythm::parse("3").is_err()); // Not a power of 2
        assert!(ParsedRhythm::parse("4*").is_err()); // Missing multiplier value
        assert!(ParsedRhythm::parse("4x").is_err()); // Invalid character
    }
    
    #[test]
    fn test_display() {
        assert_eq!(ParsedRhythm::parse("4").unwrap().to_string(), "4");
        assert_eq!(ParsedRhythm::parse("8.").unwrap().to_string(), "8.");
        assert_eq!(ParsedRhythm::parse("1*8").unwrap().to_string(), "1*8");
        assert_eq!(ParsedRhythm::parse("4~").unwrap().to_string(), "4~");
        assert_eq!(ParsedRhythm::parse("4.*2~").unwrap().to_string(), "4.*2~");
    }
    
    #[test]
    fn test_parse_articulations_shorthand() {
        // Single articulations
        let r = ParsedRhythm::parse("4->").unwrap();
        assert_eq!(r.articulations.len(), 1);
        assert_eq!(r.articulations[0], Articulation::Accent);
        
        let r = ParsedRhythm::parse("8-.").unwrap();
        assert_eq!(r.articulations.len(), 1);
        assert_eq!(r.articulations[0], Articulation::Staccato);
        
        let r = ParsedRhythm::parse("4--").unwrap();
        assert_eq!(r.articulations.len(), 1);
        assert_eq!(r.articulations[0], Articulation::Tenuto);
        
        let r = ParsedRhythm::parse("4-^").unwrap();
        assert_eq!(r.articulations.len(), 1);
        assert_eq!(r.articulations[0], Articulation::Marcato);
        
        let r = ParsedRhythm::parse("8-!").unwrap();
        assert_eq!(r.articulations.len(), 1);
        assert_eq!(r.articulations[0], Articulation::Staccatissimo);
        
        let r = ParsedRhythm::parse("4-_").unwrap();
        assert_eq!(r.articulations.len(), 1);
        assert_eq!(r.articulations[0], Articulation::Portato);
    }
    
    #[test]
    fn test_parse_articulations_long_form() {
        let r = ParsedRhythm::parse("4\\espressivo").unwrap();
        assert_eq!(r.articulations.len(), 1);
        assert_eq!(r.articulations[0], Articulation::Espressivo);
        
        let r = ParsedRhythm::parse("4\\fermata").unwrap();
        assert_eq!(r.articulations.len(), 1);
        assert_eq!(r.articulations[0], Articulation::Fermata);
    }
    
    #[test]
    fn test_parse_combined_articulations() {
        // Accent + staccato
        let r = ParsedRhythm::parse("4->-.").unwrap();
        assert_eq!(r.articulations.len(), 2);
        assert_eq!(r.articulations[0], Articulation::Accent);
        assert_eq!(r.articulations[1], Articulation::Staccato);
        
        // Dotted + tie + articulation
        let r = ParsedRhythm::parse("4.~->").unwrap();
        assert_eq!(r.dots, 1);
        assert!(r.has_tie);
        assert_eq!(r.articulations.len(), 1);
        assert_eq!(r.articulations[0], Articulation::Accent);
    }
    
    #[test]
    fn test_articulation_display() {
        assert_eq!(ParsedRhythm::parse("4->").unwrap().to_string(), "4->");
        assert_eq!(ParsedRhythm::parse("8-.").unwrap().to_string(), "8-.");
        assert_eq!(ParsedRhythm::parse("4->-.").unwrap().to_string(), "4->-.");
        assert_eq!(ParsedRhythm::parse("4\\fermata").unwrap().to_string(), "4\\fermata");
    }
    
    #[test]
    fn test_to_musical_duration_4_4() {
        // Test in 4/4 time signature
        let time_sig = (4, 4);
        
        // Quarter note = 1 beat = 0.1.00
        let r = ParsedRhythm::parse("4").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "0.1.00");
        
        // Half note = 2 beats = 0.2.00
        let r = ParsedRhythm::parse("2").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "0.2.00");
        
        // Whole note = 4 beats = 1.0.00 (wraps to next measure in 4/4)
        let r = ParsedRhythm::parse("1").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "1.0.00");
        
        // Eighth note = 0.5 beats = 0.0.50
        let r = ParsedRhythm::parse("8").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "0.0.50");
        
        // Dotted quarter = 1.5 beats = 0.1.50
        let r = ParsedRhythm::parse("4.").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "0.1.50");
        
        // Whole note × 2 = 8 beats = 2.0.00 (2 full measures)
        let r = ParsedRhythm::parse("1*2").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "2.0.00");
    }
    
    #[test]
    fn test_to_musical_duration_6_8() {
        // Test in 6/8 time signature
        let time_sig = (6, 8);
        
        // Quarter note = 2 beats in 6/8 = 0.2.00
        let r = ParsedRhythm::parse("4").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "0.2.00");
        
        // Eighth note = 1 beat in 6/8 = 0.1.00
        let r = ParsedRhythm::parse("8").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "0.1.00");
        
        // Dotted quarter = 3 beats in 6/8 = 0.3.00
        let r = ParsedRhythm::parse("4.").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "0.3.00");
        
        // Whole note = 8 beats in 6/8 = 1.2.00 (wraps to next measure)
        let r = ParsedRhythm::parse("1").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "1.2.00");
    }
    
    #[test]
    fn test_to_musical_duration_3_4() {
        // Test in 3/4 time signature
        let time_sig = (3, 4);
        
        // Quarter note = 1 beat = 0.1.00
        let r = ParsedRhythm::parse("4").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "0.1.00");
        
        // Half note = 2 beats = 0.2.00
        let r = ParsedRhythm::parse("2").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "0.2.00");
        
        // Dotted half = 3 beats = 1.0.00 (wraps to next measure in 3/4)
        let r = ParsedRhythm::parse("2.").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "1.0.00");
        
        // Whole note = 4 beats = 1.1.00 (wraps to next measure)
        let r = ParsedRhythm::parse("1").unwrap();
        let md = r.to_musical_duration(time_sig);
        assert_eq!(md.to_string(), "1.1.00");
    }
}