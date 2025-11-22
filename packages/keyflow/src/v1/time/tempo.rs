// Tempo (BPM) Parser
//
// Parses tempo notation like:
// - "120bpm"
// - "68 bpm"
// - "90"

use std::fmt;

/// Represents a tempo in beats per minute
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Tempo {
    pub bpm: u16,
    pub original: String,
}

impl Tempo {
    /// Parse a tempo string
    /// 
    /// Accepts:
    /// - "120bpm"
    /// - "68 bpm"
    /// - "90" (assumes bpm)
    pub fn parse(input: &str) -> Result<Self, String> {
        let input = input.trim().to_lowercase();
        
        // Remove "bpm" suffix if present
        let number_str = input
            .replace("bpm", "")
            .replace(" ", "")
            .trim()
            .to_string();
        
        // Parse the number
        let bpm = number_str.parse::<u16>()
            .map_err(|_| format!("Invalid tempo: '{}'", input))?;
        
        // Validate reasonable tempo range (20-300 bpm)
        if bpm < 20 || bpm > 300 {
            return Err(format!("Tempo {} is out of range (20-300 bpm)", bpm));
        }
        
        Ok(Tempo {
            bpm,
            original: input.to_string(),
        })
    }
    
    /// Convert to Lilypond tempo marking
    /// 
    /// Output: \tempo 4 = 120
    pub fn to_lilypond(&self, beat_unit: u8) -> String {
        format!("\\tempo {} = {}", beat_unit, self.bpm)
    }
}

impl fmt::Display for Tempo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}bpm", self.bpm)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_tempo_with_bpm() {
        let tempo = Tempo::parse("120bpm").unwrap();
        assert_eq!(tempo.bpm, 120);
    }
    
    #[test]
    fn test_parse_tempo_with_space() {
        let tempo = Tempo::parse("68 bpm").unwrap();
        assert_eq!(tempo.bpm, 68);
    }
    
    #[test]
    fn test_parse_tempo_number_only() {
        let tempo = Tempo::parse("90").unwrap();
        assert_eq!(tempo.bpm, 90);
    }
    
    #[test]
    fn test_parse_tempo_uppercase() {
        let tempo = Tempo::parse("120BPM").unwrap();
        assert_eq!(tempo.bpm, 120);
    }
    
    #[test]
    fn test_parse_tempo_out_of_range() {
        assert!(Tempo::parse("10").is_err());
        assert!(Tempo::parse("500").is_err());
    }
    
    #[test]
    fn test_tempo_to_lilypond() {
        let tempo = Tempo::parse("120bpm").unwrap();
        assert_eq!(tempo.to_lilypond(4), "\\tempo 4 = 120");
        assert_eq!(tempo.to_lilypond(8), "\\tempo 8 = 120");
    }
    
    #[test]
    fn test_tempo_display() {
        let tempo = Tempo::parse("68bpm").unwrap();
        assert_eq!(tempo.to_string(), "68bpm");
    }
}
