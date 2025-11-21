// Time Signature Parser
//
// Parses time signature notation like:
// - "4/4"
// - "6/8"
// - "3/4"
// - "7/8"

use std::fmt;

/// Represents a time signature
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct TimeSignature {
    pub numerator: u8,     // Top number (beats per measure)
    pub denominator: u8,   // Bottom number (note value that gets the beat)
}

impl TimeSignature {
    /// Parse a time signature string
    /// 
    /// Accepts:
    /// - "4/4"
    /// - "6/8"
    /// - "3/4"
    /// - "7/8"
    pub fn parse(input: &str) -> Result<Self, String> {
        let input = input.trim();
        
        // Split by '/'
        let parts: Vec<&str> = input.split('/').collect();
        
        if parts.len() != 2 {
            return Err(format!("Invalid time signature format: '{}'", input));
        }
        
        // Parse numerator and denominator
        let numerator = parts[0].trim().parse::<u8>()
            .map_err(|_| format!("Invalid numerator: '{}'", parts[0]))?;
        
        let denominator = parts[1].trim().parse::<u8>()
            .map_err(|_| format!("Invalid denominator: '{}'", parts[1]))?;
        
        // Validate numerator (1-32)
        if numerator < 1 || numerator > 32 {
            return Err(format!("Numerator {} is out of range (1-32)", numerator));
        }
        
        // Validate denominator is a power of 2 (1, 2, 4, 8, 16, 32)
        if !matches!(denominator, 1 | 2 | 4 | 8 | 16 | 32) {
            return Err(format!("Denominator {} must be a power of 2 (1, 2, 4, 8, 16, 32)", denominator));
        }
        
        Ok(TimeSignature {
            numerator,
            denominator,
        })
    }
    
    /// Create common time signatures
    pub fn common_time() -> Self {
        TimeSignature { numerator: 4, denominator: 4 }
    }
    
    pub fn cut_time() -> Self {
        TimeSignature { numerator: 2, denominator: 2 }
    }
    
    pub fn waltz() -> Self {
        TimeSignature { numerator: 3, denominator: 4 }
    }
    
    pub fn compound_duple() -> Self {
        TimeSignature { numerator: 6, denominator: 8 }
    }
    
    /// Check if this is a compound time signature (numerator divisible by 3)
    pub fn is_compound(&self) -> bool {
        self.numerator % 3 == 0 && self.numerator > 3
    }
    
    /// Get the number of beats per measure
    pub fn beats_per_measure(&self) -> u8 {
        if self.is_compound() {
            self.numerator / 3
        } else {
            self.numerator
        }
    }
    
    /// Convert to Lilypond time signature
    /// 
    /// Output: \time 4/4
    pub fn to_lilypond(&self) -> String {
        format!("\\time {}/{}", self.numerator, self.denominator)
    }
    
    /// Get as tuple (for convenience)
    pub fn as_tuple(&self) -> (u8, u8) {
        (self.numerator, self.denominator)
    }
}

impl fmt::Display for TimeSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_common_time() {
        let ts = TimeSignature::parse("4/4").unwrap();
        assert_eq!(ts.numerator, 4);
        assert_eq!(ts.denominator, 4);
    }
    
    #[test]
    fn test_parse_six_eight() {
        let ts = TimeSignature::parse("6/8").unwrap();
        assert_eq!(ts.numerator, 6);
        assert_eq!(ts.denominator, 8);
    }
    
    #[test]
    fn test_parse_with_spaces() {
        let ts = TimeSignature::parse(" 3 / 4 ").unwrap();
        assert_eq!(ts.numerator, 3);
        assert_eq!(ts.denominator, 4);
    }
    
    #[test]
    fn test_parse_odd_meter() {
        let ts = TimeSignature::parse("7/8").unwrap();
        assert_eq!(ts.numerator, 7);
        assert_eq!(ts.denominator, 8);
    }
    
    #[test]
    fn test_parse_invalid_format() {
        assert!(TimeSignature::parse("4").is_err());
        assert!(TimeSignature::parse("4-4").is_err());
    }
    
    #[test]
    fn test_parse_invalid_denominator() {
        assert!(TimeSignature::parse("4/3").is_err());
        assert!(TimeSignature::parse("4/5").is_err());
    }
    
    #[test]
    fn test_common_time_signatures() {
        assert_eq!(TimeSignature::common_time().as_tuple(), (4, 4));
        assert_eq!(TimeSignature::cut_time().as_tuple(), (2, 2));
        assert_eq!(TimeSignature::waltz().as_tuple(), (3, 4));
        assert_eq!(TimeSignature::compound_duple().as_tuple(), (6, 8));
    }
    
    #[test]
    fn test_is_compound() {
        assert!(!TimeSignature::parse("4/4").unwrap().is_compound());
        assert!(TimeSignature::parse("6/8").unwrap().is_compound());
        assert!(TimeSignature::parse("9/8").unwrap().is_compound());
        assert!(TimeSignature::parse("12/8").unwrap().is_compound());
        assert!(!TimeSignature::parse("3/4").unwrap().is_compound());
    }
    
    #[test]
    fn test_beats_per_measure() {
        assert_eq!(TimeSignature::parse("4/4").unwrap().beats_per_measure(), 4);
        assert_eq!(TimeSignature::parse("6/8").unwrap().beats_per_measure(), 2);
        assert_eq!(TimeSignature::parse("9/8").unwrap().beats_per_measure(), 3);
        assert_eq!(TimeSignature::parse("3/4").unwrap().beats_per_measure(), 3);
    }
    
    #[test]
    fn test_to_lilypond() {
        let ts = TimeSignature::parse("6/8").unwrap();
        assert_eq!(ts.to_lilypond(), "\\time 6/8");
    }
    
    #[test]
    fn test_display() {
        let ts = TimeSignature::parse("7/8").unwrap();
        assert_eq!(ts.to_string(), "7/8");
    }
}
