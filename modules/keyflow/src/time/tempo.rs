//! Tempo representation

/// Represents a tempo in beats per minute
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Tempo {
    pub bpm: u32,
}

impl Tempo {
    pub fn new(bpm: u32) -> Self {
        Self { bpm }
    }
    
    /// Parse tempo from string (e.g., "120bpm", "120")
    pub fn parse(s: &str) -> Option<Self> {
        let s = s.trim().to_lowercase();
        let s = s.strip_suffix("bpm").unwrap_or(&s).trim();
        
        s.parse::<u32>().ok().map(|bpm| Tempo { bpm })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_tempo() {
        let tempo = Tempo::new(120);
        assert_eq!(tempo.bpm, 120);
    }

    #[test]
    fn test_parse_with_bpm() {
        let tempo = Tempo::parse("120bpm").unwrap();
        assert_eq!(tempo.bpm, 120);
    }

    #[test]
    fn test_parse_without_bpm() {
        let tempo = Tempo::parse("120").unwrap();
        assert_eq!(tempo.bpm, 120);
    }

    #[test]
    fn test_parse_with_spaces() {
        let tempo = Tempo::parse("  120 bpm  ").unwrap();
        assert_eq!(tempo.bpm, 120);
    }

    #[test]
    fn test_parse_various_tempos() {
        assert_eq!(Tempo::parse("60").unwrap().bpm, 60);
        assert_eq!(Tempo::parse("80bpm").unwrap().bpm, 80);
        assert_eq!(Tempo::parse("100 BPM").unwrap().bpm, 100);
        assert_eq!(Tempo::parse("140").unwrap().bpm, 140);
        assert_eq!(Tempo::parse("200bpm").unwrap().bpm, 200);
    }

    #[test]
    fn test_parse_invalid() {
        assert!(Tempo::parse("invalid").is_none());
        assert!(Tempo::parse("").is_none());
        assert!(Tempo::parse("bpm").is_none());
    }

    #[test]
    fn test_tempo_copy() {
        let tempo1 = Tempo::new(120);
        let tempo2 = tempo1;
        assert_eq!(tempo1.bpm, tempo2.bpm);
    }
}
