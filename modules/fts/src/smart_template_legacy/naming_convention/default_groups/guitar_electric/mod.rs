//! Default Electric Guitar group configuration
//!
//! Electric Guitar group for electric guitar tracks.
//! Based on the fts-naming-parser config.json structure.

use super::super::Group;

/// Create the default Electric Guitar group
pub fn create_guitar_electric_group() -> Group {
    define_group! {
        name = "Guitar Electric",
        prefix = "GTR",
        patterns = ["guitar", "gtr", "gtr e", "electric guitar", "electric", "eg", "el", "e-gtr", "strat", "les paul", "tele", "sg"],
        negative_patterns = ["bass", "acoustic"],
        parent_track = "GTR ELEC",
        arrangement_patterns = ["Clean", "Crunch", "Distorted", "Lead", "Rhythm", "Power", "Chord"],
        multi_mic_patterns = ["DI", "Amp", "Cab", "Close", "Room"],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_guitar_electric_group() {
        let guitar = create_guitar_electric_group();
        
        assert_eq!(guitar.name, "Guitar Electric");
        assert_eq!(guitar.prefix, "GTR");
        assert!(guitar.patterns.contains(&"guitar".to_string()));
        assert!(guitar.patterns.contains(&"gtr".to_string()));
        assert!(guitar.patterns.contains(&"electric guitar".to_string()));
        assert!(guitar.patterns.contains(&"electric".to_string()));
        assert!(guitar.patterns.contains(&"eg".to_string()));
        assert!(guitar.patterns.contains(&"strat".to_string()));
        assert!(guitar.patterns.contains(&"les paul".to_string()));
        assert!(guitar.patterns.contains(&"tele".to_string()));
        assert!(guitar.patterns.contains(&"sg".to_string()));
        
        // Verify negative patterns
        assert!(guitar.negative_patterns.contains(&"bass".to_string()));
        assert!(guitar.negative_patterns.contains(&"acoustic".to_string()));
    }

    #[test]
    fn test_guitar_electric_arrangement_patterns() {
        let guitar = create_guitar_electric_group();
        
        // Verify arrangement patterns
        let arrangement_patterns = guitar.arrangement_patterns();
        assert!(arrangement_patterns.contains(&"Clean".to_string()));
        assert!(arrangement_patterns.contains(&"Crunch".to_string()));
        assert!(arrangement_patterns.contains(&"Distorted".to_string()));
        assert!(arrangement_patterns.contains(&"Lead".to_string()));
        assert!(arrangement_patterns.contains(&"Rhythm".to_string()));
        assert!(arrangement_patterns.contains(&"Power".to_string()));
        assert!(arrangement_patterns.contains(&"Chord".to_string()));
    }

    #[test]
    fn test_guitar_electric_multi_mic_patterns() {
        let guitar = create_guitar_electric_group();
        
        // Verify multi-mic patterns
        let multi_mic_patterns = guitar.multi_mic_patterns();
        assert!(multi_mic_patterns.contains(&"DI".to_string()));
        assert!(multi_mic_patterns.contains(&"Amp".to_string()));
        assert!(multi_mic_patterns.contains(&"Cab".to_string()));
        assert!(multi_mic_patterns.contains(&"Close".to_string()));
        assert!(multi_mic_patterns.contains(&"Room".to_string()));
    }
}

