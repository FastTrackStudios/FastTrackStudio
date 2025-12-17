//! Default Acoustic Guitar group configuration
//!
//! Acoustic Guitar group for acoustic guitar tracks.
//! Based on the fts-naming-parser config.json structure.

use super::super::Group;

/// Create the default Acoustic Guitar group
pub fn create_guitar_acoustic_group() -> Group {
    define_group! {
        name = "Guitar Acoustic",
        prefix = "AG",
        patterns = ["acoustic guitar", "acoustic", "ac", "a-gtr", "steel string", "nylon", "ag"],
        negative_patterns = ["bass", "electric"],
        parent_track = "GTR AG",
        arrangement_patterns = ["Fingerstyle", "Strum", "Pick", "Classical", "Folk", "Country", "Chug"],
        multi_mic_patterns = ["Close", "Room", "Stereo", "Mono", "Mic", "DI"],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_guitar_acoustic_group() {
        let guitar = create_guitar_acoustic_group();
        
        assert_eq!(guitar.name, "Guitar Acoustic");
        assert_eq!(guitar.prefix, "AG");
        assert!(guitar.patterns.contains(&"acoustic guitar".to_string()));
        assert!(guitar.patterns.contains(&"acoustic".to_string()));
        assert!(guitar.patterns.contains(&"ac".to_string()));
        assert!(guitar.patterns.contains(&"a-gtr".to_string()));
        assert!(guitar.patterns.contains(&"steel string".to_string()));
        assert!(guitar.patterns.contains(&"nylon".to_string()));
        assert!(guitar.patterns.contains(&"ag".to_string()));
        
        // Verify negative patterns
        assert!(guitar.negative_patterns.contains(&"bass".to_string()));
        assert!(guitar.negative_patterns.contains(&"electric".to_string()));
    }

    #[test]
    fn test_guitar_acoustic_arrangement_patterns() {
        let guitar = create_guitar_acoustic_group();
        
        // Verify arrangement patterns
        let arrangement_patterns = guitar.arrangement_patterns();
        assert!(arrangement_patterns.contains(&"Fingerstyle".to_string()));
        assert!(arrangement_patterns.contains(&"Strum".to_string()));
        assert!(arrangement_patterns.contains(&"Pick".to_string()));
        assert!(arrangement_patterns.contains(&"Classical".to_string()));
        assert!(arrangement_patterns.contains(&"Folk".to_string()));
        assert!(arrangement_patterns.contains(&"Country".to_string()));
        assert!(arrangement_patterns.contains(&"Chug".to_string()));
    }

    #[test]
    fn test_guitar_acoustic_multi_mic_patterns() {
        let guitar = create_guitar_acoustic_group();
        
        // Verify multi-mic patterns
        let multi_mic_patterns = guitar.multi_mic_patterns();
        assert!(multi_mic_patterns.contains(&"Close".to_string()));
        assert!(multi_mic_patterns.contains(&"Room".to_string()));
        assert!(multi_mic_patterns.contains(&"Stereo".to_string()));
        assert!(multi_mic_patterns.contains(&"Mono".to_string()));
        assert!(multi_mic_patterns.contains(&"Mic".to_string()));
        assert!(multi_mic_patterns.contains(&"DI".to_string()));
    }
}

