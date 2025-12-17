//! Default Synths group configuration
//!
//! Synths group for synthesizer tracks.
//! Based on the fts-naming-parser config.json structure.

use super::super::Group;

/// Create the default Synths group
pub fn create_synths_group() -> Group {
    define_group! {
        name = "Synths",
        prefix = "SY",
        patterns = ["synth", "synths", "nord", "casio", "fa06", "charang", "briteness", "moog", "prophet", "juno", "dx7", "minimoog", "sub37", "analog", "digital", "vst", "plugin"],
        negative_patterns = [],
        parent_track = "SYNTHS",
        arrangement_patterns = ["Arp", "Sequence", "Pad", "Lead", "Stab", "Sweep", "Filter", "Cutoff", "Resonance", "LFO", "Envelope", "Attack", "Decay", "Sustain", "Release"],
        multi_mic_patterns = ["DI", "Line", "Analog", "Digital", "Mono", "Stereo"],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_synths_group() {
        let synths = create_synths_group();
        
        assert_eq!(synths.name, "Synths");
        assert_eq!(synths.prefix, "SY");
        assert!(synths.patterns.contains(&"synth".to_string()));
        assert!(synths.patterns.contains(&"synths".to_string()));
        assert!(synths.patterns.contains(&"nord".to_string()));
        assert!(synths.patterns.contains(&"moog".to_string()));
        assert!(synths.patterns.contains(&"prophet".to_string()));
        assert!(synths.patterns.contains(&"juno".to_string()));
        assert!(synths.patterns.contains(&"vst".to_string()));
        assert!(synths.patterns.contains(&"plugin".to_string()));
    }

    #[test]
    fn test_synths_arrangement_patterns() {
        let synths = create_synths_group();
        
        // Verify arrangement patterns
        let arrangement_patterns = synths.arrangement_patterns();
        assert!(arrangement_patterns.contains(&"Arp".to_string()));
        assert!(arrangement_patterns.contains(&"Sequence".to_string()));
        assert!(arrangement_patterns.contains(&"Pad".to_string()));
        assert!(arrangement_patterns.contains(&"Lead".to_string()));
        assert!(arrangement_patterns.contains(&"Stab".to_string()));
        assert!(arrangement_patterns.contains(&"Sweep".to_string()));
    }

    #[test]
    fn test_synths_multi_mic_patterns() {
        let synths = create_synths_group();
        
        // Verify multi-mic patterns
        let multi_mic_patterns = synths.multi_mic_patterns();
        assert!(multi_mic_patterns.contains(&"DI".to_string()));
        assert!(multi_mic_patterns.contains(&"Line".to_string()));
        assert!(multi_mic_patterns.contains(&"Analog".to_string()));
        assert!(multi_mic_patterns.contains(&"Digital".to_string()));
        assert!(multi_mic_patterns.contains(&"Mono".to_string()));
        assert!(multi_mic_patterns.contains(&"Stereo".to_string()));
    }
}

