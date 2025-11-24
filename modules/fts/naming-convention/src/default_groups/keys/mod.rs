//! Default Keys group configuration
//!
//! Keys group includes Piano, Electric Piano, Organ, and Clavinet as sub-types.
//! Each sub-type has its own arrangement patterns and multi-mic patterns.

use crate::{FullGroup, define_group};

/// Create the default Keys group with sub-types
pub fn create_keys_group() -> FullGroup {
    let mut keys = define_group! {
        name = "Keys",
        prefix = "K",
        patterns = ["keys", "piano", "pno", "nord", "rhodes", "wurli", "keyboard", "organ", "hammond", "b3"],
        negative_patterns = [],
        parent_track = "Keys",
        arrangement_patterns = ["Chord", "Arpeggio", "Scale", "Run", "Glissando", "Trill", "Tremolo", "Sustain", "Staccato", "Legato"],
        multi_mic_patterns = ["DI", "Amp", "Close", "Room", "Stereo", "Mono"],
        children = [
            define_group! {
                name = "Piano",
                prefix = "Piano",
                patterns = ["piano", "pno", "grand", "upright"],
                arrangement_patterns = ["Classical", "Jazz", "Pop", "Ballad", "Stride", "Ragtime"],
                multi_mic_patterns = ["Close", "Room", "Stereo", "Mono", "Lid"],
            },
            define_group! {
                name = "Electric",
                prefix = "EP",
                patterns = ["electric", "rhodes", "wurlitzer", "ep"],
                arrangement_patterns = ["Clean", "Chorus", "Tremolo", "Phaser", "Vintage"],
                multi_mic_patterns = ["DI", "Amp", "Close", "Room"],
            },
            define_group! {
                name = "Organ",
                prefix = "Organ",
                patterns = ["organ", "hammond", "b3", "leslie"],
                arrangement_patterns = ["Drawbar", "Percussion", "Leslie", "Rotary", "Gospel", "Rock"],
                multi_mic_patterns = ["DI", "Leslie", "Close", "Room"],
            },
            define_group! {
                name = "Clav",
                prefix = "Clav",
                patterns = ["clav", "clavinet", "funk"],
                arrangement_patterns = ["Funk", "Wah", "Mute", "Staccato", "Percussive"],
                multi_mic_patterns = ["DI", "Amp", "Close"],
            },
        ],
    };
    
    keys
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_keys_group() {
        let keys = create_keys_group();
        
        assert_eq!(keys.name, "Keys");
        assert_eq!(keys.prefix, "K");
        assert!(keys.patterns.contains(&"keys".to_string()));
        assert!(keys.patterns.contains(&"piano".to_string()));
        assert!(keys.patterns.contains(&"pno".to_string()));
        assert!(keys.patterns.contains(&"nord".to_string()));
        assert!(keys.patterns.contains(&"rhodes".to_string()));
        assert!(keys.patterns.contains(&"wurli".to_string()));
        assert!(keys.patterns.contains(&"keyboard".to_string()));
        assert!(keys.patterns.contains(&"organ".to_string()));
        assert!(keys.patterns.contains(&"hammond".to_string()));
        assert!(keys.patterns.contains(&"b3".to_string()));
    }

    #[test]
    fn test_keys_arrangement_patterns() {
        let keys = create_keys_group();
        
        // Verify arrangement patterns
        let arrangement_patterns = keys.arrangement_patterns();
        assert!(arrangement_patterns.contains(&"Chord".to_string()));
        assert!(arrangement_patterns.contains(&"Arpeggio".to_string()));
        assert!(arrangement_patterns.contains(&"Scale".to_string()));
        assert!(arrangement_patterns.contains(&"Run".to_string()));
        assert!(arrangement_patterns.contains(&"Glissando".to_string()));
        assert!(arrangement_patterns.contains(&"Trill".to_string()));
        assert!(arrangement_patterns.contains(&"Tremolo".to_string()));
        assert!(arrangement_patterns.contains(&"Sustain".to_string()));
        assert!(arrangement_patterns.contains(&"Staccato".to_string()));
        assert!(arrangement_patterns.contains(&"Legato".to_string()));
    }

    #[test]
    fn test_keys_multi_mic_patterns() {
        let keys = create_keys_group();
        
        // Verify multi-mic patterns
        let multi_mic_patterns = keys.multi_mic_patterns();
        assert!(multi_mic_patterns.contains(&"DI".to_string()));
        assert!(multi_mic_patterns.contains(&"Amp".to_string()));
        assert!(multi_mic_patterns.contains(&"Close".to_string()));
        assert!(multi_mic_patterns.contains(&"Room".to_string()));
        assert!(multi_mic_patterns.contains(&"Stereo".to_string()));
        assert!(multi_mic_patterns.contains(&"Mono".to_string()));
    }

    #[test]
    fn test_keys_children() {
        let keys = create_keys_group();
        
        // Verify children exist
        assert!(keys.find_child("Piano").is_some());
        assert!(keys.find_child("Electric").is_some());
        assert!(keys.find_child("Organ").is_some());
        assert!(keys.find_child("Clav").is_some());
    }

    #[test]
    fn test_piano_subtype() {
        let keys = create_keys_group();
        let piano = keys.find_child("Piano").unwrap();
        
        assert_eq!(piano.name, "Piano");
        assert!(piano.patterns.contains(&"piano".to_string()));
        assert!(piano.patterns.contains(&"pno".to_string()));
        assert!(piano.patterns.contains(&"grand".to_string()));
        assert!(piano.patterns.contains(&"upright".to_string()));
        
        let arrangement_patterns = piano.arrangement_patterns();
        assert!(arrangement_patterns.contains(&"Classical".to_string()));
        assert!(arrangement_patterns.contains(&"Jazz".to_string()));
        assert!(arrangement_patterns.contains(&"Pop".to_string()));
        assert!(arrangement_patterns.contains(&"Ballad".to_string()));
        assert!(arrangement_patterns.contains(&"Stride".to_string()));
        assert!(arrangement_patterns.contains(&"Ragtime".to_string()));
        
        let multi_mic_patterns = piano.multi_mic_patterns();
        assert!(multi_mic_patterns.contains(&"Close".to_string()));
        assert!(multi_mic_patterns.contains(&"Room".to_string()));
        assert!(multi_mic_patterns.contains(&"Stereo".to_string()));
        assert!(multi_mic_patterns.contains(&"Mono".to_string()));
        assert!(multi_mic_patterns.contains(&"Lid".to_string()));
    }

    #[test]
    fn test_electric_subtype() {
        let keys = create_keys_group();
        let electric = keys.find_child("Electric").unwrap();
        
        assert_eq!(electric.name, "Electric");
        assert!(electric.patterns.contains(&"electric".to_string()));
        assert!(electric.patterns.contains(&"rhodes".to_string()));
        assert!(electric.patterns.contains(&"wurlitzer".to_string()));
        assert!(electric.patterns.contains(&"ep".to_string()));
        
        let arrangement_patterns = electric.arrangement_patterns();
        assert!(arrangement_patterns.contains(&"Clean".to_string()));
        assert!(arrangement_patterns.contains(&"Chorus".to_string()));
        assert!(arrangement_patterns.contains(&"Tremolo".to_string()));
        assert!(arrangement_patterns.contains(&"Phaser".to_string()));
        assert!(arrangement_patterns.contains(&"Vintage".to_string()));
        
        let multi_mic_patterns = electric.multi_mic_patterns();
        assert!(multi_mic_patterns.contains(&"DI".to_string()));
        assert!(multi_mic_patterns.contains(&"Amp".to_string()));
        assert!(multi_mic_patterns.contains(&"Close".to_string()));
        assert!(multi_mic_patterns.contains(&"Room".to_string()));
    }

    #[test]
    fn test_organ_subtype() {
        let keys = create_keys_group();
        let organ = keys.find_child("Organ").unwrap();
        
        assert_eq!(organ.name, "Organ");
        assert!(organ.patterns.contains(&"organ".to_string()));
        assert!(organ.patterns.contains(&"hammond".to_string()));
        assert!(organ.patterns.contains(&"b3".to_string()));
        assert!(organ.patterns.contains(&"leslie".to_string()));
        
        let arrangement_patterns = organ.arrangement_patterns();
        assert!(arrangement_patterns.contains(&"Drawbar".to_string()));
        assert!(arrangement_patterns.contains(&"Percussion".to_string()));
        assert!(arrangement_patterns.contains(&"Leslie".to_string()));
        assert!(arrangement_patterns.contains(&"Rotary".to_string()));
        assert!(arrangement_patterns.contains(&"Gospel".to_string()));
        assert!(arrangement_patterns.contains(&"Rock".to_string()));
        
        let multi_mic_patterns = organ.multi_mic_patterns();
        assert!(multi_mic_patterns.contains(&"DI".to_string()));
        assert!(multi_mic_patterns.contains(&"Leslie".to_string()));
        assert!(multi_mic_patterns.contains(&"Close".to_string()));
        assert!(multi_mic_patterns.contains(&"Room".to_string()));
    }

    #[test]
    fn test_clav_subtype() {
        let keys = create_keys_group();
        let clav = keys.find_child("Clav").unwrap();
        
        assert_eq!(clav.name, "Clav");
        assert!(clav.patterns.contains(&"clav".to_string()));
        assert!(clav.patterns.contains(&"clavinet".to_string()));
        assert!(clav.patterns.contains(&"funk".to_string()));
        
        let arrangement_patterns = clav.arrangement_patterns();
        assert!(arrangement_patterns.contains(&"Funk".to_string()));
        assert!(arrangement_patterns.contains(&"Wah".to_string()));
        assert!(arrangement_patterns.contains(&"Mute".to_string()));
        assert!(arrangement_patterns.contains(&"Staccato".to_string()));
        assert!(arrangement_patterns.contains(&"Percussive".to_string()));
        
        let multi_mic_patterns = clav.multi_mic_patterns();
        assert!(multi_mic_patterns.contains(&"DI".to_string()));
        assert!(multi_mic_patterns.contains(&"Amp".to_string()));
        assert!(multi_mic_patterns.contains(&"Close".to_string()));
    }
}

