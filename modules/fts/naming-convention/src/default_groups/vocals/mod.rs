//! Default Vocals group configuration
//!
//! Vocals group includes Lead Vocal and BGV (Backing Vocals) as sub-types.
//! Based on the fts-naming-parser config.json structure.

use crate::{FullGroup, define_group};

/// Create the default Vocals group with sub-types
pub fn create_vocals_group() -> FullGroup {
    let mut vocals = define_group! {
        name = "Vocals",
        prefix = "V",
        patterns = ["vocal", "vox", "vocals", "voice"],
        negative_patterns = [],
        parent_track = "Vocals",
        children = [
            define_group! {
                name = "Lead Vocal",
                prefix = "V",
                patterns = ["vocal", "vox", "lead vox", "lead vocal", "singer"],
                negative_patterns = ["bgv", "bgvs", "backing", "background"],
                arrangement_patterns = ["Verse", "Chorus", "Bridge", "Ad-lib", "Run", "Riff", "Vibrato", "Bend", "Slide", "Breath", "Whisper", "Belt", "Head", "Chest", "Mixed", "Shout", "Aggressive", "Power", "Scream", "Growl", "Soft", "Intimate", "Breathy"],
                layers_patterns = ["Double", "DBL", "Stack", "Tight", "Loose", "Octave", "Unison", "Harmony", "Lead"],
                multi_mic_patterns = ["Close", "Room", "Condenser", "Dynamic", "Ribbon", "Intimate", "Robust"],
                effect_patterns = ["Reverb", "Delay", "Chorus", "Flanger", "Distortion", "Autotune", "h3000", "eko", "plate", "magic", "vocoder"],
            },
            define_group! {
                name = "BGV",
                prefix = "V BGVs",
                patterns = ["v bgvs", "v bgv", "bgv", "bgvs", "backing vocal", "background vocal", "harm", "harmony", "choir", "vocal stack"],
                arrangement_patterns = ["Harmony", "Unison", "Octave", "Third", "Fifth", "Stack", "Layer", "Blend", "Call", "Response", "Ah", "Oh", "La", "Do"],
                multi_mic_patterns = ["Close", "Room", "Condenser", "Dynamic"],
                children = [
                    define_group! {
                        name = "Soprano",
                        prefix = "Soprano",
                        patterns = ["soprano", "high"],
                        arrangement_patterns = ["High", "Head", "Falsetto", "Whistle"],
                    },
                    define_group! {
                        name = "Alto",
                        prefix = "Alto",
                        patterns = ["alto", "mid", "middle"],
                        arrangement_patterns = ["Mid", "Chest", "Mixed", "Belt"],
                    },
                    define_group! {
                        name = "Tenor",
                        prefix = "Tenor",
                        patterns = ["tenor"],
                        arrangement_patterns = ["High", "Head", "Mixed", "Belt"],
                    },
                    define_group! {
                        name = "Bass",
                        prefix = "Bass",
                        patterns = ["bass", "low", "bottom"],
                        arrangement_patterns = ["Low", "Chest", "Fry"],
                    },
                    define_group! {
                        name = "Unison",
                        prefix = "Unison",
                        patterns = ["unison", "all", "stack"],
                        arrangement_patterns = ["All", "Together", "Stack", "Blend"],
                    },
                ],
            },
        ],
    };
    
    vocals
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_vocals_group() {
        let vocals = create_vocals_group();
        
        assert_eq!(vocals.name, "Vocals");
        assert_eq!(vocals.prefix, "V");
        assert!(vocals.patterns.contains(&"vocal".to_string()));
        assert!(vocals.patterns.contains(&"vox".to_string()));
        assert!(vocals.patterns.contains(&"vocals".to_string()));
        assert!(vocals.patterns.contains(&"voice".to_string()));
        assert!(vocals.has_children());
        assert_eq!(vocals.children.len(), 2);
    }

    #[test]
    fn test_vocals_children() {
        let vocals = create_vocals_group();
        
        // Verify children exist
        assert!(vocals.find_child("Lead Vocal").is_some());
        assert!(vocals.find_child("BGV").is_some());
    }

    #[test]
    fn test_lead_vocal_subtype() {
        let vocals = create_vocals_group();
        let lead_vocal = vocals.find_child("Lead Vocal").unwrap();
        
        assert_eq!(lead_vocal.name, "Lead Vocal");
        assert!(lead_vocal.patterns.contains(&"vocal".to_string()));
        assert!(lead_vocal.patterns.contains(&"vox".to_string()));
        assert!(lead_vocal.patterns.contains(&"lead vox".to_string()));
        assert!(lead_vocal.patterns.contains(&"lead vocal".to_string()));
        assert!(lead_vocal.patterns.contains(&"singer".to_string()));
        
        // Verify negative patterns
        assert!(lead_vocal.negative_patterns.contains(&"bgv".to_string()));
        assert!(lead_vocal.negative_patterns.contains(&"bgvs".to_string()));
        assert!(lead_vocal.negative_patterns.contains(&"backing".to_string()));
        assert!(lead_vocal.negative_patterns.contains(&"background".to_string()));
        
        // Verify arrangement patterns
        let arrangement_patterns = lead_vocal.arrangement_patterns();
        assert!(arrangement_patterns.contains(&"Verse".to_string()));
        assert!(arrangement_patterns.contains(&"Chorus".to_string()));
        assert!(arrangement_patterns.contains(&"Bridge".to_string()));
        assert!(arrangement_patterns.contains(&"Ad-lib".to_string()));
        assert!(arrangement_patterns.contains(&"Belt".to_string()));
        assert!(arrangement_patterns.contains(&"Head".to_string()));
        assert!(arrangement_patterns.contains(&"Chest".to_string()));
        assert!(arrangement_patterns.contains(&"Mixed".to_string()));
        
        // Verify multi-mic patterns
        let multi_mic_patterns = lead_vocal.multi_mic_patterns();
        assert!(multi_mic_patterns.contains(&"Close".to_string()));
        assert!(multi_mic_patterns.contains(&"Room".to_string()));
        assert!(multi_mic_patterns.contains(&"Condenser".to_string()));
        assert!(multi_mic_patterns.contains(&"Dynamic".to_string()));
        assert!(multi_mic_patterns.contains(&"Ribbon".to_string()));
    }

    #[test]
    fn test_lead_vocal_layers_patterns() {
        let vocals = create_vocals_group();
        let lead_vocal = vocals.find_child("Lead Vocal").unwrap();
        
        // Verify layers patterns (Double should be here, not a sub-group)
        let layers_patterns = lead_vocal.layers_patterns();
        assert!(layers_patterns.contains(&"Double".to_string()));
        assert!(layers_patterns.contains(&"DBL".to_string()));
        assert!(layers_patterns.contains(&"Stack".to_string()));
        assert!(layers_patterns.contains(&"Tight".to_string()));
        assert!(layers_patterns.contains(&"Loose".to_string()));
        assert!(layers_patterns.contains(&"Octave".to_string()));
        assert!(layers_patterns.contains(&"Unison".to_string()));
        assert!(layers_patterns.contains(&"Harmony".to_string()));
        assert!(layers_patterns.contains(&"Lead".to_string()));
    }

    #[test]
    fn test_lead_vocal_effect_patterns() {
        let vocals = create_vocals_group();
        let lead_vocal = vocals.find_child("Lead Vocal").unwrap();
        
        // Verify effect patterns (Vocal Effects should be here, not a sub-group)
        let effect_patterns = lead_vocal.effect_patterns();
        assert!(effect_patterns.contains(&"Reverb".to_string()));
        assert!(effect_patterns.contains(&"Delay".to_string()));
        assert!(effect_patterns.contains(&"Chorus".to_string()));
        assert!(effect_patterns.contains(&"Flanger".to_string()));
        assert!(effect_patterns.contains(&"Distortion".to_string()));
        assert!(effect_patterns.contains(&"Autotune".to_string()));
        assert!(effect_patterns.contains(&"h3000".to_string()));
        assert!(effect_patterns.contains(&"eko".to_string()));
        assert!(effect_patterns.contains(&"plate".to_string()));
        assert!(effect_patterns.contains(&"magic".to_string()));
        assert!(effect_patterns.contains(&"vocoder".to_string()));
    }

    #[test]
    fn test_bgv_subtype() {
        let vocals = create_vocals_group();
        let bgv = vocals.find_child("BGV").unwrap();
        
        assert_eq!(bgv.name, "BGV");
        assert_eq!(bgv.prefix, "V BGVs");
        assert!(bgv.patterns.contains(&"bgv".to_string()));
        assert!(bgv.patterns.contains(&"bgvs".to_string()));
        assert!(bgv.patterns.contains(&"backing vocal".to_string()));
        assert!(bgv.patterns.contains(&"background vocal".to_string()));
        assert!(bgv.patterns.contains(&"harm".to_string()));
        assert!(bgv.patterns.contains(&"harmony".to_string()));
        assert!(bgv.patterns.contains(&"choir".to_string()));
        assert!(bgv.patterns.contains(&"vocal stack".to_string()));
        
        // Verify arrangement patterns
        let arrangement_patterns = bgv.arrangement_patterns();
        assert!(arrangement_patterns.contains(&"Harmony".to_string()));
        assert!(arrangement_patterns.contains(&"Unison".to_string()));
        assert!(arrangement_patterns.contains(&"Octave".to_string()));
        assert!(arrangement_patterns.contains(&"Third".to_string()));
        assert!(arrangement_patterns.contains(&"Fifth".to_string()));
        assert!(arrangement_patterns.contains(&"Stack".to_string()));
        assert!(arrangement_patterns.contains(&"Layer".to_string()));
        assert!(arrangement_patterns.contains(&"Blend".to_string()));
        
        // Verify multi-mic patterns
        let multi_mic_patterns = bgv.multi_mic_patterns();
        assert!(multi_mic_patterns.contains(&"Close".to_string()));
        assert!(multi_mic_patterns.contains(&"Room".to_string()));
        assert!(multi_mic_patterns.contains(&"Condenser".to_string()));
        assert!(multi_mic_patterns.contains(&"Dynamic".to_string()));
    }

    #[test]
    fn test_bgv_children() {
        let vocals = create_vocals_group();
        let bgv = vocals.find_child("BGV").unwrap();
        
        // Verify BGV has children
        assert!(bgv.has_children());
        assert!(bgv.find_child("Soprano").is_some());
        assert!(bgv.find_child("Alto").is_some());
        assert!(bgv.find_child("Tenor").is_some());
        assert!(bgv.find_child("Bass").is_some());
        assert!(bgv.find_child("Unison").is_some());
    }

    #[test]
    fn test_lead_vocal_no_children() {
        let vocals = create_vocals_group();
        let lead_vocal = vocals.find_child("Lead Vocal").unwrap();
        
        // Verify Lead Vocal has no children (Double, Whisper, Shout, Vocal Effects are not sub-groups)
        assert!(!lead_vocal.has_children());
    }
}

