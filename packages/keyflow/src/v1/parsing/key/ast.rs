//! Key AST for building key signatures from parsed expressions

// use crate::primitives::note::Note; // TODO: Will be used when implementing key building
use crate::key::keys::{Key, ScaleType, ScaleMode, MelodicMinorMode, HarmonicMinorMode};
use crate::parsing::common::ParserErrors;
use super::expressions::KeyAST;

impl KeyAST {
    /// Build a Key from the parsed expressions
    pub fn build_key(&self) -> Result<Key, ParserErrors> {
        if !self.is_valid() {
            return Err(ParserErrors::new(self.errors.clone()));
        }
        
        let root = self.root.unwrap();
        
        // Handle custom scale
        if let Some(_semitones) = &self.custom_scale {
            // For now, we'll create a basic key with the custom scale
            // In a full implementation, we'd need to add custom scale support to Key
            return Ok(Key::new(root, ScaleType::Diatonic, ScaleMode::Ionian));
        }
        
        // Handle melodic minor modes
        if let Some(mode) = &self.melodic_minor_mode {
            return Ok(Key::new_melodic_minor_mode(root, *mode));
        }
        
        // Handle harmonic minor modes
        if let Some(mode) = &self.harmonic_minor_mode {
            return Ok(Key::new_harmonic_minor_mode(root, *mode));
        }
        
        // Handle regular scale type and mode
        let scale_type = self.scale_type.unwrap_or(ScaleType::Diatonic);
        let scale_mode = self.scale_mode.unwrap_or(ScaleMode::Ionian);
        
        Ok(Key::new(root, scale_type, scale_mode))
    }
    
    /// Parse scale name string into scale type and mode
    pub fn parse_scale_name(&mut self, name: &str) {
        match name.to_lowercase().as_str() {
            "major" | "ionian" => {
                self.scale_type = Some(ScaleType::Diatonic);
                self.scale_mode = Some(ScaleMode::Ionian);
            },
            "minor" | "aeolian" => {
                self.scale_type = Some(ScaleType::Diatonic);
                self.scale_mode = Some(ScaleMode::Aeolian);
            },
            "dorian" => {
                self.scale_type = Some(ScaleType::Diatonic);
                self.scale_mode = Some(ScaleMode::Dorian);
            },
            "phrygian" => {
                self.scale_type = Some(ScaleType::Diatonic);
                self.scale_mode = Some(ScaleMode::Phrygian);
            },
            "lydian" => {
                self.scale_type = Some(ScaleType::Diatonic);
                self.scale_mode = Some(ScaleMode::Lydian);
            },
            "mixolydian" => {
                self.scale_type = Some(ScaleType::Diatonic);
                self.scale_mode = Some(ScaleMode::Mixolydian);
            },
            "locrian" => {
                self.scale_type = Some(ScaleType::Diatonic);
                self.scale_mode = Some(ScaleMode::Locrian);
            },
            // Melodic minor modes
            "melodic minor" | "melodicminor" => {
                self.melodic_minor_mode = Some(MelodicMinorMode::MelodicMinor);
            },
            "dorian b2" | "dorianb2" => {
                self.melodic_minor_mode = Some(MelodicMinorMode::DorianFlat2);
            },
            "lydian augmented" | "lydianaugmented" => {
                self.melodic_minor_mode = Some(MelodicMinorMode::LydianAugmented);
            },
            "lydian dominant" | "lydiandominant" => {
                self.melodic_minor_mode = Some(MelodicMinorMode::LydianDominant);
            },
            "mixolydian b6" | "mixolydianb6" => {
                self.melodic_minor_mode = Some(MelodicMinorMode::MixolydianFlat6);
            },
            "aeolian b5" | "aeolianb5" => {
                self.melodic_minor_mode = Some(MelodicMinorMode::AeolianFlat5);
            },
            "super locrian" | "superlocrian" | "altered" => {
                self.melodic_minor_mode = Some(MelodicMinorMode::SuperLocrian);
            },
            // Harmonic minor modes
            "harmonic minor" | "harmonicminor" => {
                self.harmonic_minor_mode = Some(HarmonicMinorMode::HarmonicMinor);
            },
            "locrian nat 6" | "locriannat6" => {
                self.harmonic_minor_mode = Some(HarmonicMinorMode::LocrianNat6);
            },
            "ionian augmented" | "ionianaugmented" => {
                self.harmonic_minor_mode = Some(HarmonicMinorMode::IonianAugmented);
            },
            "romanian dorian" | "romaniandorian" => {
                self.harmonic_minor_mode = Some(HarmonicMinorMode::RomanianDorian);
            },
            "phrygian dominant" | "phrygiandominant" => {
                self.harmonic_minor_mode = Some(HarmonicMinorMode::PhrygianDominant);
            },
            "lydian #9" | "lydian#9" => {
                self.harmonic_minor_mode = Some(HarmonicMinorMode::LydianSharp9);
            },
            "ultra locrian" | "ultralocrian" => {
                self.harmonic_minor_mode = Some(HarmonicMinorMode::UltraLocrian);
            },
            _ => {
                self.add_error(crate::parsing::common::ParserError::Key(crate::parsing::key::KeyParserError::InvalidScaleName(name.to_string())));
            }
        }
    }
    
    /// Parse custom semitone sequence from string like "[0,2,4,5,7,9,11]"
    pub fn parse_custom_scale(&mut self, input: &str) {
        // Remove brackets and split by commas
        let cleaned = input.trim_start_matches('[').trim_end_matches(']');
        let parts: Result<Vec<u8>, _> = cleaned
            .split(',')
            .map(|s| s.trim().parse::<u8>())
            .collect();
            
        match parts {
            Ok(semitones) => {
                if semitones.is_empty() {
                    self.add_error(crate::parsing::common::ParserError::Key(crate::parsing::key::KeyParserError::EmptyCustomScale));
                } else {
                    self.custom_scale = Some(semitones);
                }
            },
            Err(_) => {
                self.add_error(crate::parsing::common::ParserError::Key(crate::parsing::key::KeyParserError::InvalidCustomScale(input.to_string())));
            }
        }
    }
}
