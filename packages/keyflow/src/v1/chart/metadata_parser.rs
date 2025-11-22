//! Metadata parsing trait
//!
//! This module defines the MetadataParser trait for parsing song metadata
//! like title, artist, tempo, time signature, and key.

use crate::key::keys::Key;
use crate::parsing::key::parser::KeyParser;
use crate::time::{Tempo, TimeSignature};
use crate::chart::chart::Chart;

/// Trait for parsing metadata from chart lines
pub trait MetadataParser {
    /// Parse a metadata line and return true if it contained valid metadata
    fn parse_metadata_line(&mut self, line: &str) -> bool;
    
    /// Parse a key signature from a string like "#G", "bE", etc.
    fn parse_key_signature(&self, input: &str) -> Result<Key, String>;
}

impl MetadataParser for Chart {
    fn parse_metadata_line(&mut self, line: &str) -> bool {
        // Try title - artist format
        if line.contains(" - ") && !line.starts_with('#') && !line.contains("bpm") {
            let parts: Vec<&str> = line.splitn(2, " - ").collect();
            if parts.len() == 2 {
                self.metadata.title = Some(parts[0].trim().to_string());
                self.metadata.artist = Some(parts[1].trim().to_string());
                return true;
            }
        }

        // Parse tempo, time signature, and key
        let tokens: Vec<&str> = line.split_whitespace().collect();
        let mut found_metadata = false;

        for token in tokens {
            // Tempo (e.g., "68bpm")
            if let Ok(tempo) = Tempo::parse(token) {
                self.tempo = Some(tempo);
                found_metadata = true;
            }
            // Time signature (e.g., "6/8")
            else if let Ok(time_sig) = TimeSignature::parse(token) {
                self.time_signature = Some(time_sig);
                found_metadata = true;
            }
            // Key signature (e.g., "#E", "#Em", "#F#")
            else if token.starts_with('#') && token.len() > 1 {
                if let Ok(key) = self.parse_key_signature(token) {
                    self.current_key = Some(key.clone());
                    self.initial_key = Some(key.clone()); // Set initial key
                    found_metadata = true;
                }
            }
        }

        found_metadata
    }

    fn parse_key_signature(&self, input: &str) -> Result<Key, String> {
        let input = input.trim();
        if input.is_empty() {
            return Err("Empty key signature".to_string());
        }

        // Parse the note using the existing Note::from_string function
        let note_str = if input.starts_with('#') {
            // #G -> G, #C# -> C#
            &input[1..]
        } else if input.starts_with('b') {
            // bE -> Eb
            let note_name = &input[1..];
            &format!("{}b", note_name)
        } else {
            // G -> G
            input
        };

        // Use the existing note parser
        let note = crate::primitives::note::Note::from_string(note_str)
            .ok_or_else(|| format!("Invalid note: {}", note_str))?;

        // Convert to format expected by key parser
        let key_string = format!("{} Major", note);

        // Use the keyflow key parser
        let mut key_parser = KeyParser::new();
        match key_parser.parse(&key_string) {
            Ok(key) => Ok(key),
            Err(errors) => Err(format!("Key parsing error: {:?}", errors)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_title_artist() {
        let mut chart = Chart::new();
        assert!(chart.parse_metadata_line("Beautiful Song - Amazing Artist"));
        assert_eq!(chart.metadata.title, Some("Beautiful Song".to_string()));
        assert_eq!(chart.metadata.artist, Some("Amazing Artist".to_string()));
    }

    #[test]
    fn test_parse_tempo() {
        let mut chart = Chart::new();
        assert!(chart.parse_metadata_line("120bpm"));
        assert_eq!(chart.tempo.as_ref().unwrap().bpm, 120);
    }

    #[test]
    fn test_parse_time_signature() {
        let mut chart = Chart::new();
        assert!(chart.parse_metadata_line("4/4"));
        assert_eq!(chart.time_signature.as_ref().unwrap().numerator, 4);
        assert_eq!(chart.time_signature.as_ref().unwrap().denominator, 4);
    }

    #[test]
    fn test_parse_key_signature() {
        let mut chart = Chart::new();
        assert!(chart.parse_metadata_line("#G"));
        assert!(chart.initial_key.is_some());
    }

    #[test]
    fn test_parse_combined_metadata() {
        let mut chart = Chart::new();
        assert!(chart.parse_metadata_line("120bpm 4/4 #G"));
        assert_eq!(chart.tempo.as_ref().unwrap().bpm, 120);
        assert_eq!(chart.time_signature.as_ref().unwrap().numerator, 4);
        assert!(chart.initial_key.is_some());
    }
}

