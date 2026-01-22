//! Parsing Utilities
//!
//! Helper functions for parsing song names, section types, and extracting numbers.

use crate::setlist::core::SectionType;
use keyflow::sections::section_type::ParsedSection;

/// Parse a song name that may be in the format "Song Name" - Artist or Song Name - Artist
/// Returns (song_name, artist_option)
pub fn parse_song_name(full_name: &str) -> (String, Option<String>) {
    let trimmed = full_name.trim();

    // Look for " - " separator
    if let Some(separator_pos) = trimmed.find(" - ") {
        let song_part = trimmed[..separator_pos].trim();
        let artist_part = trimmed[separator_pos + 3..].trim();

        // Remove quotes from song name if present
        let song_name = if song_part.starts_with('"') && song_part.ends_with('"') {
            song_part[1..song_part.len() - 1].trim().to_string()
        } else {
            song_part.to_string()
        };

        let artist = if artist_part.is_empty() {
            None
        } else {
            Some(artist_part.to_string())
        };

        (song_name, artist)
    } else {
        // No separator found, treat entire string as song name
        let song_name = if trimmed.starts_with('"') && trimmed.ends_with('"') {
            trimmed[1..trimmed.len() - 1].trim().to_string()
        } else {
            trimmed.to_string()
        };

        (song_name, None)
    }
}

/// Parse section info from a region/marker name
///
/// This function uses keyflow's `SectionType::parse_with_measure_count()` which handles:
/// - Full names: verse, chorus, bridge, intro, outro, instrumental, interlude, breakdown, hits, count
/// - Abbreviations: vs, ch, br, in, out, inst, int, bd, hit
/// - Pre/Post prefixes: pre-chorus, post-verse, etc.
/// - Quoted comments: `Interlude "Horns"`, `CH 4 "Down"`
/// - Preset modifiers: `Down CH 4`, `Build VS 8`
///
/// For DAW region names like "INST 1a", "VS 1", "Interlude A \"Horns\"", it extracts
/// the section type, optional number suffix, and optional comment.
///
/// Returns a `ParsedSection` containing the section type and optional comment.
pub fn parse_section_from_name(name: &str) -> Option<ParsedSection> {
    let name = name.trim();

    // Try using keyflow's full parser which handles comments
    if let Some(parsed) = SectionType::parse_with_measure_count(name) {
        return Some(parsed);
    }

    // If that didn't work, try extracting just the section type prefix
    // Examples: "INST 1a" -> "INST", "VS 1" -> "VS", "Interlude 2" -> "Interlude"
    let parts: Vec<&str> = name.split_whitespace().collect();
    if let Some(prefix) = parts.first() {
        if let Some(parsed) = SectionType::parse_with_measure_count(prefix) {
            return Some(parsed);
        }
    }

    // Try matching patterns like "INST1a", "VS1", "CH2" (no space between type and number)
    // Find where digits start and try parsing everything before
    let name_lower = name.to_lowercase();
    if let Some(digit_pos) = name_lower.find(|c: char| c.is_ascii_digit()) {
        if digit_pos > 0 {
            let prefix = &name[..digit_pos];
            if let Ok(section_type) = SectionType::parse(prefix) {
                return Some(ParsedSection::new(section_type));
            }
        }
    }

    // Try Pre/Post prefixes (keyflow handles "pre-" but not "pre " with space)
    if name_lower.starts_with("pre ") || name_lower.starts_with("pre-") {
        let rest = if name_lower.starts_with("pre ") {
            &name[4..]
        } else {
            &name[4..]
        };
        if let Some(inner) = parse_section_from_name(rest) {
            return Some(ParsedSection::new(SectionType::Pre(Box::new(
                inner.section_type,
            ))));
        }
    }
    if name_lower.starts_with("post ") || name_lower.starts_with("post-") {
        let rest = if name_lower.starts_with("post ") {
            &name[5..]
        } else {
            &name[5..]
        };
        if let Some(inner) = parse_section_from_name(rest) {
            return Some(ParsedSection::new(SectionType::Post(Box::new(
                inner.section_type,
            ))));
        }
    }

    None
}

/// Parse section type from a region/marker name (legacy function for compatibility)
///
/// This is a simplified version that just returns the SectionType.
/// For full parsing with comments, use `parse_section_from_name()`.
pub fn parse_section_type_from_name(name: &str) -> Option<SectionType> {
    parse_section_from_name(name).map(|parsed| parsed.section_type)
}

/// Extract number from name (e.g., "Verse 1", "CH 2")
pub fn extract_number_from_name(name: &str) -> Option<u32> {
    let name_lower = name.to_lowercase();
    let parts: Vec<&str> = name_lower.split_whitespace().collect();

    for part in &parts {
        if let Ok(num) = part.parse::<u32>() {
            return Some(num);
        }
        // Also check if number follows section type abbreviation
        if part.len() > 2 {
            if let Ok(num) = part[2..].parse::<u32>() {
                return Some(num);
            }
        }
    }

    None
}
