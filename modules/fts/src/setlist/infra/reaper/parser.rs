//! Parsing Utilities
//!
//! Helper functions for parsing song names, section types, and extracting numbers.

use crate::setlist::core::SectionType;

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

/// Parse section type from a region/marker name
/// Handles names like "INST 1a", "VS 1", "CH 2", "BR 1", etc.
/// Extracts the section type prefix before any numbers or letters
pub fn parse_section_type_from_name(name: &str) -> Option<SectionType> {
    let name = name.trim();

    // Try exact match first
    if let Ok(section_type) = SectionType::parse(name) {
        return Some(section_type);
    }

    // Extract the prefix before any number or letter suffix
    // Examples: "INST 1a" -> "INST", "VS 1" -> "VS", "CH 2" -> "CH"
    // Split by whitespace and take the first part
    let parts: Vec<&str> = name.split_whitespace().collect();
    if let Some(prefix) = parts.first() {
        // Try matching the prefix
        if let Ok(section_type) = SectionType::parse(prefix) {
            return Some(section_type);
        }
    }

    // Try matching common patterns like "INST1a", "VS1", "CH2" (no space)
    // Check if it starts with a known abbreviation followed by a digit or letter
    let name_lower = name.to_lowercase();

    // Check for full words first (longer matches first to avoid false positives)
    if name_lower.starts_with("instrumental") {
        return Some(SectionType::Instrumental);
    }
    if name_lower.starts_with("intro") {
        return Some(SectionType::Intro);
    }
    if name_lower.starts_with("outro") {
        return Some(SectionType::Outro);
    }
    if name_lower.starts_with("verse") {
        return Some(SectionType::Verse);
    }
    if name_lower.starts_with("chorus") {
        return Some(SectionType::Chorus);
    }
    if name_lower.starts_with("bridge") {
        return Some(SectionType::Bridge);
    }

    // Check for abbreviations (check longer ones first)
    // "inst" must be checked before "in" to avoid false matches
    if name_lower.starts_with("inst") {
        // Check if it's exactly "inst" or followed by a digit/letter (like "inst1a")
        if name_lower.len() == 4
            || (name_lower.len() > 4
                && name_lower
                    .chars()
                    .nth(4)
                    .map(|c| c.is_alphanumeric())
                    .unwrap_or(false))
        {
            return Some(SectionType::Instrumental);
        }
    }
    if name_lower.starts_with("vs") {
        // Check if it's exactly "vs" or followed by a digit/letter (like "vs1")
        if name_lower.len() == 2
            || (name_lower.len() > 2
                && name_lower
                    .chars()
                    .nth(2)
                    .map(|c| c.is_alphanumeric())
                    .unwrap_or(false))
        {
            return Some(SectionType::Verse);
        }
    }
    if name_lower.starts_with("ch") {
        // Check if it's exactly "ch" or followed by a digit/letter (like "ch2")
        if name_lower.len() == 2
            || (name_lower.len() > 2
                && name_lower
                    .chars()
                    .nth(2)
                    .map(|c| c.is_alphanumeric())
                    .unwrap_or(false))
        {
            return Some(SectionType::Chorus);
        }
    }
    if name_lower.starts_with("br") {
        // Check if it's exactly "br" or followed by a digit/letter (like "br1")
        if name_lower.len() == 2
            || (name_lower.len() > 2
                && name_lower
                    .chars()
                    .nth(2)
                    .map(|c| c.is_alphanumeric())
                    .unwrap_or(false))
        {
            return Some(SectionType::Bridge);
        }
    }
    if name_lower.starts_with("in") && !name_lower.starts_with("inst") {
        // Only match "in" if it's not "inst" (already checked above)
        if name_lower.len() == 2
            || (name_lower.len() > 2
                && name_lower
                    .chars()
                    .nth(2)
                    .map(|c| c.is_alphanumeric())
                    .unwrap_or(false))
        {
            return Some(SectionType::Intro);
        }
    }
    if name_lower.starts_with("out") {
        // Check if it's exactly "out" or followed by a digit/letter (like "out1")
        if name_lower.len() == 3
            || (name_lower.len() > 3
                && name_lower
                    .chars()
                    .nth(3)
                    .map(|c| c.is_alphanumeric())
                    .unwrap_or(false))
        {
            return Some(SectionType::Outro);
        }
    }

    // Try Pre/Post prefixes
    if name_lower.starts_with("pre-") {
        if let Some(rest) = name_lower.strip_prefix("pre-") {
            if let Some(inner) = parse_section_type_from_name(rest) {
                return Some(SectionType::Pre(Box::new(inner)));
            }
        }
    }
    if name_lower.starts_with("post-") {
        if let Some(rest) = name_lower.strip_prefix("post-") {
            if let Some(inner) = parse_section_type_from_name(rest) {
                return Some(SectionType::Post(Box::new(inner)));
            }
        }
    }

    None
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
