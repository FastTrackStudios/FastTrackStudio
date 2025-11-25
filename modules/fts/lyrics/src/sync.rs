//! Synchronization with setlist Song type

use crate::core::{LyricSection, Lyrics, SectionTypeHint};
use setlist::core::{Section, SectionType, Song};
use tracing::{debug, info};

/// Match lyrics sections with song sections from the setlist
pub struct SectionMatcher;

impl SectionMatcher {
    /// Match lyrics sections to song sections
    ///
    /// This attempts to automatically match lyric sections (e.g., "[Verse 1]")
    /// with song sections from the setlist based on section type and number.
    pub fn match_sections(lyrics: &mut Lyrics, song: &Song) -> Vec<SectionMatch> {
        let mut matches = Vec::new();

        info!(
            song_name = %song.name,
            lyric_sections = lyrics.sections.len(),
            song_sections = song.sections.len(),
            "Matching lyrics sections with song sections"
        );

        for lyric_section in &lyrics.sections {
            let song_section = Self::find_matching_song_section(lyric_section, song);

            if let Some(song_section) = song_section {
                debug!(
                    lyric_section = %lyric_section.name,
                    song_section = %song_section.name,
                    "Matched lyric section to song section"
                );

                matches.push(SectionMatch {
                    lyric_section_name: lyric_section.name.clone(),
                    song_section_name: song_section.name.clone(),
                    confidence: Self::calculate_confidence(lyric_section, &song_section),
                });
            } else {
                debug!(
                    lyric_section = %lyric_section.name,
                    "No matching song section found"
                );
            }
        }

        matches
    }

    /// Find a matching song section for a lyric section
    fn find_matching_song_section<'a>(
        lyric_section: &LyricSection,
        song: &'a Song,
    ) -> Option<&'a Section> {
        // Try exact name match first
        if let Some(section) = song.sections.iter().find(|s| {
            s.name.eq_ignore_ascii_case(&lyric_section.name)
        }) {
            return Some(section);
        }

        // Try matching by section type and number
        if let Some(ref hint) = lyric_section.section_type {
            let target_type = Self::hint_to_section_type(hint);
            let target_number = lyric_section.number;

            for section in &song.sections {
                if section.section_type == target_type {
                    if target_number.is_none() || section.number == target_number {
                        return Some(section);
                    }
                }
            }
        }

        // Try fuzzy matching on section type
        if let Some(ref hint) = lyric_section.section_type {
            let target_type = Self::hint_to_section_type(hint);

            for section in &song.sections {
                if section.section_type == target_type {
                    // If no number specified, return first match
                    if lyric_section.number.is_none() {
                        return Some(section);
                    }
                }
            }
        }

        None
    }

    /// Convert SectionTypeHint to SectionType
    fn hint_to_section_type(hint: &SectionTypeHint) -> SectionType {
        match hint {
            SectionTypeHint::Intro => SectionType::Intro,
            SectionTypeHint::Verse => SectionType::Verse,
            SectionTypeHint::Chorus => SectionType::Chorus,
            SectionTypeHint::Bridge => SectionType::Bridge,
            SectionTypeHint::Outro => SectionType::Outro,
            SectionTypeHint::Instrumental => SectionType::Instrumental,
            SectionTypeHint::PreChorus => SectionType::Pre(Box::new(SectionType::Chorus)),
            SectionTypeHint::PostChorus => SectionType::Post(Box::new(SectionType::Chorus)),
            SectionTypeHint::Custom(_) => SectionType::Custom,
        }
    }

    /// Calculate confidence score for a match (0.0 to 1.0)
    fn calculate_confidence(lyric_section: &LyricSection, song_section: &Section) -> f64 {
        let mut confidence: f64 = 0.0;

        // Exact name match = 1.0
        if lyric_section.name.eq_ignore_ascii_case(&song_section.name) {
            return 1.0;
        }

        // Section type match = 0.5
        if let Some(ref hint) = lyric_section.section_type {
            let target_type = Self::hint_to_section_type(hint);
            if song_section.section_type == target_type {
                confidence += 0.5;
            }
        }

        // Number match = 0.3
        if let (Some(lyric_num), Some(song_num)) = (lyric_section.number, song_section.number) {
            if lyric_num == song_num {
                confidence += 0.3;
            }
        }

        // Partial name match = 0.2
        let lyric_lower = lyric_section.name.to_lowercase();
        let song_lower = song_section.name.to_lowercase();
        if lyric_lower.contains(&song_lower) || song_lower.contains(&lyric_lower) {
            confidence += 0.2;
        }

        confidence.min(1.0)
    }

    /// Apply timing information from song sections to lyrics
    ///
    /// This updates the lyrics with timing information from matched song sections.
    pub fn apply_timing(lyrics: &mut Lyrics, song: &Song) {
        let matches = Self::match_sections(lyrics, song);

        for match_info in matches {
            // Find the lyric section
            if let Some(lyric_section) = lyrics
                .sections
                .iter_mut()
                .find(|s| s.name == match_info.lyric_section_name)
            {
                // Find the song section
                if let Some(song_section) = song
                    .sections
                    .iter()
                    .find(|s| s.name == match_info.song_section_name)
                {
                    // Calculate timing per line
                    let section_duration = song_section.duration();
                    let line_count = lyric_section.lines.len() as f64;

                    if line_count > 0.0 {
                        let time_per_line = section_duration / line_count;
                        let section_start = song_section.start_seconds();

                        for (i, line) in lyric_section.lines.iter_mut().enumerate() {
                            line.start_time = Some(section_start + (i as f64 * time_per_line));
                            line.end_time = Some(
                                section_start + ((i as f64 + 1.0) * time_per_line),
                            );
                        }
                    }
                }
            }
        }
    }
}

/// Represents a match between a lyric section and a song section
#[derive(Debug, Clone, PartialEq)]
pub struct SectionMatch {
    /// Name of the lyric section
    pub lyric_section_name: String,
    /// Name of the matched song section
    pub song_section_name: String,
    /// Confidence score (0.0 to 1.0)
    pub confidence: f64,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_lyrics;
    use setlist::core::Section;

    fn create_test_song() -> Song {
        let mut song = Song::new("Test Song".to_string()).unwrap();

        // Add sections (markers are optional for basic matching tests)
        let intro = Section::from_seconds(
            SectionType::Intro,
            0.0,
            10.0,
            "Intro".to_string(),
            None,
        )
        .unwrap();
        song.add_section(intro).unwrap();

        let verse1 = Section::from_seconds(
            SectionType::Verse,
            10.0,
            50.0,
            "Verse 1".to_string(),
            Some(1),
        )
        .unwrap();
        song.add_section(verse1).unwrap();

        let chorus = Section::from_seconds(
            SectionType::Chorus,
            50.0,
            90.0,
            "Chorus".to_string(),
            None,
        )
        .unwrap();
        song.add_section(chorus).unwrap();

        song
    }

    #[test]
    fn test_section_matching() {
        let text = r#"[Intro]
(Woo)

[Verse 1]
Test line

[Chorus]
Test line"#;

        let mut lyrics = parse_lyrics(text, "Test Song".to_string()).unwrap();
        let song = create_test_song();

        let matches = SectionMatcher::match_sections(&mut lyrics, &song);
        assert_eq!(matches.len(), 3);
        assert!(matches.iter().any(|m| m.lyric_section_name == "Intro"));
        assert!(matches.iter().any(|m| m.lyric_section_name == "Verse 1"));
        assert!(matches.iter().any(|m| m.lyric_section_name == "Chorus"));
    }

    #[test]
    fn test_apply_timing() {
        let text = r#"[Verse 1]
Line 1
Line 2"#;

        let mut lyrics = parse_lyrics(text, "Test Song".to_string()).unwrap();
        let song = create_test_song();

        SectionMatcher::apply_timing(&mut lyrics, &song);

        let verse_section = lyrics.sections.iter().find(|s| s.name == "Verse 1").unwrap();
        assert_eq!(verse_section.lines.len(), 2);
        // Timing should be applied
        assert!(verse_section.lines[0].start_time.is_some());
        assert!(verse_section.lines[1].end_time.is_some());
    }
}

