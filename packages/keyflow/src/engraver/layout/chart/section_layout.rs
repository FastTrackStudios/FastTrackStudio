//! Section layout utilities for chart rendering.
//!
//! This module provides functions for section labeling, theming,
//! and consecutive section lettering.

use crate::sections::SectionType;
use std::collections::HashMap;

use crate::engraver::layout::tlayout::{rehearsal_themes, RehearsalMarkStyle};

/// Get theme for section type.
///
/// Maps section types to visual styles (colors, borders) for rehearsal marks.
#[must_use]
pub fn get_section_theme(section_type: &SectionType) -> RehearsalMarkStyle {
    match section_type {
        SectionType::Intro | SectionType::Outro => rehearsal_themes::blue(),
        SectionType::Verse => rehearsal_themes::green(),
        SectionType::Chorus => rehearsal_themes::purple(),
        SectionType::Bridge => rehearsal_themes::light(),
        SectionType::Instrumental => rehearsal_themes::outline(),
        SectionType::CountIn | SectionType::End => rehearsal_themes::outline(),
        SectionType::Hits => rehearsal_themes::dark(),
        SectionType::Interlude => rehearsal_themes::light(),
        SectionType::Breakdown => rehearsal_themes::dark(),
        SectionType::Pre(_) | SectionType::Post(_) => rehearsal_themes::purple(),
        SectionType::Custom(_) => rehearsal_themes::dark(),
    }
}

/// Compute section letters for consecutive repeats of the same section type.
///
/// When sections of the same type appear consecutively (e.g., Interlude Interlude Interlude),
/// they get lettered A, B, C, etc. If a different section type appears in between,
/// the lettering sequence resets.
///
/// # Examples
///
/// - `VS VS CH VS` becomes `VS 1 A, VS 1 B, CH, VS 2` (letters only for consecutive)
/// - `INT INT INT INT` becomes `INT A, INT B, INT C, INT D`
#[must_use]
pub fn compute_section_letters(sections: &[crate::ChartSection]) -> HashMap<usize, char> {
    let mut letters: HashMap<usize, char> = HashMap::new();

    // Track consecutive runs of the same section type
    // We need to do two passes:
    // 1. Find all consecutive runs
    // 2. Assign letters to runs with 2+ sections

    // Group sections by consecutive runs
    let mut runs: Vec<(String, Vec<usize>)> = Vec::new();

    for (idx, chart_section) in sections.iter().enumerate() {
        let section_type = &chart_section.section.section_type;

        // Skip compact sections (count-in) - they don't get letters
        if section_type.is_compact() {
            continue;
        }

        // Skip sections that should not be rendered (End sections)
        if !section_type.should_render() {
            continue;
        }

        // Get a key for the section type (ignoring number)
        let type_key = section_type.key();

        // Check if this continues the current run
        if let Some((last_key, indices)) = runs.last_mut() {
            if *last_key == type_key {
                indices.push(idx);
                continue;
            }
        }

        // Start a new run
        runs.push((type_key, vec![idx]));
    }

    // Assign letters to runs with 2+ sections
    for (_, indices) in runs {
        if indices.len() >= 2 {
            for (i, idx) in indices.iter().enumerate() {
                // A = 0, B = 1, C = 2, etc.
                // Support up to Z (26 letters)
                if i < 26 {
                    let letter = (b'A' + i as u8) as char;
                    letters.insert(*idx, letter);
                }
            }
        }
    }

    letters
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_section_theme_intro_outro() {
        // Intro and Outro should use blue theme
        let intro_theme = get_section_theme(&SectionType::Intro);
        let outro_theme = get_section_theme(&SectionType::Outro);

        // Both should be blue (same style)
        assert_eq!(intro_theme.background_color, outro_theme.background_color);
    }

    #[test]
    fn test_get_section_theme_verse_chorus_different() {
        // Verse and Chorus should have different themes
        let verse_theme = get_section_theme(&SectionType::Verse);
        let chorus_theme = get_section_theme(&SectionType::Chorus);

        assert_ne!(verse_theme.background_color, chorus_theme.background_color);
    }

    #[test]
    fn test_get_section_theme_pre_post_chorus() {
        // Pre(Chorus) and Post(Chorus) should use purple like Chorus
        let pre_chorus_theme = get_section_theme(&SectionType::Pre(Box::new(SectionType::Chorus)));
        let post_chorus_theme =
            get_section_theme(&SectionType::Post(Box::new(SectionType::Chorus)));
        let chorus_theme = get_section_theme(&SectionType::Chorus);

        assert_eq!(pre_chorus_theme.background_color, chorus_theme.background_color);
        assert_eq!(
            post_chorus_theme.background_color,
            chorus_theme.background_color
        );
    }
}
