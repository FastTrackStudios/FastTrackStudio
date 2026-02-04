//! Semantic color mappings for song sections.
//!
//! This module provides consistent color assignments for song structure
//! elements like verses, choruses, bridges, etc. Each section type has
//! three color variants:
//! - **Bright**: For active elements, progress bars, borders
//! - **Muted**: For backgrounds, inactive states
//! - **Text**: For text displayed on muted backgrounds

use crate::{palette, Color};
use facet::Facet;

/// Color set for a song section with bright, muted, and text variants.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Facet)]
pub struct SectionColors {
    /// Bright color for active elements, progress bars, borders
    pub bright: Color,
    /// Muted color for backgrounds, inactive states
    pub muted: Color,
    /// Text color for use on muted backgrounds
    pub text: Color,
}

impl SectionColors {
    /// Create a new section color set.
    pub const fn new(bright: Color, muted: Color, text: Color) -> Self {
        Self {
            bright,
            muted,
            text,
        }
    }

    /// Get the bright color as a CSS hex string.
    pub fn bright_css(&self) -> String {
        self.bright.to_hex_string()
    }

    /// Get the muted color as a CSS hex string.
    pub fn muted_css(&self) -> String {
        self.muted.to_hex_string()
    }

    /// Get the text color as a CSS hex string.
    pub fn text_css(&self) -> String {
        self.text.to_hex_string()
    }

    /// Get the bright color as a CSS rgb() string.
    pub fn bright_rgb(&self) -> String {
        self.bright.to_css_rgb()
    }

    /// Get the muted color as a CSS rgb() string.
    pub fn muted_rgb(&self) -> String {
        self.muted.to_css_rgb()
    }
}

/// Song section types with semantic meaning.
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Facet)]
pub enum SectionType {
    /// Count-in before the song starts
    CountIn,
    /// Introduction
    Intro,
    /// Main verse section
    Verse,
    /// Pre-chorus / build-up
    PreChorus,
    /// Main chorus / hook
    Chorus,
    /// Bridge section
    Bridge,
    /// Outro / ending
    Outro,
    /// Solo section
    Solo,
    /// Breakdown section
    Breakdown,
    /// Instrumental section
    Instrumental,
    /// Interlude between sections
    Interlude,
    /// Vamp / repeated section
    Vamp,
    /// End marker
    End,
    /// Other / unknown section type
    Other,
}

impl SectionType {
    /// Parse a section type from a string.
    ///
    /// Returns `SectionType::Other` for unrecognized strings.
    pub fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "count-in" | "countin" | "count in" | "count" => Self::CountIn,
            "intro" | "introduction" => Self::Intro,
            "verse" | "v" => Self::Verse,
            "pre-chorus" | "prechorus" | "pre chorus" | "pre" => Self::PreChorus,
            "chorus" | "c" | "hook" => Self::Chorus,
            "bridge" | "b" => Self::Bridge,
            "outro" | "ending" | "end" => Self::Outro,
            "solo" => Self::Solo,
            "breakdown" | "break" => Self::Breakdown,
            "instrumental" | "inst" => Self::Instrumental,
            "interlude" | "inter" => Self::Interlude,
            "vamp" | "loop" => Self::Vamp,
            _ => Self::Other,
        }
    }

    /// Get the display name for this section type.
    pub fn display_name(&self) -> &'static str {
        match self {
            Self::CountIn => "Count-In",
            Self::Intro => "Intro",
            Self::Verse => "Verse",
            Self::PreChorus => "Pre-Chorus",
            Self::Chorus => "Chorus",
            Self::Bridge => "Bridge",
            Self::Outro => "Outro",
            Self::Solo => "Solo",
            Self::Breakdown => "Breakdown",
            Self::Instrumental => "Instrumental",
            Self::Interlude => "Interlude",
            Self::Vamp => "Vamp",
            Self::End => "End",
            Self::Other => "Section",
        }
    }
}

/// Get the colors for a section type.
pub fn section_colors(section_type: SectionType) -> SectionColors {
    match section_type {
        SectionType::CountIn => SectionColors::new(
            palette::pink::S400, // bright
            palette::pink::S800, // muted
            palette::pink::S100, // text
        ),
        SectionType::Intro => SectionColors::new(
            palette::blue::S500,
            palette::blue::S900,
            palette::blue::S100,
        ),
        SectionType::Verse => SectionColors::new(
            palette::green::S500,
            palette::green::S900,
            palette::green::S100,
        ),
        SectionType::PreChorus => SectionColors::new(
            palette::yellow::S500,
            palette::yellow::S900,
            palette::yellow::S100,
        ),
        SectionType::Chorus => {
            SectionColors::new(palette::red::S500, palette::red::S900, palette::red::S100)
        }
        SectionType::Bridge => SectionColors::new(
            palette::purple::S500,
            palette::purple::S900,
            palette::purple::S100,
        ),
        SectionType::Outro => SectionColors::new(
            palette::indigo::S500,
            palette::indigo::S900,
            palette::indigo::S100,
        ),
        SectionType::Solo => SectionColors::new(
            palette::orange::S500,
            palette::orange::S900,
            palette::orange::S100,
        ),
        SectionType::Breakdown => SectionColors::new(
            palette::pink::S500,
            palette::pink::S900,
            palette::pink::S100,
        ),
        SectionType::Instrumental => SectionColors::new(
            palette::teal::S500,
            palette::teal::S900,
            palette::teal::S100,
        ),
        SectionType::Interlude => SectionColors::new(
            palette::cyan::S500,
            palette::cyan::S900,
            palette::cyan::S100,
        ),
        SectionType::Vamp => SectionColors::new(
            palette::lime::S500,
            palette::lime::S900,
            palette::lime::S100,
        ),
        SectionType::End => SectionColors::new(
            palette::gray::S700,
            palette::gray::S800,
            palette::gray::S200,
        ),
        SectionType::Other => SectionColors::new(
            palette::slate::S500,
            palette::slate::S700,
            palette::slate::S200,
        ),
    }
}

/// Get the bright color for a section type.
pub fn bright_color(section_type: SectionType) -> Color {
    section_colors(section_type).bright
}

/// Get the muted color for a section type.
pub fn muted_color(section_type: SectionType) -> Color {
    section_colors(section_type).muted
}

/// Get the colors for a section name string.
///
/// Parses the section name and returns the appropriate colors.
pub fn colors_for_section_name(name: &str) -> SectionColors {
    section_colors(SectionType::from_str(name))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_section_type_from_str() {
        assert_eq!(SectionType::from_str("verse"), SectionType::Verse);
        assert_eq!(SectionType::from_str("Chorus"), SectionType::Chorus);
        assert_eq!(SectionType::from_str("BRIDGE"), SectionType::Bridge);
        assert_eq!(SectionType::from_str("pre-chorus"), SectionType::PreChorus);
        assert_eq!(SectionType::from_str("unknown"), SectionType::Other);
    }

    #[test]
    fn test_section_colors() {
        let verse = section_colors(SectionType::Verse);
        assert_eq!(verse.bright, palette::green::S500);

        let chorus = section_colors(SectionType::Chorus);
        assert_eq!(chorus.bright, palette::red::S500);
    }

    #[test]
    fn test_css_output() {
        let verse = section_colors(SectionType::Verse);
        assert!(verse.bright_css().starts_with('#'));
        assert!(verse.bright_rgb().starts_with("rgb("));
    }
}
