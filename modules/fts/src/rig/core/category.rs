//! Preset Category - 5-level hierarchical category system
//!
//! Provides a hierarchical preset organization with intelligent fallback:
//! 1. Generic (Base Tones): Clean, Dry, Crunch, Drive, Lead, Solo, Ambient, DI
//! 2. Genre: Blues, Jazz, Rock, Metal, Worship, Country, Pop
//! 3. Sub-Genre: Nu-Metal, Thrash Metal, Punk Rock, Classic Rock
//! 4. Archetype (Artist): John Mayer, SRV, Eddie Van Halen
//! 5. Song-Specific: Gravity, Lenny, Eruption

use std::fmt;

use facet::Facet;
use serde::{Deserialize, Serialize};

/// 5-level hierarchical preset category with fallback chain support.
///
/// # Hierarchy
/// - Level 1: Generic base tones (always present, the fallback layer)
/// - Level 2: Genre (Blues, Jazz, Rock, Metal, etc.)
/// - Level 3: Sub-genre (Nu-Metal, Classic Rock, etc.)
/// - Level 4: Archetype/Artist (John Mayer, SRV, etc.)
/// - Level 5: Song-specific (Gravity, Lenny, etc.)
///
/// # Fallback Example
/// When "Gravity Lead" is requested but unavailable:
/// 1. Try "John Mayer Lead" (archetype)
/// 2. Try "Blues Lead" (genre)
/// 3. Try "Lead" (generic)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub struct PresetCategory {
    /// Level 1: Base tone type (required)
    pub base_tone: BaseTone,
    /// Level 2: Genre (optional)
    pub genre: Option<Genre>,
    /// Level 3: Sub-genre (optional)
    pub sub_genre: Option<String>,
    /// Level 4: Archetype/Artist reference (optional)
    pub archetype: Option<Archetype>,
    /// Level 5: Song-specific identifier (optional)
    pub song: Option<SongReference>,
}

/// Level 1: Generic base tones (the fallback layer)
///
/// These are the most basic tone categories that every preset ultimately
/// falls back to. They represent fundamental guitar/keyboard sounds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Facet, Default)]
#[repr(u8)]
pub enum BaseTone {
    // Guitar tones
    #[default]
    Clean,
    Dry,
    Crunch,
    Drive,
    Lead,
    Solo,
    Ambient,
    DI,
    // Keyboard tones
    Piano,
    Organ,
    Synth,
    Pad,
    Strings,
    Brass,
    // Bass tones
    BassClean,
    BassDrive,
}

/// Level 2: Musical genres
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum Genre {
    Blues,
    Jazz,
    Rock,
    Metal,
    Country,
    Pop,
    Worship,
    Funk,
    RnB,
    Gospel,
    Folk,
    Indie,
    Punk,
    Alternative,
    Classical,
    /// Custom genre for user-defined categories
    Custom(String),
}

/// Level 4: Archetype - an artist or specific reference
///
/// Archetypes represent a specific player's tone or a well-known
/// reference point (e.g., "John Mayer Continuum Era" or "SRV Texas Flood").
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub struct Archetype {
    /// Artist or reference name (e.g., "John Mayer", "SRV")
    pub name: String,
    /// Optional era or album reference (e.g., "Continuum Era", "Texas Flood")
    pub era: Option<String>,
}

/// Level 5: Song-specific reference
///
/// The most specific level - presets designed for a particular song.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub struct SongReference {
    /// Song name
    pub name: String,
    /// Optional album
    pub album: Option<String>,
    /// Part within song (e.g., "Intro Solo", "Verse", "Outro")
    pub part: Option<String>,
}

// ============================================================================
// BaseTone Implementation
// ============================================================================

impl BaseTone {
    /// Get the display string for this base tone
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Clean => "Clean",
            Self::Dry => "Dry",
            Self::Crunch => "Crunch",
            Self::Drive => "Drive",
            Self::Lead => "Lead",
            Self::Solo => "Solo",
            Self::Ambient => "Ambient",
            Self::DI => "DI",
            Self::Piano => "Piano",
            Self::Organ => "Organ",
            Self::Synth => "Synth",
            Self::Pad => "Pad",
            Self::Strings => "Strings",
            Self::Brass => "Brass",
            Self::BassClean => "Bass Clean",
            Self::BassDrive => "Bass Drive",
        }
    }

    /// Parse a base tone from a string (case-insensitive)
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "clean" => Some(Self::Clean),
            "dry" => Some(Self::Dry),
            "crunch" => Some(Self::Crunch),
            "drive" => Some(Self::Drive),
            "lead" => Some(Self::Lead),
            "solo" => Some(Self::Solo),
            "ambient" => Some(Self::Ambient),
            "di" => Some(Self::DI),
            "piano" => Some(Self::Piano),
            "organ" => Some(Self::Organ),
            "synth" => Some(Self::Synth),
            "pad" => Some(Self::Pad),
            "strings" => Some(Self::Strings),
            "brass" => Some(Self::Brass),
            "bass clean" | "bassclean" => Some(Self::BassClean),
            "bass drive" | "bassdrive" => Some(Self::BassDrive),
            _ => None,
        }
    }

    /// Get all guitar-related base tones
    pub fn guitar_tones() -> &'static [Self] {
        &[
            Self::Clean,
            Self::Dry,
            Self::Crunch,
            Self::Drive,
            Self::Lead,
            Self::Solo,
            Self::Ambient,
            Self::DI,
        ]
    }

    /// Get all keyboard-related base tones
    pub fn keyboard_tones() -> &'static [Self] {
        &[
            Self::Piano,
            Self::Organ,
            Self::Synth,
            Self::Pad,
            Self::Strings,
            Self::Brass,
        ]
    }

    /// Get all bass-related base tones
    pub fn bass_tones() -> &'static [Self] {
        &[Self::BassClean, Self::BassDrive]
    }
}

impl fmt::Display for BaseTone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

// ============================================================================
// Genre Implementation
// ============================================================================

impl Genre {
    /// Get the display string for this genre
    pub fn as_str(&self) -> &str {
        match self {
            Self::Blues => "Blues",
            Self::Jazz => "Jazz",
            Self::Rock => "Rock",
            Self::Metal => "Metal",
            Self::Country => "Country",
            Self::Pop => "Pop",
            Self::Worship => "Worship",
            Self::Funk => "Funk",
            Self::RnB => "R&B",
            Self::Gospel => "Gospel",
            Self::Folk => "Folk",
            Self::Indie => "Indie",
            Self::Punk => "Punk",
            Self::Alternative => "Alternative",
            Self::Classical => "Classical",
            Self::Custom(s) => s.as_str(),
        }
    }

    /// Parse a genre from a string (case-insensitive)
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "blues" => Some(Self::Blues),
            "jazz" => Some(Self::Jazz),
            "rock" => Some(Self::Rock),
            "metal" => Some(Self::Metal),
            "country" => Some(Self::Country),
            "pop" => Some(Self::Pop),
            "worship" => Some(Self::Worship),
            "funk" => Some(Self::Funk),
            "r&b" | "rnb" => Some(Self::RnB),
            "gospel" => Some(Self::Gospel),
            "folk" => Some(Self::Folk),
            "indie" => Some(Self::Indie),
            "punk" => Some(Self::Punk),
            "alternative" | "alt" => Some(Self::Alternative),
            "classical" => Some(Self::Classical),
            _ => Some(Self::Custom(s.to_string())),
        }
    }
}

impl fmt::Display for Genre {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

// ============================================================================
// Archetype Implementation
// ============================================================================

impl Archetype {
    /// Create a new archetype
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            era: None,
        }
    }

    /// Create an archetype with era/album reference
    pub fn with_era(name: impl Into<String>, era: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            era: Some(era.into()),
        }
    }

    /// Display name including era if present
    pub fn display_name(&self) -> String {
        match &self.era {
            Some(era) => format!("{} ({})", self.name, era),
            None => self.name.clone(),
        }
    }
}

impl fmt::Display for Archetype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_name())
    }
}

// ============================================================================
// SongReference Implementation
// ============================================================================

impl SongReference {
    /// Create a new song reference
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            album: None,
            part: None,
        }
    }

    /// Create a song reference with album
    pub fn with_album(name: impl Into<String>, album: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            album: Some(album.into()),
            part: None,
        }
    }

    /// Create a song reference with part (e.g., "Intro", "Solo")
    pub fn with_part(name: impl Into<String>, part: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            album: None,
            part: Some(part.into()),
        }
    }

    /// Display name including part if present
    pub fn display_name(&self) -> String {
        match &self.part {
            Some(part) => format!("{} - {}", self.name, part),
            None => self.name.clone(),
        }
    }
}

impl fmt::Display for SongReference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_name())
    }
}

// ============================================================================
// PresetCategory Implementation
// ============================================================================

impl PresetCategory {
    /// Create a Level 1 category (generic base tone only)
    pub fn generic(base_tone: BaseTone) -> Self {
        Self {
            base_tone,
            genre: None,
            sub_genre: None,
            archetype: None,
            song: None,
        }
    }

    /// Create a Level 2 category (base tone + genre)
    pub fn genre(base_tone: BaseTone, genre: Genre) -> Self {
        Self {
            base_tone,
            genre: Some(genre),
            sub_genre: None,
            archetype: None,
            song: None,
        }
    }

    /// Create a Level 3 category (base tone + genre + sub-genre)
    pub fn sub_genre(
        base_tone: BaseTone,
        genre: Genre,
        sub_genre: impl Into<String>,
    ) -> Self {
        Self {
            base_tone,
            genre: Some(genre),
            sub_genre: Some(sub_genre.into()),
            archetype: None,
            song: None,
        }
    }

    /// Create a Level 4 category (with archetype)
    pub fn archetype(
        base_tone: BaseTone,
        genre: Genre,
        archetype: Archetype,
    ) -> Self {
        Self {
            base_tone,
            genre: Some(genre),
            sub_genre: None,
            archetype: Some(archetype),
            song: None,
        }
    }

    /// Create a Level 5 category (with song reference)
    pub fn song(
        base_tone: BaseTone,
        genre: Genre,
        archetype: Archetype,
        song: SongReference,
    ) -> Self {
        Self {
            base_tone,
            genre: Some(genre),
            sub_genre: None,
            archetype: Some(archetype),
            song: Some(song),
        }
    }

    /// Get the hierarchy level (1-5)
    pub fn level(&self) -> u8 {
        if self.song.is_some() {
            5
        } else if self.archetype.is_some() {
            4
        } else if self.sub_genre.is_some() {
            3
        } else if self.genre.is_some() {
            2
        } else {
            1
        }
    }

    /// Get the fallback category (one level up the hierarchy)
    ///
    /// Returns `None` for Level 1 categories (generic has no fallback).
    ///
    /// # Examples
    /// - Level 5 (Song) → Level 4 (Archetype)
    /// - Level 4 (Archetype) → Level 2 (Genre) - skips sub-genre if not set
    /// - Level 2 (Genre) → Level 1 (Generic)
    pub fn fallback(&self) -> Option<Self> {
        match self.level() {
            5 => Some(Self {
                song: None,
                ..self.clone()
            }),
            4 => Some(Self {
                archetype: None,
                song: None,
                ..self.clone()
            }),
            3 => Some(Self {
                sub_genre: None,
                archetype: None,
                song: None,
                ..self.clone()
            }),
            2 => Some(Self {
                genre: None,
                sub_genre: None,
                archetype: None,
                song: None,
                ..self.clone()
            }),
            _ => None, // Level 1 has no fallback
        }
    }

    /// Get the full fallback chain from most specific to generic
    ///
    /// Returns a vector starting with `self` and ending with the Level 1 category.
    pub fn fallback_chain(&self) -> Vec<Self> {
        let mut chain = vec![self.clone()];
        let mut current = self.clone();
        while let Some(fallback) = current.fallback() {
            chain.push(fallback.clone());
            current = fallback;
        }
        chain
    }

    /// Get the display name for this category
    ///
    /// Format: "[Song] [Archetype] [Sub-Genre] [Genre] [BaseTone]"
    /// e.g., "Gravity John Mayer Blues Lead"
    pub fn display_name(&self) -> String {
        let mut parts = Vec::new();

        if let Some(song) = &self.song {
            parts.push(song.display_name());
        }
        if let Some(archetype) = &self.archetype {
            parts.push(archetype.name.clone());
        }
        if let Some(sub_genre) = &self.sub_genre {
            parts.push(sub_genre.clone());
        }
        if let Some(genre) = &self.genre {
            parts.push(genre.as_str().to_string());
        }
        parts.push(self.base_tone.as_str().to_string());

        parts.join(" ")
    }

    /// Get the short display name (just the most specific part + base tone)
    ///
    /// e.g., "Gravity Lead" instead of "Gravity John Mayer Blues Lead"
    pub fn short_name(&self) -> String {
        let prefix = if let Some(song) = &self.song {
            song.display_name()
        } else if let Some(archetype) = &self.archetype {
            archetype.name.clone()
        } else if let Some(sub_genre) = &self.sub_genre {
            sub_genre.clone()
        } else if let Some(genre) = &self.genre {
            genre.as_str().to_string()
        } else {
            return self.base_tone.as_str().to_string();
        };

        format!("{} {}", prefix, self.base_tone)
    }

    /// Get the tree path for UI navigation
    ///
    /// Returns a vector of strings representing the path in the tree.
    /// e.g., ["Lead", "Blues", "John Mayer", "Gravity"]
    pub fn tree_path(&self) -> Vec<String> {
        let mut path = vec![self.base_tone.as_str().to_string()];

        if let Some(genre) = &self.genre {
            path.push(genre.as_str().to_string());
        }
        if let Some(sub_genre) = &self.sub_genre {
            path.push(sub_genre.clone());
        }
        if let Some(archetype) = &self.archetype {
            path.push(archetype.name.clone());
        }
        if let Some(song) = &self.song {
            path.push(song.name.clone());
        }

        path
    }

    /// Check if this category matches or is a parent of another
    ///
    /// Returns true if `other` is equal to or more specific than `self`.
    pub fn matches(&self, other: &Self) -> bool {
        // Must have same base tone
        if self.base_tone != other.base_tone {
            return false;
        }

        // Check each level - self must be equal or less specific
        if self.genre.is_some() && self.genre != other.genre {
            return false;
        }
        if self.sub_genre.is_some() && self.sub_genre != other.sub_genre {
            return false;
        }
        if self.archetype.is_some() && self.archetype != other.archetype {
            return false;
        }
        if self.song.is_some() && self.song != other.song {
            return false;
        }

        true
    }
}

impl Default for PresetCategory {
    fn default() -> Self {
        Self::generic(BaseTone::Clean)
    }
}

impl fmt::Display for PresetCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_name())
    }
}

// ============================================================================
// Conversion from Legacy PatchCategory
// ============================================================================

use super::patch::PatchCategory as LegacyPatchCategory;

impl From<LegacyPatchCategory> for PresetCategory {
    fn from(legacy: LegacyPatchCategory) -> Self {
        let base_tone = BaseTone::from_str(&legacy.base).unwrap_or(BaseTone::Clean);
        let genre = legacy.style.and_then(|s| Genre::from_str(&s));

        Self {
            base_tone,
            genre,
            sub_genre: None,
            archetype: None,
            song: None,
        }
    }
}

impl From<&LegacyPatchCategory> for PresetCategory {
    fn from(legacy: &LegacyPatchCategory) -> Self {
        let base_tone = BaseTone::from_str(&legacy.base).unwrap_or(BaseTone::Clean);
        let genre = legacy.style.as_ref().and_then(|s| Genre::from_str(s));

        Self {
            base_tone,
            genre,
            sub_genre: None,
            archetype: None,
            song: None,
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_category_levels() {
        let generic = PresetCategory::generic(BaseTone::Lead);
        assert_eq!(generic.level(), 1);

        let genre = PresetCategory::genre(BaseTone::Lead, Genre::Blues);
        assert_eq!(genre.level(), 2);

        let sub_genre = PresetCategory::sub_genre(BaseTone::Lead, Genre::Metal, "Nu-Metal");
        assert_eq!(sub_genre.level(), 3);

        let archetype = PresetCategory::archetype(
            BaseTone::Lead,
            Genre::Blues,
            Archetype::new("John Mayer"),
        );
        assert_eq!(archetype.level(), 4);

        let song = PresetCategory::song(
            BaseTone::Lead,
            Genre::Blues,
            Archetype::new("John Mayer"),
            SongReference::new("Gravity"),
        );
        assert_eq!(song.level(), 5);
    }

    #[test]
    fn test_fallback_chain() {
        let gravity_lead = PresetCategory::song(
            BaseTone::Lead,
            Genre::Blues,
            Archetype::new("John Mayer"),
            SongReference::new("Gravity"),
        );

        let chain = gravity_lead.fallback_chain();
        assert_eq!(chain.len(), 4); // Song, Archetype, Genre, Generic

        assert_eq!(chain[0].level(), 5); // Song
        assert_eq!(chain[1].level(), 4); // Archetype
        assert_eq!(chain[2].level(), 2); // Genre (skips sub-genre since not set)
        assert_eq!(chain[3].level(), 1); // Generic
    }

    #[test]
    fn test_display_names() {
        let category = PresetCategory::song(
            BaseTone::Lead,
            Genre::Blues,
            Archetype::new("John Mayer"),
            SongReference::new("Gravity"),
        );

        assert!(category.display_name().contains("Gravity"));
        assert!(category.display_name().contains("John Mayer"));
        assert!(category.display_name().contains("Lead"));

        assert_eq!(category.short_name(), "Gravity Lead");
    }

    #[test]
    fn test_tree_path() {
        let category = PresetCategory::archetype(
            BaseTone::Clean,
            Genre::Blues,
            Archetype::new("SRV"),
        );

        let path = category.tree_path();
        assert_eq!(path, vec!["Clean", "Blues", "SRV"]);
    }

    #[test]
    fn test_category_matches() {
        let generic = PresetCategory::generic(BaseTone::Lead);
        let blues_lead = PresetCategory::genre(BaseTone::Lead, Genre::Blues);
        let jm_lead = PresetCategory::archetype(
            BaseTone::Lead,
            Genre::Blues,
            Archetype::new("John Mayer"),
        );

        // Generic matches more specific categories
        assert!(generic.matches(&blues_lead));
        assert!(generic.matches(&jm_lead));

        // Genre matches more specific
        assert!(blues_lead.matches(&jm_lead));

        // More specific doesn't match less specific
        assert!(!jm_lead.matches(&blues_lead));
        assert!(!blues_lead.matches(&generic));
    }
}
