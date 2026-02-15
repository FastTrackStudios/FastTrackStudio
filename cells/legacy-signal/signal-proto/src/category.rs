//! Preset Category — 5-level hierarchy where invalid level skipping is impossible.
//!
//! The old flat struct allowed `archetype: Some(..)` with `genre: None` — a broken
//! hierarchy. This enum encodes the rule that each level requires all levels below it.
//!
//! ```text
//! Level 1: BaseTone only          (Clean, Lead, Drive, ...)
//! Level 2: + Genre                (Blues Lead, Jazz Clean, ...)
//! Level 3: + Sub-Genre            (Nu-Metal Drive, Classic Rock Crunch, ...)
//! Level 4: + Archetype            (John Mayer Blues Lead, ...)
//! Level 5: + Song                 (Gravity John Mayer Blues Lead, ...)
//! ```
//!
//! Fallback: Level 5 → 4 → 2 → 1 (skipping sub-genre if not set).

use std::fmt;

// ─────────────────────────────────────────────────────────────────────────────
// BaseTone (Level 1 — always required)
// ─────────────────────────────────────────────────────────────────────────────

/// Level 1: Generic base tone. Every preset has exactly one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::facet::Facet, Default)]
#[repr(u8)]
pub enum BaseTone {
    // Guitar
    #[default]
    Clean,
    Dry,
    Crunch,
    Drive,
    Lead,
    Solo,
    Ambient,
    DI,
    // Keyboard
    Piano,
    Organ,
    Synth,
    Pad,
    Strings,
    Brass,
    // Bass
    BassClean,
    BassDrive,
    // Vocal
    Natural,
    Warm,
    Bright,
    Breathy,
    Powerful,
}

impl BaseTone {
    pub fn as_str(self) -> &'static str {
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
            Self::Natural => "Natural",
            Self::Warm => "Warm",
            Self::Bright => "Bright",
            Self::Breathy => "Breathy",
            Self::Powerful => "Powerful",
        }
    }

    /// Parse from string (case-insensitive).
    pub fn parse(s: &str) -> Option<Self> {
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
            "natural" => Some(Self::Natural),
            "warm" => Some(Self::Warm),
            "bright" => Some(Self::Bright),
            "breathy" => Some(Self::Breathy),
            "powerful" => Some(Self::Powerful),
            _ => None,
        }
    }

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

    pub fn bass_tones() -> &'static [Self] {
        &[Self::BassClean, Self::BassDrive]
    }

    pub fn vocal_tones() -> &'static [Self] {
        &[
            Self::Natural,
            Self::Warm,
            Self::Bright,
            Self::Breathy,
            Self::Powerful,
        ]
    }
}

impl fmt::Display for BaseTone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Genre (Level 2)
// ─────────────────────────────────────────────────────────────────────────────

/// Level 2: Musical genre.
#[derive(Debug, Clone, PartialEq, Eq, Hash, ::facet::Facet)]
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
    Custom(String),
}

impl Genre {
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

    pub fn parse(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "blues" => Self::Blues,
            "jazz" => Self::Jazz,
            "rock" => Self::Rock,
            "metal" => Self::Metal,
            "country" => Self::Country,
            "pop" => Self::Pop,
            "worship" => Self::Worship,
            "funk" => Self::Funk,
            "r&b" | "rnb" => Self::RnB,
            "gospel" => Self::Gospel,
            "folk" => Self::Folk,
            "indie" => Self::Indie,
            "punk" => Self::Punk,
            "alternative" | "alt" => Self::Alternative,
            "classical" => Self::Classical,
            _ => Self::Custom(s.to_string()),
        }
    }
}

impl fmt::Display for Genre {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Archetype (Level 4)
// ─────────────────────────────────────────────────────────────────────────────

/// Level 4: Artist or reference archetype.
#[derive(Debug, Clone, PartialEq, Eq, Hash, ::facet::Facet)]
pub struct Archetype {
    pub name: String,
    pub era: Option<String>,
}

impl Archetype {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            era: None,
        }
    }

    pub fn with_era(name: impl Into<String>, era: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            era: Some(era.into()),
        }
    }

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

// ─────────────────────────────────────────────────────────────────────────────
// SongReference (Level 5)
// ─────────────────────────────────────────────────────────────────────────────

/// Level 5: Song-specific reference.
#[derive(Debug, Clone, PartialEq, Eq, Hash, ::facet::Facet)]
pub struct SongReference {
    pub name: String,
    pub album: Option<String>,
    pub part: Option<String>,
}

impl SongReference {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            album: None,
            part: None,
        }
    }

    pub fn with_album(name: impl Into<String>, album: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            album: Some(album.into()),
            part: None,
        }
    }

    pub fn with_part(name: impl Into<String>, part: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            album: None,
            part: Some(part.into()),
        }
    }

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

// ─────────────────────────────────────────────────────────────────────────────
// PresetCategory — the hierarchy-enforced enum
// ─────────────────────────────────────────────────────────────────────────────

/// 5-level hierarchical preset category.
///
/// Each variant structurally requires all lower levels to be present.
/// You **cannot** construct an `Archetype` without a `Genre`, or a `Song`
/// without an `Archetype` — the compiler rejects it.
///
/// # Fallback Chain
///
/// ```text
/// Song → Archetype → Genre → Generic
/// SubGenre → Genre → Generic
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, ::facet::Facet)]
#[repr(C)]
pub enum PresetCategory {
    /// Level 1: Base tone only
    Generic { base_tone: BaseTone },
    /// Level 2: Base tone + genre
    Genre { base_tone: BaseTone, genre: Genre },
    /// Level 3: Base tone + genre + sub-genre
    SubGenre {
        base_tone: BaseTone,
        genre: Genre,
        sub_genre: String,
    },
    /// Level 4: Base tone + genre + archetype (artist)
    Archetype {
        base_tone: BaseTone,
        genre: Genre,
        archetype: Archetype,
    },
    /// Level 5: Base tone + genre + archetype + song
    Song {
        base_tone: BaseTone,
        genre: Genre,
        archetype: Archetype,
        song: SongReference,
    },
}

impl PresetCategory {
    /// Get the base tone (always present).
    pub fn base_tone(&self) -> BaseTone {
        match self {
            Self::Generic { base_tone }
            | Self::Genre { base_tone, .. }
            | Self::SubGenre { base_tone, .. }
            | Self::Archetype { base_tone, .. }
            | Self::Song { base_tone, .. } => *base_tone,
        }
    }

    /// Get the genre (if present — Level 2+).
    pub fn genre(&self) -> Option<&Genre> {
        match self {
            Self::Generic { .. } => None,
            Self::Genre { genre, .. }
            | Self::SubGenre { genre, .. }
            | Self::Archetype { genre, .. }
            | Self::Song { genre, .. } => Some(genre),
        }
    }

    /// Get the sub-genre (if present — Level 3).
    pub fn sub_genre(&self) -> Option<&str> {
        match self {
            Self::SubGenre { sub_genre, .. } => Some(sub_genre),
            _ => None,
        }
    }

    /// Get the archetype (if present — Level 4+).
    pub fn archetype(&self) -> Option<&Archetype> {
        match self {
            Self::Archetype { archetype, .. } | Self::Song { archetype, .. } => Some(archetype),
            _ => None,
        }
    }

    /// Get the song reference (if present — Level 5).
    pub fn song(&self) -> Option<&SongReference> {
        match self {
            Self::Song { song, .. } => Some(song),
            _ => None,
        }
    }

    /// Get the hierarchy level (1–5).
    pub fn level(&self) -> u8 {
        match self {
            Self::Generic { .. } => 1,
            Self::Genre { .. } => 2,
            Self::SubGenre { .. } => 3,
            Self::Archetype { .. } => 4,
            Self::Song { .. } => 5,
        }
    }

    /// Get the fallback category (one level up).
    ///
    /// Returns `None` for Level 1 (no fallback).
    pub fn fallback(&self) -> Option<Self> {
        match self {
            Self::Generic { .. } => None,
            Self::Genre { base_tone, .. } => Some(Self::Generic {
                base_tone: *base_tone,
            }),
            Self::SubGenre {
                base_tone, genre, ..
            }
            | Self::Archetype {
                base_tone, genre, ..
            } => Some(Self::Genre {
                base_tone: *base_tone,
                genre: genre.clone(),
            }),
            Self::Song {
                base_tone,
                genre,
                archetype,
                ..
            } => Some(Self::Archetype {
                base_tone: *base_tone,
                genre: genre.clone(),
                archetype: archetype.clone(),
            }),
        }
    }

    /// Get the full fallback chain from most specific to generic.
    pub fn fallback_chain(&self) -> Vec<Self> {
        let mut chain = vec![self.clone()];
        let mut current = self.clone();
        while let Some(fb) = current.fallback() {
            chain.push(fb.clone());
            current = fb;
        }
        chain
    }

    /// Full display name: "[Song] [Archetype] [Sub-Genre] [Genre] [`BaseTone`]".
    pub fn display_name(&self) -> String {
        let mut parts = Vec::new();
        if let Some(song) = self.song() {
            parts.push(song.display_name());
        }
        if let Some(archetype) = self.archetype() {
            parts.push(archetype.name.clone());
        }
        if let Some(sub_genre) = self.sub_genre() {
            parts.push(sub_genre.to_string());
        }
        if let Some(genre) = self.genre() {
            parts.push(genre.as_str().to_string());
        }
        parts.push(self.base_tone().as_str().to_string());
        parts.join(" ")
    }

    /// Short display name (most specific part + base tone).
    pub fn short_name(&self) -> String {
        let prefix = match self {
            Self::Song { song, .. } => song.display_name(),
            Self::Archetype { archetype, .. } => archetype.name.clone(),
            Self::SubGenre { sub_genre, .. } => sub_genre.clone(),
            Self::Genre { genre, .. } => genre.as_str().to_string(),
            Self::Generic { base_tone } => return base_tone.as_str().to_string(),
        };
        format!("{} {}", prefix, self.base_tone())
    }

    /// Tree path for UI navigation.
    pub fn tree_path(&self) -> Vec<String> {
        let mut path = vec![self.base_tone().as_str().to_string()];
        if let Some(genre) = self.genre() {
            path.push(genre.as_str().to_string());
        }
        if let Some(sub_genre) = self.sub_genre() {
            path.push(sub_genre.to_string());
        }
        if let Some(archetype) = self.archetype() {
            path.push(archetype.name.clone());
        }
        if let Some(song) = self.song() {
            path.push(song.name.clone());
        }
        path
    }

    /// Check if `self` matches or is a parent of `other`.
    ///
    /// Returns true if `other` is equal to or more specific than `self`.
    pub fn matches(&self, other: &Self) -> bool {
        if self.base_tone() != other.base_tone() {
            return false;
        }
        match self {
            Self::Generic { .. } => true,
            Self::Genre { genre, .. } => other.genre() == Some(genre),
            Self::SubGenre {
                genre, sub_genre, ..
            } => other.genre() == Some(genre) && other.sub_genre() == Some(sub_genre.as_str()),
            Self::Archetype {
                genre, archetype, ..
            } => other.genre() == Some(genre) && other.archetype() == Some(archetype),
            Self::Song {
                genre,
                archetype,
                song,
                ..
            } => {
                other.genre() == Some(genre)
                    && other.archetype() == Some(archetype)
                    && other.song() == Some(song)
            }
        }
    }
}

impl Default for PresetCategory {
    fn default() -> Self {
        Self::Generic {
            base_tone: BaseTone::Clean,
        }
    }
}

impl fmt::Display for PresetCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_name())
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn levels() {
        let l1 = PresetCategory::Generic {
            base_tone: BaseTone::Lead,
        };
        let l2 = PresetCategory::Genre {
            base_tone: BaseTone::Lead,
            genre: Genre::Blues,
        };
        let l3 = PresetCategory::SubGenre {
            base_tone: BaseTone::Lead,
            genre: Genre::Metal,
            sub_genre: "Nu-Metal".into(),
        };
        let l4 = PresetCategory::Archetype {
            base_tone: BaseTone::Lead,
            genre: Genre::Blues,
            archetype: Archetype::new("John Mayer"),
        };
        let l5 = PresetCategory::Song {
            base_tone: BaseTone::Lead,
            genre: Genre::Blues,
            archetype: Archetype::new("John Mayer"),
            song: SongReference::new("Gravity"),
        };

        assert_eq!(l1.level(), 1);
        assert_eq!(l2.level(), 2);
        assert_eq!(l3.level(), 3);
        assert_eq!(l4.level(), 4);
        assert_eq!(l5.level(), 5);
    }

    #[test]
    fn fallback_chain_song_to_generic() {
        let cat = PresetCategory::Song {
            base_tone: BaseTone::Lead,
            genre: Genre::Blues,
            archetype: Archetype::new("John Mayer"),
            song: SongReference::new("Gravity"),
        };

        let chain = cat.fallback_chain();
        assert_eq!(chain.len(), 4); // Song, Archetype, Genre, Generic
        assert_eq!(chain[0].level(), 5);
        assert_eq!(chain[1].level(), 4);
        assert_eq!(chain[2].level(), 2);
        assert_eq!(chain[3].level(), 1);
    }

    #[test]
    fn fallback_subgenre_to_genre_to_generic() {
        let cat = PresetCategory::SubGenre {
            base_tone: BaseTone::Drive,
            genre: Genre::Metal,
            sub_genre: "Thrash".into(),
        };

        let chain = cat.fallback_chain();
        assert_eq!(chain.len(), 3);
        assert_eq!(chain[0].level(), 3);
        assert_eq!(chain[1].level(), 2);
        assert_eq!(chain[2].level(), 1);
    }

    #[test]
    fn generic_has_no_fallback() {
        let cat = PresetCategory::Generic {
            base_tone: BaseTone::Clean,
        };
        assert!(cat.fallback().is_none());
        assert_eq!(cat.fallback_chain().len(), 1);
    }

    #[test]
    fn display_names() {
        let cat = PresetCategory::Song {
            base_tone: BaseTone::Lead,
            genre: Genre::Blues,
            archetype: Archetype::new("John Mayer"),
            song: SongReference::new("Gravity"),
        };
        assert!(cat.display_name().contains("Gravity"));
        assert!(cat.display_name().contains("John Mayer"));
        assert!(cat.display_name().contains("Lead"));
        assert_eq!(cat.short_name(), "Gravity Lead");
    }

    #[test]
    fn tree_path() {
        let cat = PresetCategory::Archetype {
            base_tone: BaseTone::Clean,
            genre: Genre::Blues,
            archetype: Archetype::new("SRV"),
        };
        assert_eq!(cat.tree_path(), vec!["Clean", "Blues", "SRV"]);
    }

    #[test]
    fn matches_parent_child() {
        let generic = PresetCategory::Generic {
            base_tone: BaseTone::Lead,
        };
        let blues_lead = PresetCategory::Genre {
            base_tone: BaseTone::Lead,
            genre: Genre::Blues,
        };
        let jm_lead = PresetCategory::Archetype {
            base_tone: BaseTone::Lead,
            genre: Genre::Blues,
            archetype: Archetype::new("John Mayer"),
        };

        // Generic matches more specific
        assert!(generic.matches(&blues_lead));
        assert!(generic.matches(&jm_lead));

        // Genre matches more specific
        assert!(blues_lead.matches(&jm_lead));

        // More specific does NOT match less specific
        assert!(!jm_lead.matches(&blues_lead));
        assert!(!blues_lead.matches(&generic));
    }

    #[test]
    fn different_base_tone_never_matches() {
        let clean = PresetCategory::Generic {
            base_tone: BaseTone::Clean,
        };
        let lead = PresetCategory::Generic {
            base_tone: BaseTone::Lead,
        };
        assert!(!clean.matches(&lead));
    }

    #[test]
    fn base_tone_accessor() {
        let cat = PresetCategory::Song {
            base_tone: BaseTone::Drive,
            genre: Genre::Rock,
            archetype: Archetype::new("EVH"),
            song: SongReference::new("Eruption"),
        };
        assert_eq!(cat.base_tone(), BaseTone::Drive);
    }
}
