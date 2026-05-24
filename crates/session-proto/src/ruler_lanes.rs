//! FTS Ruler Manager — standardized ruler lane layout for FastTrackStudio projects.
//!
//! Every FTS project should have these ruler lanes to organize markers and regions
//! into semantic categories. The lanes are split into two groups:
//!
//! **Core lanes** (always present):
//! - SONG, SECTIONS, MARKS, KEY
//!
//! **Instrument note lanes** (created on demand per instrument):
//! - Drums, Bass, Guitar, Guitar 2, Keys, Keys 2, Lead, BGVs, etc.

use facet::Facet;

// ── Core Ruler Lanes ─────────────────────────────────────────────────────────

/// Core FTS ruler lanes — always present in every project.
///
/// `Key = 3` is reserved-but-retired. We keep the variant so the
/// `#[repr(u8)]` discriminants of Song/Sections/Marks don't shift
/// (which would silently re-number every existing project's lane
/// assignments). It's excluded from [`all`] so the lane is not
/// auto-created any more — key signatures are encoded elsewhere.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum CoreLane {
    /// Song-level markers/regions: SONGSTART, SONGEND, full-song region.
    /// **Default marker lane**.
    Song = 0,
    /// Regions for song sections (Verse, Chorus, Bridge, Outro, etc.).
    /// **Default region lane** — new regions go here.
    Sections = 1,
    /// Structural/general markers: Count-In, cues.
    Marks = 2,
    /// Reserved historical slot — see the enum doc.
    #[deprecated(note = "KEY lane retired; key signatures are encoded separately now")]
    Key = 3,
}

#[allow(deprecated)]
impl CoreLane {
    /// All core lanes that should be created in every project. `Key`
    /// is intentionally absent.
    pub const fn all() -> &'static [CoreLane] {
        &[CoreLane::Song, CoreLane::Sections, CoreLane::Marks]
    }

    /// REAPER ruler lane index (1-based).
    pub const fn lane_index(&self) -> u32 {
        *self as u32 + 1
    }

    /// Display name shown in the REAPER ruler.
    pub const fn display_name(&self) -> &'static str {
        match self {
            Self::Song => "SONG",
            Self::Sections => "SECTIONS",
            Self::Marks => "MARKS",
            Self::Key => "KEY",
        }
    }

    /// REAPER lane flags.
    /// - `8` = default region lane
    /// - `4` = default marker lane
    /// - `0` = normal
    pub const fn flags(&self) -> i32 {
        match self {
            Self::Sections => 8, // default region lane
            Self::Song => 4,     // default marker lane
            _ => 0,
        }
    }

    /// Reserved core-lane slot count. Stays at 4 even though Key is
    /// no longer auto-created — `InstrumentLane::lane_index` and the
    /// rest of the numbering offset off this value, and shifting it
    /// would silently renumber every existing project's instrument
    /// lanes.
    pub const fn count() -> u32 {
        4
    }

    pub fn from_index(index: u32) -> Option<Self> {
        Self::all()
            .iter()
            .find(|l| l.lane_index() == index)
            .copied()
    }

    pub fn from_name(name: &str) -> Option<Self> {
        let upper = name.to_uppercase();
        Self::all()
            .iter()
            .find(|l| l.display_name() == upper)
            .copied()
    }
}

// ── Instrument Note Lanes ────────────────────────────────────────────────────

/// Well-known instrument roles for per-instrument note lanes.
///
/// These are created on demand — when a user does "Insert Note for Drums",
/// the Drums lane is created if it doesn't exist yet.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum InstrumentLane {
    Drums = 0,
    Bass = 1,
    Guitar = 2,
    Guitar2 = 3,
    Keys = 4,
    Keys2 = 5,
    Lead = 6,
    BGVs = 7,
}

impl InstrumentLane {
    /// All well-known instrument lanes.
    pub const fn all() -> &'static [InstrumentLane] {
        &[
            InstrumentLane::Drums,
            InstrumentLane::Bass,
            InstrumentLane::Guitar,
            InstrumentLane::Guitar2,
            InstrumentLane::Keys,
            InstrumentLane::Keys2,
            InstrumentLane::Lead,
            InstrumentLane::BGVs,
        ]
    }

    /// REAPER ruler lane index (1-based), offset after core lanes.
    pub const fn lane_index(&self) -> u32 {
        CoreLane::count() + *self as u32 + 1
    }

    /// Display name shown in the REAPER ruler.
    pub const fn display_name(&self) -> &'static str {
        match self {
            Self::Drums => "Drums",
            Self::Bass => "Bass",
            Self::Guitar => "Guitar",
            Self::Guitar2 => "Guitar 2",
            Self::Keys => "Keys",
            Self::Keys2 => "Keys 2",
            Self::Lead => "Lead",
            Self::BGVs => "BGVs",
        }
    }

    pub fn from_name(name: &str) -> Option<Self> {
        Self::all()
            .iter()
            .find(|l| l.display_name().eq_ignore_ascii_case(name))
            .copied()
    }
}

// ── Unified Lane Reference ───────────────────────────────────────────────────

/// A reference to any FTS ruler lane (core or instrument).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(C)]
pub enum FtsLane {
    Core(CoreLane),
    Instrument(InstrumentLane),
}

impl FtsLane {
    pub fn lane_index(&self) -> u32 {
        match self {
            Self::Core(l) => l.lane_index(),
            Self::Instrument(l) => l.lane_index(),
        }
    }

    pub fn display_name(&self) -> &'static str {
        match self {
            Self::Core(l) => l.display_name(),
            Self::Instrument(l) => l.display_name(),
        }
    }

    pub fn flags(&self) -> i32 {
        match self {
            Self::Core(l) => l.flags(),
            Self::Instrument(_) => 0,
        }
    }
}

// ── Marker Classification ────────────────────────────────────────────────────

/// Classify a marker name to determine which lane it belongs to.
pub fn classify_marker_lane(name: &str) -> FtsLane {
    let trimmed = name.trim();
    let upper = trimmed.to_uppercase();

    match upper.as_str() {
        // SONG lane: song-level bounds/anchors
        "SONGSTART" | "SONGEND" => FtsLane::Core(CoreLane::Song),

        // MARKS lane: structural cues
        "COUNT-IN" | "COUNT IN" | "COUNTIN" => FtsLane::Core(CoreLane::Marks),
        // MARKS lane: render/release bounds
        "=START" | "=END" | "PREROLL" | "=PREROLL" => FtsLane::Core(CoreLane::Marks),
        _ => FtsLane::Core(CoreLane::Marks),
    }
}

/// Classify a region name to determine which lane it belongs to.
///
/// Looks at both the region name and whether it spans the full song
/// (in which case it's a SONG region, not a section).
pub fn classify_region_lane(name: &str) -> FtsLane {
    let upper = name.trim().to_uppercase();

    // Well-known section abbreviations → SECTIONS lane
    match upper.as_str() {
        "VS" | "VERSE" | "CH" | "CHORUS" | "BR" | "BRIDGE" | "INTRO" | "OUTRO" | "PRE-CH"
        | "PRE-CHORUS" | "PRECHORUS" | "SOLO" | "BREAKDOWN" | "INTERLUDE" | "TAG" | "HOOK"
        | "INSTRUMENTAL" | "CODA" | "VAMP" | "TURNAROUND" | "VERSE 1" | "VERSE 2" | "VERSE 3"
        | "VERSE 4" | "CHORUS 1" | "CHORUS 2" | "CHORUS 3" | "BRIDGE 1" | "BRIDGE 2" => {
            return FtsLane::Core(CoreLane::Sections);
        }
        _ => {}
    }

    // Section abbreviations with numbers: "VS 1", "CH 2", etc.
    if upper.starts_with("VS ")
        || upper.starts_with("CH ")
        || upper.starts_with("BR ")
        || upper.starts_with("SOLO ")
    {
        return FtsLane::Core(CoreLane::Sections);
    }

    // Default: SECTIONS lane (most regions are sections)
    FtsLane::Core(CoreLane::Sections)
}

/// Classify a region that spans the full song as a SONG region.
///
/// Call this with `is_full_song = true` when the region's start matches
/// the song start and its end matches the song end.
pub fn classify_region_lane_with_context(name: &str, is_full_song: bool) -> FtsLane {
    if is_full_song {
        FtsLane::Core(CoreLane::Song)
    } else {
        classify_region_lane(name)
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn core_lane_indices_sequential() {
        for (i, lane) in CoreLane::all().iter().enumerate() {
            assert_eq!(lane.lane_index(), i as u32 + 1);
        }
    }

    #[test]
    fn instrument_lanes_start_after_core() {
        assert_eq!(InstrumentLane::Drums.lane_index(), 5);
        assert_eq!(InstrumentLane::Bass.lane_index(), 6);
        assert_eq!(InstrumentLane::Guitar.lane_index(), 7);
        assert_eq!(InstrumentLane::BGVs.lane_index(), 12);
    }

    #[test]
    fn fts_lane_unified_index() {
        assert_eq!(FtsLane::Core(CoreLane::Song).lane_index(), 1);
        assert_eq!(FtsLane::Core(CoreLane::Sections).lane_index(), 2);
        assert_eq!(FtsLane::Core(CoreLane::Marks).lane_index(), 3);
        assert_eq!(FtsLane::Instrument(InstrumentLane::Drums).lane_index(), 5);
    }

    #[test]
    fn classify_structural_markers() {
        assert_eq!(
            classify_marker_lane("SONGSTART"),
            FtsLane::Core(CoreLane::Song)
        );
        assert_eq!(
            classify_marker_lane("SONGEND"),
            FtsLane::Core(CoreLane::Song)
        );
        assert_eq!(
            classify_marker_lane("Count-In"),
            FtsLane::Core(CoreLane::Marks)
        );
    }

    #[test]
    fn classify_bound_markers() {
        assert_eq!(
            classify_marker_lane("=START"),
            FtsLane::Core(CoreLane::Marks)
        );
        assert_eq!(classify_marker_lane("=END"), FtsLane::Core(CoreLane::Marks));
        assert_eq!(
            classify_marker_lane("PREROLL"),
            FtsLane::Core(CoreLane::Marks)
        );
    }

    #[test]
    fn default_region_and_marker_lanes() {
        let region_defaults: Vec<_> = CoreLane::all()
            .iter()
            .filter(|l| l.flags() & 8 != 0)
            .collect();
        let marker_defaults: Vec<_> = CoreLane::all()
            .iter()
            .filter(|l| l.flags() & 4 != 0)
            .collect();
        assert_eq!(region_defaults.len(), 1);
        assert_eq!(marker_defaults.len(), 1);
        assert_eq!(region_defaults[0].display_name(), "SECTIONS");
        assert_eq!(marker_defaults[0].display_name(), "SONG");
    }

    #[test]
    fn lane_layout_matches_reaper_rpp() {
        // FTS lane convention:
        // RULERLANE 1 4 SONG 0 -1
        // RULERLANE 2 8 SECTIONS 0 -1
        // RULERLANE 3 0 MARKS 0 -1
        // RULERLANE 4 0 KEY 0 -1
        // RULERLANE 5 0 Drums 0 -1
        // ...
        assert_eq!(CoreLane::Song.lane_index(), 1);
        assert_eq!(CoreLane::Sections.lane_index(), 2);
        assert_eq!(CoreLane::Marks.lane_index(), 3);
        assert_eq!(CoreLane::Key.lane_index(), 4);
        assert_eq!(InstrumentLane::Drums.lane_index(), 5);
        assert_eq!(InstrumentLane::Bass.lane_index(), 6);
        assert_eq!(InstrumentLane::Guitar.lane_index(), 7);
        assert_eq!(InstrumentLane::Guitar2.lane_index(), 8);
        assert_eq!(InstrumentLane::Keys.lane_index(), 9);
        assert_eq!(InstrumentLane::Keys2.lane_index(), 10);
        assert_eq!(InstrumentLane::Lead.lane_index(), 11);
        assert_eq!(InstrumentLane::BGVs.lane_index(), 12);
    }
}
