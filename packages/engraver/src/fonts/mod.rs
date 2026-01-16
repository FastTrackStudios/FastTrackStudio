//! SMuFL font loading and glyph management.
//!
//! This module handles loading SMuFL-compliant fonts (like Bravura) and
//! provides access to glyph metrics and anchor points for precise positioning.
//!
//! Uses the `smufl` crate for proper metadata parsing.

pub mod tessellation;

use skrifa::FontRef;
use std::io::BufReader;
use std::path::Path;

// Re-export key types from smufl crate
pub use smufl::{Glyph, Metadata as SMuFLMetadata, StaffSpaces};

// Re-export tessellation utilities
pub use tessellation::{
    get_glyph_id, tessellate_glyph, tessellate_glyph_to_ndc, GlyphVertex, GlyphVertexConstructor,
    LyonPen, TessellatedGlyph,
};

/// A loaded SMuFL font with its metadata.
pub struct SMuFLFont<'a> {
    /// The font data for rendering
    font: FontRef<'a>,
    /// SMuFL metadata (bounding boxes, anchors, engraving defaults)
    metadata: SMuFLMetadata,
}

/// Minimal valid TTF font (empty with just required tables).
/// This is the smallest valid TrueType font that skrifa will accept.
static EMPTY_FONT_DATA: &[u8] = include_bytes!("../../tests/data/empty.ttf");

impl<'a> SMuFLFont<'a> {
    /// Load a SMuFL font from font data and metadata.
    ///
    /// # Arguments
    /// * `font_data` - Raw font file bytes (OTF/TTF)
    /// * `metadata` - Pre-loaded SMuFL metadata
    ///
    /// # Errors
    /// Returns an error if the font cannot be parsed.
    pub fn new(
        font_data: &'a [u8],
        metadata: SMuFLMetadata,
    ) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        let font = FontRef::new(font_data)?;
        Ok(Self { font, metadata })
    }

    /// Try to create an empty SMuFL font for minimal contexts.
    ///
    /// This font has no glyphs and minimal metadata. Use for layout operations
    /// that don't need actual font data, such as positioning calculations.
    ///
    /// # Errors
    /// Returns an error if the embedded font data or metadata is invalid.
    /// This should never happen with valid builds.
    pub fn try_empty() -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        let font = FontRef::new(EMPTY_FONT_DATA)?;

        // Minimal valid SMuFL metadata JSON
        let metadata_json = r#"{
            "fontName": "EmptyTestFont",
            "engravingDefaults": {},
            "glyphAdvanceWidths": {},
            "glyphsWithAnchors": {},
            "glyphBBoxes": {}
        }"#;

        let metadata = SMuFLMetadata::from_reader(metadata_json.as_bytes())?;

        Ok(Self { font, metadata })
    }

    /// Create an empty SMuFL font for minimal contexts.
    ///
    /// This font has no glyphs and minimal metadata. Use for layout operations
    /// that don't need actual font data, such as positioning calculations.
    ///
    /// # Panics
    /// Panics if the embedded font data is invalid (should never happen).
    #[must_use]
    pub fn empty() -> Self {
        Self::try_empty().expect("Built-in empty font and metadata should be valid")
    }

    /// Load a SMuFL font from font data and metadata JSON reader.
    ///
    /// # Errors
    /// Returns an error if the font or metadata cannot be parsed.
    pub fn from_reader<R: std::io::Read>(
        font_data: &'a [u8],
        metadata_reader: R,
    ) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        let font = FontRef::new(font_data)?;
        let reader = BufReader::new(metadata_reader);
        let metadata = SMuFLMetadata::from_reader(reader)?;
        Ok(Self { font, metadata })
    }

    /// Get the font reference for text shaping/rendering.
    #[must_use]
    pub fn font(&self) -> &FontRef<'a> {
        &self.font
    }

    /// Get the SMuFL metadata.
    #[must_use]
    pub fn metadata(&self) -> &SMuFLMetadata {
        &self.metadata
    }

    /// Get the advance width of a glyph in staff spaces.
    #[must_use]
    pub fn advance_width(&self, glyph: Glyph) -> Option<StaffSpaces> {
        self.metadata.advance_widths.get(glyph)
    }

    /// Get the bounding box of a glyph.
    #[must_use]
    pub fn bounding_box(&self, glyph: Glyph) -> Option<smufl::BoundingBox> {
        self.metadata.bounding_boxes.get(glyph)
    }

    /// Get anchor points for a glyph.
    #[must_use]
    pub fn anchors(&self, glyph: Glyph) -> Option<smufl::Anchors> {
        self.metadata.anchors.get(glyph)
    }

    /// Get the font's units per em.
    #[must_use]
    pub fn units_per_em(&self) -> u16 {
        use skrifa::raw::TableProvider;
        self.font
            .head()
            .ok()
            .map_or(1000, |head| head.units_per_em())
    }

    /// Get the staff line thickness from engraving defaults.
    #[must_use]
    pub fn staff_line_thickness(&self) -> Option<StaffSpaces> {
        self.metadata.engraving_defaults.staff_line_thickness
    }

    /// Get the stem thickness from engraving defaults.
    #[must_use]
    pub fn stem_thickness(&self) -> Option<StaffSpaces> {
        self.metadata.engraving_defaults.stem_thickness
    }

    /// Get the beam thickness from engraving defaults.
    #[must_use]
    pub fn beam_thickness(&self) -> Option<StaffSpaces> {
        self.metadata.engraving_defaults.beam_thickness
    }
}

/// Load SMuFL metadata from a JSON file path.
///
/// # Errors
/// Returns an error if the file cannot be read or parsed.
pub fn load_metadata_from_path(
    path: impl AsRef<Path>,
) -> Result<SMuFLMetadata, Box<dyn std::error::Error + Send + Sync>> {
    let file = std::fs::File::open(path)?;
    let reader = BufReader::new(file);
    let metadata = SMuFLMetadata::from_reader(reader)?;
    Ok(metadata)
}

/// Common SMuFL glyph constants for convenience.
/// These map to the `smufl::Glyph` enum variants.
pub mod glyphs {
    use super::Glyph;

    // Noteheads
    pub const NOTEHEAD_BLACK: Glyph = Glyph::NoteheadBlack;
    pub const NOTEHEAD_HALF: Glyph = Glyph::NoteheadHalf;
    pub const NOTEHEAD_WHOLE: Glyph = Glyph::NoteheadWhole;
    pub const NOTEHEAD_DOUBLE_WHOLE: Glyph = Glyph::NoteheadDoubleWhole;

    // Rests
    pub const REST_WHOLE: Glyph = Glyph::RestWhole;
    pub const REST_HALF: Glyph = Glyph::RestHalf;
    pub const REST_QUARTER: Glyph = Glyph::RestQuarter;
    pub const REST_8TH: Glyph = Glyph::Rest8th;
    pub const REST_16TH: Glyph = Glyph::Rest16th;
    pub const REST_32ND: Glyph = Glyph::Rest32nd;

    // Clefs
    pub const G_CLEF: Glyph = Glyph::GClef;
    pub const F_CLEF: Glyph = Glyph::FClef;
    pub const C_CLEF: Glyph = Glyph::CClef;

    // Accidentals
    pub const ACCIDENTAL_SHARP: Glyph = Glyph::AccidentalSharp;
    pub const ACCIDENTAL_FLAT: Glyph = Glyph::AccidentalFlat;
    pub const ACCIDENTAL_NATURAL: Glyph = Glyph::AccidentalNatural;
    pub const ACCIDENTAL_DOUBLE_SHARP: Glyph = Glyph::AccidentalDoubleSharp;
    pub const ACCIDENTAL_DOUBLE_FLAT: Glyph = Glyph::AccidentalDoubleFlat;

    // Flags
    pub const FLAG_8TH_UP: Glyph = Glyph::Flag8thUp;
    pub const FLAG_8TH_DOWN: Glyph = Glyph::Flag8thDown;
    pub const FLAG_16TH_UP: Glyph = Glyph::Flag16thUp;
    pub const FLAG_16TH_DOWN: Glyph = Glyph::Flag16thDown;

    // Time signatures
    pub const TIME_SIG_0: Glyph = Glyph::TimeSig0;
    pub const TIME_SIG_1: Glyph = Glyph::TimeSig1;
    pub const TIME_SIG_2: Glyph = Glyph::TimeSig2;
    pub const TIME_SIG_3: Glyph = Glyph::TimeSig3;
    pub const TIME_SIG_4: Glyph = Glyph::TimeSig4;
    pub const TIME_SIG_5: Glyph = Glyph::TimeSig5;
    pub const TIME_SIG_6: Glyph = Glyph::TimeSig6;
    pub const TIME_SIG_7: Glyph = Glyph::TimeSig7;
    pub const TIME_SIG_8: Glyph = Glyph::TimeSig8;
    pub const TIME_SIG_9: Glyph = Glyph::TimeSig9;
    pub const TIME_SIG_COMMON: Glyph = Glyph::TimeSigCommon;
    pub const TIME_SIG_CUT_COMMON: Glyph = Glyph::TimeSigCutCommon;

    // Dynamics
    pub const DYNAMIC_PIANO: Glyph = Glyph::DynamicPiano;
    pub const DYNAMIC_MEZZO: Glyph = Glyph::DynamicMezzo;
    pub const DYNAMIC_FORTE: Glyph = Glyph::DynamicForte;

    // Articulations
    pub const ARTIC_ACCENT_ABOVE: Glyph = Glyph::ArticAccentAbove;
    pub const ARTIC_STACCATO_ABOVE: Glyph = Glyph::ArticStaccatoAbove;
    pub const ARTIC_TENUTO_ABOVE: Glyph = Glyph::ArticTenutoAbove;

    // Fermatas
    pub const FERMATA_ABOVE: Glyph = Glyph::FermataAbove;
    pub const FERMATA_BELOW: Glyph = Glyph::FermataBelow;

    // Slash noteheads (for rhythmic notation / rhythm slashes)
    /// Slash notehead for quarter/eighth notes (filled)
    pub const NOTEHEAD_SLASH: Glyph = Glyph::NoteheadSlashHorizontalEnds;
    /// Slash notehead for half notes (open)
    pub const NOTEHEAD_SLASH_HALF: Glyph = Glyph::NoteheadSlashWhiteHalf;
    /// Slash notehead for whole notes (open)
    pub const NOTEHEAD_SLASH_WHOLE: Glyph = Glyph::NoteheadSlashWhiteWhole;
    /// Slash notehead for double whole notes
    pub const NOTEHEAD_SLASH_DOUBLE_WHOLE: Glyph = Glyph::NoteheadSlashWhiteDoubleWhole;
}
