//! Note layout implementation.
//!
//! Handles layout of individual noteheads, including position on staff,
//! accidentals, dots, and ledger lines.

use kurbo::{Point, Rect};
use peniko::Color;

use crate::layout::context::LayoutContext;
use crate::layout::shape::Shape;
use crate::scene::id::{ElementType, SemanticId};
use crate::scene::node::SceneNode;
use crate::scene::paint::PaintCommand;

use super::LayoutData;

/// SMuFL codepoints for noteheads.
pub mod glyphs {
    /// Whole note (semibreve)
    pub const NOTEHEAD_WHOLE: char = '\u{E0A2}';
    /// Half note (minim)
    pub const NOTEHEAD_HALF: char = '\u{E0A3}';
    /// Quarter note and shorter (crotchet)
    pub const NOTEHEAD_BLACK: char = '\u{E0A4}';
    /// Double whole note (breve)
    pub const NOTEHEAD_DOUBLE_WHOLE: char = '\u{E0A0}';

    // Accidentals
    /// Sharp
    pub const ACCIDENTAL_SHARP: char = '\u{E262}';
    /// Flat
    pub const ACCIDENTAL_FLAT: char = '\u{E260}';
    /// Natural
    pub const ACCIDENTAL_NATURAL: char = '\u{E261}';
    /// Double sharp
    pub const ACCIDENTAL_DOUBLE_SHARP: char = '\u{E263}';
    /// Double flat
    pub const ACCIDENTAL_DOUBLE_FLAT: char = '\u{E264}';

    // Augmentation dots
    /// Augmentation dot
    pub const AUGMENTATION_DOT: char = '\u{E1E7}';
}

/// Accidental type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Accidental {
    None,
    Sharp,
    Flat,
    Natural,
    DoubleSharp,
    DoubleFlat,
}

impl Accidental {
    /// Get the SMuFL glyph for this accidental.
    #[must_use]
    pub const fn glyph(&self) -> Option<char> {
        match self {
            Self::None => None,
            Self::Sharp => Some(glyphs::ACCIDENTAL_SHARP),
            Self::Flat => Some(glyphs::ACCIDENTAL_FLAT),
            Self::Natural => Some(glyphs::ACCIDENTAL_NATURAL),
            Self::DoubleSharp => Some(glyphs::ACCIDENTAL_DOUBLE_SHARP),
            Self::DoubleFlat => Some(glyphs::ACCIDENTAL_DOUBLE_FLAT),
        }
    }

    /// Get the width of this accidental in spatiums.
    #[must_use]
    pub const fn width(&self) -> f64 {
        match self {
            Self::None => 0.0,
            Self::Sharp => 1.2,
            Self::Flat => 0.9,
            Self::Natural => 0.7,
            Self::DoubleSharp => 1.0,
            Self::DoubleFlat => 1.4,
        }
    }
}

/// Duration type for notehead selection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NoteDuration {
    DoubleWhole,
    Whole,
    Half,
    Quarter,
    Eighth,
    Sixteenth,
    ThirtySecond,
    SixtyFourth,
}

impl NoteDuration {
    /// Get the SMuFL glyph for this duration's notehead.
    #[must_use]
    pub const fn notehead_glyph(&self) -> char {
        match self {
            Self::DoubleWhole => glyphs::NOTEHEAD_DOUBLE_WHOLE,
            Self::Whole => glyphs::NOTEHEAD_WHOLE,
            Self::Half => glyphs::NOTEHEAD_HALF,
            _ => glyphs::NOTEHEAD_BLACK,
        }
    }

    /// Check if this duration requires a stem.
    #[must_use]
    pub const fn has_stem(&self) -> bool {
        !matches!(self, Self::DoubleWhole | Self::Whole)
    }

    /// Get the number of flags (for eighth notes and shorter).
    #[must_use]
    pub const fn flag_count(&self) -> u8 {
        match self {
            Self::Eighth => 1,
            Self::Sixteenth => 2,
            Self::ThirtySecond => 3,
            Self::SixtyFourth => 4,
            _ => 0,
        }
    }
}

/// Note layout parameters.
#[derive(Debug, Clone)]
pub struct NoteParams {
    /// Unique identifier for this note
    pub id: u64,
    /// Duration type
    pub duration: NoteDuration,
    /// Staff line position (0 = middle line, positive = up)
    pub line: i32,
    /// Accidental to display
    pub accidental: Accidental,
    /// Number of augmentation dots
    pub dots: u8,
    /// Whether this note is part of a chord with offset noteheads
    pub offset_x: f64,
    /// Whether to draw ledger lines
    pub ledger_lines: bool,
}

impl Default for NoteParams {
    fn default() -> Self {
        Self {
            id: 0,
            duration: NoteDuration::Quarter,
            line: 0,
            accidental: Accidental::None,
            dots: 0,
            offset_x: 0.0,
            ledger_lines: true,
        }
    }
}

/// Layout a single note.
///
/// # Returns
/// Tuple of (LayoutData, SceneNode) containing position/shape and visual representation.
#[must_use]
pub fn layout_note(params: &NoteParams, ctx: &LayoutContext) -> (LayoutData, SceneNode) {
    let spatium = ctx.spatium();
    let staff_line_distance = spatium; // Distance between staff lines

    // Calculate Y position from staff line
    // Line 0 = middle line (B4 in treble clef)
    // Positive lines go up, negative go down
    let y = -params.line as f64 * staff_line_distance / 2.0;

    // Start X at 0, adjusted for accidentals
    let mut x = 0.0;

    let mut commands = Vec::new();
    let mut total_width = 0.0;

    // Draw accidental if present
    if let Some(acc_glyph) = params.accidental.glyph() {
        let acc_width = params.accidental.width() * spatium;
        let acc_x = x;

        commands.push(PaintCommand::glyph(
            acc_glyph,
            Point::new(acc_x, y),
            spatium,
            Color::BLACK,
        ));

        x += acc_width + spatium * 0.15; // Small gap after accidental
        total_width += acc_width + spatium * 0.15;
    }

    // Draw notehead
    let notehead_x = x + params.offset_x;
    let notehead_glyph = params.duration.notehead_glyph();
    let notehead_width = spatium * 1.18; // Standard notehead width

    commands.push(PaintCommand::glyph(
        notehead_glyph,
        Point::new(notehead_x, y),
        spatium,
        Color::BLACK,
    ));

    total_width += notehead_width;

    // Draw ledger lines if needed
    if params.ledger_lines {
        let ledger_commands = draw_ledger_lines(params.line, notehead_x, notehead_width, spatium);
        commands.extend(ledger_commands);
    }

    // Draw augmentation dots
    if params.dots > 0 {
        let dot_x = notehead_x + notehead_width + spatium * 0.25;
        let dot_y = if params.line % 2 == 0 {
            y - staff_line_distance / 4.0 // Move dot up if on a line
        } else {
            y
        };

        for i in 0..params.dots {
            commands.push(PaintCommand::glyph(
                glyphs::AUGMENTATION_DOT,
                Point::new(dot_x + i as f64 * spatium * 0.5, dot_y),
                spatium,
                Color::BLACK,
            ));
        }

        total_width += spatium * 0.25 + params.dots as f64 * spatium * 0.5;
    }

    // Calculate bounding box (relative to note position)
    let half_height = spatium * 0.5;
    let bbox = Rect::new(0.0, -half_height, total_width, half_height);

    // Create shape for collision detection (in world coordinates)
    let world_bbox = Rect::new(0.0, y - half_height, total_width, y + half_height);
    let shape = Shape::from_rect(world_bbox);

    // Create layout data with proper position
    let layout = LayoutData::new(Point::new(0.0, y), bbox, shape);

    // Create scene node with semantic ID
    let semantic_id = SemanticId::new(ElementType::Note, params.id);
    let node = SceneNode::leaf(semantic_id, commands)
        .with_metadata("pitch-line", params.line.to_string());

    (layout, node)
}

/// Draw ledger lines for notes outside the staff.
fn draw_ledger_lines(line: i32, notehead_x: f64, notehead_width: f64, spatium: f64) -> Vec<PaintCommand> {
    let mut commands = Vec::new();
    let line_extension = spatium * 0.4; // How far ledger line extends past notehead
    let line_width = spatium * 0.16;
    let staff_line_distance = spatium;

    // Ledger lines above staff (line > 5, i.e., above top line)
    if line > 5 {
        let mut l = 6;
        while l <= line {
            if l % 2 == 0 {
                // Only draw on even lines (actual lines, not spaces)
                let ledger_y = -l as f64 * staff_line_distance / 2.0;
                commands.push(PaintCommand::line(
                    Point::new(notehead_x - line_extension, ledger_y),
                    Point::new(notehead_x + notehead_width + line_extension, ledger_y),
                    Color::BLACK,
                    line_width,
                ));
            }
            l += 1;
        }
    }

    // Ledger lines below staff (line < -5, i.e., below bottom line)
    if line < -5 {
        let mut l = -6;
        while l >= line {
            if l % 2 == 0 {
                let ledger_y = -l as f64 * staff_line_distance / 2.0;
                commands.push(PaintCommand::line(
                    Point::new(notehead_x - line_extension, ledger_y),
                    Point::new(notehead_x + notehead_width + line_extension, ledger_y),
                    Color::BLACK,
                    line_width,
                ));
            }
            l -= 1;
        }
    }

    commands
}

/// Calculate the shape for a note (for collision detection).
#[must_use]
pub fn note_shape(params: &NoteParams, ctx: &LayoutContext) -> Shape {
    let spatium = ctx.spatium();
    let y = -params.line as f64 * spatium / 2.0;
    let half_height = spatium * 0.5;

    let mut width = spatium * 1.18; // Notehead width
    if params.accidental != Accidental::None {
        width += params.accidental.width() * spatium + spatium * 0.15;
    }
    if params.dots > 0 {
        width += spatium * 0.25 + params.dots as f64 * spatium * 0.5;
    }

    Shape::from_rect(Rect::new(0.0, y - half_height, width, y + half_height))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::layout::context::LayoutConfiguration;
    use crate::style::MStyle;

    fn test_ctx() -> LayoutContext<'static> {
        let config = LayoutConfiguration::default();
        let style = Box::leak(Box::new(MStyle::default()));
        LayoutContext::new_for_test(config, style)
    }

    #[test]
    fn test_layout_simple_note() {
        let ctx = test_ctx();
        let params = NoteParams {
            id: 1,
            duration: NoteDuration::Quarter,
            line: 0,
            ..Default::default()
        };

        let (layout, node) = layout_note(&params, &ctx);

        assert!(!layout.bbox.is_zero_area());
        assert!(node.id.is_some());
        assert!(!node.commands.is_empty());
    }

    #[test]
    fn test_layout_note_with_accidental() {
        let ctx = test_ctx();
        let params = NoteParams {
            id: 2,
            duration: NoteDuration::Quarter,
            line: 0,
            accidental: Accidental::Sharp,
            ..Default::default()
        };

        let (layout, node) = layout_note(&params, &ctx);

        // Should have at least 2 commands (accidental + notehead)
        assert!(node.commands.len() >= 2);
        // Bounding box should be wider with accidental
        assert!(layout.bbox.width() > ctx.spatium());
    }

    #[test]
    fn test_layout_note_with_dots() {
        let ctx = test_ctx();
        let params = NoteParams {
            id: 3,
            duration: NoteDuration::Half,
            line: 2,
            dots: 2,
            ..Default::default()
        };

        let (layout, node) = layout_note(&params, &ctx);

        // Should have notehead + 2 dots = 3 commands minimum
        assert!(node.commands.len() >= 3);
    }

    #[test]
    fn test_notehead_glyphs() {
        assert_eq!(NoteDuration::Whole.notehead_glyph(), glyphs::NOTEHEAD_WHOLE);
        assert_eq!(NoteDuration::Half.notehead_glyph(), glyphs::NOTEHEAD_HALF);
        assert_eq!(NoteDuration::Quarter.notehead_glyph(), glyphs::NOTEHEAD_BLACK);
        assert_eq!(NoteDuration::Eighth.notehead_glyph(), glyphs::NOTEHEAD_BLACK);
    }

    #[test]
    fn test_stem_required() {
        assert!(!NoteDuration::Whole.has_stem());
        assert!(!NoteDuration::DoubleWhole.has_stem());
        assert!(NoteDuration::Half.has_stem());
        assert!(NoteDuration::Quarter.has_stem());
    }

    #[test]
    fn test_flag_count() {
        assert_eq!(NoteDuration::Quarter.flag_count(), 0);
        assert_eq!(NoteDuration::Eighth.flag_count(), 1);
        assert_eq!(NoteDuration::Sixteenth.flag_count(), 2);
        assert_eq!(NoteDuration::SixtyFourth.flag_count(), 4);
    }
}
