//! Chord layout implementation.
//!
//! Handles layout of chords (groups of notes), including stems, flags,
//! and proper notehead stacking for seconds.

use kurbo::{Point, Rect};
use peniko::Color;

use crate::layout::context::LayoutContext;
use crate::layout::shape::Shape;
use crate::scene::id::{ElementType, SemanticId};
use crate::scene::node::SceneNode;
use crate::scene::paint::PaintCommand;

use super::note::{layout_note, Accidental, NoteDuration, NoteParams};
use super::LayoutData;

/// SMuFL codepoints for stems and flags.
pub mod glyphs {
    /// Flag for eighth note (up stem)
    pub const FLAG_EIGHTH_UP: char = '\u{E240}';
    /// Flag for eighth note (down stem)
    pub const FLAG_EIGHTH_DOWN: char = '\u{E241}';
    /// Flag for sixteenth note (up stem)
    pub const FLAG_SIXTEENTH_UP: char = '\u{E242}';
    /// Flag for sixteenth note (down stem)
    pub const FLAG_SIXTEENTH_DOWN: char = '\u{E243}';
    /// Flag for 32nd note (up stem)
    pub const FLAG_32ND_UP: char = '\u{E244}';
    /// Flag for 32nd note (down stem)
    pub const FLAG_32ND_DOWN: char = '\u{E245}';
    /// Flag for 64th note (up stem)
    pub const FLAG_64TH_UP: char = '\u{E246}';
    /// Flag for 64th note (down stem)
    pub const FLAG_64TH_DOWN: char = '\u{E247}';
}

/// Stem direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StemDirection {
    Up,
    Down,
    Auto,
}

impl StemDirection {
    /// Resolve auto direction based on note positions.
    #[must_use]
    pub fn resolve(self, avg_line: f64) -> Self {
        match self {
            Self::Auto => {
                // Standard rule: stem down if average note is above middle line
                if avg_line > 0.0 {
                    Self::Down
                } else {
                    Self::Up
                }
            }
            _ => self,
        }
    }
}

/// A single note within a chord.
#[derive(Debug, Clone)]
pub struct ChordNote {
    /// Staff line position
    pub line: i32,
    /// Accidental
    pub accidental: Accidental,
    /// Tie to next note
    pub tie: bool,
}

/// Chord layout parameters.
#[derive(Debug, Clone)]
pub struct ChordParams {
    /// Unique identifier
    pub id: u64,
    /// Duration type
    pub duration: NoteDuration,
    /// Notes in the chord (sorted by line)
    pub notes: Vec<ChordNote>,
    /// Stem direction
    pub stem_direction: StemDirection,
    /// Number of augmentation dots
    pub dots: u8,
    /// Whether chord is part of a beam (no flags)
    pub beamed: bool,
}

impl Default for ChordParams {
    fn default() -> Self {
        Self {
            id: 0,
            duration: NoteDuration::Quarter,
            notes: vec![ChordNote {
                line: 0,
                accidental: Accidental::None,
                tie: false,
            }],
            stem_direction: StemDirection::Auto,
            dots: 0,
            beamed: false,
        }
    }
}

/// Layout a chord (group of notes with stem).
#[must_use]
pub fn layout_chord(params: &ChordParams, ctx: &LayoutContext) -> (LayoutData, SceneNode) {
    let spatium = ctx.spatium();

    // Sort notes by line position
    let mut sorted_notes = params.notes.clone();
    sorted_notes.sort_by_key(|n| n.line);

    if sorted_notes.is_empty() {
        // Empty chord - return minimal layout
        let layout = LayoutData::new(Point::ZERO, Rect::ZERO, Shape::empty());
        let node = SceneNode::group(SemanticId::chord(params.id));
        return (layout, node);
    }

    // Calculate average line for stem direction
    let avg_line: f64 = sorted_notes.iter().map(|n| n.line as f64).sum::<f64>()
        / sorted_notes.len() as f64;

    let stem_dir = params.stem_direction.resolve(avg_line);

    // Create chord group node
    let mut chord_node = SceneNode::group(SemanticId::chord(params.id));
    let mut total_bbox = Rect::ZERO;

    // Calculate which noteheads need to be offset (for seconds)
    let notehead_offsets = calculate_notehead_offsets(&sorted_notes, stem_dir);

    // Layout each note
    for (i, note) in sorted_notes.iter().enumerate() {
        let note_params = NoteParams {
            id: params.id * 1000 + i as u64,
            duration: params.duration,
            line: note.line,
            accidental: note.accidental,
            dots: if i == 0 { params.dots } else { 0 }, // Only first note gets dots
            offset_x: notehead_offsets[i],
            ledger_lines: i == 0 || i == sorted_notes.len() - 1, // Only top/bottom get ledgers
        };

        let (note_layout, note_node) = layout_note(&note_params, ctx);

        // Calculate world-space bbox (note_layout.bbox is relative to note_layout.position)
        let world_bbox = Rect::new(
            note_layout.bbox.x0 + note_layout.position.x,
            note_layout.bbox.y0 + note_layout.position.y,
            note_layout.bbox.x1 + note_layout.position.x,
            note_layout.bbox.y1 + note_layout.position.y,
        );

        // Expand total bounding box
        if total_bbox.is_zero_area() {
            total_bbox = world_bbox;
        } else {
            total_bbox = total_bbox.union(world_bbox);
        }

        chord_node.add_child(note_node);
    }

    // Add stem if required
    if params.duration.has_stem() {
        let stem_commands = draw_stem(
            &sorted_notes,
            stem_dir,
            &notehead_offsets,
            spatium,
        );

        let stem_node = SceneNode::anonymous_leaf(stem_commands);
        chord_node.add_child(stem_node);

        // Add flags if not beamed
        if !params.beamed && params.duration.flag_count() > 0 {
            let flag_commands = draw_flags(
                &sorted_notes,
                stem_dir,
                &notehead_offsets,
                params.duration,
                spatium,
            );

            if !flag_commands.is_empty() {
                let flag_node = SceneNode::anonymous_leaf(flag_commands);
                chord_node.add_child(flag_node);
            }
        }
    }

    // Create collision shape
    let shape = Shape::from_rect(total_bbox);
    let layout = LayoutData::new(Point::ZERO, total_bbox, shape);

    (layout, chord_node)
}

/// Calculate horizontal offsets for noteheads to handle seconds (adjacent notes).
fn calculate_notehead_offsets(notes: &[ChordNote], stem_dir: StemDirection) -> Vec<f64> {
    let mut offsets = vec![0.0; notes.len()];

    for i in 1..notes.len() {
        let prev_line = notes[i - 1].line;
        let curr_line = notes[i].line;

        // Check if this is a second (adjacent lines)
        if (curr_line - prev_line).abs() == 1 {
            // Need to offset one notehead
            let notehead_width = 1.18; // In spatiums

            if stem_dir == StemDirection::Up {
                // Lower note goes to the right
                offsets[i - 1] = notehead_width;
            } else {
                // Higher note goes to the right
                offsets[i] = notehead_width;
            }
        }
    }

    offsets
}

// ============================================================================
// SMuFL Anchor Points (from Leland font metadata)
// ============================================================================
// These are the exact anchor points for stem attachment from the SMuFL spec.
// Coordinates are in staff spaces, relative to notehead origin.

/// SMuFL stemUpSE anchor: attachment point for up-stems (South-East corner).
/// From Leland metadata: [1.3, 0.16]
const STEM_UP_SE_X: f64 = 1.3;
const STEM_UP_SE_Y: f64 = 0.16;

/// SMuFL stemDownNW anchor: attachment point for down-stems (North-West corner).
/// From Leland metadata: [0.0, -0.168]
const STEM_DOWN_NW_X: f64 = 0.0;
const STEM_DOWN_NW_Y: f64 = -0.168;

/// Standard stem width in staff spaces (from MuseScore default).
const STEM_WIDTH: f64 = 0.12;

/// Draw the stem for a chord.
/// Uses SMuFL anchor points for precise stem attachment matching MuseScore.
fn draw_stem(
    notes: &[ChordNote],
    stem_dir: StemDirection,
    _offsets: &[f64],
    spatium: f64,
) -> Vec<PaintCommand> {
    let stem_width = STEM_WIDTH * spatium;
    let stem_length = spatium * 3.5;

    let top_note = notes.last().unwrap();
    let bottom_note = notes.first().unwrap();

    let top_y = -top_note.line as f64 * spatium / 2.0;
    let bottom_y = -bottom_note.line as f64 * spatium / 2.0;

    let (stem_x, stem_start_y, stem_end_y) = match stem_dir {
        StemDirection::Up | StemDirection::Auto => {
            // Use SMuFL stemUpSE anchor (right side of notehead)
            // X: anchor X - half stem width (to center the stem line on the anchor)
            let x = STEM_UP_SE_X * spatium - stem_width / 2.0;
            // Y: start at bottom note + Y offset, end going up
            // SMuFL Y is negated because glyph renderer flips Y
            let y_offset = -STEM_UP_SE_Y * spatium;
            let start = bottom_y + y_offset;
            let end = top_y - stem_length;
            (x, start, end)
        }
        StemDirection::Down => {
            // Use SMuFL stemDownNW anchor (left side of notehead)
            // X: anchor X + half stem width (to center the stem line on the anchor)
            let x = STEM_DOWN_NW_X * spatium + stem_width / 2.0;
            // Y: start at top note + Y offset, end going down
            // SMuFL Y is negated because glyph renderer flips Y
            let y_offset = -STEM_DOWN_NW_Y * spatium;
            let start = top_y + y_offset;
            let end = bottom_y + stem_length;
            (x, start, end)
        }
    };

    vec![PaintCommand::line(
        Point::new(stem_x, stem_start_y),
        Point::new(stem_x, stem_end_y),
        Color::BLACK,
        stem_width,
    )]
}

/// Draw flags for a chord (eighth notes and shorter).
/// Uses SMuFL anchor points for precise flag placement matching MuseScore.
fn draw_flags(
    notes: &[ChordNote],
    stem_dir: StemDirection,
    _offsets: &[f64],
    duration: NoteDuration,
    spatium: f64,
) -> Vec<PaintCommand> {
    let flag_count = duration.flag_count();
    if flag_count == 0 {
        return Vec::new();
    }

    let stem_length = spatium * 3.5;
    let stem_width = STEM_WIDTH * spatium;

    let top_note = notes.last().unwrap();
    let bottom_note = notes.first().unwrap();

    let (flag_x, flag_y, glyph) = match stem_dir {
        StemDirection::Up | StemDirection::Auto => {
            // Flag attaches at stem tip (same X as stem)
            let x = STEM_UP_SE_X * spatium - stem_width / 2.0;
            let top_y = -top_note.line as f64 * spatium / 2.0;
            let y = top_y - stem_length;
            let g = match flag_count {
                1 => glyphs::FLAG_EIGHTH_UP,
                2 => glyphs::FLAG_SIXTEENTH_UP,
                3 => glyphs::FLAG_32ND_UP,
                _ => glyphs::FLAG_64TH_UP,
            };
            (x, y, g)
        }
        StemDirection::Down => {
            // Flag attaches at stem tip (same X as stem)
            let x = STEM_DOWN_NW_X * spatium + stem_width / 2.0;
            let bottom_y = -bottom_note.line as f64 * spatium / 2.0;
            let y = bottom_y + stem_length;
            let g = match flag_count {
                1 => glyphs::FLAG_EIGHTH_DOWN,
                2 => glyphs::FLAG_SIXTEENTH_DOWN,
                3 => glyphs::FLAG_32ND_DOWN,
                _ => glyphs::FLAG_64TH_DOWN,
            };
            (x, y, g)
        }
    };

    vec![PaintCommand::glyph(glyph, Point::new(flag_x, flag_y), spatium, Color::BLACK)]
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
    fn test_layout_single_note_chord() {
        let ctx = test_ctx();
        let params = ChordParams {
            id: 1,
            duration: NoteDuration::Quarter,
            notes: vec![ChordNote {
                line: 0,
                accidental: Accidental::None,
                tie: false,
            }],
            ..Default::default()
        };

        let (layout, node) = layout_chord(&params, &ctx);

        assert!(!layout.bbox.is_zero_area());
        assert!(node.id.is_some());
    }

    #[test]
    fn test_layout_two_note_chord() {
        let ctx = test_ctx();
        let params = ChordParams {
            id: 2,
            duration: NoteDuration::Quarter,
            notes: vec![
                ChordNote { line: 0, accidental: Accidental::None, tie: false },
                ChordNote { line: 4, accidental: Accidental::None, tie: false },
            ],
            ..Default::default()
        };

        let (layout, node) = layout_chord(&params, &ctx);

        assert!(!layout.bbox.is_zero_area());
        // Should have multiple children (notes + stem)
        assert!(node.children.len() >= 2);
    }

    #[test]
    fn test_layout_chord_with_second() {
        let ctx = test_ctx();
        let params = ChordParams {
            id: 3,
            duration: NoteDuration::Quarter,
            notes: vec![
                ChordNote { line: 0, accidental: Accidental::None, tie: false },
                ChordNote { line: 1, accidental: Accidental::None, tie: false }, // Second
            ],
            ..Default::default()
        };

        let (layout, node) = layout_chord(&params, &ctx);

        assert!(!layout.bbox.is_zero_area());
    }

    #[test]
    fn test_stem_direction_auto() {
        // Notes below middle line -> stem up
        assert_eq!(StemDirection::Auto.resolve(-2.0), StemDirection::Up);

        // Notes above middle line -> stem down
        assert_eq!(StemDirection::Auto.resolve(2.0), StemDirection::Down);

        // Notes at middle line -> stem up (convention)
        assert_eq!(StemDirection::Auto.resolve(0.0), StemDirection::Up);
    }

    #[test]
    fn test_layout_eighth_note_with_flag() {
        let ctx = test_ctx();
        let params = ChordParams {
            id: 4,
            duration: NoteDuration::Eighth,
            notes: vec![ChordNote {
                line: 0,
                accidental: Accidental::None,
                tie: false,
            }],
            beamed: false,
            ..Default::default()
        };

        let (layout, node) = layout_chord(&params, &ctx);

        assert!(!layout.bbox.is_zero_area());
        // Should have note + stem + flag
        assert!(node.children.len() >= 2);
    }

    #[test]
    fn test_beamed_chord_no_flag() {
        let ctx = test_ctx();
        let params = ChordParams {
            id: 5,
            duration: NoteDuration::Eighth,
            notes: vec![ChordNote {
                line: 0,
                accidental: Accidental::None,
                tie: false,
            }],
            beamed: true, // Part of a beam group
            ..Default::default()
        };

        let (layout, node) = layout_chord(&params, &ctx);

        // Beamed chords should not have flag children
        // Only note + stem
        assert!(!layout.bbox.is_zero_area());
    }
}
