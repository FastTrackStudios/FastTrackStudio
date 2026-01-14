//! Beam layout system ported from MuseScore.
//!
//! Handles beam rendering for groups of notes, including:
//! - Primary and secondary beams
//! - Beam angle calculation
//! - Beamlets for partial beams
//! - Stem length adjustment

use kurbo::{BezPath, Point, Rect};
use peniko::Color;

use crate::scene::paint::PaintCommand;

use super::chord::StemDirection;
use super::note::NoteDuration;

/// Configuration for beam layout.
#[derive(Debug, Clone)]
pub struct BeamLayoutConfig {
    /// Beam thickness in spatiums
    pub beam_thickness: f64,
    /// Distance between beam lines (for 16ths, etc.) in spatiums
    pub beam_spacing: f64,
    /// Minimum stem length in spatiums
    pub min_stem_length: f64,
    /// Maximum beam slope (rise per spatium of run)
    pub max_slope: f64,
    /// Beamlet length in spatiums
    pub beamlet_length: f64,
}

impl Default for BeamLayoutConfig {
    fn default() -> Self {
        Self {
            beam_thickness: 0.5,
            beam_spacing: 0.25,
            min_stem_length: 2.5,
            max_slope: 0.5,
            beamlet_length: 1.2,
        }
    }
}

/// Information about a note in a beam group.
#[derive(Debug, Clone)]
pub struct BeamNote {
    /// X position relative to beam group start
    pub x: f64,
    /// Staff line position
    pub line: i32,
    /// Note duration (determines number of beams)
    pub duration: NoteDuration,
    /// Stem direction (should be consistent within beam)
    pub stem_direction: StemDirection,
}

impl BeamNote {
    /// Y position of notehead center (relative to middle staff line).
    /// Matches `layout_note`'s convention: positive line = up on staff = smaller Y.
    /// Screen coordinates (Y-down), so we negate to convert line to Y.
    pub fn y_center(&self, spatium: f64) -> f64 {
        -self.line as f64 * spatium / 2.0
    }

    /// Number of beam lines needed for this note.
    pub fn beam_count(&self) -> usize {
        self.duration.flag_count() as usize
    }
}

/// Result of beam layout.
#[derive(Debug, Clone)]
pub struct BeamLayout {
    /// Paint commands for the beam
    pub commands: Vec<PaintCommand>,
    /// Adjusted stem endpoints for each note (y position at stem tip)
    pub stem_tips: Vec<f64>,
    /// Bounding box of the entire beam
    pub bbox: Rect,
}

/// Layout a beam group.
///
/// Returns paint commands for the beams and adjusted stem tip positions.
pub fn layout_beam(
    notes: &[BeamNote],
    spatium: f64,
    config: &BeamLayoutConfig,
) -> BeamLayout {
    if notes.len() < 2 {
        return BeamLayout {
            commands: Vec::new(),
            stem_tips: Vec::new(),
            bbox: Rect::ZERO,
        };
    }

    // Determine beam direction (all notes in a beam share direction)
    let stem_dir = determine_beam_direction(notes);

    // Calculate beam endpoints
    let (start_y, end_y) = calculate_beam_position(notes, stem_dir, spatium, config);

    // Calculate stem tips for each note
    let stem_tips = calculate_stem_tips(notes, start_y, end_y, spatium);

    // Generate beam commands
    let mut commands = Vec::new();
    let mut bbox = Rect::ZERO;

    // Draw stems from noteheads to beam using SMuFL anchor points
    let stem_width = spatium * STEM_WIDTH;
    let half_beam_thickness = config.beam_thickness * spatium / 2.0;

    // Small overlap to ensure stems fully intersect beam (prevents tiny gaps due to anti-aliasing)
    let stem_beam_overlap = spatium * 0.05;

    // Get beam line parameters for calculating Y at any X position
    let first_x = notes[0].x;
    let last_x = notes.last().unwrap().x;
    let beam_run = last_x - first_x;

    for (_i, note) in notes.iter().enumerate() {
        let note_y = note.y_center(spatium);

        // Calculate the actual stem X position (includes notehead width offset)
        let stem_x = stem_x_for_note(note, stem_dir, spatium);

        // Calculate beam Y at the STEM position, not at note.x
        // This is critical for angled beams - the stem must reach the beam where it actually is
        let beam_center_y = if beam_run.abs() < 0.001 {
            start_y
        } else {
            // Interpolate beam Y at the stem X position
            let t = (stem_x - first_x) / beam_run;
            start_y + t * (end_y - start_y)
        };

        // Adjust stem tip to connect to the correct edge of the beam
        // For stem UP: beam is above (smaller Y), stem connects to bottom edge (center + half)
        // For stem DOWN: beam is below (larger Y), stem connects to top edge (center - half)
        // Subtract overlap to extend stem INTO the beam for clean visual connection
        let stem_tip_y = match stem_dir {
            StemDirection::Up | StemDirection::Auto => beam_center_y + half_beam_thickness - stem_beam_overlap,
            StemDirection::Down => beam_center_y - half_beam_thickness + stem_beam_overlap,
        };

        // Use SMuFL anchor points for stem attachment
        let stem_attach_y = note_y + stem_y_offset(stem_dir, spatium);

        // Stem from notehead anchor to beam edge
        let stem_cmd = PaintCommand::line(
            Point::new(stem_x, stem_attach_y),
            Point::new(stem_x, stem_tip_y),
            Color::BLACK,
            stem_width,
        );
        if let Some(cmd_bbox) = stem_cmd.bounding_box() {
            if bbox.is_zero_area() {
                bbox = cmd_bbox;
            } else {
                bbox = bbox.union(cmd_bbox);
            }
        }
        commands.push(stem_cmd);
    }

    // Find the maximum beam level needed
    let max_beams = notes.iter().map(|n| n.beam_count()).max().unwrap_or(0);

    // Draw each beam level
    for level in 0..max_beams {
        let beam_commands = draw_beam_level(notes, level, start_y, end_y, stem_dir, spatium, config);
        for cmd in &beam_commands {
            if let Some(cmd_bbox) = cmd.bounding_box() {
                if bbox.is_zero_area() {
                    bbox = cmd_bbox;
                } else {
                    bbox = bbox.union(cmd_bbox);
                }
            }
        }
        commands.extend(beam_commands);
    }

    BeamLayout {
        commands,
        stem_tips,
        bbox,
    }
}

/// Determine beam direction based on note positions.
fn determine_beam_direction(notes: &[BeamNote]) -> StemDirection {
    // If any note has explicit direction, use it
    for note in notes {
        if note.stem_direction != StemDirection::Auto {
            return note.stem_direction;
        }
    }

    // Calculate average line position
    let avg_line: f64 = notes.iter().map(|n| n.line as f64).sum::<f64>() / notes.len() as f64;

    // Standard rule: stem up if average is below middle line
    if avg_line >= 0.0 {
        StemDirection::Down
    } else {
        StemDirection::Up
    }
}

/// Calculate the Y position of the primary beam at start and end.
///
/// This follows MuseScore's approach: calculate a base beam position using
/// minimum stem lengths, then apply a slope based on the note contour.
fn calculate_beam_position(
    notes: &[BeamNote],
    stem_dir: StemDirection,
    spatium: f64,
    config: &BeamLayoutConfig,
) -> (f64, f64) {
    let first = &notes[0];
    let last = notes.last().unwrap();

    let first_y = first.y_center(spatium);
    let last_y = last.y_center(spatium);

    // Calculate base beam positions ensuring minimum stem length for first and last notes
    let (first_base, last_base) = match stem_dir {
        StemDirection::Up | StemDirection::Auto => {
            // Stems go up, beam is above notes (smaller Y in screen coords)
            (
                first_y - config.min_stem_length * spatium,
                last_y - config.min_stem_length * spatium,
            )
        }
        StemDirection::Down => {
            // Stems go down, beam is below notes (larger Y in screen coords)
            (
                first_y + config.min_stem_length * spatium,
                last_y + config.min_stem_length * spatium,
            )
        }
    };

    // Calculate slope based on note positions
    let run = last.x - first.x;
    if run.abs() < 0.001 {
        // Notes are at same X position, return flat beam
        let beam_y = (first_base + last_base) / 2.0;
        return (beam_y, beam_y);
    }

    // Ideal slope follows the notes but reduced
    let ideal_slope = (last_base - first_base) / run;

    // Clamp slope to maximum (reduces extreme slopes)
    let clamped_slope = ideal_slope.clamp(-config.max_slope, config.max_slope);

    // Recalculate end position using clamped slope
    let mut start_y = first_base;
    let mut end_y = first_base + clamped_slope * run;

    // Ensure all notes have minimum stem length
    for note in notes {
        let note_y = note.y_center(spatium);
        let t = (note.x - first.x) / run;
        let beam_at_note = start_y + t * (end_y - start_y);

        let required_beam_y = match stem_dir {
            StemDirection::Up | StemDirection::Auto => note_y - config.min_stem_length * spatium,
            StemDirection::Down => note_y + config.min_stem_length * spatium,
        };

        // Adjust beam if any note would have too short a stem
        match stem_dir {
            StemDirection::Up | StemDirection::Auto => {
                // Beam must be at or above (smaller Y) the required position
                if beam_at_note > required_beam_y {
                    let offset = beam_at_note - required_beam_y;
                    start_y -= offset;
                    end_y -= offset;
                }
            }
            StemDirection::Down => {
                // Beam must be at or below (larger Y) the required position
                if beam_at_note < required_beam_y {
                    let offset = required_beam_y - beam_at_note;
                    start_y += offset;
                    end_y += offset;
                }
            }
        }
    }

    (start_y, end_y)
}

/// Calculate stem tip Y position for each note along the beam.
fn calculate_stem_tips(
    notes: &[BeamNote],
    start_y: f64,
    end_y: f64,
    spatium: f64,
) -> Vec<f64> {
    if notes.is_empty() {
        return Vec::new();
    }

    let first_x = notes[0].x;
    let last_x = notes.last().unwrap().x;
    let run = last_x - first_x;

    notes
        .iter()
        .map(|note| {
            if run.abs() < 0.001 {
                start_y
            } else {
                // Linear interpolation along beam
                let t = (note.x - first_x) / run;
                start_y + t * (end_y - start_y)
            }
        })
        .collect()
}

// ============================================================================
// SMuFL Anchor Points (from Leland font metadata)
// ============================================================================
// These are the exact anchor points for stem attachment from the SMuFL spec.
// Coordinates are in staff spaces, relative to notehead origin (top-left of bbox).

/// SMuFL stemUpSE anchor: attachment point for up-stems (South-East corner).
/// From Leland metadata: [1.3, 0.16]
const STEM_UP_SE_X: f64 = 1.3;
const STEM_UP_SE_Y: f64 = 0.16;

/// SMuFL stemDownNW anchor: attachment point for down-stems (North-West corner).
/// From Leland metadata: [0.0, -0.168]
const STEM_DOWN_NW_X: f64 = 0.0;
const STEM_DOWN_NW_Y: f64 = -0.168;

/// Notehead width for fallback when anchor is unavailable (in staff spaces).
/// SMuFL noteheadBlack bounding box width.
const NOTEHEAD_WIDTH: f64 = 1.3;

/// Standard stem width in staff spaces (from MuseScore default).
const STEM_WIDTH: f64 = 0.12;

// ============================================================================
// Beam Anchor Types (matching MuseScore's ChordBeamAnchorType)
// ============================================================================

/// Beam anchor position type, matching MuseScore's ChordBeamAnchorType.
/// This determines how the stem width adjustment is applied for beam connections.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChordBeamAnchorType {
    /// First note of beam group - stem edge towards beam start
    Start,
    /// Middle note of beam group - stem center
    Middle,
    /// Last note of beam group - stem edge towards beam end
    End,
}

// ============================================================================
// MuseScore-compatible Stem Position Functions
// ============================================================================

/// Calculate stem X position relative to notehead origin.
/// Matches MuseScore's `StemLayout::stemPosX(const Chord* item)`.
///
/// For up-stems: returns stemUpSE.x (right side of notehead)
/// For down-stems: returns stemDownNW.x (left side of notehead)
fn stem_pos_x(stem_dir: StemDirection) -> f64 {
    match stem_dir {
        StemDirection::Up | StemDirection::Auto => {
            // Use SMuFL stemUpSE anchor
            // MuseScore also applies noteWidthOffset, but for standard noteheads this is 0
            STEM_UP_SE_X
        }
        StemDirection::Down => {
            // Use SMuFL stemDownNW anchor
            STEM_DOWN_NW_X
        }
    }
}

/// Calculate beam anchor X position.
/// Matches MuseScore's `BeamTremoloLayout::chordBeamAnchorX`.
///
/// The key insight is that stem width matters for beam connections:
/// - Start anchor (first note): stem edge facing outward
/// - Middle anchor: stem center
/// - End anchor (last note): stem edge facing outward
fn chord_beam_anchor_x(
    note: &BeamNote,
    stem_dir: StemDirection,
    anchor_type: ChordBeamAnchorType,
    spatium: f64,
) -> f64 {
    let stem_x = note.x + stem_pos_x(stem_dir) * spatium;
    let stem_width = STEM_WIDTH * spatium;

    match anchor_type {
        ChordBeamAnchorType::Start => {
            match stem_dir {
                StemDirection::Up | StemDirection::Auto => {
                    // Up-stem start: subtract full stem width
                    // (stem extends to the left of the anchor point)
                    stem_x - stem_width
                }
                StemDirection::Down => {
                    // Down-stem start: no adjustment
                    stem_x
                }
            }
        }
        ChordBeamAnchorType::Middle => {
            match stem_dir {
                StemDirection::Up | StemDirection::Auto => {
                    // Up-stem middle: subtract half stem width (center)
                    stem_x - stem_width / 2.0
                }
                StemDirection::Down => {
                    // Down-stem middle: add half stem width (center)
                    stem_x + stem_width / 2.0
                }
            }
        }
        ChordBeamAnchorType::End => {
            match stem_dir {
                StemDirection::Up | StemDirection::Auto => {
                    // Up-stem end: no adjustment
                    stem_x
                }
                StemDirection::Down => {
                    // Down-stem end: add full stem width
                    stem_x + stem_width
                }
            }
        }
    }
}

/// Calculate the X position of the stem for a note (for drawing the stem line).
/// Uses the middle anchor type since stem drawing should use stem center.
fn stem_x_for_note(note: &BeamNote, stem_dir: StemDirection, spatium: f64) -> f64 {
    chord_beam_anchor_x(note, stem_dir, ChordBeamAnchorType::Middle, spatium)
}

/// Calculate the Y offset for stem attachment relative to notehead center.
/// Uses SMuFL anchor Y coordinates with Y-flip compensation.
///
/// SMuFL uses Y-up coordinates where positive Y is upward.
/// Our rendering uses Y-down (screen coordinates) where positive Y is downward.
/// The glyph renderer applies `Affine::scale_non_uniform(1.0, -1.0)` to flip Y.
/// Therefore, SMuFL Y coordinates must be negated for our coordinate system.
fn stem_y_offset(stem_dir: StemDirection, spatium: f64) -> f64 {
    match stem_dir {
        StemDirection::Up | StemDirection::Auto => {
            // SMuFL stemUpSE.y is positive (below center in SMuFL Y-up)
            // After Y-flip, this becomes negative (above center in screen Y-down)
            // But we want stem to attach at SE corner which is below center in screen coords
            // So we negate to get positive (downward) offset
            -STEM_UP_SE_Y * spatium
        }
        StemDirection::Down => {
            // SMuFL stemDownNW.y is negative (above center in SMuFL Y-up)
            // After Y-flip, this becomes positive (below center in screen Y-down)
            // But we want stem to attach at NW corner which is above center in screen coords
            // So we negate to get negative (upward) offset
            -STEM_DOWN_NW_Y * spatium
        }
    }
}

/// Draw a single beam level (0 = primary beam, 1 = secondary for 16ths, etc.)
fn draw_beam_level(
    notes: &[BeamNote],
    level: usize,
    start_y: f64,
    end_y: f64,
    stem_dir: StemDirection,
    spatium: f64,
    config: &BeamLayoutConfig,
) -> Vec<PaintCommand> {
    let mut commands = Vec::new();

    // Offset for this beam level
    let level_offset = match stem_dir {
        StemDirection::Up | StemDirection::Auto => {
            (config.beam_thickness + config.beam_spacing) * spatium * level as f64
        }
        StemDirection::Down => {
            -(config.beam_thickness + config.beam_spacing) * spatium * level as f64
        }
    };

    // Find segments where this beam level applies
    let segments = find_beam_segments(notes, level);

    for (start_idx, end_idx) in segments {
        let segment_start = &notes[start_idx];
        let segment_end = &notes[end_idx];

        // Calculate beam anchor X positions for endpoints using correct anchor types
        // First note of segment uses Start anchor, last note uses End anchor
        let seg_start_stem_x = chord_beam_anchor_x(
            segment_start,
            stem_dir,
            ChordBeamAnchorType::Start,
            spatium,
        );
        let seg_end_stem_x = chord_beam_anchor_x(
            segment_end,
            stem_dir,
            ChordBeamAnchorType::End,
            spatium,
        );

        // Calculate beam Y at segment endpoints using actual stem X positions
        // This ensures the beam Y matches where the stems actually are
        let first_x = notes[0].x;
        let last_x = notes.last().unwrap().x;
        let run = last_x - first_x;

        let (seg_start_y, seg_end_y) = if run.abs() < 0.001 {
            (start_y + level_offset, end_y + level_offset)
        } else {
            // Use stem X positions for Y interpolation, not note.x
            let t1 = (seg_start_stem_x - first_x) / run;
            let t2 = (seg_end_stem_x - first_x) / run;
            let y1 = start_y + t1 * (end_y - start_y) + level_offset;
            let y2 = start_y + t2 * (end_y - start_y) + level_offset;
            (y1, y2)
        };

        // Draw beam as filled polygon (connecting stem positions)
        let half_thickness = config.beam_thickness * spatium / 2.0;

        let mut path = BezPath::new();
        path.move_to(Point::new(seg_start_stem_x, seg_start_y - half_thickness));
        path.line_to(Point::new(seg_end_stem_x, seg_end_y - half_thickness));
        path.line_to(Point::new(seg_end_stem_x, seg_end_y + half_thickness));
        path.line_to(Point::new(seg_start_stem_x, seg_start_y + half_thickness));
        path.close_path();

        commands.push(PaintCommand::filled_path(path, Color::BLACK));
    }

    // Draw beamlets for isolated notes at this level
    let beamlets = find_beamlets(notes, level);
    for (note_idx, is_before) in beamlets {
        let note = &notes[note_idx];
        let note_stem_x = stem_x_for_note(note, stem_dir, spatium);

        // Calculate beam Y at the stem position (not note.x)
        let first_x = notes[0].x;
        let last_x = notes.last().unwrap().x;
        let run = last_x - first_x;

        let note_beam_y = if run.abs() < 0.001 {
            start_y + level_offset
        } else {
            // Use stem X for interpolation
            let t = (note_stem_x - first_x) / run;
            start_y + t * (end_y - start_y) + level_offset
        };

        let beamlet_len = config.beamlet_length * spatium;
        let (beamlet_start_x, beamlet_end_x) = if is_before {
            (note_stem_x - beamlet_len, note_stem_x)
        } else {
            (note_stem_x, note_stem_x + beamlet_len)
        };

        // Slope adjustment for beamlet endpoints
        let slope_per_unit = if run.abs() > 0.001 {
            (end_y - start_y) / run
        } else {
            0.0
        };

        let beamlet_start_y = note_beam_y + slope_per_unit * (beamlet_start_x - note_stem_x);
        let beamlet_end_y = note_beam_y + slope_per_unit * (beamlet_end_x - note_stem_x);

        let half_thickness = config.beam_thickness * spatium / 2.0;

        let mut path = BezPath::new();
        path.move_to(Point::new(beamlet_start_x, beamlet_start_y - half_thickness));
        path.line_to(Point::new(beamlet_end_x, beamlet_end_y - half_thickness));
        path.line_to(Point::new(beamlet_end_x, beamlet_end_y + half_thickness));
        path.line_to(Point::new(beamlet_start_x, beamlet_start_y + half_thickness));
        path.close_path();

        commands.push(PaintCommand::filled_path(path, Color::BLACK));
    }

    commands
}

/// Find continuous segments where a beam level applies.
/// Returns (start_index, end_index) pairs.
fn find_beam_segments(notes: &[BeamNote], level: usize) -> Vec<(usize, usize)> {
    let mut segments = Vec::new();
    let mut segment_start: Option<usize> = None;

    for (i, note) in notes.iter().enumerate() {
        let has_beam_at_level = note.beam_count() > level;

        if has_beam_at_level {
            if segment_start.is_none() {
                segment_start = Some(i);
            }
        } else if let Some(start) = segment_start {
            // End of segment
            if i > start + 1 {
                // Only add if segment has at least 2 notes
                segments.push((start, i - 1));
            }
            segment_start = None;
        }
    }

    // Handle segment at end
    if let Some(start) = segment_start {
        if notes.len() > start + 1 {
            segments.push((start, notes.len() - 1));
        }
    }

    // For primary beam (level 0), ensure we have at least one segment spanning all notes
    if level == 0 && segments.is_empty() && notes.len() >= 2 {
        segments.push((0, notes.len() - 1));
    }

    segments
}

/// Find notes that need beamlets at a given level.
/// Returns (note_index, is_before) pairs.
fn find_beamlets(notes: &[BeamNote], level: usize) -> Vec<(usize, bool)> {
    let mut beamlets = Vec::new();

    for (i, note) in notes.iter().enumerate() {
        let has_beam = note.beam_count() > level;
        if !has_beam {
            continue;
        }

        // Check if neighbors have this beam level
        let prev_has = i > 0 && notes[i - 1].beam_count() > level;
        let next_has = i < notes.len() - 1 && notes[i + 1].beam_count() > level;

        // Need beamlet if isolated at this level
        if !prev_has && !next_has {
            // Determine direction based on position in group
            let is_before = i > notes.len() / 2;
            beamlets.push((i, is_before));
        }
    }

    beamlets
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_beam_direction_auto() {
        let notes = vec![
            BeamNote {
                x: 0.0,
                line: -2,
                duration: NoteDuration::Eighth,
                stem_direction: StemDirection::Auto,
            },
            BeamNote {
                x: 20.0,
                line: -4,
                duration: NoteDuration::Eighth,
                stem_direction: StemDirection::Auto,
            },
        ];

        let dir = determine_beam_direction(&notes);
        assert_eq!(dir, StemDirection::Up); // Notes above middle line
    }

    #[test]
    fn test_beam_two_eighths() {
        let notes = vec![
            BeamNote {
                x: 0.0,
                line: 0,
                duration: NoteDuration::Eighth,
                stem_direction: StemDirection::Up,
            },
            BeamNote {
                x: 25.0,
                line: -2,
                duration: NoteDuration::Eighth,
                stem_direction: StemDirection::Up,
            },
        ];

        let config = BeamLayoutConfig::default();
        let result = layout_beam(&notes, 5.0, &config);

        assert!(!result.commands.is_empty());
        assert_eq!(result.stem_tips.len(), 2);
    }

    #[test]
    fn test_beam_with_sixteenths() {
        let notes = vec![
            BeamNote {
                x: 0.0,
                line: 0,
                duration: NoteDuration::Sixteenth,
                stem_direction: StemDirection::Up,
            },
            BeamNote {
                x: 15.0,
                line: -1,
                duration: NoteDuration::Sixteenth,
                stem_direction: StemDirection::Up,
            },
            BeamNote {
                x: 30.0,
                line: -2,
                duration: NoteDuration::Eighth,
                stem_direction: StemDirection::Up,
            },
        ];

        let config = BeamLayoutConfig::default();
        let result = layout_beam(&notes, 5.0, &config);

        // Should have primary beam and partial secondary beam
        assert!(!result.commands.is_empty());
    }
}
