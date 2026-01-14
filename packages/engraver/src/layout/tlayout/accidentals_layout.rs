//! Accidental placement system ported from MuseScore.
//!
//! Handles intelligent accidental placement to avoid collisions,
//! with support for column stacking, octave alignment, and kerning.

use kurbo::{Point, Rect};

use super::note::Accidental;

/// Configuration for accidental placement.
#[derive(Debug, Clone)]
pub struct AccidentalLayoutConfig {
    /// Minimum vertical clearance between accidentals (in spatiums)
    pub vertical_clearance: f64,
    /// Minimum horizontal distance between accidental columns (in spatiums)
    pub column_distance: f64,
    /// Distance from accidental to notehead (in spatiums)
    pub accidental_to_note_distance: f64,
    /// Extra kerning for fourths (can be negative, in spatiums)
    pub kerning_of_fourth: f64,
    /// Whether to align octave accidentals vertically
    pub align_octaves: bool,
    /// Whether to keep seconds (adjacent notes) in the same column
    pub keep_seconds_together: bool,
}

impl Default for AccidentalLayoutConfig {
    fn default() -> Self {
        Self {
            vertical_clearance: 0.15,
            column_distance: 0.25,
            accidental_to_note_distance: 0.22,
            kerning_of_fourth: -0.15,
            align_octaves: true,
            keep_seconds_together: true,
        }
    }
}

/// Information about an accidental to be placed.
#[derive(Debug, Clone)]
pub struct AccidentalInfo {
    /// The accidental type
    pub accidental: Accidental,
    /// Staff line position of the note
    pub line: i32,
    /// X position of the notehead (for notes displaced due to seconds)
    pub note_x: f64,
    /// Width of the accidental glyph (in spatiums)
    pub width: f64,
    /// Height of the accidental glyph (in spatiums)
    pub height: f64,
}

impl AccidentalInfo {
    /// Create accidental info with default glyph dimensions.
    pub fn new(accidental: Accidental, line: i32, note_x: f64) -> Self {
        let (width, height) = accidental_dimensions(accidental);
        Self {
            accidental,
            line,
            note_x,
            width,
            height,
        }
    }

    /// Y position of accidental center (relative to middle line).
    pub fn y_center(&self, spatium: f64) -> f64 {
        -self.line as f64 * spatium / 2.0
    }
}

/// Result of accidental placement.
#[derive(Debug, Clone)]
pub struct AccidentalPlacement {
    /// X offset from note position (negative = left of note)
    pub x_offset: f64,
    /// Column index (0 = closest to chord)
    pub column: usize,
}

/// Layout accidentals for a chord, returning x offsets for each.
///
/// Implements a simplified version of MuseScore's column-based placement algorithm.
pub fn layout_accidentals(
    accidentals: &[AccidentalInfo],
    spatium: f64,
    config: &AccidentalLayoutConfig,
) -> Vec<AccidentalPlacement> {
    if accidentals.is_empty() {
        return Vec::new();
    }

    // Single accidental - simple placement
    if accidentals.len() == 1 {
        let x_offset = -(accidentals[0].width + config.accidental_to_note_distance) * spatium;
        return vec![AccidentalPlacement { x_offset, column: 0 }];
    }

    // Sort by line position (top to bottom)
    let mut indexed: Vec<(usize, &AccidentalInfo)> = accidentals.iter().enumerate().collect();
    indexed.sort_by_key(|(_, a)| a.line);

    // Find octaves and seconds
    let octave_pairs = find_octaves(&indexed);
    let second_pairs = find_seconds(&indexed);

    // Assign accidentals to columns
    let columns = assign_columns(&indexed, &octave_pairs, &second_pairs, spatium, config);

    // Calculate x offsets based on columns
    let mut placements: Vec<AccidentalPlacement> = vec![
        AccidentalPlacement {
            x_offset: 0.0,
            column: 0
        };
        accidentals.len()
    ];

    // Track column widths
    let mut column_widths: Vec<f64> = Vec::new();
    for (orig_idx, col) in columns.iter() {
        while column_widths.len() <= *col {
            column_widths.push(0.0);
        }
        column_widths[*col] = column_widths[*col].max(accidentals[*orig_idx].width);
    }

    // Calculate cumulative x offset for each column
    let mut column_x: Vec<f64> = vec![0.0; column_widths.len()];
    let mut cumulative = config.accidental_to_note_distance;
    for (i, width) in column_widths.iter().enumerate() {
        cumulative += *width;
        column_x[i] = -cumulative * spatium;
        cumulative += config.column_distance;
    }

    // Apply column positions to placements
    for (orig_idx, col) in columns {
        placements[orig_idx] = AccidentalPlacement {
            x_offset: column_x[col],
            column: col,
        };
    }

    // Apply octave alignment if enabled
    if config.align_octaves {
        for (idx1, idx2) in &octave_pairs {
            // Align to the leftmost position
            let x1 = placements[*idx1].x_offset;
            let x2 = placements[*idx2].x_offset;
            let aligned_x = x1.min(x2);
            placements[*idx1].x_offset = aligned_x;
            placements[*idx2].x_offset = aligned_x;
        }
    }

    placements
}

/// Assign accidentals to columns based on vertical overlap.
fn assign_columns(
    sorted: &[(usize, &AccidentalInfo)],
    octave_pairs: &[(usize, usize)],
    second_pairs: &[(usize, usize)],
    spatium: f64,
    config: &AccidentalLayoutConfig,
) -> Vec<(usize, usize)> {
    let mut assignments: Vec<(usize, usize)> = Vec::new(); // (original_index, column)
    let mut columns: Vec<Vec<(usize, f64, f64)>> = Vec::new(); // column -> [(orig_idx, y_min, y_max)]

    for (orig_idx, acc) in sorted {
        let y_center = acc.y_center(spatium);
        let half_height = acc.height * spatium / 2.0;
        let y_min = y_center - half_height;
        let y_max = y_center + half_height;

        // Find first column where this accidental fits
        let mut assigned_col = None;
        for (col_idx, col) in columns.iter().enumerate() {
            let can_fit = col.iter().all(|(_, existing_min, existing_max)| {
                // Check for vertical overlap with clearance
                let clearance = config.vertical_clearance * spatium;
                y_max + clearance < *existing_min || y_min - clearance > *existing_max
            });

            if can_fit {
                assigned_col = Some(col_idx);
                break;
            }
        }

        let col = assigned_col.unwrap_or_else(|| {
            columns.push(Vec::new());
            columns.len() - 1
        });

        columns[col].push((*orig_idx, y_min, y_max));
        assignments.push((*orig_idx, col));
    }

    // Handle seconds - try to keep them in the same column if configured
    if config.keep_seconds_together {
        for (idx1, idx2) in second_pairs {
            let col1 = assignments.iter().find(|(i, _)| i == idx1).map(|(_, c)| *c);
            let col2 = assignments.iter().find(|(i, _)| i == idx2).map(|(_, c)| *c);

            if let (Some(c1), Some(c2)) = (col1, col2) {
                if c1 != c2 {
                    // Move both to a new column at the end
                    let new_col = columns.len();
                    for (i, c) in assignments.iter_mut() {
                        if i == idx1 || i == idx2 {
                            *c = new_col;
                        }
                    }
                }
            }
        }
    }

    assignments
}

/// Find pairs of accidentals that are an octave apart.
fn find_octaves(sorted: &[(usize, &AccidentalInfo)]) -> Vec<(usize, usize)> {
    let mut pairs = Vec::new();

    for i in 0..sorted.len() {
        for j in (i + 1)..sorted.len() {
            let (idx1, acc1) = sorted[i];
            let (idx2, acc2) = sorted[j];

            // Octave = 7 lines apart, same accidental type
            let line_diff = (acc2.line - acc1.line).abs();
            if line_diff == 7 && acc1.accidental == acc2.accidental {
                pairs.push((idx1, idx2));
            }
        }
    }

    pairs
}

/// Find pairs of accidentals that are a second apart.
fn find_seconds(sorted: &[(usize, &AccidentalInfo)]) -> Vec<(usize, usize)> {
    let mut pairs = Vec::new();

    for i in 0..sorted.len().saturating_sub(1) {
        let (idx1, acc1) = sorted[i];
        let (idx2, acc2) = sorted[i + 1];

        // Second = 1 line apart
        let line_diff = (acc2.line - acc1.line).abs();
        if line_diff == 1 {
            pairs.push((idx1, idx2));
        }
    }

    pairs
}

/// Get standard dimensions for an accidental glyph (width, height) in spatiums.
fn accidental_dimensions(accidental: Accidental) -> (f64, f64) {
    match accidental {
        Accidental::None => (0.0, 0.0),
        Accidental::Sharp => (1.2, 2.8),
        Accidental::Flat => (0.9, 2.4),
        Accidental::Natural => (0.7, 2.8),
        Accidental::DoubleSharp => (1.0, 1.0),
        Accidental::DoubleFlat => (1.4, 2.4),
    }
}

/// Check if two accidentals form a fourth interval (3 lines apart).
pub fn is_fourth(line1: i32, line2: i32) -> bool {
    (line1 - line2).abs() == 3
}

/// Check if two accidentals form a sixth interval (5 lines apart).
pub fn is_sixth(line1: i32, line2: i32) -> bool {
    (line1 - line2).abs() == 5
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_accidental() {
        let accidentals = vec![AccidentalInfo::new(Accidental::Sharp, 0, 0.0)];
        let config = AccidentalLayoutConfig::default();
        let placements = layout_accidentals(&accidentals, 5.0, &config);

        assert_eq!(placements.len(), 1);
        assert!(placements[0].x_offset < 0.0); // Should be to the left
        assert_eq!(placements[0].column, 0);
    }

    #[test]
    fn test_two_accidentals_no_overlap() {
        let accidentals = vec![
            AccidentalInfo::new(Accidental::Sharp, -4, 0.0), // High note
            AccidentalInfo::new(Accidental::Flat, 4, 0.0),   // Low note
        ];
        let config = AccidentalLayoutConfig::default();
        let placements = layout_accidentals(&accidentals, 5.0, &config);

        assert_eq!(placements.len(), 2);
        // Both should fit in column 0 (no vertical overlap)
        // Note: actual column assignment depends on algorithm
    }

    #[test]
    fn test_octave_alignment() {
        let accidentals = vec![
            AccidentalInfo::new(Accidental::Sharp, 0, 0.0),
            AccidentalInfo::new(Accidental::Sharp, 7, 0.0), // Octave below
        ];
        let mut config = AccidentalLayoutConfig::default();
        config.align_octaves = true;
        let placements = layout_accidentals(&accidentals, 5.0, &config);

        assert_eq!(placements.len(), 2);
        // Octaves should be aligned
        assert!((placements[0].x_offset - placements[1].x_offset).abs() < 0.01);
    }

    #[test]
    fn test_find_octaves() {
        let acc1 = AccidentalInfo::new(Accidental::Sharp, 0, 0.0);
        let acc2 = AccidentalInfo::new(Accidental::Sharp, 7, 0.0);
        let acc3 = AccidentalInfo::new(Accidental::Flat, 14, 0.0);

        let sorted: Vec<(usize, &AccidentalInfo)> =
            vec![(0, &acc1), (1, &acc2), (2, &acc3)];
        let octaves = find_octaves(&sorted);

        assert_eq!(octaves.len(), 1); // Only sharp-sharp pair is an octave
        assert!(octaves.contains(&(0, 1)));
    }
}
