//! Layout-related model types for score rendering.
//!
//! This module contains types related to how music is laid out on the page,
//! including line/page breaks, system organization, and rehearsal marks.

use serde::{Deserialize, Serialize};

/// A rehearsal mark (section marker) in a score.
///
/// Rehearsal marks are used to:
/// - Label sections (Intro, Verse, Chorus, Bridge, etc.)
/// - Force line breaks at section boundaries
/// - Provide navigation points for musicians
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RehearsalMark {
    /// The text to display (e.g., "A", "Verse", "Chorus")
    pub text: String,
    /// Whether this mark forces a line break
    pub forces_break: bool,
    /// Style of the rehearsal mark
    pub style: RehearsalMarkStyle,
}

impl RehearsalMark {
    /// Create a new rehearsal mark with the given text.
    #[must_use]
    pub fn new(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            forces_break: true, // By default, section markers force breaks
            style: RehearsalMarkStyle::default(),
        }
    }

    /// Create a section marker (Intro, Verse, etc.)
    #[must_use]
    pub fn section(name: impl Into<String>) -> Self {
        Self::new(name)
    }

    /// Create a simple letter marker (A, B, C, etc.)
    #[must_use]
    pub fn letter(letter: char) -> Self {
        Self {
            text: letter.to_string(),
            forces_break: true,
            style: RehearsalMarkStyle::Boxed,
        }
    }

    /// Set whether this mark forces a line break.
    #[must_use]
    pub fn with_break(mut self, forces_break: bool) -> Self {
        self.forces_break = forces_break;
        self
    }

    /// Set the style of the rehearsal mark.
    #[must_use]
    pub fn with_style(mut self, style: RehearsalMarkStyle) -> Self {
        self.style = style;
        self
    }
}

/// Style options for rehearsal marks.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, PartialEq, Eq)]
pub enum RehearsalMarkStyle {
    /// No box, just text
    #[default]
    Plain,
    /// Text in a rectangular box
    Boxed,
    /// Text in a rounded rectangle (capsule)
    Capsule,
    /// Text in a circle (for single letters/numbers)
    Circle,
}

/// A layout break indicating where to start a new line, page, or section.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum LayoutBreak {
    /// Force a line (system) break after this measure
    Line,
    /// Force a page break after this measure
    Page,
    /// Section break (implies line break, also affects spacing)
    Section,
    /// Prevent automatic breaks at this point
    NoBreak,
}

/// Policy for determining line breaks in a score.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LineBreakPolicy {
    /// Automatic line breaks based on available width
    Auto,
    /// Fixed number of measures per line (unless a section starts)
    FixedMeasuresPerLine {
        /// Number of measures per line
        measures: u32,
        /// Whether to break at section markers regardless of measure count
        break_at_sections: bool,
    },
    /// Break only at explicit markers and section boundaries
    SectionBased,
    /// Break only at explicit markers
    ExplicitOnly,
}

impl Default for LineBreakPolicy {
    fn default() -> Self {
        Self::FixedMeasuresPerLine {
            measures: 4,
            break_at_sections: true,
        }
    }
}

impl LineBreakPolicy {
    /// Create a policy with 4 measures per line, breaking at sections.
    #[must_use]
    pub fn four_per_line() -> Self {
        Self::FixedMeasuresPerLine {
            measures: 4,
            break_at_sections: true,
        }
    }

    /// Create a policy with a custom number of measures per line.
    #[must_use]
    pub fn measures_per_line(count: u32) -> Self {
        Self::FixedMeasuresPerLine {
            measures: count,
            break_at_sections: true,
        }
    }

    /// Create an auto-layout policy (fill to width).
    #[must_use]
    pub fn auto() -> Self {
        Self::Auto
    }
}

/// Information about a system (line of music).
///
/// A system is a horizontal grouping of measures that spans one line
/// across the page, potentially including multiple staves.
#[derive(Debug, Clone, Default)]
pub struct SystemInfo {
    /// Index of the first measure in this system (0-based)
    pub start_measure: usize,
    /// Number of measures in this system
    pub measure_count: usize,
    /// Whether this is the first system of a section
    pub is_section_start: bool,
    /// The rehearsal mark at the start of this system, if any
    pub rehearsal_mark: Option<RehearsalMark>,
    /// Whether this system starts a new page
    pub starts_new_page: bool,
}

impl SystemInfo {
    /// Create a new system info.
    #[must_use]
    pub fn new(start_measure: usize, measure_count: usize) -> Self {
        Self {
            start_measure,
            measure_count,
            is_section_start: false,
            rehearsal_mark: None,
            starts_new_page: false,
        }
    }

    /// Get the index of the last measure in this system.
    #[must_use]
    pub fn end_measure(&self) -> usize {
        self.start_measure + self.measure_count.saturating_sub(1)
    }
}

/// Result of laying out measures into systems.
#[derive(Debug, Clone, Default)]
pub struct SystemLayout {
    /// Information about each system
    pub systems: Vec<SystemInfo>,
    /// Total number of measures
    pub total_measures: usize,
}

impl SystemLayout {
    /// Create a new empty system layout.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a system layout with the given number of measures per line.
    ///
    /// This is a simple algorithm that doesn't consider section breaks.
    /// For section-aware layout, use `from_score_with_policy`.
    #[must_use]
    pub fn fixed_measures_per_line(total_measures: usize, measures_per_line: usize) -> Self {
        let mut systems = Vec::new();
        let mut current = 0;

        while current < total_measures {
            let count = (total_measures - current).min(measures_per_line);
            systems.push(SystemInfo::new(current, count));
            current += count;
        }

        Self {
            systems,
            total_measures,
        }
    }

    /// Get the number of systems.
    #[must_use]
    pub fn system_count(&self) -> usize {
        self.systems.len()
    }

    /// Get information about a specific system.
    #[must_use]
    pub fn get_system(&self, index: usize) -> Option<&SystemInfo> {
        self.systems.get(index)
    }

    /// Find which system a measure belongs to.
    #[must_use]
    pub fn system_for_measure(&self, measure_index: usize) -> Option<usize> {
        self.systems.iter().position(|sys| {
            measure_index >= sys.start_measure && measure_index <= sys.end_measure()
        })
    }
}

/// Compute system layout from measures with rehearsal marks.
///
/// # Arguments
/// * `total_measures` - Total number of measures in the score
/// * `section_starts` - Indices of measures that start new sections (have rehearsal marks)
/// * `policy` - The line break policy to use
///
/// # Returns
/// A `SystemLayout` describing how measures are grouped into systems
pub fn compute_system_layout(
    total_measures: usize,
    section_starts: &[usize],
    policy: &LineBreakPolicy,
) -> SystemLayout {
    if total_measures == 0 {
        return SystemLayout::default();
    }

    match policy {
        LineBreakPolicy::Auto => {
            // For now, auto just uses 4 per line
            // In the future, this would compute based on width
            SystemLayout::fixed_measures_per_line(total_measures, 4)
        }
        LineBreakPolicy::FixedMeasuresPerLine {
            measures,
            break_at_sections,
        } => {
            compute_fixed_with_sections(
                total_measures,
                section_starts,
                *measures as usize,
                *break_at_sections,
            )
        }
        LineBreakPolicy::SectionBased => {
            compute_section_based_layout(total_measures, section_starts)
        }
        LineBreakPolicy::ExplicitOnly => {
            // One big system (in practice, explicit breaks would be handled separately)
            SystemLayout {
                systems: vec![SystemInfo::new(0, total_measures)],
                total_measures,
            }
        }
    }
}

/// Compute layout with fixed measures per line, breaking at sections.
fn compute_fixed_with_sections(
    total_measures: usize,
    section_starts: &[usize],
    measures_per_line: usize,
    break_at_sections: bool,
) -> SystemLayout {
    let mut systems: Vec<SystemInfo> = Vec::new();
    let mut current = 0;
    let mut measures_in_current_line = 0;

    while current < total_measures {
        // Check if we're at a section start
        let at_section_start = section_starts.contains(&current);

        // Determine if we need to start a new system
        let need_new_system = systems.is_empty()
            || (break_at_sections && at_section_start && measures_in_current_line > 0)
            || measures_in_current_line >= measures_per_line;

        if need_new_system && !systems.is_empty() {
            // Finalize the previous system with its measure count
            if let Some(last) = systems.last_mut() {
                last.measure_count = measures_in_current_line;
            }
            measures_in_current_line = 0;
        }

        if systems.is_empty() || need_new_system {
            let mut sys = SystemInfo::new(current, 0);
            sys.is_section_start = at_section_start;
            systems.push(sys);
        }

        current += 1;
        measures_in_current_line += 1;
    }

    // Finalize the last system
    if let Some(last) = systems.last_mut() {
        last.measure_count = measures_in_current_line;
    }

    SystemLayout {
        systems,
        total_measures,
    }
}

/// Compute layout based purely on section boundaries.
fn compute_section_based_layout(total_measures: usize, section_starts: &[usize]) -> SystemLayout {
    if section_starts.is_empty() {
        return SystemLayout {
            systems: vec![SystemInfo::new(0, total_measures)],
            total_measures,
        };
    }

    let mut systems = Vec::new();
    let mut sorted_starts: Vec<usize> = section_starts.to_vec();
    sorted_starts.sort_unstable();

    // Add 0 if not present (start of score)
    if sorted_starts.first() != Some(&0) {
        sorted_starts.insert(0, 0);
    }

    for (i, &start) in sorted_starts.iter().enumerate() {
        let end = sorted_starts.get(i + 1).copied().unwrap_or(total_measures);
        let mut sys = SystemInfo::new(start, end - start);
        sys.is_section_start = i > 0 || section_starts.contains(&0);
        systems.push(sys);
    }

    SystemLayout {
        systems,
        total_measures,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixed_measures_per_line() {
        let layout = SystemLayout::fixed_measures_per_line(16, 4);
        assert_eq!(layout.system_count(), 4);
        assert_eq!(layout.systems[0].start_measure, 0);
        assert_eq!(layout.systems[0].measure_count, 4);
        assert_eq!(layout.systems[3].start_measure, 12);
        assert_eq!(layout.systems[3].measure_count, 4);
    }

    #[test]
    fn test_fixed_with_sections() {
        let section_starts = vec![0, 8]; // Intro at 0, Verse at 8
        let layout = compute_system_layout(
            16,
            &section_starts,
            &LineBreakPolicy::FixedMeasuresPerLine {
                measures: 4,
                break_at_sections: true,
            },
        );

        // Should break at measure 8 (section start) even though we're at 4 measures
        // Expected: [0-3], [4-7], [8-11], [12-15]
        assert_eq!(layout.system_count(), 4);
    }

    #[test]
    fn test_section_based_layout() {
        let section_starts = vec![0, 4, 12];
        let layout = compute_system_layout(16, &section_starts, &LineBreakPolicy::SectionBased);

        // Should have 3 systems: [0-3], [4-11], [12-15]
        assert_eq!(layout.system_count(), 3);
        assert_eq!(layout.systems[0].measure_count, 4);
        assert_eq!(layout.systems[1].measure_count, 8);
        assert_eq!(layout.systems[2].measure_count, 4);
    }
}
