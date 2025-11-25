use crate::chart::Chart;
use crate::primitives::Note;

/// Represents a group of measures with optional repeat annotation
#[derive(Debug)]
struct MeasureGroup {
    start_idx: usize,
    count: usize,          // Repeat count (1 = no repeat)
    pattern_length: usize, // Number of measures in the pattern before repeat
}

// ANSI color codes for beautiful terminal output
mod colors {
    pub const RESET: &str = "\x1b[0m";
    pub const BOLD: &str = "\x1b[1m";
    pub const DIM: &str = "\x1b[2m";

    // Semantic colors for different information types
    pub const METADATA: &str = "\x1b[96m"; // Bright Cyan - titles, artists
    pub const SECTION: &str = "\x1b[95m"; // Bright Magenta - section names
    pub const DURATION: &str = "\x1b[93m"; // Bright Yellow - measure counts, positions
    pub const CHORD: &str = "\x1b[92m"; // Bright Green - chord symbols
    pub const KEY_SIG: &str = "\x1b[94m"; // Bright Blue - keys and key changes
    pub const TIME_SIG: &str = "\x1b[91m"; // Bright Red - time signatures
    pub const TEMPO: &str = "\x1b[33m"; // Yellow - tempo markings
    pub const MEASURE_NUM: &str = "\x1b[90m"; // Dark Gray - measure numbers
    pub const BORDER: &str = "\x1b[37m"; // White - borders and dividers
}

impl Chart {
    /// Display measures using a grid-based system
    /// - 4 measures per line
    /// - Each measure divided into 16th note slots (16 slots per measure in 4/4)
    /// - Chords placed at their exact positions
    /// - Visual markers for beat divisions
    fn display_measures_grid(
        f: &mut std::fmt::Formatter<'_>,
        measures: &[crate::chart::types::Measure],
        section_start_measures: u32,
        border: &str,
        bold: &str,
        reset: &str,
        chord_color: &str,
        dim: &str,
    ) -> std::fmt::Result {
        Self::display_measures_grid_with_next_section(
            f, measures, section_start_measures, None, border, bold, reset, chord_color, dim
        )
    }
    
    fn display_measures_grid_with_next_section(
        f: &mut std::fmt::Formatter<'_>,
        measures: &[crate::chart::types::Measure],
        section_start_measures: u32,
        next_section_first_measure_chords: Option<&[&crate::chart::types::ChordInstance]>,
        border: &str,
        bold: &str,
        reset: &str,
        chord_color: &str,
        dim: &str,
    ) -> std::fmt::Result {
        const MEASURES_PER_LINE: usize = 4;
        const SLOTS_PER_MEASURE: usize = 16; // 16th notes in 4/4 time
        const CHARS_PER_SLOT: usize = 2; // Fixed width per slot for even division
        
        let mut measure_idx = 0;
        let time_sig = if !measures.is_empty() {
            crate::time::TimeSignature::new(measures[0].time_signature.0, measures[0].time_signature.1)
        } else {
            crate::time::TimeSignature::common_time()
        };
        let beats_per_measure = time_sig.numerator as usize;
        let slots_per_beat = SLOTS_PER_MEASURE / beats_per_measure; // Usually 4 for 4/4
        let _subdivisions_per_slot = (beats_per_measure * 1000) / SLOTS_PER_MEASURE; // Usually 250 for 4/4

        while measure_idx < measures.len() {
            // Start a new line
            write!(f, "{}{}║{} ", border, bold, reset)?;

            // Determine how many measures to show on this line
            let measures_on_line = MEASURES_PER_LINE.min(measures.len() - measure_idx);
            let line_end_idx = measure_idx + measures_on_line;

            // Collect all chords from measures on this line with their positions
            // Handle pushed chords that belong to previous measure
            // Also check next section's pushed chords if provided
            let mut chords_with_positions: Vec<(usize, usize, &crate::chart::types::ChordInstance)> = Vec::new();
            
            // Check next section's pushed chords if provided and if last measure is on this line
            if let Some(next_chords) = next_section_first_measure_chords {
                let last_measure_abs = section_start_measures + measures.len() as u32 - 1;
                let last_measure_idx = measures.len() - 1;
                
                // Check if last measure is on this line
                if last_measure_idx >= measure_idx && last_measure_idx < line_end_idx {
                    for chord in next_chords.iter() {
                        let chord_abs_measures = chord.position.total_duration.measures;
                        // If this chord belongs to the last measure of current section (pushed from next section)
                        if chord_abs_measures == last_measure_abs {
                            let chord_abs_beats = chord.position.total_duration.beats;
                            let chord_abs_subdivisions = chord.position.total_duration.subdivisions;
                            let beats_in_slots = chord_abs_beats as usize * slots_per_beat;
                            let subdivisions_in_slots = (chord_abs_subdivisions as usize * SLOTS_PER_MEASURE) / (beats_per_measure * 1000);
                            let slot = beats_in_slots + subdivisions_in_slots;
                            let target_m_idx = last_measure_idx - measure_idx;
                            if target_m_idx < measures_on_line {
                                chords_with_positions.push((target_m_idx, slot.min(SLOTS_PER_MEASURE - 1), *chord));
                            }
                        }
                    }
                }
            }
            
            // Collect all chords from all measures on this line, then place them by their actual position
            for (m_idx, measure) in measures[measure_idx..line_end_idx].iter().enumerate() {
                let measure_abs_measures = section_start_measures + (measure_idx + m_idx) as u32;
                
                for chord in &measure.chords {
                    // Calculate position within the measure
                    // chord.position.total_duration is absolute from song start
                    let chord_abs_measures = chord.position.total_duration.measures;
                    let chord_abs_beats = chord.position.total_duration.beats;
                    let chord_abs_subdivisions = chord.position.total_duration.subdivisions;
                    
                    // Determine which measure on this line this chord belongs to based on its position
                    // Chords can be pushed into previous measures, so check all measures on this line
                    let target_measure_abs = chord_abs_measures;
                    
                    // Check if this chord belongs to any measure on this line
                    let target_m_idx_opt = (0..measures_on_line).find(|&line_m_idx| {
                        let line_measure_abs = section_start_measures + (measure_idx + line_m_idx) as u32;
                        target_measure_abs == line_measure_abs
                    });
                    
                    if let Some(target_m_idx) = target_m_idx_opt {
                        // Calculate slot within the target measure
                        let beats_in_slots = chord_abs_beats as usize * slots_per_beat;
                        let subdivisions_in_slots = (chord_abs_subdivisions as usize * SLOTS_PER_MEASURE) / (beats_per_measure * 1000);
                        let slot = beats_in_slots + subdivisions_in_slots;
                        let slot = slot.min(SLOTS_PER_MEASURE - 1);
                        chords_with_positions.push((target_m_idx, slot, chord));
                    }
                }
            }

            // Create grid for this line: [measure][slot]
            let mut grid: Vec<Vec<Option<&crate::chart::types::ChordInstance>>> = 
                vec![vec![None; SLOTS_PER_MEASURE]; measures_on_line];

            // Place chords in grid
            for (m_idx, slot, chord) in chords_with_positions {
                if m_idx < measures_on_line && slot < SLOTS_PER_MEASURE {
                    grid[m_idx][slot] = Some(chord);
                }
            }

            // Display the grid
            for measure_idx_in_line in 0..measures_on_line {
                let measure = &measures[measure_idx + measure_idx_in_line];
                
                // Show text cues if any
                if !measure.text_cues.is_empty() {
                    for cue in &measure.text_cues {
                        write!(
                            f,
                            "{}@{} \"{}\"{} ",
                            "\x1b[38;5;208m", // Orange color for cue markers
                            cue.group,
                            cue.text,
                            reset
                        )?;
                    }
                }

                // Display measure with grid - evenly divided into 16 slots of fixed width
                for slot in 0..SLOTS_PER_MEASURE {
                    // Visual markers only at beat boundaries (every 4 slots in 4/4)
                    if slot > 0 && slot % slots_per_beat == 0 {
                        // Beat boundary - subtle marker
                        write!(f, "{}·{}", dim, reset)?;
                    }

                    // Each slot gets exactly CHARS_PER_SLOT characters for even division
                    if let Some(chord) = grid[measure_idx_in_line][slot] {
                        let mut chord_text = String::new();
                        
                        // Show push/pull notation
                        // Push: apostrophe BEFORE chord ('C)
                        // Pull: apostrophe AFTER chord (C')
                        if let Some((is_push, _amount)) = chord.push_pull {
                            if is_push {
                                chord_text.push('\'');
                            }
                        }
                        
                        chord_text.push_str(&chord.full_symbol);
                        
                        // Show bass note for slash chords
                        if let Some(ref bass) = chord.parsed.bass {
                            chord_text.push_str(&format!("/{}", bass));
                        }
                        
                        // Show pull notation (apostrophe AFTER)
                        if let Some((is_push, _amount)) = chord.push_pull {
                            if !is_push {
                                chord_text.push('\'');
                            }
                        }
                        
                        // Write chord (can overflow slot width if needed)
                        write!(f, "{}{}{}", chord_color, bold, chord_text)?;
                        write!(f, "{}", reset)?;
                        
                        // Pad to fixed slot width for alignment
                        let visible_len = chord_text.chars().count();
                        if visible_len < CHARS_PER_SLOT {
                            for _ in visible_len..CHARS_PER_SLOT {
                                write!(f, " ")?;
                            }
                        }
                    } else {
                        // Empty slot - pad with spaces to maintain grid
                        for _ in 0..CHARS_PER_SLOT {
                            write!(f, " ")?;
                        }
                    }
                }

                // Measure separator (except for last measure on line)
                if measure_idx_in_line < measures_on_line - 1 {
                    write!(f, " {}|{} ", dim, reset)?;
                }
            }

            writeln!(f)?;
            measure_idx = line_end_idx;
        }

        Ok(())
    }

    /// Group measures for display, respecting explicit repeat annotations
    /// Returns groups where each group represents either:
    /// - A sequence of measures with a repeat annotation at the end
    /// - A single measure without repeat
    fn group_repeating_measures(measures: &[crate::chart::types::Measure]) -> Vec<MeasureGroup> {
        if measures.is_empty() {
            return vec![];
        }

        let mut groups = Vec::new();
        let mut i = 0;

        while i < measures.len() {
            // Look ahead to find if there's a repeat annotation in the upcoming measures
            let mut pattern_length = 1;
            let mut repeat_count = 1;

            // Scan forward to find a measure with repeat_count > 1
            for j in i..measures.len() {
                if measures[j].repeat_count > 1 {
                    // Found a repeat annotation - this marks the end of the pattern
                    pattern_length = j - i + 1;
                    repeat_count = measures[j].repeat_count;
                    break;
                } else if j > i && measures[j].repeat_count == 1 {
                    // This is a regular measure, include it
                    pattern_length = j - i + 1;
                }
            }

            groups.push(MeasureGroup {
                start_idx: i,
                count: repeat_count,
                pattern_length,
            });

            // Skip: pattern_length for display + (pattern_length * (repeat_count - 1)) for duplicates
            // This way we show the pattern once, but skip all the duplicated measures
            i += pattern_length * repeat_count;
        }

        groups
    }
}

impl Chart {
    /// Calculate the width needed for displaying 4 measures with the grid system
    fn calculate_content_width(&self) -> usize {
        const MEASURES_PER_LINE: usize = 4;
        const CHARS_PER_SLOT: usize = 2;
        
        // Get time signature (default to 4/4 if not set)
        let time_sig = self.time_signature.unwrap_or_else(|| crate::time::TimeSignature::common_time());
        let beats_per_measure = time_sig.numerator as usize;
        let slots_per_measure = 16; // 16th notes (works for 4/4, adjust for other time sigs if needed)
        
        // Calculate width for one measure:
        // - Slots: slots_per_measure * CHARS_PER_SLOT
        // - Beat markers: (beats_per_measure - 1) * 1 (the · character)
        let measure_width = (slots_per_measure * CHARS_PER_SLOT) + (beats_per_measure - 1);
        
        // Calculate width for 4 measures:
        // - 4 measures
        // - 3 measure separators (|) with spacing: 3 chars each
        let content_width = (measure_width * MEASURES_PER_LINE) + (3 * (MEASURES_PER_LINE - 1));
        
        // Add border padding: "║ " at start (2 chars) + " " at end (1 char) = 3
        content_width + 3
    }
}

impl std::fmt::Display for Chart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colors::*;

        // Calculate dynamic border width based on content
        let content_width = self.calculate_content_width();
        let border_width = content_width.max(55); // Minimum width for header/metadata lines
        
        // Top border
        let top_border = "═".repeat(border_width - 2); // -2 for corner chars
        writeln!(
            f,
            "{}{}╔{}╗{}",
            BORDER, BOLD, top_border, RESET
        )?;

        // Title and artist (METADATA color)
        if let Some(ref title) = self.metadata.title {
            write!(
                f,
                "{}{}║{} {}{}{}",
                BORDER, BOLD, RESET, METADATA, BOLD, title
            )?;
            if let Some(ref artist) = self.metadata.artist {
                write!(f, " {} by {}{}", RESET, METADATA, artist)?;
            }
            writeln!(f, "{}", RESET)?;
        }

        // Tempo, time signature, key on a styled line
        if self.tempo.is_some() || self.time_signature.is_some() || self.initial_key.is_some() {
            write!(f, "{}{}║{} ", BORDER, BOLD, RESET)?;

            if let Some(ref tempo) = self.tempo {
                write!(f, "{}♩ = {}{} bpm  ", TEMPO, tempo.bpm, RESET)?;
            }
            if let Some(ref ts) = self.time_signature {
                write!(
                    f,
                    "{}{}{}/{}{}  ",
                    TIME_SIG, BOLD, ts.numerator, ts.denominator, RESET
                )?;
            }
            if let Some(ref key) = self.initial_key {
                write!(f, "{}Key: {}{}", KEY_SIG, key.root().name(), RESET)?;
            }
            writeln!(f)?;
        }

        // Middle border
        let middle_border = "═".repeat(border_width - 2);
        writeln!(
            f,
            "{}{}╠{}╣{}",
            BORDER, BOLD, middle_border, RESET
        )?;

        // Sections
        // Calculate cumulative measure counts for position tracking
        let mut cumulative_measures = 0u32;
        
        for (section_idx, section) in self.sections.iter().enumerate() {
            if section_idx > 0 {
                writeln!(
                    f,
                    "{}{}║{}   {}───────────────────────────────────────────────────────────{}",
                    BORDER, BOLD, RESET, DIM, RESET
                )?;
            }

            // Section header (SECTION color)
            write!(
                f,
                "{}{}║{} {}{}{}{}",
                BORDER,
                BOLD,
                RESET,
                SECTION,
                BOLD,
                section.section.display_name(),
                RESET
            )?;

            // Measure count (DURATION color)
            if let Some(count) = section.section.measure_count {
                write!(f, " {}[{} measures]{}", DURATION, count, RESET)?;
            }
            writeln!(f)?;

            // Show measures with chords using grid-based display
            if !section.measures.is_empty() {
                let has_chords = section.measures.iter().any(|m| !m.chords.is_empty());
                
                // Check if next section has pushed chords that belong to last measure of this section
                let next_section_first_measure_chords = if section_idx + 1 < self.sections.len() {
                    let next_section = &self.sections[section_idx + 1];
                    let last_measure_abs = cumulative_measures + section.measures.len() as u32 - 1;
                    
                    // Check if next section's first measure has pushed chords
                    if !next_section.measures.is_empty() {
                        let next_first_measure = &next_section.measures[0];
                        let pushed_chords: Vec<_> = next_first_measure.chords.iter()
                            .filter(|chord| {
                                let chord_abs_measures = chord.position.total_duration.measures;
                                // If chord belongs to previous measure (pushed from start of next section), include it
                                // Check that it's actually a push (not pull) and that it's in the previous measure
                                if let Some((is_push, _)) = chord.push_pull {
                                    is_push && chord_abs_measures == last_measure_abs
                                } else {
                                    false
                                }
                            })
                            .collect();
                        if !pushed_chords.is_empty() {
                            Some(pushed_chords)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };

                if has_chords {
                    Self::display_measures_grid_with_next_section(
                        f, 
                        &section.measures, 
                        cumulative_measures,
                        next_section_first_measure_chords.as_deref(),
                        BORDER, 
                        BOLD, 
                        RESET, 
                        CHORD, 
                        DIM
                    )?;
                }
            }
            
            // Update cumulative measure count for next section
            cumulative_measures += section.measures.len() as u32;
        }

        // Bottom border
        let bottom_border = "═".repeat(border_width - 2);
        writeln!(
            f,
            "{}{}╚{}╝{}",
            BORDER, BOLD, bottom_border, RESET
        )?;

        Ok(())
    }
}
