use crate::chart::Chart;
use crate::primitives::Note;

/// Represents a group of measures with optional repeat annotation
#[derive(Debug)]
struct MeasureGroup {
    start_idx: usize,
    count: usize,         // Repeat count (1 = no repeat)
    pattern_length: usize, // Number of measures in the pattern before repeat
}

// ANSI color codes for beautiful terminal output
mod colors {
    pub const RESET: &str = "\x1b[0m";
    pub const BOLD: &str = "\x1b[1m";
    pub const DIM: &str = "\x1b[2m";
    
    // Semantic colors for different information types
    pub const METADATA: &str = "\x1b[96m";      // Bright Cyan - titles, artists
    pub const SECTION: &str = "\x1b[95m";       // Bright Magenta - section names
    pub const DURATION: &str = "\x1b[93m";      // Bright Yellow - measure counts, positions
    pub const CHORD: &str = "\x1b[92m";         // Bright Green - chord symbols
    pub const KEY_SIG: &str = "\x1b[94m";       // Bright Blue - keys and key changes
    pub const TIME_SIG: &str = "\x1b[91m";      // Bright Red - time signatures
    pub const TEMPO: &str = "\x1b[33m";         // Yellow - tempo markings
    pub const MEASURE_NUM: &str = "\x1b[90m";   // Dark Gray - measure numbers
    pub const BORDER: &str = "\x1b[37m";        // White - borders and dividers
}

impl Chart {
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

impl std::fmt::Display for Chart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colors::*;
        
        // Top border
        writeln!(f, "{}{}╔═══════════════════════════════════════════════════════════╗{}", BORDER, BOLD, RESET)?;
        
        // Title and artist (METADATA color)
        if let Some(ref title) = self.metadata.title {
            write!(f, "{}{}║{} {}{}{}", BORDER, BOLD, RESET, METADATA, BOLD, title)?;
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
                write!(f, "{}{}{}/{}{}  ", TIME_SIG, BOLD, ts.numerator, ts.denominator, RESET)?;
            }
            if let Some(ref key) = self.initial_key {
                write!(f, "{}Key: {}{}", KEY_SIG, key.root().name(), RESET)?;
            }
            writeln!(f)?;
        }
        
        // Middle border
        writeln!(f, "{}{}╠═══════════════════════════════════════════════════════════╣{}", BORDER, BOLD, RESET)?;
        
        // Sections
        for (section_idx, section) in self.sections.iter().enumerate() {
            if section_idx > 0 {
                writeln!(f, "{}{}║{}   {}───────────────────────────────────────────────────────────{}", BORDER, BOLD, RESET, DIM, RESET)?;
            }
            
            // Section header (SECTION color)
            write!(f, "{}{}║{} {}{}{}{}", BORDER, BOLD, RESET, SECTION, BOLD, section.section.display_name(), RESET)?;
            
            // Measure count (DURATION color)
            if let Some(count) = section.section.measure_count {
                write!(f, " {}[{} measures]{}", DURATION, count, RESET)?;
            }
            writeln!(f)?;
            
            // Show measures with chords
            if !section.measures.is_empty() {
                let has_chords = section.measures.iter().any(|m| !m.chords.is_empty());
                
                if has_chords {
                    // Detect repeating patterns to show them compactly
                    let measure_groups = Self::group_repeating_measures(&section.measures);
                    
                    write!(f, "{}{}║{} ", BORDER, BOLD, RESET)?;
                    
                    for group in measure_groups {
                        // Show all measures in the pattern
                        for measure_idx in 0..group.pattern_length {
                            let measure = &section.measures[group.start_idx + measure_idx];
                            
                            // Show text cues for this measure inline before chords
                            if !measure.text_cues.is_empty() {
                                // Get the position from the first chord in this measure
                                let position_str = if let Some(first_chord) = measure.chords.first() {
                                    format!("{}@{}.{}.{}{}",
                                        DIM,
                                        first_chord.position.total_duration.measures,
                                        first_chord.position.total_duration.beats,
                                        first_chord.position.total_duration.subdivisions,
                                        RESET
                                    )
                                } else {
                                    String::new()
                                };
                                
                                for cue in &measure.text_cues {
                                    write!(f, "{}@{} \"{}\"{}{} ",
                                        "\x1b[38;5;208m", // Orange color for cue markers
                                        cue.group,
                                        cue.text,
                                        RESET,
                                        position_str
                                    )?;
                                }
                            }
                            
                            if !measure.chords.is_empty() {
                                // Chords in the measure (CHORD color with rhythm info)
                                for (j, chord) in measure.chords.iter().enumerate() {
                                    if j > 0 {
                                        write!(f, " ")?;
                                    }
                                    write!(f, "{}{}{}", CHORD, BOLD, chord.full_symbol)?;
                                    
                                    // Show bass note for slash chords
                                    if let Some(ref bass) = chord.parsed.bass {
                                        write!(f, "/{}", bass)?;
                                    }
                                    
                                    // Show rhythm notation
                                    use crate::chord::ChordRhythm;
                                    match &chord.rhythm {
                                        ChordRhythm::Slashes(count) => {
                                            write!(f, "{}{}", "/".repeat(*count as usize), RESET)?;
                                        }
                                        ChordRhythm::Lily { duration, dotted, .. } => {
                                            use crate::chord::LilySyntax;
                                            let dur_str = match duration {
                                                LilySyntax::Whole => "1",
                                                LilySyntax::Half => "2",
                                                LilySyntax::Quarter => "4",
                                                LilySyntax::Eighth => "8",
                                                LilySyntax::Sixteenth => "16",
                                                LilySyntax::ThirtySecond => "32",
                                            };
                                            write!(f, "_{}{}{}", dur_str, if *dotted { "." } else { "" }, RESET)?;
                                        }
                                        ChordRhythm::Default => {
                                            write!(f, "{}", RESET)?;
                                        }
                                        _ => {
                                            write!(f, "{}", RESET)?;
                                        }
                                    }
                                    
                                    // Show push/pull with apostrophes (like input syntax)
                                    if let Some((is_push, amount)) = chord.push_pull {
                                        use crate::chord::PushPullAmount;
                                        let count = match amount {
                                            PushPullAmount::Eighth => 1,
                                            PushPullAmount::Sixteenth => 2,
                                            PushPullAmount::ThirtySecond => 3,
                                        };
                                        let apostrophes = "'".repeat(count);
                                        if is_push {
                                            // Push: apostrophes before
                                            write!(f, "{}{}{}", DIM, apostrophes, RESET)?;
                                        } else {
                                            // Pull: apostrophes after
                                            write!(f, "{}{}{}", DIM, apostrophes, RESET)?;
                                        }
                                    }
                                    
                                    // Show the MusicalDuration (duration of this chord)
                                    write!(f, "{}[{}.{}.{}]",
                                        DIM,
                                        chord.duration.measures,
                                        chord.duration.beats,
                                        chord.duration.subdivisions
                                    )?;
                                    
                                    // Show absolute position
                                    write!(f, "{}@{}.{}.{}{}",
                                        DIM,
                                        chord.position.total_duration.measures,
                                        chord.position.total_duration.beats,
                                        chord.position.total_duration.subdivisions,
                                        RESET
                                    )?;
                                }
                                write!(f, "  ")?;
                            }
                        }
                        
                        // Show repeat count after all measures in the pattern
                        if group.count > 1 {
                            write!(f, "{}{}x{}{} ", DIM, BOLD, group.count, RESET)?;
                        }
                    }
                    writeln!(f)?;
                }
            }
        }
        
        // Bottom border
        writeln!(f, "{}{}╚═══════════════════════════════════════════════════════════╝{}", BORDER, BOLD, RESET)?;
        
        Ok(())
    }
}
