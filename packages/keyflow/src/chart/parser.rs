//! Chart Parser
//!
//! Main parsing logic for charts
//!
//! Three-phase parsing:
//! 1. Metadata phase: Parse title, tempo, time signature, key
//! 2. Content phase: Parse sections and their measures
//! 3. Post-processing: Auto-number sections, finalize positions

use super::chart::Chart;
use super::cues::TextCue;
use super::types::{ChartSection, ChordInstance, KeyChange, Measure};
use crate::chord::{Chord, ChordRhythm, PushPullAmount};
use crate::key::Key;
use crate::metadata::SongMetadata;
use crate::parsing::Lexer;
use crate::sections::{Section, SectionNumberer, SectionType};
use crate::time::Duration as _; // Import Duration trait for to_beats()
use crate::time::{AbsolutePosition, MusicalDuration, Tempo, TimeSignature};
use crate::RootNotation;

/// Repeat count specification
#[derive(Debug, Clone, Copy, PartialEq)]
enum RepeatCount {
    /// Fixed number of repeats (e.g., x4)
    Fixed(usize),
    /// Auto-calculate based on section length (x^)
    Auto,
}

impl Chart {
    /// Strip comments from a line (everything after ;)
    fn strip_comment(line: &str) -> &str {
        if let Some(pos) = line.find(';') {
            line[..pos].trim()
        } else {
            line
        }
    }

    /// Parse a chart from input string
    pub fn parse(input: &str) -> Result<Self, String> {
        let mut chart = Self::new();
        let lines: Vec<&str> = input
            .lines()
            .map(|l| Self::strip_comment(l.trim()))
            .collect();

        if lines.is_empty() {
            return Err("Empty input".to_string());
        }

        // Phase 1: Parse metadata at the beginning
        let mut line_idx = chart.parse_metadata(&lines, 0)?;

        // Phase 2: Parse sections and content
        line_idx = chart.parse_sections(&lines, line_idx)?;

        // Phase 3: Post-processing
        chart.post_process();

        let _ = line_idx; // Suppress unused warning

        Ok(chart)
    }

    /// Phase 1: Parse metadata
    fn parse_metadata(&mut self, lines: &[&str], start_idx: usize) -> Result<usize, String> {
        let mut idx = start_idx;

        // Skip empty lines and parse settings
        while idx < lines.len() {
            if lines[idx].is_empty() {
                idx += 1;
                continue;
            }

            // Check for settings (lines starting with /)
            if lines[idx].starts_with('/') {
                self.parse_setting(lines[idx])?;
                idx += 1;
                continue;
            }

            break;
        }

        if idx >= lines.len() {
            return Ok(idx);
        }

        // First non-empty line is typically "Title - Artist"
        let (title, artist) = SongMetadata::parse_title_artist(lines[idx]);
        self.metadata.title = title;
        self.metadata.artist = artist;
        idx += 1;

        // Skip empty lines and parse more settings
        while idx < lines.len() {
            if lines[idx].is_empty() {
                idx += 1;
                continue;
            }

            // Check for settings (lines starting with /)
            if lines[idx].starts_with('/') {
                self.parse_setting(lines[idx])?;
                idx += 1;
                continue;
            }

            break;
        }

        // Next line might be "120bpm 4/4 #G" (tempo, time sig, key)
        if idx < lines.len() {
            let line = lines[idx];
            if Self::looks_like_metadata_line(line) {
                self.parse_metadata_line(line)?;
                idx += 1;
            }
        }

        Ok(idx)
    }

    /// Check if a line looks like metadata (contains tempo, time sig, or key)
    fn looks_like_metadata_line(line: &str) -> bool {
        line.contains("bpm") || line.contains('/') || line.contains('#') || line.contains('b')
    }

    /// Parse a metadata line (e.g., "120bpm 4/4 #G")
    fn parse_metadata_line(&mut self, line: &str) -> Result<(), String> {
        let parts: Vec<&str> = line.split_whitespace().collect();

        for part in parts {
            // Try tempo
            if part.ends_with("bpm") || part.parse::<u32>().is_ok() {
                if let Some(tempo) = Tempo::parse(part) {
                    self.tempo = Some(tempo);
                    continue;
                }
            }

            // Try time signature
            if part.contains('/') {
                if let Some((num, den)) = Self::parse_time_signature(part) {
                    self.time_signature = Some(TimeSignature::new(num, den));
                    continue;
                }
            }

            // Try key signature
            if part.starts_with('#') || part.starts_with('b') {
                if let Ok(key) = Key::parse(part) {
                    self.current_key = Some(key.clone());
                    self.initial_key = Some(key);
                    continue;
                }
            }
        }

        Ok(())
    }

    /// Parse time signature (e.g., "4/4", "6/8")
    fn parse_time_signature(s: &str) -> Option<(u8, u8)> {
        let parts: Vec<&str> = s.split('/').collect();
        if parts.len() == 2 {
            let num = parts[0].parse::<u8>().ok()?;
            let den = parts[1].parse::<u8>().ok()?;
            Some((num, den))
        } else {
            None
        }
    }

    /// Parse a setting line (e.g., "/SMART_REPEATS=true")
    fn parse_setting(&mut self, line: &str) -> Result<(), String> {
        self.settings.parse_setting_line(line)
    }

    /// Normalize chord case - capitalize first letter for note names
    /// This allows "cmaj7", "dm7", "g7", "bbmaj7" to be parsed as "Cmaj7", "Dm7", "G7", "Bbmaj7"
    fn normalize_chord_case(token: &str) -> String {
        if token.is_empty() {
            return token.to_string();
        }

        // Check if this is a Roman numeral (starts with I, V, i, or v)
        // Roman numerals should preserve their case
        let first_char = token.chars().next().unwrap();
        if first_char == 'I' || first_char == 'V' || first_char == 'i' || first_char == 'v' {
            // Check if the second character is also a Roman numeral character
            if let Some(second_char) = token.chars().nth(1) {
                if second_char == 'I'
                    || second_char == 'V'
                    || second_char == 'i'
                    || second_char == 'v'
                    || second_char == '/'
                    || second_char == '_'
                    || second_char == '\''
                {
                    // This is a Roman numeral - preserve case
                    return token.to_string();
                }
            } else {
                // Single character I, V, i, or v - likely a Roman numeral
                return token.to_string();
            }
        }

        // If the first character is a lowercase letter (a-g), capitalize it
        if first_char.is_lowercase() && first_char.is_alphabetic() {
            let mut chars = token.chars();
            chars.next(); // skip first
            let mut result = first_char.to_uppercase().to_string();

            // Check if the second character is 'b' or '#' - if so, keep it as is
            // This handles "bbmaj7" -> "Bbmaj7" correctly
            result.push_str(chars.as_str());
            result
        } else {
            token.to_string()
        }
    }

    /// Extract leading apostrophes for push notation
    /// Examples: "'C" -> (1, "C")
    ///           "''Em" -> (2, "Em")
    ///           "C" -> (0, "C")
    fn extract_leading_apostrophes(token: &str) -> (usize, &str) {
        let mut count = 0;
        let mut chars = token.chars();

        while let Some(ch) = chars.next() {
            if ch == '\'' {
                count += 1;
            } else {
                // Found non-apostrophe, return count and rest of string
                return (count, &token[count..]);
            }
        }

        // All apostrophes (shouldn't happen in practice)
        (count, "")
    }

    /// Extract trailing apostrophes for pull notation
    /// Examples: "C'" -> ("C", 1)
    ///           "Em''" -> ("Em", 2)
    ///           "D'//  -> ("D//", 1) - stops at rhythm notation
    ///           "C" -> ("C", 0)
    fn extract_trailing_apostrophes(token: &str) -> (String, usize) {
        // First, find where the chord part ends and rhythm notation begins
        // Rhythm notation: /, _, or more apostrophes
        let rhythm_start = token
            .find(|c: char| c == '/' || c == '_')
            .unwrap_or(token.len());

        // Only extract apostrophes between chord and rhythm
        let chord_and_apostrophes = &token[..rhythm_start];
        let rhythm_part = &token[rhythm_start..];

        let mut count = 0;
        // Count trailing apostrophes from the chord part only
        for ch in chord_and_apostrophes.chars().rev() {
            if ch == '\'' {
                count += 1;
            } else {
                break;
            }
        }

        if count > 0 {
            // Remove apostrophes but keep rhythm
            let chord_only = &chord_and_apostrophes[..chord_and_apostrophes.len() - count];
            let result = format!("{}{}", chord_only, rhythm_part);
            (result, count)
        } else {
            (token.to_string(), 0)
        }
    }

    /// Split a slash chord into chord and bass parts
    /// Examples: "1/3" -> ("1", Some("3"))
    ///           "Cmaj7/E" -> ("Cmaj7", Some("E"))
    ///           "g//" -> ("g//", None)  (rhythm slashes, not a slash chord)
    ///           "g" -> ("g", None)
    fn split_slash_chord(token: &str) -> (&str, Option<&str>) {
        // Find the first slash
        if let Some(slash_pos) = token.find('/') {
            // Check if this is followed by another slash (rhythm notation)
            if slash_pos + 1 < token.len() {
                let after_slash = &token[slash_pos + 1..];
                if after_slash.starts_with('/')
                    || after_slash.starts_with('_')
                    || after_slash.starts_with('\'')
                    || after_slash.is_empty()
                {
                    // This is rhythm notation, not a slash chord
                    return (token, None);
                }

                // Check if what follows the slash looks like a note/degree
                let bass_candidate = after_slash.chars().next().unwrap();
                if bass_candidate.is_alphabetic() || bass_candidate.is_ascii_digit() {
                    // This looks like a slash chord
                    // Extract just the bass note (stop at any rhythm notation)
                    let bass_end = after_slash
                        .find(|c: char| c == '/' || c == '_' || c == '\'')
                        .unwrap_or(after_slash.len());
                    return (&token[..slash_pos], Some(&after_slash[..bass_end]));
                }
            }
        }

        (token, None)
    }

    /// Apply automatic durations to chords between measure separators
    /// If chords are between | separators, split the measure evenly
    /// Examples:
    ///   "| G C |" → "| G_2 C_2 |" (2 chords = half notes each)
    ///   "G C | D" → "G_2 C_2 | D_1" (2 chords before | = half notes, 1 after = whole)
    ///   "G C E D | A" → "G_4 C_4 E_4 D_4 | A_1" (4 chords before | = quarter notes, 1 after = whole)
    fn apply_auto_durations_between_separators(line: &str, beats_per_measure: f64) -> String {
        // Check if line has any separators - if not, return as-is
        if !line.contains('|') {
            return line.to_string();
        }

        // Split by | to get segments
        let segments: Vec<&str> = line.split('|').collect();
        let mut result = String::new();

        for (i, segment) in segments.iter().enumerate() {
            let segment = segment.trim();
            
            // Handle empty segments (multiple | in a row or at start/end)
            if segment.is_empty() {
                // Add separator if not at the very end
                if i < segments.len() - 1 {
                    if !result.is_empty() {
                        result.push(' ');
                    }
                    result.push('|');
                }
                continue;
            }

            // Count chords in this segment (exclude commands, cues, etc.)
            // Count ALL chords, even those with explicit durations, to calculate segment duration
            let tokens: Vec<&str> = segment.split_whitespace().collect();
            let chord_count = tokens.iter().filter(|t| {
                // Count as chord if it's not a command, cue, or other special token
                !t.starts_with('/') && !t.starts_with('@') && !t.starts_with('"')
            }).count();
            
            // Count chords WITHOUT explicit durations (these need auto-duration)
            let chords_needing_duration = tokens.iter().filter(|t| {
                !t.starts_with('/') && !t.starts_with('@') && !t.starts_with('"') && !t.contains('_')
            }).count();

            // Calculate duration per chord
            // If all chords have explicit durations, use whole note as default
            // Otherwise, split the measure evenly among all chords (including those with explicit durations)
            let duration_per_chord = if chord_count > 0 {
                beats_per_measure / chord_count as f64
            } else {
                beats_per_measure
            };
            
            // Only apply auto-duration if there are chords that need it
            let should_apply_auto_duration = chords_needing_duration > 0;

            // Convert beats to LilySyntax duration
            // beats_per_measure = 4 in 4/4, so:
            // 4 beats = Whole (1)
            // 2 beats = Half (2)
            // 1 beat = Quarter (4)
            // 0.5 beats = Eighth (8)
            let lily_duration = if (duration_per_chord - beats_per_measure).abs() < 0.001 {
                "1" // Whole note
            } else if (duration_per_chord - beats_per_measure / 2.0).abs() < 0.001 {
                "2" // Half note
            } else if (duration_per_chord - beats_per_measure / 4.0).abs() < 0.001 {
                "4" // Quarter note
            } else if (duration_per_chord - beats_per_measure / 8.0).abs() < 0.001 {
                "8" // Eighth note
            } else if (duration_per_chord - beats_per_measure / 16.0).abs() < 0.001 {
                "16" // Sixteenth note
            } else {
                // Default to whole note if we can't match exactly
                "1"
            };

            // Rebuild segment with durations
            let mut segment_result = String::new();
            for token in &tokens {
                if !segment_result.is_empty() {
                    segment_result.push(' ');
                }

                // Check if this is a chord (not a command, cue, etc.)
                if !token.starts_with('/') && !token.starts_with('@') && !token.starts_with('"') {
                    // Check if token already has a duration
                    if token.contains('_') {
                        // Already has duration, keep as is
                        segment_result.push_str(token);
                    } else if should_apply_auto_duration {
                        // Add automatic duration
                        segment_result.push_str(token);
                        segment_result.push('_');
                        segment_result.push_str(lily_duration);
                    } else {
                        // No chords need duration, keep as is
                        segment_result.push_str(token);
                    }
                } else {
                    // Keep non-chord tokens as is
                    segment_result.push_str(token);
                }
            }

            if !result.is_empty() && !segment_result.is_empty() {
                result.push(' ');
            }
            result.push_str(&segment_result);

            // Add separator if not last segment
            if i < segments.len() - 1 {
                result.push(' ');
                result.push('|');
            }
        }

        result
    }

    /// Extract repeat syntax from the end of a line
    /// Examples: "6 5 4 4 x4" -> ("6 5 4 4", 4)
    ///           "g c d" -> ("g c d", 1)
    fn extract_repeat_syntax(line: &str) -> (&str, RepeatCount) {
        // Look for pattern like "x4" or "x^" at the end
        let tokens: Vec<&str> = line.split_whitespace().collect();
        if tokens.is_empty() {
            return (line, RepeatCount::Fixed(1));
        }

        let last_token = tokens[tokens.len() - 1];

        // Check if last token matches x^ pattern (auto-repeat)
        if last_token == "x^" || last_token == "X^" {
            // Find where the last token starts in the original line
            if let Some(pos) = line.rfind(last_token) {
                let line_without_repeat = line[..pos].trim();
                return (line_without_repeat, RepeatCount::Auto);
            }
        }

        // Check if last token matches xN pattern (case insensitive)
        if (last_token.starts_with('x') || last_token.starts_with('X')) && last_token.len() > 1 {
            if let Ok(count) = last_token[1..].parse::<usize>() {
                if count > 0 {
                    // Find where the last token starts in the original line
                    if let Some(pos) = line.rfind(last_token) {
                        let line_without_repeat = line[..pos].trim();
                        return (line_without_repeat, RepeatCount::Fixed(count));
                    }
                }
            }
        }

        (line, RepeatCount::Fixed(1))
    }

    /// Extract the root portion from the original token (before quality info and rhythm notation)
    /// Examples: "cmaj7" -> "C", "Dm7" -> "D", "g#m" -> "G#", "I" -> "I", "vi" -> "vi", "1" -> "1"
    /// Also handles rhythm notation: "e//" -> "E", "g_4" -> "G", "1///" -> "1"
    fn extract_root_from_token(token: &str) -> String {
        if token.is_empty() {
            return token.to_string();
        }

        // First, strip any rhythm notation (slashes, underscores, etc.)
        // Find the first occurrence of /, _, or ' and take everything before it
        let chord_part = if let Some(slash_pos) = token.find('/') {
            &token[..slash_pos]
        } else if let Some(underscore_pos) = token.find('_') {
            &token[..underscore_pos]
        } else if let Some(apostrophe_pos) = token.find('\'') {
            &token[..apostrophe_pos]
        } else {
            token
        };

        if chord_part.is_empty() {
            return chord_part.to_string();
        }

        let mut chars = chord_part.chars();
        let first = chars.next().unwrap();

        // If it's a digit (scale degree 1-7), return just the digit
        if first.is_ascii_digit() {
            return first.to_string();
        }

        // If it's a Roman numeral (I-VII), extract the Roman numeral part (preserve case!)
        if first == 'I' || first == 'V' || first == 'i' || first == 'v' {
            let roman_part: String = chord_part
                .chars()
                .take_while(|c| *c == 'I' || *c == 'V' || *c == 'i' || *c == 'v')
                .collect();
            return roman_part;
        }

        // Otherwise, it's a note name - capitalize it and include accidental if present
        let first_upper = first.to_uppercase().to_string();
        let second = chars.next();
        if let Some(c) = second {
            if c == 'b' || c == '#' {
                return format!("{}{}", first_upper, c);
            }
        }

        first_upper
    }

    /// Phase 2: Parse sections and content
    fn parse_sections(&mut self, lines: &[&str], start_idx: usize) -> Result<usize, String> {
        let mut idx = start_idx;

        while idx < lines.len() {
            let line = lines[idx];

            // Skip empty lines
            if line.is_empty() {
                idx += 1;
                continue;
            }

            // Split potential inline content by comma
            let (marker_part, inline_content) = if let Some(comma_idx) = line.find(',') {
                let (marker, content) = line.split_at(comma_idx);
                (marker.trim(), Some(content[1..].trim()))
            } else {
                (line.trim(), None)
            };

            // Check for "pre" or "post" special handling (based on marker part only)
            let line_lower = marker_part.to_lowercase();
            if line_lower == "pre" || line_lower.starts_with("pre ") {
                idx = self.parse_pre_section(lines, idx)?;
                continue;
            }
            if line_lower == "post" || line_lower.starts_with("post ") {
                idx = self.parse_post_section(lines, idx)?;
                continue;
            }

            // Check for subsection prefix (^)
            let (is_subsection, section_marker) = if marker_part.starts_with('^') {
                (true, &marker_part[1..])
            } else {
                (false, marker_part)
            };

            // Check if this is a section marker (based on marker part only)
            if let Some((section_type, measure_count)) = SectionType::parse(section_marker) {
                if let Some(content) = inline_content {
                    // Handle inline content separated by comma
                    let mut section =
                        Section::new(section_type.clone()).with_subsection(is_subsection);
                    if let Some(count) = measure_count {
                        section.measure_count = Some(count);
                    }

                    let measures = if content.is_empty() {
                        // Empty inline content
                        if let Some(count) = measure_count {
                            vec![Measure::new(); count]
                        } else {
                            self.templates
                                .recall_transposed(&section_type, self.current_key.as_ref())
                                .unwrap_or_default()
                        }
                    } else {
                        // Has inline content - clear section memory and parse
                        self.chord_memory.clear_section(&section_type);
                        let parsed_measures =
                            self.parse_section_measures(&[content], &section_type, measure_count)?;

                        // Save as template if not Intro/Outro/Pre/Post
                        if !matches!(
                            section_type,
                            SectionType::Intro
                                | SectionType::Outro
                                | SectionType::Pre(_)
                                | SectionType::Post(_)
                        ) {
                            self.templates.store(
                                &section_type,
                                &parsed_measures,
                                self.current_key.as_ref(),
                            );
                        }

                        parsed_measures
                    };

                    let chart_section = ChartSection::new(section).with_measures(measures);
                    self.sections.push(chart_section);
                    idx += 1;
                } else {
                    // Parse section content from subsequent lines
                    idx = self.parse_section_content(
                        lines,
                        idx,
                        section_type,
                        measure_count,
                        is_subsection,
                    )?;
                }
            } else {
                // Not a recognized line, skip it
                idx += 1;
            }
        }

        Ok(idx)
    }

    /// Parse a section and its content
    fn parse_section_content(
        &mut self,
        lines: &[&str],
        start_idx: usize,
        section_type: SectionType,
        measure_count: Option<usize>,
        is_subsection: bool,
    ) -> Result<usize, String> {
        let mut idx = start_idx + 1; // Skip section marker line
        let mut section = Section::new(section_type.clone()).with_subsection(is_subsection);
        if let Some(count) = measure_count {
            section.measure_count = Some(count);
        }

        // Collect content lines for this section
        let mut content_lines = Vec::new();

        // Peek at next line to see if it's content or another section
        while idx < lines.len() {
            let line = lines[idx];

            // If empty line, check what's after it
            if line.is_empty() {
                idx += 1;
                continue;
            }

            // If this looks like a new section or pre/post, stop collecting content
            let (marker_part, _) = if let Some(comma_idx) = line.find(',') {
                let (marker, content) = line.split_at(comma_idx);
                (marker.trim(), Some(content[1..].trim()))
            } else {
                (line.trim(), None)
            };
            if SectionType::parse(marker_part).is_some() {
                break;
            }

            let line_lower = line.to_lowercase();
            if line_lower == "pre"
                || line_lower.starts_with("pre ")
                || line_lower == "post"
                || line_lower.starts_with("post ")
            {
                break;
            }

            // This is content for the current section
            content_lines.push(line);
            idx += 1;
        }

        // Parse the content lines into measures
        let mut measures = if content_lines.is_empty() {
            // No explicit content - recall from template
            if let Some(count) = measure_count {
                // Has measure count - create empty measures
                vec![Measure::new(); count]
            } else {
                // No measure count - recall template if available
                self.templates
                    .recall_transposed(&section_type, self.current_key.as_ref())
                    .unwrap_or_default()
            }
        } else {
            // Has explicit content - clear section memory for this section type
            // This allows the section to build its own memory from scratch or inherit from global
            self.chord_memory.clear_section(&section_type);

            let parsed_measures = self.parse_section_measures(&content_lines, &section_type, measure_count)?;

            // Save as template if not Intro/Outro/Pre/Post
            if !matches!(
                section_type,
                SectionType::Intro
                    | SectionType::Outro
                    | SectionType::Pre(_)
                    | SectionType::Post(_)
            ) {
                self.templates
                    .store(&section_type, &parsed_measures, self.current_key.as_ref());
            }

            parsed_measures
        };

        // If a measure count was specified, ensure the section is exactly that long
        if let Some(count) = measure_count {
            let actual_measures = measures.len();
            if actual_measures < count {
                // Pad with space (rest) chord instances to reach the specified count
                let padding_needed = count - actual_measures;
                let time_sig = self.time_signature.unwrap_or(TimeSignature::common_time());

                for _ in 0..padding_needed {
                    // Create a space chord that fills one full measure
                    let space_duration = MusicalDuration::new(1, 0, 0); // One full measure
                    let note = crate::primitives::MusicalNote::from_string("C").unwrap();
                    let root_notation = RootNotation::from_note_name(note);
                    let space_chord = ChordInstance::new(
                        root_notation.clone(),
                        "s".to_string(), // Space symbol
                        crate::chord::Chord::new(root_notation, crate::chord::ChordQuality::Major),
                        ChordRhythm::Space {
                            duration: crate::chord::LilySyntax::Whole,
                            dotted: false,
                            multiplier: None,
                        },
                        "s".to_string(),
                        space_duration,
                        AbsolutePosition::at_beginning(), // Will be calculated in post-processing
                    );

                    let mut space_measure = Measure::new();
                    space_measure.time_signature = (time_sig.numerator, time_sig.denominator);
                    space_measure.chords.push(space_chord);
                    measures.push(space_measure);
                }
            } else if actual_measures > count {
                // Warn or truncate if content exceeds specified count
                // For now, we'll allow it but could add a warning
                eprintln!(
                    "Warning: Section {:?} has {} measures but specified {} - using actual content",
                    section_type, actual_measures, count
                );
            }
        }

        // Create chart section
        let chart_section = ChartSection::new(section).with_measures(measures);
        self.sections.push(chart_section);

        Ok(idx)
    }

    /// Parse "pre" section (pre-chorus, pre-verse, etc.)
    fn parse_pre_section(&mut self, lines: &[&str], start_idx: usize) -> Result<usize, String> {
        let line = lines[start_idx];
        let parts: Vec<&str> = line.split_whitespace().collect();

        // Default to Pre-Chorus
        let section_type = SectionType::Pre(Box::new(SectionType::Chorus));
        let measure_count = if parts.len() > 1 {
            parts[1].parse::<usize>().ok()
        } else {
            None
        };

        self.parse_section_content(lines, start_idx, section_type, measure_count, false)
    }

    /// Parse "post" section (post-chorus, post-verse, etc.)
    fn parse_post_section(&mut self, lines: &[&str], start_idx: usize) -> Result<usize, String> {
        let line = lines[start_idx];
        let parts: Vec<&str> = line.split_whitespace().collect();

        // Default to Post-Chorus
        let section_type = SectionType::Post(Box::new(SectionType::Chorus));
        let measure_count = if parts.len() > 1 {
            parts[1].parse::<usize>().ok()
        } else {
            None
        };

        self.parse_section_content(lines, start_idx, section_type, measure_count, false)
    }

    /// Parse section content lines into measures
    fn parse_section_measures(
        &mut self,
        lines: &[&str],
        section_type: &SectionType,
        section_measure_count: Option<usize>,
    ) -> Result<Vec<Measure>, String> {
        let mut measures: Vec<Measure> = Vec::new();
        let mut pending_cues: Vec<TextCue> = Vec::new();

        for line in lines {
            // Check if this is a text cue line
            if line.trim().starts_with('@') {
                // Parse text cue - always keep it pending for the next chord line
                match TextCue::parse(line) {
                    Ok(cue) => {
                        pending_cues.push(cue);
                    }
                    Err(e) => {
                        eprintln!("Warning: Failed to parse text cue '{}': {}", line, e);
                    }
                }
            } else {
                // Parse chords from line
                // Pass section measure_count for x^ calculation
                let mut line_measures = self.parse_chord_line(line, section_type, section_measure_count)?;

                // If we have pending cues, attach them to the first new measure
                if !pending_cues.is_empty() && !line_measures.is_empty() {
                    line_measures[0].text_cues.append(&mut pending_cues);
                }

                measures.extend(line_measures);
            }
        }

        Ok(measures)
    }

    /// Parse a line of chords into measures
    fn parse_chord_line(
        &mut self,
        line: &str,
        section_type: &SectionType,
        section_measure_count: Option<usize>,
    ) -> Result<Vec<Measure>, String> {
        use crate::time::Duration as DurationTrait;

        let mut time_sig = self.time_signature.unwrap_or(TimeSignature::common_time());
        let mut beats_per_measure = time_sig.numerator as f64;

        // Check for repeat syntax at the end of the line (e.g., "6 5 4 4 x4")
        let (line_to_parse, repeat_count) = Self::extract_repeat_syntax(line);

        // Preprocess: Calculate automatic durations for chords between measure separators
        // If chords are between | separators, split the measure evenly
        // e.g., "| G C |" → "G_2 C_2", "G C | D" → "G_2 C_2 D_1"
        let line_with_auto_durations = Self::apply_auto_durations_between_separators(&line_to_parse, beats_per_measure);

        let tokens_str: Vec<&str> = line_with_auto_durations.split_whitespace().collect();
        let mut measures: Vec<Measure> = Vec::new();
        let mut current_measure = Measure::new();
        let mut current_measure_beats = 0.0;
        let mut pending_cue: Option<TextCue> = None;
        let mut just_processed_separator = false; // Track if we just processed a | separator
        let mut measure_was_created_by_separator = false; // Track if current measure was created by |

        use super::commands::Command;

        for token_str in &tokens_str {
            // Check for command (e.g., "/fermata", "/accent")
            // Commands are applied to the PREVIOUS chord
            if token_str.starts_with('/') {
                if let Some(cmd) = Command::parse_slash(token_str) {
                    // Apply command to the last chord in the current measure
                    if let Some(last_chord) = current_measure.chords.last_mut() {
                        last_chord.commands.push(cmd);
                    } else if !measures.is_empty() {
                        // If current measure is empty, apply to last chord of previous measure
                        if let Some(last_measure) = measures.last_mut() {
                            if let Some(last_chord) = last_measure.chords.last_mut() {
                                last_chord.commands.push(cmd);
                            }
                        }
                    }
                    continue;
                }
            }

            // Check for inline text cue (e.g., "@keys", "synth", "here" followed by closing quote logic)
            if token_str.starts_with('@') {
                // Start of an inline cue - collect tokens until we find the closing quote
                // For simplicity, assume the cue is in format: @group "text in quotes"
                // We need to look ahead and collect the full cue string
                // For now, we'll handle this by reconstructing the cue from remaining tokens

                // Find the cue in the original line starting from this position
                if let Some(at_pos) = line_to_parse.find(token_str) {
                    let cue_start = &line_to_parse[at_pos..];
                    // Try to parse the cue
                    if let Some(quote_start) = cue_start.find('"') {
                        if let Some(quote_end) = cue_start[quote_start + 1..].find('"') {
                            let cue_str = &cue_start[..quote_start + 1 + quote_end + 1];
                            if let Ok(cue) = TextCue::parse(cue_str) {
                                pending_cue = Some(cue);
                            }
                        }
                    }
                }
                continue;
            }

            // Skip tokens that are part of the cue text (between quotes)
            if pending_cue.is_some() && (token_str.starts_with('"') || token_str.ends_with('"')) {
                continue;
            }

            // Check for measure separator (|)
            // This forces a measure boundary regardless of beat count
            if *token_str == "|" {
                // Finalize current measure if it has chords, or if it was created by a previous separator
                // (This allows multiple | in a row to create empty measures)
                // But don't push auto-created empty measures (created when a measure fills up)
                if !current_measure.chords.is_empty() || measure_was_created_by_separator {
                    measures.push(current_measure.clone());
                }
                // Always start a new measure after |
                current_measure = Measure::new();
                current_measure.time_signature = (time_sig.numerator, time_sig.denominator);
                current_measure_beats = 0.0;
                just_processed_separator = true; // Mark that we just processed a separator
                measure_was_created_by_separator = true; // Mark that this measure was created by |
                continue;
            }

            // Check for time signature change (e.g., "6/8", "3/4")
            if token_str.contains('/') && !token_str.starts_with('/') {
                if let Some((num, den)) = Self::parse_time_signature(token_str) {
                    // Update the time signature for subsequent measures
                    time_sig = TimeSignature::new(num, den);
                    self.time_signature = Some(time_sig);

                    // If we have a current measure, finalize it before the time sig change
                    if !current_measure.chords.is_empty() {
                        measures.push(current_measure.clone());
                        current_measure = Measure::new();
                        current_measure_beats = 0.0;
                    }

                    // Update time signature for new measures
                    current_measure.time_signature = (num, den);
                    beats_per_measure = num as f64;
                    continue;
                }
            }

            // Check for key change - should be ONLY a key signature (short token like "#G", "bBb")
            // Not a chord like "bbmaj7" or "g7"
            let looks_like_key_sig =
                token_str.len() <= 3 && (token_str.starts_with('#') || token_str.starts_with('b'));

            if looks_like_key_sig {
                if let Ok(new_key) = Key::parse(token_str) {
                    // Track key change
                    let position = AbsolutePosition::at_beginning(); // TODO: Calculate actual position
                    let section_index = 0; // TODO: Track current section index
                    let key_change = KeyChange::new(
                        position,
                        self.current_key.clone(),
                        new_key.clone(),
                        section_index,
                    );
                    self.key_changes.push(key_change);
                    self.current_key = Some(new_key);
                    continue;
                }
            }

            // Parse chord
            match self.parse_chord_token(token_str, section_type, time_sig) {
                Ok(chord) => {
                    let chord_beats = chord.duration.to_beats(time_sig);

                    // If we just processed a separator, we're already in a new measure
                    // Don't create another one automatically
                    if !just_processed_separator {
                        // Check if adding this chord would exceed the measure
                        if current_measure_beats + chord_beats > beats_per_measure + 0.001 {
                            // small epsilon for float comparison
                            // Current measure is full, start a new one
                            if !current_measure.chords.is_empty() {
                                measures.push(current_measure.clone());
                            }
                            current_measure = Measure::new();
                            current_measure.time_signature = (time_sig.numerator, time_sig.denominator);
                            current_measure_beats = 0.0;
                            measure_was_created_by_separator = false; // Auto-created, not by separator
                        }
                    }
                    just_processed_separator = false; // Reset flag after processing chord
                    measure_was_created_by_separator = false; // Reset flag - measure now has content

                    // Add chord to current measure
                    current_measure.chords.push(chord);
                    current_measure_beats += chord_beats;

                    // Attach pending cue to this measure if present
                    if let Some(cue) = pending_cue.take() {
                        current_measure.text_cues.push(cue);
                    }

                    // If we've completed exactly one measure, start a new one
                    // (but only if we didn't just process a separator)
                    // When we just processed a separator, we're already in a fresh measure,
                    // so we don't want to auto-create another one
                    if !just_processed_separator && (current_measure_beats - beats_per_measure).abs() < 0.001 {
                        // small epsilon for float comparison
                        measures.push(current_measure.clone());
                        current_measure = Measure::new();
                        current_measure.time_signature = (time_sig.numerator, time_sig.denominator);
                        current_measure_beats = 0.0;
                        // Auto-created measure, not by separator
                    }
                }
                Err(_e) => {
                    // Skip unparseable tokens silently
                    // (might be a formatting token or invalid chord)
                }
            }
        }

        // Add last measure if it has chords
        // (If we just processed a separator, the empty measure was already pushed)
        if !current_measure.chords.is_empty() {
            measures.push(current_measure);
        }

        // Handle repeat count
        let final_repeat_count = match repeat_count {
            RepeatCount::Fixed(count) => count,
            RepeatCount::Auto => {
                // Calculate auto-repeat based on section length vs phrase length
                // We need to calculate phrase length in BEATS, not measures,
                // because chords with explicit durations (like 6_2) might not fill complete measures
                
                if measures.is_empty() {
                    // Empty phrase, can't calculate
                    return Err("Cannot use x^ with empty phrase".to_string());
                }
                
                // Calculate total beats in the phrase
                let phrase_beats: f64 = measures
                    .iter()
                    .flat_map(|m| &m.chords)
                    .map(|chord| chord.duration.to_beats(time_sig))
                    .sum();
                
                // Get section length in measures
                let section_measures = if let Some(count) = section_measure_count {
                    count
                } else {
                    // If no explicit measure count, we can't calculate auto-repeat
                    // This is a limitation - x^ requires an explicit section length
                    return Err("Cannot use x^ without explicit section measure count (e.g., 'VS 16')".to_string());
                };
                
                // Convert section length to beats
                let section_beats = section_measures as f64 * beats_per_measure;
                
                // Calculate how many times to repeat: section_beats / phrase_beats
                let repeat_count_f = section_beats / phrase_beats;
                
                // Check if it's a whole number (within floating point precision)
                let repeat_count_rounded = repeat_count_f.round();
                if (repeat_count_f - repeat_count_rounded).abs() > 0.001 {
                    return Err(format!(
                        "Cannot use x^: section length ({} measures = {} beats) is not evenly divisible by phrase length ({} beats). Would need {} repeats.",
                        section_measures, section_beats, phrase_beats, repeat_count_f
                    ));
                }
                
                let repeat_count_int = repeat_count_rounded as usize;
                if repeat_count_int == 0 {
                    return Err(format!(
                        "Cannot use x^: phrase length ({} beats) is longer than section length ({} measures = {} beats)",
                        phrase_beats, section_measures, section_beats
                    ));
                }
                
                repeat_count_int
            }
        };

        // If repeat count specified, store it on the last measure of the pattern
        // and duplicate the measures
        if final_repeat_count > 1 && !measures.is_empty() {
            let pattern_length = measures.len();

            // Store repeat count on the last measure of the pattern for display purposes
            // Do this BEFORE cloning so it's preserved in the original
            measures[pattern_length - 1].repeat_count = final_repeat_count;

            // Duplicate all measures for the actual playback/structure
            let original_measures = measures.clone();
            for _ in 1..final_repeat_count {
                let mut repeated = original_measures.clone();
                // Clear repeat_count on duplicates so only first occurrence shows it
                for measure in &mut repeated {
                    measure.repeat_count = 1;
                }
                measures.extend(repeated);
            }
        }

        Ok(measures)
    }

    /// Parse a single chord token
    fn parse_chord_token(
        &mut self,
        token: &str,
        section_type: &SectionType,
        time_sig: TimeSignature,
    ) -> Result<ChordInstance, String> {
        use super::commands::Command;

        // Check for one-time override (prefix !)
        let (is_override, token_clean) = if token.starts_with('!') {
            (true, &token[1..])
        } else {
            (false, token)
        };

        // Check for accent shorthand (->)
        let (has_accent, token_no_accent) = if token_clean.contains("->") {
            (true, token_clean.replace("->", ""))
        } else {
            (false, token_clean.to_string())
        };
        let token_clean = token_no_accent.as_str();

        // Check for push notation (leading apostrophes: 'C, ''Em, etc.)
        let (push_count, token_after_push) = Self::extract_leading_apostrophes(token_clean);

        // Check for pull notation (trailing apostrophes: C', Em'', etc.)
        let (token_after_pull, pull_count) = Self::extract_trailing_apostrophes(token_after_push);

        // Check for slash chord (e.g., "1/3", "Cmaj7/E", "g/b")
        // But NOT rhythm slashes (e.g., "g//", "C///")
        let (chord_part, bass_part) = Self::split_slash_chord(&token_after_pull);

        // Normalize case for chord parsing - capitalize first letter if it's a note name
        // This allows "cmaj7" to be parsed as "Cmaj7"
        let normalized_token = Self::normalize_chord_case(chord_part);

        // Parse the chord using the Chord parser
        let mut lexer = Lexer::new(normalized_token.clone());
        let tokens = lexer.tokenize();

        let mut chord = Chord::parse(&tokens)
            .map_err(|e| format!("Failed to parse chord '{}': {:?}", chord_part, e))?;

        // Add bass note if this is a slash chord
        if let Some(bass_str) = bass_part {
            let bass_normalized = Self::normalize_chord_case(bass_str);
            let bass_notation = RootNotation::from_string(&bass_normalized)
                .ok_or_else(|| format!("Invalid bass note: {}", bass_str))?;
            chord.bass = Some(bass_notation);
        }

        // Extract the root from the ORIGINAL token (before normalization, but after apostrophes)
        // This preserves the original casing/format (lowercase note, scale degree, Roman numeral)
        // For scale degrees with quality (e.g., "2maj"), we need to extract just the number part
        // But we pass the full chord_part to process_chord so it can detect explicit quality
        let root_from_token = Self::extract_root_from_token(chord_part);

        // Use ChordMemory to process this chord and get the appropriate full symbol
        // Pass chord_part (which includes quality like "2maj") so it can detect explicit quality
        let full_symbol = self.chord_memory.process_chord(
            &root_from_token,
            chord_part, // Use chord_part (after apostrophes stripped) - includes "2maj" not just "2"
            &chord.normalized,
            section_type,
            is_override,
            self.current_key.as_ref(),
        );

        // Get rhythm and duration (push/pull will be applied later)
        let rhythm = chord.duration.clone().unwrap_or(ChordRhythm::Default);
        let duration = rhythm.to_duration(time_sig);

        // Store push/pull counts for later adjustment
        let push_pull_info = if push_count > 0 {
            use crate::chord::PushPullAmount;
            PushPullAmount::from_count(push_count as u8).map(|amt| (true, amt))
        } else if pull_count > 0 {
            use crate::chord::PushPullAmount;
            PushPullAmount::from_count(pull_count as u8).map(|amt| (false, amt))
        } else {
            None
        };

        // Create a RootNotation from the original token to preserve casing
        // (e.g., "vi" stays "vi", not "Vi")
        // Fall back to the parsed chord's root if we can't parse the original
        let root_notation =
            RootNotation::from_string(&root_from_token).unwrap_or_else(|| chord.root.clone());

        // Create chord instance
        let mut instance = ChordInstance::new(
            root_notation,
            full_symbol,
            chord,
            rhythm,
            token.to_string(),
            duration,
            AbsolutePosition::at_beginning(), // Will be calculated in post-processing
        )
        .with_push_pull(push_pull_info);

        // Add accent command if present
        if has_accent {
            instance = instance.add_command(Command::Accent);
        }

        Ok(instance)
    }

    /// Phase 3: Post-processing
    fn post_process(&mut self) {
        // Auto-number sections using batch method for retroactive split letter assignment
        let mut numberer = SectionNumberer::new();
        let mut sections_to_number: Vec<Section> =
            self.sections.iter().map(|cs| cs.section.clone()).collect();

        numberer.number_sections(&mut sections_to_number);

        // Apply the numbering back to the chart sections
        for (i, section) in sections_to_number.iter().enumerate() {
            self.sections[i].section.number = section.number;
            self.sections[i].section.split_letter = section.split_letter;
        }

        // Set ending key
        self.ending_key = self.current_key.clone();

        // Apply push/pull timing adjustments
        self.apply_push_pull_adjustments();

        // Calculate absolute positions for all elements
        self.calculate_absolute_positions();

        // TODO: Handle template recall
    }

    /// Apply push/pull timing adjustments
    /// Push: lengthen current chord, shorten previous chord by same amount
    ///       If no previous chord, insert a space (tacet) before
    /// Pull: lengthen current chord, shorten next chord by same amount
    ///       If no next chord, insert a space after
    /// This keeps the total duration constant
    /// Works across measure boundaries
    fn apply_push_pull_adjustments(&mut self) {
        use crate::chord::{Chord as ChordStruct, ChordRhythm};
        use crate::primitives::RootNotation;

        for section in &mut self.sections {
            // Get time signature from first measure or default (before we borrow mutably)
            let time_sig = if !section.measures.is_empty() {
                TimeSignature::new(
                    section.measures[0].time_signature.0,
                    section.measures[0].time_signature.1,
                )
            } else {
                TimeSignature::new(4, 4)
            };

            // We need to work with indices to insert new chords
            // First pass: identify chords that need space inserted
            let mut insertions: Vec<(usize, usize, ChordInstance)> = Vec::new(); // (measure_idx, chord_idx, space_chord)

            let mut measure_idx = 0;
            for measure in &section.measures {
                let mut chord_idx = 0;
                for chord in &measure.chords {
                    if let Some((is_push, amount)) = chord.push_pull {
                        let _adjustment = Self::push_pull_to_duration(amount);

                        // Check if this is the first chord overall
                        let is_first = measure_idx == 0 && chord_idx == 0;

                        if is_push && is_first {
                            // Need to insert space before
                            // Initial space has 0 duration, will be filled by push adjustment
                            let space_duration = MusicalDuration::new(0, 0, 0);
                            let note = crate::primitives::MusicalNote::from_string("C").unwrap();
                            let root_notation = RootNotation::from_note_name(note);
                            let space_chord = ChordInstance::new(
                                root_notation.clone(),
                                "s".to_string(), // Space symbol
                                ChordStruct::new(root_notation, crate::chord::ChordQuality::Major),
                                ChordRhythm::Space {
                                    duration: crate::chord::LilySyntax::Whole,
                                    dotted: false,
                                    multiplier: None,
                                },
                                "s".to_string(),
                                space_duration,
                                AbsolutePosition::at_beginning(),
                            );
                            insertions.push((measure_idx, chord_idx, space_chord));
                        }
                    }
                    chord_idx += 1;
                }
                measure_idx += 1;
            }

            // Apply insertions (in reverse order to maintain indices)
            for (measure_idx, chord_idx, space_chord) in insertions.into_iter().rev() {
                section.measures[measure_idx]
                    .chords
                    .insert(chord_idx, space_chord);
            }

            // Second pass: flatten all chords and apply duration adjustments
            let mut all_chords: Vec<&mut ChordInstance> = Vec::new();
            for measure in &mut section.measures {
                for chord in &mut measure.chords {
                    all_chords.push(chord);
                }
            }

            // Apply adjustments across all chords
            let mut i = 0;
            while i < all_chords.len() {
                if let Some((is_push, amount)) = all_chords[i].push_pull {
                    let adjustment = Self::push_pull_to_duration(amount);

                    if is_push && i > 0 {
                        // Push: shorten current chord, lengthen previous chord (or space) by same amount
                        // This makes the current chord play earlier by "stealing" time from itself
                        let prev_duration = all_chords[i - 1].duration.to_beats(time_sig);
                        let curr_duration = all_chords[i].duration.to_beats(time_sig);

                        let new_prev = prev_duration + adjustment; // Previous gets longer (space fills in)
                        let new_curr = (curr_duration - adjustment).max(0.0); // Current gets shorter

                        all_chords[i - 1].duration =
                            MusicalDuration::from_beats(new_prev, time_sig);
                        all_chords[i].duration = MusicalDuration::from_beats(new_curr, time_sig);
                    } else if !is_push && i + 1 < all_chords.len() {
                        // Pull: lengthen current chord, shorten next chord
                        let curr_duration = all_chords[i].duration.to_beats(time_sig);
                        let next_duration = all_chords[i + 1].duration.to_beats(time_sig);

                        let new_curr = curr_duration + adjustment;
                        let new_next = (next_duration - adjustment).max(0.0);

                        all_chords[i].duration = MusicalDuration::from_beats(new_curr, time_sig);
                        all_chords[i + 1].duration =
                            MusicalDuration::from_beats(new_next, time_sig);
                    }
                }
                i += 1;
            }
        }
    }

    /// Convert push/pull amount to beat adjustment
    fn push_pull_to_duration(amount: PushPullAmount) -> f64 {
        use crate::chord::PushPullAmount;
        match amount {
            PushPullAmount::Eighth => 0.5,         // eighth note
            PushPullAmount::Sixteenth => 0.25,     // sixteenth note
            PushPullAmount::ThirtySecond => 0.125, // 32nd note
        }
    }

    /// Calculate absolute positions for all elements in the chart
    /// This accumulates durations as we traverse sections and measures
    fn calculate_absolute_positions(&mut self) {
        use crate::time::Duration as DurationTrait;

        // Start at position 0.0.0
        let mut current_position = MusicalDuration::new(0, 0, 0);
        let mut current_time_sig = self.time_signature.unwrap_or(TimeSignature::common_time());

        for (section_idx, section) in self.sections.iter_mut().enumerate() {
            for measure in &mut section.measures {
                // Update time signature if this measure has one
                if measure.time_signature
                    != (current_time_sig.numerator, current_time_sig.denominator)
                {
                    current_time_sig =
                        TimeSignature::new(measure.time_signature.0, measure.time_signature.1);
                }

                // Assign positions to all chords in this measure
                for chord in &mut measure.chords {
                    // Calculate position adjustment for push/pull
                    // Push (leading apostrophe 'C): move earlier (subtract from position)
                    // Pull (trailing apostrophe C'): move later (add to position)
                    let position_adjustment = if let Some((is_push, amount)) = chord.push_pull {
                        let adjustment_beats = Self::push_pull_to_duration(amount);
                        if is_push {
                            // Push (leading apostrophe 'C): move earlier
                            // Subtract adjustment to start before the natural position
                            -adjustment_beats
                        } else {
                            // Pull (trailing apostrophe C'): move later  
                            // Add adjustment to start after the natural position
                            adjustment_beats
                        }
                    } else {
                        0.0
                    };
                    
                    // Set the chord's position, adjusted for push/pull
                    // For push (leading apostrophe), subtract adjustment to start earlier
                    // For pull (trailing apostrophe), add adjustment to start later
                    let base_position_beats = current_position.to_beats(current_time_sig);
                    let adjusted_position_beats = base_position_beats + position_adjustment;
                    let beats_per_measure = current_time_sig.numerator as f64;
                    
                    // Calculate which measure this position is in
                    let base_measure_num = (base_position_beats / beats_per_measure).floor() as i32;
                    let adjusted_measure_num = (adjusted_position_beats / beats_per_measure).floor() as i32;
                    
                    // Calculate the absolute position, handling positions in previous measure
                    let adjusted_position = if adjusted_measure_num < base_measure_num {
                        // Position is in previous measure (pushed)
                        // Calculate position within the previous measure
                        let position_in_prev_measure = adjusted_position_beats - (adjusted_measure_num as f64 * beats_per_measure);
                        // If negative, it means we're before the start of that measure
                        // Adjust to be within that measure
                        let position_in_prev_measure = if position_in_prev_measure < 0.0 {
                            beats_per_measure + position_in_prev_measure
                        } else {
                            position_in_prev_measure
                        };
                        // Create position in previous measure
                        MusicalDuration::from_beats(
                            (adjusted_measure_num.max(0) as f64 * beats_per_measure) + position_in_prev_measure.max(0.0),
                            current_time_sig
                        )
                    } else {
                        MusicalDuration::from_beats(adjusted_position_beats.max(0.0), current_time_sig)
                    };
                    chord.position = AbsolutePosition::new(adjusted_position, section_idx);

                    // Add this chord's duration to the current position (use original position, not adjusted)
                    let chord_duration_beats = chord.duration.to_beats(current_time_sig);
                    let new_position_beats =
                        current_position.to_beats(current_time_sig) + chord_duration_beats;
                    current_position =
                        MusicalDuration::from_beats(new_position_beats, current_time_sig);
                }
            }
        }

        // Update key changes with their actual positions
        // (They should be at the same position as the chord they precede)
        for _key_change in &mut self.key_changes {
            // Find the measure/chord where this key change occurs
            // For now, we'll keep the positions as they were set during parsing
            // In a more sophisticated implementation, we'd look up the exact position
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitives::{MusicalNote, Note};
    use crate::time::Duration;

    #[test]
    fn test_parse_simple_chord_line() {
        let input = r#"
My Song - Artist Name

120bpm 4/4 #C

vs
cmaj7 Dm7 g7 Cmaj7
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Verify metadata
        assert_eq!(chart.metadata.title, Some("My Song".to_string()));
        assert_eq!(chart.metadata.artist, Some("Artist Name".to_string()));
        assert_eq!(chart.tempo.unwrap().bpm, 120);
        assert_eq!(chart.time_signature.unwrap().numerator, 4);
        assert_eq!(chart.time_signature.unwrap().denominator, 4);

        // Verify key
        let c_major = Key::major(MusicalNote::c());
        assert_eq!(chart.initial_key, Some(c_major.clone()));
        assert_eq!(chart.current_key, Some(c_major.clone()));

        // Verify sections
        assert_eq!(chart.sections.len(), 1);
        assert_eq!(chart.sections[0].section.section_type, SectionType::Verse);

        // Verify chords parsed (lowercase and uppercase work)
        let measures = &chart.sections[0].measures;
        assert_eq!(measures.len(), 4); // Each chord creates a measure for now

        // Check first chord (lowercase input)
        assert_eq!(measures[0].chords.len(), 1);
        assert_eq!(measures[0].chords[0].full_symbol, "Cmaj7");
    }

    #[test]
    fn test_parse_with_key_change() {
        let input = r#"
Key Change Test

120bpm 4/4 #C

vs
cmaj7 dm7 #G gmaj7 am7
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Verify initial key
        let c_major = Key::major(MusicalNote::c());
        assert_eq!(chart.initial_key, Some(c_major));

        // Verify ending key is G major
        let g_major = Key::major(MusicalNote::g());
        assert_eq!(chart.ending_key, Some(g_major.clone()));
        assert_eq!(chart.current_key, Some(g_major));

        // Verify key change was recorded
        assert_eq!(chart.key_changes.len(), 1);
        assert_eq!(chart.key_changes[0].to_key.root.name(), "G");

        // Verify chords - should have 4 chords (key change token is skipped)
        let measures = &chart.sections[0].measures;
        assert_eq!(measures.len(), 4);
    }

    #[test]
    fn test_parse_multiple_sections_with_key_change() {
        let input = r#"
Multi Section Test

120bpm 4/4 #C

vs
cmaj7 dm7

ch
Gmaj7 am7

br
#D dmaj7 Em7
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Should have 3 sections
        assert_eq!(chart.sections.len(), 3);

        // Verify section types
        assert_eq!(chart.sections[0].section.section_type, SectionType::Verse);
        assert_eq!(chart.sections[1].section.section_type, SectionType::Chorus);
        assert_eq!(chart.sections[2].section.section_type, SectionType::Bridge);

        // Verify ending key is D major
        let d_major = Key::major(MusicalNote::d());
        assert_eq!(chart.ending_key, Some(d_major));

        // Verify key change was recorded
        assert_eq!(chart.key_changes.len(), 1);
        assert_eq!(chart.key_changes[0].to_key.root.name(), "D");
    }

    #[test]
    fn test_chord_memory_across_sections() {
        let input = r#"
Memory Test

120bpm 4/4 #C

vs
cmaj7 dm7 g7

ch
c d g
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Verify verse has full chord symbols
        let verse_measures = &chart.sections[0].measures;
        assert_eq!(verse_measures.len(), 3);

        // Verify chorus chords recall memory
        let chorus_measures = &chart.sections[1].measures;
        assert_eq!(chorus_measures.len(), 3);

        // The chord memory should have remembered qualities from verse
        // Note: The current implementation stores normalized forms
        // We can verify chords were parsed successfully
        assert!(chorus_measures[0].chords[0].full_symbol.contains("C"));
        assert!(chorus_measures[1].chords[0].full_symbol.contains("D"));
        assert!(chorus_measures[2].chords[0].full_symbol.contains("G"));
    }

    #[test]
    fn test_time_signature_in_metadata() {
        let input = r#"
Time Sig Test

140bpm 6/8 #G

vs
gmaj7
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Verify time signature
        assert_eq!(chart.time_signature.unwrap().numerator, 6);
        assert_eq!(chart.time_signature.unwrap().denominator, 8);

        // Verify key
        let g_major = Key::major(MusicalNote::g());
        assert_eq!(chart.initial_key, Some(g_major));
    }

    #[test]
    fn test_section_numbering() {
        let input = r#"
Numbering Test

120bpm 4/4 #C

vs
cmaj7

ch
gmaj7

vs
dm7

ch
am7
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Should have 4 sections
        assert_eq!(chart.sections.len(), 4);

        // Verify auto-numbering
        assert_eq!(chart.sections[0].section.number, Some(1)); // Verse 1
        assert_eq!(chart.sections[1].section.number, Some(1)); // Chorus 1
        assert_eq!(chart.sections[2].section.number, Some(2)); // Verse 2
        assert_eq!(chart.sections[3].section.number, Some(2)); // Chorus 2
    }

    #[test]
    fn test_empty_section_with_measure_count() {
        let input = r#"
Empty Section Test

120bpm 4/4 #C

vs 4
cmaj7 dm7 em7 fmaj7

ch 4
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Should have 2 sections
        assert_eq!(chart.sections.len(), 2);

        // Verse should have 4 measures with chords
        assert_eq!(chart.sections[0].measures.len(), 4);

        // Chorus should have 4 empty measures (template recall placeholder)
        assert_eq!(chart.sections[1].measures.len(), 4);
        assert_eq!(chart.sections[1].section.measure_count, Some(4));
    }

    #[test]
    fn test_parse_section_with_inline_chords() {
        let input = r#"
Inline Chords Test

120bpm 4/4 #C

vs 4, cmaj7 dm7 g7 cmaj7
"#;
        let chart = Chart::parse(input).expect("Failed to parse chart");

        assert_eq!(chart.sections.len(), 1);
        let section = &chart.sections[0];
        assert_eq!(section.section.section_type, SectionType::Verse);
        assert_eq!(section.section.measure_count, Some(4));
        assert_eq!(section.measures.len(), 4);
        assert_eq!(section.measures[0].chords[0].full_symbol, "Cmaj7");
    }

    #[test]
    fn test_parse_section_with_measure_count_and_chords_on_next_line() {
        let input = r#"
Next Line Chords Test

120bpm 4/4 #C

vs 4
cmaj7 dm7 g7 cmaj7
"#;
        let chart = Chart::parse(input).expect("Failed to parse chart");

        assert_eq!(chart.sections.len(), 1);
        let section = &chart.sections[0];
        assert_eq!(section.section.section_type, SectionType::Verse);
        assert_eq!(section.section.measure_count, Some(4));
        assert_eq!(section.measures.len(), 4);
        assert_eq!(section.measures[0].chords[0].full_symbol, "Cmaj7");
    }

    #[test]
    fn test_inline_empty_section_with_measure_count() {
        let input = r#"
Inline Empty Section Test

120bpm 4/4 #C

vs 4,
ch 8
"#;
        let chart = Chart::parse(input).expect("Failed to parse chart");

        assert_eq!(chart.sections.len(), 2);

        let verse = &chart.sections[0];
        assert_eq!(verse.section.section_type, SectionType::Verse);
        assert_eq!(verse.section.measure_count, Some(4));
        assert_eq!(verse.measures.len(), 4); // 4 empty measures

        let chorus = &chart.sections[1];
        assert_eq!(chorus.section.section_type, SectionType::Chorus);
        assert_eq!(chorus.section.measure_count, Some(8));
        assert_eq!(chorus.measures.len(), 8); // 8 empty measures
    }

    #[test]
    fn test_parse_multiple_key_changes() {
        let input = r#"
Multiple Keys

120bpm 4/4 #C

vs
cmaj7 #G gmaj7 #D dmaj7
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Should have recorded 2 key changes
        assert_eq!(chart.key_changes.len(), 2);

        // Verify key progression: C -> G -> D
        assert_eq!(chart.key_changes[0].to_key.root.name(), "G");
        assert_eq!(chart.key_changes[1].to_key.root.name(), "D");

        // Ending key should be D major
        let d_major = Key::major(MusicalNote::d());
        assert_eq!(chart.ending_key, Some(d_major));
    }

    #[test]
    fn test_parse_with_flat_key() {
        let input = r#"
Flat Key Test

120bpm 4/4 bBb

vs
bbmaj7 cm7 dm7
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Verify Bb major key
        assert_eq!(chart.initial_key.as_ref().unwrap().root.name(), "Bb");

        // Should have 3 chords (lowercase flats work)
        assert_eq!(chart.sections[0].measures.len(), 3);
    }

    #[test]
    fn test_pre_and_post_sections() {
        let input = r#"
Pre/Post Test

120bpm 4/4 #C

vs
cmaj7

pre 2
dm7 g7

ch
Cmaj7

post 2
Dm7 cmaj7
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Should have 4 sections
        assert_eq!(chart.sections.len(), 4);

        // Verify section types
        assert_eq!(chart.sections[0].section.section_type, SectionType::Verse);
        assert!(matches!(
            chart.sections[1].section.section_type,
            SectionType::Pre(_)
        ));
        assert_eq!(chart.sections[2].section.section_type, SectionType::Chorus);
        assert!(matches!(
            chart.sections[3].section.section_type,
            SectionType::Post(_)
        ));
    }

    #[test]
    fn test_chord_parsing_with_duration_calculation() {
        let input = r#"
Duration Test

120bpm 4/4 #C

vs
cmaj7 dm7 EM7 fmaj7
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Each chord should have a duration calculated (mix of cases works)
        let measures = &chart.sections[0].measures;
        for measure in measures {
            for chord in &measure.chords {
                // Duration should be set (even if default)
                assert!(chord.duration.to_beats(chart.time_signature.unwrap()) > 0.0);
            }
        }
    }

    #[test]
    fn test_smart_repeat_syntax() {
        let input = r#"
Smart Repeat Test

120bpm 4/4 #C

VS 16
cmaj7 dm7 x^
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Should have 16 measures total (2-bar phrase repeated 8 times)
        assert_eq!(chart.sections.len(), 1);
        let measures = &chart.sections[0].measures;
        
        // The phrase is 2 bars (cmaj7 = 1 bar, dm7 = 1 bar in 4/4)
        // Section is 16 bars, so we need 8 repeats
        // Total: 2 bars * 8 = 16 bars
        assert_eq!(measures.len(), 16);
        
        // Verify the repeat count is stored on the last measure of the pattern
        // The pattern is 2 measures, so measure[1] (second measure) should have repeat_count = 8
        assert_eq!(measures[1].repeat_count, 8, 
            "Repeat count should be 8 on measure 1, but got {}. All measures: {:?}", 
            measures[1].repeat_count,
            measures.iter().map(|m| m.repeat_count).collect::<Vec<_>>());
    }

    #[test]
    fn test_smart_repeat_syntax_8_bars() {
        let input = r#"
Smart Repeat Test 2

120bpm 4/4 #C

VS 8
cmaj7 dm7 x^
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Should have 8 measures total (2-bar phrase repeated 4 times)
        assert_eq!(chart.sections.len(), 1);
        let measures = &chart.sections[0].measures;
        
        // The phrase is 2 bars (cmaj7 = 1 bar, dm7 = 1 bar)
        // Section is 8 bars, so we need 4 repeats
        // Total: 2 bars * 4 = 8 bars
        assert_eq!(measures.len(), 8);
        
        // Verify the repeat count is stored on the last measure of the pattern
        assert_eq!(measures[1].repeat_count, 4);
    }

    #[test]
    fn test_smart_repeat_syntax_error_no_measure_count() {
        let input = r#"
Smart Repeat Error Test

120bpm 4/4 #C

vs
cmaj7 dm7 x^
"#;

        // Should fail because no explicit measure count is specified
        let result = Chart::parse(input);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.contains("Cannot use x^ without explicit section measure count"));
    }

    #[test]
    fn test_smart_repeat_syntax_error_not_divisible() {
        let input = r#"
Smart Repeat Error Test 2

120bpm 4/4 #C

VS 15
6_2 5 4 5 x^
"#;

        // Should fail because section length (15 measures = 60 beats) is not evenly divisible by phrase length (14 beats)
        let result = Chart::parse(input);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.contains("not evenly divisible") || error.contains("not divisible"));
    }

    #[test]
    fn test_measure_separator() {
        let input = r#"
Measure Separator Test

120bpm 4/4 #C

vs
cmaj7_2 dm7_2 | em7_2 fmaj7_2
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Should have 2 measures (separated by |)
        // Each chord is 2 beats (half note), so 2 chords per measure
        assert_eq!(chart.sections.len(), 1);
        let measures = &chart.sections[0].measures;
        assert_eq!(measures.len(), 2);

        // First measure should have cmaj7 and dm7
        assert_eq!(measures[0].chords.len(), 2);
        assert!(measures[0].chords[0].full_symbol.contains("C"));
        assert!(measures[0].chords[1].full_symbol.contains("D"));

        // Second measure should have em7 and fmaj7
        assert_eq!(measures[1].chords.len(), 2);
        assert!(measures[1].chords[0].full_symbol.contains("E"));
        assert!(measures[1].chords[1].full_symbol.contains("F"));
    }

    #[test]
    fn test_measure_separator_with_partial_measure() {
        let input = r#"
Measure Separator Partial Test

120bpm 4/4 #C

vs
cmaj7_2 | dm7_2 em7_2 | fmaj7_2
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Should have 3 measures (separated by |)
        // Each chord is 2 beats (half note)
        assert_eq!(chart.sections.len(), 1);
        let measures = &chart.sections[0].measures;
        assert_eq!(measures.len(), 3);

        // First measure should have just cmaj7
        assert_eq!(measures[0].chords.len(), 1);
        assert!(measures[0].chords[0].full_symbol.contains("C"));

        // Second measure should have dm7 and em7
        assert_eq!(measures[1].chords.len(), 2);
        assert!(measures[1].chords[0].full_symbol.contains("D"));
        assert!(measures[1].chords[1].full_symbol.contains("E"));

        // Third measure should have just fmaj7
        assert_eq!(measures[2].chords.len(), 1);
        assert!(measures[2].chords[0].full_symbol.contains("F"));
    }

    #[test]
    fn test_measure_separator_multiple_in_row() {
        let input = r#"
Measure Separator Multiple Test

120bpm 4/4 #C

vs
cmaj7_2 | | dm7_2
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Should have 3 measures (cmaj7, empty, dm7)
        // Each chord is 2 beats (half note)
        assert_eq!(chart.sections.len(), 1);
        let measures = &chart.sections[0].measures;
        assert_eq!(measures.len(), 3);

        // First measure should have cmaj7
        assert_eq!(measures[0].chords.len(), 1);
        assert!(measures[0].chords[0].full_symbol.contains("C"));

        // Second measure should be empty (or have no chords)
        assert_eq!(measures[1].chords.len(), 0);

        // Third measure should have dm7
        assert_eq!(measures[2].chords.len(), 1);
        assert!(measures[2].chords[0].full_symbol.contains("D"));
    }

    #[test]
    fn test_auto_duration_two_chords_between_separators() {
        let input = r#"
Auto Duration Test

120bpm 4/4 #C

vs
| G C |
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Should have 1 measure with 2 chords, each 2 beats (half notes)
        assert_eq!(chart.sections.len(), 1);
        let measures = &chart.sections[0].measures;
        assert_eq!(measures.len(), 1);
        assert_eq!(measures[0].chords.len(), 2);

        // Each chord should be 2 beats (half note in 4/4)
        let time_sig = chart.time_signature.unwrap();
        assert!((measures[0].chords[0].duration.to_beats(time_sig) - 2.0).abs() < 0.001);
        assert!((measures[0].chords[1].duration.to_beats(time_sig) - 2.0).abs() < 0.001);
    }

    #[test]
    fn test_auto_duration_four_chords_before_separator() {
        let input = r#"
Auto Duration Test 2

120bpm 4/4 #C

vs
G C E D | A
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // Should have 2 measures
        // First: G C E D (each 1 beat = quarter note)
        // Second: A (4 beats = whole note)
        assert_eq!(chart.sections.len(), 1);
        let measures = &chart.sections[0].measures;
        assert_eq!(measures.len(), 2);

        // First measure should have 4 chords
        assert_eq!(measures[0].chords.len(), 4);
        let time_sig = chart.time_signature.unwrap();
        // Each should be 1 beat (quarter note)
        for chord in &measures[0].chords {
            assert!((chord.duration.to_beats(time_sig) - 1.0).abs() < 0.001);
        }

        // Second measure should have 1 chord (4 beats = whole note)
        assert_eq!(measures[1].chords.len(), 1);
        assert!((measures[1].chords[0].duration.to_beats(time_sig) - 4.0).abs() < 0.001);
    }

    #[test]
    fn test_auto_duration_preserves_explicit_durations() {
        let input = r#"
Auto Duration Test 3

120bpm 4/4 #C

vs
G_2 C | D
"#;

        let chart = Chart::parse(input).expect("Failed to parse chart");

        // G_2 should keep its explicit duration (2 beats)
        // C should get auto duration (2 beats, since it's 1 of 2 chords before |)
        // D should get auto duration (4 beats, since it's 1 chord after |)
        assert_eq!(chart.sections.len(), 1);
        let measures = &chart.sections[0].measures;
        let time_sig = chart.time_signature.unwrap();

        // First measure: G_2 (2 beats) and C (2 beats)
        assert_eq!(measures[0].chords.len(), 2);
        assert!((measures[0].chords[0].duration.to_beats(time_sig) - 2.0).abs() < 0.001); // G_2
        assert!((measures[0].chords[1].duration.to_beats(time_sig) - 2.0).abs() < 0.001); // C

        // Second measure: D (4 beats)
        assert_eq!(measures[1].chords.len(), 1);
        assert!((measures[1].chords[0].duration.to_beats(time_sig) - 4.0).abs() < 0.001); // D
    }
}
