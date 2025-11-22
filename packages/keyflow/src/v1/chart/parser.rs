// Main Parser - Orchestrates all parsing components
//
// This is the main entry point that brings together:
// - Metadata parsing (title, artist, tempo, time signature, key)
// - Section parsing (verse, chorus, etc.)
// - Chord parsing with rhythm notation
// - Melody parsing (Lilypond and scale degree notation)
// - Text cues (@all, @keys, etc.)
//
// Key Features:
// - Chord memory: Remembers chord qualities (e.g., "g" expands to "Gmaj7")
// - Section-specific memory: Each section type can have its own chord memory
// - Section templates: Repeated sections reuse chord progressions
// - One-time overrides: "!g7" uses G7 without affecting memory

use std::collections::HashMap;
use std::fmt;
use crate::sections::{Section, SectionType};
use crate::chord::chord::ChordData;
use crate::key::keys::{Key, ScaleType, ScaleMode};
use crate::metadata::SongMetadata;
use crate::time::{Tempo, TimeSignature, MusicalDuration, MusicalPosition};
use crate::parsing::key::parser::KeyParser;
use crate::parsing::common::{Lexer, Token, TokenType};
use crate::chart::element_parser::{ChartElementParser, ChartElement};
use crate::chart::chord_memory::ChordMemory;
use crate::chart::types::{ChartSection, Measure, ChordInstance, RootNote, KeyChange, Position};
use crate::chart::chart::Chart;
use crate::chart::metadata_parser::MetadataParser;
use colored::*;

// Import the working duration implementation
use crate::rhythm::chord_rhythm::{ChordWithRhythm, ChordRhythm, TimingModifier};

// ============================================================================
// PARSER IMPLEMENTATION
// ============================================================================
// Note: Struct definitions have been moved to chart/types.rs and time/duration.rs

impl Chart {
    /// Parse duration from rest or space notation (e.g., "r4", "s2")
    fn parse_duration_from_rest_space(&self, notation: &str) -> Result<MusicalDuration, String> {
        if notation.len() < 2 {
            return Err("Invalid rest/space notation".to_string());
        }
        
        let duration_str = &notation[1..]; // Skip the 'r' or 's'
        if let Ok(beats) = duration_str.parse::<u32>() {
            // Convert beats to MusicalDuration
            // For now, assume quarter note = 1 beat
            Ok(MusicalDuration::new(0, beats, 0))
        } else {
            Err("Invalid duration in rest/space notation".to_string())
        }
    }
    
    /// Calculate the total duration from the beginning of the chart up to a specific section and position
    fn calculate_total_duration(&self, section_index: usize, position_in_section: MusicalDuration) -> MusicalDuration {
        let mut total_duration = MusicalDuration::new(0, 0, 0);
        let time_signature = self.time_signature.unwrap_or(TimeSignature { numerator: 4, denominator: 4 });
        
        // Add duration of all sections before the target section
        for (i, section) in self.sections.iter().enumerate() {
            if i >= section_index {
                break;
            }
            
            // Calculate section duration as MusicalDuration
            let section_duration = if section.measures.is_empty() {
                // Section with no chords - use explicit length
                MusicalDuration::new(section.section.measures as u32, 0, 0)
        } else {
                // Calculate from actual chord content
                let mut section_total = MusicalDuration::new(0, 0, 0);
                for measure in &section.measures {
                    for chord in &measure.chords {
                        section_total = section_total.add(&chord.duration, (time_signature.numerator, time_signature.denominator));
                    }
                }
                section_total
            };
            total_duration = total_duration.add(&section_duration, (time_signature.numerator, time_signature.denominator));
        }
        
        // Add the position within the target section
        total_duration = total_duration.add(&position_in_section, (time_signature.numerator, time_signature.denominator));
        
        total_duration
    }
    
    pub fn new() -> Self {
        Self {
            metadata: SongMetadata::new(),
            sections: Vec::new(),
            chord_memory: HashMap::new(),
            section_chord_memory: HashMap::new(),
            section_templates: HashMap::new(),
            template_original_keys: HashMap::new(),
            current_key: None,
            initial_key: None,
            ending_key: None,
            key_changes: Vec::new(),
            tempo: None,
            time_signature: None,
            chord_memory_manager: ChordMemory::new(),
        }
    }

    /// Generate a complete Lilypond document from the parsed chart
    pub fn to_lilypond(&self) -> String {
        let mut output = String::new();
        
        // Generate header
        output.push_str(&self.generate_header());
        output.push('\n');
        
        // Generate score
        output.push_str("\\score {\n");
        output.push_str("  <<\n");
        
        // Generate chord names
        output.push_str(&self.generate_chord_names());
        output.push('\n');
        
        // Generate staff with rhythm notation
        output.push_str(&self.generate_staff());
        
        output.push_str("  >>\n");
        output.push_str("  \n");
        output.push_str(&self.generate_layout());
        output.push_str("}\n");
        
        output
    }
    
    
    /// Generate Lilypond header block
    fn generate_header(&self) -> String {
        let mut header = String::from("\\header {\n");
        
        if let Some(ref title) = self.metadata.title {
            header.push_str(&format!("  title = \"{}\"\n", title));
        }
        
        if let Some(ref artist) = self.metadata.artist {
            header.push_str(&format!("  composer = \"{}\"\n", artist));
        }
        
        header.push_str("}\n");
        header
    }
    
    /// Generate chord progression in \\chordmode
    fn generate_chord_names(&self) -> String {
        let mut output = String::from("  \\new ChordNames = \"chordProgression\" {\n");
        output.push_str("    \\set chordChanges = ##t\n");
        output.push_str("    \\set chordNameExceptions = #chordExceptions\n");
        output.push_str("    \\set chordRootNamer = #musejazz-chord-name->markup\n");
        output.push_str("    \\override ChordName.font-size = #3\n");
        output.push_str("    \\override ChordName.font-name = #\"MuseJazz Text\"\n");
        output.push_str("    \\override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #0.9\n");
        output.push_str("    \n");
        output.push_str("    \\chordmode {\n");
        
        // Generate chords for each section
        for section in &self.sections {
            for measure in &section.measures {
                if measure.chords.is_empty() {
                    // Empty measure - use space
                    output.push_str("      s1 |\n");
                } else {
                    // Generate chord with proper spacing for slash notation
                    if let Some(first_chord) = measure.chords.first() {
                        let lily_chord = self.chord_to_lilypond(first_chord);
                        output.push_str(&format!("      {}", lily_chord));
                        
                        // Add spaces for remaining beats (assuming 4/4 time)
                        for _ in 1..4 {
                            output.push_str(" s4");
                        }
                        output.push_str(" |\n");
                    }
                }
            }
        }
        
        output.push_str("    }\n");
        output.push_str("  }\n");
        output
    }
    
    /// Generate staff with rhythm notation and section markers
    fn generate_staff(&self) -> String {
        let mut output = String::from("  \\new Staff \\with {\n");
        output.push_str("    \\consists #key-change-tracker-engraver\n");
        output.push_str("  } {\n");
        output.push_str("    \\clef treble\n");
        output.push_str("    \\set Staff.printKeyCancellation = ##f\n");
        output.push_str("    \\override Score.Script.font-size = #2\n");
        output.push_str("    \\override Score.Script.color = #red\n");
        
        // Set tempo if present
        if let Some(ref tempo) = self.tempo {
            output.push_str(&format!("    {}\n", tempo.to_lilypond(4)));
        }
        
        // Set initial time signature
        if let Some(ref time_sig) = self.time_signature {
            output.push_str(&format!("    \\time {}/{}\n", time_sig.numerator, time_sig.denominator));
        } else {
            output.push_str("    \\time 4/4\n");
        }
        
        // Set initial key
        if let Some(ref key) = self.current_key {
            output.push_str(&format!("    {}\n", self.key_to_lilypond(key)));
        } else {
            output.push_str("    \\key c \\major\n");
        }
        
        output.push_str("    \n");
        output.push_str("    \\autoInlineBreaks {\n");
        output.push_str("        \\rh{\n");
        output.push_str("\n");
        
        // Generate section markers and rhythm notation
        for (_section_idx, section) in self.sections.iter().enumerate() {
            let section_name = section.section.generate_chart_name();
            output.push_str(&format!("      \\mark \"{}\"\n", section_name));
            
            // Generate measures with text cues
            for (_measure_idx, measure) in section.measures.iter().enumerate() {
                // Add text cues for this measure
                for cue in &measure.text_cues {
                    // Simplified text cue handling for now
                    output.push_str(&format!("      % {}\n", cue));
                }
                
                // Add key change if present in this measure
                if let Some(ref key) = measure.key {
                    output.push_str(&format!("      {}\n", self.key_to_lilypond(key)));
                }
                
                // Generate rhythm content for this measure
                if !measure.chords.is_empty() {
                    // For slash notation, each measure should be a whole note space
                    output.push_str("      s1\n");
                } else {
                    // Empty measure - use space
                    output.push_str("      s1\n");
                }
            }
        }
        
        output.push_str("        }\n");
        output.push_str("    }\n");
        output.push_str("  }\n");
        output
    }
    
    /// Generate layout block
    fn generate_layout(&self) -> String {
        let mut output = String::from("  \\layout {\n");
        output.push_str("    \\context {\n");
        output.push_str("      \\Score\n");
        output.push_str("      \\ekmSmuflOn #'all\n");
        output.push_str("    }\n");
        output.push_str("    indent = 0\n");
        output.push_str("    ragged-right = ##f\n");
        output.push_str("  }\n");
        output
    }
    
    /// Convert a ChordInstance to Lilypond chord notation
    fn chord_to_lilypond(&self, chord: &ChordInstance) -> String {
        // Convert root to lilypond notation (lowercase with accidentals)
        let root = self.note_to_lilypond_root(&chord.root.note_name);
        
        // For now, use the full_symbol as the quality
        // TODO: Implement proper quality conversion from ChordData
        let quality = if chord.full_symbol.len() > chord.root.note_name.len() {
            &chord.full_symbol[chord.root.note_name.len()..]
        } else {
            ""
        };
        
        // Default duration is quarter note for slash notation
        format!("{}4:{}", root, quality)
    }
    
    /// Convert note name to Lilypond root notation (e.g., "C#" -> "cs", "Bb" -> "bf")
    fn note_to_lilypond_root(&self, note: &str) -> String {
        let mut chars = note.chars();
        let base = chars.next().unwrap().to_lowercase().to_string();
        
        // Handle accidentals
        let accidental = chars.next();
        match accidental {
            Some('#') => format!("{}s", base),
            Some('b') => format!("{}f", base),
            _ => base,
        }
    }
    
    /// Convert a Key to Lilypond key notation
    fn key_to_lilypond(&self, key: &Key) -> String {
        // Convert root note to lilypond notation
        let root = self.note_to_lilypond_root(&key.root.to_string());
        
        // Convert scale type to lilypond mode
        let mode = match key.scale_type {
            ScaleType::Diatonic => match key.mode {
                ScaleMode::Ionian => "\\major",
                ScaleMode::Aeolian => "\\minor",
                _ => "\\major", // Default to major for other modes
            },
            _ => "\\major", // Default to major for other scale types
        };
        
        format!("\\key {} {}", root, mode)
    }
    
    /// Main parsing entry point
    pub fn parse(input: &str) -> Result<Self, String> {
        let mut chart = Self::new();
        
        let lines: Vec<&str> = input.lines().map(|l| l.trim()).collect();
        let mut i = 0;
        
        // Phase 1: Parse metadata at the beginning
        while i < lines.len() {
            let line = lines[i];
            
            if line.is_empty() {
                i += 1;
                continue;
            }
            
            // Try to parse metadata
            if chart.parse_metadata_line(line) {
                i += 1;
                continue;
            }
            
            // If we hit content (section or chords), stop looking for metadata
            if Self::is_section_marker(line) || Self::looks_like_content(line) {
                break;
            }
            
            i += 1;
        }
        
        // Phase 2: Parse sections and their content
        while i < lines.len() {
            let line = lines[i];
            
            if line.is_empty() {
                i += 1;
                continue;
            }
            
            // Handle PRE/POST sections (including with numbers like "pre 2")
            let line_lower = line.to_lowercase().trim().to_string();
            if line_lower == "pre" || line_lower == "post" || 
               line_lower.starts_with("pre ") || line_lower.starts_with("post ") {
                i = chart.parse_pre_post_section(&lines, i)?;
                continue;
            }
            
            // Try to parse as a regular section
            if let Some(section) = Self::try_parse_section(line) {
                i = chart.parse_section_content(&lines, i, section)?;
                continue;
            }
            
                    i += 1;
                }
        
        // Phase 3: Auto-number sections
        chart.auto_number_sections();
        
        // Set the ending key (current key at the end of parsing)
        chart.ending_key = chart.current_key.clone();
        
        Ok(chart)
    }
    
    // ============================================================================
    // METADATA PARSING
    // ============================================================================
    
    // Metadata parsing methods moved to metadata_parser.rs trait
    
    // ============================================================================
    // SECTION PARSING
    // ============================================================================
    
    fn is_section_marker(line: &str) -> bool {
        Self::try_parse_section(line).is_some() || 
        line.to_lowercase().trim() == "pre" || 
        line.to_lowercase().trim() == "post" ||
        line.to_lowercase().trim().starts_with("pre ") ||
        line.to_lowercase().trim().starts_with("post ")
    }
    
    fn try_parse_section(line: &str) -> Option<Section> {
        // Remove leading ! if present (one-time override marker)
        let effective_line = line.trim_start_matches('!').trim();
        
        // Try full format: "Intro 4", "Verse 8"
        if let Some(section) = Self::parse_full_section(effective_line) {
            return Some(section);
        }
        
        // Try abbreviated: "vs", "ch", "br"
        if let Some(section) = Self::parse_abbreviated_section(effective_line) {
            return Some(section);
        }
        
        None
    }
    
    fn parse_full_section(line: &str) -> Option<Section> {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 2 {
            return None;
        }
        
        let name_lower = parts[0].to_lowercase();
        let measures = parts[1].parse::<u32>().ok()?;
        
        let section_type = match name_lower.as_str() {
            "intro" => SectionType::Intro,
            "verse" => SectionType::Verse,
            "chorus" => SectionType::Chorus,
            "bridge" => SectionType::Bridge,
            "outro" => SectionType::Outro,
            "instrumental" | "inst" => SectionType::Instrumental,
            _ => return None,
        };
        
        Some(Section::new_no_number(section_type, measures))
    }
    
    fn parse_abbreviated_section(line: &str) -> Option<Section> {
        let main = line.split('(').next()?.trim();
        let parts: Vec<&str> = main.split_whitespace().collect();
        
        if parts.is_empty() {
            return None;
        }
        
        let abbrev = parts[0].to_lowercase();
        let measures = if parts.len() > 1 {
            // Check if the second part is a single number (explicit length)
            if parts.len() == 2 && parts[1].parse::<u32>().is_ok() {
                parts[1].parse::<u32>().unwrap()
        } else {
                // Multiple parts or non-numeric - treat as chord progression (auto-length)
                0
            }
        } else {
            0
        };
        
        let section_type = match abbrev.as_str() {
            "vs" | "verse" => SectionType::Verse,
            "ch" | "chorus" => SectionType::Chorus,
            "br" | "bridge" => SectionType::Bridge,
            "in" | "intro" => SectionType::Intro,
            "out" | "outro" => SectionType::Outro,
            "pre" => {
                // Don't handle "pre" here - let it fall through to parse_pre_post_section
                // which can look ahead to determine the correct target section
                return None;
            }
            s if s.starts_with("inst") => SectionType::Instrumental,
            _ => return None,
        };
        
        Some(Section::new_no_number(section_type, measures))
    }
    
    fn parse_pre_post_section(&mut self, lines: &[&str], mut i: usize) -> Result<usize, String> {
        let line_lower = lines[i].to_lowercase();
        let line_lower = line_lower.trim();
        let is_pre = line_lower == "pre" || line_lower.starts_with("pre ");
        
        // Parse the section line to extract length if specified
        let parts: Vec<&str> = line_lower.split_whitespace().collect();
        let declared_length = if parts.len() > 1 {
            parts[1].parse::<u32>().unwrap_or(4) // Default to 4 if parsing fails
        } else {
            4 // Default length
        };
        
        i += 1;
        
        // Collect content lines
        let mut content_lines = Vec::new();
        while i < lines.len() && !Self::is_section_marker(lines[i]) && !lines[i].is_empty() {
            content_lines.push(lines[i].to_string());
            i += 1;
        }
        
        // Determine target section type
        let target_type = if is_pre {
            // Look ahead
            let mut found_type = None;
            for j in i..lines.len() {
                if !lines[j].trim().is_empty() {
                    if let Some(next_section) = Self::try_parse_section(lines[j]) {
                        found_type = Some(next_section.section_type);
                        break;
                    }
                }
            }
            found_type
        } else {
            // Look back
            self.sections.last().map(|s| s.section.section_type.clone())
        };
        
        if let Some(base_type) = target_type {
            let section_type = if is_pre {
                SectionType::Pre(Box::new(base_type))
            } else {
                SectionType::Post(Box::new(base_type))
            };
            
            let section = Section::new_no_number(section_type.clone(), declared_length);
            let (measures, from_template) = if !content_lines.is_empty() {
                let parsed = self.parse_content_lines(&content_lines, &section_type)?;
                
                // Validate that the chord content duration matches the section length (only if length was explicitly specified)
                if declared_length > 0 {
                    // Calculate the actual duration of the chord content
                    let time_signature = self.time_signature.unwrap_or(TimeSignature { numerator: 4, denominator: 4 });
                    let mut total_beats = 0.0;
                    
                    for measure in &parsed {
                        for chord in &measure.chords {
                            let chord_beats = chord.duration.to_beats((time_signature.numerator, time_signature.denominator));
                            total_beats += chord_beats;
                        }
                    }
                    
                    // Convert total beats to measures
                    let beats_per_measure = time_signature.numerator as f64;
                    let actual_measures = total_beats / beats_per_measure;
                    
                    // Check if the actual duration matches the declared length (with some tolerance for floating point)
                    if (actual_measures - declared_length as f64).abs() > 0.01 {
                        return Err(format!(
                            "Section length mismatch: {} section specified {} measures but chord content spans {:.1} measures. Expected chord content to fit within {} measures.",
                            section_type.full_name(),
                            declared_length,
                            actual_measures,
                            declared_length
                        ));
                    }
                }
                
                self.section_templates.insert(section_type.clone(), parsed.clone());
                // Store the original key when the template was created
                if let Some(current_key) = &self.current_key {
                    self.template_original_keys.insert(section_type.clone(), current_key.clone());
                }
                (parsed, false)  // Explicitly defined content
            } else {
                (self.get_section_template(&section_type, section.measures), true)  // From template
            };
            
            self.sections.push(ChartSection { section, measures, from_template });
        }
        
        Ok(i)
    }
    
    fn parse_section_content(&mut self, lines: &[&str], mut i: usize, mut section: Section) -> Result<usize, String> {
        i += 1;  // Move past section marker
        
        // Check if this is a one-time override (!vs, !ch, etc.)
        let section_line = lines[i - 1];
        let is_override = section_line.trim().starts_with('!');
        let effective_section_line = if is_override {
            &section_line[1..]
        } else {
            section_line
        };
        
        // Parse the section line to see if it has chord content
        let parts: Vec<&str> = effective_section_line.split_whitespace().collect();
        let has_chords_on_line = if parts.len() > 1 {
            // Check if the second part is a single number (explicit length)
            if parts.len() == 2 && parts[1].parse::<u32>().is_ok() {
                false // Explicit length, no chords on this line
            } else {
                // Check if there's a comma after the length (length specification only with comma)
                if parts.len() >= 3 && parts[1].parse::<u32>().is_ok() && parts[2] == "," {
                    true // Explicit length with comma, chords follow
                } else {
                    true // Chord progression on this line (no comma = no length specification)
                }
            }
        } else {
            false // No additional content on this line
        };
        
        
        // Collect content lines
        let mut content_lines = Vec::new();
        
        // If there are chords on the section line, add them
        if has_chords_on_line {
            let chord_content = if parts.len() >= 3 && parts[1].parse::<u32>().is_ok() && parts[2] == "," {
                // Explicit length with comma: vs 8, 1 4 6 5
                parts[3..].join(" ")
            } else {
                // No explicit length (no comma): vs 1 4 6 5
                parts[1..].join(" ")
            };
            content_lines.push(chord_content);
        }
        
        // Collect additional content lines until next section or empty line
        while i < lines.len() && !Self::is_section_marker(lines[i]) && !lines[i].is_empty() {
            content_lines.push(lines[i].to_string());
            i += 1;
        }
        
        
        // Parse content
        let (measures, actual_length, from_template) = if !content_lines.is_empty() {
            // Has content - parse it
            let parsed = self.parse_content_lines(&content_lines, &section.section_type)?;
            let length = parsed.len() as u32;
            
            // Validate that the chord content duration matches the section length (only if length was explicitly specified)
            if section.measures > 0 {
                // Calculate the actual duration of the chord content
                let time_signature = self.time_signature.unwrap_or(TimeSignature { numerator: 4, denominator: 4 });
                let mut total_beats = 0.0;
                
                for measure in &parsed {
                    for chord in &measure.chords {
                        let chord_beats = chord.duration.to_beats((time_signature.numerator, time_signature.denominator));
                        total_beats += chord_beats;
                    }
                }
                
                // Convert total beats to measures
                let beats_per_measure = time_signature.numerator as f64;
                let actual_measures = total_beats / beats_per_measure;
                
                // Check if the actual duration matches the declared length (with some tolerance for floating point)
                if (actual_measures - section.measures as f64).abs() > 0.01 {
                return Err(format!(
                        "Section length mismatch: {} section specified {} measures but chord content spans {:.1} measures. Expected chord content to fit within {} measures.",
                    section.section_type.full_name(),
                    section.measures,
                        actual_measures,
                    section.measures
                ));
                }
            }
            
            // Clear section memory for explicit redefinitions (so they use global memory)
            self.section_chord_memory.remove(&section.section_type);
            
            // Save as template ONLY if not an override and not Intro/Outro/Pre/Post
            if !is_override && !matches!(section.section_type, 
                SectionType::Intro | SectionType::Outro | 
                SectionType::Pre(_) | SectionType::Post(_)) {
                self.section_templates.insert(section.section_type.clone(), parsed.clone());
                // Store the original key when the template was created
                if let Some(current_key) = &self.current_key {
                    self.template_original_keys.insert(section.section_type.clone(), current_key.clone());
                }
            }
            
            (parsed, length, false)  // Explicitly defined content
        } else {
            // No content - recall from template if available, otherwise use default length
            let template = self.get_section_template(&section.section_type, 0);
            let length = template.len() as u32;
            
            
            // If no template exists, use default or explicit measure count
            if length == 0 && section.measures > 0 {
                (vec![Measure::new(); section.measures as usize], section.measures, false)  // Default measures, not from template
            } else if length == 0 {
                (vec![Measure::new(); 4], 4, false)  // Default 4 measures
            } else {
                (template, length, true)  // From template
            }
        };
        
        // Update section length
        section.measures = actual_length;
        self.sections.push(ChartSection { section, measures, from_template });
        
        Ok(i)
    }
    
    // ============================================================================
    // CONTENT PARSING (Chords, Melodies, Text Cues)
    // ============================================================================
    
    fn looks_like_content(line: &str) -> bool {
        !line.is_empty() && 
        (line.chars().any(|c| "ABCDEFGabcdefg".contains(c)) || 
         line.starts_with('@'))
    }
    
    fn parse_content_lines(&mut self, lines: &[String], section_type: &SectionType) -> Result<Vec<Measure>, String> {
        let mut measures: Vec<Measure> = Vec::new();
        let mut pending_cues: Vec<String> = Vec::new();
        
        for line in lines {
            // Check what type of content this is
            if line.starts_with('@') {
                // Text cue line - simplified parsing for now
                let cue = line.to_string();
                if let Some(last_measure) = measures.last_mut() {
                    // If we have measures, add to the last one
                    last_measure.text_cues.push(cue);
                } else {
                    // No measures yet, keep pending for the next measure
                    pending_cues.push(cue);
                }
            } else if line.starts_with("m{") || line.starts_with("melody{") || line.starts_with("\\melody") {
                // Melody line - TODO: implement inline melody parsing
                // For now, just skip
            } else {
                // Assume chord line
                let mut chord_measures = self.parse_chord_line(line, section_type)?;
                
                // If we have pending cues, attach them to the first new measure
                if !pending_cues.is_empty() && !chord_measures.is_empty() {
                    chord_measures[0].text_cues.append(&mut pending_cues);
                }
                
                measures.append(&mut chord_measures);
            }
        }
        
        // Apply duration adjustments for push/pull timing
        let time_signature = self.time_signature.unwrap_or(TimeSignature { numerator: 4, denominator: 4 });
        self.adjust_chord_durations(&mut measures, (time_signature.numerator, time_signature.denominator));
        
        Ok(measures)
    }
    
    /// Builds a complete chord symbol by combining consecutive tokens that form a chord
    fn build_chord_symbol(&self, tokens: &[Token], start_idx: usize) -> String {
        let mut chord_str = String::new();
        let mut i = start_idx;
        
        while i < tokens.len() {
            let token_str = match &tokens[i].token_type {
                TokenType::Note(note) => {
                    // If we already have a chord symbol and encounter a new note, stop
                    if !chord_str.is_empty() && i > start_idx {
                        break;
                    }
                    note
                },
                TokenType::Extension(ext) => ext,
                TokenType::Number(num) => {
                    // Numbers can be extensions (9, 11, 13) or scale degrees (1-7)
                    // If we already have a chord symbol with a note root, this is an extension
                    // If this is the first token, it's a scale degree
                    // If the first token was a scale degree, subsequent numbers are separate chords
                    
                    // Check if we started with a Note token
                    let has_note_root = if !chord_str.is_empty() && start_idx < tokens.len() {
                        matches!(tokens[start_idx].token_type, TokenType::Note(_))
                    } else {
                        false
                    };
                    
                    if !chord_str.is_empty() && i > start_idx && has_note_root {
                        // We have a note root, so this number is an extension
                        num
                    } else if chord_str.is_empty() {
                        // This is the first token, it's a scale degree
                        if let Ok(degree) = num.parse::<u8>() {
                            if degree >= 1 && degree <= 7 {
                                num
                            } else {
                                // Numbers > 7 are not valid scale degrees, stop here
                        break;
                    }
                        } else {
                            // Not a valid number, stop here
                            break;
                        }
                    } else {
                        // We have a chord string but no note root (started with scale degree)
                        // This is a separate chord, stop here
                        break;
                    }
                },
                TokenType::Identifier(id) => {
                    // If we already have a chord symbol and encounter a new identifier, stop
                    // This handles cases like "I IV V" where each Roman numeral should be a separate chord
                    if !chord_str.is_empty() && i > start_idx {
                        break;
                    }
                    id
                },
                // Include chord quality tokens
                TokenType::Maj => "maj",
                TokenType::Minor => "m",
                TokenType::Add => "add",
                TokenType::Sus => "sus",
                TokenType::Aug => "aug",
                TokenType::Dim => "dim",
                TokenType::Alt => "alt",
                TokenType::Omit => "omit",
                // Stop at tokens that would separate chord symbols
                TokenType::Slash | TokenType::Apostrophe | TokenType::Exclamation | TokenType::Eof => break,
                _ => break,
            };
            chord_str.push_str(token_str);
            i += 1;
        }
        
        chord_str
    }
    
    /// Counts how many consecutive tokens form a chord symbol
    fn count_chord_tokens(&self, tokens: &[Token], start_idx: usize) -> usize {
        let mut count = 0;
        let mut i = start_idx;
        
        while i < tokens.len() {
            match &tokens[i].token_type {
                TokenType::Note(_) => {
                    // If we already have tokens and encounter a new note, stop
                    if count > 0 && i > start_idx {
                        break;
                    }
                    count += 1;
                },
                TokenType::Extension(_) => {
                    count += 1;
                },
                TokenType::Number(num) => {
                    // Numbers can be extensions (9, 11, 13) or scale degrees (1-7)
                    // If we already have a chord symbol with a note root, this is an extension
                    // If this is the first token, it's a scale degree
                    // If the first token was a scale degree, subsequent numbers are separate chords
                    
                    // Check if we've already counted a Note token
                    let has_note_root = if count > 0 && start_idx < tokens.len() {
                        matches!(tokens[start_idx].token_type, TokenType::Note(_))
                    } else {
                        false
                    };
                    
                    if count > 0 && i > start_idx && has_note_root {
                        // We have a note root, so this number is an extension
                        count += 1;
                    } else if count == 0 {
                        // This is the first token, it's a scale degree
                        if let Ok(degree) = num.parse::<u8>() {
                            if degree >= 1 && degree <= 7 {
                                count += 1;
                            } else {
                                // Numbers > 7 are not valid scale degrees, stop here
                        break;
                    }
                        } else {
                            // Not a valid number, stop here
                            break;
                        }
                    } else {
                        // We have tokens but no note root (started with scale degree)
                        // This is a separate chord, stop here
                        break;
                    }
                },
                TokenType::Identifier(_) => {
                    // If we already have tokens and encounter a new identifier, stop
                    // This handles cases like "I IV V" where each Roman numeral should be a separate chord
                    if count > 0 && i > start_idx {
                        break;
                    }
                    count += 1;
                },
                // Include chord quality tokens
                TokenType::Maj | TokenType::Minor | TokenType::Add | TokenType::Sus |
                TokenType::Aug | TokenType::Dim | TokenType::Alt | TokenType::Omit => {
                    count += 1;
                },
                // Stop at tokens that would separate chord symbols
                TokenType::Slash | TokenType::Apostrophe | TokenType::Exclamation | TokenType::Eof => break,
                _ => break,
            }
            i += 1;
        }
        
        count
    }
    
    fn parse_chord_line(&mut self, line: &str, section_type: &SectionType) -> Result<Vec<Measure>, String> {
        // Preprocess the line to expand repeated notation (e.g., "1*4" -> "1 1 1 1")
        let expanded_line = self.expand_repeated_notation(line);
        
        // Use the new element parser to parse the line
        let mut element_parser = ChartElementParser::new();
        let elements = element_parser.parse_line(&expanded_line)
            .map_err(|e| format!("Failed to parse chord line: {}", e))?;
        
        // Process the elements and build measures
        let mut measures = Vec::new();
            let mut chords_in_measure: Vec<ChordInstance> = Vec::new();
        let time_signature = self.time_signature.as_ref()
            .map(|ts| (ts.numerator, ts.denominator))
            .unwrap_or((4, 4));
        
        for element in elements {
            match element {
                ChartElement::KeyChange(key) => {
                    // Handle key change
                    let from_key = self.current_key.clone();
                    
                    // Calculate position for key change
                    let mut beat_position = MusicalDuration::new(0, 0, 0);
                    for chord in &chords_in_measure {
                        beat_position = beat_position.add(&chord.duration, time_signature);
                    }
                    
                    let section_index = self.sections.len();
                    let total_duration = self.calculate_total_duration(section_index, beat_position);
                    
                    let position = MusicalPosition {
                        total_duration,
                        section_index,
                    };
                    
                    let key_change = KeyChange {
                        position,
                                from_key: from_key.clone(),
                        to_key: key.clone(),
                    };
                    self.key_changes.push(key_change);
                                
                    // Transpose chord memory
                            if let Some(ref from_key) = from_key {
                                self.transpose_chord_memory(from_key, &key);
                            }
                    
                    self.current_key = Some(key);
                },
                ChartElement::Chord(chord_data) => {
                    // Calculate position for this chord
                    let mut beat_position = MusicalDuration::new(0, 0, 0);
                    for chord in &chords_in_measure {
                        beat_position = beat_position.add(&chord.duration, time_signature);
                    }
                    
                    let section_index = self.sections.len();
                    let total_duration = self.calculate_total_duration(section_index, beat_position);
                    
                    let position = MusicalPosition {
                        total_duration,
                        section_index,
                    };
                    
                    // Convert ChordData to ChordInstance with chord memory
                    let chord_instance = self.chord_data_to_instance(
                        chord_data,
                        String::new(), // TODO: Get original token from mini-parser
                        section_type,
                        position,
                        time_signature,
                    );
                    
                    chords_in_measure.push(chord_instance);
                },
                ChartElement::Rest(notation) => {
                    // Parse rest duration
                    if let Ok(chord_with_rhythm) = ChordWithRhythm::parse(&notation, Some(time_signature)) {
                        let duration_beats = self.calculate_chord_duration(&chord_with_rhythm.rhythm, time_signature);
                        let duration = MusicalDuration::from_beats(duration_beats, time_signature);
                        
                        // Calculate position for this rest
                        let mut beat_position = MusicalDuration::new(0, 0, 0);
                        for chord in &chords_in_measure {
                            beat_position = beat_position.add(&chord.duration, time_signature);
                        }
                        
                        let section_index = self.sections.len();
                        let total_duration = self.calculate_total_duration(section_index, beat_position);
                        
                        let position = MusicalPosition {
                            total_duration,
                            section_index,
                        };
                        
                        let chord_instance = ChordInstance {
                            root: RootNote::new(String::new(), String::new()),
                            full_symbol: String::new(),
                            parsed: ChordData {
                                root: crate::primitives::note::Note::c(),
                                intervals: vec![],
                                bass: None,
                            },
                            rhythm: chord_with_rhythm.rhythm,
                            timing_modifier: chord_with_rhythm.timing_modifier,
                            original_token: notation,
                            duration,
                            position,
                        };
                        chords_in_measure.push(chord_instance);
                    }
                },
                ChartElement::Space(notation) => {
                    // Parse space duration
                    if let Ok(chord_with_rhythm) = ChordWithRhythm::parse(&notation, Some(time_signature)) {
                        let duration_beats = self.calculate_chord_duration(&chord_with_rhythm.rhythm, time_signature);
                        let duration = MusicalDuration::from_beats(duration_beats, time_signature);
                        
                        // Calculate position for this space
                        let mut beat_position = MusicalDuration::new(0, 0, 0);
                        for chord in &chords_in_measure {
                            beat_position = beat_position.add(&chord.duration, time_signature);
                        }
                        
                        let section_index = self.sections.len();
                        let total_duration = self.calculate_total_duration(section_index, beat_position);
                        
                        let position = MusicalPosition {
                            total_duration,
                            section_index,
                        };
                        
                        let chord_instance = ChordInstance {
                            root: RootNote::new(String::new(), String::new()),
                            full_symbol: String::new(),
                            parsed: ChordData {
                                root: crate::primitives::note::Note::c(),
                                intervals: vec![],
                                bass: None,
                            },
                            rhythm: chord_with_rhythm.rhythm,
                            timing_modifier: chord_with_rhythm.timing_modifier,
                            original_token: notation,
                            duration,
                            position,
                        };
                        chords_in_measure.push(chord_instance);
                    }
                },
            }
        }
        
        // Group chords into measures
        if !chords_in_measure.is_empty() {
            measures.push(Measure {
                chords: chords_in_measure,
            });
        }
        
        Ok(measures)
    }
    
    // ============================================================================
    // CHORD MEMORY AND RHYTHM INTEGRATION
    // ============================================================================
    
    /// Helper method to convert ChordData from mini-parser to ChordInstance with memory
    fn chord_data_to_instance(
        &mut self,
        chord_data: ChordData,
        original_token: String,
        section_type: &SectionType,
        position: MusicalPosition,
        time_signature: (u8, u8),
    ) -> ChordInstance {
        // Calculate duration from the chord data (default to 1 measure for now)
        let duration = MusicalDuration::from_beats(time_signature.0 as f64, time_signature);
        
        // Extract root note from chord data
        let root_note_str = chord_data.root.to_string();
        let root = RootNote::new(root_note_str.clone(), original_token.clone());
        
        // Get the full symbol (root + quality)
        let full_symbol = chord_data.to_display_name();
        
        // Update chord memory
        self.chord_memory.insert(root_note_str.clone(), full_symbol.clone());
        self.section_chord_memory
            .entry(section_type.clone())
            .or_insert_with(HashMap::new)
            .insert(root_note_str, full_symbol.clone());
        
        ChordInstance {
            root,
            full_symbol,
            parsed: chord_data,
            rhythm: ChordRhythm::Default,
            timing_modifier: None,
            original_token,
            duration,
            position,
        }
    }
    
    // ============================================================================
    // CHORD MEMORY SYSTEM
    // ============================================================================
    
    /// Apply default chord quality for note names, scale degrees, and Roman numerals
    /// Returns a ChordData with the proper chord structure
    fn apply_default_quality_for_note_name(&self, root: &str) -> Option<crate::chord::chord::ChordData> {
        // Get current key signature
        let key = match self.current_key.as_ref() {
            Some(key) => key,
            None => return None,
        };
        
        // Get the scale name
        let scale_name = match (key.scale_type, key.mode) {
            (crate::key::keys::ScaleType::Diatonic, crate::key::keys::ScaleMode::Ionian) => "Ionian",
            (crate::key::keys::ScaleType::Diatonic, crate::key::keys::ScaleMode::Aeolian) => "Aeolian",
            (crate::key::keys::ScaleType::Diatonic, crate::key::keys::ScaleMode::Dorian) => "Dorian",
            (crate::key::keys::ScaleType::Diatonic, crate::key::keys::ScaleMode::Phrygian) => "Phrygian",
            (crate::key::keys::ScaleType::Diatonic, crate::key::keys::ScaleMode::Lydian) => "Lydian",
            (crate::key::keys::ScaleType::Diatonic, crate::key::keys::ScaleMode::Mixolydian) => "Mixolydian",
            (crate::key::keys::ScaleType::Diatonic, crate::key::keys::ScaleMode::Locrian) => "Locrian",
            (crate::key::keys::ScaleType::MelodicMinor, _) => "Melodic Minor",
            (crate::key::keys::ScaleType::HarmonicMinor, _) => "Harmonic Minor",
            _ => "Ionian", // Default fallback
        };
        
        // Try to analyze as different types of chord symbols
        let analysis = if let Ok(degree) = root.parse::<u8>() {
            // Scale degree (1-7)
            if degree >= 1 && degree <= 7 {
                crate::key::keys::Key::analyze_chord_by_degree(degree, scale_name)
            } else {
                None
            }
        } else if matches!(root.to_uppercase().as_str(), "I" | "II" | "III" | "IV" | "V" | "VI" | "VII") {
            // Roman numeral
            crate::key::keys::Key::analyze_chord_by_roman(root, scale_name)
        } else {
            // Note name - parse and analyze
            if let Some(root_note) = crate::primitives::note::Note::from_string(root) {
                crate::key::keys::Key::analyze_chord_by_note(root_note, key.root, scale_name)
            } else {
                None
            }
        };
        
        if let Some(analysis) = analysis {
            
            // Get the root note for the chord
            let chord_root = if let Ok(degree) = root.parse::<u8>() {
                // For scale degrees, get the actual note from the key
                if let Some(note) = key.get_scale_degree_note(degree as usize) {
                    note
                } else {
                    return None;
                }
            } else if matches!(root.to_uppercase().as_str(), "I" | "II" | "III" | "IV" | "V" | "VI" | "VII") {
                // For Roman numerals, convert to scale degree first
                let degree = match root.to_uppercase().as_str() {
                    "I" => 1, "II" => 2, "III" => 3, "IV" => 4, "V" => 5, "VI" => 6, "VII" => 7,
                    _ => return None,
                };
                if let Some(note) = key.get_scale_degree_note(degree) {
                    note
                } else {
                    return None;
                }
            } else {
                // For note names, parse directly
                if let Some(note) = crate::primitives::note::Note::from_string(root) {
                    note
                } else {
                    return None;
                }
            };
            
            // Create the appropriate chord based on the analysis
            let chord_data = match analysis.chord_quality.as_str() {
                "Major" => {
                    use crate::chord::chord::Chord;
                    Chord::maj().with_root(chord_root).to_data()
                },
                "Minor" => {
                    use crate::chord::chord::Chord;
                    Chord::min().with_root(chord_root).to_data()
                },
                "Dominant7" => {
                    use crate::chord::chord::Chord;
                    Chord::dom7().with_root(chord_root).to_data()
                },
                "Maj7" => {
                    use crate::chord::chord::Chord;
                    Chord::maj7().with_root(chord_root).to_data()
                },
                "Minor7" => {
                    use crate::chord::chord::Chord;
                    Chord::min7().with_root(chord_root).to_data()
                },
                "Minor7b5" => {
                    use crate::chord::chord::Chord;
                    Chord::half_dim7().with_root(chord_root).to_data()
                },
                "Diminished" => {
                    use crate::chord::chord::Chord;
                    Chord::dim().with_root(chord_root).to_data()
                },
                "Augmented" => {
                    use crate::chord::chord::Chord;
                    Chord::aug().with_root(chord_root).to_data()
                },
                _ => {
                    // Default to major if unknown
                    use crate::chord::chord::Chord;
                    Chord::maj().with_root(chord_root).to_data()
                }
            };
            
            Some(chord_data)
        } else {
            None
        }
    }
    
    
    
    fn parse_chord_with_memory(&mut self, token: &str, section_type: &SectionType) -> Result<ChordInstance, String> {
        // Handle rest and space tokens specially
        if token.starts_with('r') || token.starts_with('s') {
            let chord_with_rhythm = ChordWithRhythm::parse(token, Some((4, 4)))?;
            let time_signature = self.time_signature.unwrap_or(TimeSignature { numerator: 4, denominator: 4 });
            let duration_beats = self.calculate_chord_duration(&chord_with_rhythm.rhythm, (time_signature.numerator, time_signature.denominator));
            let duration = MusicalDuration::from_beats(duration_beats, (time_signature.numerator, time_signature.denominator));
            
            return Ok(ChordInstance {
                root: RootNote::new(String::new(), String::new()),
                full_symbol: String::new(),
                parsed: crate::chord::chord::ChordData {
                    root: crate::primitives::note::Note::c(),
                    intervals: vec![],
                    bass: None,
                },
                rhythm: chord_with_rhythm.rhythm,
                timing_modifier: chord_with_rhythm.timing_modifier,
                original_token: token.to_string(),
                duration,
            });
        }
        
        // Check for one-time override (!) before parsing
        let (is_override, effective_token) = if token.starts_with('!') {
            (true, token[1..].to_string())
        } else {
            (false, token.to_string())
        };
        
        // Handle scale degrees and Roman numerals by converting to note names for parsing
        // but preserve the original format in the result
        let (token_to_parse, original_root) = if let Some(key) = &self.current_key {
            // Extract the chord name part (before any duration notation like _4 or ////)
            let chord_name_part = if let Some(underscore_pos) = effective_token.find('_') {
                &effective_token[..underscore_pos]
            } else if let Some(slash_pos) = effective_token.find('/') {
                &effective_token[..slash_pos]
            } else {
                &effective_token
            };
            
            // Check if this is a scale degree (1-7)
            if let Ok(degree) = chord_name_part.parse::<u8>() {
                if degree >= 1 && degree <= 7 {
                    // Convert scale degree to note name for parsing, but keep original format
                    let note_name = self.scale_degree_to_note(degree, key);
                    let converted_token = effective_token.replace(chord_name_part, &note_name);
                    (converted_token, chord_name_part.to_string())
                } else {
                    // Not a valid scale degree (1-7), treat as regular chord
                    (effective_token.clone(), String::new())
                }
            } else {
                // Check if this is a Roman numeral (I, II, III, IV, V, VI, VII)
                let roman_to_degree = match chord_name_part.to_uppercase().as_str() {
                    "I" => Some(1), "II" => Some(2), "III" => Some(3), "IV" => Some(4),
                    "V" => Some(5), "VI" => Some(6), "VII" => Some(7), _ => None,
                };
                
                if let Some(degree) = roman_to_degree {
                    // Convert Roman numeral to note name for parsing, but keep original format
                    let note_name = self.scale_degree_to_note(degree, key);
                    let converted_token = effective_token.replace(chord_name_part, &note_name);
                    (converted_token, chord_name_part.to_string())
                } else {
                    // Not a scale degree or Roman numeral, so original_root should be empty
                    (effective_token.clone(), String::new())
                }
            }
        } else {
            (effective_token.clone(), String::new())
        };
        
        // For simple note names, we don't need ChordWithRhythm::parse since we handle timing separately
        // Only use ChordWithRhythm::parse for complex notation with durations or slashes
        let chord_with_rhythm = if token_to_parse.contains('_') || token_to_parse.contains('/') || 
                                   token_to_parse.starts_with('r') || token_to_parse.starts_with('s') {
            ChordWithRhythm::parse(&token_to_parse, Some((4, 4)))
                .unwrap_or_else(|_| {
                    ChordWithRhythm {
                        chord: None,
                        rhythm: ChordRhythm::Default,
                        timing_modifier: None,
                        original: token_to_parse.to_string(),
                    }
                })
        } else {
            // Simple note name - create default rhythm
            ChordWithRhythm {
                chord: None,
                rhythm: ChordRhythm::Default,
                timing_modifier: None,
                original: token_to_parse.to_string(),
            }
        };
        
        // Extract chord name from the parsed chord
        let chord_name = if let Some(chord) = &chord_with_rhythm.chord {
            // Convert ChordData to a string representation
            chord.to_display_name()
        } else {
            // For rests and spaces, use empty string
            String::new()
        };
        
        // Handle empty chord names (rests and spaces)
        if effective_token.is_empty() {
            // Initialize duration for rests/spaces based on rhythm
            let time_signature = self.time_signature.unwrap_or(TimeSignature { numerator: 4, denominator: 4 });
            let duration_beats = self.calculate_chord_duration(&chord_with_rhythm.rhythm, (time_signature.numerator, time_signature.denominator));
            let duration = MusicalDuration::from_beats(duration_beats, (time_signature.numerator, time_signature.denominator));
            
            return Ok(ChordInstance {
                root: RootNote::new(String::new(), String::new()),
                full_symbol: String::new(),
                parsed: crate::chord::chord::ChordData {
                    root: crate::primitives::note::Note::c(),
                    intervals: vec![],
                    bass: None,
                },
                rhythm: chord_with_rhythm.rhythm,
                timing_modifier: chord_with_rhythm.timing_modifier,
                original_token: token.to_string(),
                duration, // Initialize with calculated duration
            });
        }
        
        // Always preserve the original format in the root field
        let root = if !original_root.is_empty() {
            // For scale degrees and Roman numerals, use the original format
            original_root.clone()
        } else {
            // For regular note names, extract the root from the original effective_token
            // This preserves the original case and format
            Self::extract_root(&effective_token)
        };
        
        // For memory lookup, we need to use the converted note name (from token_to_parse)
        let converted_root = Self::extract_root(&token_to_parse);
        
        // For normalized_token, we need to use the original format but with proper capitalization
        let normalized_token = if !original_root.is_empty() {
            // For scale degrees/Roman numerals, use the original format
            effective_token.clone()
        } else {
            // For regular note names, capitalize the root
        let root_len = if effective_token.len() > 1 && 
            (effective_token.chars().nth(1) == Some('b') || effective_token.chars().nth(1) == Some('#')) {
            2
        } else {
            1
        };
            format!("{}{}", root, &effective_token[root_len..])
        };
        
        // Extract chord name part from normalized_token for memory storage (remove duration notation)
        let memory_token = if let Some(underscore_pos) = normalized_token.find('_') {
            &normalized_token[..underscore_pos]
        } else if let Some(slash_pos) = normalized_token.find('/') {
            &normalized_token[..slash_pos]
        } else {
            &normalized_token
        };
        
        // Determine full symbol using memory
        let full_symbol = if is_override {
            // One-time override: don't touch memory
            normalized_token
        } else if effective_token.len() > root.len() {
            // Has quality - remember in both global and section memory (use converted_root for memory)
            // Store only the chord quality, not the duration notation
            self.chord_memory.insert(converted_root.clone(), memory_token.to_string());
            self.section_chord_memory
                .entry(section_type.clone())
                .or_insert_with(HashMap::new)
                .insert(converted_root.clone(), memory_token.to_string());
            // Use the memory token (without duration) as the full symbol
            memory_token.to_string()
        } else {
            // Just root - lookup in section memory first, then global (use converted_root for memory)
            let section_memory = self.section_chord_memory.get(section_type);
            
            if let Some(mapping) = section_memory.and_then(|m| m.get(&converted_root)) {
                // Found in section memory - use it but preserve duration notation from normalized_token
                if let Some(underscore_pos) = normalized_token.find('_') {
                    format!("{}{}", mapping, &normalized_token[underscore_pos..])
                } else if let Some(slash_pos) = normalized_token.find('/') {
                    format!("{}{}", mapping, &normalized_token[slash_pos..])
                } else {
                mapping.clone()
                }
            } else {
                // Use global and save to section memory
                let global_value = self.chord_memory
                    .get(&converted_root)
                    .cloned()
                    .unwrap_or_else(|| memory_token.to_string());
                
                self.section_chord_memory
                    .entry(section_type.clone())
                    .or_insert_with(HashMap::new)
                    .insert(converted_root.clone(), global_value.clone());
                
                // Apply global value but preserve duration notation from normalized_token
                if let Some(underscore_pos) = normalized_token.find('_') {
                    format!("{}{}", global_value, &normalized_token[underscore_pos..])
                } else if let Some(slash_pos) = normalized_token.find('/') {
                    format!("{}{}", global_value, &normalized_token[slash_pos..])
                } else {
                global_value
                }
            }
        };
        
        // Extract chord name part from full_symbol (remove duration notation)
        let chord_name_part = if let Some(underscore_pos) = full_symbol.find('_') {
            &full_symbol[..underscore_pos]
        } else if let Some(slash_pos) = full_symbol.find('/') {
            &full_symbol[..slash_pos]
        } else {
            &full_symbol
        };
        
        // Check if this is a scale degree, Roman numeral, or note name
        let is_scale_degree = root.parse::<u8>().map(|d| d >= 1 && d <= 7).unwrap_or(false);
        let is_roman_numeral = matches!(root.to_uppercase().as_str(), "I" | "II" | "III" | "IV" | "V" | "VI" | "VII");
        
        let final_symbol = if is_scale_degree || is_roman_numeral {
            // For scale degrees and Roman numerals, apply chord quality directly
            if let Some(chord_data) = self.apply_default_quality_for_note_name(&root) {
                chord_data.to_display_name()
        } else {
                chord_name_part.to_string()
            }
        } else {
            // For note names, convert scale degrees/Roman numerals first, then apply quality
            let converted_symbol = self.convert_scale_degree_or_roman_numeral(chord_name_part);
            
            if converted_symbol.len() == root.len() {
                // Simple note name without quality - apply default based on key
                if let Some(chord_data) = self.apply_default_quality_for_note_name(&root) {
                    chord_data.to_display_name()
                } else {
                    converted_symbol
                }
            } else {
                // Has some quality - check if it's a complete quality or needs expansion
                if self.is_complete_chord_quality(&converted_symbol) {
                    converted_symbol
                } else {
                    // Partial quality (like "Em", "Gmaj", "C7") - apply default quality to complete it
                    if let Some(chord_data) = self.apply_default_quality_for_note_name(&root) {
                        chord_data.to_display_name()
            } else {
                converted_symbol
                    }
                }
            }
        };
        
        // Parse the chord using the keyflow chord parser
        let mut chord_parser = ChordParser::new();
        let parsed = chord_parser.parse(&final_symbol)
            .map_err(|e| format!("Failed to parse chord '{}': {:?}", final_symbol, e))?;
        
        // Initialize default duration based on rhythm
        let time_signature = self.time_signature.unwrap_or(TimeSignature { numerator: 4, denominator: 4 });
        let duration_beats = self.calculate_chord_duration(&chord_with_rhythm.rhythm, (time_signature.numerator, time_signature.denominator));
        let duration = MusicalDuration::from_beats(duration_beats, (time_signature.numerator, time_signature.denominator));
        
        // Create the root note with both converted name and original format
        let root_note = if !original_root.is_empty() {
            // For scale degrees and Roman numerals, use the original format
            RootNote::new(converted_root.clone(), original_root.clone())
        } else {
            // For regular note names, use the extracted root as both note name and original format
            let extracted_root = Self::extract_root(&effective_token);
            RootNote::new(extracted_root.clone(), extracted_root)
        };
        
        Ok(ChordInstance {
            root: root_note,
            full_symbol: final_symbol.clone(),  // Store the final parsed symbol
            parsed,
            rhythm: chord_with_rhythm.rhythm,
            timing_modifier: chord_with_rhythm.timing_modifier,
            original_token: token.to_string(),  // Store the original token
            duration, // Initialize with calculated duration
        })
    }
    
    /// Check if a chord symbol represents a complete quality
    /// Complete qualities include: Em7, Gmaj7, C7, Dmin7, F#maj9, etc.
    /// Partial qualities include: Em, Gmaj, C, Dmin, F#maj, etc.
    fn is_complete_chord_quality(&self, chord_symbol: &str) -> bool {
        if chord_symbol.len() <= 1 {
            return false; // Just a root note
        }
        
        // Check if it ends with a number (7, 9, 11, 13, etc.)
        let last_char = chord_symbol.chars().last().unwrap();
        if last_char.is_ascii_digit() {
            return true;
        }
        
        // Check for common complete quality endings
        let lower = chord_symbol.to_lowercase();
        if lower.ends_with("maj7") || lower.ends_with("min7") || lower.ends_with("dim7") || 
           lower.ends_with("aug7") || lower.ends_with("sus4") || lower.ends_with("sus2") ||
           lower.ends_with("add9") || lower.ends_with("add11") || lower.ends_with("add13") {
            return true;
        }
        
        // Check for slash chords (like G/B)
        if chord_symbol.contains('/') {
            return true;
        }
        
        false
    }
    
    /// Convert scale degrees (1-7) and Roman numerals (I-VII) to actual note names
    fn convert_scale_degree_or_roman_numeral(&self, symbol: &str) -> String {
        if let Some(key) = &self.current_key {
            // Handle scale degrees (1-7)
            if let Ok(degree) = symbol.parse::<u8>() {
                if degree >= 1 && degree <= 7 {
                    return self.scale_degree_to_note(degree, key);
                }
            }
            
            // Handle Roman numerals (I, II, III, IV, V, VI, VII)
            let roman_to_degree = match symbol.to_uppercase().as_str() {
                "I" => Some(1),
                "II" => Some(2),
                "III" => Some(3),
                "IV" => Some(4),
                "V" => Some(5),
                "VI" => Some(6),
                "VII" => Some(7),
                _ => None,
            };
            
            if let Some(degree) = roman_to_degree {
                return self.scale_degree_to_note(degree, key);
            }
        }
        
        // If not a scale degree or Roman numeral, capitalize the first character (note name)
        if symbol.len() > 0 {
            let mut result = String::new();
            result.push(symbol.chars().next().unwrap().to_ascii_uppercase());
            if symbol.len() > 1 {
                result.push_str(&symbol[1..]);
            }
            result
        } else {
        symbol.to_string()
        }
    }
    
    /// Convert a scale degree (1-7) to the actual note name in the given key
    fn scale_degree_to_note(&self, degree: u8, key: &Key) -> String {
        if degree < 1 || degree > 7 {
            panic!("Invalid scale degree: {} (must be 1-7)", degree);
        }
        
        // Use the key's actual scale to get the note
        if let Some(note) = key.get_scale_degree_note(degree as usize) {
            note.to_string()
        } else {
            panic!("Failed to get scale degree {} from key {:?}", degree, key);
        }
    }
    
    
    /// Expand repeated notation (e.g., "1*4" -> "1 1 1 1")
    fn expand_repeated_notation(&self, line: &str) -> String {
        let mut result = String::new();
        let tokens: Vec<&str> = line.split_whitespace().collect();
        
        for (i, token) in tokens.iter().enumerate() {
            if i > 0 {
                result.push(' ');
            }
            
            // Check for repeated notation (e.g., "1*4", "G*2")
            if let Some(star_pos) = token.find('*') {
                let chord_part = &token[..star_pos];
                let count_part = &token[star_pos + 1..];
                
                if let Ok(count) = count_part.parse::<usize>() {
                    // Repeat the chord the specified number of times
                    for j in 0..count {
                        if j > 0 {
                            result.push(' ');
                        }
                        result.push_str(chord_part);
                    }
                } else {
                    // If parsing count fails, use the token as-is
                    result.push_str(token);
                }
            } else {
                // No repetition, use token as-is
                result.push_str(token);
            }
        }
        
        result
    }
    
    fn extract_root(token: &str) -> String {
        let chars: Vec<char> = token.chars().collect();
        if chars.is_empty() {
            return String::new();
        }
        
        let mut root = String::new();
        root.push(chars[0].to_ascii_uppercase());
        
        if chars.len() > 1 && (chars[1] == 'b' || chars[1] == '#') {
            root.push(chars[1]);
        }
        
        root
    }
    
    fn get_section_template(&self, section_type: &SectionType, measure_count: u32) -> Vec<Measure> {
        if let Some(template) = self.section_templates.get(section_type) {
            // Clone the template and transpose chords if key has changed
            let mut measures = template.clone();
            
            // If we have a current key, transpose the template chords to match it
            if let Some(current_key) = &self.current_key {
                // Get the original key when the template was created
                if let Some(original_key) = self.template_original_keys.get(section_type) {
                    // Only transpose if the keys are different
                    if original_key.root != current_key.root || original_key.scale_type != current_key.scale_type {
                        for measure in &mut measures {
                            for chord in &mut measure.chords {
                                // Transpose the chord to the new key
                                if let Ok(transposed_chord) = self.transpose_chord_to_key(chord, original_key, current_key) {
                                    *chord = transposed_chord;
                                }
                            }
                        }
                    }
                }
            }
            
            // Remove key changes that match the current key (don't duplicate key events)
            if let Some(current_key) = &self.current_key {
                for measure in &mut measures {
                    if let Some(ref measure_key) = measure.key {
                        if measure_key.root == current_key.root && measure_key.scale_type == current_key.scale_type {
                            measure.key = None; // Remove duplicate key change
                        }
                    }
                }
            }
            
            // If measure_count is 0, use template as-is (no adjustment)
            if measure_count == 0 {
                return measures;
            }
            
            // Otherwise, adjust to the specified length
            let mut adjusted = measures;
            if adjusted.len() < measure_count as usize {
                while adjusted.len() < measure_count as usize {
                    adjusted.push(Measure::new());
                }
            } else if adjusted.len() > measure_count as usize {
                adjusted.truncate(measure_count as usize);
            }
            
            adjusted
        } else {
            // No template exists - create empty measures
            if measure_count > 0 {
                vec![Measure::new(); measure_count as usize]
            } else {
                Vec::new()
            }
        }
    }
    
    /// Transpose chord memory from one key to another
    fn transpose_chord_memory(&mut self, from_key: &Key, to_key: &Key) {
        // For chord memory transposition, we need to be more careful about what we're transposing
        // If the chord memory contains scale degrees or Roman numerals, we should transpose them
        // If they contain explicit note names with qualities, we should only transpose the root
        
        // Transpose global chord memory
        let mut new_global_memory = HashMap::new();
        for (root, full_symbol) in &self.chord_memory {
            // Check if this is a scale degree or Roman numeral in the original token
            let is_scale_degree = root.parse::<u8>().map(|d| d >= 1 && d <= 7).unwrap_or(false);
            let is_roman_numeral = matches!(root.to_uppercase().as_str(), "I" | "II" | "III" | "IV" | "V" | "VI" | "VII");
            
            if is_scale_degree || is_roman_numeral {
                // This is a scale degree or Roman numeral - transpose it
                let temp_chord = ChordInstance {
                    root: RootNote::new(root.clone(), root.clone()),
                    full_symbol: full_symbol.clone(),
                    parsed: crate::chord::chord::ChordData {
                        root: crate::primitives::note::Note::c(),
                        intervals: vec![],
                        bass: None,
                    },
                    rhythm: ChordRhythm::Default,
                    timing_modifier: None,
                    original_token: root.clone(),
                    duration: MusicalDuration::new(0, 4, 0), // Temporary chord for transposition
                };
                
                if let Ok(transposed_chord) = self.transpose_chord_to_key(&temp_chord, from_key, to_key) {
                    new_global_memory.insert(transposed_chord.root.note_name.clone(), transposed_chord.full_symbol.clone());
                }
            } else {
                // This is an explicit note name with quality - only transpose the root note
                if let Some(root_note) = crate::primitives::note::Note::from_string(root) {
                    // Calculate the semitone difference between the keys
                    let from_root = from_key.root;
                    let to_root = to_key.root;
                    let from_semitone = from_root.to_semitone();
                    let to_semitone = to_root.to_semitone();
                    let semitone_diff = (to_semitone + 12 - from_semitone) % 12;
                    
                    // Transpose the root note by the semitone difference
                    let transposed_note = root_note.transpose_semitones(semitone_diff);
                    let new_root = transposed_note.to_string();
                    
                    // Keep the same quality, just change the root
                    let new_full_symbol = if full_symbol.len() > root.len() {
                        format!("{}{}", new_root, &full_symbol[root.len()..])
                    } else {
                        new_root.clone()
                    };
                    
                    new_global_memory.insert(new_root, new_full_symbol);
                }
            }
        }
        self.chord_memory = new_global_memory;
        
        // Transpose section-specific chord memory
        let mut new_section_memory = HashMap::new();
        for (section_type, section_memory) in &self.section_chord_memory {
            let mut new_section = HashMap::new();
            for (root, full_symbol) in section_memory {
                // Check if this is a scale degree or Roman numeral in the original token
                let is_scale_degree = root.parse::<u8>().map(|d| d >= 1 && d <= 7).unwrap_or(false);
                let is_roman_numeral = matches!(root.to_uppercase().as_str(), "I" | "II" | "III" | "IV" | "V" | "VI" | "VII");
                
                if is_scale_degree || is_roman_numeral {
                    // This is a scale degree or Roman numeral - transpose it
                    let temp_chord = ChordInstance {
                        root: RootNote::new(root.clone(), root.clone()),
                        full_symbol: full_symbol.clone(),
                        parsed: crate::chord::chord::ChordData {
                            root: crate::primitives::note::Note::c(),
                            intervals: vec![],
                            bass: None,
                        },
                        rhythm: ChordRhythm::Default,
                        timing_modifier: None,
                        original_token: root.clone(),
                        duration: MusicalDuration::new(0, 4, 0), // Temporary chord for transposition
                    };
                    
                    if let Ok(transposed_chord) = self.transpose_chord_to_key(&temp_chord, from_key, to_key) {
                        new_section.insert(transposed_chord.root.note_name.clone(), transposed_chord.full_symbol.clone());
                    }
                } else {
                    // This is an explicit note name with quality - only transpose the root note
                    if let Some(root_note) = crate::primitives::note::Note::from_string(root) {
                        // Calculate the semitone difference between the keys
                        let from_root = from_key.root;
                        let to_root = to_key.root;
                        let from_semitone = from_root.to_semitone();
                        let to_semitone = to_root.to_semitone();
                        let semitone_diff = (to_semitone + 12 - from_semitone) % 12;
                        
                        // Transpose the root note by the semitone difference
                        let transposed_note = root_note.transpose_semitones(semitone_diff);
                        let new_root = transposed_note.to_string();
                        
                        // Keep the same quality, just change the root
                        let new_full_symbol = if full_symbol.len() > root.len() {
                            format!("{}{}", new_root, &full_symbol[root.len()..])
                        } else {
                            new_root.clone()
                        };
                        
                        new_section.insert(new_root, new_full_symbol);
                    }
                }
            }
            new_section_memory.insert(section_type.clone(), new_section);
        }
        self.section_chord_memory = new_section_memory;
    }
    
    /// Transpose a chord from one key to another
    fn transpose_chord_to_key(&self, chord: &ChordInstance, from_key: &Key, to_key: &Key) -> Result<ChordInstance, String> {
        // If the chord is a scale degree or Roman numeral, we need to transpose it
        let is_scale_degree = chord.original_token.parse::<u8>().map(|d| d >= 1 && d <= 7).unwrap_or(false);
        let is_roman_numeral = matches!(chord.original_token.to_uppercase().as_str(), "I" | "II" | "III" | "IV" | "V" | "VI" | "VII");
        
        if is_scale_degree || is_roman_numeral {
            // For scale degrees and Roman numerals, we need to find the note in the original key
            // and then find the corresponding scale degree in the new key
            
            let degree = if is_scale_degree {
                chord.original_token.parse::<u8>().unwrap()
            } else {
                // Convert Roman numeral to degree
                match chord.original_token.to_uppercase().as_str() {
                    "I" => 1, "II" => 2, "III" => 3, "IV" => 4,
                    "V" => 5, "VI" => 6, "VII" => 7,
                    _ => return Err("Invalid Roman numeral".to_string()),
                }
            };
            
            // Get the note from the original key
            let original_note = from_key.get_scale_degree_note(degree as usize)
                .ok_or_else(|| "Invalid scale degree".to_string())?;
            
            // Find the corresponding scale degree in the new key
            let new_degree = to_key.degree_of_note(original_note)
                .ok_or_else(|| "Note not found in new key".to_string())?;
            
            // Create a new chord with the transposed scale degree
            let new_token = if is_scale_degree {
                new_degree.to_string()
            } else {
                // Convert degree back to Roman numeral
                match new_degree {
                    1 => "I", 2 => "II", 3 => "III", 4 => "IV",
                    5 => "V", 6 => "VI", 7 => "VII",
                    _ => return Err("Invalid degree".to_string()),
                }.to_string()
            };
            
            // Create a new chord instance with the transposed token
            let mut new_chord = chord.clone();
            new_chord.original_token = new_token.clone();
            
            if is_scale_degree {
                // For scale degree chords, don't set a specific root note
                // The root will be determined by the current key when the chord is played
                new_chord.full_symbol = new_token.clone();
            } else {
                // For Roman numeral chords, we can set the root note
                new_chord.root = RootNote::new(new_token.clone(), new_token.clone());
                new_chord.full_symbol = new_token.clone();
            }
            
            Ok(new_chord)
        } else {
            // For note names, transpose the root note
            let root_note = crate::primitives::note::Note::from_string(&chord.root.note_name)
                .ok_or_else(|| "Invalid note name".to_string())?;
            
            // Calculate the semitone difference between the keys
            let from_root = from_key.root;
            let to_root = to_key.root;
            let from_semitone = from_root.to_semitone();
            let to_semitone = to_root.to_semitone();
            let semitone_diff = (to_semitone + 12 - from_semitone) % 12;
            
            // Transpose the root note by the semitone difference
            let transposed_note = root_note.transpose_semitones(semitone_diff);
            
            // Create new chord with transposed root, preserving the chord quality
            let new_root = transposed_note.to_string();
            let mut new_chord = chord.clone();
            new_chord.original_token = new_root.clone();
            new_chord.root = RootNote::new(new_root.clone(), new_root.clone());
            
            // Preserve the chord quality by transposing the full symbol
            let new_full_symbol = if chord.full_symbol.len() > chord.root.note_name.len() {
                // Has quality - transpose root but keep quality
                format!("{}{}", new_root, &chord.full_symbol[chord.root.note_name.len()..])
            } else {
                // No quality - just the root note
                new_root.clone()
            };
            
            // Re-parse the chord with the new full symbol to get the correct parsed data
            let mut chord_parser = ChordParser::new();
            if let Ok(parsed_chord) = chord_parser.parse(&new_full_symbol) {
                new_chord.parsed = parsed_chord;
                new_chord.full_symbol = new_full_symbol;
            }
            
            Ok(new_chord)
        }
    }
    
    // ============================================================================
    // AUTO-NUMBERING
    // ============================================================================
    
    fn auto_number_sections(&mut self) {
        // Count occurrences of each section type
        let mut type_counts: HashMap<SectionType, u32> = HashMap::new();
        for section in &self.sections {
            let stype = &section.section.section_type;
            if !matches!(stype, SectionType::Intro | SectionType::Outro | 
                         SectionType::Pre(_) | SectionType::Post(_)) {
                *type_counts.entry(stype.clone()).or_insert(0) += 1;
            }
        }
        
        // Assign numbers and split letters
        let mut counters: HashMap<SectionType, u32> = HashMap::new();
        let mut prev_type: Option<SectionType> = None;
        let mut consecutive_count = 0;
        let mut current_group_number: Option<u32> = None;
        
        for i in 0..self.sections.len() {
            let stype = self.sections[i].section.section_type.clone();
            
            // Don't number Intro/Outro/Pre/Post
            if matches!(stype, SectionType::Intro | SectionType::Outro | 
                        SectionType::Pre(_) | SectionType::Post(_)) {
                prev_type = Some(stype);
                consecutive_count = 0;
                current_group_number = None;
                continue;
            }
            
            // Only number if appears more than once
            let total_count = type_counts.get(&stype).copied().unwrap_or(0);
            let should_number = total_count > 1;
            
            // Check if consecutive
            let is_consecutive = prev_type.as_ref() == Some(&stype);
            
            if is_consecutive {
                consecutive_count += 1;
                
                // First consecutive: setup group
                if consecutive_count == 1 && i > 0 {
                    if let Some(prev_num) = self.sections[i - 1].section.number {
                        current_group_number = Some(prev_num);
                    } else if should_number {
                        let count = counters.entry(stype.clone()).or_insert(0);
                        *count += 1;
                        current_group_number = Some(*count);
                        self.sections[i - 1].section.number = Some(*count);
                    }
                    self.sections[i - 1].section.split_letter = Some('a');
                    consecutive_count = 2;
                }
                
                // Assign split letter
                let letter = (b'a' + consecutive_count as u8 - 1) as char;
                self.sections[i].section.split_letter = Some(letter);
                if should_number {
                    self.sections[i].section.number = current_group_number;
                }
            } else {
                // Not consecutive
                if should_number {
                    let count = counters.entry(stype.clone()).or_insert(0);
                    *count += 1;
                    self.sections[i].section.number = Some(*count);
                    current_group_number = Some(*count);
                } else {
                    self.sections[i].section.number = None;
                    current_group_number = None;
                }
                consecutive_count = 0;
            }
            
            prev_type = Some(stype);
        }
    }
    
    /// Adjust chord durations based on timing modifiers (push/pull)
    /// 
    /// This method processes all measures and adjusts chord durations when timing
    /// modifiers are present. Push timing shortens the previous chord, pull timing
    /// shortens the current chord and extends the next chord.
    fn adjust_chord_durations(&mut self, measures: &mut Vec<Measure>, time_signature: (u8, u8)) {
        let (numerator, _denominator) = time_signature;
        let _beats_per_measure = numerator as f64;
        
        // First pass: collect all chords with their measure and index
        let mut all_chords = Vec::new();
        for (measure_idx, measure) in measures.iter().enumerate() {
            for (chord_idx, chord) in measure.chords.iter().enumerate() {
                all_chords.push((measure_idx, chord_idx, chord.timing_modifier.clone()));
            }
        }
        
        // Second pass: apply timing adjustments
        for (measure_idx, chord_idx, timing_modifier) in all_chords {
            if let Some(timing_modifier) = timing_modifier {
                let offset_beats = match timing_modifier {
                    TimingModifier::PushEighth | TimingModifier::DelayEighth => 0.5,
                    TimingModifier::PushSixteenth | TimingModifier::DelaySixteenth => 0.25,
                    TimingModifier::PushThirtySecond | TimingModifier::DelayThirtySecond => 0.125,
                };
                
                match timing_modifier {
                    // Push timing: chord starts earlier, previous chord gets shorter
                    TimingModifier::PushEighth | 
                    TimingModifier::PushSixteenth | 
                    TimingModifier::PushThirtySecond => {
                        // Find the previous chord (could be in previous measure)
                        if chord_idx > 0 {
                            // Previous chord in same measure
                            let prev_chord = &mut measures[measure_idx].chords[chord_idx - 1];
                            let current_beats = prev_chord.duration.to_beats(time_signature);
                            prev_chord.duration = MusicalDuration::from_beats(current_beats - offset_beats, time_signature);
                        } else if measure_idx > 0 {
                            // Previous chord in previous measure
                            let prev_measure = &mut measures[measure_idx - 1];
                            if !prev_measure.chords.is_empty() {
                                let last_chord_idx = prev_measure.chords.len() - 1;
                                let prev_chord = &mut prev_measure.chords[last_chord_idx];
                                let current_beats = prev_chord.duration.to_beats(time_signature);
                                prev_chord.duration = MusicalDuration::from_beats(current_beats - offset_beats, time_signature);
                            }
                        }
                    },
                    
                    // Pull timing: chord starts later, this chord gets shorter, previous chord extends
                    TimingModifier::DelayEighth | 
                    TimingModifier::DelaySixteenth | 
                    TimingModifier::DelayThirtySecond => {
                        // Shorten the current chord
                        let current_chord = &mut measures[measure_idx].chords[chord_idx];
                        let current_beats = current_chord.duration.to_beats(time_signature);
                        current_chord.duration = MusicalDuration::from_beats(current_beats - offset_beats, time_signature);
                        
                        // Extend the previous chord (if it exists)
                        if chord_idx > 0 {
                            // Previous chord in same measure
                            let prev_chord = &mut measures[measure_idx].chords[chord_idx - 1];
                            let prev_beats = prev_chord.duration.to_beats(time_signature);
                            prev_chord.duration = MusicalDuration::from_beats(prev_beats + offset_beats, time_signature);
                        } else if measure_idx > 0 {
                            // Previous chord in previous measure
                            let prev_measure = &mut measures[measure_idx - 1];
                            if !prev_measure.chords.is_empty() {
                                let last_chord_idx = prev_measure.chords.len() - 1;
                                let prev_chord = &mut prev_measure.chords[last_chord_idx];
                                let prev_beats = prev_chord.duration.to_beats(time_signature);
                                prev_chord.duration = MusicalDuration::from_beats(prev_beats + offset_beats, time_signature);
                            }
                        }
                    }
                }
            }
        }
    }
    // calculate_section_duration moved to chart.rs
    
    /// Calculate the total duration in beats for a chord based on its rhythm
    fn calculate_chord_duration(&self, rhythm: &ChordRhythm, time_signature: (u8, u8)) -> f64 {
        let (numerator, _denominator) = time_signature;
        let beats_per_measure = numerator as f64;
        
        match rhythm {
            ChordRhythm::Default => beats_per_measure, // Full measure
            ChordRhythm::Duration(parsed_rhythm) => {
                // Calculate beats from ParsedRhythm
                let base_beats = beats_per_measure / parsed_rhythm.duration as f64;
                let dotted_beats = if parsed_rhythm.dots > 0 {
                    base_beats * 1.5
                } else {
                    base_beats
                };
                dotted_beats * parsed_rhythm.multiplier.unwrap_or(1) as f64
            },
            ChordRhythm::Slashes(slashes) => {
                // Each slash represents a beat in the time signature
                slashes.len() as f64
            },
            ChordRhythm::RestOrSpace { rhythm: parsed_rhythm, .. } => {
                // Same calculation as Duration
                let base_beats = beats_per_measure / parsed_rhythm.duration as f64;
                let dotted_beats = if parsed_rhythm.dots > 0 {
                    base_beats * 1.5
                } else {
                    base_beats
                };
                dotted_beats * parsed_rhythm.multiplier.unwrap_or(1) as f64
            }
        }
    }
}

// Display implementation moved to chart.rs

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_basic_chart_parsing() {
        let input = r#"Song Title - Artist Name
        68bpm 6/8 #G
in 4
Gmaj7 Cadd9 Em7 D13sus
vs
e d e g"#;
        
        let chart = Chart::parse(input).unwrap();
        
        assert_eq!(chart.metadata.title, Some("Song Title".to_string()));
        assert_eq!(chart.metadata.artist, Some("Artist Name".to_string()));
        assert!(chart.tempo.is_some());
        assert_eq!(chart.tempo.as_ref().unwrap().bpm, 68);
        assert!(chart.time_signature.is_some());
        assert_eq!(chart.time_signature.as_ref().unwrap().numerator, 6);
        assert_eq!(chart.time_signature.as_ref().unwrap().denominator, 8);
        
        assert_eq!(chart.sections.len(), 2);
        assert_eq!(chart.sections[0].section.section_type, SectionType::Intro);
        assert_eq!(chart.sections[1].section.section_type, SectionType::Verse);
    }
    
    #[test]
    fn test_chord_memory() {
        let input = r#"intro
Gmaj7 Cadd9
vs
g c"#;
        
        let chart = Chart::parse(input).unwrap();
        
        // Verse should expand g->Gmaj7, c->Cadd9
        let verse_chord1 = &chart.sections[1].measures[0].chords[0];
        assert_eq!(verse_chord1.root.display(), "G");
        assert_eq!(verse_chord1.full_symbol, "Gmaj7");
        
        let verse_chord2 = &chart.sections[1].measures[1].chords[0];
        assert_eq!(verse_chord2.root.display(), "C");
        assert_eq!(verse_chord2.full_symbol, "Cadd9");
    }
    
    #[test]
    fn test_one_time_override() {
        let input = r#"intro
Gmaj7
vs
!G7 g"#;
        
        let chart = Chart::parse(input).unwrap();
        
        // First g in verse is override (G7)
        assert_eq!(chart.sections[1].measures[0].chords[0].full_symbol, "G7");
        // Second g should still be Gmaj7 (global memory unchanged)
        assert_eq!(chart.sections[1].measures[1].chords[0].full_symbol, "Gmaj7");
    }
    
    #[test]
    fn test_pretty_display() {
        let input = r#"Beautiful Song - Amazing Artist
120bpm 4/4 #G

intro 2
Gmaj7 Cadd9
vs 4
g c d g
pre 2
a d
ch 8
g c d g a d g c
post 2
g c"#;
        
        let chart = Chart::parse(input).unwrap();
        let pretty_output = format!("{}", chart);
        
        // Verify the output contains expected elements
        assert!(pretty_output.contains("Beautiful Song"));
        assert!(pretty_output.contains("Amazing Artist"));
        assert!(pretty_output.contains("120 bpm"));
        assert!(pretty_output.contains("4/4"));
        assert!(pretty_output.contains("G Major"));
        assert!(pretty_output.contains("Intro"));
        assert!(pretty_output.contains("Verse"));
        assert!(pretty_output.contains("Pre-Chorus"));
        assert!(pretty_output.contains("Chorus"));
        assert!(pretty_output.contains("Post-Chorus"));
        
        // Print the pretty output for visual verification
        println!("{}", chart);
    }
}
