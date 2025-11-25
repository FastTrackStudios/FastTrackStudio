//! Output generators for different lyric formats

use crate::core::{LyricLine, LyricSection, Lyrics, Syllable, Word};
use crate::source::DerivedLyrics;

/// Generate a formatted lyric sheet (for printing/display)
pub struct LyricSheet;

impl LyricSheet {
    /// Generate formatted text output
    pub fn generate(lyrics: &Lyrics) -> String {
        let mut output = String::new();

        // Add song title
        output.push_str(&format!("{}\n", lyrics.song_name));
        output.push_str(&"=".repeat(lyrics.song_name.len()));
        output.push_str("\n\n");

        // Add each section
        for section in &lyrics.sections {
            output.push_str(&format!("[{}]\n", section.name));
            output.push_str("\n");

            for line in &section.lines {
                output.push_str(&line.display_text());
                output.push_str("\n");
            }

            output.push_str("\n");
        }

        output
    }
}

/// Configuration for slide break suggestions
#[derive(Debug, Clone, PartialEq)]
pub struct SlideBreakConfig {
    /// Maximum character count per slide (if exceeded, create new slide)
    pub max_chars: usize,
    /// Maximum word count per slide (if exceeded, create new slide)
    pub max_words: usize,
    /// Minimum character count to bundle lines together
    /// If both current and next line are below this, bundle them
    pub min_chars_to_bundle: usize,
    /// Minimum word count to bundle lines together
    /// If both current and next line are below this, bundle them
    pub min_words_to_bundle: usize,
}

impl Default for SlideBreakConfig {
    fn default() -> Self {
        Self {
            max_chars: 100,
            max_words: 15,
            min_chars_to_bundle: 50,
            min_words_to_bundle: 8,
        }
    }
}

/// Generate slides (line-by-line for ProPresenter-style display)
pub struct Slides;

impl Slides {
    /// Generate slides - each line becomes a slide
    pub fn generate(lyrics: &Lyrics) -> Vec<Slide> {
        Self::generate_with_config(lyrics, SlideBreakConfig::default())
    }

    /// Detect repeating patterns in a section
    /// Returns a vector of pattern boundaries (indices where patterns start)
    fn detect_patterns(section: &LyricSection) -> Vec<usize> {
        let lines = &section.lines;
        if lines.len() < 4 {
            return vec![0]; // Too short to detect patterns
        }

        // Try to find pattern length by looking for repeated structures
        // Check for patterns that divide the section evenly
        for pattern_len in 4..=(lines.len() / 2) {
            if lines.len() % pattern_len == 0 {
                let num_patterns = lines.len() / pattern_len;
                if num_patterns < 2 {
                    continue; // Need at least 2 patterns
                }
                
                // Check if lines repeat in this pattern
                // Compare regular text (ignoring background vocals for pattern detection)
                let mut is_pattern = true;
                
                for pattern_idx in 1..num_patterns {
                    let start1 = 0;
                    let start2 = pattern_idx * pattern_len;
                    
                    // Compare line structures (similarity check)
                    for i in 0..pattern_len {
                        let line1 = &lines[start1 + i];
                        let line2 = &lines[start2 + i];
                        
                        // Compare regular text only (ignore parenthetical parts for pattern detection)
                        let text1 = line1.regular_text();
                        let text2 = line2.regular_text();
                        
                        // If both are empty, they match
                        if text1.is_empty() && text2.is_empty() {
                            continue;
                        }
                        
                        // If one is empty and the other isn't, they don't match
                        if text1.is_empty() || text2.is_empty() {
                            is_pattern = false;
                            break;
                        }
                        
                        let len1 = text1.chars().count();
                        let len2 = text2.chars().count();
                        
                        // Allow variance in length (up to 40% difference for pattern detection)
                        // This handles cases where background vocals are added
                        if len1 > 0 && len2 > 0 {
                            let diff_ratio = (len1 as f64 - len2 as f64).abs() / len1.max(len2) as f64;
                            if diff_ratio > 0.4 {
                                is_pattern = false;
                                break;
                            }
                        }
                        
                        // Also check if the first few words are similar (for better pattern detection)
                        let words1: Vec<&str> = text1.split_whitespace().take(3).collect();
                        let words2: Vec<&str> = text2.split_whitespace().take(3).collect();
                        
                        if !words1.is_empty() && !words2.is_empty() {
                            // Check if at least the first word matches (case-insensitive)
                            if words1[0].to_lowercase() != words2[0].to_lowercase() {
                                is_pattern = false;
                                break;
                            }
                        }
                    }
                    
                    if !is_pattern {
                        break;
                    }
                }
                
                if is_pattern {
                    // Found a pattern! Mark boundaries
                    let mut pattern_boundaries = Vec::new();
                    for i in 0..=num_patterns {
                        pattern_boundaries.push(i * pattern_len);
                    }
                    return pattern_boundaries;
                }
            }
        }
        
        // No clear pattern found, treat as single pattern
        vec![0]
    }

    /// Generate slides with auto-suggested breaks based on character/word counts
    pub fn generate_with_config(lyrics: &Lyrics, config: SlideBreakConfig) -> Vec<Slide> {
        Self::generate_with_config_and_annotations(lyrics, config, None)
    }

    /// Generate slides with annotations (respects custom slide breaks)
    pub fn generate_with_config_and_annotations(
        lyrics: &Lyrics,
        config: SlideBreakConfig,
        derived: Option<&DerivedLyrics>,
    ) -> Vec<Slide> {
        let mut slides = Vec::new();

        for (section_idx, section) in lyrics.sections.iter().enumerate() {
            // Detect patterns in this section
            let pattern_boundaries = Self::detect_patterns(section);
            
            // Process each pattern separately
            for pattern_idx in 0..pattern_boundaries.len() {
                let pattern_start = pattern_boundaries[pattern_idx];
                let pattern_end = if pattern_idx + 1 < pattern_boundaries.len() {
                    pattern_boundaries[pattern_idx + 1]
                } else {
                    section.lines.len()
                };
                
                let pattern_lines = &section.lines[pattern_start..pattern_end];
                
                // Calculate ideal lines per slide for even distribution
                let total_lines = pattern_lines.len();
                if total_lines == 0 {
                    continue;
                }
                
                // Calculate word counts for each line
                let line_word_counts: Vec<usize> = pattern_lines.iter()
                    .map(|l| l.regular_text().split_whitespace().count())
                    .collect();
                
                // Target: 2 lines per slide for even distribution (preferred)
                let mut target_lines_per_slide = 2;
                
                // Enforce strict 19-word maximum per slide
                let max_words_per_slide = config.max_words.min(19);
                
                // Check if we can fit 2 lines per slide based on word limits
                // Look at the maximum words in any 2 consecutive lines
                let mut max_words_in_two_lines = 0;
                for i in 0..(total_lines.saturating_sub(1)) {
                    let words_in_two = line_word_counts[i] + line_word_counts[i + 1];
                    max_words_in_two_lines = max_words_in_two_lines.max(words_in_two);
                }
                
                // If the max words in 2 lines exceeds the limit, use 1 line per slide
                if max_words_in_two_lines > max_words_per_slide {
                    target_lines_per_slide = 1;
                } else {
                    // Check if we can fit 3 lines (for very short lines)
                    // Only use 3 if it results in perfectly even distribution
                    let mut max_words_in_three_lines = 0;
                    for i in 0..(total_lines.saturating_sub(2)) {
                        let words_in_three = line_word_counts[i] + line_word_counts[i + 1] + line_word_counts[i + 2];
                        max_words_in_three_lines = max_words_in_three_lines.max(words_in_three);
                    }
                    
                    // Only use 3 if:
                    // 1. It fits within word limits (max 19 words)
                    // 2. It results in perfectly even distribution (total_lines % 3 == 0)
                    // 3. It's better than using 2 (e.g., 9 lines: 3,3,3 vs 2,2,2,2,1)
                    if max_words_in_three_lines <= max_words_per_slide && 
                       total_lines % 3 == 0 && 
                       total_lines >= 6 {
                        target_lines_per_slide = 3;
                    } else {
                        // Default to 2 for even distribution
                        target_lines_per_slide = 2;
                    }
                }
                
                // Create slides with even distribution within this pattern
                // Calculate ideal number of slides for even distribution (aim for 2 lines per slide)
                let num_slides = (total_lines as f64 / target_lines_per_slide as f64).ceil() as usize;
                let ideal_lines_per_slide = if num_slides > 0 {
                    (total_lines as f64 / num_slides as f64).ceil() as usize
                } else {
                    target_lines_per_slide
                };
                
                // Pre-calculate how many lines each slide should get for even distribution
                // For 8 lines with target 2, we want: 2, 2, 2, 2
                let mut lines_per_slide: Vec<usize> = Vec::new();
                let base_lines = total_lines / num_slides;
                let extra_lines = total_lines % num_slides;
                
                // Distribute base_lines to all slides, then add 1 to the first extra_lines slides
                for i in 0..num_slides {
                    let lines = base_lines + if i < extra_lines { 1 } else { 0 };
                    lines_per_slide.push(lines);
                }
                
                let mut line_idx = 0;
                let mut slide_num = 0;
                let mut current_slide_lines = Vec::new();
                
                while line_idx < total_lines && slide_num < lines_per_slide.len() {
                    // Check for custom slide break at this position
                    let absolute_line_idx = pattern_start + line_idx;
                    let has_custom_break = derived
                        .and_then(|d| Some(d.is_custom_slide_break(section_idx, absolute_line_idx)))
                        .unwrap_or(false);

                    // If there's a custom break and we already have lines, finish current slide
                    if has_custom_break && !current_slide_lines.is_empty() {
                        let slide_text = current_slide_lines
                            .iter()
                            .map(|l: &LyricLine| l.display_text())
                            .collect::<Vec<_>>()
                            .join("\n");
                        
                        slides.push(Slide {
                            section_name: section.name.clone(),
                            text: slide_text,
                            is_section_header: false,
                            lines: current_slide_lines.clone(),
                        });
                        current_slide_lines.clear();
                        slide_num += 1;
                    }

                    let mut slide_lines = Vec::new();
                    let mut slide_words = 0;
                    
                    // Get target lines for this slide from pre-calculated distribution
                    let target_lines = lines_per_slide[slide_num];
                    
                    // Take the lines for this slide, prioritizing word limits
                    // Enforce strict 19-word maximum per slide
                    let max_words_per_slide = config.max_words.min(19);
                    let mut lines_taken = 0;
                    for i in 0..target_lines.min(total_lines - line_idx) {
                        let line = &pattern_lines[line_idx + i];
                        let line_words = line_word_counts[line_idx + i];
                        
                        // Check if adding this line would exceed word limit (primary check)
                        // Use the stricter of config.max_words and 19
                        let would_exceed_words = slide_words + line_words > max_words_per_slide;
                        
                        // If we haven't taken any lines yet and this would exceed, we must take it anyway
                        // (exception for very long lines - they get their own slide)
                        if would_exceed_words && lines_taken > 0 {
                            break; // Stop here, this line goes to next slide
                        }
                        
                        // Also check character limit as a safety check (but not primary)
                        let line_text = line.regular_text();
                        let line_chars = line_text.chars().count();
                        let current_slide_chars: usize = slide_lines.iter()
                            .map(|l: &LyricLine| l.regular_text().chars().count())
                            .sum();
                        let would_exceed_chars = current_slide_chars + line_chars > config.max_chars;
                        
                        // Only break on char limit if we already have at least one line
                        if would_exceed_chars && lines_taken > 0 {
                            break;
                        }
                        
                        slide_lines.push(line.clone());
                        slide_words += line_words;
                        lines_taken += 1;
                    }
                    
                    if !slide_lines.is_empty() {
                        let lines_count = slide_lines.len();
                        let slide_text = slide_lines
                            .iter()
                            .map(|l| l.display_text())
                            .collect::<Vec<_>>()
                            .join("\n");
                        
                        slides.push(Slide {
                            section_name: section.name.clone(),
                            text: slide_text,
                            is_section_header: false,
                            lines: slide_lines,
                        });
                        
                        line_idx += lines_count;
                        slide_num += 1;
                    } else {
                        // Safety: if we can't add any lines, force add one
                        let line = pattern_lines[line_idx].clone();
                        let slide_text = line.display_text();
                        slides.push(Slide {
                            section_name: section.name.clone(),
                            text: slide_text,
                            is_section_header: false,
                            lines: vec![line],
                        });
                        line_idx += 1;
                        slide_num += 1;
                    }
                }
                
                // Add any remaining lines
                while line_idx < total_lines {
                    let line = pattern_lines[line_idx].clone();
                    let slide_text = line.display_text();
                    slides.push(Slide {
                        section_name: section.name.clone(),
                        text: slide_text,
                        is_section_header: false,
                        lines: vec![line],
                    });
                    line_idx += 1;
                }
            }
        }

        slides
    }
}

/// Represents a single slide
#[derive(Debug, Clone, PartialEq)]
pub struct Slide {
    /// The section this slide belongs to
    pub section_name: String,
    /// The text content of the slide
    pub text: String,
    /// Whether this is a section header slide
    pub is_section_header: bool,
    /// The lines that make up this slide
    pub lines: Vec<LyricLine>,
}

/// Generate syllables (karaoke-style syllable splits)
pub struct Syllables;

impl Syllables {
    /// Generate syllable breakdown for all lyrics
    pub fn generate(lyrics: &Lyrics) -> Vec<SyllableLine> {
        let mut syllable_lines = Vec::new();

        for section in &lyrics.sections {
            for line in &section.lines {
                let words = Self::break_into_words(&line.regular_text());
                let syllables = words
                    .iter()
                    .flat_map(|word| word.syllables.clone())
                    .collect();

                syllable_lines.push(SyllableLine {
                    section_name: section.name.clone(),
                    original_text: line.text.clone(),
                    syllables,
                });
            }
        }

        syllable_lines
    }

    /// Break text into words and syllables
    fn break_into_words(text: &str) -> Vec<Word> {
        text.split_whitespace()
            .map(|word| Word::from_text(word))
            .collect()
    }
}

/// Represents a line broken down into syllables
#[derive(Debug, Clone, PartialEq)]
pub struct SyllableLine {
    /// The section this line belongs to
    pub section_name: String,
    /// The original text of the line
    pub original_text: String,
    /// Syllables in this line
    pub syllables: Vec<Syllable>,
}

/// Generate MIDI data for lyrics (notes for each syllable)
pub struct MidiGenerator;

impl MidiGenerator {
    /// Generate MIDI events for all lyrics
    ///
    /// This creates a simple representation that can be converted to actual MIDI format.
    /// Each syllable gets a note event.
    pub fn generate(lyrics: &Lyrics, default_note: u8, default_velocity: u8) -> Vec<MidiEvent> {
        let mut events = Vec::new();
        let mut current_time = 0.0;

        for section in &lyrics.sections {
            for line in &section.lines {
                // Break line into words and syllables
                let words = Syllables::break_into_words(&line.regular_text());
                let syllables: Vec<Syllable> = words
                    .iter()
                    .flat_map(|word| word.syllables.clone())
                    .collect();

                // Create MIDI events for each syllable
                // Default duration per syllable (can be customized)
                let duration_per_syllable = 0.5; // seconds

                for syllable in syllables {
                    let start_time = line.start_time.unwrap_or(current_time);
                    let end_time = line
                        .end_time
                        .unwrap_or(start_time + duration_per_syllable);

                    events.push(MidiEvent {
                        start_time,
                        end_time,
                        note: syllable.midi_note.unwrap_or(default_note),
                        velocity: syllable.midi_velocity.unwrap_or(default_velocity),
                        text: syllable.text.clone(),
                        section_name: section.name.clone(),
                    });

                    current_time = end_time;
                }
            }
        }

        events
    }
}

/// Represents a MIDI event for a syllable
#[derive(Debug, Clone, PartialEq)]
pub struct MidiEvent {
    /// Start time in seconds
    pub start_time: f64,
    /// End time in seconds
    pub end_time: f64,
    /// MIDI note number (0-127)
    pub note: u8,
    /// MIDI velocity (0-127)
    pub velocity: u8,
    /// The text/syllable this event represents
    pub text: String,
    /// The section this event belongs to
    pub section_name: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_lyrics;

    #[test]
    fn test_lyric_sheet() {
        let text = r#"[Intro]
(Woo)
Well!

[Verse 1]
Test line"#;

        let lyrics = parse_lyrics(text, "Test Song".to_string()).unwrap();
        let sheet = LyricSheet::generate(&lyrics);
        assert!(sheet.contains("Test Song"));
        assert!(sheet.contains("[Intro]"));
        assert!(sheet.contains("(Woo)"));
    }

    #[test]
    fn test_slides() {
        let text = r#"[Intro]
Line 1
Line 2"#;

        let lyrics = parse_lyrics(text, "Test Song".to_string()).unwrap();
        let slides = Slides::generate(&lyrics);
        assert_eq!(slides.len(), 3); // Section header + 2 lines
        assert!(slides[0].is_section_header);
        assert!(!slides[1].is_section_header);
    }

    #[test]
    fn test_slides_with_bundling() {
        let text = r#"[Bridge]
Waiting, I'm patient
In keeping, I'm wanting more
Picking up the loose puzzle pieces"#;

        let lyrics = parse_lyrics(text, "Test Song".to_string()).unwrap();
        let config = SlideBreakConfig {
            max_chars: 100,
            max_words: 15,
            min_chars_to_bundle: 50,
            min_words_to_bundle: 8,
        };
        let slides = Slides::generate_with_config(&lyrics, config);
        
        // Should bundle the first two short lines together
        assert!(slides.len() >= 2); // Section header + at least 1 content slide
        let content_slides: Vec<_> = slides.iter().filter(|s| !s.is_section_header).collect();
        assert!(!content_slides.is_empty());
        
        // First slide should contain both short lines
        let first_slide = &content_slides[0];
        assert!(first_slide.text.contains("Waiting, I'm patient"));
        assert!(first_slide.text.contains("In keeping, I'm wanting more"));
    }

    #[test]
    fn test_syllables() {
        let text = r#"[Verse 1]
Hello world"#;

        let lyrics = parse_lyrics(text, "Test Song".to_string()).unwrap();
        let syllable_lines = Syllables::generate(&lyrics);
        assert_eq!(syllable_lines.len(), 1);
        assert!(!syllable_lines[0].syllables.is_empty());
    }

    #[test]
    fn test_midi_generator() {
        let text = r#"[Verse 1]
Test line"#;

        let lyrics = parse_lyrics(text, "Test Song".to_string()).unwrap();
        let events = MidiGenerator::generate(&lyrics, 60, 100);
        assert!(!events.is_empty());
        assert_eq!(events[0].note, 60);
    }
}

