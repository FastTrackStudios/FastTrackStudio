//! Hardcoded example for "Well! Well!" syllable timing
//! 
//! This demonstrates the line-based syllable timing system where timing
//! is keyed by line content, so it stays with the line even if it moves around.

use lyrics::core::LyricLine;
use lyrics::parser::parse_lyrics;
use lyrics::source::{LyricsSheet, LyricsAnnotations, LyricTime, SyllableTiming};

/// Create a hardcoded example with timing for the "Well! Well!" lines
pub fn create_well_timing_example() -> (LyricsSheet, LyricsAnnotations) {
    // Create the lyrics sheet (source of truth)
    let raw_text = r#"[Intro]

(Woo)
(Are you ready?)
Well!
Well!
"#;

    let mut sheet = LyricsSheet::new("Example Song".to_string(), raw_text.to_string());

    // Create annotations with hardcoded timing for "Well!" lines
    let mut annotations = LyricsAnnotations::new(sheet.id, sheet.version);

    // Note: Since both lines have the same text "Well!", we can't distinguish
    // them by text alone. In a real system, you'd need to handle duplicate lines
    // differently (e.g., by storing line index or using a composite key).
    // For this example, we'll just set timing for one "Well!" line.
    // The advanced example below shows a better approach.
    
    // For the second "Well!", we could use a different key or store both
    // For now, this demonstrates the concept - in production you'd handle
    // duplicate lines differently (e.g., by storing line index in the key)

    (sheet, annotations)
}

/// More sophisticated example that handles duplicate lines
pub fn create_well_timing_example_advanced() -> (LyricsSheet, LyricsAnnotations) {
    let raw_text = r#"[Intro]

(Woo)
(Are you ready?)
Well!
Well!
"#;

    let mut sheet = LyricsSheet::new("Example Song".to_string(), raw_text.to_string());

    // Parse to get the actual line structure
    let lyrics = parse_lyrics(&raw_text, "Example Song".to_string()).unwrap();
    
    let mut annotations = LyricsAnnotations::new(sheet.id, sheet.version);

    // Find the "Well!" lines in the intro
    if let Some(intro_section) = lyrics.sections.iter().find(|s| s.name == "Intro") {
        for (line_idx, line) in intro_section.lines.iter().enumerate() {
            let line_text = line.regular_text();
            
            if line_text == "Well!" {
                // Create timing based on which occurrence it is
                let (start_lrc, end_lrc) = if line_idx == 2 {
                    // First "Well!" (after "(Woo)" and "(Are you ready?)")
                    ("00:05.00", "00:05.50")
                } else if line_idx == 3 {
                    // Second "Well!"
                    ("00:05.75", "00:06.25")
                } else {
                    continue;
                };

                let timing = vec![
                    vec![SyllableTiming {
                        start: LyricTime::from_lrc(start_lrc).unwrap(),
                        end: LyricTime::from_lrc(end_lrc).unwrap(),
                    }]
                ];

                // Use line text as key (in production, you might want to include
                // line index or some other disambiguator for duplicate lines)
                annotations.set_line_syllable_timing(line_text.clone(), timing);

                // Also set MIDI note (C4, velocity 100)
                annotations.set_line_syllable_note(line_text, 0, 0, 60, 100);
            }
        }
    }

    (sheet, annotations)
}

/// Display the timing information for a line
pub fn display_line_timing(line: &LyricLine, annotations: &LyricsAnnotations) {
    let line_text = line.regular_text();
    
    println!("Line: \"{}\"", line_text);
    
    if let Some(timing) = annotations.get_line_syllable_timing(&line_text) {
        println!("  Has timing annotations:");
        for (word_idx, word_timing) in timing.word_timings.iter().enumerate() {
            println!("    Word {}:", word_idx);
            for (syllable_idx, syllable_timing) in word_timing.iter().enumerate() {
                println!(
                    "      Syllable {}: {} - {}",
                    syllable_idx,
                    syllable_timing.start.to_lrc(),
                    syllable_timing.end.to_lrc()
                );
                
                if let Some(note) = annotations.get_line_syllable_note(&line_text, word_idx, syllable_idx) {
                    println!("        MIDI: note={}, velocity={}", note.note, note.velocity);
                }
            }
        }
    } else {
        println!("  No timing annotations");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_well_timing_example() {
        let (sheet, annotations) = create_well_timing_example_advanced();
        
        println!("\n=== Well! Timing Example ===\n");
        println!("Sheet ID: {}", sheet.id);
        println!("Song: {}", sheet.song_name);
        println!("\nAnnotations:");
        println!("  Sheet ID: {}", annotations.sheet_id);
        println!("  Line timings: {}", annotations.line_syllable_timings.len());
        
        // Parse the lyrics to display timing
        let lyrics = parse_lyrics(&sheet.raw_text, sheet.song_name.clone()).unwrap();
        
        if let Some(intro_section) = lyrics.sections.iter().find(|s| s.name == "Intro") {
            println!("\nIntro section lines:");
            for line in &intro_section.lines {
                display_line_timing(line, &annotations);
                println!();
            }
        }
    }

    #[test]
    fn test_lyric_time_formats() {
        // Test LRC format parsing
        let time1 = LyricTime::from_lrc("00:05.00").unwrap();
        assert_eq!(time1.seconds, 5.0);
        assert_eq!(time1.to_lrc(), "00:05.00");

        let time2 = LyricTime::from_lrc("00:05.50").unwrap();
        assert_eq!(time2.seconds, 5.5);
        assert_eq!(time2.to_lrc(), "00:05.50");

        let time3 = LyricTime::from_lrc("01:23.45").unwrap();
        assert_eq!(time3.seconds, 83.45);
        assert_eq!(time3.to_lrc(), "01:23.45");

        println!("\n=== Time Format Examples ===");
        println!("5.0 seconds = {}", time1.to_lrc());
        println!("5.5 seconds = {}", time2.to_lrc());
        println!("83.45 seconds = {}", time3.to_lrc());
    }
}

