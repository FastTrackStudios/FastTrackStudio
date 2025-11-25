//! Example lyrics test with full song lyrics

use lyrics::core::{Lyrics, LinePart};
use lyrics::parser::{parse_lyrics, parse_lyrics_with_config, ParserConfig, BackgroundVocalPattern};
use colored::*;

const EXAMPLE_LYRICS: &str = r#"[Intro]

(Woo)
(Are you ready?)
Well!
Well!

[Verse 1]

Drowning, fishing, dropping, screaming under the lights
I'm feeling everything crashing, burning, I lost track of time. Into you

[Chorus]

I'm breathing, I'm breathing, I think I'm reading you well
I'm breathing, I'm breathing, I think I'm reading you well

[Verse 2]

Burning, crashing, trampoline, the edge of the sun
I think I fell in love with lightning bolt, I'm ready to run. Into you

[Chorus]

I'm screaming, I'm screaming, I think I'm reading you well
Believe it, believe it, I think I'm reading you well (Well, think I'm reading you well)
(I think I'm reading you well)

[Bridge]

Waiting, I'm patient
In keeping, I'm wanting more
Picking up the loose puzzle pieces
Scattered around the floor
Linking, I'm drifting
Just waiting for you to know
Following the crumbs that I left for you
Leading to my door

Waiting, I'm patient (Hey, oh)
In keeping, I'm wanting more (Hey)
Picking up the loose puzzle pieces
Scattered around the floor
Linking, I'm drifting (Hey, oh)
Just waiting for you to know (Hey)
Following the crumbs that I left for you
Leading to my door

[Outro]

I think I'm reading, I, I think I'm reading you well
I think I'm screaming, I, I think I'm reading you well
I'm breathing, I'm breathing, I think I'm reading you well
I think I'm reading you well
(Okay)
"#;

#[test]
fn test_parse_example_lyrics() {
    let lyrics = parse_lyrics(EXAMPLE_LYRICS, "Example Song".to_string()).unwrap();
    
    // Verify basic structure
    assert_eq!(lyrics.song_name, "Example Song");
    assert!(lyrics.sections.len() >= 5); // Intro, Verse 1, Chorus, Verse 2, Bridge, Outro
    
    // Check for specific sections
    let intro_sections: Vec<_> = lyrics.find_sections_by_name("Intro");
    assert!(!intro_sections.is_empty(), "Should have Intro section");
    
    let verse1_sections: Vec<_> = lyrics.find_sections_by_name("Verse 1");
    assert!(!verse1_sections.is_empty(), "Should have Verse 1 section");
    
    let chorus_sections: Vec<_> = lyrics.find_sections_by_name("Chorus");
    assert!(!chorus_sections.is_empty(), "Should have Chorus section");
    
    let bridge_sections: Vec<_> = lyrics.find_sections_by_name("Bridge");
    assert!(!bridge_sections.is_empty(), "Should have Bridge section");
    
    let outro_sections: Vec<_> = lyrics.find_sections_by_name("Outro");
    assert!(!outro_sections.is_empty(), "Should have Outro section");
    
    // Verify parenthetical parts are parsed correctly
    let intro = &intro_sections[0];
    assert!(intro.lines.len() >= 4, "Intro should have at least 4 lines");
    
    // Check for parenthetical lines
    let has_parenthetical = intro.lines.iter().any(|line| {
        line.parts.iter().any(|part| {
            matches!(part, lyrics::core::LinePart::Parenthetical(_))
        })
    });
    assert!(has_parenthetical, "Intro should have parenthetical parts");
    
    println!("Parsed {} sections", lyrics.sections.len());
    for section in &lyrics.sections {
        println!("Section: {} ({} lines)", section.name, section.lines.len());
    }
}

#[test]
fn test_parse_example_lyrics_with_lead_background_split() {
    let config = ParserConfig {
        split_lead_background: true,
        background_vocal_pattern: BackgroundVocalPattern::Parenthetical,
    };
    
    let lyrics = parse_lyrics_with_config(EXAMPLE_LYRICS, "Example Song".to_string(), config).unwrap();
    
    // Verify split metadata is set
    assert_eq!(
        lyrics.get_metadata("split_lead_background"),
        Some(&"true".to_string())
    );
    
    // Count background vocal parts (parenthetical)
    let mut total_background_parts = 0;
    let mut total_lead_parts = 0;
    
    for section in &lyrics.sections {
        for line in &section.lines {
            for part in &line.parts {
                match part {
                    lyrics::core::LinePart::Parenthetical(_) => {
                        total_background_parts += 1;
                    }
                    lyrics::core::LinePart::Regular(_) => {
                        total_lead_parts += 1;
                    }
                }
            }
        }
    }
    
    println!("\n{}", "═".repeat(80).bright_black());
    println!("{} {}", "📊".bright_cyan(), "Vocal Parts Statistics".bright_white().bold());
    println!("{}", "═".repeat(80).bright_black());
    println!(
        "  {} {}",
        "🎤 Lead vocals:".bright_white(),
        total_lead_parts.to_string().bright_green().bold()
    );
    println!(
        "  {} {}",
        "🎵 Background vocals:".bright_black(),
        total_background_parts.to_string().bright_yellow().bold()
    );
    println!("{}", "═".repeat(80).bright_black());
    
    // Should have background vocals (parenthetical parts)
    assert!(total_background_parts > 0, "Should have background vocal parts");
    assert!(total_lead_parts > 0, "Should have lead vocal parts");
    
    // Display the split
    display_lead_background_split(&lyrics);
    
    // Display slides with auto-suggested breaks
    display_slides(&lyrics);
}

/// Display lyrics in a beautiful, colored format
fn display_lyrics(lyrics: &Lyrics) {
    // Header
    println!("\n{}", "═".repeat(80).bright_black());
    println!("{} {}", "🎵".bright_yellow(), lyrics.song_name.bright_white().bold());
    println!("{}", "═".repeat(80).bright_black());
    
    // Sections
    for (idx, section) in lyrics.sections.iter().enumerate() {
        // Section header
        let section_color = match section.section_type {
            Some(lyrics::core::SectionTypeHint::Intro) => Color::Cyan,
            Some(lyrics::core::SectionTypeHint::Verse) => Color::Green,
            Some(lyrics::core::SectionTypeHint::Chorus) => Color::Magenta,
            Some(lyrics::core::SectionTypeHint::Bridge) => Color::Yellow,
            Some(lyrics::core::SectionTypeHint::Outro) => Color::Red,
            _ => Color::White,
        };
        
        println!("\n{}", "─".repeat(80).bright_black());
        // Section name already contains the number (e.g., "Verse 1"), so just use it directly
        println!(
            "{} {} {} {}",
            format!("[{}]", idx + 1).bright_black(),
            "📝".bright_black(),
            section.name.color(section_color).bold(),
            format!("({} lines)", section.lines.len()).bright_black()
        );
        println!("{}", "─".repeat(80).bright_black());
        
        // Lines
        for (line_idx, line) in section.lines.iter().enumerate() {
            // Build the line with colored parts
            let mut line_parts = Vec::new();
            for part in &line.parts {
                match part {
                    LinePart::Regular(text) => {
                        line_parts.push(text.white().to_string());
                    }
                    LinePart::Parenthetical(text) => {
                        line_parts.push(format!("({})", text).bright_black().italic().to_string());
                    }
                }
            }
            
            // Print line number and content
            let line_number = format!("{:2}", line_idx + 1).bright_black();
            let line_content = line_parts.join(" ");
            println!("  {} │ {}", line_number, line_content);
        }
    }
    
    // Footer
    println!("\n{}", "═".repeat(80).bright_black());
    println!(
        "{} {} sections, {} total lines",
        "✨".bright_yellow(),
        lyrics.sections.len().to_string().bright_white().bold(),
        lyrics.sections.iter().map(|s| s.lines.len()).sum::<usize>().to_string().bright_white().bold()
    );
    println!("{}", "═".repeat(80).bright_black());
}

/// Display lead and background vocal split
fn display_lead_background_split(lyrics: &Lyrics) {
    println!("\n{}", "═".repeat(80).bright_black());
    println!("{} {}", "🎤".bright_cyan(), "Lead & Background Vocals Split".bright_white().bold());
    println!("{}", "═".repeat(80).bright_black());
    
    for section in &lyrics.sections {
        let section_color = match section.section_type {
            Some(lyrics::core::SectionTypeHint::Intro) => Color::Cyan,
            Some(lyrics::core::SectionTypeHint::Verse) => Color::Green,
            Some(lyrics::core::SectionTypeHint::Chorus) => Color::Magenta,
            Some(lyrics::core::SectionTypeHint::Bridge) => Color::Yellow,
            Some(lyrics::core::SectionTypeHint::Outro) => Color::Red,
            _ => Color::White,
        };
        
        println!("\n{} {}", "📝".bright_black(), section.name.color(section_color).bold());
        println!("{}", "─".repeat(80).bright_black());
        
        for line in &section.lines {
            // Check if line has only parenthetical parts
            let has_regular = line.parts.iter().any(|p| matches!(p, LinePart::Regular(_)));
            let has_parenthetical = line.parts.iter().any(|p| matches!(p, LinePart::Parenthetical(_)));
            
            if has_regular {
                // Line has regular text - show it as lead vocal
                let lead_text = line.regular_text();
                if !lead_text.is_empty() {
                    println!("  {} {}", "🎤".bright_white(), lead_text.white());
                }
            }
            
            if has_parenthetical {
                // Line has parenthetical text - show it as background vocal
                let bg_parts: Vec<String> = line.parts
                    .iter()
                    .filter_map(|p| {
                        if let LinePart::Parenthetical(text) = p {
                            Some(text.clone())
                        } else {
                            None
                        }
                    })
                    .collect();
                
                if !bg_parts.is_empty() {
                    let bg_text = bg_parts.join(", ");
                    println!("  {} {}", "🎵".bright_black(), format!("({})", bg_text).bright_black().italic());
                }
            }
        }
    }
    
    println!("\n{}", "═".repeat(80).bright_black());
}

/// Display slides with auto-suggested breaks
fn display_slides(lyrics: &Lyrics) {
    use lyrics::output::{Slides, SlideBreakConfig};
    
    println!("\n{}", "═".repeat(80).bright_black());
    println!("{} {}", "📽️".bright_cyan(), "Auto-Suggested Slides".bright_white().bold());
    println!("{}", "═".repeat(80).bright_black());
    
    // Config: bundle short lines together (like "Waiting, I'm patient" + "In keeping, I'm wanting more")
    // "Waiting, I'm patient" = ~21 chars, "In keeping, I'm wanting more" = ~29 chars
    // Combined = ~50 chars, so we want min_chars_to_bundle to be around 30-35
    // "Picking up the loose puzzle pieces" = ~35 chars, so it should NOT be bundled
    // For even distribution, we want 2 lines per slide when possible
    let config = SlideBreakConfig {
        max_chars: 120,
        max_words: 20,
        min_chars_to_bundle: 32,  // Lines under 32 chars can be bundled (catches the first two)
        min_words_to_bundle: 7,   // Lines under 7 words can be bundled
    };
    
    let slides = Slides::generate_with_config(lyrics, config);
    
    for (slide_idx, slide) in slides.iter().enumerate() {
        // Get section color based on section name
        let section_color = match lyrics.sections.iter().find(|s| s.name == slide.section_name) {
            Some(s) => match s.section_type {
                Some(lyrics::core::SectionTypeHint::Intro) => Color::Cyan,
                Some(lyrics::core::SectionTypeHint::Verse) => Color::Green,
                Some(lyrics::core::SectionTypeHint::Chorus) => Color::Magenta,
                Some(lyrics::core::SectionTypeHint::Bridge) => Color::Yellow,
                Some(lyrics::core::SectionTypeHint::Outro) => Color::Red,
                _ => Color::White,
            },
            None => Color::White,
        };
        
        println!("\n{}", "─".repeat(80).bright_black());
        println!(
            "{} {} {} {}",
            format!("[Slide {}]", slide_idx + 1).bright_black(),
            format!("[{}]", slide.section_name).color(section_color).bold(),
            "📽️".bright_yellow(),
            format!("({} lines)", slide.lines.len()).bright_black()
        );
        
        for (line_idx, line) in slide.lines.iter().enumerate() {
            // Build the line with colored parts
            let mut line_parts = Vec::new();
            for part in &line.parts {
                match part {
                    LinePart::Regular(text) => {
                        line_parts.push(text.white().to_string());
                    }
                    LinePart::Parenthetical(text) => {
                        line_parts.push(format!("({})", text).bright_black().italic().to_string());
                    }
                }
            }
            
            let line_content = line_parts.join(" ");
            println!("  {} │ {}", format!("{:2}", line_idx + 1).bright_black(), line_content);
        }
    }
    
    println!("\n{}", "═".repeat(80).bright_black());
    println!(
        "{} {} total slides",
        "✨".bright_yellow(),
        slides.len().to_string().bright_white().bold()
    );
    println!("{}", "═".repeat(80).bright_black());
}

#[test]
fn test_example_lyrics_sections_and_lines() {
    let lyrics = parse_lyrics(EXAMPLE_LYRICS, "Example Song".to_string()).unwrap();
    
    // Verify we have the expected sections
    let section_names: Vec<&str> = lyrics.sections.iter().map(|s| s.name.as_str()).collect();
    
    assert!(section_names.contains(&"Intro"));
    assert!(section_names.contains(&"Verse 1"));
    assert!(section_names.contains(&"Chorus"));
    assert!(section_names.contains(&"Verse 2"));
    assert!(section_names.contains(&"Bridge"));
    assert!(section_names.contains(&"Outro"));
    
    // Display with beautiful formatting (only if running single test)
    // Commented out to avoid interleaved output when running all tests
    // display_lyrics(&lyrics);
}

