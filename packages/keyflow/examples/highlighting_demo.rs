//! Highlighting Demo Example
//!
//! Demonstrates syntax highlighting for Keyflow notation using ANSI terminal colors.
//!
//! # Usage
//!
//! ```bash
//! cargo run -p keyflow --example highlighting_demo --features highlighting
//! ```

use keyflow::highlighting::{Highlighter, Theme};

mod render {
    use keyflow::highlighting::{HighlightSpan, Theme};

    /// Render highlighted spans to ANSI terminal escape codes.
    pub fn to_ansi(source: &str, spans: &[HighlightSpan], theme: &Theme) -> String {
        let mut output = String::with_capacity(source.len() * 2);
        let mut pos = 0;

        for span in spans {
            // Add unstyled text before this span
            if span.span.start > pos {
                output.push_str(&source[pos..span.span.start]);
            }

            // Add the styled span
            if let Some(text) = span.extract(source) {
                let style = theme.style_for(span.kind);
                let color_code = style.color.to_ansi_256();
                let mut codes = vec![format!("38;5;{color_code}")];

                if style.bold {
                    codes.push("1".to_string());
                }
                if style.italic {
                    codes.push("3".to_string());
                }
                if style.underline {
                    codes.push("4".to_string());
                }

                output.push_str(&format!("\x1b[{}m", codes.join(";")));
                output.push_str(text);
                output.push_str("\x1b[0m"); // Reset
            }

            pos = span.span.end();
        }

        // Add any remaining unstyled text
        if pos < source.len() {
            output.push_str(&source[pos..]);
        }

        output
    }
}

const DEMO_CHART: &str = r#"Amazing Grace - Traditional
120bpm 4/4 #G

COUNT 2

IN 4
G G7 | C G | G | D

VS 8
G G7 | C G | G | D | G G7 | C G | G D | G

CH 8 "Build"
C | G | C | G | G7 | C G | G D | G

BR 4
Am | Em | C | D7

VS 8
G G7 | C G | G | D | G G7 | C G | G D | G

OUT 4
G | D | G | G
"#;

fn main() {
    println!("=== Keyflow Syntax Highlighting Demo ===\n");

    let theme = Theme::default_dark();

    println!("Highlighting a full chart:\n");
    println!("{}", "─".repeat(60));

    // Highlight each line
    for line in DEMO_CHART.lines() {
        let spans = Highlighter::highlight_line(line);
        let highlighted = render::to_ansi(line, &spans, &theme);
        println!("{highlighted}");
    }

    println!("{}", "─".repeat(60));
    println!();

    // Show some individual chord examples
    println!("Individual chord examples:\n");

    let chords = [
        "Gmaj7", "Am7b5", "Dm9", "Bb7#11", "Csus4", "F/A", "Em7_8", "'G",    // push notation
        "G////", // slash rhythm
        "r4",    // rest
    ];

    for chord in &chords {
        let spans = Highlighter::highlight_line(chord);
        let highlighted = render::to_ansi(chord, &spans, &theme);
        println!("  {:<12} → {highlighted}", chord);
    }

    println!();

    // Show section examples
    println!("Section marker examples:\n");

    let sections = [
        "vs 8",
        "ch 4 \"Build\"",
        "intro 2",
        "[SOLO Keys] 8",
        "Down ch 4",
    ];

    for section in &sections {
        let spans = Highlighter::highlight_line(section);
        let highlighted = render::to_ansi(section, &spans, &theme);
        println!("  {:<20} → {highlighted}", section);
    }

    println!();

    // Show metadata examples
    println!("Metadata examples:\n");

    let metadata = [
        "Song Title - Artist Name",
        "120bpm",
        "4/4",
        "#G",
        "; This is a comment",
    ];

    for meta in &metadata {
        let spans = Highlighter::highlight_line(meta);
        let highlighted = render::to_ansi(meta, &spans, &theme);
        println!("  {:<25} → {highlighted}", meta);
    }

    println!();
    println!("=== Demo Complete ===");
}
