//! Debug CLI for Keyflow chart layout inspection.
//!
//! A diagnostic tool for inspecting chart parsing and layout results.
//! Useful for finding layout bugs, verifying chord-notehead alignment,
//! and understanding measure structure.
//!
//! # Usage
//!
//! ```bash
//! # Parse a single line of chart syntax
//! cargo run -p keyflow --bin debug-keyflow --features engraver -- "Cm/Eb / 'Eb // | 'Eb / 'F/C /"
//!
//! # Interactive mode (reads from stdin)
//! cargo run -p keyflow --bin debug-keyflow --features engraver
//!
//! # Show detailed segment info
//! cargo run -p keyflow --bin debug-keyflow --features engraver -- --segments "Cm 'F/C"
//!
//! # Show scene tree structure
//! cargo run -p keyflow --bin debug-keyflow --features engraver -- --tree "'F/C . | Cm ."
//! ```

use std::io::{self, BufRead, Write};
use std::sync::Arc;

use keyflow::Chart;
use keyflow::engraver::fonts::SMuFLFont;
use keyflow::engraver::layout::chart::rhythm_builder::{
    RhythmBuildConfig, RhythmSource, build_rhythm, measure_has_explicit_chord_rhythm,
};
use keyflow::engraver::layout::chart::{ChartLayoutEngine, LayoutMode};
use keyflow::engraver::notation::RhythmEntry;
use keyflow::engraver::scene::node::SceneNode;
use keyflow::engraver::style::MStyle;

// Font paths (same as snippet_viewer)
const SMUFL_FONT_PATH: &str = "libs/reference/sheet-music/musescore/fonts/bravura/Bravura.otf";
const SMUFL_METADATA_PATH: &str =
    "libs/reference/sheet-music/musescore/fonts/bravura/bravura_metadata.json";
const TEXT_FONT_PATH: &str = "libs/reference/sheet-music/musescore/fonts/FreeSans.ttf";
const MUSEJAZZ_FONT_PATH: &str =
    "libs/reference/sheet-music/musescore/fonts/musejazz/MuseJazzText.otf";

/// CLI configuration
struct Config {
    /// Show detailed segment information
    show_segments: bool,
    /// Show scene tree structure
    show_tree: bool,
    /// Show rhythm builder details
    show_rhythm: bool,
    /// Chart input (if provided on command line)
    input: Option<String>,
    /// File path to read chart from
    file_path: Option<String>,
    /// Read from stdin
    read_stdin: bool,
    /// Page width for layout
    page_width: f64,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            show_segments: false,
            show_tree: false,
            show_rhythm: false,
            input: None,
            file_path: None,
            read_stdin: false,
            page_width: 612.0, // 8.5" in points
        }
    }
}

fn parse_args() -> Config {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut config = Config::default();
    let mut positional = Vec::new();

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--segments" | "-s" => config.show_segments = true,
            "--tree" | "-t" => config.show_tree = true,
            "--rhythm" | "-r" => config.show_rhythm = true,
            "--all" | "-a" => {
                config.show_segments = true;
                config.show_tree = true;
                config.show_rhythm = true;
            }
            "--width" | "-w" => {
                i += 1;
                if i < args.len() {
                    config.page_width = args[i].parse().unwrap_or(612.0);
                }
            }
            "--file" | "-f" => {
                i += 1;
                if i < args.len() {
                    config.file_path = Some(args[i].clone());
                }
            }
            "--stdin" => config.read_stdin = true,
            "--help" | "-h" => {
                print_help();
                std::process::exit(0);
            }
            arg if !arg.starts_with('-') => {
                positional.push(arg.to_string());
            }
            _ => {
                eprintln!("Unknown option: {}", args[i]);
            }
        }
        i += 1;
    }

    if !positional.is_empty() {
        config.input = Some(positional.join(" "));
    }

    config
}

fn print_help() {
    println!(
        r#"debug-keyflow - Keyflow chart layout diagnostic tool

USAGE:
    debug-keyflow [OPTIONS] [CHART_INPUT]

OPTIONS:
    -s, --segments    Show detailed segment information (x positions, widths, ticks)
    -t, --tree        Show scene tree structure
    -r, --rhythm      Show rhythm builder details
    -a, --all         Show all debug information
    -w, --width <W>   Set page width in points (default: 612.0)
    -f, --file <F>    Read chart from file (supports multi-line)
    --stdin           Read full chart from stdin (supports multi-line)
    -h, --help        Show this help message

EXAMPLES:
    # Parse a measure with push chords
    debug-keyflow "Cm/Eb / 'Eb //"

    # Show detailed segment info
    debug-keyflow -s "'F/C . | Cm ."

    # Read from a file (multi-line support)
    debug-keyflow -f mychart.txt

    # Read from stdin (pipe or heredoc)
    echo -e "Count 2: | |\nVS: Cm | Fm" | debug-keyflow --stdin

    # Interactive mode (type charts, press Enter)
    debug-keyflow

    # Full debug output
    debug-keyflow -a "Cm 'F/C | 'Eb Fm"

CHART SYNTAX:
    Cm          - C minor chord
    Cm/Eb       - C minor with E-flat bass
    'Cm         - Push (anticipate) the chord
    / or .      - Repeat previous chord
    |           - Measure separator
    ////        - Four slashes (4 beats)
    Count 2:    - Count-in section with 2 measures
    VS:         - Verse section
"#
    );
}

/// Load fonts for layout engine
fn load_fonts() -> Result<LayoutFonts, String> {
    let font_data: &'static [u8] = Box::leak(
        std::fs::read(SMUFL_FONT_PATH)
            .map_err(|e| format!("Failed to load Bravura font: {e}"))?
            .into_boxed_slice(),
    );

    let metadata_file = std::fs::File::open(SMUFL_METADATA_PATH)
        .map_err(|e| format!("Failed to load Bravura metadata: {e}"))?;

    let smufl_font: &'static SMuFLFont<'static> = Box::leak(Box::new(
        SMuFLFont::from_reader(font_data, metadata_file)
            .map_err(|e| format!("Failed to parse SMuFL font: {e}"))?,
    ));

    let text_font_data = Arc::new(
        std::fs::read(TEXT_FONT_PATH).map_err(|e| format!("Failed to load text font: {e}"))?,
    );

    let musejazz_font_data = Arc::new(
        std::fs::read(MUSEJAZZ_FONT_PATH)
            .map_err(|e| format!("Failed to load MuseJazz font: {e}"))?,
    );

    Ok(LayoutFonts {
        _smufl_font: smufl_font,
        text_font_data,
        musejazz_font_data,
    })
}

struct LayoutFonts {
    _smufl_font: &'static SMuFLFont<'static>,
    text_font_data: Arc<Vec<u8>>,
    musejazz_font_data: Arc<Vec<u8>>,
}

/// Wrap input in a minimal chart structure for parsing
fn wrap_as_chart(input: &str) -> String {
    // Check if input already has section markers (case-insensitive check for common sections)
    let input_lower = input.to_lowercase();
    let has_section_marker = input.contains("\nVS")
        || input.contains("\nCH")
        || input.contains("\nCount")
        || input.contains("\ncount")
        || input.starts_with("VS")
        || input.starts_with("CH")
        || input.starts_with("IN")
        || input.starts_with("Count")
        || input.starts_with("count")
        || input_lower.starts_with("verse")
        || input_lower.starts_with("chorus")
        || input_lower.starts_with("intro");

    if has_section_marker {
        // Already has structure, add minimal header if needed
        if !input.lines().any(|l| l.contains("bpm")) {
            format!("Debug Chart\n120bpm 4/4\n\n{input}")
        } else {
            input.to_string()
        }
    } else {
        // Wrap single line/measures in a VS section
        format!("Debug Chart\n120bpm 4/4\n\nVS\n{input}")
    }
}

/// Analyze and print chart information
fn analyze_chart(input: &str, config: &Config, fonts: &LayoutFonts) {
    let chart_source = wrap_as_chart(input);

    #[cfg(debug_assertions)]
    {
        eprintln!("[debug] Wrapped chart source:");
        eprintln!("---BEGIN---");
        eprintln!("{}", chart_source);
        eprintln!("---END---");
    }

    // Parse the chart
    let chart = match Chart::parse(&chart_source) {
        Ok(c) => c,
        Err(e) => {
            println!("Parse error: {e}");
            return;
        }
    };

    println!("\n{}", "=".repeat(70));
    println!("INPUT: {input}");
    println!("{}", "=".repeat(70));

    // Print parsed structure
    println!("\n## Parsed Structure");
    println!(
        "Title: {}",
        chart.metadata.title.as_deref().unwrap_or("Untitled")
    );
    if let Some(tempo) = &chart.tempo {
        println!("Tempo: {} bpm", tempo.bpm);
    }
    if let Some(ts) = &chart.time_signature {
        println!("Time: {}/{}", ts.numerator, ts.denominator);
    }
    if let Some(key) = &chart.initial_key {
        println!("Key: {key:?}");
    }
    println!("Sections: {}", chart.sections.len());

    // Get default time signature for rhythm building
    let time_sig = chart
        .time_signature
        .map(|ts| (ts.numerator as u8, ts.denominator as u8))
        .unwrap_or((4, 4));

    // Analyze each section
    for (sec_idx, section) in chart.sections.iter().enumerate() {
        let measures = section.measures();
        println!(
            "\n### Section {} - {:?} ({} measures)",
            sec_idx,
            section.section.section_type,
            measures.len()
        );

        for (m_idx, measure) in measures.iter().enumerate() {
            println!("\n  Measure {}:", m_idx);

            // Chords
            if !measure.chords.is_empty() {
                print!("    Chords: ");
                for (c_idx, chord) in measure.chords.iter().enumerate() {
                    let push_marker = if let Some((is_push, amount)) = &chord.push_pull {
                        if *is_push {
                            format!("'{:?}", amount.base)
                        } else {
                            format!(",{:?}", amount.base)
                        }
                    } else {
                        String::new()
                    };
                    if c_idx > 0 {
                        print!(", ");
                    }
                    print!("[{}]{}{}", c_idx, push_marker, chord.full_symbol);
                }
                println!();
            }

            // Rhythm elements
            if !measure.rhythm_elements.is_empty() {
                print!("    RhythmElements: ");
                for (r_idx, elem) in measure.rhythm_elements.iter().enumerate() {
                    if r_idx > 0 {
                        print!(", ");
                    }
                    match elem {
                        keyflow::chart::types::RhythmElement::Chord(c) => {
                            print!("C({})", c.full_symbol);
                        }
                        keyflow::chart::types::RhythmElement::Rest(r) => {
                            print!("R({:?})", r.rhythm);
                        }
                        keyflow::chart::types::RhythmElement::Space(s) => {
                            print!("S({:?})", s.rhythm);
                        }
                    }
                }
                println!();
            }

            // Show rhythm builder details if requested
            if config.show_rhythm {
                print_rhythm_details(measure, time_sig);
            }
        }
    }

    // Run layout engine
    let style: &'static MStyle = Box::leak(Box::new(MStyle::default()));
    let engine = ChartLayoutEngine::new(
        style,
        fonts.text_font_data.clone(),
        fonts.musejazz_font_data.clone(),
    );

    let layout_mode = LayoutMode::Paginated {
        page_width: config.page_width,
        page_height: 792.0,
    };

    let layout = engine.layout_chart(&chart, &layout_mode);

    println!("\n## Layout Result");
    println!(
        "Total size: {:.1} x {:.1} pts",
        layout.total_width, layout.total_height
    );
    println!("Pages: {}", layout.pages.len());
    println!("Beat positions: {}", layout.beat_positions.len());

    // Show detailed segment info if requested
    if config.show_segments {
        println!("\n## Beat Positions (segment details)");
        for (i, bp) in layout.beat_positions.iter().enumerate() {
            println!(
                "  [{}] page={} sys={} measure={} beat={} | x={:.1} width={:.1} | time={:.3}s",
                i, bp.page, bp.system, bp.measure, bp.beat, bp.x, bp.width, bp.time_start
            );
        }
    }

    // Show scene tree if requested
    if config.show_tree {
        println!("\n## Scene Tree");
        print_scene_tree(&layout.scene, 0, 3); // Max depth of 3
    }

    println!();
}

/// Print rhythm builder details for a measure
fn print_rhythm_details(measure: &keyflow::chart::types::Measure, time_sig: (u8, u8)) {
    let has_explicit = measure_has_explicit_chord_rhythm(measure);
    println!("    Rhythm: explicit={}", has_explicit);

    let source = if has_explicit {
        RhythmSource::ExplicitRhythm(&measure.rhythm_elements)
    } else {
        RhythmSource::SlashNotation {
            chords: &measure.chords,
            spillbacks: None,
        }
    };

    let config = RhythmBuildConfig {
        time_signature: time_sig,
        use_stems: true,
        auto_rhythm_slashes: false,
    };

    let result = build_rhythm(source, &config);

    print!("    Entries: ");
    for (i, entry) in result.entries.iter().enumerate() {
        if i > 0 {
            print!(", ");
        }
        match entry {
            RhythmEntry::Note(d) => {
                print!("N({:?})", d.kind);
            }
            RhythmEntry::Rest(d) => {
                print!("R({:?})", d.kind);
            }
        }
    }
    println!();

    println!(
        "    Ticks: {} | Triplets: {} | Pushes: {}",
        result.total_ticks,
        result.tuplet_specs.len(),
        result.internal_push_positions.len()
    );
}

/// Print scene tree structure (abbreviated)
fn print_scene_tree(node: &SceneNode, depth: usize, max_depth: usize) {
    if depth > max_depth {
        return;
    }

    let indent = "  ".repeat(depth);
    let id_str = format!("{:?}", node.id);
    let child_count = node.children.len();
    let transform = if node.transform == kurbo::Affine::IDENTITY {
        String::new()
    } else {
        let coeffs = node.transform.as_coeffs();
        format!(" @ ({:.1}, {:.1})", coeffs[4], coeffs[5])
    };

    println!("{}{}{} [{}ch]", indent, id_str, transform, child_count);

    for child in &node.children {
        print_scene_tree(child, depth + 1, max_depth);
    }
}

fn main() {
    let config = parse_args();

    // Load fonts
    let fonts = match load_fonts() {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Error loading fonts: {e}");
            eprintln!("Make sure you're running from the workspace root.");
            std::process::exit(1);
        }
    };

    // Read from file if specified
    if let Some(ref path) = config.file_path {
        match std::fs::read_to_string(path) {
            Ok(content) => {
                analyze_chart(&content, &config, &fonts);
                return;
            }
            Err(e) => {
                eprintln!("Error reading file '{}': {}", path, e);
                std::process::exit(1);
            }
        }
    }

    // Read all from stdin if --stdin flag is set
    if config.read_stdin {
        let stdin = io::stdin();
        let mut content = String::new();
        for line in stdin.lock().lines() {
            if let Ok(l) = line {
                content.push_str(&l);
                content.push('\n');
            }
        }
        if !content.trim().is_empty() {
            analyze_chart(&content, &config, &fonts);
        }
        return;
    }

    if let Some(ref input) = config.input {
        // Analyze provided input
        analyze_chart(input, &config, &fonts);
    } else {
        // Interactive mode
        println!("debug-keyflow - Interactive mode");
        println!("Enter chart syntax (Ctrl+D to exit):");
        println!("  Example: Cm/Eb / 'Eb // | 'Eb / 'F/C /");
        println!();

        let stdin = io::stdin();
        print!("> ");
        io::stdout().flush().unwrap();

        for line in stdin.lock().lines() {
            match line {
                Ok(input) if !input.trim().is_empty() => {
                    analyze_chart(&input, &config, &fonts);
                }
                Ok(_) => {} // Empty line
                Err(_) => break,
            }
            print!("> ");
            io::stdout().flush().unwrap();
        }
        println!("\nBye!");
    }
}
