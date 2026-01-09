//! Keyflow to LilyPond Conversion
//!
//! Converts keyflow Chart structures to LilyPond notation for PDF rendering

use keyflow::chart::Chart;
use keyflow::sections::SectionType;
use keyflow::time::TimeSignature;

/// Convert a keyflow Chart to LilyPond notation
pub fn chart_to_lilypond(chart: &Chart) -> String {
    let mut output = String::new();

    // Header
    output.push_str("\\header {\n");
    if let Some(title) = &chart.metadata.title {
        output.push_str(&format!(
            "  title = \"{}\"\n",
            escape_lilypond_string(title)
        ));
    }
    if let Some(artist) = &chart.metadata.artist {
        output.push_str(&format!(
            "  composer = \"{}\"\n",
            escape_lilypond_string(artist)
        ));
    }
    output.push_str("}\n\n");

    // Score block
    output.push_str("\\score {\n");
    output.push_str("  <<\n");

    // Chord names staff
    output.push_str("  \\new ChordNames = \"chordProgression\" {\n");
    output.push_str("    \\set chordChanges = ##t\n");
    output.push_str("    \\override ChordName.font-size = #3\n");
    output.push_str("    \\override ChordName.font-name = #\"MuseJazz Text\"\n");
    output.push_str(
        "    \\override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #0.9\n",
    );
    output.push_str("    \n");
    output.push_str("    \\chordmode {\n");

    // Process sections
    for (section_idx, section) in chart.sections.iter().enumerate() {
        // Add section mark
        let section_mark = format_section_mark(&section.section, section_idx, chart);
        if !section_mark.is_empty() {
            output.push_str(&format!("      {}\n", section_mark));
        }

        // Process measures in this section
        for (measure_idx, measure) in section.measures.iter().enumerate() {
            let position = chart.calculate_position(section_idx, measure_idx);

            // Check for key change at this position
            if let Some(key_change) = chart.key_changes.iter().find(|kc| {
                kc.position.total_duration.measure == position.total_duration.measure
                    && kc.section_index == section_idx
            }) {
                output.push_str(&format!("      {}\n", key_change.to_key.to_lilypond()));
            }

            // Check for time signature change at this position
            if let Some(ts_change) = chart.time_signature_changes.iter().find(|ts| {
                ts.position.total_duration.measure == position.total_duration.measure
                    && ts.section_index == section_idx
            }) {
                output.push_str(&format!(
                    "      {}\n",
                    time_signature_to_lilypond(&ts_change.time_signature)
                ));
            }

            // Process chords in this measure
            if measure.chords.is_empty() {
                // Empty measure - use skip
                let duration = rhythm_to_lilypond_duration(&measure.time_signature, None);
                output.push_str(&format!("      s{}\n", duration));
            } else {
                // Get current key for resolving scale degrees/roman numerals
                let current_key = chart.key_at_position(&position);

                // Process each chord
                for chord in &measure.chords {
                    let chord_str = chord.to_lilypond(current_key);
                    output.push_str(&format!("      {}\n", chord_str));
                }
            }

            // Add measure separator if not last measure in section
            if measure_idx < section.measures.len() - 1 {
                output.push_str("      |\n");
            }
        }
    }

    output.push_str("    }\n");
    output.push_str("  }\n\n");

    // Staff for rhythmic notation
    output.push_str("  \\new Staff {\n");
    output.push_str("    \\clef treble\n");

    // Initial time signature
    if let Some(ts) = chart
        .initial_time_signature
        .as_ref()
        .or(chart.time_signature.as_ref())
    {
        output.push_str(&format!("    {}\n", time_signature_to_lilypond(ts)));
    } else {
        output.push_str("    \\time 4/4\n");
    }

    // Initial key
    if let Some(key) = chart.initial_key.as_ref() {
        output.push_str(&format!("    {}\n", key.to_lilypond()));
    }

    output.push_str("    \n");
    // Note: \autoInlineBreaks and \rh are custom commands from utility files
    // For now, we'll skip them since we're just using basic skips
    // output.push_str("    \\autoInlineBreaks {\n");
    // output.push_str("      \\rh{\n");
    // output.push_str("        \n");

    // Process sections for rhythmic notation
    for (section_idx, section) in chart.sections.iter().enumerate() {
        // Add section mark
        let section_mark = format_section_mark(&section.section, section_idx, chart);
        if !section_mark.is_empty() {
            output.push_str(&format!("        {}\n", section_mark));
        }

        // Process measures
        for (measure_idx, measure) in section.measures.iter().enumerate() {
            let position = chart.calculate_position(section_idx, measure_idx);

            // Check for key change
            if let Some(key_change) = chart.key_changes.iter().find(|kc| {
                kc.position.total_duration.measure == position.total_duration.measure
                    && kc.section_index == section_idx
            }) {
                output.push_str(&format!("        {}\n", key_change.to_key.to_lilypond()));
            }

            // Check for time signature change
            if let Some(ts_change) = chart.time_signature_changes.iter().find(|ts| {
                ts.position.total_duration.measure == position.total_duration.measure
                    && ts.section_index == section_idx
            }) {
                output.push_str(&format!(
                    "        {}\n",
                    time_signature_to_lilypond(&ts_change.time_signature)
                ));
            }

            // Process chords for rhythmic notation
            if measure.chords.is_empty() {
                // Empty measure
                let duration = rhythm_to_lilypond_duration(&measure.time_signature, None);
                output.push_str(&format!("        s{}\n", duration));
            } else {
                // Use skip for now - could be enhanced with actual rhythm notation
                let duration = rhythm_to_lilypond_duration(&measure.time_signature, None);
                output.push_str(&format!("        s{}\n", duration));
            }
        }
    }

    // Close the commented-out custom command blocks
    // output.push_str("      }\n");
    // output.push_str("    }\n");
    output.push_str("  }\n");
    output.push_str("  >>\n");
    output.push_str("  \n");
    output.push_str("  \\layout {\n");
    output.push_str("    indent = 0\n");
    output.push_str("    ragged-right = ##f\n");
    output.push_str("  }\n");
    output.push_str("}\n");

    output
}

/// Convert rhythm to LilyPond duration for skips/spaces
fn rhythm_to_lilypond_duration(
    time_sig: &(u8, u8),
    _rhythm: Option<&keyflow::chord::ChordRhythm>,
) -> String {
    // Default: whole note for 4/4, half note for 2/4, etc.
    match time_sig.0 {
        2 => "2".to_string(),
        3 => "2.".to_string(), // Dotted half for 3/4
        4 => "1".to_string(),  // Whole note for 4/4
        6 => "2.".to_string(), // Dotted half for 6/8
        _ => "1".to_string(),  // Default to whole note
    }
}

/// Convert time signature to LilyPond notation
fn time_signature_to_lilypond(ts: &TimeSignature) -> String {
    format!("\\time {}/{}", ts.numerator, ts.denominator)
}

/// Format section mark for LilyPond
fn format_section_mark(
    section: &keyflow::sections::Section,
    _section_idx: usize,
    _chart: &Chart,
) -> String {
    let section_name = match &section.section_type {
        SectionType::Intro => "IN".to_string(),
        SectionType::Verse => {
            if let Some(num) = section.number {
                format!("VS {}", num)
            } else {
                "VS".to_string()
            }
        }
        SectionType::Chorus => {
            if let Some(num) = section.number {
                format!("CH {}", num)
            } else {
                "CH".to_string()
            }
        }
        SectionType::Bridge => {
            if let Some(num) = section.number {
                format!("BR {}", num)
            } else {
                "BR".to_string()
            }
        }
        SectionType::Outro => "OUT".to_string(),
        SectionType::Instrumental => "INST".to_string(),
        SectionType::Pre(inner) => {
            let inner_name = match **inner {
                SectionType::Chorus => "CH",
                SectionType::Verse => "VS",
                _ => "PRE",
            };
            format!("PRE-{}", inner_name)
        }
        SectionType::Post(inner) => {
            let inner_name = match **inner {
                SectionType::Chorus => "CH",
                SectionType::Verse => "VS",
                _ => "POST",
            };
            format!("POST-{}", inner_name)
        }
        SectionType::Custom(name) => name.clone(),
    };

    format!(r#"\mark "{}""#, section_name)
}

/// Escape special characters in LilyPond strings
fn escape_lilypond_string(s: &str) -> String {
    s.replace("\\", "\\\\")
        .replace("\"", "\\\"")
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\t", "\\t")
}

/// Render a keyflow Chart to a LilyPond PDF file
///
/// # Arguments
/// * `chart` - The keyflow Chart to render
/// * `output_path` - Path where the PDF should be written (e.g., "output.pdf")
/// * `should_compile` - Whether to compile the LilyPond file to PDF
///
/// # Returns
/// * `Ok(String)` - Path to the generated PDF file
/// * `Err(String)` - Error message if rendering fails
pub fn render_chart_to_pdf(
    chart: &Chart,
    output_path: &str,
    should_compile: bool,
) -> Result<String, String> {
    use crate::lilypond_builder;

    // Convert chart to LilyPond notation
    let lilypond_content = chart_to_lilypond(chart);

    // Build complete LilyPond document with all utilities inlined
    // This eliminates path issues by directly embedding all utility content
    // output_dir should be the directory where the .ly file will be written
    let output_dir = std::path::Path::new(output_path)
        .parent()
        .and_then(|p| p.to_str())
        .unwrap_or(".");

    let complete_document =
        lilypond_builder::build_minimal_lilypond_document(&lilypond_content, output_dir);

    // Write to .ly file
    let ly_path = output_path.replace(".pdf", ".ly");
    if output_path.ends_with(".pdf") {
        // User provided .pdf, we'll create .ly and compile
    } else {
        // User provided .ly or other, use as-is
    }

    // Write and optionally compile
    lilypond_builder::write_and_compile(&complete_document, &ly_path, should_compile)
        .map_err(|e| format!("Failed to write/compile LilyPond file: {}", e))?;

    if should_compile {
        let pdf_path = ly_path.replace(".ly", ".pdf");
        Ok(pdf_path)
    } else {
        Ok(ly_path)
    }
}
