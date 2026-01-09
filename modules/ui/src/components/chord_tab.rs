//! Chord Tab Display Component
//!
//! Displays chords in a tab format with 4 measures per row, following lilypond-utils rules.
//! Each measure has the same width, and displays chords, time signatures, and key changes.

use dioxus::prelude::*;
use keyflow::chart::display::format_chord;
use keyflow::chart::types::Measure;
use keyflow::chord::{Chord, ChordQuality, ChordRhythm};
use keyflow::metadata::SongMetadata;
use keyflow::primitives::{MusicalNote, RootNotation};
use keyflow::sections::{Section, SectionType};
use keyflow::time::{AbsolutePosition, MusicalDuration, MusicalPosition, TimeSignature};
use keyflow::{Chart, ChartSection, ChordInstance, Key};

/// Default chart text from well_jacob_collier.rs
const DEFAULT_CHART_TEXT: &str = r#"Well - Jacob Collier
120bpm 4/4 #E

Intro 4
1_2 2maj_2 | 4 | x2

VS 16
1_2 2maj_2 | 4 | x^

[Hits]
'1_2 '2maj_2 | '4 |

VS

[Hits]

INST 8

[SOLO Keys] 8

Br 16

br

Outro 16
"#;

/// Props for the ChordTabView component
#[derive(Props, PartialEq, Clone)]
pub struct ChordTabViewProps {
    /// Optional chart data (if None, uses parsed text or example data)
    #[props(default = None)]
    pub chart: Option<Chart>,
}

/// Display chord tab with 4 measures per row
#[component]
pub fn ChordTabView(props: ChordTabViewProps) -> Element {
    // Text editor state
    let mut chart_text = use_signal(|| DEFAULT_CHART_TEXT.to_string());
    let mut is_editing = use_signal(|| false);

    // Parse chart text in real-time
    let parsed_chart = use_memo(move || Chart::parse(&chart_text()));

    // Determine which chart to use: prop > parsed > test
    let chart = use_memo(move || {
        if let Some(chart) = &props.chart {
            Some(chart.clone())
        } else if let Ok(chart) = parsed_chart() {
            Some(chart)
        } else {
            // Fallback to test chart if parsing fails
            Some(create_test_chart())
        }
    });

    let parse_error = use_memo(move || parsed_chart().err());

    rsx! {
        div {
            class: "chord-tab-view h-full flex flex-col bg-background",
            // Header with edit toggle
            div {
                class: "flex-shrink-0 border-b border-border p-4 flex items-center justify-between",
                h1 {
                    class: "text-xl font-bold text-foreground",
                    "Chord Tab"
                }
                button {
                    class: "px-4 py-2 rounded-md bg-primary text-primary-foreground hover:bg-primary/90 font-medium text-sm",
                    onclick: move |_| is_editing.set(!is_editing()),
                    if is_editing() { "View" } else { "Edit" }
                }
            }

            // Content area
            div {
                class: "flex-1 overflow-hidden flex",
                // Text editor (left side when editing)
                if is_editing() {
                    div {
                        class: "w-1/2 border-r border-border flex flex-col",
                        div {
                            class: "flex-shrink-0 p-4 border-b border-border",
                            h2 {
                                class: "text-lg font-semibold text-foreground mb-2",
                                "Chart Text"
                            }
                            if let Some(error) = parse_error() {
                                div {
                                    class: "text-sm text-destructive bg-destructive/10 p-2 rounded mb-2",
                                    "Parse Error: {error}"
                                }
                            } else {
                                div {
                                    class: "text-sm text-muted-foreground",
                                    "Chart parsed successfully"
                                }
                            }
                        }
                        textarea {
                            class: "flex-1 p-4 font-mono text-sm bg-card border-0 resize-none focus:outline-none",
                            value: "{chart_text()}",
                            oninput: move |evt| chart_text.set(evt.value()),
                            placeholder: "Enter chart text here...",
                        }
                    }
                }

                // Chart display (right side when editing, full width when viewing)
                div {
                    class: if is_editing() { "w-1/2 overflow-y-auto p-6" } else { "flex-1 overflow-y-auto p-6" },
                    if let Some(chart_data) = chart() {
                        div {
                            class: "chord-tab-container max-w-full",
                            // Display chart metadata
                            if let Some(title) = &chart_data.metadata.title {
                                div {
                                    class: "mb-6",
                                    h1 {
                                        class: "text-2xl font-bold text-foreground",
                                        {title.clone()}
                                    }
                                    if let Some(artist) = &chart_data.metadata.artist {
                                        p {
                                            class: "text-muted-foreground",
                                            {artist.clone()}
                                        }
                                    }
                                }
                            }
                            // Display initial key and time signature
                            div {
                                class: "mb-4 flex gap-4 text-sm",
                                if let Some(key) = &chart_data.initial_key {
                                    div {
                                        class: "text-muted-foreground",
                                        "Key: "
                                        span {
                                            class: "font-semibold text-foreground",
                                            {key.short_name()}
                                        }
                                    }
                                }
                                if let Some(ts) = &chart_data.initial_time_signature {
                                    div {
                                        class: "text-muted-foreground",
                                        "Time: "
                                        span {
                                            class: "font-semibold text-foreground",
                                            "{ts.numerator}/{ts.denominator}"
                                        }
                                    }
                                }
                            }
                            // Display sections
                            for (section_idx, section) in chart_data.sections.iter().enumerate() {
                                SectionDisplay {
                                    section: section.clone(),
                                    section_index: section_idx,
                                    chart: chart_data.clone(),
                                }
                            }
                        }
                    } else {
                        div {
                            class: "flex items-center justify-center h-full",
                            div {
                                class: "text-center",
                                h2 {
                                    class: "text-xl font-semibold text-foreground mb-2",
                                    "No chart available"
                                }
                                p {
                                    class: "text-sm text-muted-foreground",
                                    "The chart data is empty or could not be loaded."
                                }
                                if let Some(error) = parse_error() {
                                    div {
                                        class: "mt-4 p-4 bg-destructive/10 rounded text-destructive text-sm",
                                        "Parse Error: {error}"
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Display a section with its measures
#[component]
fn SectionDisplay(section: ChartSection, section_index: usize, chart: Chart) -> Element {
    let section_name = section.section.display_name();

    // Calculate starting measure number (sum of all previous sections)
    let start_measure_number = {
        let mut measure_num = 1u32;
        for prev_section in chart.sections.iter().take(section_index) {
            measure_num += prev_section.measures.len() as u32;
        }
        measure_num
    };

    rsx! {
        div {
            class: "section mb-8",
            // Display measures in rows of 4 (section marker will be rendered in left margin)
            {render_section_measures(&section.measures, start_measure_number, section_index, chart.clone(), section_name.clone())}
        }
    }
}

/// Render measures for a section in rows of 4
fn render_section_measures(
    measures: &[Measure],
    start_measure: u32,
    section_index: usize,
    chart: Chart,
    section_name: String,
) -> Element {
    let rows = chunk_measures(measures, 4);

    // Calculate measure numbers for each row
    let mut measure_numbers: Vec<Vec<u32>> = Vec::new();
    let mut current_measure = start_measure;
    for row in &rows {
        let mut row_numbers = Vec::new();
        for idx in 0..row.len() {
            row_numbers.push(current_measure + idx as u32);
        }
        measure_numbers.push(row_numbers);
        current_measure += row.len() as u32;
    }

    rsx! {
        for (row_idx, row) in rows.iter().enumerate() {
            div {
                class: "measure-row flex gap-0 mb-6 w-full relative pl-16",
                // Left margin content
                div {
                    class: "absolute left-0 top-0 bottom-0 w-12 flex flex-col items-center justify-center",
                    // Section marker on first row only
                    if row_idx == 0 {
                        div {
                            class: "px-2 py-1 bg-red-500 text-white text-xs font-bold rounded border-2 border-red-600 dark:bg-red-600 dark:border-red-700 whitespace-nowrap",
                            {section_name.clone()}
                        }
                    }
                    // Measure number
                    if let Some(first_measure_num) = measure_numbers.get(row_idx).and_then(|nums| nums.first()) {
                        div {
                            class: "mt-2 text-sm font-semibold text-foreground",
                            {first_measure_num.to_string()}
                        }
                    }
                }
                for (idx_in_row, measure) in row.iter().enumerate() {
                    if let Some(measure_num) = measure_numbers.get(row_idx).and_then(|nums| nums.get(idx_in_row)) {
                        MeasureBox {
                            measure: measure.clone(),
                            measure_number: *measure_num,
                            section_index: section_index,
                            chart: chart.clone(),
                            is_first_in_row: idx_in_row == 0,
                            is_last_in_row: idx_in_row == row.len() - 1,
                        }
                    }
                }
            }
        }
    }
}

/// Display a single measure in sheet music style
#[component]
fn MeasureBox(
    measure: Measure,
    measure_number: u32,
    section_index: usize,
    chart: Chart,
    is_first_in_row: bool,
    is_last_in_row: bool,
) -> Element {
    // Check for key change at this measure
    let key_at_measure = chart
        .key_changes
        .iter()
        .find(|kc| {
            kc.position.total_duration.measure == measure_number as i32 - 1
                && kc.section_index == section_index
        })
        .map(|kc| kc.to_key.clone());

    // Use time signature from measure (which is stored as (numerator, denominator))
    // Convert to TimeSignature type
    let time_sig = TimeSignature::new(
        measure.time_signature.0 as i32,
        measure.time_signature.1 as i32,
    );
    let beats_per_measure = time_sig.numerator as usize;

    // Group chords by beat position within this measure
    // Chords are positioned absolutely, so we need to get their beat within this measure
    let mut chords_by_beat: Vec<(usize, Vec<ChordInstance>)> = Vec::new();
    for chord in &measure.chords {
        // Get the beat position (0-indexed) within the measure
        // The chord's position is absolute, so we need to extract the beat
        let beat = chord.position.total_duration.beat.max(0) as usize;
        let beat_idx = if beat < beats_per_measure {
            beat
        } else {
            beats_per_measure - 1
        };

        // Find or create entry for this beat
        if let Some(entry) = chords_by_beat.iter_mut().find(|(b, _)| *b == beat_idx) {
            entry.1.push(chord.clone());
        } else {
            chords_by_beat.push((beat_idx, vec![chord.clone()]));
        }
    }

    // Calculate beat positions as percentages
    // Position each beat evenly across the measure (0%, 25%, 50%, 75% for 4/4)
    let beat_positions: Vec<(usize, f32)> = (0..beats_per_measure)
        .map(|beat_idx| {
            // Center each beat in its division: (beat_idx + 0.5) / beats_per_measure
            let percent = ((beat_idx as f32 + 0.5) / beats_per_measure as f32) * 100.0;
            (beat_idx, percent)
        })
        .collect();

    rsx! {
        div {
            class: "measure flex-1 min-w-0 relative",
            style: "flex: 1 1 25%; max-width: 25%; min-height: 140px;",
            // Sheet music layout
            div {
                class: "flex flex-col h-full",
                // Chord lane (above staff) - positioned to align with beats
                div {
                    class: "chord-lane relative h-14 mb-1 px-2",
                    style: "min-height: 56px;",
                    for (beat_idx, percent) in &beat_positions {
                        if let Some((_, chords)) = chords_by_beat.iter().find(|(b, _)| *b == *beat_idx) {
                            div {
                                class: "absolute top-0",
                                style: format!("left: {}%; transform: translateX(-50%);", percent),
                                for chord in chords {
                                    ChordDisplay {
                                        chord: chord.clone(),
                                    }
                                }
                            }
                        }
                    }
                }

                // Staff with 5 lines and barlines (barlines only on staff, not extending to chord lane)
                div {
                    class: "staff relative flex-1 px-2",
                    style: "min-height: 60px;",
                    // Draw 5 staff lines
                    for line_num in 0..5 {
                        div {
                            class: "absolute left-0 right-0 border-t border-foreground/60",
                            style: format!("top: {}%;", (line_num as f32 * 25.0)),
                        }
                    }

                    // Left barline (only on staff, extends through all staff lines)
                    div {
                        class: "absolute left-0 top-0 bottom-0 w-px bg-foreground",
                        style: "z-index: 10;",
                    }

                    // Right barline (only on last measure of row, only on staff)
                    if is_last_in_row {
                        div {
                            class: "absolute right-0 top-0 bottom-0 w-px bg-foreground",
                            style: "z-index: 10;",
                        }
                    }

                    // Clef and key signature (only on first measure of row)
                    if is_first_in_row {
                        // Bass clef symbol - much bigger
                        div {
                            class: "absolute left-1 top-1/2 -translate-y-1/2 font-serif text-foreground z-20",
                            style: "font-family: 'Bravura', 'Gootville', serif; font-size: 3rem; line-height: 1;",
                            "𝄢"
                        }
                        // Key signature (show sharps/flats if key is present)
                        if let Some(key) = &key_at_measure.or_else(|| chart.initial_key.clone()) {
                            div {
                                class: "absolute left-12 top-1/2 -translate-y-1/2 text-base font-semibold text-foreground z-20",
                                {key.short_name()}
                            }
                        }
                    }

                    // Beat slashes - bigger and centered on middle staff line (50% = line 2 of 5)
                    for (beat_idx, percent) in &beat_positions {
                        div {
                            class: "absolute z-10",
                            style: format!("left: {}%; top: 50%; transform: translateX(-50%) translateY(-50%);", percent),
                            span {
                                class: "text-foreground font-mono leading-none",
                                style: "font-size: 1.75rem;",
                                "/"
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Display a single chord above the staff
#[component]
fn ChordDisplay(chord: ChordInstance) -> Element {
    let formatted_symbol = format_chord(&chord.full_symbol);
    rsx! {
        div {
            class: "chord-symbol text-center",
            span {
                class: "inline-block px-2 py-1 font-semibold whitespace-nowrap text-foreground",
                style: "font-family: 'Times New Roman', serif; font-size: 1.5rem; line-height: 1.2;",
                {formatted_symbol}
            }
        }
    }
}

/// Chunk measures into rows of 4
fn chunk_measures(measures: &[Measure], chunk_size: usize) -> Vec<Vec<Measure>> {
    measures
        .chunks(chunk_size)
        .map(|chunk| chunk.to_vec())
        .collect()
}

/// Create a test Chart with hardcoded data based on "Silent Night"
fn create_test_chart() -> Chart {
    use keyflow::chart::types::Measure;

    let mut chart = Chart::new();

    // Set metadata
    chart.metadata = SongMetadata {
        title: Some("Silent Night".to_string()),
        artist: None,
        ..Default::default()
    };

    // Set initial key (A major)
    let a_major = Key::major(MusicalNote::from_string("A").unwrap());
    chart.initial_key = Some(a_major.clone());
    chart.current_key = Some(a_major.clone());

    // Set initial time signature (4/4)
    let time_4_4 = TimeSignature::new(4, 4);
    chart.initial_time_signature = Some(time_4_4);
    chart.time_signature = Some(time_4_4);

    // Helper function to create a chord instance
    // We'll calculate positions manually based on section and measure indices
    let mut total_measures = 0i32;
    let create_chord = |symbol: &str,
                        section_idx: usize,
                        measure_idx: usize,
                        total_measures_before: i32|
     -> ChordInstance {
        // Extract root note from symbol (simplified - just get first letter)
        let root_str = symbol
            .chars()
            .take_while(|c| {
                c.is_alphabetic() && *c != 'm' && *c != 'a' && *c != 'd' && *c != 'u' && *c != 's'
            })
            .collect::<String>();
        let root_note = if root_str.is_empty() {
            MusicalNote::from_string("C").unwrap()
        } else {
            MusicalNote::from_string(&root_str)
                .unwrap_or_else(|| MusicalNote::from_string("C").unwrap())
        };
        let root = RootNotation::from_note_name(root_note);

        // Parse chord quality from symbol
        let quality = if symbol.contains('m') && !symbol.contains("maj") && !symbol.contains("add")
        {
            ChordQuality::Minor
        } else if symbol.contains("sus2") {
            ChordQuality::Suspended(keyflow::chord::SuspendedType::Second)
        } else if symbol.contains("sus") {
            ChordQuality::Suspended(keyflow::chord::SuspendedType::Fourth)
        } else if symbol.contains("dim") {
            ChordQuality::Diminished
        } else if symbol.contains("aug") {
            ChordQuality::Augmented
        } else {
            ChordQuality::Major
        };

        // Parse family (7th type) from symbol
        let family = if symbol.contains("maj7") || symbol.contains("M7") {
            Some(keyflow::chord::ChordFamily::Major7)
        } else if symbol.contains("m7") || symbol.contains("min7") {
            Some(keyflow::chord::ChordFamily::Minor7)
        } else if symbol.contains("7") && !symbol.contains("maj7") {
            Some(keyflow::chord::ChordFamily::Dominant7)
        } else {
            None
        };

        let parsed_chord = if let Some(fam) = family {
            Chord::with_family(root.clone(), quality, fam)
        } else {
            Chord::new(root.clone(), quality)
        };

        // Create position manually - measure is total_measures_before + measure_idx, at beat 0
        let position = AbsolutePosition::new(
            MusicalPosition::try_new(total_measures_before + measure_idx as i32, 0, 0)
                .unwrap_or(MusicalPosition::start()),
            section_idx,
        );

        ChordInstance::new(
            root,
            symbol.to_string(),
            parsed_chord,
            ChordRhythm::Default,
            symbol.to_string(),
            MusicalDuration::new(0, 0, 0),
            position,
        )
    };

    // Section 1: Count-In (1 measure)
    let mut count_in_measures = vec![Measure::new().with_time_signature((4, 4))];
    count_in_measures[0]
        .chords
        .push(create_chord("A", 0, 0, total_measures));
    chart.sections.push(
        ChartSection::new(Section::new(SectionType::Custom("Count-In".to_string())))
            .with_measures(count_in_measures),
    );
    total_measures += 1;

    // Section 2: VS 1 (12 measures)
    let mut vs1_measures = Vec::new();
    for i in 0..12 {
        let mut measure = Measure::new().with_time_signature((4, 4));

        match i {
            0 => measure.chords.push(create_chord("A", 1, i, total_measures)),
            6 => measure
                .chords
                .push(create_chord("Eadd11", 1, i, total_measures)),
            9 => measure
                .chords
                .push(create_chord("F#m", 1, i, total_measures)),
            10 => measure
                .chords
                .push(create_chord("A/C#", 1, i, total_measures)),
            12 => measure
                .chords
                .push(create_chord("Dsus2", 1, i, total_measures)),
            15 => measure.chords.push(create_chord("A", 1, i, total_measures)),
            19 => measure
                .chords
                .push(create_chord("Dsus2", 1, i, total_measures)),
            22 => measure.chords.push(create_chord("A", 1, i, total_measures)),
            _ => {}
        }
        vs1_measures.push(measure);
    }
    chart.sections.push(
        ChartSection::new(Section::new(SectionType::Verse).with_measure_count(12))
            .with_measures(vs1_measures),
    );
    total_measures += 12;

    // Section 3: INST (4 measures)
    let mut inst_measures = Vec::new();
    for i in 0..4 {
        let mut measure = Measure::new().with_time_signature((4, 4));

        match i {
            0 => measure
                .chords
                .push(create_chord("F#m", 2, i, total_measures)),
            1 => measure.chords.push(create_chord("A", 2, i, total_measures)),
            2 => measure
                .chords
                .push(create_chord("Esus4", 2, i, total_measures)),
            3 => {
                measure.chords.push(create_chord("A", 2, i, total_measures));
                measure
                    .chords
                    .push(create_chord("Asus2/B", 2, i, total_measures));
            }
            _ => {}
        }
        inst_measures.push(measure);
    }
    chart.sections.push(
        ChartSection::new(Section::new(SectionType::Custom("INST".to_string())))
            .with_measures(inst_measures),
    );
    total_measures += 4;

    // Section 4: VS 2 (12 measures)
    let mut vs2_measures = Vec::new();
    for i in 0..12 {
        let mut measure = Measure::new().with_time_signature((4, 4));

        match i {
            0 => measure
                .chords
                .push(create_chord("Asus2/B", 3, i, total_measures)),
            1 => measure
                .chords
                .push(create_chord("Esus2/F#", 3, i, total_measures)),
            2 => measure
                .chords
                .push(create_chord("Esus2/F#", 3, i, total_measures)),
            3 => measure.chords.push(create_chord("A", 3, i, total_measures)),
            4 => measure
                .chords
                .push(create_chord("Asus2/B", 3, i, total_measures)),
            5 => measure
                .chords
                .push(create_chord("Asus4/D", 3, i, total_measures)),
            6 => measure
                .chords
                .push(create_chord("Asus4/D", 3, i, total_measures)),
            7 => measure.chords.push(create_chord("A", 3, i, total_measures)),
            _ => {}
        }
        vs2_measures.push(measure);
    }
    chart.sections.push(
        ChartSection::new(Section::new(SectionType::Verse).with_measure_count(12))
            .with_measures(vs2_measures),
    );
    total_measures += 12;

    // Add a few more sections to demonstrate layout
    for section_num in 0..2 {
        let mut section_measures = Vec::new();
        for i in 0..8 {
            let mut measure = Measure::new().with_time_signature((4, 4));
            if i % 2 == 0 {
                measure
                    .chords
                    .push(create_chord("A", 4 + section_num, i, total_measures));
            }
            section_measures.push(measure);
        }
        let section_name = if section_num == 0 { "INST" } else { "VS 3" };
        chart.sections.push(
            ChartSection::new(Section::new(SectionType::Custom(section_name.to_string())))
                .with_measures(section_measures),
        );
        total_measures += 8;
    }

    chart
}
