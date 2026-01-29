//! Test 024: MIDI Import - Movin' Out (Sammy Rae & The Friends)
//!
//! Tests new features:
//! - Staccato push notation (>'.Chord) for eighth-note-only pushed chords
//! - Line breaks with | for complex sections
//! - Repeat syntax (x2, x4) for repeated patterns
//! - Solo sections (Guitar Solo, Synth Solo, Drum Solo)
//! - Sixteenth note rhythms

use keyflow::engraver::import::{generate_chart_text, MidiChartConfig, MidiFile};

fn load_midi() -> MidiFile {
    let bytes = include_bytes!("fixtures/movin_out_sammy_rae.mid");
    MidiFile::parse(bytes).expect("Failed to parse MIDI file")
}

fn generate_movin_out_chart() -> String {
    let midi = load_midi();
    let config = MidiChartConfig {
        key_root: Some("A".to_string()),
        title: Some(
            "Movin' Out - Sammy Rae & The Friends\nTranscribed By: Cody Wright".to_string(),
        ),
    };
    generate_chart_text(&midi, &config)
}

#[test]
fn test_chart_generates_without_panic() {
    let chart_text = generate_movin_out_chart();
    println!("{}", chart_text);
    assert!(!chart_text.is_empty(), "Chart text should not be empty");
}

// NOTE: This test documents the EXPECTED output after implementing new features:
// - Staccato push notation (>'.Chord)
// - Line breaks with |
// - Repeat syntax (x2, x4)
// - Solo sections
// - Sixteenth note rhythms
//
// For now, this test is SKIPPED until those features are implemented.
// Run test_chart_generates_without_panic to see current output.
#[test]
fn test_exact_output() {
    let chart_text = generate_movin_out_chart();

    let expected = r#"Movin' Out - Sammy Rae & The Friends
Transcribed By: Cody Wright
150bpm 4/4 #A

COUNT 2

IN
'F#m7 'B7 'E 'Amaj7 x2

VS 1A 8
'F#m7 'B7 'E 'Amaj7 x2

VS 1B 8
'F#m7 B7 D 'E
'F#m7 'B7 'E 'Amaj7

CH
D E C#/F 'F#m / 'Ebm //
D 'Abm7b5 'C#7

INST 4
'F#m7 'B7 'E 'Amaj7

VS
s1 r4 r4 r8t 'B7 / Ddim7 // 'Abm7b5 // 'Amaj7
'F#m7 'B7 'E 'Amaj7

VS 2B 8
F#m7 B7 D E
'F#m7 'B7 'E 'Amaj7

CH
D / D/F# / E / E7/D / C# / C#/F / F#m / F#m7/E /
D / D/F# / Abm7b5 C#7

INST 8
s1 s1 s1 s1 | 'F#m7 / 'F#7/A# // 'Bm7 / 'D6 // .'E / r4 E r8t r4t .'E/G# r4 A A // .'A r8t .'Fdim r8t F#m7_8t

VS
s1 s1 D E
'F#m7 'B7 'E 'Amaj7

CH
D E C#/F 'F#m / 'Ebm //
D 'Abm7b5 'C#7

INST 56
'F#m7 'B7 'E 'Amaj7 | 'F#m7 / 'F#7/C# // 'B7 'E // 'E/G# / A // .'tA_8t r8t 'B /
'F#/A# Bm7 E/G# C#7 / Bdim7 / | F#/A# Bm7 E/G# Ddim7 / Bdim7 /
>F#/A# Bm7 E/G# Ddim7 / Bdim7 / | Bdim7_8t F#/A# /// F#/A#_8t Bm7 /// Bm7_8t E/G# /// E/G#_8t Ddim7 // Bdim7 /
F#/A# Bm7 E/G# Ddim7 / Bdim7 / | F#/A# Bm7 E/G# Ddim7 / Bdim7 / x3
F#/A# s1 s1 s1 | s1 s1 s1 s1
"#;

    assert_eq!(
        chart_text.trim(),
        expected.trim(),
        "Chart text must match EXACTLY.\n\nActual:\n{}\n\nExpected:\n{}",
        chart_text.trim(),
        expected.trim(),
    );
}
