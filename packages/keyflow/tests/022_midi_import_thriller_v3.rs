//! Test 022: MIDI Import - Thriller V3 (Dirty Loops)
//!
//! Uses the centralized `generate_chart_text` pipeline to convert a MIDI file
//! into Keyflow chart text and asserts the output matches the expected notation
//! EXACTLY — no differences allowed.

use keyflow::engraver::import::{generate_chart_text, MidiChartConfig, MidiFile};

fn load_midi() -> MidiFile {
    let bytes = include_bytes!("fixtures/thriller_dirty_loops_v3.mid");
    MidiFile::parse(bytes).expect("Failed to parse MIDI file")
}

fn generate_thriller_chart() -> String {
    let midi = load_midi();
    let config = MidiChartConfig {
        key_root: Some("Eb".to_string()),
        title: Some(
            "Thriller - Dirty Loops, Cory Wong\nTranscribed By: Cody Wright".to_string(),
        ),
    };
    generate_chart_text(&midi, &config)
}

#[test]
fn test_chart_generates_without_panic() {
    let chart_text = generate_thriller_chart();
    println!("{}", chart_text);
    assert!(!chart_text.is_empty(), "Chart text should not be empty");
}

#[test]
fn test_exact_output() {
    let chart_text = generate_thriller_chart();

    let expected = r#"Thriller - Dirty Loops, Cory Wong
Transcribed By: Cody Wright
131bpm 4/4 #Eb
/push = triplet

COUNT 2

HITS
r8t >Ab9_8t r8t r8t r8t >F9_8t r2 s1

IN
>'Cm . . .

VS
>'F/C . Cm .
'F/C . Cm .
'F/C . Cm .
'F/C . Cm Cm9

CH
>Cm/Eb / 'Eb ///  'Eb / 'F/C / 'Cm //  'F/A  'Fm9
>Cm/Eb / 'Eb ///  'Eb / 'F/C / 'Cm //  'F/A  r8t >Ab9_8t r8t r4t >'F9_8t r4t >Fm/Ab /

INST 4
Cm . F6 // Abdim7 'Csus2 // 'C5 //

VS
F/C . Cm .
'F/C . Cm .
'F/C . 'Cm .
'F/C . Cm // Gm7 // 'Abmaj7 / Abmaj7#5 / 'Db7#11/G //

CH
>Cm/Eb / 'Eb ///  'Eb / 'F/C / 'Cm //  'F/A  'Fm9
>Cm/Eb / 'Eb ///  'Eb / 'F/C / 'Cm //  'F/A  r8t >Ab9_8t r8t r4t >'F9_8t r4t >Fm/Ab /

BR
>'_4F7 . Abmaj9 //// // r8t >Abmaj9_8t r8t >Bb_8t r4t
'>Cm7 | Ebmaj7/Bb | Am7b5 | Abmaj7 |
G7sus4 | 'G7

VS
>'F/C . Cm .
'F/C . Cm Db7#11 // 'Cmaj9 //
'F7 //// // 'Fmaj7/C 'Cm7 .
'F 'Am // 'Dbmaj7 // 'Gmaj7 // 'Fm11 // 'Eb9 / Bbm/F / 'Gb7b9 //

CH 3A 10
>'Cm/Eb / 'Eb /// | 'Eb / 'F/C / 'Cm // | 'F/A //// | 'Fm9 ////
>Cm/Eb / 'Eb /// | 'Eb / 'F/C / 'Cm // | 'F/A |
r8t >Ab9_8t r8t r8t >'F9_8t r8t r4 r8t >Bb9_8t r8t
r8t r8t C7#11_8t r4 r8t Dbmaj7_8t r8t r4
'Bb11

CH 3B 8
>Cm/Eb / 'Eb /// | 'Eb / 'F/C / 'Cm // | 'F/A //// | 'Fm9 ////
>Cm/Eb / 'Eb /// | 'Eb / 'F/C / 'Cm // | 'F/A | r8t >Ab9_8t r8t r8t >'F9_8t r8t r4 >Cm

Interlude A 8
>'Cm . . .
. . . .

Interlude B 8 "HORNS"
/push 4
'Cm . 'Cm7b5 .
'Cm Cm/maj7 'B/C .

Interlude C 8 "WINDS"
C C+ // C // Cm7b5 Cmaj7
'Cmaj7 . Fm/C Cdim7

Interlude D 8 "TRUMPETS"
Fm6 . 'Dbmaj7/F .
D/F . B7/F .

Outro 8
Em7b5/D 'Dmaj9 Em7b5/D 'Dmaj7
Em7b5/D 'Dmaj9 Gm7/D 'D11sus4

Outro 8
'Gm7/D 'Dadd9 'Em7b5/D 'Dadd9
'Em7b5/D 'Dadd9 'Bbmaj7 'C6add11

Hits 4
'C#/G . . .
"#;

    assert_eq!(
        chart_text.trim(),
        expected.trim(),
        "Chart text must match EXACTLY.\n\nActual:\n{}\n\nExpected:\n{}",
        chart_text.trim(),
        expected.trim(),
    );
}
