use fasttrackaudio::{LyricLine, MusicProject, MusicProjectStep, SectionType, SongSection};

#[test]
fn workflow_steps_are_in_expected_order() {
    assert_eq!(
        MusicProject::canonical_steps(),
        [
            MusicProjectStep::Writing,
            MusicProjectStep::Recording,
            MusicProjectStep::Production,
            MusicProjectStep::Editing,
            MusicProjectStep::Mixing,
            MusicProjectStep::Mastering,
            MusicProjectStep::Approved,
            MusicProjectStep::Released,
        ]
    );
}

#[test]
fn workflow_advances_and_regresses() {
    let mut project = MusicProject::new("Test Song");
    assert_eq!(project.step, MusicProjectStep::Writing);
    assert!(project.advance_step());
    assert_eq!(project.step, MusicProjectStep::Recording);
    assert!(project.regress_step());
    assert_eq!(project.step, MusicProjectStep::Writing);
}

#[test]
fn project_can_store_keyflow_chart_and_song_sections() {
    let chart = r#"
Vienna (Live) - Billy Joel
120bpm 4/4 #Gm

vs verse 1
Gm//// A#//// F//// Gm////
[lyrics] {Gm}Slow down you {A#}crazy child
ch chorus
D#//// A#//// F//// Gm////
"#;

    let mut project = MusicProject::new("Vienna").with_artist("Billy Joel");
    project.set_chart_text(chart).expect("chart parses");
    project.add_section(
        SongSection::new("Verse 1", SectionType::Verse)
            .with_lyrics(LyricLine::parse_simple("Slow down you crazy child"))
            .with_notes("Lead vocal in octave doubles"),
    );

    assert!(project.chart.is_some());
    assert_eq!(project.chart_text.as_deref(), Some(chart.trim()));
    assert_eq!(project.sections.len(), 1);
    assert_eq!(project.sections[0].section_type, SectionType::Verse);
    assert_eq!(
        project.sections[0]
            .lyrics
            .as_ref()
            .map(LyricLine::full_text),
        Some("Slow down you crazy child".to_string())
    );
}

#[test]
fn project_exposes_lyrics_from_keyflow_chart() {
    let chart = r#"
Song Title
120bpm 4/4 #C

VS
C G Am F
[lyrics] {C}Hello {G}world {Am}from {F}keyflow
"#;

    let mut project = MusicProject::new("Song Title");
    project.set_chart_text(chart).expect("chart parses");

    let lyrics: Vec<_> = project
        .lyric_lines()
        .map(|line| {
            line.syllables
                .iter()
                .map(|s| s.text.as_str())
                .collect::<Vec<_>>()
        })
        .collect();

    assert_eq!(lyrics, vec![vec!["Hello", "world", "from", "keyflow"]]);
}
