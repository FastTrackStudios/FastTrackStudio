//! Mock data for testing the performance view without a real DAW connection

use daw_proto::{MusicalPosition, Position, PositionInSeconds};
use session_proto::setlist::{ActiveIndices, Setlist};
use session_proto::song::{Section, SectionType, Song};
use session_ui::components::{MeasureIndicator, TempoMarkerData};
use session_ui::TransportState;
use std::collections::HashMap;

/// Create mock setlist with sample songs
pub fn create_mock_setlist() -> Setlist {
    Setlist {
        id: Some("mock-setlist-1".to_string()),
        name: "Sunday Worship Set".to_string(),
        songs: vec![create_song_1(), create_song_2(), create_song_3()],
    }
}

/// Create mock active indices (currently on Song 1, Chorus section)
pub fn create_mock_active_indices() -> ActiveIndices {
    ActiveIndices {
        song_index: Some(0),
        section_index: Some(2), // Chorus
        slide_index: None,
        song_progress: Some(0.45),   // 45% through song
        section_progress: Some(0.6), // 60% through section
        is_playing: true,
        looping: false,
        loop_selection: None,
    }
}

/// Create mock transport states for all songs
pub fn create_mock_transport_states() -> HashMap<usize, TransportState> {
    let mut map = HashMap::new();

    // Song 0 transport (actively playing)
    map.insert(
        0,
        TransportState {
            position: daw_proto::Position::new(
                Some(daw_proto::MusicalPosition::new(20, 1, 0)), // Measure 20
                Some(daw_proto::TimePosition::from_seconds(78.0)), // In Chorus section (60-105)
                None,
            ),
            bpm: 120.0,
            time_sig_num: 4,
            time_sig_denom: 4,
            is_playing: true,
            is_looping: false,
            loop_region: None,
        },
    );

    map
}

/// Create mock tempo markers for the song progress bar
pub fn create_mock_tempo_markers() -> Vec<TempoMarkerData> {
    vec![
        // Initial tempo at 0%
        TempoMarkerData {
            position_percent: 0.0,
            label: "120 bpm".to_string(),
            is_tempo: true,
            is_time_sig: false,
            show_line_only: false,
        },
        // Initial time signature at 0%
        TempoMarkerData {
            position_percent: 0.0,
            label: "4/4".to_string(),
            is_tempo: false,
            is_time_sig: true,
            show_line_only: false,
        },
        // Tempo change at Bridge (56%)
        TempoMarkerData {
            position_percent: 56.0,
            label: "110".to_string(),
            is_tempo: true,
            is_time_sig: false,
            show_line_only: false,
        },
        // Time signature change at Bridge
        TempoMarkerData {
            position_percent: 56.0,
            label: "6/8".to_string(),
            is_tempo: false,
            is_time_sig: true,
            show_line_only: false,
        },
        // Tempo change back at final Chorus (69%)
        TempoMarkerData {
            position_percent: 69.0,
            label: "125".to_string(),
            is_tempo: true,
            is_time_sig: false,
            show_line_only: false,
        },
        // Time signature change back
        TempoMarkerData {
            position_percent: 69.0,
            label: "4/4".to_string(),
            is_tempo: false,
            is_time_sig: true,
            show_line_only: false,
        },
    ]
}

/// Create mock measure positions for the current section
/// This creates measure indicators for the Chorus section (60-105 seconds)
pub fn create_mock_measure_indicators() -> Vec<MeasureIndicator> {
    // Chorus is 45 seconds at 120 BPM, 4/4 time
    // Seconds per beat = 60/120 = 0.5
    // Seconds per measure = 0.5 * 4 = 2 seconds
    // Number of measures = 45 / 2 = 22.5 measures

    let section_duration = 45.0; // 105 - 60
    let bpm = 120.0;
    let beats_per_measure = 4;
    let seconds_per_beat = 60.0 / bpm;
    let seconds_per_measure = seconds_per_beat * beats_per_measure as f64;
    let num_measures = (section_duration / seconds_per_measure).ceil() as i32;

    (0..num_measures)
        .map(|i| {
            let position_percent = (i as f64 * seconds_per_measure / section_duration) * 100.0;
            MeasureIndicator {
                position_percent,
                measure_number: i + 1,
                time_signature: Some((4, 4)),
                musical_position: MusicalPosition::new(i, 0, 0),
            }
        })
        .collect()
}

fn create_song_1() -> Song {
    // Create measure positions for the entire song
    // Song is 240 seconds at 120 BPM, 4/4 time
    // Seconds per measure = 0.5 * 4 = 2 seconds
    // Number of measures = 240 / 2 = 120 measures
    let bpm = 120.0;
    let beats_per_measure = 4;
    let seconds_per_beat = 60.0 / bpm;
    let seconds_per_measure = seconds_per_beat * beats_per_measure as f64;
    let song_duration = 240.0;
    let num_measures = (song_duration / seconds_per_measure).ceil() as i32;

    let measure_positions: Vec<Position> = (0..num_measures)
        .map(|i| {
            let seconds = i as f64 * seconds_per_measure;
            Position::new(
                Some(MusicalPosition::new(i, 0, 0)),
                Some(PositionInSeconds::from_seconds(seconds)),
                None,
            )
        })
        .collect();

    Song {
        id: Some("song-1".to_string()),
        name: "How Great is Our God".to_string(),
        project_guid: "mock-project-1".to_string(),
        start_seconds: 0.0,
        end_seconds: 240.0,
        count_in_seconds: Some(4.0),
        sections: vec![
            Section {
                id: Some(1),
                name: "Intro".to_string(),
                section_type: SectionType::Intro,
                start_seconds: 0.0,
                end_seconds: 30.0,
                number: None,
                color: None,
            },
            Section {
                id: Some(2),
                name: "Verse 1".to_string(),
                section_type: SectionType::Verse,
                start_seconds: 30.0,
                end_seconds: 60.0,
                number: Some(1),
                color: None,
            },
            Section {
                id: Some(3),
                name: "Chorus".to_string(),
                section_type: SectionType::Chorus,
                start_seconds: 60.0,
                end_seconds: 105.0,
                number: None,
                color: None,
            },
            Section {
                id: Some(4),
                name: "Verse 2".to_string(),
                section_type: SectionType::Verse,
                start_seconds: 105.0,
                end_seconds: 135.0,
                number: Some(2),
                color: None,
            },
            Section {
                id: Some(5),
                name: "Bridge".to_string(),
                section_type: SectionType::Bridge,
                start_seconds: 135.0,
                end_seconds: 165.0,
                number: None,
                color: None,
            },
            Section {
                id: Some(6),
                name: "Chorus".to_string(),
                section_type: SectionType::Chorus,
                start_seconds: 165.0,
                end_seconds: 210.0,
                number: None,
                color: None,
            },
            Section {
                id: Some(7),
                name: "Outro".to_string(),
                section_type: SectionType::Outro,
                start_seconds: 210.0,
                end_seconds: 240.0,
                number: None,
                color: None,
            },
        ],
        tempo: Some(120.0),
        time_signature: Some(daw_proto::TimeSignature::new(4, 4)),
        measure_positions,
    }
}

fn create_song_2() -> Song {
    // 3/4 time at 95 BPM
    let bpm = 95.0;
    let beats_per_measure = 3;
    let seconds_per_beat = 60.0 / bpm;
    let seconds_per_measure = seconds_per_beat * beats_per_measure as f64;
    let song_duration = 180.0;
    let num_measures = (song_duration / seconds_per_measure).ceil() as i32;

    let measure_positions: Vec<Position> = (0..num_measures)
        .map(|i| {
            let seconds = i as f64 * seconds_per_measure;
            Position::new(
                Some(MusicalPosition::new(i, 0, 0)),
                Some(PositionInSeconds::from_seconds(seconds)),
                None,
            )
        })
        .collect();

    Song {
        id: Some("song-2".to_string()),
        name: "Holy, Holy, Holy".to_string(),
        project_guid: "mock-project-2".to_string(),
        start_seconds: 0.0,
        end_seconds: 180.0,
        count_in_seconds: Some(4.0),
        sections: vec![
            Section {
                id: Some(11),
                name: "Verse 1".to_string(),
                section_type: SectionType::Verse,
                start_seconds: 0.0,
                end_seconds: 45.0,
                number: Some(1),
                color: None,
            },
            Section {
                id: Some(12),
                name: "Chorus".to_string(),
                section_type: SectionType::Chorus,
                start_seconds: 45.0,
                end_seconds: 90.0,
                number: None,
                color: None,
            },
            Section {
                id: Some(13),
                name: "Verse 2".to_string(),
                section_type: SectionType::Verse,
                start_seconds: 90.0,
                end_seconds: 135.0,
                number: Some(2),
                color: None,
            },
            Section {
                id: Some(14),
                name: "Chorus".to_string(),
                section_type: SectionType::Chorus,
                start_seconds: 135.0,
                end_seconds: 180.0,
                number: None,
                color: None,
            },
        ],
        tempo: Some(95.0),
        time_signature: Some(daw_proto::TimeSignature::new(3, 4)),
        measure_positions,
    }
}

fn create_song_3() -> Song {
    // 3/4 time at 80 BPM
    let bpm = 80.0;
    let beats_per_measure = 3;
    let seconds_per_beat = 60.0 / bpm;
    let seconds_per_measure = seconds_per_beat * beats_per_measure as f64;
    let song_duration = 200.0;
    let num_measures = (song_duration / seconds_per_measure).ceil() as i32;

    let measure_positions: Vec<Position> = (0..num_measures)
        .map(|i| {
            let seconds = i as f64 * seconds_per_measure;
            Position::new(
                Some(MusicalPosition::new(i, 0, 0)),
                Some(PositionInSeconds::from_seconds(seconds)),
                None,
            )
        })
        .collect();

    Song {
        id: Some("song-3".to_string()),
        name: "Amazing Grace".to_string(),
        project_guid: "mock-project-3".to_string(),
        start_seconds: 0.0,
        end_seconds: 200.0,
        count_in_seconds: Some(4.0),
        sections: vec![
            Section {
                id: Some(21),
                name: "Intro".to_string(),
                section_type: SectionType::Intro,
                start_seconds: 0.0,
                end_seconds: 20.0,
                number: None,
                color: None,
            },
            Section {
                id: Some(22),
                name: "Verse 1".to_string(),
                section_type: SectionType::Verse,
                start_seconds: 20.0,
                end_seconds: 60.0,
                number: Some(1),
                color: None,
            },
            Section {
                id: Some(23),
                name: "Verse 2".to_string(),
                section_type: SectionType::Verse,
                start_seconds: 60.0,
                end_seconds: 100.0,
                number: Some(2),
                color: None,
            },
            Section {
                id: Some(24),
                name: "Verse 3".to_string(),
                section_type: SectionType::Verse,
                start_seconds: 100.0,
                end_seconds: 140.0,
                number: Some(3),
                color: None,
            },
            Section {
                id: Some(25),
                name: "Solo".to_string(),
                section_type: SectionType::Solo,
                start_seconds: 140.0,
                end_seconds: 170.0,
                number: None,
                color: None,
            },
            Section {
                id: Some(26),
                name: "Outro".to_string(),
                section_type: SectionType::Outro,
                start_seconds: 170.0,
                end_seconds: 200.0,
                number: None,
                color: None,
            },
        ],
        tempo: Some(80.0),
        time_signature: Some(daw_proto::TimeSignature::new(3, 4)),
        measure_positions,
    }
}
