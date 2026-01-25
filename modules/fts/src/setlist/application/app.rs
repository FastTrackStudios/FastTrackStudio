//! Application-focused helpers that operate directly on the `Setlist` struct.
//!
//! Similar to the transport module, the application layer augments the core type with
//! higher-level workflows (sample data generation, persistence helpers, etc.) without
//! introducing an additional wrapper or provider.

use crate::setlist::core::{SectionType, Setlist, SetlistError, Song};
use chrono::Utc;
use daw::marker_region::application::TempoTimePoint;
use daw::marker_region::core::Marker;
use daw::primitives::{Position, TimeSignature};
use std::path::Path;

impl Setlist {
    /// Create a new setlist with default application metadata pre-populated.
    pub fn new_with_app_metadata(name: impl Into<String>) -> Result<Self, SetlistError> {
        let mut setlist = Setlist::new(name.into())?;
        setlist.set_metadata("created_at", Utc::now().to_rfc3339());
        setlist.set_metadata("created_with", "FastTrackStudio".to_string());
        setlist.set_metadata("module_version", env!("CARGO_PKG_VERSION").to_string());
        Ok(setlist)
    }

    /// Convenience constructor that returns a "New Setlist" instance with metadata.
    pub fn default_app_setlist() -> Result<Self, SetlistError> {
        let mut setlist = Self::new_with_app_metadata("New Setlist")?;
        setlist.set_metadata("created_by", "FastTrackStudio Desktop");
        Ok(setlist)
    }

    /// Generate a sample concert setlist that can be used for demos/tests.
    pub fn sample_concert_setlist() -> Result<Self, SetlistError> {
        let mut setlist = Self::new_with_app_metadata("Sample Concert Setlist")?;
        setlist.set_metadata("venue", "Demo Venue");
        setlist.set_metadata("date", "2024-01-15");
        setlist.set_metadata("created_by", "FastTrackStudio Desktop");

        let songs = vec![
            // Song 1: Opening Song - 120 BPM, 4/4, with tempo change at bridge
            Self::build_sample_song(
                "Opening Song",
                0.0,
                180.0,
                vec![
                    ("Intro", SectionType::Intro, 0.0, 15.0),
                    ("Verse 1", SectionType::Verse, 15.0, 45.0),
                    ("Chorus 1", SectionType::Chorus, 45.0, 75.0),
                    ("Verse 2", SectionType::Verse, 75.0, 105.0),
                    ("Chorus 2", SectionType::Chorus, 105.0, 135.0),
                    ("Bridge", SectionType::Bridge, 135.0, 150.0),
                    ("Final Chorus", SectionType::Chorus, 150.0, 180.0),
                ],
                120.0,
                (4, 4),
                vec![
                    // Tempo drops at bridge (135s from song start)
                    (135.0, 100.0, None),
                    // Tempo returns at final chorus (150s from song start)
                    (150.0, 120.0, None),
                ],
            )?,
            // Song 2: Ballad - 72 BPM, 6/8
            Self::build_sample_song(
                "Ballad",
                200.0,
                320.0,
                vec![
                    ("Verse 1", SectionType::Verse, 200.0, 240.0),
                    ("Chorus", SectionType::Chorus, 240.0, 280.0),
                    ("Verse 2", SectionType::Verse, 280.0, 300.0),
                    ("Outro", SectionType::Outro, 300.0, 320.0),
                ],
                72.0,
                (6, 8),
                vec![], // No tempo changes
            )?,
            // Song 3: Rock Anthem - 140 BPM, 4/4, with time signature change at bridge
            Self::build_sample_song(
                "Rock Anthem",
                340.0,
                520.0,
                vec![
                    ("Intro", SectionType::Intro, 340.0, 360.0),
                    ("Verse 1", SectionType::Verse, 360.0, 390.0),
                    (
                        "Pre-Chorus",
                        SectionType::Pre(Box::new(SectionType::Chorus)),
                        390.0,
                        405.0,
                    ),
                    ("Chorus 1", SectionType::Chorus, 405.0, 435.0),
                    ("Interlude", SectionType::Interlude, 435.0, 450.0),
                    ("Verse 2", SectionType::Verse, 450.0, 480.0),
                    (
                        "Pre-Chorus 2",
                        SectionType::Pre(Box::new(SectionType::Chorus)),
                        480.0,
                        495.0,
                    ),
                    ("Chorus 2", SectionType::Chorus, 495.0, 510.0),
                    ("Outro", SectionType::Outro, 510.0, 520.0),
                ],
                140.0,
                (4, 4),
                vec![
                    // Time signature change at interlude (435s - 340s = 95s from song start)
                    (95.0, 140.0, Some((7, 8))),
                    // Back to 4/4 at verse 2 (450s - 340s = 110s from song start)
                    (110.0, 140.0, Some((4, 4))),
                ],
            )?,
        ];

        for song in songs {
            setlist.add_song(song)?;
        }

        Ok(setlist)
    }

    /// Persist the setlist to a JSON file on disk (after validating).
    pub fn save_to_path<P: AsRef<Path>>(&self, path: P) -> Result<(), SetlistError> {
        self.validate()?;

        let json = serde_json::to_string_pretty(self)
            .map_err(|e| SetlistError::json_error(e.to_string()))?;

        std::fs::write(path.as_ref(), json)
            .map_err(|e| SetlistError::io_error(format!("Failed to write file: {}", e)))?;

        Ok(())
    }

    /// Load a setlist from a JSON file created by `save_to_path`.
    pub fn load_from_path<P: AsRef<Path>>(path: P) -> Result<Self, SetlistError> {
        let content = std::fs::read_to_string(path.as_ref())
            .map_err(|e| SetlistError::io_error(format!("Failed to read file: {}", e)))?;

        let setlist: Setlist = serde_json::from_str(&content)
            .map_err(|e| SetlistError::json_error(format!("Failed to parse JSON: {}", e)))?;

        setlist.validate()?;
        Ok(setlist)
    }

    fn build_sample_song(
        name: &str,
        start: f64,
        end: f64,
        sections: Vec<(&str, SectionType, f64, f64)>,
        starting_tempo: f64,
        starting_time_sig: (i32, i32),
        tempo_changes: Vec<(f64, f64, Option<(i32, i32)>)>, // (position_in_song, tempo, optional_time_sig)
    ) -> Result<Song, SetlistError> {
        let mut song = Song::new(name.to_string())?;
        song.set_start_marker(Marker::from_seconds(start, "SONGSTART".to_string()));
        song.set_end_marker(Marker::from_seconds(end, "=END".to_string()));
        song.set_metadata("created_by", "sample_data");

        // Set starting tempo and time signature
        song.starting_tempo = Some(starting_tempo);
        song.starting_time_signature = Some(TimeSignature {
            numerator: starting_time_sig.0,
            denominator: starting_time_sig.1,
        });

        // Add tempo/time signature changes
        for (position_in_song, tempo, time_sig) in tempo_changes {
            let point = TempoTimePoint::new_full_from_seconds(
                position_in_song,
                tempo,
                None,                                       // shape
                time_sig.map(|(n, d)| (n, d)),              // time_signature
                None,                                       // selected
                None,                                       // bezier_tension
                None,                                       // metronome_pattern
            );
            song.tempo_time_sig_changes.push(point);
        }

        // Build measure positions based on tempo and time signature
        // Include both time position and musical position (measure number)
        let song_duration = end - start;
        let beats_per_measure = starting_time_sig.0 as f64;
        let seconds_per_beat = 60.0 / starting_tempo;
        let seconds_per_measure = seconds_per_beat * beats_per_measure;

        let mut measure_time = 0.0;
        let mut measure_number = 0i32; // 0-based internally, displayed as 1-based
        while measure_time < song_duration {
            use daw::primitives::MusicalPosition;
            let mut pos = Position::from_seconds(measure_time);
            // Set the musical position (measure is 0-based internally)
            pos.musical = MusicalPosition::new(measure_number, 0, 0);
            song.measure_positions.push(pos);
            measure_time += seconds_per_measure;
            measure_number += 1;
        }

        for (section_name, section_type, section_start, section_end) in sections {
            use crate::setlist::core::section::section_from_seconds;
            let section = section_from_seconds(
                section_type,
                section_start,
                section_end,
                section_name.to_string(),
                None,
            )?;
            song.add_section(section)?;
        }

        song.auto_number_sections();
        Ok(song)
    }
}
