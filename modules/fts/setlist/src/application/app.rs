//! Application-focused helpers that operate directly on the `Setlist` struct.
//!
//! Similar to the transport module, the application layer augments the core type with
//! higher-level workflows (sample data generation, persistence helpers, etc.) without
//! introducing an additional wrapper or provider.

use crate::core::{Section, SectionType, Setlist, SetlistError, Song};
use chrono::Utc;
use marker_region::core::Marker;
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
            )?,
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
            )?,
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
                    ("Verse 2", SectionType::Verse, 435.0, 465.0),
                    (
                        "Pre-Chorus 2",
                        SectionType::Pre(Box::new(SectionType::Chorus)),
                        465.0,
                        480.0,
                    ),
                    ("Chorus 2", SectionType::Chorus, 480.0, 510.0),
                    ("Outro", SectionType::Outro, 510.0, 520.0),
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
    ) -> Result<Song, SetlistError> {
        let mut song = Song::new(name.to_string())?;
        song.set_start_marker(Marker::from_seconds(start, "SONGSTART".to_string()));
        song.set_end_marker(Marker::from_seconds(end, "=END".to_string()));
        song.set_metadata("created_by", "sample_data");

        for (section_name, section_type, section_start, section_end) in sections {
            let section = Section::from_seconds(
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
