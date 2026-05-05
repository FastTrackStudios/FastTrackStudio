use std::path::PathBuf;

use facet::Facet;
use thiserror::Error;

use crate::{Chart, LyricLine, MusicProjectId, MusicProjectStep, SongSection, Track};

/// Errors from music project operations.
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum MusicProjectError {
    #[error("failed to parse keyflow chart: {0}")]
    ChartParse(String),
}

/// A music project that tracks production progress plus song assets.
#[derive(Clone, Default, PartialEq, Facet)]
pub struct MusicProject {
    pub id: MusicProjectId,
    pub title: String,
    pub artist: Option<String>,
    pub step: MusicProjectStep,
    pub chart_text: Option<String>,
    pub chart: Option<Chart>,
    pub sections: Vec<SongSection>,
    pub tempo_bpm: Option<f64>,
    pub time_signature: Option<daw::service::TimeSignature>,
    pub daw_project_path: Option<PathBuf>,
    pub notes: Vec<String>,
}

impl std::fmt::Debug for MusicProject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MusicProject")
            .field("id", &self.id)
            .field("title", &self.title)
            .field("artist", &self.artist)
            .field("step", &self.step)
            .field("chart_text", &self.chart_text.as_ref().map(|s| s.len()))
            .field("chart", &self.chart.as_ref().map(|_| "parsed"))
            .field("sections", &self.sections)
            .field("tempo_bpm", &self.tempo_bpm)
            .field("time_signature", &self.time_signature)
            .field("daw_project_path", &self.daw_project_path)
            .field("notes", &self.notes)
            .finish()
    }
}

impl MusicProject {
    pub fn new(title: impl Into<String>) -> Self {
        Self {
            title: title.into(),
            ..Self::default()
        }
    }

    pub fn with_artist(mut self, artist: impl Into<String>) -> Self {
        self.artist = Some(artist.into());
        self
    }

    pub fn add_note(&mut self, note: impl Into<String>) {
        self.notes.push(note.into());
    }

    pub fn add_section(&mut self, section: SongSection) {
        self.sections.push(section);
    }

    pub fn set_tempo_bpm(&mut self, tempo_bpm: f64) {
        self.tempo_bpm = Some(tempo_bpm);
    }

    pub fn set_time_signature(&mut self, time_signature: daw::service::TimeSignature) {
        self.time_signature = Some(time_signature);
    }

    pub fn set_daw_project_path(&mut self, path: impl Into<PathBuf>) {
        self.daw_project_path = Some(path.into());
    }

    /// Parse and attach Keyflow chart text.
    ///
    /// The chart may include both chord structure and lyrics tracks; both
    /// are kept as part of the parsed keyflow `Chart`.
    pub fn set_chart_text(
        &mut self,
        chart_text: impl Into<String>,
    ) -> Result<(), MusicProjectError> {
        let chart_text = chart_text.into().trim().to_string();
        let chart = keyflow::parse(chart_text.as_str())
            .map_err(|err| MusicProjectError::ChartParse(err.to_string()))?;
        self.chart_text = Some(chart_text);
        self.chart = Some(chart);
        Ok(())
    }

    pub fn lyrics_tracks(&self) -> impl Iterator<Item = &Track> {
        self.chart
            .iter()
            .flat_map(|chart| chart.sections.iter())
            .filter_map(|section| section.lyrics_track())
    }

    pub fn lyric_lines(&self) -> impl Iterator<Item = &LyricLine> {
        self.lyrics_tracks()
            .filter_map(|track| track.lyrics.as_ref())
    }

    pub fn clear_chart(&mut self) {
        self.chart_text = None;
        self.chart = None;
    }

    pub fn advance_step(&mut self) -> bool {
        if let Some(next) = self.step.next() {
            self.step = next;
            true
        } else {
            false
        }
    }

    pub fn regress_step(&mut self) -> bool {
        if let Some(previous) = self.step.previous() {
            self.step = previous;
            true
        } else {
            false
        }
    }

    pub fn is_released(&self) -> bool {
        self.step == MusicProjectStep::Released
    }

    pub fn canonical_steps() -> [MusicProjectStep; 8] {
        MusicProjectStep::all()
    }
}
