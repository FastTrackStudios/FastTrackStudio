//! Songwriting sub-workflow — lyrics, chords, structure, revisions.
//!
//! Attaches to a song only when collaborative writing is happening.
//! Tracks lyric drafts, chord charts, song structure, and writing session notes.
//!
//! ## Storage
//! ```text
//! songs/Sunrise/
//! ├── song.md             ← SongManifest (references writing if active)
//! └── writing/
//!     ├── writing.md      ← WritingWorkflow metadata
//!     ├── lyrics-v1.md    ← Lyric drafts
//!     ├── lyrics-v2.md
//!     └── chords.md       ← Chord chart
//! ```
//!
//! Not every song needs this. A cover song or an instrumental has no writing
//! workflow. It appears when someone creates the `writing/` folder.

use chrono::NaiveDate;
use facet::Facet;

/// Songwriting sub-workflow — lives in `songs/<Title>/writing/`.
/// Only exists when the song involves collaborative writing.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct WritingWorkflow {
    /// Current status of the writing process.
    pub status: WritingStatus,
    /// Who is writing (may differ from performers/engineers).
    #[facet(default)]
    pub writers: Vec<String>,
    /// Song structure (verse, chorus, bridge, etc.)
    #[facet(default)]
    pub structure: Vec<SongSection>,
    /// Lyric revisions, newest first.
    #[facet(default)]
    pub lyric_versions: Vec<LyricVersion>,
    /// Chord chart reference.
    pub chord_chart: Option<ChordChart>,
    /// Writing session notes (separate from production sessions).
    #[facet(default)]
    pub notes: String,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum WritingStatus {
    #[default]
    InProgress,
    ReviewingLyrics,
    ReviewingStructure,
    Locked,    // structure finalized, ready for production
    Published, // lyrics released/registered
}

impl WritingStatus {
    pub fn is_locked(&self) -> bool {
        matches!(self, Self::Locked | Self::Published)
    }
}

/// A section of the song structure.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct SongSection {
    /// Section type: "Intro", "Verse 1", "Chorus", "Bridge", "Outro", etc.
    pub label: String,
    /// Duration estimate in seconds.
    pub duration_seconds: Option<u32>,
    /// Key for this section (if it modulates).
    pub key: Option<String>,
    /// Tempo for this section (if it changes).
    pub tempo: Option<u32>,
    /// Lyrics for this section.
    #[facet(default)]
    pub lyrics: String,
    /// Chords for this section (e.g. "Am | F | C | G").
    #[facet(default)]
    pub chords: String,
    /// Notes (arrangement ideas, dynamics, etc.)
    #[facet(default)]
    pub notes: String,
}

/// A versioned lyric draft.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct LyricVersion {
    pub version: u32,
    pub date: Option<NaiveDate>,
    pub author: Option<String>,
    /// The full lyrics text.
    #[facet(default)]
    pub text: String,
    /// What changed from the previous version.
    #[facet(default)]
    pub changelog: String,
    /// File path if stored as a separate .md file.
    pub file: Option<String>,
}

/// Chord chart for the song.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ChordChart {
    /// Key signature.
    pub key: String,
    /// Capo position (0 = no capo).
    pub capo: Option<u32>,
    /// Time signature (e.g. "4/4").
    pub time_signature: Option<String>,
    /// Tempo.
    pub tempo: Option<u32>,
    /// Full chord progression as text.
    /// Each line: "Section: Chord1 | Chord2 | Chord3 | Chord4"
    #[facet(default)]
    pub progression: String,
    /// File path if stored as a separate file.
    pub file: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn writing_workflow_lifecycle() {
        let mut wf = WritingWorkflow {
            status: WritingStatus::InProgress,
            writers: vec!["cody".into(), "amy".into()],
            ..Default::default()
        };

        // Define song structure
        wf.structure = vec![
            SongSection {
                label: "Intro".into(),
                duration_seconds: Some(8),
                chords: "Am | F".into(),
                ..Default::default()
            },
            SongSection {
                label: "Verse 1".into(),
                duration_seconds: Some(30),
                lyrics: "Walking down the boulevard\nCity lights are fading hard".into(),
                chords: "Am | F | C | G".into(),
                ..Default::default()
            },
            SongSection {
                label: "Chorus".into(),
                duration_seconds: Some(24),
                lyrics: "Sunrise, sunrise\nEverything will be alright".into(),
                chords: "F | C | G | Am".into(),
                ..Default::default()
            },
        ];

        // Add lyric revisions
        wf.lyric_versions.push(LyricVersion {
            version: 1,
            author: Some("cody".into()),
            text: "Walking down the boulevard...".into(),
            changelog: "Initial draft".into(),
            ..Default::default()
        });
        wf.lyric_versions.push(LyricVersion {
            version: 2,
            author: Some("amy".into()),
            text: "Walking through the boulevard...".into(),
            changelog: "Changed 'down' to 'through', rewrote bridge".into(),
            ..Default::default()
        });

        // Add chord chart
        wf.chord_chart = Some(ChordChart {
            key: "Am".into(),
            tempo: Some(120),
            time_signature: Some("4/4".into()),
            progression: "Intro: Am | F\nVerse: Am | F | C | G\nChorus: F | C | G | Am".into(),
            ..Default::default()
        });

        assert_eq!(wf.structure.len(), 3);
        assert_eq!(wf.lyric_versions.len(), 2);
        assert!(!wf.status.is_locked());

        // Lock when ready for production
        wf.status = WritingStatus::Locked;
        assert!(wf.status.is_locked());
    }

    #[test]
    fn song_without_writing() {
        // An instrumental or cover doesn't need a writing workflow
        // WritingWorkflow is None / not present — the song.md just doesn't have it
        let manifest = super::super::SongManifest {
            title: "Jazz Interlude".into(),
            key: Some("Dm".into()),
            tempo: Some(95),
            // No writing workflow — phases go straight to recording
            ..Default::default()
        };
        assert!(manifest.lyrics.is_empty());
    }
}
