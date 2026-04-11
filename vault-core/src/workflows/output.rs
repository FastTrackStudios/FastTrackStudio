//! Project outputs — versioned deliverables with feedback and approval.
//!
//! An output is a finished artifact from a project (final mix, master,
//! album artwork, video edit) that other projects can reference.
//!
//! Stored as `outputs/outputs.md` within a project folder.

use chrono::NaiveDate;
use facet::Facet;

/// Versioned output history for a project.
/// Stored as `outputs/outputs.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct OutputManifest {
    pub title: String,
    /// Path to the parent project (relative).
    pub project: String,
    /// Current active version number.
    pub current_version: u32,
    /// All output versions.
    #[facet(default)]
    pub outputs: Vec<Output>,
}

/// A single versioned output file.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Output {
    pub version: u32,
    /// Filename relative to the outputs/ directory.
    pub file: String,
    pub date: Option<NaiveDate>,
    pub status: OutputStatus,
    #[facet(default)]
    pub notes: String,
    /// Who approved this version (if approved).
    pub approved_by: Option<String>,
    /// Feedback from reviewers.
    #[facet(default)]
    pub feedback: Vec<Feedback>,
    /// File format metadata.
    pub format: Option<String>,
    pub sample_rate: Option<u32>,
    pub bit_depth: Option<u32>,
    pub duration_seconds: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum OutputStatus {
    #[default]
    Draft,
    Review,
    ChangesRequested,
    Approved,
    Superseded,
    Rejected,
}

/// Feedback on an output version.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Feedback {
    pub from: String,
    pub comment: String,
    pub timestamp: Option<NaiveDate>,
    /// Optional timecode reference (e.g. "1:23" for audio comment).
    pub timecode: Option<String>,
    pub resolved: bool,
}

// ── Project Link ─────────────────────────────────────────────────────────────

/// A reference from one project to another project's output.
/// Used in setlists, event performances, etc.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ProjectLink {
    /// Path to the linked project folder.
    pub project: String,
    /// Path to the specific output file (within the project's outputs/).
    pub output: Option<String>,
    /// Which version of the output.
    pub version: Option<u32>,
    /// Display title (may differ from project title).
    pub title: Option<String>,
}

// ── Release Metadata ─────────────────────────────────────────────────────────

/// Metadata for a released track/album. Stored in the song's `project.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ReleaseMetadata {
    pub isrc: Option<String>,
    pub upc: Option<String>,
    pub iswc: Option<String>,
    /// Release date.
    pub release_date: Option<NaiveDate>,
    /// Distribution platform (DistroKid, TuneCore, etc.).
    pub distributor: Option<String>,
    /// Credits.
    #[facet(default)]
    pub credits: Vec<Credit>,
    /// Lyrics.
    #[facet(default)]
    pub lyrics: String,
    /// Genre tags.
    #[facet(default)]
    pub genres: Vec<String>,
}

/// A credit entry for a release.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Credit {
    pub name: String,
    pub role: String,
    /// e.g. "ASCAP", "BMI"
    pub pro: Option<String>,
    pub ipi: Option<String>,
    pub split_percent: Option<f64>,
}
