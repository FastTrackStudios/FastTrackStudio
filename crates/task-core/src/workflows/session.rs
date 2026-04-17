//! Production sessions — work log for studio and production projects.
//!
//! Tracks writing, recording, mixing, mastering, and review sessions.
//! Stored as `sessions/YYYY-MM-DD-<type>.md` files in the project folder.

use chrono::{NaiveDate, NaiveTime};
use facet::Facet;

/// A single production session (writing, recording, mixing, etc.)
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Session {
    pub title: String,
    pub session_type: SessionType,
    pub date: Option<NaiveDate>,
    pub start_time: Option<NaiveTime>,
    pub end_time: Option<NaiveTime>,
    pub duration_minutes: Option<u32>,

    /// Who attended / participated.
    #[facet(default)]
    pub attendees: Vec<String>,

    /// What was worked on (song titles, track names, etc.)
    #[facet(default)]
    pub tracks_worked: Vec<String>,

    /// Studio / room / location.
    pub location: Option<String>,

    /// Link to DAW session file (relative path).
    pub daw_session: Option<String>,
    pub daw: Option<String>,
    pub sample_rate: Option<u32>,
    pub tempo: Option<f64>,

    #[facet(default)]
    pub notes: String,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum SessionType {
    #[default]
    Writing,
    PreProduction,
    Recording,
    Overdub,
    Editing,
    Mixing,
    Mastering,
    Review,
}

/// A tracked audio stem / asset within a project.
/// Stored as `stems.md` or per-track files.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Stem {
    pub name: String,
    pub track: Option<String>,
    pub stem_type: StemType,
    pub status: StemStatus,
    pub format: Option<String>,
    pub sample_rate: Option<u32>,
    pub bit_depth: Option<u32>,
    pub path: Option<String>,
    #[facet(default)]
    pub notes: String,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum StemType {
    #[default]
    Raw,
    Edited,
    Comped,
    Printed,
    Bounce,
    Master,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum StemStatus {
    #[default]
    Pending,
    Recorded,
    Edited,
    Mixed,
    Approved,
}
