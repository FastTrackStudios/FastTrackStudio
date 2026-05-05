//! Service for reading and transforming on-disk DAW project files.
//!
//! Lets non-CLI clients (extensions, UI, agents) consume the same `.RPP`
//! summarization and setlist-combine helpers that the daw CLI exposes,
//! without each one having to depend on `dawfile-reaper` directly.

use facet::Facet;
use vox::service;

/// Stable summary row for one track inside a parsed project file.
#[derive(Debug, Clone, Default, Facet)]
pub struct ProjectTrackSummary {
    pub name: String,
    pub item_count: u32,
    pub fx_count: u32,
}

/// Result of summarizing a project file from disk.
#[derive(Debug, Clone, Default, Facet)]
pub struct ProjectSummary {
    pub path: String,
    /// REAPER project format version (numeric).
    pub version: f64,
    /// REAPER project format version string.
    pub version_string: String,
    pub track_count: u32,
    pub marker_count: u32,
    pub region_count: u32,
    pub tracks: Vec<ProjectTrackSummary>,
    /// Empty on success; populated when the path could not be read or parsed.
    pub error: String,
}

/// Per-song info returned alongside a combined setlist.
#[derive(Debug, Clone, Default, Facet)]
pub struct SetlistSong {
    /// One-based index into the combined output.
    pub index: u32,
    pub name: String,
    pub global_start_seconds: f64,
    pub duration_seconds: f64,
}

/// Options controlling [`DawFileService::combine_setlist`].
#[derive(Debug, Clone, Default, Facet)]
pub struct CombineSetlistOptions {
    /// Bars of silence to insert between consecutive songs in the output.
    pub gap_measures: u32,
}

/// Result of combining an `.RPL` setlist into a single `.RPP`.
#[derive(Debug, Clone, Default, Facet)]
pub struct CombineSetlistResult {
    pub input: String,
    pub output: String,
    pub song_count: u32,
    pub gap_measures: u32,
    pub songs: Vec<SetlistSong>,
    pub total_seconds: f64,
    /// Empty on success.
    pub error: String,
}

/// Read-only / pure-function project file operations exposed over vox.
#[service]
pub trait DawFileService {
    /// Parse the `.RPP` at `path` and return a high-level summary.
    ///
    /// `error` is populated on failure (rather than returning a `Result`)
    /// so the caller can surface the same shape regardless of outcome.
    async fn summarize_project(&self, path: String) -> ProjectSummary;

    /// Combine an `.RPL` setlist into a single `.RPP` saved at `output`.
    /// When `output` is empty, the combined file is written next to `input`
    /// using the input's stem.
    async fn combine_setlist(
        &self,
        input: String,
        output: String,
        options: CombineSetlistOptions,
    ) -> CombineSetlistResult;
}
