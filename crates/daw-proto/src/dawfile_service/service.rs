//! Read-only / pure-function project file operations exposed over vox.

use super::{CombineSetlistOptions, CombineSetlistResult, ProjectSummary};
use vox::service;

#[service]
pub trait DawFileService {
    /// Parse the `.RPP` at `path` and return a high-level summary.
    ///
    /// `error` is populated on failure (rather than returning a `Result`)
    /// so the caller can surface the same shape regardless of outcome.
    async fn summarize_project(&self, path: String) -> ProjectSummary;

    /// Combine an `.RPL` setlist into a single `.RPP` saved at `output`.
    /// When `output` is empty, the combined file is written next to
    /// `input` using the input's stem.
    async fn combine_setlist(
        &self,
        input: String,
        output: String,
        options: CombineSetlistOptions,
    ) -> CombineSetlistResult;
}
