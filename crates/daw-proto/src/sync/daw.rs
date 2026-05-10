use crate::DawResult;

use super::Project;

/// Root sync handle. Entry point for all sync DAW operations.
///
/// Implementations are constructed by their host crate (e.g. `ReaperMainThread::try_new()`)
/// and exposed via `daw::current()` once the architecture migration lands.
pub trait Daw {
    type Project<'a>: Project + 'a
    where
        Self: 'a;

    /// Handle to the currently focused project tab.
    fn current_project(&self) -> DawResult<Self::Project<'_>>;

    /// Handle to a specific project by GUID.
    fn project(&self, guid: &str) -> DawResult<Self::Project<'_>>;

    /// All open projects.
    fn projects(&self) -> Vec<crate::ProjectInfo>;

    /// Print to the REAPER console / equivalent.
    fn show_console_msg(&self, msg: &str);

    /// Last-touched FX param across the host (None if nothing touched yet).
    fn last_touched_fx(&self) -> Option<crate::LastTouchedFx>;
}
