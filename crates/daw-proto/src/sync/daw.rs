use crate::DawResult;

use super::{ActionRegistry, AudioEngine, PluginLoader, Project, Toolbar, WindowGeometry};

/// Root sync handle. Entry point for all sync DAW operations.
///
/// Implementations are constructed by their host crate (e.g. `ReaperMainThread::try_new()`)
/// and exposed via `daw::current()` once the architecture migration lands.
pub trait Daw {
    type Project<'a>: Project + 'a
    where
        Self: 'a;
    type ActionRegistry<'a>: ActionRegistry + 'a
    where
        Self: 'a;
    type AudioEngine<'a>: AudioEngine + 'a
    where
        Self: 'a;
    type PluginLoader<'a>: PluginLoader + 'a
    where
        Self: 'a;
    type Toolbar<'a>: Toolbar + 'a
    where
        Self: 'a;
    type WindowGeometry<'a>: WindowGeometry + 'a
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

    fn action_registry(&self) -> Self::ActionRegistry<'_>;
    fn audio_engine(&self) -> Self::AudioEngine<'_>;
    fn plugin_loader(&self) -> Self::PluginLoader<'_>;
    fn toolbar(&self) -> Self::Toolbar<'_>;
    fn window_geometry(&self) -> Self::WindowGeometry<'_>;
}
