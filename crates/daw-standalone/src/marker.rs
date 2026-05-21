//! `impl Markers for Standalone` — post-architect::rpc port.
//!
//! The standalone backend is single-threaded and lives in-process, so
//! its dispatcher is the current thread. All state is held in the
//! existing per-project `ProjectState::markers` map keyed by
//! `next_marker_id`.
//!
//! The 500-line async `MarkerService` impl that used to live here
//! (event streaming, mock-data scaffolding, navigation goto, range
//! queries, lane methods) was retired alongside the sync trait
//! consolidation — see the architect refactor notes. Restoring any
//! of those surfaces happens on a sibling trait, not by re-bloating
//! the canonical `Markers`.

use architect::HasDispatcher;
use architect::dispatch::CurrentThreadDispatcher;
use daw_proto::Markers;
use daw_proto::{DawError, DawResult, Marker, Position, PositionInSeconds, ProjectContext};

use crate::sync::Standalone;

impl HasDispatcher for Standalone {
    type Dispatcher = CurrentThreadDispatcher;

    fn dispatcher(&self) -> Self::Dispatcher {
        CurrentThreadDispatcher
    }
}

impl Markers for Standalone {
    fn all(&self, project: ProjectContext) -> Vec<Marker> {
        let guid = match resolve_project(self, &project) {
            Some(g) => g,
            None => return Vec::new(),
        };
        self.with_project(&guid, |p| p.markers.values().cloned().collect())
            .unwrap_or_default()
    }

    fn get(&self, project: ProjectContext, id: u32) -> Option<Marker> {
        let guid = resolve_project(self, &project)?;
        self.with_project(&guid, |p| p.markers.get(&id).cloned())
            .ok()
            .flatten()
    }

    fn count(&self, project: ProjectContext) -> u32 {
        let Some(guid) = resolve_project(self, &project) else {
            return 0;
        };
        self.with_project(&guid, |p| p.markers.len() as u32)
            .unwrap_or(0)
    }

    fn add(&self, project: ProjectContext, position: f64, name: &str) -> DawResult<u32> {
        let guid = resolve_project(self, &project)
            .ok_or_else(|| DawError::not_found("Project", "current"))?;
        self.with_project_mut(&guid, |p| {
            let id = p.next_marker_id;
            p.next_marker_id += 1;
            let marker = Marker {
                id: Some(id),
                ..Marker::new(
                    Position::from_time(PositionInSeconds::from_seconds(position)),
                    name.to_string(),
                )
            };
            p.markers.insert(id, marker);
            id
        })
    }

    fn remove(&self, project: ProjectContext, id: u32) -> DawResult<()> {
        let guid = resolve_project(self, &project)
            .ok_or_else(|| DawError::not_found("Project", "current"))?;
        self.with_project_mut(&guid, |p| {
            p.markers
                .remove(&id)
                .map(|_| ())
                .ok_or_else(|| DawError::not_found("Marker", &id.to_string()))
        })?
    }

    fn set_position(&self, project: ProjectContext, id: u32, position: f64) -> DawResult<()> {
        let guid = resolve_project(self, &project)
            .ok_or_else(|| DawError::not_found("Project", "current"))?;
        self.with_project_mut(&guid, |p| {
            let m = p
                .markers
                .get_mut(&id)
                .ok_or_else(|| DawError::not_found("Marker", &id.to_string()))?;
            m.position = Position::from_time(PositionInSeconds::from_seconds(position));
            Ok::<(), DawError>(())
        })?
    }

    fn rename(&self, project: ProjectContext, id: u32, name: &str) -> DawResult<()> {
        let guid = resolve_project(self, &project)
            .ok_or_else(|| DawError::not_found("Project", "current"))?;
        self.with_project_mut(&guid, |p| {
            let m = p
                .markers
                .get_mut(&id)
                .ok_or_else(|| DawError::not_found("Marker", &id.to_string()))?;
            m.name = name.to_string();
            Ok::<(), DawError>(())
        })?
    }

    fn set_color(&self, project: ProjectContext, id: u32, color: u32) -> DawResult<()> {
        let guid = resolve_project(self, &project)
            .ok_or_else(|| DawError::not_found("Project", "current"))?;
        self.with_project_mut(&guid, |p| {
            let m = p
                .markers
                .get_mut(&id)
                .ok_or_else(|| DawError::not_found("Marker", &id.to_string()))?;
            m.color = Some(color);
            Ok::<(), DawError>(())
        })?
    }

    fn set_lane(&self, _project: ProjectContext, _id: u32, _lane: Option<u32>) -> DawResult<()> {
        // Standalone has no lane concept yet; accept the call so
        // consumers writing portable code don't have to branch.
        Ok(())
    }

    async fn subscribe(
        &self,
        _project: ProjectContext,
        _tx: vox::Tx<daw_proto::marker::MarkerStreamEvent>,
    ) {
    }
}

/// Map a `ProjectContext` onto a concrete guid the standalone state
/// can index. `Current` resolves to the standalone's tracked current
/// project (or `None` if none was ever seeded).
fn resolve_project(daw: &Standalone, ctx: &ProjectContext) -> Option<String> {
    match ctx {
        ProjectContext::Project(guid) => Some(guid.clone()),
        ProjectContext::Current => {
            let state = daw.state.lock().ok()?;
            state.current_project_guid.clone()
        }
    }
}
