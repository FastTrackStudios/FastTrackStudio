//! `impl TempoMap for Standalone` — post-architect::rpc port.
//!
//! Backed by `ProjectState::tempo_points: Vec<TempoPoint>`. The old
//! ~400-line async impl with parallel state + event streams retired
//! in favor of operating directly on the canonical project state.

use daw_proto::TempoMap;
use daw_proto::{DawError, DawResult, ProjectContext, TempoPoint};

use crate::sync::Standalone;

fn resolve_project(daw: &Standalone, ctx: &ProjectContext) -> Option<String> {
    match ctx {
        ProjectContext::Project(guid) => Some(guid.clone()),
        ProjectContext::Current => {
            let state = daw.state.lock().ok()?;
            state.current_project_guid.clone()
        }
    }
}

fn not_found_proj() -> DawError {
    DawError::not_found("Project", "context")
}

impl TempoMap for Standalone {
    fn get_tempo_points(&self, project: ProjectContext) -> Vec<TempoPoint> {
        let Some(guid) = resolve_project(self, &project) else {
            return Vec::new();
        };
        self.with_project(&guid, |p| p.tempo_points.clone())
            .unwrap_or_default()
    }

    fn get_tempo_point(&self, project: ProjectContext, index: u32) -> Option<TempoPoint> {
        let guid = resolve_project(self, &project)?;
        self.with_project(&guid, |p| p.tempo_points.get(index as usize).cloned())
            .ok()
            .flatten()
    }

    fn tempo_point_count(&self, project: ProjectContext) -> u32 {
        let Some(guid) = resolve_project(self, &project) else {
            return 0;
        };
        self.with_project(&guid, |p| p.tempo_points.len() as u32)
            .unwrap_or(0)
    }

    fn get_tempo_at(&self, project: ProjectContext, seconds: f64) -> f64 {
        let Some(guid) = resolve_project(self, &project) else {
            return 120.0;
        };
        self.with_project(&guid, |p| {
            p.tempo_points
                .iter()
                .filter(|pt| pt.position_seconds() <= seconds)
                .last()
                .map(|pt| pt.bpm)
                .unwrap_or_else(|| p.transport.tempo.bpm())
        })
        .unwrap_or(120.0)
    }

    fn get_time_signature_at(&self, project: ProjectContext, seconds: f64) -> (i32, i32) {
        let Some(guid) = resolve_project(self, &project) else {
            return (4, 4);
        };
        self.with_project(&guid, |p| {
            p.tempo_points
                .iter()
                .filter(|pt| pt.position_seconds() <= seconds)
                .filter_map(|pt| pt.time_signature.as_ref())
                .last()
                .map(|ts| (ts.numerator as i32, ts.denominator as i32))
                .unwrap_or((
                    p.transport.time_signature.numerator as i32,
                    p.transport.time_signature.denominator as i32,
                ))
        })
        .unwrap_or((4, 4))
    }

    fn time_to_musical(&self, project: ProjectContext, seconds: f64) -> (i32, i32, f64) {
        let bpm = <Self as TempoMap>::get_tempo_at(self, project.clone(), seconds);
        let (num, _denom) = <Self as TempoMap>::get_time_signature_at(self, project, seconds);
        let beats_per_second = bpm / 60.0;
        let total_beats = seconds * beats_per_second;
        let measure = (total_beats / num as f64).floor() as i32 + 1;
        let beat = (total_beats % num as f64).floor() as i32 + 1;
        let fraction = (total_beats % 1.0) - (total_beats % 1.0).floor();
        (measure, beat, fraction)
    }

    fn musical_to_time(
        &self,
        project: ProjectContext,
        measure: i32,
        beat: i32,
        fraction: f64,
    ) -> f64 {
        let bpm = <Self as TempoMap>::get_tempo_at(self, project.clone(), 0.0);
        let (num, _denom) = <Self as TempoMap>::get_time_signature_at(self, project, 0.0);
        let total_beats =
            ((measure - 1).max(0) as f64) * (num as f64) + ((beat - 1).max(0) as f64) + fraction;
        let beats_per_second = bpm / 60.0;
        total_beats / beats_per_second
    }

    fn add_tempo_point(&self, project: ProjectContext, seconds: f64, bpm: f64) -> DawResult<u32> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let idx = p.tempo_points.len() as u32;
            let mut pt = TempoPoint::default();
            pt.position =
                daw_proto::Position::from_time(daw_proto::PositionInSeconds::from_seconds(seconds));
            pt.bpm = bpm;
            p.tempo_points.push(pt);
            p.tempo_points.sort_by(|a, b| {
                a.position_seconds()
                    .partial_cmp(&b.position_seconds())
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
            idx
        })
    }

    fn remove_tempo_point(&self, project: ProjectContext, index: u32) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = index as usize;
            if i < p.tempo_points.len() {
                p.tempo_points.remove(i);
                Ok::<(), DawError>(())
            } else {
                Err(DawError::not_found("TempoPoint", &index.to_string()))
            }
        })?
    }

    fn set_tempo_at_point(&self, project: ProjectContext, index: u32, bpm: f64) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = index as usize;
            let pt = p
                .tempo_points
                .get_mut(i)
                .ok_or_else(|| DawError::not_found("TempoPoint", &index.to_string()))?;
            pt.bpm = bpm;
            Ok::<(), DawError>(())
        })?
    }

    fn set_time_signature_at_point(
        &self,
        project: ProjectContext,
        index: u32,
        numerator: i32,
        denominator: i32,
    ) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = index as usize;
            let pt = p
                .tempo_points
                .get_mut(i)
                .ok_or_else(|| DawError::not_found("TempoPoint", &index.to_string()))?;
            pt.time_signature = Some(daw_proto::TimeSignature::new(
                numerator as u32,
                denominator as u32,
            ));
            Ok::<(), DawError>(())
        })?
    }

    fn move_tempo_point(&self, project: ProjectContext, index: u32, seconds: f64) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let i = index as usize;
            let pt = p
                .tempo_points
                .get_mut(i)
                .ok_or_else(|| DawError::not_found("TempoPoint", &index.to_string()))?;
            pt.position =
                daw_proto::Position::from_time(daw_proto::PositionInSeconds::from_seconds(seconds));
            p.tempo_points.sort_by(|a, b| {
                a.position_seconds()
                    .partial_cmp(&b.position_seconds())
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
            Ok::<(), DawError>(())
        })?
    }

    fn get_default_tempo(&self, project: ProjectContext) -> f64 {
        let Some(guid) = resolve_project(self, &project) else {
            return 120.0;
        };
        self.with_project(&guid, |p| p.transport.tempo.bpm())
            .unwrap_or(120.0)
    }

    fn set_default_tempo(&self, project: ProjectContext, bpm: f64) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.tempo = daw_proto::Tempo::from_bpm(bpm);
        })
    }

    fn get_default_time_signature(&self, project: ProjectContext) -> (i32, i32) {
        let Some(guid) = resolve_project(self, &project) else {
            return (4, 4);
        };
        self.with_project(&guid, |p| {
            (
                p.transport.time_signature.numerator as i32,
                p.transport.time_signature.denominator as i32,
            )
        })
        .unwrap_or((4, 4))
    }

    fn set_default_time_signature(
        &self,
        project: ProjectContext,
        numerator: i32,
        denominator: i32,
    ) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.time_signature =
                daw_proto::TimeSignature::new(numerator as u32, denominator as u32);
        })
    }
}
