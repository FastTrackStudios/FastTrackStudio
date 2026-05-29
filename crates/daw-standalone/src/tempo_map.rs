//! `impl TempoMap for Standalone` — post-architect::rpc port.
//!
//! Backed by `ProjectState::tempo_points: Vec<TempoPoint>`. The old
//! ~400-line async impl with parallel state + event streams retired
//! in favor of operating directly on the canonical project state.

use std::sync::Arc;

use daw_proto::TempoMap;
use daw_proto::tempo_map::{TempoMapEvent, TempoMapStreamEvent};
use daw_proto::{DawError, DawResult, ProjectContext, TempoPoint};

use crate::sync::Standalone;
use crate::transport_engine::build_dynamic_from_time_bpm;

/// Rebuild the project's [`DynamicTempoMap`] from its `tempo_points`
/// and install it on the engine bundle. Call after any mutation that
/// changes the tempo point list. With 0 or 1 points, clears the
/// dynamic map (engine falls back to the single BPM in
/// `transport.tempo`).
fn refresh_dynamic_tempo(daw: &Standalone, guid: &str) {
    let pairs: Vec<(f64, f64)> = daw
        .with_project(guid, |p| {
            p.tempo_points
                .iter()
                .map(|pt| (pt.position_seconds(), pt.bpm))
                .collect()
        })
        .unwrap_or_default();

    let bundle = daw.transport_engine_for(guid);
    if pairs.len() < 2 {
        // Single-tempo case — let the engine's static BPM handle it.
        // Also push that single BPM (if any) into the engine.
        if let Some(&(_, bpm)) = pairs.first() {
            bundle.shared.set_tempo_bpm(bpm);
        }
        bundle.set_dynamic_tempo(None);
        return;
    }

    match build_dynamic_from_time_bpm(&pairs) {
        Ok(map) => bundle.set_dynamic_tempo(Some(Arc::new(map))),
        Err(e) => {
            tracing::warn!(
                ?e,
                project = %guid,
                "tempo-map build failed; engine keeps prior dynamic map",
            );
        }
    }
}

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

fn publish_tempo_map_event(daw: &Standalone, project_guid: &str, event: TempoMapEvent) {
    let _ = daw.tempo_map_events.send(TempoMapStreamEvent {
        project_guid: project_guid.to_string(),
        event,
    });
}

fn sort_tempo_points(points: &mut [TempoPoint]) {
    points.sort_by(|a, b| {
        a.position_seconds()
            .partial_cmp(&b.position_seconds())
            .unwrap_or(std::cmp::Ordering::Equal)
    });
}

#[derive(Clone, Copy)]
struct MusicalCursor {
    measure: i32,
    quarter_offset: f64,
    quarter_notes_per_measure: f64,
    quarter_notes_per_display_beat: f64,
}

impl MusicalCursor {
    fn new(metric: SignatureMetric) -> Self {
        Self {
            measure: 0,
            quarter_offset: 0.0,
            quarter_notes_per_measure: metric.quarter_notes_per_measure,
            quarter_notes_per_display_beat: metric.quarter_notes_per_display_beat,
        }
    }

    fn advance(&mut self, quarter_notes: f64) {
        if quarter_notes <= 0.0 {
            return;
        }
        let total = self.quarter_offset + quarter_notes;
        let measures = (total / self.quarter_notes_per_measure).floor();
        self.measure += measures as i32;
        self.quarter_offset = total - (measures * self.quarter_notes_per_measure);
        if (self.quarter_notes_per_measure - self.quarter_offset).abs() < 1e-9 {
            self.measure += 1;
            self.quarter_offset = 0.0;
        }
    }

    fn apply_time_signature(&mut self, metric: SignatureMetric) {
        if self.quarter_offset > 1e-9 {
            self.measure += 1;
            self.quarter_offset = 0.0;
        }
        self.quarter_notes_per_measure = metric.quarter_notes_per_measure;
        self.quarter_notes_per_display_beat = metric.quarter_notes_per_display_beat;
    }

    fn as_parts(self) -> (i32, i32, f64) {
        let display_beat_offset = self.quarter_offset / self.quarter_notes_per_display_beat;
        let beat_floor = display_beat_offset.floor();
        (
            self.measure + 1,
            beat_floor as i32 + 1,
            display_beat_offset - beat_floor,
        )
    }
}

#[derive(Clone, Copy)]
struct SignatureMetric {
    quarter_notes_per_measure: f64,
    quarter_notes_per_display_beat: f64,
}

fn signature_metric(time_signature: &daw_proto::TimeSignature) -> SignatureMetric {
    let numerator = time_signature.numerator.max(1) as f64;
    let denominator = time_signature.denominator.max(1) as f64;
    let quarter_notes_per_display_beat = 4.0 / denominator;
    SignatureMetric {
        quarter_notes_per_measure: numerator * quarter_notes_per_display_beat,
        quarter_notes_per_display_beat,
    }
}

fn seconds_to_musical(
    points: &[TempoPoint],
    default_bpm: f64,
    default_time_signature: &daw_proto::TimeSignature,
    target_seconds: f64,
) -> (i32, i32, f64) {
    if target_seconds <= 0.0 {
        return (1, 1, 0.0);
    }

    let mut cursor = MusicalCursor::new(signature_metric(default_time_signature));
    let mut segment_start_seconds = 0.0;
    let mut bpm = default_bpm.max(f64::EPSILON);

    for point in points {
        let point_seconds = point.position_seconds();
        if point_seconds <= 0.0 {
            bpm = point.bpm.max(f64::EPSILON);
            if let Some(ts) = &point.time_signature {
                cursor.apply_time_signature(signature_metric(ts));
            }
            continue;
        }

        let segment_end_seconds = point_seconds.min(target_seconds);
        let segment_seconds = (segment_end_seconds - segment_start_seconds).max(0.0);
        cursor.advance(segment_seconds * (bpm / 60.0));

        if target_seconds <= point_seconds {
            return cursor.as_parts();
        }

        segment_start_seconds = point_seconds;
        bpm = point.bpm.max(f64::EPSILON);
        if let Some(ts) = &point.time_signature {
            cursor.apply_time_signature(signature_metric(ts));
        }
    }

    let segment_seconds = (target_seconds - segment_start_seconds).max(0.0);
    cursor.advance(segment_seconds * (bpm / 60.0));
    cursor.as_parts()
}

fn musical_to_seconds(
    points: &[TempoPoint],
    default_bpm: f64,
    default_time_signature: &daw_proto::TimeSignature,
    measure: i32,
    beat: i32,
    fraction: f64,
) -> f64 {
    let target_measure = (measure - 1).max(0);
    let target_display_beat_offset = (beat - 1).max(0) as f64 + fraction.max(0.0);
    let mut cursor = MusicalCursor::new(signature_metric(default_time_signature));
    let mut segment_start_seconds = 0.0;
    let mut bpm = default_bpm.max(f64::EPSILON);

    for point in points {
        let point_seconds = point.position_seconds();
        if point_seconds <= 0.0 {
            bpm = point.bpm.max(f64::EPSILON);
            if let Some(ts) = &point.time_signature {
                cursor.apply_time_signature(signature_metric(ts));
            }
            continue;
        }

        let target_quarter_offset =
            target_display_beat_offset * cursor.quarter_notes_per_display_beat;
        if target_measure < cursor.measure
            || (target_measure == cursor.measure && target_quarter_offset <= cursor.quarter_offset)
        {
            return segment_start_seconds;
        }

        let segment_seconds = (point_seconds - segment_start_seconds).max(0.0);
        let segment_beats = segment_seconds * (bpm / 60.0);
        let target_quarters_from_cursor = ((target_measure - cursor.measure) as f64
            * cursor.quarter_notes_per_measure)
            + target_quarter_offset
            - cursor.quarter_offset;
        if target_quarters_from_cursor <= segment_beats + 1e-9 {
            return segment_start_seconds + (target_quarters_from_cursor.max(0.0) / (bpm / 60.0));
        }

        cursor.advance(segment_beats);
        segment_start_seconds = point_seconds;
        bpm = point.bpm.max(f64::EPSILON);
        if let Some(ts) = &point.time_signature {
            cursor.apply_time_signature(signature_metric(ts));
        }
    }

    let target_quarter_offset = target_display_beat_offset * cursor.quarter_notes_per_display_beat;
    let target_quarters_from_cursor = ((target_measure - cursor.measure) as f64
        * cursor.quarter_notes_per_measure)
        + target_quarter_offset
        - cursor.quarter_offset;
    segment_start_seconds + (target_quarters_from_cursor.max(0.0) / (bpm / 60.0))
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
                .rfind(|pt| pt.position_seconds() <= seconds)
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
                .next_back()
                .map(|ts| (ts.numerator as i32, ts.denominator as i32))
                .unwrap_or((
                    p.transport.time_signature.numerator as i32,
                    p.transport.time_signature.denominator as i32,
                ))
        })
        .unwrap_or((4, 4))
    }

    fn time_to_musical(&self, project: ProjectContext, seconds: f64) -> (i32, i32, f64) {
        let Some(guid) = resolve_project(self, &project) else {
            return (1, 1, 0.0);
        };
        self.with_project(&guid, |p| {
            seconds_to_musical(
                &p.tempo_points,
                p.transport.tempo.bpm(),
                &p.transport.time_signature,
                seconds,
            )
        })
        .unwrap_or((1, 1, 0.0))
    }

    fn musical_to_time(
        &self,
        project: ProjectContext,
        measure: i32,
        beat: i32,
        fraction: f64,
    ) -> f64 {
        let Some(guid) = resolve_project(self, &project) else {
            return 0.0;
        };
        self.with_project(&guid, |p| {
            musical_to_seconds(
                &p.tempo_points,
                p.transport.tempo.bpm(),
                &p.transport.time_signature,
                measure,
                beat,
                fraction,
            )
        })
        .unwrap_or(0.0)
    }

    fn add_tempo_point(&self, project: ProjectContext, seconds: f64, bpm: f64) -> DawResult<u32> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let (idx, point, points) = self.with_project_mut(&guid, |p| {
            let mut pt = TempoPoint::default();
            pt.position =
                daw_proto::Position::from_time(daw_proto::PositionInSeconds::from_seconds(seconds));
            pt.bpm = bpm;
            p.tempo_points.push(pt);
            sort_tempo_points(&mut p.tempo_points);
            let idx = p
                .tempo_points
                .iter()
                .position(|candidate| {
                    (candidate.position_seconds() - seconds).abs() < f64::EPSILON
                        && (candidate.bpm - bpm).abs() < f64::EPSILON
                })
                .unwrap_or(p.tempo_points.len() - 1) as u32;
            (
                idx,
                p.tempo_points[idx as usize].clone(),
                p.tempo_points.clone(),
            )
        })?;
        refresh_dynamic_tempo(self, &guid);
        publish_tempo_map_event(self, &guid, TempoMapEvent::PointAdded(point));
        publish_tempo_map_event(self, &guid, TempoMapEvent::MapChanged(points));
        Ok(idx)
    }

    fn remove_tempo_point(&self, project: ProjectContext, index: u32) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let res: DawResult<Vec<TempoPoint>> = self.with_project_mut(&guid, |p| {
            let i = index as usize;
            if i < p.tempo_points.len() {
                p.tempo_points.remove(i);
                Ok::<_, DawError>(p.tempo_points.clone())
            } else {
                Err(DawError::not_found("TempoPoint", &index.to_string()))
            }
        })?;
        if let Ok(points) = &res {
            refresh_dynamic_tempo(self, &guid);
            publish_tempo_map_event(self, &guid, TempoMapEvent::PointRemoved(index));
            publish_tempo_map_event(self, &guid, TempoMapEvent::MapChanged(points.clone()));
        }
        res.map(|_| ())
    }

    fn set_tempo_at_point(&self, project: ProjectContext, index: u32, bpm: f64) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let res: DawResult<(TempoPoint, Vec<TempoPoint>)> = self.with_project_mut(&guid, |p| {
            let i = index as usize;
            let pt = p
                .tempo_points
                .get_mut(i)
                .ok_or_else(|| DawError::not_found("TempoPoint", &index.to_string()))?;
            pt.bpm = bpm;
            Ok::<_, DawError>((pt.clone(), p.tempo_points.clone()))
        })?;
        if let Ok((point, points)) = &res {
            refresh_dynamic_tempo(self, &guid);
            publish_tempo_map_event(self, &guid, TempoMapEvent::PointChanged(point.clone()));
            publish_tempo_map_event(self, &guid, TempoMapEvent::MapChanged(points.clone()));
        }
        res.map(|_| ())
    }

    fn set_time_signature_at_point(
        &self,
        project: ProjectContext,
        index: u32,
        numerator: i32,
        denominator: i32,
    ) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let res: DawResult<(TempoPoint, Vec<TempoPoint>)> = self.with_project_mut(&guid, |p| {
            let i = index as usize;
            let pt = p
                .tempo_points
                .get_mut(i)
                .ok_or_else(|| DawError::not_found("TempoPoint", &index.to_string()))?;
            pt.time_signature = Some(daw_proto::TimeSignature::new(
                numerator as u32,
                denominator as u32,
            ));
            Ok::<_, DawError>((pt.clone(), p.tempo_points.clone()))
        })?;
        if let Ok((point, points)) = &res {
            refresh_dynamic_tempo(self, &guid);
            publish_tempo_map_event(self, &guid, TempoMapEvent::PointChanged(point.clone()));
            publish_tempo_map_event(self, &guid, TempoMapEvent::MapChanged(points.clone()));
        }
        res.map(|_| ())
    }

    fn move_tempo_point(&self, project: ProjectContext, index: u32, seconds: f64) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let res: DawResult<(TempoPoint, Vec<TempoPoint>)> = self.with_project_mut(&guid, |p| {
            let i = index as usize;
            let pt = p
                .tempo_points
                .get_mut(i)
                .ok_or_else(|| DawError::not_found("TempoPoint", &index.to_string()))?;
            pt.position =
                daw_proto::Position::from_time(daw_proto::PositionInSeconds::from_seconds(seconds));
            let changed = pt.clone();
            sort_tempo_points(&mut p.tempo_points);
            Ok::<_, DawError>((changed, p.tempo_points.clone()))
        })?;
        if let Ok((point, points)) = &res {
            refresh_dynamic_tempo(self, &guid);
            publish_tempo_map_event(self, &guid, TempoMapEvent::PointChanged(point.clone()));
            publish_tempo_map_event(self, &guid, TempoMapEvent::MapChanged(points.clone()));
        }
        res.map(|_| ())
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
        })?;
        // Mirror into the engine: refresh_dynamic_tempo handles the
        // "0 or 1 points → static" branch, otherwise the multi-point
        // map remains authoritative.
        self.transport_engine_for(&guid).shared.set_tempo_bpm(bpm);
        let points = self
            .with_project(&guid, |p| p.tempo_points.clone())
            .unwrap_or_default();
        publish_tempo_map_event(self, &guid, TempoMapEvent::MapChanged(points));
        Ok(())
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
            p.tempo_points.clone()
        })
        .map(|points| {
            publish_tempo_map_event(self, &guid, TempoMapEvent::MapChanged(points));
        })
    }

    async fn subscribe(
        &self,
        project: ProjectContext,
        tx: vox::Tx<daw_proto::tempo_map::TempoMapStreamEvent>,
    ) {
        let project_guid = resolve_project(self, &project);
        let mut rx = self.tempo_map_events.subscribe();
        moire::task::spawn(async move {
            loop {
                match rx.recv().await {
                    Ok(event) => {
                        if project_guid
                            .as_ref()
                            .is_some_and(|guid| event.project_guid != *guid)
                        {
                            continue;
                        }
                        if tx.send(event).await.is_err() {
                            return;
                        }
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Closed) => return,
                    Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => continue,
                }
            }
        });
    }
}
