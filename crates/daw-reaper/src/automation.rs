//! REAPER Automation/Envelope Implementation
//!
//! Implements [`daw_proto::AutomationService`] by dispatching the REAPER
//! envelope C API to the main thread via [`crate::main_thread`]. See
//! [issue #22](https://github.com/FastTrackStudios/daw/issues/22) for
//! the full API surface and rationale.
//!
//! ## Coverage
//!
//! Phase 1 (this file): full read path (`get_envelopes`, `get_envelope`,
//! `get_points`, `get_points_in_range`, `get_value_at`) and full point
//! CRUD (`add_point`, `delete_point`, `set_point`,
//! `delete_points_in_range`).
//!
//! Phase 2 (deferred): `set_visible`, `set_armed`, `set_automation_mode`,
//! `get_global_automation_override`, `set_global_automation_override` —
//! these need state-chunk parsing or REAPER APIs not exposed in the
//! pinned reaper-rs version. Stubbed below with a debug log so the
//! trait compiles and registration works.

use crate::main_thread;
use crate::safe_wrappers::envelope as env_sw;
use crate::track::{resolve_project, resolve_track};
use daw_proto::{
    AutomationService, ProjectContext,
    automation::{
        AddPointParams, Envelope, EnvelopeLocation, EnvelopePoint, EnvelopeRef, EnvelopeShape,
        EnvelopeType, SetPointParams, TimeRangeParams,
    },
    primitives::{AutomationMode, PositionInSeconds},
    track::TrackRef,
};
use reaper_high::Reaper;
use reaper_low::raw::TrackEnvelope;
use tracing::debug;

/// REAPER automation service — zero-field, all state lives in REAPER.
#[derive(Clone)]
pub struct ReaperAutomation;

impl ReaperAutomation {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ReaperAutomation {
    fn default() -> Self {
        Self::new()
    }
}

/// Map a raw REAPER shape index to the proto enum. Unknown values
/// default to `Linear`.
fn shape_from_raw(raw: i32) -> EnvelopeShape {
    match raw {
        0 => EnvelopeShape::Linear,
        1 => EnvelopeShape::Square,
        2 => EnvelopeShape::SlowStartEnd,
        3 => EnvelopeShape::FastStart,
        4 => EnvelopeShape::FastEnd,
        5 => EnvelopeShape::Bezier,
        _ => EnvelopeShape::Linear,
    }
}

fn shape_to_raw(shape: EnvelopeShape) -> i32 {
    match shape {
        EnvelopeShape::Linear => 0,
        EnvelopeShape::Square => 1,
        EnvelopeShape::SlowStartEnd => 2,
        EnvelopeShape::FastStart => 3,
        EnvelopeShape::FastEnd => 4,
        EnvelopeShape::Bezier => 5,
    }
}

/// Map an `EnvelopeType` to the chunk-name tag REAPER uses to expose
/// the envelope (e.g. `<VOLENV2`, `<PANENV2`). Used to disambiguate
/// pre-/post-FX envelopes that share a display name.
fn envelope_type_chunk_tag(ty: EnvelopeType) -> &'static str {
    match ty {
        EnvelopeType::Volume => "<VOLENV2",
        EnvelopeType::VolumePrefx => "<VOLENV",
        EnvelopeType::Pan => "<PANENV2",
        EnvelopeType::PanPrefx => "<PANENV",
        EnvelopeType::Width => "<WIDTHENV2",
        EnvelopeType::WidthPrefx => "<WIDTHENV",
        EnvelopeType::Mute => "<MUTEENV",
        EnvelopeType::FxParam => "<PARMENV",
    }
}

/// Common envelope-types we enumerate when listing all envelopes on a
/// track. FX-parameter envelopes are intentionally excluded from this
/// enumeration (they require an FX index + param scan); use a future
/// `list_fx_envelopes` once that's implemented.
const TRACK_ENVELOPE_TYPES: &[EnvelopeType] = &[
    EnvelopeType::Volume,
    EnvelopeType::VolumePrefx,
    EnvelopeType::Pan,
    EnvelopeType::PanPrefx,
    EnvelopeType::Width,
    EnvelopeType::WidthPrefx,
    EnvelopeType::Mute,
];

/// Resolve a `TrackEnvelope*` from a [`EnvelopeLocation`]. Runs on the
/// main thread; callers should already be inside a `main_thread::query`
/// or `main_thread::run` closure.
fn resolve_envelope(
    project_ctx: &ProjectContext,
    location: &EnvelopeLocation,
) -> Option<*mut TrackEnvelope> {
    let project = resolve_project(project_ctx)?;
    let track = resolve_track(&project, &location.track)?;
    let low = Reaper::get().medium_reaper().low();
    let track_ptr = track.raw().ok()?.as_ptr();

    match &location.envelope {
        EnvelopeRef::Type(ty) => {
            let tag = envelope_type_chunk_tag(*ty);
            let env = env_sw::get_track_envelope_by_chunk_name(low, track_ptr, tag);
            if !env.is_null() { Some(env) } else { None }
        }
        EnvelopeRef::ByName(name) => {
            let env = env_sw::get_track_envelope_by_name(low, track_ptr, name);
            if !env.is_null() { Some(env) } else { None }
        }
        EnvelopeRef::FxParam { .. } => {
            // FxParam envelopes need a separate API path
            // (`GetFXEnvelope`); not implemented in Phase 1.
            None
        }
    }
}

/// Build an [`Envelope`] proto struct from a track + envelope handle.
fn build_envelope(
    track_guid: &str,
    envelope_type: EnvelopeType,
    env: *mut TrackEnvelope,
) -> Envelope {
    let low = Reaper::get().medium_reaper().low();
    let name = env_sw::get_envelope_name(low, env).unwrap_or_default();
    let point_count = env_sw::count_envelope_points(low, env);
    Envelope {
        track_guid: track_guid.to_string(),
        envelope_type,
        name,
        fx_guid: None,
        param_index: None,
        // Visibility / armed / mode aren't queryable through the
        // envelope handle alone — needs a chunk parse. Phase 2.
        visible: true,
        armed: false,
        automation_mode: AutomationMode::TrimRead,
        point_count,
    }
}

impl AutomationService for ReaperAutomation {
    async fn get_envelopes(&self, project: ProjectContext, track: TrackRef) -> Vec<Envelope> {
        debug!("ReaperAutomation::get_envelopes");
        main_thread::query(move || {
            let proj = resolve_project(&project)?;
            let track_obj = resolve_track(&proj, &track)?;
            let track_guid = track_obj.guid().to_string_without_braces();
            let track_ptr = track_obj.raw().ok()?.as_ptr();
            let low = Reaper::get().medium_reaper().low();
            let mut out = Vec::new();
            for &ty in TRACK_ENVELOPE_TYPES {
                let env = env_sw::get_track_envelope_by_chunk_name(
                    low,
                    track_ptr,
                    envelope_type_chunk_tag(ty),
                );
                if !env.is_null() {
                    out.push(build_envelope(&track_guid, ty, env));
                }
            }
            Some(out)
        })
        .await
        .flatten()
        .unwrap_or_default()
    }

    async fn get_envelope(
        &self,
        project: ProjectContext,
        location: EnvelopeLocation,
    ) -> Option<Envelope> {
        debug!("ReaperAutomation::get_envelope");
        main_thread::query(move || {
            let env = resolve_envelope(&project, &location)?;
            let proj = resolve_project(&project)?;
            let track = resolve_track(&proj, &location.track)?;
            let track_guid = track.guid().to_string_without_braces();
            let ty = match &location.envelope {
                EnvelopeRef::Type(t) => *t,
                _ => EnvelopeType::Volume,
            };
            Some(build_envelope(&track_guid, ty, env))
        })
        .await
        .flatten()
    }

    async fn set_visible(
        &self,
        _project: ProjectContext,
        _location: EnvelopeLocation,
        _visible: bool,
    ) {
        debug!("ReaperAutomation::set_visible — Phase 2, not implemented");
    }

    async fn set_armed(&self, _project: ProjectContext, _location: EnvelopeLocation, _armed: bool) {
        debug!("ReaperAutomation::set_armed — Phase 2, not implemented");
    }

    async fn set_automation_mode(
        &self,
        _project: ProjectContext,
        _location: EnvelopeLocation,
        _mode: AutomationMode,
    ) {
        debug!("ReaperAutomation::set_automation_mode — Phase 2, not implemented");
    }

    async fn get_points(
        &self,
        project: ProjectContext,
        location: EnvelopeLocation,
    ) -> Vec<EnvelopePoint> {
        debug!("ReaperAutomation::get_points");
        main_thread::query(move || {
            let env = resolve_envelope(&project, &location)?;
            let low = Reaper::get().medium_reaper().low();
            let count = env_sw::count_envelope_points(low, env);
            let mut out = Vec::with_capacity(count as usize);
            for i in 0..count {
                if let Some(p) = env_sw::get_envelope_point(low, env, i) {
                    out.push(EnvelopePoint {
                        index: i,
                        time: PositionInSeconds::from_seconds(p.time),
                        value: p.value,
                        shape: shape_from_raw(p.shape),
                        tension: p.tension,
                        selected: p.selected,
                    });
                }
            }
            Some(out)
        })
        .await
        .flatten()
        .unwrap_or_default()
    }

    async fn get_points_in_range(
        &self,
        project: ProjectContext,
        location: EnvelopeLocation,
        range: TimeRangeParams,
    ) -> Vec<EnvelopePoint> {
        debug!("ReaperAutomation::get_points_in_range");
        let all = self.get_points(project, location).await;
        let start = range.start.as_seconds();
        let end = range.end.as_seconds();
        all.into_iter()
            .filter(|p| {
                let t = p.time.as_seconds();
                t >= start && t <= end
            })
            .collect()
    }

    async fn get_value_at(
        &self,
        project: ProjectContext,
        location: EnvelopeLocation,
        time: PositionInSeconds,
    ) -> f64 {
        debug!("ReaperAutomation::get_value_at");
        main_thread::query(move || {
            let env = resolve_envelope(&project, &location)?;
            let low = Reaper::get().medium_reaper().low();
            let (value, _, _, _) =
                env_sw::evaluate_envelope(low, env, time.as_seconds(), 44100.0, 1)?;
            Some(value)
        })
        .await
        .flatten()
        .unwrap_or(0.0)
    }

    async fn add_point(
        &self,
        project: ProjectContext,
        location: EnvelopeLocation,
        params: AddPointParams,
    ) -> u32 {
        debug!("ReaperAutomation::add_point");
        let inserted = main_thread::query(move || {
            let env = resolve_envelope(&project, &location)?;
            let low = Reaper::get().medium_reaper().low();
            let ok = env_sw::insert_envelope_point(
                low,
                env,
                params.time.as_seconds(),
                params.value,
                shape_to_raw(params.shape),
                0.0,
                false,
                true,
            );
            if !ok {
                return None;
            }
            // REAPER's InsertEnvelopePoint doesn't return the new
            // index; sort + scan to find the point closest to
            // `params.time`. Float equality is fragile so we pick the
            // nearest neighbour rather than asserting equality.
            let count = env_sw::count_envelope_points(low, env);
            let target = params.time.as_seconds();
            let mut best = (0u32, f64::INFINITY);
            for i in 0..count {
                if let Some(p) = env_sw::get_envelope_point(low, env, i) {
                    let d = (p.time - target).abs();
                    if d < best.1 {
                        best = (i, d);
                    }
                }
            }
            Some(best.0)
        })
        .await
        .flatten();
        inserted.unwrap_or(0)
    }

    async fn delete_point(&self, project: ProjectContext, location: EnvelopeLocation, index: u32) {
        debug!("ReaperAutomation::delete_point index={index}");
        main_thread::run(move || {
            if let Some(env) = resolve_envelope(&project, &location) {
                let low = Reaper::get().medium_reaper().low();
                let _ = env_sw::delete_envelope_point(low, env, index);
            }
        });
    }

    async fn set_point(
        &self,
        project: ProjectContext,
        location: EnvelopeLocation,
        params: SetPointParams,
    ) {
        debug!("ReaperAutomation::set_point index={}", params.index);
        main_thread::run(move || {
            if let Some(env) = resolve_envelope(&project, &location) {
                let low = Reaper::get().medium_reaper().low();
                let _ = env_sw::set_envelope_point(
                    low,
                    env,
                    params.index,
                    Some(params.time.as_seconds()),
                    Some(params.value),
                    Some(shape_to_raw(params.shape)),
                    None,
                    None,
                    true,
                );
            }
        });
    }

    async fn delete_points_in_range(
        &self,
        project: ProjectContext,
        location: EnvelopeLocation,
        range: TimeRangeParams,
    ) {
        debug!("ReaperAutomation::delete_points_in_range");
        main_thread::run(move || {
            if let Some(env) = resolve_envelope(&project, &location) {
                let low = Reaper::get().medium_reaper().low();
                let _ = env_sw::delete_envelope_points_in_range(
                    low,
                    env,
                    range.start.as_seconds(),
                    range.end.as_seconds(),
                );
            }
        });
    }

    async fn get_global_automation_override(
        &self,
        _project: ProjectContext,
    ) -> Option<AutomationMode> {
        debug!("ReaperAutomation::get_global_automation_override — Phase 2, returning None");
        None
    }

    async fn set_global_automation_override(
        &self,
        _project: ProjectContext,
        _mode: Option<AutomationMode>,
    ) {
        debug!("ReaperAutomation::set_global_automation_override — Phase 2, not implemented");
    }
}
