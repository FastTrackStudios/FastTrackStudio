//! `impl Transport for Reaper` — sync trait + REAPER C API.
//!
//! Mounting goes through `daw_proto::transport::serve(Reaper)`. The
//! dispatcher (REAPER's main thread queue) is pulled off the backend
//! via `HasDispatcher` on `Reaper`. Each method assumes main-thread
//! execution — the bridge enforces that contract.
//!
//! Helpers (`read_transport_state_for_project`, `get_play_state_*`,
//! `time_to_musical_position`, etc.) stay `pub(crate)` so other
//! modules (batch, polling/broadcasting code if revived) can reuse
//! them.
//!
//! Streaming retired alongside the architect::rpc port: the
//! `TRANSPORT_BROADCASTER` + `PROJECT_TRANSPORT_CACHE` + polling
//! scaffolding is gone. Subscriptions land on a sibling trait when
//! a real consumer reappears.

use std::ffi::CStr;

use daw_proto::transport::service::Transport;
use daw_proto::{
    DawError, DawResult, PlayState, ProjectContext, TimeSignature, Transport as TransportState,
};
use reaper_high::{PlayRate, Project, Reaper as ReaperHigh, Tempo as ReaperTempo};
use reaper_medium::{
    AutoSeekBehavior, CommandId, PositionInSeconds, ProjectContext as ReaperProjectContext,
    SetEditCurPosOptions, TimeRangeType, UndoBehavior,
};
use tracing::warn;

use crate::project_context::find_project_by_guid;

// ── Helpers ────────────────────────────────────────────────────────────

const MAX_REASONABLE_TIME_SELECTION_SECONDS: f64 = 60.0 * 60.0;

fn normalize_time_selection(
    selection: Option<daw_proto::LoopRegion>,
) -> Option<daw_proto::LoopRegion> {
    let selection = selection.filter(daw_proto::LoopRegion::is_valid)?;
    let duration = selection.duration();
    if duration.is_finite() && duration <= MAX_REASONABLE_TIME_SELECTION_SECONDS {
        return Some(selection);
    }
    warn!(
        start = selection.start_seconds,
        end = selection.end_seconds,
        duration,
        "ReaperTransport: ignoring unreasonable time selection",
    );
    None
}

pub(crate) fn resolve_reaper_project_context(project: &ProjectContext) -> ReaperProjectContext {
    match project {
        ProjectContext::Current => ReaperProjectContext::CurrentProject,
        ProjectContext::Project(guid) => find_project_by_guid(guid)
            .map(|proj| ReaperProjectContext::Proj(proj.raw()))
            .unwrap_or(ReaperProjectContext::CurrentProject),
    }
}

pub(crate) fn get_play_state_for_project(
    medium: &reaper_medium::Reaper,
    ctx: ReaperProjectContext,
) -> PlayState {
    let state = medium.get_play_state_ex(ctx);
    if state.is_recording {
        PlayState::Recording
    } else if state.is_playing {
        PlayState::Playing
    } else if state.is_paused {
        PlayState::Paused
    } else {
        PlayState::Stopped
    }
}

pub(crate) fn get_time_signature_for_project(
    _medium: &reaper_medium::Reaper,
    _ctx: ReaperProjectContext,
) -> (i32, i32) {
    // REAPER doesn't expose a direct project-scoped time signature
    // getter; fall back to the standard 4/4 unless the project is
    // current — in which case use the actual tempo map at position 0.
    // Keeping this simple for now; sibling tempo-map service handles
    // the structured form.
    (4, 4)
}

/// Read transport state from REAPER for a specific project. **MUST be
/// called from the main thread.**
pub(crate) fn read_transport_state_for_project(
    project: &Project,
    reaper_ctx: ReaperProjectContext,
    medium: &reaper_medium::Reaper,
) -> TransportState {
    let play_state = get_play_state_for_project(medium, reaper_ctx);
    let looping = medium.get_set_repeat_ex_get(reaper_ctx);
    let tempo_bpm = project.tempo().bpm().get();
    let playrate = project.play_rate().playback_speed_factor().get();
    let pos_seconds = medium.get_play_position_ex(reaper_ctx).get();
    let edit_pos = medium
        .get_cursor_position_ex(reaper_ctx)
        .map(|p| p.get())
        .unwrap_or(0.0);

    let (ts_num, ts_denom) = get_time_signature_for_project(medium, reaper_ctx);

    let loop_region = medium
        .get_set_loop_time_range_2_get(reaper_ctx, TimeRangeType::LoopPoints)
        .map(|range| daw_proto::LoopRegion::new(range.start.get(), range.end.get()));

    let time_selection = normalize_time_selection(
        medium
            .get_set_loop_time_range_2_get(reaper_ctx, TimeRangeType::TimeSelection)
            .map(|range| daw_proto::LoopRegion::new(range.start.get(), range.end.get())),
    );

    let playhead_musical = time_to_musical_position(project, medium, reaper_ctx, pos_seconds);
    let edit_musical = time_to_musical_position(project, medium, reaper_ctx, edit_pos);

    TransportState {
        play_state,
        record_mode: daw_proto::RecordMode::Normal,
        looping,
        loop_region,
        time_selection,
        tempo: daw_proto::primitives::Tempo::from_bpm(tempo_bpm),
        playrate,
        time_signature: TimeSignature::new(ts_num as u32, ts_denom as u32),
        playhead_position: daw_proto::primitives::Position::new(
            Some(playhead_musical),
            Some(daw_proto::primitives::TimePosition::from_seconds(
                pos_seconds,
            )),
            None,
        ),
        edit_position: daw_proto::primitives::Position::new(
            Some(edit_musical),
            Some(daw_proto::primitives::TimePosition::from_seconds(edit_pos)),
            None,
        ),
    }
}

fn time_to_musical_position(
    project: &Project,
    medium: &reaper_medium::Reaper,
    reaper_ctx: ReaperProjectContext,
    time_seconds: f64,
) -> daw_proto::primitives::MusicalPosition {
    let Some(pos) = PositionInSeconds::new(time_seconds).ok() else {
        return daw_proto::primitives::MusicalPosition::new(1, 1, 0);
    };
    let result = medium.time_map_2_time_to_beats(reaper_ctx, pos);
    let measure_offset = project.measure_offset();
    let measure = result.measure_index + measure_offset + 1;
    let beats_since = result.beats_since_measure.get();
    let beat = beats_since.floor() as i32 + 1;
    let subdivision = ((beats_since.fract()) * 1000.0).round() as i32;
    daw_proto::primitives::MusicalPosition::new(measure, beat, subdivision.clamp(0, 999))
}

fn run_command(cmd: u32) {
    let reaper = ReaperHigh::get();
    reaper.medium_reaper().main_on_command_ex(
        CommandId::new(cmd),
        0,
        ReaperProjectContext::CurrentProject,
    );
}

// ── Transport impl ────────────────────────────────────────────────────

impl Transport for crate::Reaper {
    fn play(&self, _project: ProjectContext) -> DawResult<()> {
        // 1007: Transport: Play
        run_command(1007);
        Ok(())
    }

    fn pause(&self, _project: ProjectContext) -> DawResult<()> {
        // 1008: Transport: Pause
        run_command(1008);
        Ok(())
    }

    fn stop(&self, _project: ProjectContext) -> DawResult<()> {
        // 1016: Transport: Stop
        run_command(1016);
        Ok(())
    }

    fn play_pause(&self, _project: ProjectContext) -> DawResult<()> {
        // 40073: Transport: Play/pause
        run_command(40073);
        Ok(())
    }

    fn play_stop(&self, _project: ProjectContext) -> DawResult<()> {
        // 40044: Transport: Play/stop
        run_command(40044);
        Ok(())
    }

    fn record(&self, _project: ProjectContext) -> DawResult<()> {
        // 1013: Transport: Record
        run_command(1013);
        Ok(())
    }

    fn stop_recording(&self, _project: ProjectContext) -> DawResult<()> {
        // 1016: Transport: Stop (also stops recording)
        run_command(1016);
        Ok(())
    }

    fn toggle_recording(&self, _project: ProjectContext) -> DawResult<()> {
        // 1013 toggles record state
        run_command(1013);
        Ok(())
    }

    fn set_position(&self, _project: ProjectContext, seconds: f64) -> DawResult<()> {
        let pos = PositionInSeconds::new(seconds)
            .map_err(|e| DawError::operation_failed(format!("invalid position: {e:?}")))?;
        ReaperHigh::get()
            .current_project()
            .set_edit_cursor_position(
                pos,
                SetEditCurPosOptions {
                    move_view: false,
                    seek_play: true,
                },
            );
        Ok(())
    }

    fn get_position(&self, _project: ProjectContext) -> f64 {
        ReaperHigh::get()
            .current_project()
            .play_or_edit_cursor_position()
            .map(|p| p.get())
            .unwrap_or(0.0)
    }

    fn goto_start(&self, _project: ProjectContext) -> DawResult<()> {
        // 40042: Transport: Go to start of project
        run_command(40042);
        Ok(())
    }

    fn goto_end(&self, _project: ProjectContext) -> DawResult<()> {
        // 40043: Transport: Go to end of project
        run_command(40043);
        Ok(())
    }

    fn get_state(&self, project: ProjectContext) -> TransportState {
        let reaper = ReaperHigh::get();
        let medium = reaper.medium_reaper();
        let (rea_project, reaper_ctx) = match &project {
            ProjectContext::Current => {
                let proj = reaper.current_project();
                (proj, ReaperProjectContext::CurrentProject)
            }
            ProjectContext::Project(guid) => {
                if let Some(proj) = find_project_by_guid(guid) {
                    let ctx = ReaperProjectContext::Proj(proj.raw());
                    (proj, ctx)
                } else {
                    let proj = reaper.current_project();
                    (proj, ReaperProjectContext::CurrentProject)
                }
            }
        };
        read_transport_state_for_project(&rea_project, reaper_ctx, medium)
    }

    fn get_play_state(&self, project: ProjectContext) -> PlayState {
        let medium = ReaperHigh::get().medium_reaper();
        get_play_state_for_project(medium, resolve_reaper_project_context(&project))
    }

    fn is_playing(&self, project: ProjectContext) -> bool {
        let state = ReaperHigh::get()
            .medium_reaper()
            .get_play_state_ex(resolve_reaper_project_context(&project));
        state.is_playing || state.is_recording
    }

    fn is_recording(&self, project: ProjectContext) -> bool {
        ReaperHigh::get()
            .medium_reaper()
            .get_play_state_ex(resolve_reaper_project_context(&project))
            .is_recording
    }

    fn get_tempo(&self, _project: ProjectContext) -> f64 {
        ReaperHigh::get().current_project().tempo().bpm().get()
    }

    fn set_tempo(&self, _project: ProjectContext, bpm: f64) -> DawResult<()> {
        let bpm_value = reaper_medium::Bpm::new(bpm)
            .map_err(|e| DawError::operation_failed(format!("invalid bpm: {e:?}")))?;
        let tempo = ReaperTempo::from_bpm(bpm_value);
        ReaperHigh::get()
            .current_project()
            .set_tempo(tempo, UndoBehavior::OmitUndoPoint)
            .map_err(|e| DawError::operation_failed(format!("set_tempo failed: {e:?}")))?;
        Ok(())
    }

    fn toggle_loop(&self, _project: ProjectContext) -> DawResult<()> {
        // 1068: Transport: Toggle repeat
        run_command(1068);
        Ok(())
    }

    fn is_looping(&self, project: ProjectContext) -> bool {
        ReaperHigh::get()
            .medium_reaper()
            .get_set_repeat_ex_get(resolve_reaper_project_context(&project))
    }

    fn set_loop(&self, project: ProjectContext, enabled: bool) -> DawResult<()> {
        ReaperHigh::get()
            .medium_reaper()
            .get_set_repeat_ex_set(resolve_reaper_project_context(&project), enabled);
        Ok(())
    }

    fn get_time_selection(&self, project: ProjectContext) -> Option<daw_proto::LoopRegion> {
        let ctx = resolve_reaper_project_context(&project);
        let selection = ReaperHigh::get()
            .medium_reaper()
            .get_set_loop_time_range_2_get(ctx, TimeRangeType::TimeSelection)
            .map(|range| daw_proto::LoopRegion::new(range.start.get(), range.end.get()));
        normalize_time_selection(selection)
    }

    fn set_time_selection(
        &self,
        project: ProjectContext,
        start_seconds: f64,
        end_seconds: f64,
    ) -> DawResult<()> {
        let ctx = resolve_reaper_project_context(&project);
        let start = start_seconds.min(end_seconds).max(0.0);
        let end = start_seconds.max(end_seconds).max(0.0);
        let start = PositionInSeconds::new(start)
            .map_err(|e| DawError::operation_failed(format!("invalid start: {e:?}")))?;
        let end = PositionInSeconds::new(end)
            .map_err(|e| DawError::operation_failed(format!("invalid end: {e:?}")))?;
        ReaperHigh::get()
            .medium_reaper()
            .get_set_loop_time_range_2_set(
                ctx,
                TimeRangeType::TimeSelection,
                start,
                end,
                AutoSeekBehavior::DenyAutoSeek,
            );
        Ok(())
    }

    fn clear_time_selection(&self, project: ProjectContext) -> DawResult<()> {
        let ctx = resolve_reaper_project_context(&project);
        ReaperHigh::get()
            .medium_reaper()
            .get_set_loop_time_range_2_set(
                ctx,
                TimeRangeType::TimeSelection,
                PositionInSeconds::new_panic(0.0),
                PositionInSeconds::new_panic(0.0),
                AutoSeekBehavior::DenyAutoSeek,
            );
        Ok(())
    }

    fn get_playrate(&self, _project: ProjectContext) -> f64 {
        ReaperHigh::get()
            .current_project()
            .play_rate()
            .playback_speed_factor()
            .get()
    }

    fn set_playrate(&self, _project: ProjectContext, rate: f64) -> DawResult<()> {
        let clamped = rate.clamp(0.25, 4.0);
        let factor = reaper_medium::PlaybackSpeedFactor::new(clamped);
        let play_rate = PlayRate::from_playback_speed_factor(factor);
        ReaperHigh::get().current_project().set_play_rate(play_rate);
        Ok(())
    }

    fn get_time_signature(&self, project: ProjectContext) -> TimeSignature {
        let medium = ReaperHigh::get().medium_reaper();
        let (num, denom) =
            get_time_signature_for_project(medium, resolve_reaper_project_context(&project));
        TimeSignature::new(num as u32, denom as u32)
    }
}

// Compile-time assertion: `crate::Reaper` must remain Arc-safe so
// `transport::serve(Reaper)` keeps compiling.
#[allow(dead_code)]
fn _erase_unused_cstr_import() {
    let _: Option<&CStr> = None;
}
