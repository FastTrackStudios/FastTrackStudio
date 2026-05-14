//! Dispatch logic — routes each BatchOp variant to the corresponding service call.

use super::resolve::{resolve_fx_chain_arg, resolve_project_arg, resolve_track_arg};
use daw_proto::batch::*;
use daw_proto::*;

/// Dispatch a single batch operation, returning its output.
///
/// This is called on an async runtime thread. Each service impl internally
/// dispatches to REAPER's main thread via `main_thread::query()`.
#[allow(clippy::too_many_arguments)]
pub async fn dispatch_op(
    op: &BatchOp,
    outputs: &[Option<StepOutput>],
    project_svc: &crate::ReaperProject,
    transport_svc: &crate::Reaper,
    track_svc: &crate::Reaper,
    fx_svc: &crate::ReaperFx,
    routing_svc: &crate::ReaperRouting,
    item_svc: &crate::ReaperItem,
    take_svc: &crate::ReaperTake,
    marker_svc: &crate::Reaper,
    region_svc: &crate::Reaper,
    tempo_map_svc: &crate::Reaper,
    midi_svc: &crate::ReaperMidi,
    live_midi_svc: &crate::ReaperLiveMidi,
    ext_state_svc: &crate::ReaperExtState,
    audio_engine_svc: &crate::ReaperAudioEngine,
    position_svc: &crate::ReaperPositionConversion,
    health_svc: &crate::ReaperHealth,
    action_registry_svc: &crate::ReaperActionRegistry,
    toolbar_svc: &crate::ReaperToolbar,
    plugin_loader_svc: &crate::ReaperPluginLoader,
    peak_svc: &crate::ReaperPeak,
    resource_svc: &crate::resource::ReaperResource,
    audio_accessor_svc: &crate::ReaperAudioAccessor,
) -> Result<StepOutput, String> {
    match op {
        BatchOp::Project(op) => dispatch_project(op, outputs, project_svc).await,
        BatchOp::Transport(op) => dispatch_transport(op, outputs, transport_svc).await,
        BatchOp::Track(op) => dispatch_track(op, outputs, track_svc).await,
        BatchOp::Fx(op) => dispatch_fx(op, outputs, fx_svc).await,
        BatchOp::Routing(op) => dispatch_routing(op, outputs, routing_svc).await,
        BatchOp::Item(op) => dispatch_item(op, outputs, item_svc).await,
        BatchOp::Take(op) => dispatch_take(op, outputs, take_svc).await,
        BatchOp::Automation(_) => Err("AutomationService is not yet implemented".to_string()),
        BatchOp::Marker(op) => dispatch_marker(op, outputs, marker_svc).await,
        BatchOp::Region(op) => dispatch_region(op, outputs, region_svc).await,
        BatchOp::TempoMap(op) => dispatch_tempo_map(op, outputs, tempo_map_svc).await,
        BatchOp::Midi(op) => dispatch_midi(op, outputs, midi_svc).await,
        BatchOp::LiveMidi(op) => dispatch_live_midi(op, live_midi_svc).await,
        BatchOp::ExtState(op) => dispatch_ext_state(op, outputs, ext_state_svc).await,
        BatchOp::AudioEngine(op) => dispatch_audio_engine(op, audio_engine_svc).await,
        BatchOp::PositionConversion(op) => {
            dispatch_position_conversion(op, outputs, position_svc).await
        }
        BatchOp::Health(op) => dispatch_health(op, health_svc).await,
        BatchOp::ActionRegistry(op) => dispatch_action_registry(op, action_registry_svc).await,
        BatchOp::Toolbar(op) => dispatch_toolbar(op, toolbar_svc).await,
        BatchOp::PluginLoader(op) => dispatch_plugin_loader(op, plugin_loader_svc).await,
        BatchOp::Peak(op) => dispatch_peak(op, outputs, peak_svc).await,
        BatchOp::Resource(op) => dispatch_resource(op, resource_svc).await,
        BatchOp::AudioAccessor(op) => {
            dispatch_audio_accessor(op, outputs, audio_accessor_svc).await
        }
    }
}

// =============================================================================
// Project dispatch
// =============================================================================

async fn dispatch_project(
    op: &ProjectOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::ReaperProject,
) -> Result<StepOutput, String> {
    use daw_proto::ProjectService;
    match op {
        ProjectOp::GetCurrent => Ok(StepOutput::OptProjectInfo(svc.get_current().await)),
        ProjectOp::Get(id) => Ok(StepOutput::OptProjectInfo(svc.get(id.clone()).await)),
        ProjectOp::List => Ok(StepOutput::ProjectInfoList(svc.list().await)),
        ProjectOp::Select(id) => Ok(StepOutput::Bool(svc.select(id.clone()).await)),
        ProjectOp::Open(path) => Ok(StepOutput::OptProjectInfo(svc.open(path.clone()).await)),
        ProjectOp::Create => Ok(StepOutput::OptProjectInfo(svc.create().await)),
        ProjectOp::Close(id) => Ok(StepOutput::Bool(svc.close(id.clone()).await)),
        ProjectOp::GetBySlot(slot) => Ok(StepOutput::OptProjectInfo(svc.get_by_slot(*slot).await)),
        ProjectOp::BeginUndoBlock(p, label) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.begin_undo_block(ctx, label.clone()).await;
            Ok(StepOutput::Unit)
        }
        ProjectOp::EndUndoBlock(p, label, scope) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.end_undo_block(ctx, label.clone(), scope.clone()).await;
            Ok(StepOutput::Unit)
        }
        ProjectOp::Undo(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::Bool(svc.undo(ctx).await))
        }
        ProjectOp::Redo(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::Bool(svc.redo(ctx).await))
        }
        ProjectOp::LastUndoLabel(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptStr(svc.last_undo_label(ctx).await))
        }
        ProjectOp::LastRedoLabel(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptStr(svc.last_redo_label(ctx).await))
        }
        ProjectOp::RunCommand(p, cmd) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::Bool(svc.run_command(ctx, cmd.clone()).await))
        }
        ProjectOp::Save(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.save(ctx).await;
            Ok(StepOutput::Unit)
        }
        ProjectOp::SaveAll => {
            svc.save_all().await;
            Ok(StepOutput::Unit)
        }
        ProjectOp::SetRulerLaneName(p, lane, name) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_ruler_lane_name(ctx, *lane, name.clone()).await;
            Ok(StepOutput::Unit)
        }
        ProjectOp::GetRulerLaneName(p, lane) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::Str(svc.get_ruler_lane_name(ctx, *lane).await))
        }
        ProjectOp::RulerLaneCount(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::U32(svc.ruler_lane_count(ctx).await))
        }
    }
}

// =============================================================================
// Transport dispatch
// =============================================================================

async fn dispatch_transport(
    op: &TransportOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::Reaper,
) -> Result<StepOutput, String> {
    use daw_proto::sync::Transport;
    match op {
        TransportOp::Play(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::play(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::Pause(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::pause(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::Stop(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::stop(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::PlayPause(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::play_pause(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::PlayStop(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::play_stop(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::Record(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::record(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::StopRecording(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::stop_recording(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::ToggleRecording(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::toggle_recording(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::SetPosition(p, secs) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::set_position(svc, ctx, *secs).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::GetPosition(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::F64(Transport::get_position(svc, ctx)))
        }
        TransportOp::GotoStart(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::goto_start(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::GotoEnd(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::goto_end(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::GetState(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::Transport(Transport::get_state(svc, ctx)))
        }
        TransportOp::GetPlayState(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::PlayState(Transport::get_play_state(svc, ctx)))
        }
        TransportOp::IsPlaying(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::Bool(Transport::is_playing(svc, ctx)))
        }
        TransportOp::IsRecording(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::Bool(Transport::is_recording(svc, ctx)))
        }
        TransportOp::GetTempo(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::F64(Transport::get_tempo(svc, ctx)))
        }
        TransportOp::SetTempo(p, bpm) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::set_tempo(svc, ctx, *bpm).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::ToggleLoop(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::toggle_loop(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::IsLooping(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::Bool(Transport::is_looping(svc, ctx)))
        }
        TransportOp::SetLoop(p, enabled) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::set_loop(svc, ctx, *enabled).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::GetPlayrate(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::F64(Transport::get_playrate(svc, ctx)))
        }
        TransportOp::SetPlayrate(p, rate) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Transport::set_playrate(svc, ctx, *rate).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TransportOp::GetTimeSignature(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::TimeSignature(Transport::get_time_signature(
                svc, ctx,
            )))
        }
    }
}

// =============================================================================
// Track dispatch
// =============================================================================

async fn dispatch_track(
    op: &TrackOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::Reaper,
) -> Result<StepOutput, String> {
    use daw_proto::sync::Tracks;
    match op {
        TrackOp::GetTracks(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::TrackList(Tracks::all(svc, ctx)))
        }
        TrackOp::GetTrack(p, t) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Ok(StepOutput::OptTrack(Tracks::get(svc, ctx, tr)))
        }
        TrackOp::TrackCount(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::U32(Tracks::count(svc, ctx)))
        }
        TrackOp::GetSelectedTracks(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::TrackList(Tracks::selected(svc, ctx)))
        }
        TrackOp::GetMasterTrack(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptTrack(Tracks::master(svc, ctx)))
        }
        TrackOp::SetMuted(p, t, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Tracks::set_muted(svc, ctx, tr, *v).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::SetSoloed(p, t, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Tracks::set_soloed(svc, ctx, tr, *v).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::SetSoloExclusive(p, t) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Tracks::set_solo_exclusive(svc, ctx, tr).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::ClearAllSolo(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Tracks::clear_all_solo(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::SetArmed(p, t, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Tracks::set_armed(svc, ctx, tr, *v).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::SetVolume(p, t, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Tracks::set_volume(svc, ctx, tr, *v).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::SetPan(p, t, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Tracks::set_pan(svc, ctx, tr, *v).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::SetSelected(p, t, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Tracks::set_selected(svc, ctx, tr, *v).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::SelectExclusive(p, t) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Tracks::select_exclusive(svc, ctx, tr).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::ClearSelection(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Tracks::clear_selection(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::MuteAll(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Tracks::mute_all(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::UnmuteAll(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Tracks::unmute_all(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::AddTrack(p, name, at) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let id = Tracks::add(svc, ctx, name, *at).map_err(|e| e.to_string())?;
            Ok(StepOutput::Str(id))
        }
        TrackOp::RemoveTrack(p, t) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Tracks::remove(svc, ctx, tr).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::RenameTrack(p, t, name) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Tracks::rename(svc, ctx, tr, name).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::SetTrackColor(p, t, color) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Tracks::set_color(svc, ctx, tr, *color).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        TrackOp::RemoveAllTracks(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Tracks::remove_all(svc, ctx).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
    }
}

// =============================================================================
// FX dispatch
// =============================================================================

async fn dispatch_fx(
    op: &FxOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::ReaperFx,
) -> Result<StepOutput, String> {
    use daw_proto::FxService;

    // Helper to build FxTarget from chain arg + fx ref
    let target = |c: &FxChainArg, f: &FxRef| -> Result<FxTarget, String> {
        Ok(FxTarget {
            context: resolve_fx_chain_arg(c, outputs)?,
            fx: f.clone(),
        })
    };

    match op {
        FxOp::ListInstalledFx => Ok(StepOutput::InstalledFxList(svc.list_installed_fx().await)),
        FxOp::GetLastTouchedFx => Ok(StepOutput::OptLastTouchedFx(
            svc.get_last_touched_fx().await,
        )),
        FxOp::GetFxList(p, c) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let chain = resolve_fx_chain_arg(c, outputs)?;
            Ok(StepOutput::FxList(svc.get_fx_list(ctx, chain).await))
        }
        FxOp::GetFx(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptFx(svc.get_fx(ctx, target(c, f)?).await))
        }
        FxOp::FxCount(p, c) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let chain = resolve_fx_chain_arg(c, outputs)?;
            Ok(StepOutput::U32(svc.fx_count(ctx, chain).await))
        }
        FxOp::SetFxEnabled(p, c, f, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.set_fx_enabled(ctx, target(c, f)?, *v).await)
        }
        FxOp::SetFxOffline(p, c, f, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.set_fx_offline(ctx, target(c, f)?, *v).await)
        }
        FxOp::AddFx(p, c, name) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let chain = resolve_fx_chain_arg(c, outputs)?;
            Ok(StepOutput::OptStr(
                svc.add_fx(ctx, chain, name.clone()).await,
            ))
        }
        FxOp::AddFxAt(p, req) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptStr(svc.add_fx_at(ctx, req.clone()).await))
        }
        FxOp::RemoveFx(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.remove_fx(ctx, target(c, f)?).await)
        }
        FxOp::MoveFx(p, c, f, idx) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.move_fx(ctx, target(c, f)?, *idx).await)
        }
        FxOp::GetParameters(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::FxParameterList(
                svc.get_parameters(ctx, target(c, f)?).await,
            ))
        }
        FxOp::GetParameter(p, c, f, idx) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptFxParameter(
                svc.get_parameter(ctx, target(c, f)?, *idx).await,
            ))
        }
        FxOp::SetParameter(p, req) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.set_parameter(ctx, req.clone()).await)
        }
        FxOp::GetParameterByName(p, c, f, name) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptFxParameter(
                svc.get_parameter_by_name(ctx, target(c, f)?, name.clone())
                    .await,
            ))
        }
        FxOp::SetParameterByName(p, req) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.set_parameter_by_name(ctx, req.clone()).await)
        }
        FxOp::GetPresetIndex(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptFxPresetIndex(
                svc.get_preset_index(ctx, target(c, f)?).await,
            ))
        }
        FxOp::NextPreset(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.next_preset(ctx, target(c, f)?).await)
        }
        FxOp::PrevPreset(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.prev_preset(ctx, target(c, f)?).await)
        }
        FxOp::SetPreset(p, c, f, idx) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.set_preset(ctx, target(c, f)?, *idx).await)
        }
        FxOp::OpenFxUi(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.open_fx_ui(ctx, target(c, f)?).await)
        }
        FxOp::CloseFxUi(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.close_fx_ui(ctx, target(c, f)?).await)
        }
        FxOp::ToggleFxUi(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.toggle_fx_ui(ctx, target(c, f)?).await)
        }
        FxOp::GetNamedConfig(p, c, f, key) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptStr(
                svc.get_named_config(ctx, target(c, f)?, key.clone()).await,
            ))
        }
        FxOp::SetNamedConfig(p, req) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.set_named_config(ctx, req.clone()).await)
        }
        FxOp::GetFxLatency(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptFxLatency(
                svc.get_fx_latency(ctx, target(c, f)?).await,
            ))
        }
        FxOp::GetParamModulation(p, c, f, idx) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptFxParamModulation(
                svc.get_param_modulation(ctx, target(c, f)?, *idx).await,
            ))
        }
        FxOp::GetFxChannelConfig(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptFxChannelConfig(
                svc.get_fx_channel_config(ctx, target(c, f)?).await,
            ))
        }
        FxOp::SetFxChannelConfig(p, c, f, config) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(
                svc.set_fx_channel_config(ctx, target(c, f)?, config.clone())
                    .await,
            )
        }
        FxOp::SilenceFxOutput(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::ResultFxPinMappings(
                svc.silence_fx_output(ctx, target(c, f)?).await,
            ))
        }
        FxOp::RestoreFxOutput(p, c, f, saved) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(
                svc.restore_fx_output(ctx, target(c, f)?, saved.clone())
                    .await,
            )
        }
        FxOp::GetFxStateChunk(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptBytes(
                svc.get_fx_state_chunk(ctx, target(c, f)?).await,
            ))
        }
        FxOp::SetFxStateChunk(p, c, f, chunk) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(
                svc.set_fx_state_chunk(ctx, target(c, f)?, chunk.clone())
                    .await,
            )
        }
        FxOp::GetFxStateChunkEncoded(p, c, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptStr(
                svc.get_fx_state_chunk_encoded(ctx, target(c, f)?).await,
            ))
        }
        FxOp::SetFxStateChunkEncoded(p, c, f, encoded) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(
                svc.set_fx_state_chunk_encoded(ctx, target(c, f)?, encoded.clone())
                    .await,
            )
        }
        FxOp::GetFxChainState(p, c) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let chain = resolve_fx_chain_arg(c, outputs)?;
            Ok(StepOutput::FxStateChunkList(
                svc.get_fx_chain_state(ctx, chain).await,
            ))
        }
        FxOp::SetFxChainState(p, c, chunks) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let chain = resolve_fx_chain_arg(c, outputs)?;
            result_to_output(svc.set_fx_chain_state(ctx, chain, chunks.clone()).await)
        }
        FxOp::GetFxTree(p, c) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let chain = resolve_fx_chain_arg(c, outputs)?;
            Ok(StepOutput::FxTree(svc.get_fx_tree(ctx, chain).await))
        }
        FxOp::CreateContainer(p, req) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptFxNodeId(
                svc.create_container(ctx, req.clone()).await,
            ))
        }
        FxOp::MoveToContainer(p, req) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.move_to_container(ctx, req.clone()).await)
        }
        FxOp::MoveFromContainer(p, req) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.move_from_container(ctx, req.clone()).await)
        }
        FxOp::SetRoutingMode(p, c, node_id, mode) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let chain = resolve_fx_chain_arg(c, outputs)?;
            result_to_output(
                svc.set_routing_mode(ctx, chain, node_id.clone(), *mode)
                    .await,
            )
        }
        FxOp::GetContainerChannelConfig(p, c, id) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let chain = resolve_fx_chain_arg(c, outputs)?;
            Ok(StepOutput::OptFxContainerChannelConfig(
                svc.get_container_channel_config(ctx, chain, id.clone())
                    .await,
            ))
        }
        FxOp::SetContainerChannelConfig(p, req) => {
            let ctx = resolve_project_arg(p, outputs)?;
            result_to_output(svc.set_container_channel_config(ctx, req.clone()).await)
        }
        FxOp::EncloseInContainer(p, req) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptFxNodeId(
                svc.enclose_in_container(ctx, req.clone()).await,
            ))
        }
        FxOp::ExplodeContainer(p, c, id) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let chain = resolve_fx_chain_arg(c, outputs)?;
            result_to_output(svc.explode_container(ctx, chain, id.clone()).await)
        }
        FxOp::RenameContainer(p, c, id, name) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let chain = resolve_fx_chain_arg(c, outputs)?;
            result_to_output(
                svc.rename_container(ctx, chain, id.clone(), name.clone())
                    .await,
            )
        }
        FxOp::GetFxChainChunkText(p, c) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let chain = resolve_fx_chain_arg(c, outputs)?;
            Ok(StepOutput::OptStr(
                svc.get_fx_chain_chunk_text(ctx, chain).await,
            ))
        }
        FxOp::InsertFxChainChunk(p, c, text) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let chain = resolve_fx_chain_arg(c, outputs)?;
            result_to_output(svc.insert_fx_chain_chunk(ctx, chain, text.clone()).await)
        }
    }
}

// =============================================================================
// Routing dispatch
// =============================================================================

async fn dispatch_routing(
    op: &RoutingOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::ReaperRouting,
) -> Result<StepOutput, String> {
    use daw_proto::RoutingService;
    match op {
        RoutingOp::GetSends(p, t) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Ok(StepOutput::RouteList(svc.get_sends(ctx, tr).await))
        }
        RoutingOp::GetReceives(p, t) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Ok(StepOutput::RouteList(svc.get_receives(ctx, tr).await))
        }
        RoutingOp::GetHardwareOutputs(p, t) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Ok(StepOutput::RouteList(
                svc.get_hardware_outputs(ctx, tr).await,
            ))
        }
        RoutingOp::GetRoute(p, loc) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptRoute(svc.get_route(ctx, loc.clone()).await))
        }
        RoutingOp::AddSend(p, src, dst) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let s = resolve_track_arg(src, outputs)?;
            let d = resolve_track_arg(dst, outputs)?;
            Ok(StepOutput::OptU32(svc.add_send(ctx, s, d).await))
        }
        RoutingOp::AddHardwareOutput(p, t, hw) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Ok(StepOutput::OptU32(
                svc.add_hardware_output(ctx, tr, *hw).await,
            ))
        }
        RoutingOp::RemoveRoute(p, loc) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.remove_route(ctx, loc.clone()).await;
            Ok(StepOutput::Unit)
        }
        RoutingOp::SetVolume(p, loc, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_volume(ctx, loc.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        RoutingOp::SetPan(p, loc, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_pan(ctx, loc.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        RoutingOp::SetMuted(p, loc, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_muted(ctx, loc.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        RoutingOp::SetMono(p, loc, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_mono(ctx, loc.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        RoutingOp::SetPhase(p, loc, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_phase(ctx, loc.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        RoutingOp::SetSendMode(p, t, r, mode) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            svc.set_send_mode(ctx, tr, r.clone(), *mode).await;
            Ok(StepOutput::Unit)
        }
        RoutingOp::SetSourceChannels(p, loc, start, num) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_source_channels(ctx, loc.clone(), *start, *num)
                .await;
            Ok(StepOutput::Unit)
        }
        RoutingOp::SetDestChannels(p, loc, start, num) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_dest_channels(ctx, loc.clone(), *start, *num).await;
            Ok(StepOutput::Unit)
        }
        RoutingOp::GetParentSendEnabled(p, t) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Ok(StepOutput::Bool(svc.get_parent_send_enabled(ctx, tr).await))
        }
        RoutingOp::SetParentSendEnabled(p, t, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            svc.set_parent_send_enabled(ctx, tr, *v).await;
            Ok(StepOutput::Unit)
        }
    }
}

// =============================================================================
// Item dispatch
// =============================================================================

async fn dispatch_item(
    op: &ItemOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::ReaperItem,
) -> Result<StepOutput, String> {
    use daw_proto::ItemService;
    match op {
        ItemOp::GetItems(p, t) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Ok(StepOutput::ItemList(svc.get_items(ctx, tr).await))
        }
        ItemOp::GetItem(p, item) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptItem(svc.get_item(ctx, item.clone()).await))
        }
        ItemOp::GetAllItems(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::ItemList(svc.get_all_items(ctx).await))
        }
        ItemOp::GetSelectedItems(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::ItemList(svc.get_selected_items(ctx).await))
        }
        ItemOp::ItemCount(p, t) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Ok(StepOutput::U32(svc.item_count(ctx, tr).await))
        }
        ItemOp::AddItem(p, t, pos, len) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Ok(StepOutput::OptStr(svc.add_item(ctx, tr, *pos, *len).await))
        }
        ItemOp::DeleteItem(p, item) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.delete_item(ctx, item.clone()).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::DuplicateItem(p, item) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptStr(
                svc.duplicate_item(ctx, item.clone()).await,
            ))
        }
        ItemOp::SetPosition(p, item, pos) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_position(ctx, item.clone(), *pos).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetLength(p, item, len) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_length(ctx, item.clone(), *len).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::MoveToTrack(p, item, t) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            svc.move_to_track(ctx, item.clone(), tr).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetSnapOffset(p, item, offset) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_snap_offset(ctx, item.clone(), *offset).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetMuted(p, item, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_muted(ctx, item.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetSelected(p, item, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_selected(ctx, item.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetLocked(p, item, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_locked(ctx, item.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SelectAllItems(p, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.select_all_items(ctx, *v).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetVolume(p, item, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_volume(ctx, item.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetFadeIn(p, item, len, shape) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_fade_in(ctx, item.clone(), *len, *shape).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetFadeOut(p, item, len, shape) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_fade_out(ctx, item.clone(), *len, *shape).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetLoopSource(p, item, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_loop_source(ctx, item.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetBeatAttachMode(p, item, mode) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_beat_attach_mode(ctx, item.clone(), *mode).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetAutoStretch(p, item, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_auto_stretch(ctx, item.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetColor(p, item, color) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_color(ctx, item.clone(), *color).await;
            Ok(StepOutput::Unit)
        }
        ItemOp::SetGroupId(p, item, gid) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_group_id(ctx, item.clone(), *gid).await;
            Ok(StepOutput::Unit)
        }
    }
}

// =============================================================================
// Take dispatch
// =============================================================================

async fn dispatch_take(
    op: &TakeOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::ReaperTake,
) -> Result<StepOutput, String> {
    use daw_proto::TakeService;
    match op {
        TakeOp::GetTakes(p, item) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::TakeList(svc.get_takes(ctx, item.clone()).await))
        }
        TakeOp::GetTake(p, item, take) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptTake(
                svc.get_take(ctx, item.clone(), take.clone()).await,
            ))
        }
        TakeOp::GetActiveTake(p, item) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptTake(
                svc.get_active_take(ctx, item.clone()).await,
            ))
        }
        TakeOp::TakeCount(p, item) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::U32(svc.take_count(ctx, item.clone()).await))
        }
        TakeOp::AddTake(p, item) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptStr(svc.add_take(ctx, item.clone()).await))
        }
        TakeOp::DeleteTake(p, item, take) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.delete_take(ctx, item.clone(), take.clone()).await;
            Ok(StepOutput::Unit)
        }
        TakeOp::SetActiveTake(p, item, take) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_active_take(ctx, item.clone(), take.clone()).await;
            Ok(StepOutput::Unit)
        }
        TakeOp::SetName(p, item, take, name) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_name(ctx, item.clone(), take.clone(), name.clone())
                .await;
            Ok(StepOutput::Unit)
        }
        TakeOp::SetColor(p, item, take, color) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_color(ctx, item.clone(), take.clone(), *color).await;
            Ok(StepOutput::Unit)
        }
        TakeOp::SetVolume(p, item, take, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_volume(ctx, item.clone(), take.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        TakeOp::SetPlayRate(p, item, take, rate) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_play_rate(ctx, item.clone(), take.clone(), *rate)
                .await;
            Ok(StepOutput::Unit)
        }
        TakeOp::SetPitch(p, item, take, semi) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_pitch(ctx, item.clone(), take.clone(), *semi).await;
            Ok(StepOutput::Unit)
        }
        TakeOp::SetPreservePitch(p, item, take, v) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_preserve_pitch(ctx, item.clone(), take.clone(), *v)
                .await;
            Ok(StepOutput::Unit)
        }
        TakeOp::SetStartOffset(p, item, take, offset) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_start_offset(ctx, item.clone(), take.clone(), *offset)
                .await;
            Ok(StepOutput::Unit)
        }
        TakeOp::SetSourceFile(p, item, take, path) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_source_file(ctx, item.clone(), take.clone(), path.clone())
                .await;
            Ok(StepOutput::Unit)
        }
        TakeOp::GetSourceType(p, item, take) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::SourceType(
                svc.get_source_type(ctx, item.clone(), take.clone()).await,
            ))
        }
    }
}

// Note: AutomationService dispatch is not implemented because ReaperAutomation
// is a stub. Batch operations for automation return an error at dispatch time.

// =============================================================================
// Marker dispatch
// =============================================================================

async fn dispatch_marker(
    op: &MarkerOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::Reaper,
) -> Result<StepOutput, String> {
    use daw_proto::sync::Markers;
    // Fully-qualified calls — `Reaper` impls both `Markers` and
    // `Tracks`, and many verbs (`all`, `get`, `count`, `add`,
    // `remove`, `rename`, `set_color`) appear on both.
    match op {
        MarkerOp::GetMarkers(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::MarkerList(Markers::all(svc, ctx)))
        }
        MarkerOp::GetMarker(p, id) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptMarker(Markers::get(svc, ctx, *id)))
        }
        MarkerOp::MarkerCount(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::Usize(Markers::count(svc, ctx) as u64))
        }
        MarkerOp::AddMarker(p, pos, name) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let id = Markers::add(svc, ctx, *pos, name).map_err(|e| e.to_string())?;
            Ok(StepOutput::U32(id))
        }
        MarkerOp::RemoveMarker(p, id) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Markers::remove(svc, ctx, *id).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        MarkerOp::MoveMarker(p, id, pos) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Markers::set_position(svc, ctx, *id, *pos).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        MarkerOp::RenameMarker(p, id, name) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Markers::rename(svc, ctx, *id, name).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        MarkerOp::SetMarkerColor(p, id, color) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Markers::set_color(svc, ctx, *id, *color).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
    }
}

// =============================================================================
// Region dispatch
// =============================================================================

async fn dispatch_region(
    op: &RegionOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::Reaper,
) -> Result<StepOutput, String> {
    use daw_proto::sync::Regions;
    // Fully-qualified calls — `Reaper` impls multiple service traits
    // and many verbs overlap.
    match op {
        RegionOp::GetRegions(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::RegionList(Regions::all(svc, ctx)))
        }
        RegionOp::GetRegion(p, id) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptRegion(Regions::get(svc, ctx, *id)))
        }
        RegionOp::RegionCount(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::Usize(Regions::count(svc, ctx) as u64))
        }
        RegionOp::AddRegion(p, s, e, name) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let id = Regions::add(svc, ctx, *s, *e, name).map_err(|e| e.to_string())?;
            Ok(StepOutput::U32(id))
        }
        RegionOp::RemoveRegion(p, id) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Regions::remove(svc, ctx, *id).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        RegionOp::SetRegionBounds(p, id, s, e) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Regions::set_bounds(svc, ctx, *id, *s, *e).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        RegionOp::RenameRegion(p, id, name) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Regions::rename(svc, ctx, *id, name).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
        RegionOp::SetRegionColor(p, id, color) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Regions::set_color(svc, ctx, *id, *color).map_err(|e| e.to_string())?;
            Ok(StepOutput::Unit)
        }
    }
}

// =============================================================================
// TempoMap dispatch
// =============================================================================

async fn dispatch_tempo_map(
    op: &TempoMapOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::Reaper,
) -> Result<StepOutput, String> {
    use daw_proto::sync::TempoMap;
    match op {
        TempoMapOp::GetTempoPoints(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::TempoPointList(TempoMap::get_tempo_points(
                svc, ctx,
            )))
        }
        TempoMapOp::GetTempoPoint(p, i) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptTempoPoint(TempoMap::get_tempo_point(
                svc, ctx, *i,
            )))
        }
        TempoMapOp::TempoPointCount(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::Usize(
                TempoMap::tempo_point_count(svc, ctx) as u64
            ))
        }
        TempoMapOp::GetTempoAt(p, secs) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::F64(TempoMap::get_tempo_at(svc, ctx, *secs)))
        }
        TempoMapOp::GetTimeSignatureAt(p, secs) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let (n, d) = TempoMap::get_time_signature_at(svc, ctx, *secs);
            Ok(StepOutput::I32Pair(n, d))
        }
        TempoMapOp::TimeToMusical(p, secs) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let (m, b, f) = TempoMap::time_to_musical(svc, ctx, *secs);
            Ok(StepOutput::MusicalTime(m, b, f))
        }
        TempoMapOp::MusicalToTime(p, m, b, f) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::F64(TempoMap::musical_to_time(
                svc, ctx, *m, *b, *f,
            )))
        }
        TempoMapOp::AddTempoPoint(p, secs, bpm) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let idx =
                TempoMap::add_tempo_point(svc, ctx, *secs, *bpm).map_err(|e| format!("{e:?}"))?;
            Ok(StepOutput::U32(idx))
        }
        TempoMapOp::RemoveTempoPoint(p, i) => {
            let ctx = resolve_project_arg(p, outputs)?;
            TempoMap::remove_tempo_point(svc, ctx, *i).map_err(|e| format!("{e:?}"))?;
            Ok(StepOutput::Unit)
        }
        TempoMapOp::SetTempoAtPoint(p, i, bpm) => {
            let ctx = resolve_project_arg(p, outputs)?;
            TempoMap::set_tempo_at_point(svc, ctx, *i, *bpm).map_err(|e| format!("{e:?}"))?;
            Ok(StepOutput::Unit)
        }
        TempoMapOp::SetTimeSignatureAtPoint(p, i, n, d) => {
            let ctx = resolve_project_arg(p, outputs)?;
            TempoMap::set_time_signature_at_point(svc, ctx, *i, *n, *d)
                .map_err(|e| format!("{e:?}"))?;
            Ok(StepOutput::Unit)
        }
        TempoMapOp::MoveTempoPoint(p, i, secs) => {
            let ctx = resolve_project_arg(p, outputs)?;
            TempoMap::move_tempo_point(svc, ctx, *i, *secs).map_err(|e| format!("{e:?}"))?;
            Ok(StepOutput::Unit)
        }
        TempoMapOp::GetDefaultTempo(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::F64(TempoMap::get_default_tempo(svc, ctx)))
        }
        TempoMapOp::SetDefaultTempo(p, bpm) => {
            let ctx = resolve_project_arg(p, outputs)?;
            TempoMap::set_default_tempo(svc, ctx, *bpm).map_err(|e| format!("{e:?}"))?;
            Ok(StepOutput::Unit)
        }
        TempoMapOp::GetDefaultTimeSignature(p) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let (n, d) = TempoMap::get_default_time_signature(svc, ctx);
            Ok(StepOutput::I32Pair(n, d))
        }
        TempoMapOp::SetDefaultTimeSignature(p, n, d) => {
            let ctx = resolve_project_arg(p, outputs)?;
            TempoMap::set_default_time_signature(svc, ctx, *n, *d).map_err(|e| format!("{e:?}"))?;
            Ok(StepOutput::Unit)
        }
    }
}

// =============================================================================
// MIDI dispatch
// =============================================================================

async fn dispatch_midi(
    op: &MidiOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::ReaperMidi,
) -> Result<StepOutput, String> {
    use daw_proto::MidiService;
    match op {
        MidiOp::GetNotes(loc) => Ok(StepOutput::MidiNoteList(svc.get_notes(loc.clone()).await)),
        MidiOp::GetNotesInRange(loc, range) => Ok(StepOutput::MidiNoteList(
            svc.get_notes_in_range(loc.clone(), range.clone()).await,
        )),
        MidiOp::GetSelectedNotes(loc) => Ok(StepOutput::MidiNoteList(
            svc.get_selected_notes(loc.clone()).await,
        )),
        MidiOp::NoteCount(loc) => Ok(StepOutput::U32(svc.note_count(loc.clone()).await)),
        MidiOp::CreateMidiItem(p, t, start, end) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Ok(StepOutput::OptMidiTakeLocation(
                svc.create_midi_item(ctx, tr, *start, *end).await,
            ))
        }
        MidiOp::AddNote(loc, note) => Ok(StepOutput::U32(
            svc.add_note(loc.clone(), note.clone()).await,
        )),
        MidiOp::AddNotes(loc, notes) => Ok(StepOutput::U32List(
            svc.add_notes(loc.clone(), notes.clone()).await,
        )),
        MidiOp::DeleteNote(loc, i) => {
            svc.delete_note(loc.clone(), *i).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::DeleteNotes(loc, indices) => {
            svc.delete_notes(loc.clone(), indices.clone()).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::DeleteSelectedNotes(loc) => {
            svc.delete_selected_notes(loc.clone()).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::SetNotePitch(loc, i, v) => {
            svc.set_note_pitch(loc.clone(), *i, *v).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::SetNoteVelocity(loc, i, v) => {
            svc.set_note_velocity(loc.clone(), *i, *v).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::SetNotePosition(loc, i, v) => {
            svc.set_note_position(loc.clone(), *i, *v).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::SetNoteLength(loc, i, v) => {
            svc.set_note_length(loc.clone(), *i, *v).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::SetNoteChannel(loc, i, v) => {
            svc.set_note_channel(loc.clone(), *i, *v).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::SetNoteSelected(loc, i, v) => {
            svc.set_note_selected(loc.clone(), *i, *v).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::SetNoteMuted(loc, i, v) => {
            svc.set_note_muted(loc.clone(), *i, *v).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::SelectAllNotes(loc, v) => {
            svc.select_all_notes(loc.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::TransposeNotes(loc, indices, semi) => {
            svc.transpose_notes(loc.clone(), indices.clone(), *semi)
                .await;
            Ok(StepOutput::Unit)
        }
        MidiOp::QuantizeNotes(loc, params) => {
            svc.quantize_notes(loc.clone(), params.clone()).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::HumanizeNotes(loc, params) => {
            svc.humanize_notes(loc.clone(), params.clone()).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::GetCcs(loc, ctrl) => Ok(StepOutput::MidiCCList(
            svc.get_ccs(loc.clone(), *ctrl).await,
        )),
        MidiOp::AddCc(loc, cc) => Ok(StepOutput::U32(svc.add_cc(loc.clone(), cc.clone()).await)),
        MidiOp::DeleteCc(loc, i) => {
            svc.delete_cc(loc.clone(), *i).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::SetCcValue(loc, i, v) => {
            svc.set_cc_value(loc.clone(), *i, *v).await;
            Ok(StepOutput::Unit)
        }
        MidiOp::GetPitchBends(loc) => Ok(StepOutput::MidiPitchBendList(
            svc.get_pitch_bends(loc.clone()).await,
        )),
        MidiOp::AddPitchBend(loc, pb) => Ok(StepOutput::U32(
            svc.add_pitch_bend(loc.clone(), pb.clone()).await,
        )),
        MidiOp::GetProgramChanges(loc) => Ok(StepOutput::MidiProgramChangeList(
            svc.get_program_changes(loc.clone()).await,
        )),
        MidiOp::GetSysex(loc) => Ok(StepOutput::MidiSysExList(svc.get_sysex(loc.clone()).await)),
    }
}

// =============================================================================
// Live MIDI dispatch
// =============================================================================

async fn dispatch_live_midi(
    op: &LiveMidiOp,
    svc: &crate::ReaperLiveMidi,
) -> Result<StepOutput, String> {
    use daw_proto::LiveMidiService;
    match op {
        LiveMidiOp::GetInputDevices => Ok(StepOutput::MidiInputDeviceList(
            svc.get_input_devices().await,
        )),
        LiveMidiOp::GetOutputDevices => Ok(StepOutput::MidiOutputDeviceList(
            svc.get_output_devices().await,
        )),
        LiveMidiOp::GetInputDevice(id) => Ok(StepOutput::OptMidiInputDevice(
            svc.get_input_device(*id).await,
        )),
        LiveMidiOp::GetOutputDevice(id) => Ok(StepOutput::OptMidiOutputDevice(
            svc.get_output_device(*id).await,
        )),
        LiveMidiOp::OpenInputDevice(id) => Ok(StepOutput::Bool(svc.open_input_device(*id).await)),
        LiveMidiOp::CloseInputDevice(id) => {
            svc.close_input_device(*id).await;
            Ok(StepOutput::Unit)
        }
        LiveMidiOp::OpenOutputDevice(id) => Ok(StepOutput::Bool(svc.open_output_device(*id).await)),
        LiveMidiOp::CloseOutputDevice(id) => {
            svc.close_output_device(*id).await;
            Ok(StepOutput::Unit)
        }
        LiveMidiOp::SendMidi(dev, msg, timing) => {
            svc.send_midi(*dev, msg.clone(), *timing).await;
            Ok(StepOutput::Unit)
        }
        LiveMidiOp::SendMidiBatch(dev, events) => {
            svc.send_midi_batch(*dev, events.clone()).await;
            Ok(StepOutput::Unit)
        }
        LiveMidiOp::StuffMidiMessage(target, msg) => {
            svc.stuff_midi_message(*target, msg.clone()).await;
            Ok(StepOutput::Unit)
        }
    }
}

// =============================================================================
// ExtState dispatch
// =============================================================================

async fn dispatch_ext_state(
    op: &ExtStateOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::ReaperExtState,
) -> Result<StepOutput, String> {
    use daw_proto::ExtStateService;
    match op {
        ExtStateOp::GetExtState(section, key) => Ok(StepOutput::OptStr(
            svc.get_ext_state(section.clone(), key.clone()).await,
        )),
        ExtStateOp::SetExtState(section, key, value, persist) => {
            svc.set_ext_state(section.clone(), key.clone(), value.clone(), *persist)
                .await;
            Ok(StepOutput::Unit)
        }
        ExtStateOp::DeleteExtState(section, key, persist) => {
            svc.delete_ext_state(section.clone(), key.clone(), *persist)
                .await;
            Ok(StepOutput::Unit)
        }
        ExtStateOp::HasExtState(section, key) => Ok(StepOutput::Bool(
            svc.has_ext_state(section.clone(), key.clone()).await,
        )),
        ExtStateOp::GetProjectExtState(p, section, key) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptStr(
                svc.get_project_ext_state(ctx, section.clone(), key.clone())
                    .await,
            ))
        }
        ExtStateOp::SetProjectExtState(p, section, key, value) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.set_project_ext_state(ctx, section.clone(), key.clone(), value.clone())
                .await;
            Ok(StepOutput::Unit)
        }
        ExtStateOp::DeleteProjectExtState(p, section, key) => {
            let ctx = resolve_project_arg(p, outputs)?;
            svc.delete_project_ext_state(ctx, section.clone(), key.clone())
                .await;
            Ok(StepOutput::Unit)
        }
        ExtStateOp::HasProjectExtState(p, section, key) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::Bool(
                svc.has_project_ext_state(ctx, section.clone(), key.clone())
                    .await,
            ))
        }
    }
}

// =============================================================================
// Audio engine dispatch
// =============================================================================

async fn dispatch_audio_engine(
    op: &AudioEngineOp,
    svc: &crate::ReaperAudioEngine,
) -> Result<StepOutput, String> {
    use daw_proto::AudioEngineService;
    match op {
        AudioEngineOp::GetState => Ok(StepOutput::AudioEngineState(svc.get_state().await)),
        AudioEngineOp::GetLatency => Ok(StepOutput::AudioLatency(svc.get_latency().await)),
        AudioEngineOp::GetOutputLatencySeconds => {
            Ok(StepOutput::F64(svc.get_output_latency_seconds().await))
        }
        AudioEngineOp::IsRunning => Ok(StepOutput::Bool(svc.is_running().await)),
        AudioEngineOp::GetAudioInputs => {
            Ok(StepOutput::AudioInputInfo(svc.get_audio_inputs().await))
        }
    }
}

// =============================================================================
// Position conversion dispatch
// =============================================================================

async fn dispatch_position_conversion(
    op: &PositionConversionOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::ReaperPositionConversion,
) -> Result<StepOutput, String> {
    use daw_proto::PositionConversionService;
    match op {
        PositionConversionOp::TimeToBeats(p, pos, mode) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::TimeToBeatsResult(
                svc.time_to_beats(ctx, *pos, *mode).await,
            ))
        }
        PositionConversionOp::BeatsToTime(p, pos, mode) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::PositionInSeconds(
                svc.beats_to_time(ctx, *pos, *mode).await,
            ))
        }
        PositionConversionOp::TimeToQuarterNotes(p, pos) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::TimeToQuarterNotesResult(
                svc.time_to_quarter_notes(ctx, *pos).await,
            ))
        }
        PositionConversionOp::QuarterNotesToTime(p, pos) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::PositionInSeconds(
                svc.quarter_notes_to_time(ctx, *pos).await,
            ))
        }
        PositionConversionOp::QuarterNotesToMeasure(p, pos) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::QuarterNotesToMeasureResult(
                svc.quarter_notes_to_measure(ctx, *pos).await,
            ))
        }
        PositionConversionOp::BeatsToQuarterNotes(p, pos) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::PositionInQuarterNotes(
                svc.beats_to_quarter_notes(ctx, *pos).await,
            ))
        }
        PositionConversionOp::QuarterNotesToBeats(p, pos) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::PositionInBeats(
                svc.quarter_notes_to_beats(ctx, *pos).await,
            ))
        }
    }
}

// =============================================================================
// Health dispatch
// =============================================================================

async fn dispatch_health(op: &HealthOp, svc: &crate::ReaperHealth) -> Result<StepOutput, String> {
    use daw_proto::HealthService;
    match op {
        HealthOp::Ping => Ok(StepOutput::Bool(svc.ping().await)),
        HealthOp::ShowConsoleMsg(msg) => {
            svc.show_console_msg(msg.clone()).await;
            Ok(StepOutput::Unit)
        }
    }
}

// =============================================================================
// Action registry dispatch
// =============================================================================

async fn dispatch_action_registry(
    op: &ActionRegistryOp,
    svc: &crate::ReaperActionRegistry,
) -> Result<StepOutput, String> {
    use daw_proto::ActionRegistryService;
    match op {
        ActionRegistryOp::RegisterAction(name, desc, menu, toggle) => Ok(StepOutput::U32(
            svc.register_action(name.clone(), desc.clone(), *menu, *toggle)
                .await,
        )),
        ActionRegistryOp::UnregisterAction(name) => {
            Ok(StepOutput::Bool(svc.unregister_action(name.clone()).await))
        }
        ActionRegistryOp::IsRegistered(name) => {
            Ok(StepOutput::Bool(svc.is_registered(name.clone()).await))
        }
        ActionRegistryOp::LookupCommandId(name) => Ok(StepOutput::OptU32(
            svc.lookup_command_id(name.clone()).await,
        )),
        ActionRegistryOp::IsInActionList(name) => {
            Ok(StepOutput::Bool(svc.is_in_action_list(name.clone()).await))
        }
        ActionRegistryOp::ListActions(request) => Ok(StepOutput::ActionList(
            svc.list_actions(request.clone()).await,
        )),
        ActionRegistryOp::ExecuteCommand(id) => {
            svc.execute_command(*id).await;
            Ok(StepOutput::Unit)
        }
        ActionRegistryOp::ExecuteNamedAction(name) => Ok(StepOutput::Bool(
            svc.execute_named_action(name.clone()).await,
        )),
        ActionRegistryOp::ExecuteAction(action_id) => Ok(StepOutput::ActionExecution(
            svc.execute_action(action_id.clone()).await,
        )),
        ActionRegistryOp::SetToggleState(name, v) => {
            svc.set_toggle_state(name.clone(), *v).await;
            Ok(StepOutput::Unit)
        }
        ActionRegistryOp::GetToggleState(name) => Ok(StepOutput::OptBool(
            svc.get_toggle_state(name.clone()).await,
        )),
    }
}

// =============================================================================
// Toolbar dispatch
// =============================================================================

async fn dispatch_toolbar(
    op: &ToolbarOp,
    svc: &crate::ReaperToolbar,
) -> Result<StepOutput, String> {
    use daw_proto::ToolbarService;
    match op {
        ToolbarOp::AddButton(btn, wf) => Ok(StepOutput::ToolbarResult(
            svc.add_button(btn.clone(), wf.clone()).await,
        )),
        ToolbarOp::UpdateButton(btn, wf) => Ok(StepOutput::ToolbarResult(
            svc.update_button(btn.clone(), wf.clone()).await,
        )),
        ToolbarOp::RemoveButton(target, name) => Ok(StepOutput::ToolbarResult(
            svc.remove_button(target.clone(), name.clone()).await,
        )),
        ToolbarOp::MoveButton(target, name, position) => Ok(StepOutput::ToolbarResult(
            svc.move_button(target.clone(), name.clone(), *position)
                .await,
        )),
        ToolbarOp::SetButtonIcon(target, name, icon) => Ok(StepOutput::ToolbarResult(
            svc.set_button_icon(target.clone(), name.clone(), icon.clone())
                .await,
        )),
        ToolbarOp::RemoveWorkflowButtons(wf) => Ok(StepOutput::ToolbarResult(
            svc.remove_workflow_buttons(wf.clone()).await,
        )),
        ToolbarOp::IsAvailable => Ok(StepOutput::Bool(svc.is_available().await)),
        ToolbarOp::GetTrackedButtons => Ok(StepOutput::TrackedButtonList(
            svc.get_tracked_buttons().await,
        )),
    }
}

// =============================================================================
// Plugin loader dispatch
// =============================================================================

async fn dispatch_plugin_loader(
    op: &PluginLoaderOp,
    svc: &crate::ReaperPluginLoader,
) -> Result<StepOutput, String> {
    use daw_proto::plugin_loader::PluginLoaderService;
    match op {
        PluginLoaderOp::LoadPlugin(path) => Ok(StepOutput::PluginLoadResult(
            svc.load_plugin(path.clone()).await,
        )),
        PluginLoaderOp::ListLoaded => Ok(StepOutput::LoadedPluginInfoList(svc.list_loaded().await)),
        PluginLoaderOp::IsLoaded(path) => Ok(StepOutput::Bool(svc.is_loaded(path.clone()).await)),
    }
}

// =============================================================================
// Peak dispatch
// =============================================================================

async fn dispatch_peak(
    op: &PeakOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::ReaperPeak,
) -> Result<StepOutput, String> {
    use daw_proto::PeakService;
    match op {
        PeakOp::GetTrackPeak(p, t, ch) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Ok(StepOutput::TrackPeak(
                svc.get_track_peak(ctx, tr, *ch).await,
            ))
        }
        PeakOp::GetTakePeaks(p, item, take, bs) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::TakePeakData(
                svc.get_take_peaks(ctx, item.clone(), take.clone(), *bs)
                    .await,
            ))
        }
    }
}

// =============================================================================
// Resource dispatch
// =============================================================================

async fn dispatch_resource(
    op: &ResourceOp,
    svc: &crate::resource::ReaperResource,
) -> Result<StepOutput, String> {
    use daw_proto::resource::ResourceService;
    match op {
        ResourceOp::GetResourcePath => Ok(StepOutput::Path(svc.get_resource_path().await)),
        ResourceOp::GetIniFilePath => Ok(StepOutput::Path(svc.get_ini_file_path().await)),
        ResourceOp::GetColorThemePath => Ok(StepOutput::OptPath(svc.get_color_theme_path().await)),
    }
}

// =============================================================================
// Audio accessor dispatch
// =============================================================================

async fn dispatch_audio_accessor(
    op: &AudioAccessorOp,
    outputs: &[Option<StepOutput>],
    svc: &crate::ReaperAudioAccessor,
) -> Result<StepOutput, String> {
    use daw_proto::AudioAccessorService;
    match op {
        AudioAccessorOp::CreateTrackAccessor(p, t) => {
            let ctx = resolve_project_arg(p, outputs)?;
            let tr = resolve_track_arg(t, outputs)?;
            Ok(StepOutput::OptStr(svc.create_track_accessor(ctx, tr).await))
        }
        AudioAccessorOp::CreateTakeAccessor(p, item, take) => {
            let ctx = resolve_project_arg(p, outputs)?;
            Ok(StepOutput::OptStr(
                svc.create_take_accessor(ctx, item.clone(), take.clone())
                    .await,
            ))
        }
        AudioAccessorOp::HasStateChanged(id) => {
            Ok(StepOutput::Bool(svc.has_state_changed(id.clone()).await))
        }
        AudioAccessorOp::GetSamples(req) => Ok(StepOutput::AudioSampleData(
            svc.get_samples(req.clone()).await,
        )),
        AudioAccessorOp::DestroyAccessor(id) => {
            svc.destroy_accessor(id.clone()).await;
            Ok(StepOutput::Unit)
        }
    }
}

// =============================================================================
// Helpers
// =============================================================================

/// Convert a `Result<(), String>` into a `StepOutput`.
fn result_to_output(result: Result<(), String>) -> Result<StepOutput, String> {
    match result {
        Ok(()) => Ok(StepOutput::Unit),
        Err(e) => Err(e),
    }
}
