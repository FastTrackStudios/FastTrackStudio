//! Structured operations shared by DAW CLI commands.

use std::path::{Path, PathBuf};

use daw::Daw;
use eyre::Result;
use serde_json::{Value, json};

use crate::{flags_str, format_position, fx_type_str, pan_to_string, resolve_track, vol_to_db};

fn shape_name(shape: &'static facet::Shape) -> String {
    match shape.module_path {
        Some(module) => format!("{module}::{}", shape.type_identifier),
        None => shape.type_identifier.to_string(),
    }
}

fn service_descriptor_json(service: &'static vox::ServiceDescriptor) -> Value {
    json!({
        "service": service.service_name,
        "doc": service.doc,
        "methods": service.methods.iter().map(|method| json!({
            "id": method.id.0,
            "service": method.service_name,
            "method": method.method_name,
            "doc": method.doc,
            "args_shape": shape_name(method.args_shape),
            "return_shape": shape_name(method.return_shape),
            "retry": {
                "persist": method.retry.persist,
                "idempotent": method.retry.idem,
            },
            "args": method.args.iter().map(|arg| json!({
                "name": arg.name,
                "shape": shape_name(arg.shape),
            })).collect::<Vec<_>>(),
        })).collect::<Vec<_>>(),
    })
}

fn daw_service_descriptors() -> Vec<&'static vox::ServiceDescriptor> {
    vec![
        daw::service::action_registry_service_service_descriptor(),
        daw::service::audio_accessor_service_service_descriptor(),
        daw::service::audio_engine_service_service_descriptor(),
        daw::service::automation_service_service_descriptor(),
        daw::service::batch_service_service_descriptor(),
        daw::service::dock_host_service_service_descriptor(),
        daw::service::ext_state_service_service_descriptor(),
        daw::service::fx_service_service_descriptor(),
        daw::service::health_service_service_descriptor(),
        daw::service::input_service_service_descriptor(),
        daw::service::item_service_service_descriptor(),
        daw::service::live_midi_service_service_descriptor(),
        daw::service::marker_service_service_descriptor(),
        daw::service::midi_analysis_service_service_descriptor(),
        daw::service::midi_service_service_descriptor(),
        daw::service::peak_service_service_descriptor(),
        daw::service::plugin_loader_service_service_descriptor(),
        daw::service::position_conversion_service_service_descriptor(),
        daw::service::project_service_service_descriptor(),
        daw::service::region_service_service_descriptor(),
        daw::service::resource_service_service_descriptor(),
        daw::service::routing_service_service_descriptor(),
        daw::service::take_service_service_descriptor(),
        daw::service::tempo_map_service_service_descriptor(),
        daw::service::toolbar_service_service_descriptor(),
        daw::service::track_service_service_descriptor(),
        daw::service::transport_service_service_descriptor(),
        daw::service::ui_service_service_descriptor(),
    ]
}

pub fn service_catalog() -> Value {
    Value::Array(
        daw_service_descriptors()
            .into_iter()
            .map(service_descriptor_json)
            .collect(),
    )
}

pub async fn execute_batch(daw: &Daw, request: Value) -> Result<Value> {
    let json = serde_json::to_string(&request)?;
    let request: daw::service::BatchRequest = facet_json::from_str(&json)
        .map_err(|err| eyre::eyre!("invalid BatchRequest JSON: {err}"))?;
    let response = daw.execute_batch(request).await?;
    let response_json = facet_json::to_string(&response)
        .map_err(|err| eyre::eyre!("serialize BatchResponse: {err}"))?;
    Ok(serde_json::from_str(&response_json)?)
}

fn fx_param_json(p: &daw::service::FxParameter) -> Value {
    let mut obj = json!({
        "index": p.index,
        "name": p.name,
        "value": p.value,
        "formatted": p.formatted,
        "is_toggle": p.is_toggle,
    });
    if let Some(steps) = p.step_count {
        obj["step_count"] = json!(steps);
    }
    if !p.step_labels.is_empty() {
        obj["step_labels"] = json!(
            p.step_labels
                .iter()
                .map(|(value, label)| json!({ "value": value, "label": label }))
                .collect::<Vec<_>>()
        );
    }
    obj
}

pub async fn project_info(daw: &Daw) -> Result<Value> {
    let project = daw.current_project().await?;
    let info = project.info().await?;
    let track_count = project.n_tracks().await?;
    let transport = project.transport().get_state().await?;

    Ok(json!({
        "name": info.name,
        "path": info.path,
        "guid": info.guid,
        "track_count": track_count,
        "tempo": transport.tempo.bpm,
        "time_signature": {
            "numerator": transport.time_signature.numerator,
            "denominator": transport.time_signature.denominator,
        },
    }))
}

pub async fn tracks(daw: &Daw) -> Result<Value> {
    let project = daw.current_project().await?;
    let all_tracks = project.tracks().all().await?;
    Ok(Value::Array(
        all_tracks
            .iter()
            .map(|t| {
                json!({
                    "index": t.index,
                    "name": t.name,
                    "guid": t.guid,
                    "muted": t.muted,
                    "soloed": t.soloed,
                    "armed": t.armed,
                    "flags": flags_str(t.muted, t.soloed, t.armed),
                    "selected": t.selected,
                    "volume": t.volume,
                    "volume_db": vol_to_db(t.volume),
                    "pan": t.pan,
                    "pan_display": pan_to_string(t.pan),
                    "is_folder": t.is_folder,
                    "folder_depth": t.folder_depth,
                    "fx_count": t.fx_count,
                    "input_fx_count": t.input_fx_count,
                })
            })
            .collect(),
    ))
}

pub async fn track(daw: &Daw, track_arg: &str) -> Result<Value> {
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    let t = handle.info().await?;
    Ok(json!({
        "index": t.index,
        "name": t.name,
        "guid": t.guid,
        "muted": t.muted,
        "soloed": t.soloed,
        "armed": t.armed,
        "flags": flags_str(t.muted, t.soloed, t.armed),
        "selected": t.selected,
        "volume": t.volume,
        "volume_db": vol_to_db(t.volume),
        "pan": t.pan,
        "pan_display": pan_to_string(t.pan),
        "is_folder": t.is_folder,
        "folder_depth": t.folder_depth,
        "parent_guid": t.parent_guid,
        "visible_in_tcp": t.visible_in_tcp,
        "visible_in_mixer": t.visible_in_mixer,
        "fx_count": t.fx_count,
        "input_fx_count": t.input_fx_count,
        "color": t.color,
    }))
}

pub async fn fx(daw: &Daw, track_arg: &str) -> Result<Value> {
    let (guid, track_name) = resolve_track(daw, track_arg).await?;
    let project = daw.current_project().await?;
    let handle = project
        .tracks()
        .by_guid(&guid)
        .await?
        .ok_or_else(|| eyre::eyre!("Track not found"))?;
    let fx_list = handle.fx_chain().all().await?;
    Ok(json!({
        "track": track_name,
        "track_guid": guid,
        "fx": fx_list.iter().map(|f| json!({
            "index": f.index,
            "name": f.name,
            "plugin_name": f.plugin_name,
            "plugin_type": fx_type_str(&f.plugin_type),
            "guid": f.guid,
            "enabled": f.enabled,
            "offline": f.offline,
            "parameter_count": f.parameter_count,
            "preset_name": f.preset_name,
        })).collect::<Vec<_>>(),
    }))
}

pub async fn plugins(daw: &Daw) -> Result<Value> {
    let plugins = daw.installed_plugins().await?;
    Ok(Value::Array(
        plugins
            .iter()
            .map(|p| {
                json!({
                    "name": p.name,
                    "ident": p.ident,
                })
            })
            .collect(),
    ))
}

pub async fn last_touched_fx(daw: &Daw) -> Result<Value> {
    let touched = daw.last_touched_fx().await?;
    Ok(json!({
        "last_touched_fx": touched.as_ref().map(|fx| format!("{fx:?}")),
    }))
}

pub async fn fx_params(daw: &Daw, track_arg: &str, fx_arg: &str) -> Result<Value> {
    let (_, track_name) = resolve_track(daw, track_arg).await?;
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = crate::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;
    let fx_info = fx_handle.info().await?;
    let params = fx_handle.parameters().await?;
    Ok(json!({
        "track": track_name,
        "fx": {
            "index": fx_info.index,
            "name": fx_info.name,
            "guid": fx_info.guid,
        },
        "parameters": params.iter().map(fx_param_json).collect::<Vec<_>>(),
    }))
}

pub async fn fx_set_param(
    daw: &Daw,
    track_arg: &str,
    fx_arg: &str,
    param: u32,
    value: f64,
) -> Result<Value> {
    let (_, track_name) = resolve_track(daw, track_arg).await?;
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = crate::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;
    fx_handle.param(param).set(value).await?;
    let updated = fx_handle.param(param).info().await?;
    Ok(json!({
        "track": track_name,
        "fx_guid": fx_handle.guid(),
        "parameter": fx_param_json(&updated),
    }))
}

pub async fn fx_set_param_by_name(
    daw: &Daw,
    track_arg: &str,
    fx_arg: &str,
    param: &str,
    value: f64,
) -> Result<Value> {
    let (_, track_name) = resolve_track(daw, track_arg).await?;
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = crate::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;
    fx_handle.param_by_name(param).set(value).await?;
    let updated = fx_handle.param_by_name(param).info().await?;
    Ok(json!({
        "track": track_name,
        "fx_guid": fx_handle.guid(),
        "parameter": fx_param_json(&updated),
    }))
}

pub async fn fx_add(
    daw: &Daw,
    track_arg: &str,
    fx_name: &str,
    at_index: Option<u32>,
) -> Result<Value> {
    let (_, track_name) = resolve_track(daw, track_arg).await?;
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    let fx_handle = match at_index {
        Some(index) => handle.fx_chain().add_at(fx_name, index).await?,
        None => handle.fx_chain().add(fx_name).await?,
    };
    let info = fx_handle.info().await?;
    Ok(json!({
        "track": track_name,
        "fx": {
            "index": info.index,
            "name": info.name,
            "plugin_name": info.plugin_name,
            "guid": info.guid,
            "enabled": info.enabled,
        }
    }))
}

pub async fn fx_remove(daw: &Daw, track_arg: &str, fx_arg: &str) -> Result<Value> {
    let (_, track_name) = resolve_track(daw, track_arg).await?;
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = crate::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;
    let info = fx_handle.info().await?;
    fx_handle.remove().await?;
    Ok(json!({
        "removed": true,
        "track": track_name,
        "fx": {
            "index": info.index,
            "name": info.name,
            "guid": info.guid,
        }
    }))
}

pub async fn fx_set_enabled(
    daw: &Daw,
    track_arg: &str,
    fx_arg: &str,
    enabled: bool,
) -> Result<Value> {
    let (_, track_name) = resolve_track(daw, track_arg).await?;
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = crate::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;
    if enabled {
        fx_handle.enable().await?;
    } else {
        fx_handle.disable().await?;
    }
    let info = fx_handle.info().await?;
    Ok(json!({
        "track": track_name,
        "fx_guid": info.guid,
        "enabled": info.enabled,
    }))
}

pub async fn fx_move(daw: &Daw, track_arg: &str, fx_arg: &str, index: u32) -> Result<Value> {
    let (_, track_name) = resolve_track(daw, track_arg).await?;
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = crate::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;
    fx_handle.move_to(index).await?;
    let info = fx_handle.info().await?;
    Ok(json!({
        "track": track_name,
        "fx_guid": info.guid,
        "index": info.index,
    }))
}

pub async fn fx_ui(daw: &Daw, track_arg: &str, fx_arg: &str, action: &str) -> Result<Value> {
    let (_, track_name) = resolve_track(daw, track_arg).await?;
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = crate::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;
    match action {
        "open" => fx_handle.open_ui().await?,
        "close" => fx_handle.close_ui().await?,
        "toggle" => fx_handle.toggle_ui().await?,
        _ => eyre::bail!("unknown FX UI action: {action}"),
    }
    let info = fx_handle.info().await?;
    Ok(json!({
        "track": track_name,
        "fx_guid": info.guid,
        "window_open": info.window_open,
    }))
}

pub async fn fx_preset(
    daw: &Daw,
    track_arg: &str,
    fx_arg: &str,
    action: &str,
    index: Option<u32>,
) -> Result<Value> {
    let (_, track_name) = resolve_track(daw, track_arg).await?;
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = crate::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;
    match action {
        "get" => {}
        "next" => fx_handle.next_preset().await?,
        "previous" | "prev" => fx_handle.prev_preset().await?,
        "set" => {
            fx_handle
                .set_preset(index.ok_or_else(|| eyre::eyre!("preset index is required"))?)
                .await?
        }
        _ => eyre::bail!("unknown preset action: {action}"),
    }
    Ok(json!({
        "track": track_name,
        "fx_guid": fx_handle.guid(),
        "preset": fx_handle.preset_index().await?.map(|preset| format!("{preset:?}")),
    }))
}

pub async fn transport(daw: &Daw) -> Result<Value> {
    let project = daw.current_project().await?;
    let state = project.transport().get_state().await?;
    Ok(json!({
        "play_state": format!("{:?}", state.play_state),
        "record_mode": format!("{:?}", state.record_mode),
        "looping": state.looping,
        "tempo": state.tempo.bpm,
        "playrate": state.playrate,
        "time_signature": {
            "numerator": state.time_signature.numerator,
            "denominator": state.time_signature.denominator,
        },
        "playhead": format_position(&state.playhead_position),
        "edit_cursor": format_position(&state.edit_position),
        "loop_region": state.loop_region.as_ref().map(|lr| json!({
            "start_seconds": lr.start_seconds,
            "end_seconds": lr.end_seconds,
        })),
    }))
}

pub async fn transport_control(daw: &Daw, action: &str) -> Result<Value> {
    let project = daw.current_project().await?;
    let transport = project.transport();
    match action {
        "play" => transport.play().await?,
        "pause" => transport.pause().await?,
        "stop" => transport.stop().await?,
        "play_pause" => transport.play_pause().await?,
        "play_stop" => transport.play_stop().await?,
        "record" => transport.record().await?,
        "stop_recording" => transport.stop_recording().await?,
        "toggle_recording" => transport.toggle_recording().await?,
        "goto_start" => transport.goto_start().await?,
        "goto_end" => transport.goto_end().await?,
        "toggle_loop" => transport.toggle_loop().await?,
        _ => eyre::bail!("unknown transport action: {action}"),
    }
    transport_state_for_project(&project).await
}

async fn transport_state_for_project(project: &daw::Project) -> Result<Value> {
    let state = project.transport().get_state().await?;
    Ok(json!({
        "play_state": format!("{:?}", state.play_state),
        "record_mode": format!("{:?}", state.record_mode),
        "looping": state.looping,
        "tempo": state.tempo.bpm,
        "playrate": state.playrate,
        "time_signature": {
            "numerator": state.time_signature.numerator,
            "denominator": state.time_signature.denominator,
        },
        "playhead": format_position(&state.playhead_position),
        "edit_cursor": format_position(&state.edit_position),
    }))
}

pub async fn transport_set_position(daw: &Daw, seconds: f64) -> Result<Value> {
    let project = daw.current_project().await?;
    project.transport().set_position(seconds).await?;
    transport_state_for_project(&project).await
}

pub async fn transport_set_tempo(daw: &Daw, bpm: f64) -> Result<Value> {
    let project = daw.current_project().await?;
    project.transport().set_tempo(bpm).await?;
    transport_state_for_project(&project).await
}

pub async fn transport_set_loop(daw: &Daw, enabled: bool) -> Result<Value> {
    let project = daw.current_project().await?;
    project.transport().set_loop(enabled).await?;
    transport_state_for_project(&project).await
}

pub async fn transport_set_playrate(daw: &Daw, rate: f64) -> Result<Value> {
    let project = daw.current_project().await?;
    project.transport().set_playrate(rate).await?;
    transport_state_for_project(&project).await
}

pub async fn transport_goto_measure(daw: &Daw, measure: i32) -> Result<Value> {
    let project = daw.current_project().await?;
    project.transport().goto_measure(measure).await?;
    transport_state_for_project(&project).await
}

pub async fn markers(daw: &Daw) -> Result<Value> {
    let project = daw.current_project().await?;
    let markers = project.markers().all().await?;
    Ok(Value::Array(
        markers
            .iter()
            .map(|m| {
                json!({
                    "id": m.id,
                    "name": m.name,
                    "position": format_position(&m.position),
                    "color": m.color,
                    "guid": m.guid,
                })
            })
            .collect(),
    ))
}

pub async fn regions(daw: &Daw) -> Result<Value> {
    let project = daw.current_project().await?;
    let regions = project.regions().all().await?;
    Ok(Value::Array(
        regions
            .iter()
            .map(|r| {
                json!({
                    "id": r.id,
                    "name": r.name,
                    "start": format_position(&r.time_range.start),
                    "end": format_position(&r.time_range.end),
                    "color": r.color,
                    "guid": r.guid,
                })
            })
            .collect(),
    ))
}

pub async fn marker_add(daw: &Daw, position: f64, name: &str, lane: Option<u32>) -> Result<Value> {
    let project = daw.current_project().await?;
    let id = match lane {
        Some(lane) => project.markers().add_in_lane(position, name, lane).await?,
        None => project.markers().add(position, name).await?,
    };
    Ok(json!({ "id": id, "position_seconds": position, "name": name }))
}

pub async fn marker_remove(daw: &Daw, id: u32) -> Result<Value> {
    let project = daw.current_project().await?;
    project.markers().remove(id).await?;
    Ok(json!({ "removed": true, "id": id }))
}

pub async fn marker_move(daw: &Daw, id: u32, position: f64) -> Result<Value> {
    let project = daw.current_project().await?;
    project.markers().move_to(id, position).await?;
    Ok(json!({ "id": id, "position_seconds": position }))
}

pub async fn marker_rename(daw: &Daw, id: u32, name: &str) -> Result<Value> {
    let project = daw.current_project().await?;
    project.markers().rename(id, name).await?;
    Ok(json!({ "id": id, "name": name }))
}

pub async fn region_add(
    daw: &Daw,
    start: f64,
    end: f64,
    name: &str,
    lane: Option<u32>,
) -> Result<Value> {
    let project = daw.current_project().await?;
    let id = match lane {
        Some(lane) => {
            project
                .regions()
                .add_in_lane(start, end, name, lane)
                .await?
        }
        None => project.regions().add(start, end, name).await?,
    };
    Ok(json!({ "id": id, "start_seconds": start, "end_seconds": end, "name": name }))
}

pub async fn region_remove(daw: &Daw, id: u32) -> Result<Value> {
    let project = daw.current_project().await?;
    project.regions().remove(id).await?;
    Ok(json!({ "removed": true, "id": id }))
}

pub async fn region_set_bounds(daw: &Daw, id: u32, start: f64, end: f64) -> Result<Value> {
    let project = daw.current_project().await?;
    project.regions().set_bounds(id, start, end).await?;
    Ok(json!({ "id": id, "start_seconds": start, "end_seconds": end }))
}

pub async fn region_rename(daw: &Daw, id: u32, name: &str) -> Result<Value> {
    let project = daw.current_project().await?;
    project.regions().rename(id, name).await?;
    Ok(json!({ "id": id, "name": name }))
}

pub async fn projects(daw: &Daw) -> Result<Value> {
    let projects = daw.projects().await?;
    let mut arr = Vec::new();
    for (i, p) in projects.iter().enumerate() {
        let info = p.info().await?;
        arr.push(json!({
            "index": i,
            "name": info.name,
            "guid": info.guid,
            "path": info.path,
        }));
    }
    Ok(Value::Array(arr))
}

pub async fn create_project(daw: &Daw) -> Result<Value> {
    let project = daw.create_project().await?;
    let info = project.info().await?;
    Ok(json!({ "name": info.name, "guid": info.guid, "path": info.path }))
}

pub async fn select_project(daw: &Daw, guid: &str) -> Result<Value> {
    let project = daw.select_project(guid).await?;
    let info = project.info().await?;
    Ok(json!({ "selected": true, "name": info.name, "guid": info.guid, "path": info.path }))
}

pub async fn open_project(daw: &Daw, path: &str) -> Result<Value> {
    let project = daw.open_project(path).await?;
    let info = project.info().await?;
    Ok(json!({
        "name": info.name,
        "guid": info.guid,
        "path": info.path,
    }))
}

pub async fn close_project(daw: &Daw, guid: Option<&str>) -> Result<Value> {
    let target_guid = match guid {
        Some(guid) => guid.to_string(),
        None => daw.current_project().await?.info().await?.guid,
    };
    daw.close_project(&target_guid).await?;
    Ok(json!({ "closed": true, "guid": target_guid }))
}

pub async fn save_project(daw: &Daw) -> Result<Value> {
    let project = daw.current_project().await?;
    let info = project.info().await?;
    project.save().await?;
    Ok(json!({ "saved": true, "guid": info.guid, "path": info.path }))
}

pub async fn save_all_projects(daw: &Daw) -> Result<Value> {
    daw.save_all_projects().await?;
    Ok(json!({ "saved_all": true }))
}

pub async fn project_undo(daw: &Daw) -> Result<Value> {
    let project = daw.current_project().await?;
    Ok(json!({ "undone": project.undo().await? }))
}

pub async fn project_redo(daw: &Daw) -> Result<Value> {
    let project = daw.current_project().await?;
    Ok(json!({ "redone": project.redo().await? }))
}

pub async fn project_run_command(daw: &Daw, command: &str) -> Result<Value> {
    let project = daw.current_project().await?;
    Ok(json!({ "command": command, "executed": project.run_command(command).await? }))
}

pub async fn project_info_string(daw: &Daw, key: &str, value: Option<&str>) -> Result<Value> {
    let project = daw.current_project().await?;
    if let Some(value) = value {
        project.set_info_string(key, value).await?;
    }
    Ok(json!({ "key": key, "value": project.get_info_string(key).await? }))
}

pub async fn project_info_number(daw: &Daw, key: &str, value: Option<f64>) -> Result<Value> {
    let project = daw.current_project().await?;
    if let Some(value) = value {
        project.set_info(key, value).await?;
    }
    Ok(json!({ "key": key, "value": project.get_info(key).await? }))
}

pub async fn add_track(daw: &Daw, name: Option<&str>, at_index: Option<u32>) -> Result<Value> {
    let project = daw.current_project().await?;
    let handle = project
        .tracks()
        .add(name.unwrap_or("New Track"), at_index)
        .await?;
    let info = handle.info().await?;
    Ok(json!({
        "index": info.index,
        "name": info.name,
        "guid": info.guid,
    }))
}

pub async fn track_set(daw: &Daw, track_arg: &str, field: &str, value: Value) -> Result<Value> {
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    match field {
        "muted" => {
            if value
                .as_bool()
                .ok_or_else(|| eyre::eyre!("muted expects bool"))?
            {
                handle.mute().await?
            } else {
                handle.unmute().await?
            }
        }
        "soloed" => {
            if value
                .as_bool()
                .ok_or_else(|| eyre::eyre!("soloed expects bool"))?
            {
                handle.solo().await?
            } else {
                handle.unsolo().await?
            }
        }
        "armed" => {
            if value
                .as_bool()
                .ok_or_else(|| eyre::eyre!("armed expects bool"))?
            {
                handle.arm().await?
            } else {
                handle.disarm().await?
            }
        }
        "selected" => {
            if value
                .as_bool()
                .ok_or_else(|| eyre::eyre!("selected expects bool"))?
            {
                handle.select().await?
            } else {
                handle.deselect().await?
            }
        }
        "volume" => {
            handle
                .set_volume(
                    value
                        .as_f64()
                        .ok_or_else(|| eyre::eyre!("volume expects number"))?,
                )
                .await?
        }
        "pan" => {
            handle
                .set_pan(
                    value
                        .as_f64()
                        .ok_or_else(|| eyre::eyre!("pan expects number"))?,
                )
                .await?
        }
        "name" => {
            handle
                .rename(
                    value
                        .as_str()
                        .ok_or_else(|| eyre::eyre!("name expects string"))?,
                )
                .await?
        }
        "color" => {
            handle
                .set_color(
                    value
                        .as_u64()
                        .ok_or_else(|| eyre::eyre!("color expects integer"))?
                        as u32,
                )
                .await?
        }
        "folder_depth" => {
            handle
                .set_folder_depth(
                    value
                        .as_i64()
                        .ok_or_else(|| eyre::eyre!("folder_depth expects integer"))?
                        as i32,
                )
                .await?
        }
        "num_channels" => {
            handle
                .set_num_channels(
                    value
                        .as_u64()
                        .ok_or_else(|| eyre::eyre!("num_channels expects integer"))?
                        as u32,
                )
                .await?
        }
        "visible_in_tcp" => {
            handle
                .set_visible_in_tcp(
                    value
                        .as_bool()
                        .ok_or_else(|| eyre::eyre!("visible_in_tcp expects bool"))?,
                )
                .await?
        }
        "visible_in_mixer" => {
            handle
                .set_visible_in_mixer(
                    value
                        .as_bool()
                        .ok_or_else(|| eyre::eyre!("visible_in_mixer expects bool"))?,
                )
                .await?
        }
        "parent_send" => {
            handle
                .set_parent_send(
                    value
                        .as_bool()
                        .ok_or_else(|| eyre::eyre!("parent_send expects bool"))?,
                )
                .await?
        }
        _ => eyre::bail!("unsupported track field: {field}"),
    }
    let info = handle.info().await?;
    Ok(json!({
        "index": info.index,
        "name": info.name,
        "guid": info.guid,
        "muted": info.muted,
        "soloed": info.soloed,
        "armed": info.armed,
        "selected": info.selected,
        "volume": info.volume,
        "pan": info.pan,
        "color": info.color,
        "visible_in_tcp": info.visible_in_tcp,
        "visible_in_mixer": info.visible_in_mixer,
    }))
}

pub async fn track_move(daw: &Daw, track_arg: &str, index: u32) -> Result<Value> {
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    handle.move_to_index(index).await?;
    let info = handle.info().await?;
    Ok(json!({ "guid": info.guid, "index": info.index, "name": info.name }))
}

pub async fn track_ext_state(
    daw: &Daw,
    track_arg: &str,
    section: &str,
    key: &str,
    value: Option<&str>,
) -> Result<Value> {
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    if let Some(value) = value {
        handle.set_ext_state(section, key, value).await?;
    }
    Ok(
        json!({ "section": section, "key": key, "value": handle.get_ext_state(section, key).await? }),
    )
}

pub async fn track_delete_ext_state(
    daw: &Daw,
    track_arg: &str,
    section: &str,
    key: &str,
) -> Result<Value> {
    let handle = crate::resolve_track_handle(daw, track_arg).await?;
    handle.delete_ext_state(section, key).await?;
    Ok(json!({ "deleted": true, "section": section, "key": key }))
}

pub async fn remove_track(daw: &Daw, track_arg: &str) -> Result<Value> {
    let (guid, name) = resolve_track(daw, track_arg).await?;
    let project = daw.current_project().await?;
    project
        .tracks()
        .remove(daw::service::TrackRef::Guid(guid.clone()))
        .await?;
    Ok(json!({
        "removed": true,
        "name": name,
        "guid": guid,
    }))
}

pub async fn ext_state_get(daw: &Daw, section: &str, key: &str) -> Result<Value> {
    let ext = daw.ext_state();
    Ok(json!({ "section": section, "key": key, "value": ext.get(section, key).await? }))
}

pub async fn ext_state_set(
    daw: &Daw,
    section: &str,
    key: &str,
    value: &str,
    persist: bool,
) -> Result<Value> {
    let ext = daw.ext_state();
    ext.set(section, key, value, persist).await?;
    Ok(json!({ "section": section, "key": key, "value": value, "persist": persist }))
}

pub async fn ext_state_delete(daw: &Daw, section: &str, key: &str, persist: bool) -> Result<Value> {
    let ext = daw.ext_state();
    ext.delete(section, key, persist).await?;
    Ok(json!({ "deleted": true, "section": section, "key": key, "persist": persist }))
}

pub async fn audio_engine(daw: &Daw) -> Result<Value> {
    let engine = daw.audio_engine();
    Ok(json!({
        "state": format!("{:?}", engine.get_state().await?),
        "latency": format!("{:?}", engine.get_latency().await?),
        "output_latency_seconds": engine.output_latency_seconds().await?,
        "is_running": engine.is_running().await?,
        "inputs": format!("{:?}", engine.get_audio_inputs().await?),
    }))
}

pub async fn audio_engine_control(daw: &Daw, action: &str) -> Result<Value> {
    let engine = daw.audio_engine();
    match action {
        "init" => engine.init().await?,
        "quit" => engine.quit().await?,
        _ => eyre::bail!("unknown audio engine action: {action}"),
    }
    Ok(json!({ "action": action, "is_running": engine.is_running().await? }))
}

pub async fn plugin_loader_load(daw: &Daw, path: &str) -> Result<Value> {
    Ok(
        json!({ "path": path, "result": format!("{:?}", daw.plugin_loader().load_plugin(path).await?) }),
    )
}

pub async fn plugin_loader_list(daw: &Daw) -> Result<Value> {
    Ok(json!({ "loaded": format!("{:?}", daw.plugin_loader().list_loaded().await?) }))
}

pub async fn action_execute(daw: &Daw, action_id: &str) -> Result<Value> {
    let result = daw
        .action_registry()
        .execute_action_detailed(action_id)
        .await?;
    Ok(json!({
        "requested_action": result.requested_action,
        "executed": result.executed,
        "command_id": result.command_id,
        "command_name": result.command_name,
        "description": result.description,
        "origin": result.origin.map(|origin| format!("{origin:?}")),
        "provider": result.provider,
        "provider_tags": result.provider_tags,
        "registered_by_fts": result.registered_by_fts,
        "toggle_state_before": result.toggle_state_before,
        "toggle_state_after": result.toggle_state_after,
    }))
}

struct ActionAlias {
    alias: &'static str,
    action_id: &'static str,
    provider: &'static str,
    description: &'static str,
}

const ACTION_ALIASES: &[ActionAlias] = &[
    ActionAlias {
        alias: "transport.play",
        action_id: "1007",
        provider: "reaper",
        description: "Transport: Play",
    },
    ActionAlias {
        alias: "transport.stop",
        action_id: "1016",
        provider: "reaper",
        description: "Transport: Stop",
    },
    ActionAlias {
        alias: "transport.pause",
        action_id: "1008",
        provider: "reaper",
        description: "Transport: Pause",
    },
    ActionAlias {
        alias: "transport.record",
        action_id: "1013",
        provider: "reaper",
        description: "Transport: Record",
    },
    ActionAlias {
        alias: "transport.play_stop",
        action_id: "40044",
        provider: "reaper",
        description: "Transport: Play/stop",
    },
    ActionAlias {
        alias: "edit.undo",
        action_id: "40029",
        provider: "reaper",
        description: "Edit: Undo",
    },
    ActionAlias {
        alias: "edit.redo",
        action_id: "40030",
        provider: "reaper",
        description: "Edit: Redo",
    },
    ActionAlias {
        alias: "marker.insert",
        action_id: "40157",
        provider: "reaper",
        description: "Markers: Insert marker at current position",
    },
    ActionAlias {
        alias: "region.insert",
        action_id: "40306",
        provider: "reaper",
        description: "Regions: Insert region from time selection",
    },
];

fn action_alias(alias: &str) -> Option<&'static ActionAlias> {
    ACTION_ALIASES
        .iter()
        .find(|entry| entry.alias.eq_ignore_ascii_case(alias))
}

pub fn action_aliases() -> Value {
    json!({
        "count": ACTION_ALIASES.len(),
        "aliases": ACTION_ALIASES.iter().map(|entry| json!({
            "alias": entry.alias,
            "action_id": entry.action_id,
            "provider": entry.provider,
            "description": entry.description,
        })).collect::<Vec<_>>(),
    })
}

pub async fn action_execute_alias(daw: &Daw, alias: &str) -> Result<Value> {
    let entry = action_alias(alias).ok_or_else(|| eyre::eyre!("unknown action alias: {alias}"))?;
    let mut value = action_execute(daw, entry.action_id).await?;
    value["alias"] = json!(entry.alias);
    value["alias_description"] = json!(entry.description);
    value["alias_provider"] = json!(entry.provider);
    Ok(value)
}

pub async fn action_lookup(daw: &Daw, command_name: &str) -> Result<Value> {
    let registry = daw.action_registry();
    Ok(json!({
        "command_name": command_name,
        "registered": registry.is_registered(command_name).await?,
        "in_action_list": registry.is_in_action_list(command_name).await?,
        "command_id": registry.lookup_command_id(command_name).await?,
        "toggle_state": registry.get_toggle_state(command_name).await?,
    }))
}

pub async fn action_list(
    daw: &Daw,
    filter: &str,
    section: &str,
    query: Option<&str>,
    limit: Option<u32>,
) -> Result<Value> {
    let filter = match filter.trim().to_ascii_lowercase().as_str() {
        "all" => daw::service::ActionListFilter::All,
        "reaper" | "native" | "built-in" | "builtin" => daw::service::ActionListFilter::Reaper,
        "non-reaper" | "nonreaper" | "extension" | "extensions" | "custom" => {
            daw::service::ActionListFilter::NonReaper
        }
        "sws" | "sws/s&m" | "s&m" => daw::service::ActionListFilter::Sws,
        "fts" | "fasttrackstudio" => daw::service::ActionListFilter::Fts,
        "registered" | "local" => daw::service::ActionListFilter::Registered,
        _ => eyre::bail!("action filter must be all, reaper, non-reaper, sws, fts, or registered"),
    };
    let section = parse_action_section(section)?;

    let request = daw::service::ActionListRequest {
        filter,
        section,
        query: query.map(str::to_string),
        limit,
    };
    let response = daw.action_registry().list_actions(request).await?;
    Ok(json!({
        "filter": format!("{filter:?}"),
        "section": {
            "id": section.unique_id(),
            "name": section.name(),
        },
        "query": query,
        "count": response.actions.len(),
        "total_count": response.total_count,
        "limited": limit.is_some_and(|limit| response.total_count > limit),
        "actions": response.actions.iter().map(|action| json!({
            "command_id": action.command_id,
            "section_id": action.section_id,
            "section_name": action.section_name,
            "command_name": action.command_name,
            "description": action.description,
            "origin": format!("{:?}", action.origin),
            "provider": action.provider,
            "provider_tags": action.provider_tags,
            "registered_by_fts": action.registered_by_fts,
            "toggle_state": action.toggle_state,
        })).collect::<Vec<_>>(),
    }))
}

fn parse_action_section(section: &str) -> Result<daw::service::ActionSection> {
    match section.trim().to_ascii_lowercase().as_str() {
        "main" | "0" => Ok(daw::service::ActionSection::Main),
        "main-alt" | "main_alt" | "100" => Ok(daw::service::ActionSection::MainAlt),
        "midi-editor" | "midi_editor" | "midi" | "32060" => {
            Ok(daw::service::ActionSection::MidiEditor)
        }
        "midi-event-list-editor" | "midi_event_list_editor" | "midi-event-list" | "32061" => {
            Ok(daw::service::ActionSection::MidiEventListEditor)
        }
        "midi-inline-editor" | "midi_inline_editor" | "midi-inline" | "32062" => {
            Ok(daw::service::ActionSection::MidiInlineEditor)
        }
        "media-explorer" | "media_explorer" | "explorer" | "32063" => {
            Ok(daw::service::ActionSection::MediaExplorer)
        }
        raw => raw
            .parse::<u32>()
            .map(daw::service::ActionSection::Custom)
            .map_err(|_| {
                eyre::eyre!(
                    "action section must be main, main-alt, midi-editor, midi-event-list-editor, midi-inline-editor, media-explorer, or a numeric section ID"
                )
            }),
    }
}

pub async fn action_set_toggle(daw: &Daw, command_name: &str, is_on: bool) -> Result<Value> {
    daw.action_registry()
        .set_toggle_state(command_name, is_on)
        .await?;
    Ok(
        json!({ "command_name": command_name, "toggle_state": daw.action_registry().get_toggle_state(command_name).await? }),
    )
}

pub async fn toolbar_status(daw: &Daw) -> Result<Value> {
    let toolbar = daw.toolbar();
    Ok(json!({
        "available": toolbar.is_available().await?,
        "tracked_buttons": format!("{:?}", toolbar.get_tracked_buttons().await?),
    }))
}

pub fn rpp_summary(path: &str) -> Result<Value> {
    let content = std::fs::read_to_string(path)?;
    let project = dawfile_reaper::parse_project_text(&content)
        .map_err(|e| eyre::eyre!("parse RPP {path}: {e}"))?;
    Ok(json!({
        "path": path,
        "version": project.version,
        "version_string": project.version_string,
        "track_count": project.tracks.len(),
        "marker_count": project.markers_regions.markers.len(),
        "region_count": project.markers_regions.regions.len(),
        "tracks": project.tracks.iter().map(|t| json!({
            "name": t.name,
            "items": t.items.len(),
            "fx_count": t.fx_chain.as_ref().map(|fx| fx.plugin_count()).unwrap_or(0),
        })).collect::<Vec<_>>(),
    }))
}

pub fn combine_rpl(input: &str, output: Option<&str>, gap_measures: u32) -> Result<Value> {
    use dawfile_reaper::setlist_rpp::{self, CombineOptions};

    let input_path = Path::new(input);
    if !input_path.exists() {
        eyre::bail!("Input file not found: {}", input);
    }

    let output_path = output.map(PathBuf::from).unwrap_or_else(|| {
        let stem = input_path.file_stem().unwrap_or_default();
        let parent = input_path.parent().unwrap_or(Path::new("."));
        parent.join(format!("{}.RPP", stem.to_string_lossy()))
    });

    let options = CombineOptions {
        gap_measures,
        trim_to_bounds: false,
    };
    let (combined, song_infos) = setlist_rpp::combine_rpl(input_path, &options)?;
    std::fs::write(&output_path, &combined)?;

    Ok(json!({
        "input": input,
        "output": output_path.display().to_string(),
        "song_count": song_infos.len(),
        "gap_measures": gap_measures,
        "songs": song_infos.iter().enumerate().map(|(i, info)| json!({
            "index": i + 1,
            "name": info.name,
            "global_start_seconds": info.global_start_seconds,
            "duration_seconds": info.duration_seconds,
        })).collect::<Vec<_>>(),
        "total_seconds": song_infos.last().map(|info| info.global_start_seconds + info.duration_seconds).unwrap_or(0.0),
    }))
}
