//! Structured operations shared by the CLI and MCP server.

use std::path::{Path, PathBuf};

use daw::Daw;
use eyre::Result;
use serde_json::{Value, json};

use crate::{flags_str, format_position, fx_type_str, pan_to_string, resolve_track, vol_to_db};

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

pub async fn open_project(daw: &Daw, path: &str) -> Result<Value> {
    let project = daw.open_project(path).await?;
    let info = project.info().await?;
    Ok(json!({
        "name": info.name,
        "guid": info.guid,
        "path": info.path,
    }))
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
