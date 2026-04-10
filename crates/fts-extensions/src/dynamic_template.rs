//! Dynamic Template integration — in-process track organization, auto-color, and visibility.
//!
//! Ported from the SHM guest process (dynamic-template-extension) to run
//! in-process within FTS-Extensions. Uses the same daw API but via LocalCaller
//! instead of daw-bridge SHM.

use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};

use daw::Daw;
use dynamic_template::{auto_color, default_config, OrganizeIntoTracks};
use dynamic_template_proto::{
    actions::dynamic_template_actions,
    auto_color::actions::auto_color_actions,
    visibility_manager::actions::visibility_manager_actions,
};
use tracing::{info, warn};

use crate::actions::ActionDefs;

// ── State ───────────────────────────────────────────────────────

static STATE: OnceLock<Arc<Mutex<DtState>>> = OnceLock::new();

struct DtState {
    auto_color_enabled: bool,
    group_cache: HashMap<String, String>,
}

fn state() -> Arc<Mutex<DtState>> {
    STATE
        .get_or_init(|| {
            Arc::new(Mutex::new(DtState {
                auto_color_enabled: false,
                group_cache: HashMap::new(),
            }))
        })
        .clone()
}

// ── Action Definitions ──────────────────────────────────────────

/// Build action definitions for all dynamic-template actions.
/// These get registered alongside the other FTS actions.
pub fn build_action_defs() -> ActionDefs {
    let mut defs = Vec::new();

    // Core dynamic-template actions
    for def in dynamic_template_actions::definitions() {
        let cmd_name = def.id.to_command_id();
        let display = def.display_name();
        let name = cmd_name.clone();
        defs.push((
            cmd_name,
            display,
            Arc::new(move || dispatch_action(&name)) as Arc<dyn Fn() + Send + Sync>,
        ));
    }

    // Auto-color actions
    for def in auto_color_actions::definitions() {
        let cmd_name = def.id.to_command_id();
        let display = def.display_name();
        let name = cmd_name.clone();
        defs.push((
            cmd_name,
            display,
            Arc::new(move || dispatch_action(&name)) as Arc<dyn Fn() + Send + Sync>,
        ));
    }

    // Visibility manager actions
    for def in visibility_manager_actions::definitions() {
        let cmd_name = def.id.to_command_id();
        let display = def.display_name();
        let name = cmd_name.clone();
        defs.push((
            cmd_name,
            display,
            Arc::new(move || dispatch_action(&name)) as Arc<dyn Fn() + Send + Sync>,
        ));
    }

    info!("Built {} dynamic-template action defs", defs.len());
    defs
}

/// Subscribe to track events for auto-color re-classification.
/// Call once after DAW is initialized.
pub fn subscribe_track_events(daw: &Daw, tokio_runtime: &tokio::runtime::Runtime) {
    let daw = daw.clone();
    tokio_runtime.spawn(async move {
        let Ok(project) = daw.current_project().await else { return; };
        let Ok(mut track_rx) = project.tracks().subscribe().await else { return; };
        info!("[dynamic-template] Subscribed to track events");

        loop {
            match track_rx.recv().await {
                Ok(Some(event)) => {
                    handle_track_event(&*event, &daw).await;
                }
                Ok(None) | Err(_) => {
                    info!("[dynamic-template] Track event stream ended");
                    break;
                }
            }
        }
    });
}

// ── Action Dispatch ─────────────────────────────────────────────

fn dispatch_action(command_name: &str) {
    let Some(daw) = crate::Global::try_daw() else { return; };
    let state = state();

    // Run async action on the tokio runtime
    if let Some(global) = crate::GLOBAL.get() {
        let daw = daw.clone();
        let command = command_name.to_string();
        global.tokio_runtime.spawn(async move {
            if let Err(e) = handle_action(&command, &daw, &state).await {
                warn!("[dynamic-template] Action {command} failed: {e}");
            }
        });
    }
}

async fn handle_action(
    command_name: &str,
    daw: &Daw,
    state: &Arc<Mutex<DtState>>,
) -> eyre::Result<()> {
    use auto_color_actions as ac;
    use dynamic_template_actions as dt;
    use visibility_manager_actions as vm;

    let sort_selected = dt::SORT_SELECTED.to_id().to_command_id();
    let sort_all = dt::SORT_ALL.to_id().to_command_id();
    let log_status = dt::LOG_STATUS.to_id().to_command_id();
    let log_groups = dt::LOG_GROUPS.to_id().to_command_id();
    let color_all = ac::COLOR_ALL.to_id().to_command_id();
    let color_selected = ac::COLOR_SELECTED.to_id().to_command_id();
    let toggle = ac::TOGGLE.to_id().to_command_id();
    let clear_all = ac::CLEAR_ALL.to_id().to_command_id();
    let clear_selected = ac::CLEAR_SELECTED.to_id().to_command_id();
    let show_all = vm::SHOW_ALL.to_id().to_command_id();
    let hide_all = vm::HIDE_ALL.to_id().to_command_id();
    let rebuild_cache = vm::REBUILD_CACHE.to_id().to_command_id();
    let vis_toggle_prefix = "FTS_VISIBILITY_MANAGER_TOGGLE_";

    match command_name {
        // Sorting
        n if n == sort_selected => sort_tracks(daw, true).await?,
        n if n == sort_all => sort_tracks(daw, false).await?,
        n if n == log_status => {
            let project = daw.current_project().await?;
            let count = project.tracks().count().await?;
            let enabled = state.lock().unwrap().auto_color_enabled;
            info!("[dynamic-template] Status: {count} tracks, auto_color={enabled}");
        }
        n if n == log_groups => {
            let config = default_config();
            let names: Vec<&str> = config.groups.iter().map(|g| g.name.as_str()).collect();
            info!("[dynamic-template] Groups: {names:?}");
        }

        // Auto-color
        n if n == color_all => {
            let count = color_tracks(daw, false).await?;
            state.lock().unwrap().auto_color_enabled = true;
            info!("[dynamic-template] Colored {count} tracks");
        }
        n if n == color_selected => {
            let count = color_tracks(daw, true).await?;
            info!("[dynamic-template] Colored {count} selected tracks");
        }
        n if n == toggle => {
            let enabled = state.lock().unwrap().auto_color_enabled;
            if enabled {
                let count = clear_track_colors(daw, false).await?;
                state.lock().unwrap().auto_color_enabled = false;
                info!("[dynamic-template] Auto-color OFF, cleared {count}");
            } else {
                let count = color_tracks(daw, false).await?;
                state.lock().unwrap().auto_color_enabled = true;
                info!("[dynamic-template] Auto-color ON, colored {count}");
            }
        }
        n if n == clear_all => {
            let count = clear_track_colors(daw, false).await?;
            state.lock().unwrap().auto_color_enabled = false;
            info!("[dynamic-template] Cleared {count} tracks");
        }
        n if n == clear_selected => {
            let count = clear_track_colors(daw, true).await?;
            info!("[dynamic-template] Cleared {count} selected");
        }

        // Visibility
        n if n == show_all => show_all_tracks(daw).await?,
        n if n == hide_all => {
            let mut cache = state.lock().unwrap().group_cache.clone();
            hide_all_group_tracks(daw, &mut cache).await?;
            state.lock().unwrap().group_cache = cache;
        }
        n if n == rebuild_cache => {
            let mut cache = HashMap::new();
            rebuild_group_cache(daw, &mut cache).await?;
            let len = cache.len();
            state.lock().unwrap().group_cache = cache;
            info!("[dynamic-template] Rebuilt cache: {len} entries");
        }
        cmd if cmd.starts_with(vis_toggle_prefix) => {
            let group_name = cmd.strip_prefix(vis_toggle_prefix).unwrap().to_lowercase();
            let mut cache = state.lock().unwrap().group_cache.clone();
            toggle_group_visibility(daw, &mut cache, &group_name).await?;
            state.lock().unwrap().group_cache = cache;
        }

        _ => info!("[dynamic-template] Unhandled: {command_name}"),
    }

    Ok(())
}

async fn handle_track_event(event: &daw::service::TrackEvent, daw: &Daw) {
    match event {
        daw::service::TrackEvent::Added { .. }
        | daw::service::TrackEvent::Removed { .. }
        | daw::service::TrackEvent::Renamed { .. } => {
            let enabled = {
                let s = state();
                let mut locked = s.lock().unwrap();
                locked.group_cache.clear();
                locked.auto_color_enabled
            };

            if enabled {
                if let Ok(n) = color_tracks(daw, false).await {
                    info!("[dynamic-template] Re-colored {n} tracks after change");
                }
            }
        }
        _ => {}
    }
}

// ── Operations (ported from dynamic-template-extension) ─────────

async fn sort_tracks(daw: &Daw, selected_only: bool) -> eyre::Result<()> {
    let project = daw.current_project().await?;
    let tracks = project.tracks();

    let source = if selected_only {
        let handles = tracks.selected().await?;
        let mut infos = Vec::with_capacity(handles.len());
        for h in &handles { infos.push(h.info().await?); }
        infos
    } else {
        tracks.all().await?
    };

    if source.is_empty() { return Ok(()); }

    let names: Vec<String> = source.iter().map(|t| t.name.clone()).collect();
    let config = default_config();
    let hierarchy = names.organize_into_tracks(&config, None)?;

    info!("[dynamic-template] Organized {} → {} tracks", source.len(), hierarchy.tracks.len());
    tracks.apply_hierarchy(hierarchy).await?;
    Ok(())
}

async fn color_tracks(daw: &Daw, selected_only: bool) -> eyre::Result<u32> {
    let project = daw.current_project().await?;
    let tracks = project.tracks();

    let infos = if selected_only {
        let handles = tracks.selected().await?;
        let mut v = Vec::with_capacity(handles.len());
        for h in &handles { v.push(h.info().await?); }
        v
    } else {
        tracks.all().await?
    };

    if infos.is_empty() { return Ok(0); }

    let names: Vec<String> = infos.iter().map(|t| t.name.clone()).collect();
    let color_map = auto_color::classify_and_color(names);

    let mut colored = 0u32;
    for info in &infos {
        if let Some(color) = color_map.get(&info.name) {
            if let Some(handle) = tracks.by_guid(&info.guid).await? {
                handle.set_color(color.to_hex()).await?;
                colored += 1;
            }
        }
    }
    Ok(colored)
}

async fn clear_track_colors(daw: &Daw, selected_only: bool) -> eyre::Result<u32> {
    let project = daw.current_project().await?;
    let tracks = project.tracks();

    let infos = if selected_only {
        let handles = tracks.selected().await?;
        let mut v = Vec::with_capacity(handles.len());
        for h in &handles { v.push(h.info().await?); }
        v
    } else {
        tracks.all().await?
    };

    let mut cleared = 0u32;
    for info in &infos {
        if let Some(handle) = tracks.by_guid(&info.guid).await? {
            handle.set_color(0).await?;
            cleared += 1;
        }
    }
    Ok(cleared)
}

async fn show_all_tracks(daw: &Daw) -> eyre::Result<()> {
    let project = daw.current_project().await?;
    let tracks = project.tracks();
    for track in tracks.all().await? {
        if let Some(handle) = tracks.by_guid(&track.guid).await? {
            handle.show_in_tcp().await?;
            handle.show_in_mixer().await?;
        }
    }
    Ok(())
}

async fn hide_all_group_tracks(daw: &Daw, cache: &mut HashMap<String, String>) -> eyre::Result<()> {
    if cache.is_empty() {
        rebuild_group_cache(daw, cache).await?;
    }
    let project = daw.current_project().await?;
    let tracks = project.tracks();
    for track in tracks.all().await? {
        if cache.contains_key(&track.name) {
            if let Some(handle) = tracks.by_guid(&track.guid).await? {
                handle.hide_in_tcp().await?;
            }
        }
    }
    Ok(())
}

async fn rebuild_group_cache(daw: &Daw, cache: &mut HashMap<String, String>) -> eyre::Result<()> {
    cache.clear();
    let project = daw.current_project().await?;
    let all = project.tracks().all().await?;
    let names: Vec<String> = all.iter().map(|t| t.name.clone()).collect();
    let config = default_config();
    let color_map = auto_color::classify_and_color(names);
    for (name, _) in &color_map {
        cache.insert(name.clone(), String::new());
    }
    Ok(())
}

async fn toggle_group_visibility(
    daw: &Daw,
    cache: &mut HashMap<String, String>,
    group_name: &str,
) -> eyre::Result<()> {
    if cache.is_empty() {
        rebuild_group_cache(daw, cache).await?;
    }

    let config = default_config();
    let group = config.groups.iter().find(|g| g.name.to_lowercase() == group_name);
    let Some(_group) = group else {
        info!("[dynamic-template] Unknown visibility group: {group_name}");
        return Ok(());
    };

    let project = daw.current_project().await?;
    let tracks = project.tracks();
    let all = tracks.all().await?;
    let names: Vec<String> = all.iter().map(|t| t.name.clone()).collect();
    let color_map = auto_color::classify_and_color(names);

    for track in &all {
        if color_map.contains_key(&track.name) {
            if let Some(handle) = tracks.by_guid(&track.guid).await? {
                if track.visible_in_tcp {
                    handle.hide_in_tcp().await?;
                } else {
                    handle.show_in_tcp().await?;
                }
            }
        }
    }

    Ok(())
}
