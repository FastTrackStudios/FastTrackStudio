//! Dynamic Template domain SHM guest process.
//!
//! Connects to REAPER via daw-bridge SHM and manages dynamic template state:
//! auto-color classification, visibility manager, and template sorting.
//!
//! Registers dynamic-template-domain actions with REAPER and handles their
//! execution locally when triggered. The host (daw-bridge) is domain-agnostic.
//!
//! Placed in `UserPlugins/fts-extensions/` and hot-reloaded by daw-bridge.

use std::collections::HashMap;

use daw::Daw;
use daw_extension_runtime::GuestOptions;
use dynamic_template::{OrganizeIntoTracks, auto_color, default_config};
use dynamic_template_proto::{
    actions::dynamic_template_actions, auto_color::actions::auto_color_actions,
    visibility_manager::actions::visibility_manager_actions,
};
use eyre::Result;
use tracing::{debug, info, warn};

fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into()),
        )
        .init();

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;

    rt.block_on(run())
}

async fn run() -> Result<()> {
    let pid = std::process::id();
    info!("[dynamic-template:{pid}] Dynamic Template extension starting");

    let daw = daw_extension_runtime::connect(GuestOptions {
        role: "dynamic-template",
        ..Default::default()
    })
    .await?;

    debug!("[dynamic-template:{pid}] Connected to REAPER via SHM");

    // Signal that we're alive — tests read this to verify the extension connected
    daw.ext_state()
        .set("FTS_DYNAMIC_TEMPLATE_EXT", "status", "ready", false)
        .await?;
    daw.ext_state()
        .set("FTS_DYNAMIC_TEMPLATE_EXT", "pid", &pid.to_string(), false)
        .await?;
    debug!("[dynamic-template:{pid}] Health beacon written");

    // Register dynamic-template-domain actions with REAPER.
    // Action definitions live in dynamic-template-proto — single source of truth.
    let registry = daw.action_registry();

    let mut total = 0usize;
    let mut registered = 0usize;

    // Core dynamic-template actions (sort selected, sort all, import & sort, etc.)
    for def in dynamic_template_actions::definitions() {
        total += 1;
        let cmd_name = def.id.to_command_id();
        let cmd_id = registry.register(&cmd_name, &def.display_name()).await?;
        if cmd_id == 0 {
            warn!("[dynamic-template:{pid}] Failed to register action: {cmd_name}");
        } else {
            registered += 1;
        }
    }

    // Auto-color actions (color all, color selected, toggle, clear)
    for def in auto_color_actions::definitions() {
        total += 1;
        let cmd_name = def.id.to_command_id();
        let cmd_id = registry.register(&cmd_name, &def.display_name()).await?;
        if cmd_id == 0 {
            warn!("[dynamic-template:{pid}] Failed to register action: {cmd_name}");
        } else {
            registered += 1;
        }
    }

    // Visibility manager actions (per-group toggles, show/hide all, rebuild cache)
    for def in visibility_manager_actions::definitions() {
        total += 1;
        let cmd_name = def.id.to_command_id();
        let cmd_id = registry.register(&cmd_name, &def.display_name()).await?;
        if cmd_id == 0 {
            warn!("[dynamic-template:{pid}] Failed to register action: {cmd_name}");
        } else {
            registered += 1;
        }
    }
    info!("[dynamic-template:{pid}] Registered {registered}/{total} dynamic-template actions");

    // Subscribe to action trigger events and handle them locally.
    let mut action_rx = registry.subscribe_actions().await?;
    debug!("[dynamic-template:{pid}] Subscribed to action events");

    // Subscribe to track events for auto-color (re-classify when tracks change).
    let project = daw.current_project().await?;
    let mut track_rx = project.tracks().subscribe().await?;
    debug!("[dynamic-template:{pid}] Subscribed to track events");

    // Track whether auto-color is currently enabled (for toggle action)
    let mut auto_color_enabled = false;

    // Cache: track name → group name, rebuilt on classify operations
    let mut group_cache: HashMap<String, String> = HashMap::new();

    // Event loop — handle action triggers and track changes from REAPER
    loop {
        tokio::select! {
            result = action_rx.recv() => {
                match result {
                    Ok(Some(event)) => {
                        match &*event {
                            daw::service::ActionEvent::Triggered { command_name } => {
                                if let Err(e) = handle_action(
                                    command_name,
                                    &daw,
                                    &mut auto_color_enabled,
                                    &mut group_cache,
                                ).await {
                                    warn!("[dynamic-template] Action {command_name} failed: {e}");
                                }
                            }
                        }
                    }
                    Ok(None) | Err(_) => {
                        info!("[dynamic-template:{pid}] Action event stream ended");
                        break;
                    }
                }
            }
            result = track_rx.recv() => {
                match result {
                    Ok(Some(event)) => {
                        if let Err(e) = handle_track_event(
                            &*event,
                            &daw,
                            auto_color_enabled,
                            &mut group_cache,
                        ).await {
                            warn!("[dynamic-template] Track event handler failed: {e}");
                        }
                    }
                    Ok(None) | Err(_) => {
                        info!("[dynamic-template:{pid}] Track event stream ended");
                        break;
                    }
                }
            }
        }
    }

    Ok(())
}

async fn handle_action(
    command_name: &str,
    daw: &Daw,
    auto_color_enabled: &mut bool,
    group_cache: &mut HashMap<String, String>,
) -> Result<()> {
    info!("[dynamic-template] Action triggered: {command_name}");

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
        // =====================================================================
        // Sorting actions
        // =====================================================================
        n if n == sort_selected => {
            sort_tracks(daw, true).await?;
        }
        n if n == sort_all => {
            sort_tracks(daw, false).await?;
        }
        n if n == log_status => {
            let project = daw.current_project().await?;
            let count = project.tracks().count().await?;
            info!("[dynamic-template] Status: {count} tracks, auto_color={auto_color_enabled}");
        }
        n if n == log_groups => {
            let config = default_config();
            let group_names: Vec<&str> = config.groups.iter().map(|g| g.name.as_str()).collect();
            info!("[dynamic-template] Groups: {group_names:?}");
        }

        // =====================================================================
        // Auto-color actions
        // =====================================================================
        n if n == color_all => {
            let n = color_tracks(daw, false).await?;
            info!("[dynamic-template] Colored {n} tracks");
            *auto_color_enabled = true;
        }
        n if n == color_selected => {
            let n = color_tracks(daw, true).await?;
            info!("[dynamic-template] Colored {n} selected tracks");
        }
        n if n == toggle => {
            if *auto_color_enabled {
                let n = clear_track_colors(daw, false).await?;
                info!("[dynamic-template] Auto-color OFF, cleared {n} tracks");
                *auto_color_enabled = false;
            } else {
                let n = color_tracks(daw, false).await?;
                info!("[dynamic-template] Auto-color ON, colored {n} tracks");
                *auto_color_enabled = true;
            }
        }
        n if n == clear_all => {
            let n = clear_track_colors(daw, false).await?;
            info!("[dynamic-template] Cleared colors on {n} tracks");
            *auto_color_enabled = false;
        }
        n if n == clear_selected => {
            let n = clear_track_colors(daw, true).await?;
            info!("[dynamic-template] Cleared colors on {n} selected tracks");
        }

        // =====================================================================
        // Visibility manager actions
        // =====================================================================
        n if n == show_all => {
            show_all_tracks(daw).await?;
        }
        n if n == hide_all => {
            hide_all_group_tracks(daw, group_cache).await?;
        }
        n if n == rebuild_cache => {
            rebuild_group_cache(daw, group_cache).await?;
            info!(
                "[dynamic-template] Rebuilt group cache: {} entries",
                group_cache.len()
            );
        }
        cmd if cmd.starts_with(vis_toggle_prefix) => {
            let group_name = cmd.strip_prefix(vis_toggle_prefix).unwrap().to_lowercase();
            toggle_group_visibility(daw, group_cache, &group_name).await?;
        }

        _ => {
            info!("[dynamic-template] Unhandled action: {command_name}");
        }
    }

    Ok(())
}

async fn handle_track_event(
    event: &daw::service::TrackEvent,
    daw: &Daw,
    auto_color_enabled: bool,
    group_cache: &mut HashMap<String, String>,
) -> Result<()> {
    info!("[dynamic-template] Track event: {event:?}");

    // When tracks change and auto-color is enabled, re-apply colors
    match event {
        daw::service::TrackEvent::Added { .. }
        | daw::service::TrackEvent::Removed { .. }
        | daw::service::TrackEvent::Renamed { .. } => {
            // Invalidate the group cache since track layout changed
            group_cache.clear();

            if auto_color_enabled {
                let n = color_tracks(daw, false).await?;
                info!("[dynamic-template] Re-colored {n} tracks after track change");
            }
        }
        _ => {}
    }

    Ok(())
}

// =============================================================================
// Sorting
// =============================================================================

/// Sort tracks by organizing them into a hierarchical template.
///
/// If `selected_only` is true, only selected tracks are reorganized.
/// The hierarchy is applied by renaming tracks and setting folder depths.
async fn sort_tracks(daw: &Daw, selected_only: bool) -> Result<()> {
    use std::time::Instant;

    let t0 = Instant::now();
    let project = daw.current_project().await?;
    let tracks = project.tracks();
    let t_project = t0.elapsed();

    let t1 = Instant::now();
    let source_tracks = if selected_only {
        let handles = tracks.selected().await?;
        let mut infos = Vec::with_capacity(handles.len());
        for h in &handles {
            infos.push(h.info().await?);
        }
        infos
    } else {
        tracks.all().await?
    };
    let t_fetch = t1.elapsed();

    if source_tracks.is_empty() {
        info!("[dynamic-template] No tracks to sort");
        return Ok(());
    }

    let t2 = Instant::now();
    let names: Vec<String> = source_tracks.iter().map(|t| t.name.clone()).collect();
    let config = default_config();
    let hierarchy = names.organize_into_tracks(&config, None)?;
    let t_organize = t2.elapsed();

    info!(
        "[dynamic-template] Organized {} tracks into {} template tracks",
        source_tracks.len(),
        hierarchy.tracks.len()
    );

    // Apply the hierarchy atomically — single main-thread tick, preserves items
    let t3 = Instant::now();
    tracks.apply_hierarchy(hierarchy).await?;
    let t_apply = t3.elapsed();

    info!(
        "[dynamic-template] Timing: project={:?} fetch_tracks={:?} organize={:?} apply_hierarchy={:?} total={:?}",
        t_project,
        t_fetch,
        t_organize,
        t_apply,
        t0.elapsed()
    );
    Ok(())
}

// =============================================================================
// Auto-Color
// =============================================================================

/// Classify tracks and apply instrument-group colors.
///
/// If `selected_only` is true, only selected tracks are colored.
/// Returns the number of tracks that were colored.
async fn color_tracks(daw: &Daw, selected_only: bool) -> Result<u32> {
    let project = daw.current_project().await?;
    let tracks_handle = project.tracks();

    let track_infos = if selected_only {
        let handles = tracks_handle.selected().await?;
        let mut infos = Vec::with_capacity(handles.len());
        for h in &handles {
            infos.push(h.info().await?);
        }
        infos
    } else {
        tracks_handle.all().await?
    };

    if track_infos.is_empty() {
        return Ok(0);
    }

    let names: Vec<String> = track_infos.iter().map(|t| t.name.clone()).collect();
    let color_map = auto_color::classify_and_color(names);

    project.begin_undo_block("FTS: Auto-color tracks").await?;

    let mut colored = 0u32;
    for info in &track_infos {
        if let Some(color) = color_map.get(&info.name) {
            if let Some(handle) = tracks_handle.by_guid(&info.guid).await? {
                handle.set_color(color.to_hex()).await?;
                colored += 1;
            }
        }
    }

    project.end_undo_block("FTS: Auto-color tracks").await?;
    Ok(colored)
}

/// Clear colors from tracks (reset to default).
///
/// If `selected_only` is true, only selected tracks are cleared.
/// Returns the number of tracks cleared.
async fn clear_track_colors(daw: &Daw, selected_only: bool) -> Result<u32> {
    let project = daw.current_project().await?;
    let tracks_handle = project.tracks();

    let track_infos = if selected_only {
        let handles = tracks_handle.selected().await?;
        let mut infos = Vec::with_capacity(handles.len());
        for h in &handles {
            infos.push(h.info().await?);
        }
        infos
    } else {
        tracks_handle.all().await?
    };

    project.begin_undo_block("FTS: Clear track colors").await?;

    let mut cleared = 0u32;
    for info in &track_infos {
        if let Some(handle) = tracks_handle.by_guid(&info.guid).await? {
            handle.set_color(0).await?;
            cleared += 1;
        }
    }

    project.end_undo_block("FTS: Clear track colors").await?;
    Ok(cleared)
}

// =============================================================================
// Visibility Manager
// =============================================================================

/// Rebuild the group classification cache from current tracks.
async fn rebuild_group_cache(daw: &Daw, cache: &mut HashMap<String, String>) -> Result<()> {
    cache.clear();

    let project = daw.current_project().await?;
    let all_tracks = project.tracks().all().await?;
    let names: Vec<String> = all_tracks.iter().map(|t| t.name.clone()).collect();

    let config = default_config();
    if let Ok(structure) = dynamic_template::monarchy_sort(names, &config) {
        collect_group_assignments(&structure, None, cache);
    }

    Ok(())
}

/// Recursively walk the monarchy structure and map item names to their top-level group.
fn collect_group_assignments(
    structure: &dynamic_template::Structure<dynamic_template::ItemMetadata>,
    top_group: Option<&str>,
    cache: &mut HashMap<String, String>,
) {
    let current_group = if structure.name != "root" && !structure.name.is_empty() {
        // Use the top-level group name (first non-root ancestor)
        top_group.unwrap_or(&structure.name)
    } else {
        // Root node — children will set the group
        "root"
    };

    for item in &structure.items {
        if current_group != "root" {
            cache.insert(item.original.clone(), current_group.to_string());
        }
    }

    for child in &structure.children {
        let group = if top_group.is_some() {
            top_group
        } else if structure.name != "root" && !structure.name.is_empty() {
            Some(structure.name.as_str())
        } else {
            None
        };
        collect_group_assignments(child, group, cache);
    }
}

/// Toggle visibility of tracks belonging to a specific group.
async fn toggle_group_visibility(
    daw: &Daw,
    group_cache: &mut HashMap<String, String>,
    group_name: &str,
) -> Result<()> {
    // Rebuild cache if empty
    if group_cache.is_empty() {
        rebuild_group_cache(daw, group_cache).await?;
    }

    let project = daw.current_project().await?;
    let all_tracks = project.tracks().all().await?;

    // Normalize the group name for matching (e.g., "drums" → "Drums")
    let normalized = capitalize(group_name);

    // Find tracks in this group
    let group_tracks: Vec<_> = all_tracks
        .iter()
        .filter(|t| group_cache.get(&t.name).map(|g| g.as_str()) == Some(&normalized))
        .collect();

    if group_tracks.is_empty() {
        info!("[dynamic-template] No tracks found for group '{normalized}'");
        return Ok(());
    }

    // Determine toggle direction: if any are visible, hide all; otherwise show all
    let any_visible = group_tracks.iter().any(|t| t.visible_in_tcp);
    let new_visibility = !any_visible;

    project
        .begin_undo_block(&format!("FTS: Toggle {normalized} visibility"))
        .await?;

    let tracks_handle = project.tracks();
    for track in &group_tracks {
        if let Some(handle) = tracks_handle.by_guid(&track.guid).await? {
            handle.set_visible_in_tcp(new_visibility).await?;
            handle.set_visible_in_mixer(new_visibility).await?;
        }
    }

    project
        .end_undo_block(&format!("FTS: Toggle {normalized} visibility"))
        .await?;

    let action = if new_visibility { "Showing" } else { "Hiding" };
    info!(
        "[dynamic-template] {action} {} {normalized} tracks",
        group_tracks.len()
    );
    Ok(())
}

/// Show all tracks in the project.
async fn show_all_tracks(daw: &Daw) -> Result<()> {
    let project = daw.current_project().await?;
    let all_tracks = project.tracks().all().await?;

    project.begin_undo_block("FTS: Show all tracks").await?;

    let tracks_handle = project.tracks();
    for track in &all_tracks {
        if !track.visible_in_tcp || !track.visible_in_mixer {
            if let Some(handle) = tracks_handle.by_guid(&track.guid).await? {
                handle.set_visible_in_tcp(true).await?;
                handle.set_visible_in_mixer(true).await?;
            }
        }
    }

    project.end_undo_block("FTS: Show all tracks").await?;
    info!("[dynamic-template] Showing all {} tracks", all_tracks.len());
    Ok(())
}

/// Hide all tracks that belong to a classified group.
async fn hide_all_group_tracks(daw: &Daw, group_cache: &mut HashMap<String, String>) -> Result<()> {
    if group_cache.is_empty() {
        rebuild_group_cache(daw, group_cache).await?;
    }

    let project = daw.current_project().await?;
    let all_tracks = project.tracks().all().await?;

    project
        .begin_undo_block("FTS: Hide all group tracks")
        .await?;

    let tracks_handle = project.tracks();
    let mut hidden = 0u32;
    for track in &all_tracks {
        if group_cache.contains_key(&track.name) {
            if let Some(handle) = tracks_handle.by_guid(&track.guid).await? {
                handle.set_visible_in_tcp(false).await?;
                handle.set_visible_in_mixer(false).await?;
                hidden += 1;
            }
        }
    }

    project.end_undo_block("FTS: Hide all group tracks").await?;
    info!("[dynamic-template] Hid {hidden} group tracks");
    Ok(())
}

/// Capitalize the first letter of a string.
fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
    }
}
