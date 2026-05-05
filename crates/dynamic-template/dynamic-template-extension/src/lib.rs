//! Dynamic Template integrated REAPER extension.
//!
//! Loaded directly by REAPER from `UserPlugins/`. Manages auto-color
//! classification, visibility manager, and template sorting — registers the
//! corresponding actions and handles their triggers in-process.

use std::cell::OnceCell;
use std::collections::HashMap;
use std::error::Error;

use daw::Daw;
use daw_extension_runtime::ExtensionRuntime;
use dynamic_template::{auto_color, default_config, OrganizeIntoTracks};
use dynamic_template_proto::{
    actions::dynamic_template_actions,
    auto_color::actions::auto_color_actions,
    visibility_manager::actions::visibility_manager_actions,
};
use eyre::Result;
use fragile::Fragile;
use reaper_low::PluginContext;
use reaper_macros::reaper_extension_plugin;
use tracing::{info, warn};

thread_local! {
    static APP: OnceCell<Fragile<DynamicTemplateExtension>> = const { OnceCell::new() };
}

struct DynamicTemplateExtension {
    runtime: ExtensionRuntime,
}

impl DynamicTemplateExtension {
    fn new(context: PluginContext) -> Result<Self> {
        let runtime = ExtensionRuntime::new(context)?;
        let daw = runtime.build_daw()?;

        runtime.spawn(async move {
            if let Err(e) = run(daw).await {
                warn!("[dynamic-template] event loop ended: {e}");
            }
        });

        Ok(Self { runtime })
    }

    fn timer(&self) {
        self.runtime.process_tasks();
    }
}

extern "C" fn timer_callback() {
    APP.with(|cell| {
        if let Some(app) = cell.get() {
            app.get().timer();
        }
    });
}

#[reaper_extension_plugin]
fn plugin_main(context: PluginContext) -> std::result::Result<(), Box<dyn Error>> {
    init_tracing();
    info!("dynamic-template-extension starting");

    let app = DynamicTemplateExtension::new(context).map_err(|e| -> Box<dyn Error> { e.into() })?;
    app.runtime.add_timer(timer_callback).map_err(|e| -> Box<dyn Error> { e.into() })?;

    let stored = APP.with(|cell| cell.set(Fragile::new(app)).is_ok());
    if !stored {
        return Err("dynamic-template-extension already initialized".into());
    }

    info!("dynamic-template-extension loaded");
    Ok(())
}

fn init_tracing() {
    let Ok(log_file) = std::fs::File::create("/tmp/dynamic-template-extension.log") else {
        return;
    };
    let subscriber = tracing_subscriber::fmt()
        .with_writer(std::sync::Mutex::new(log_file))
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .finish();
    let _ = tracing::subscriber::set_global_default(subscriber);
}

async fn run(daw: Daw) -> Result<()> {
    let pid = std::process::id();
    info!("[dynamic-template:{pid}] runtime started");

    // Register dynamic-template-domain actions with REAPER.
    // Action definitions live in dynamic-template-proto — single source of truth.
    let registry = daw.action_registry();

    // Core dynamic-template actions (sort selected, sort all, import & sort, etc.)
    for def in dynamic_template_actions::definitions() {
        let cmd_name = def.id.to_command_id();
        let cmd_id = registry.register(&cmd_name, &def.description).await?;
        if cmd_id == 0 {
            warn!("[dynamic-template:{pid}] Failed to register action: {cmd_name}");
        } else {
            tracing::debug!("[dynamic-template:{pid}] Registered {cmd_name} (cmd_id={cmd_id})");
        }
    }

    // Auto-color actions (color all, color selected, toggle, clear)
    for def in auto_color_actions::definitions() {
        let cmd_name = def.id.to_command_id();
        let cmd_id = registry.register(&cmd_name, &def.description).await?;
        if cmd_id == 0 {
            warn!("[dynamic-template:{pid}] Failed to register action: {cmd_name}");
        } else {
            tracing::debug!("[dynamic-template:{pid}] Registered {cmd_name} (cmd_id={cmd_id})");
        }
    }

    // Visibility manager actions (per-group toggles, show/hide all, rebuild cache)
    for def in visibility_manager_actions::definitions() {
        let cmd_name = def.id.to_command_id();
        let cmd_id = registry.register(&cmd_name, &def.description).await?;
        if cmd_id == 0 {
            warn!("[dynamic-template:{pid}] Failed to register action: {cmd_name}");
        } else {
            tracing::debug!("[dynamic-template:{pid}] Registered {cmd_name} (cmd_id={cmd_id})");
        }
    }
    info!("[dynamic-template:{pid}] All dynamic-template actions registered");

    // Subscribe to action trigger events and handle them locally.
    let mut action_rx = registry.subscribe_actions().await?;
    info!("[dynamic-template:{pid}] Subscribed to action events");

    // Track whether auto-color is currently enabled (for toggle action)
    let mut auto_color_enabled = false;

    // Cache: track name → group name, rebuilt on classify operations
    let mut group_cache: HashMap<String, String> = HashMap::new();

    // Event loop — handle action triggers from REAPER.
    // (Track event subscription was dropped during the vox 0.46 migration:
    // TrackEvent doesn't impl Reborrow so SelfRef::get isn't available. Auto-color
    // re-application on track changes will be re-wired once that lands.)
    while let Ok(Some(event)) = action_rx.recv().await {
        match event.get() {
            daw::service::ActionEvent::Triggered { command_name } => {
                if let Err(e) = handle_action(
                    command_name.as_str(),
                    &daw,
                    &mut auto_color_enabled,
                    &mut group_cache,
                )
                .await
                {
                    warn!("[dynamic-template] Action {command_name} failed: {e}");
                }
            }
        }
    }

    info!("[dynamic-template:{pid}] action event stream ended");
    Ok(())
}

async fn handle_action(
    command_name: &str,
    daw: &Daw,
    auto_color_enabled: &mut bool,
    group_cache: &mut HashMap<String, String>,
) -> Result<()> {
    info!("[dynamic-template] Action triggered: {command_name}");

    match command_name {
        // =====================================================================
        // Sorting actions
        // =====================================================================
        "fts.dynamic_template.sort_selected" => {
            sort_tracks(daw, true).await?;
        }
        "fts.dynamic_template.sort_all" => {
            sort_tracks(daw, false).await?;
        }
        "fts.dynamic_template.log_status" => {
            let project = daw.current_project().await?;
            let count = project.tracks().count().await?;
            info!("[dynamic-template] Status: {count} tracks, auto_color={auto_color_enabled}");
        }
        "fts.dynamic_template.log_groups" => {
            let config = default_config();
            let group_names: Vec<&str> = config.groups.iter().map(|g| g.name.as_str()).collect();
            info!("[dynamic-template] Groups: {group_names:?}");
        }

        // =====================================================================
        // Auto-color actions
        // =====================================================================
        "fts.auto_color.color_all" => {
            let n = color_tracks(daw, false).await?;
            info!("[dynamic-template] Colored {n} tracks");
            *auto_color_enabled = true;
        }
        "fts.auto_color.color_selected" => {
            let n = color_tracks(daw, true).await?;
            info!("[dynamic-template] Colored {n} selected tracks");
        }
        "fts.auto_color.toggle" => {
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
        "fts.auto_color.clear_all" => {
            let n = clear_track_colors(daw, false).await?;
            info!("[dynamic-template] Cleared colors on {n} tracks");
            *auto_color_enabled = false;
        }
        "fts.auto_color.clear_selected" => {
            let n = clear_track_colors(daw, true).await?;
            info!("[dynamic-template] Cleared colors on {n} selected tracks");
        }

        // =====================================================================
        // Visibility manager actions
        // =====================================================================
        "fts.visibility_manager.show_all" => {
            show_all_tracks(daw).await?;
        }
        "fts.visibility_manager.hide_all" => {
            hide_all_group_tracks(daw, group_cache).await?;
        }
        "fts.visibility_manager.rebuild_cache" => {
            rebuild_group_cache(daw, group_cache).await?;
            info!("[dynamic-template] Rebuilt group cache: {} entries", group_cache.len());
        }
        cmd if cmd.starts_with("fts.visibility_manager.toggle_") => {
            let group_name = cmd
                .strip_prefix("fts.visibility_manager.toggle_")
                .unwrap();
            toggle_group_visibility(daw, group_cache, group_name).await?;
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
    let project = daw.current_project().await?;
    let tracks = project.tracks();

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

    if source_tracks.is_empty() {
        info!("[dynamic-template] No tracks to sort");
        return Ok(());
    }

    let names: Vec<String> = source_tracks.iter().map(|t| t.name.clone()).collect();
    let config = default_config();
    let hierarchy = names.organize_into_tracks(&config, None)?;

    info!(
        "[dynamic-template] Organized {} tracks into {} template tracks",
        source_tracks.len(),
        hierarchy.tracks.len()
    );

    // Apply the hierarchy to REAPER: remove old tracks, create new ones
    project.begin_undo_block("FTS: Sort tracks").await?;

    // Remove the source tracks (we'll recreate them in the right order)
    for track in &source_tracks {
        tracks
            .remove(daw::service::TrackRef::Guid(track.guid.clone()))
            .await?;
    }

    // Create tracks from the hierarchy
    for node in &hierarchy.tracks {
        let handle = tracks.add(&node.name, None).await?;

        // Apply folder depth
        let depth = node.folder_depth_change.to_raw_value();
        if depth != 0 {
            handle.set_folder_depth(depth).await?;
        }

        // Apply color if the hierarchy node has one
        if let Some(color) = node.color {
            handle.set_color(color).await?;
        }
    }

    project.end_undo_block("FTS: Sort tracks").await?;
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
    info!("[dynamic-template] {action} {} {normalized} tracks", group_tracks.len());
    Ok(())
}

/// Show all tracks in the project.
async fn show_all_tracks(daw: &Daw) -> Result<()> {
    let project = daw.current_project().await?;
    let all_tracks = project.tracks().all().await?;

    project
        .begin_undo_block("FTS: Show all tracks")
        .await?;

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
async fn hide_all_group_tracks(
    daw: &Daw,
    group_cache: &mut HashMap<String, String>,
) -> Result<()> {
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
