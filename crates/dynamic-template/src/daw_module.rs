//! DawModule implementation for dynamic-template.

use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex};

use daw::module::{ActionDef, DawModule, ModuleContext};
use daw::reaper::track::{add_track_on_main_thread, set_folder_depth_on_main_thread};

use crate::{
    auto_color, default_config, monarchy_sort, ItemMetadata, OrganizeIntoTracks, Structure,
};
use dynamic_template_proto::{
    actions::dynamic_template_actions, auto_color::actions::auto_color_actions,
    visibility_manager::actions::visibility_manager_actions,
};

type Task = Pin<Box<dyn Future<Output = ()> + Send>>;
type TaskSpawner = Arc<dyn Fn(Task) + Send + Sync>;

struct State {
    auto_color_enabled: bool,
    group_cache: HashMap<String, Vec<String>>,
    spawner: Option<TaskSpawner>,
}

struct CreateTemplateSpec {
    command_suffix: &'static str,
    folders: &'static [&'static str],
    tracks: &'static [&'static str],
}

static STATE: std::sync::OnceLock<Arc<Mutex<State>>> = std::sync::OnceLock::new();

fn state() -> Arc<Mutex<State>> {
    STATE
        .get_or_init(|| {
            Arc::new(Mutex::new(State {
                auto_color_enabled: false,
                group_cache: HashMap::new(),
                spawner: None,
            }))
        })
        .clone()
}

pub struct DynamicTemplateModule;

impl DawModule for DynamicTemplateModule {
    fn name(&self) -> &str {
        "dynamic-template"
    }

    fn display_name(&self) -> &str {
        "Dynamic Template"
    }

    fn actions(&self) -> Vec<ActionDef> {
        let mut defs = Vec::new();

        for def in dynamic_template_actions::definitions() {
            let cmd = def.id.to_command_id();
            let name = def.display_name();
            let cmd2 = cmd.clone();
            defs.push(ActionDef::new(cmd, name, move || dispatch(&cmd2)));
        }

        for def in auto_color_actions::definitions() {
            let cmd = def.id.to_command_id();
            let name = def.display_name();
            let cmd2 = cmd.clone();
            defs.push(ActionDef::new(cmd, name, move || dispatch(&cmd2)));
        }

        for def in visibility_manager_actions::definitions() {
            let cmd = def.id.to_command_id();
            let name = def.display_name();
            let cmd2 = cmd.clone();
            defs.push(ActionDef::new(cmd, name, move || dispatch(&cmd2)));
        }

        defs
    }

    fn init(&self, ctx: &ModuleContext) {
        let runtime = ctx.runtime.clone();
        state().lock().unwrap().spawner = Some(Arc::new(move |task| {
            runtime.spawn(task);
        }));
        tracing::info!("[dynamic-template] runtime initialized");
    }

    fn subscribe(&self, ctx: &ModuleContext) {
        let rt = ctx.runtime.clone();
        rt.spawn(async {
            let Some(daw) = daw::get() else { return };
            let Ok(project) = daw.current_project().await else {
                return;
            };
            let Ok(mut track_rx) = project.tracks().subscribe().await else {
                return;
            };
            tracing::info!("[dynamic-template] subscribed to track events");

            loop {
                match track_rx.recv().await {
                    Ok(Some(_event)) => {
                        let enabled = {
                            let s = state();
                            let mut locked = s.lock().unwrap();
                            locked.group_cache.clear();
                            locked.auto_color_enabled
                        };
                        if enabled {
                            if let Err(err) = color_tracks(daw, false).await {
                                tracing::warn!(
                                    "[dynamic-template] auto-color refresh failed: {err}"
                                );
                            }
                        }
                    }
                    Ok(None) | Err(_) => break,
                }
            }
        });
    }
}

fn dispatch(command_name: &str) {
    let Some(daw) = daw::get().cloned() else {
        tracing::warn!("[dynamic-template] action ignored; daw facade is not initialized");
        return;
    };
    let state = state();
    let spawner = {
        let locked = state.lock().unwrap();
        locked.spawner.clone()
    };
    let Some(spawner) = spawner else {
        tracing::warn!("[dynamic-template] action ignored; module runtime is not initialized");
        return;
    };
    let command = command_name.to_string();
    tracing::info!("[dynamic-template] dispatching action {command}");
    spawner(Box::pin(async move {
        if let Err(err) = handle_action(&command, &daw, &state).await {
            tracing::warn!("[dynamic-template] action failed for {command}: {err}");
        }
    }));
}

async fn handle_action(
    command_name: &str,
    daw: &daw::Daw,
    state: &Arc<Mutex<State>>,
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
    let show_all_cmd = vm::SHOW_ALL.to_id().to_command_id();
    let hide_all_cmd = vm::HIDE_ALL.to_id().to_command_id();
    let rebuild_cache_cmd = vm::REBUILD_CACHE.to_id().to_command_id();
    let vis_toggle_prefix = "FTS_VISIBILITY_MANAGER_TOGGLE_";
    let create_prefix = "FTS_DYNAMIC_TEMPLATE_CREATE_NEW_";

    match command_name {
        n if n == sort_selected => sort_tracks(daw, true).await?,
        n if n == sort_all => sort_tracks(daw, false).await?,
        n if n == log_status => log_status_action(state),
        n if n == log_groups => log_groups_action(),
        n if n == color_all => {
            color_tracks(daw, false).await?;
            state.lock().unwrap().auto_color_enabled = true;
        }
        n if n == color_selected => {
            color_tracks(daw, true).await?;
        }
        n if n == toggle => {
            let enabled = state.lock().unwrap().auto_color_enabled;
            if enabled {
                clear_track_colors(daw, false).await?;
                state.lock().unwrap().auto_color_enabled = false;
            } else {
                color_tracks(daw, false).await?;
                state.lock().unwrap().auto_color_enabled = true;
            }
        }
        n if n == clear_all => {
            clear_track_colors(daw, false).await?;
            state.lock().unwrap().auto_color_enabled = false;
        }
        n if n == clear_selected => {
            clear_track_colors(daw, true).await?;
        }
        n if n == show_all_cmd => show_all_tracks(daw).await?,
        n if n == hide_all_cmd => hide_all_group_tracks(daw, state).await?,
        n if n == rebuild_cache_cmd => {
            let cache = rebuild_group_cache(daw).await?;
            tracing::info!(
                "[dynamic-template] rebuilt visibility cache for {} groups",
                cache.len()
            );
            state.lock().unwrap().group_cache = cache;
        }
        cmd if cmd.starts_with(vis_toggle_prefix) => {
            let group = cmd.strip_prefix(vis_toggle_prefix).unwrap();
            toggle_group_visibility(daw, state, group).await?;
        }
        cmd if cmd.starts_with(create_prefix) => {
            let suffix = cmd.strip_prefix(create_prefix).unwrap();
            create_template_group(suffix)?;
        }
        _ => tracing::debug!("[dynamic-template] unhandled action: {command_name}"),
    }
    Ok(())
}

async fn sort_tracks(daw: &daw::Daw, selected_only: bool) -> eyre::Result<()> {
    let project = daw.current_project().await?;
    let tracks = project.tracks();
    let source = if selected_only {
        let handles = tracks.selected().await?;
        let mut v = Vec::new();
        for h in &handles {
            v.push(h.info().await?);
        }
        v
    } else {
        tracks.all().await?
    };
    if source.is_empty() {
        return Ok(());
    }
    let names: Vec<String> = source.iter().map(|t| t.name.clone()).collect();
    let config = default_config();
    let hierarchy = names.organize_into_tracks(&config, None)?;
    tracks.apply_hierarchy(hierarchy).await?;
    Ok(())
}

async fn color_tracks(daw: &daw::Daw, selected_only: bool) -> eyre::Result<()> {
    let project = daw.current_project().await?;
    let tracks = project.tracks();
    let infos = if selected_only {
        let handles = tracks.selected().await?;
        let mut v = Vec::new();
        for h in &handles {
            v.push(h.info().await?);
        }
        v
    } else {
        tracks.all().await?
    };
    if infos.is_empty() {
        return Ok(());
    }
    let names: Vec<String> = infos.iter().map(|t| t.name.clone()).collect();
    let color_map = auto_color::classify_and_color(names);
    for info in &infos {
        if let Some(color) = color_map.get(&info.name) {
            if let Some(handle) = tracks.by_guid(&info.guid).await? {
                handle.set_color(color.to_hex()).await?;
            }
        }
    }
    Ok(())
}

async fn clear_track_colors(daw: &daw::Daw, selected_only: bool) -> eyre::Result<()> {
    let project = daw.current_project().await?;
    let tracks = project.tracks();
    let infos = if selected_only {
        let handles = tracks.selected().await?;
        let mut v = Vec::new();
        for h in &handles {
            v.push(h.info().await?);
        }
        v
    } else {
        tracks.all().await?
    };
    for info in &infos {
        if let Some(handle) = tracks.by_guid(&info.guid).await? {
            handle.set_color(0).await?;
        }
    }
    Ok(())
}

async fn show_all_tracks(daw: &daw::Daw) -> eyre::Result<()> {
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

async fn hide_all_group_tracks(daw: &daw::Daw, state: &Arc<Mutex<State>>) -> eyre::Result<()> {
    let cache = ensure_group_cache(daw, state).await?;
    let target_names: HashSet<String> = cache.values().flatten().cloned().collect();
    set_named_tracks_visible(daw, &target_names, false).await?;
    tracing::info!(
        "[dynamic-template] hid {} classified group tracks",
        target_names.len()
    );
    Ok(())
}

async fn toggle_group_visibility(
    daw: &daw::Daw,
    state: &Arc<Mutex<State>>,
    group_name: &str,
) -> eyre::Result<()> {
    let cache = ensure_group_cache(daw, state).await?;
    let key = normalize_key(group_name);
    let Some(names) = cache.get(&key) else {
        tracing::info!("[dynamic-template] no tracks matched visibility group {group_name}");
        return Ok(());
    };
    let target_names: HashSet<String> = names.iter().cloned().collect();
    let project = daw.current_project().await?;
    let tracks = project.tracks();
    let infos = tracks.all().await?;
    let should_show = !infos
        .iter()
        .filter(|track| target_names.contains(&track.name))
        .any(|track| track.visible_in_tcp || track.visible_in_mixer);

    set_named_tracks_visible(daw, &target_names, should_show).await?;
    tracing::info!(
        "[dynamic-template] {} {} tracks for visibility group {group_name}",
        if should_show { "showed" } else { "hid" },
        target_names.len()
    );
    Ok(())
}

async fn set_named_tracks_visible(
    daw: &daw::Daw,
    target_names: &HashSet<String>,
    visible: bool,
) -> eyre::Result<()> {
    let project = daw.current_project().await?;
    let tracks = project.tracks();
    for track in tracks.all().await? {
        if !target_names.contains(&track.name) {
            continue;
        }
        if let Some(handle) = tracks.by_guid(&track.guid).await? {
            handle.set_visible_in_tcp(visible).await?;
            handle.set_visible_in_mixer(visible).await?;
        }
    }
    Ok(())
}

async fn ensure_group_cache(
    daw: &daw::Daw,
    state: &Arc<Mutex<State>>,
) -> eyre::Result<HashMap<String, Vec<String>>> {
    let existing = state.lock().unwrap().group_cache.clone();
    if !existing.is_empty() {
        return Ok(existing);
    }
    let cache = rebuild_group_cache(daw).await?;
    state.lock().unwrap().group_cache = cache.clone();
    Ok(cache)
}

async fn rebuild_group_cache(daw: &daw::Daw) -> eyre::Result<HashMap<String, Vec<String>>> {
    let project = daw.current_project().await?;
    let tracks = project.tracks();
    let names: Vec<String> = tracks.all().await?.into_iter().map(|t| t.name).collect();
    let structure = monarchy_sort(names, &default_config())?;
    let mut cache = HashMap::new();
    collect_group_cache(&structure, &mut Vec::new(), &mut cache);
    for names in cache.values_mut() {
        names.sort();
        names.dedup();
    }
    Ok(cache)
}

fn collect_group_cache(
    structure: &Structure<ItemMetadata>,
    path: &mut Vec<String>,
    cache: &mut HashMap<String, Vec<String>>,
) {
    let pushed = !structure.name.is_empty() && structure.name != "root";
    if pushed {
        path.push(structure.name.clone());
    }

    for item in &structure.items {
        for group in path.iter() {
            cache
                .entry(normalize_key(group))
                .or_default()
                .push(item.original.clone());
        }
        if !path.is_empty() {
            cache
                .entry(normalize_key(&path.join("_")))
                .or_default()
                .push(item.original.clone());
        }
    }

    for child in &structure.children {
        collect_group_cache(child, path, cache);
    }

    if pushed {
        path.pop();
    }
}

fn log_status_action(state: &Arc<Mutex<State>>) {
    let locked = state.lock().unwrap();
    tracing::info!(
        "[dynamic-template] status: auto_color_enabled={}, cached_groups={}",
        locked.auto_color_enabled,
        locked.group_cache.len()
    );
}

fn log_groups_action() {
    let groups = [
        "Drums",
        "Percussion",
        "Bass",
        "Guitars",
        "Keys",
        "Synths",
        "Horns",
        "Harmonica",
        "Strings",
        "Vocals",
        "Choir",
        "Orchestra",
        "SFX",
        "Guide",
        "Reference",
        "Stem Split",
    ];
    tracing::info!(
        "[dynamic-template] configured groups: {}",
        groups.join(", ")
    );
}

fn create_template_group(command_suffix: &str) -> eyre::Result<()> {
    let Some(spec) = create_template_specs()
        .iter()
        .find(|spec| spec.command_suffix == command_suffix)
    else {
        tracing::warn!(
            "[dynamic-template] unknown create-template action suffix: {command_suffix}"
        );
        return Ok(());
    };

    let command_suffix = spec.command_suffix;
    let folders = spec.folders;
    let tracks = spec.tracks;
    daw::reaper::main_thread::run(move || {
        let project_tracks = current_project_tracks();
        let existing: HashSet<String> = project_tracks
            .iter()
            .map(|track| track.name.clone())
            .collect();
        let plan = plan_create_insertion(&project_tracks, folders);
        let suffix = next_version_suffix(&existing, plan.version_root);
        let mut created = 0usize;
        let mut insert_index = plan.insert_index;

        tracing::info!(
            "[dynamic-template] create plan group={} root={} insert_index={} folders_to_create={} closing_depth={} existing_tracks={}",
            folders.last().unwrap_or(&command_suffix),
            plan.version_root,
            plan.insert_index,
            plan.folders_to_create.join("/"),
            plan.closing_depth,
            project_tracks.len()
        );

        if let Some(adjustment) = plan.previous_folder_close_adjustment {
            if let Err(err) =
                set_folder_depth_on_main_thread(&adjustment.guid, adjustment.new_depth)
            {
                tracing::warn!(
                    "[dynamic-template] failed to prepare parent folder insertion: {err}"
                );
            }
        }

        for folder in plan.folders_to_create {
            if let Some(guid) =
                add_track_on_main_thread(&with_suffix(folder, &suffix), Some(insert_index))
            {
                if let Err(err) = set_folder_depth_on_main_thread(&guid, 1) {
                    tracing::warn!(
                        "[dynamic-template] failed to set folder depth for {folder}: {err}"
                    );
                }
                insert_index += 1;
                created += 1;
            }
        }

        let leaf_tracks = if tracks.is_empty() {
            &["Main"][..]
        } else {
            tracks
        };
        for (index, track) in leaf_tracks.iter().enumerate() {
            if let Some(guid) =
                add_track_on_main_thread(&with_suffix(track, &suffix), Some(insert_index))
            {
                if index == leaf_tracks.len() - 1 {
                    let depth = -plan.closing_depth;
                    if let Err(err) = set_folder_depth_on_main_thread(&guid, depth) {
                        tracing::warn!(
                            "[dynamic-template] failed to close folder depth for {track}: {err}"
                        );
                    }
                }
                insert_index += 1;
                created += 1;
            }
        }
        tracing::info!(
            "[dynamic-template] created template group {} at index {} with {} tracks",
            folders.last().unwrap_or(&command_suffix),
            plan.insert_index,
            created
        );
    });
    Ok(())
}

struct CreateInsertionPlan {
    insert_index: u32,
    folders_to_create: &'static [&'static str],
    version_root: &'static str,
    closing_depth: i32,
    previous_folder_close_adjustment: Option<FolderCloseAdjustment>,
}

struct FolderCloseAdjustment {
    guid: String,
    new_depth: i32,
}

fn current_project_tracks() -> Vec<daw::service::Track> {
    let Some(daw) = daw::main_thread_daw() else {
        return Vec::new();
    };
    daw.track_list()
}

fn plan_create_insertion(
    tracks: &[daw::service::Track],
    folders: &'static [&'static str],
) -> CreateInsertionPlan {
    let root = folders[0];
    if folders.len() > 1 {
        if let Some(parent) = find_top_level_folder(tracks, root) {
            let (insert_index, previous_folder_close_adjustment) =
                insertion_point_inside_folder(tracks, parent);
            return CreateInsertionPlan {
                insert_index,
                folders_to_create: &folders[1..],
                version_root: folders[1],
                closing_depth: folders.len() as i32,
                previous_folder_close_adjustment,
            };
        }
    }

    CreateInsertionPlan {
        insert_index: insertion_index_for_top_level_group(tracks, root),
        folders_to_create: folders,
        version_root: root,
        closing_depth: folders.len() as i32,
        previous_folder_close_adjustment: None,
    }
}

fn find_top_level_folder(tracks: &[daw::service::Track], group_name: &str) -> Option<usize> {
    let key = normalize_key(group_name);
    tracks.iter().position(|track| {
        track.parent_guid.is_none()
            && track.folder_depth > 0
            && normalize_key(&base_track_name(&track.name)) == key
    })
}

fn insertion_point_inside_folder(
    tracks: &[daw::service::Track],
    folder_index: usize,
) -> (u32, Option<FolderCloseAdjustment>) {
    let end = folder_end_exclusive(tracks, folder_index);
    if end <= folder_index + 1 {
        return (tracks[folder_index].index + 1, None);
    }
    let previous = &tracks[end - 1];
    let adjustment = (previous.folder_depth < 0).then(|| FolderCloseAdjustment {
        guid: previous.guid.clone(),
        new_depth: previous.folder_depth + 1,
    });
    (track_insert_index_at(tracks, end), adjustment)
}

fn insertion_index_for_top_level_group(tracks: &[daw::service::Track], group_name: &str) -> u32 {
    let Some(target_order) = default_group_order(group_name) else {
        return track_insert_index_at(tracks, tracks.len());
    };

    let mut fallback = track_insert_index_at(tracks, tracks.len());
    for (index, track) in tracks.iter().enumerate() {
        if track.parent_guid.is_some() {
            continue;
        }
        let Some(order) = default_group_order(&base_track_name(&track.name)) else {
            continue;
        };
        if order > target_order {
            return track.index;
        }
        if order <= target_order {
            fallback = track_insert_index_at(tracks, folder_end_exclusive(tracks, index));
        }
    }
    fallback
}

fn track_insert_index_at(tracks: &[daw::service::Track], position: usize) -> u32 {
    tracks
        .get(position)
        .map(|track| track.index)
        .unwrap_or(tracks.len() as u32)
}

fn folder_end_exclusive(tracks: &[daw::service::Track], folder_index: usize) -> usize {
    if tracks
        .get(folder_index)
        .is_none_or(|track| track.folder_depth <= 0)
    {
        return folder_index + 1;
    }

    let mut depth = 0i32;
    for (index, track) in tracks.iter().enumerate().skip(folder_index) {
        depth += track.folder_depth;
        if index > folder_index && depth <= 0 {
            return index + 1;
        }
    }
    tracks.len()
}

fn default_group_order(group_name: &str) -> Option<usize> {
    const GROUPS: &[&str] = &[
        "Drums",
        "Percussion",
        "Bass",
        "Guitars",
        "Keys",
        "Synths",
        "Horns",
        "Harmonica",
        "Strings",
        "Vocals",
        "Choir",
        "Orchestra",
        "SFX",
        "Guide",
        "Reference",
        "Stem Split",
    ];
    let key = normalize_key(group_name);
    GROUPS.iter().position(|group| normalize_key(group) == key)
}

fn base_track_name(name: &str) -> String {
    let Some((prefix, suffix)) = name.rsplit_once(' ') else {
        return name.to_string();
    };
    if suffix.chars().all(|ch| ch.is_ascii_digit()) {
        prefix.to_string()
    } else {
        name.to_string()
    }
}

fn next_version_suffix(existing: &HashSet<String>, root_name: &str) -> String {
    if !existing.contains(root_name) {
        return String::new();
    }
    for index in 2.. {
        let suffix = format!(" {index}");
        if !existing.contains(&format!("{root_name}{suffix}")) {
            return suffix;
        }
    }
    unreachable!()
}

fn with_suffix(name: &str, suffix: &str) -> String {
    format!("{name}{suffix}")
}

fn normalize_key(value: &str) -> String {
    let mut key = String::new();
    let mut last_was_sep = false;
    for ch in value.chars() {
        if ch.is_ascii_alphanumeric() {
            key.push(ch.to_ascii_lowercase());
            last_was_sep = false;
        } else if !last_was_sep && !key.is_empty() {
            key.push('_');
            last_was_sep = true;
        }
    }
    while key.ends_with('_') {
        key.pop();
    }
    key
}

fn create_template_specs() -> &'static [CreateTemplateSpec] {
    &[
        CreateTemplateSpec {
            command_suffix: "DRUMS",
            folders: &["Drums"],
            tracks: &["Kick", "Snare", "Toms", "Hi-Hat", "Overheads", "Room"],
        },
        CreateTemplateSpec {
            command_suffix: "DRUM_KIT",
            folders: &["Drums", "Drum Kit"],
            tracks: &["Kick", "Snare", "Toms", "Hi-Hat", "Overheads", "Room"],
        },
        CreateTemplateSpec {
            command_suffix: "ELECTRONIC_KIT",
            folders: &["Drums", "Electronic Kit"],
            tracks: &["Kick", "Snare", "Clap", "Hats", "Perc"],
        },
        CreateTemplateSpec {
            command_suffix: "PERCUSSION",
            folders: &["Percussion"],
            tracks: &["Shaker", "Tambourine", "Conga", "Perc Loop"],
        },
        CreateTemplateSpec {
            command_suffix: "BASS",
            folders: &["Bass"],
            tracks: &["Bass"],
        },
        CreateTemplateSpec {
            command_suffix: "BASS_GUITAR",
            folders: &["Bass", "Bass Guitar"],
            tracks: &["DI", "Amp"],
        },
        CreateTemplateSpec {
            command_suffix: "BASS_SYNTH",
            folders: &["Bass", "Bass Synth"],
            tracks: &["Bass Synth"],
        },
        CreateTemplateSpec {
            command_suffix: "UPRIGHT_BASS",
            folders: &["Bass", "Upright Bass"],
            tracks: &["Upright Bass"],
        },
        CreateTemplateSpec {
            command_suffix: "GUITARS",
            folders: &["Guitars"],
            tracks: &["Electric Guitar", "Acoustic Guitar"],
        },
        CreateTemplateSpec {
            command_suffix: "ELECTRIC_GUITAR",
            folders: &["Guitars", "Electric Guitar"],
            tracks: &["DI", "Amp", "Lead"],
        },
        CreateTemplateSpec {
            command_suffix: "ACOUSTIC_GUITAR",
            folders: &["Guitars", "Acoustic Guitar"],
            tracks: &["Acoustic Guitar"],
        },
        CreateTemplateSpec {
            command_suffix: "KEYS",
            folders: &["Keys"],
            tracks: &["Piano", "Organ", "Electric Keys"],
        },
        CreateTemplateSpec {
            command_suffix: "PIANO",
            folders: &["Keys", "Piano"],
            tracks: &["Piano"],
        },
        CreateTemplateSpec {
            command_suffix: "ORGAN",
            folders: &["Keys", "Organ"],
            tracks: &["Organ"],
        },
        CreateTemplateSpec {
            command_suffix: "ELECTRIC_KEYS",
            folders: &["Keys", "Electric Keys"],
            tracks: &["Electric Keys"],
        },
        CreateTemplateSpec {
            command_suffix: "SYNTHS",
            folders: &["Synths"],
            tracks: &["Lead", "Pad", "Arp", "FX"],
        },
        CreateTemplateSpec {
            command_suffix: "SYNTH_LEAD",
            folders: &["Synths", "Lead"],
            tracks: &["Synth Lead"],
        },
        CreateTemplateSpec {
            command_suffix: "SYNTH_PAD",
            folders: &["Synths", "Pad"],
            tracks: &["Synth Pad"],
        },
        CreateTemplateSpec {
            command_suffix: "SYNTH_ARP",
            folders: &["Synths", "Arp"],
            tracks: &["Synth Arp"],
        },
        CreateTemplateSpec {
            command_suffix: "HORNS",
            folders: &["Horns"],
            tracks: &["Trumpet", "Trombone", "Saxophone"],
        },
        CreateTemplateSpec {
            command_suffix: "TRUMPET",
            folders: &["Horns", "Trumpet"],
            tracks: &["Trumpet"],
        },
        CreateTemplateSpec {
            command_suffix: "TROMBONE",
            folders: &["Horns", "Trombone"],
            tracks: &["Trombone"],
        },
        CreateTemplateSpec {
            command_suffix: "SAXOPHONE",
            folders: &["Horns", "Saxophone"],
            tracks: &["Saxophone"],
        },
        CreateTemplateSpec {
            command_suffix: "HARMONICA",
            folders: &["Harmonica"],
            tracks: &["Harmonica"],
        },
        CreateTemplateSpec {
            command_suffix: "STRINGS",
            folders: &["Strings"],
            tracks: &["Violin", "Viola", "Cello", "Bass"],
        },
        CreateTemplateSpec {
            command_suffix: "VOCALS",
            folders: &["Vocals"],
            tracks: &["Lead Vocal", "Background Vocal", "Harmony"],
        },
        CreateTemplateSpec {
            command_suffix: "LEAD_VOCALS",
            folders: &["Vocals", "Lead Vocals"],
            tracks: &["Lead Vocal"],
        },
        CreateTemplateSpec {
            command_suffix: "BACKGROUND_VOCALS",
            folders: &["Vocals", "Background Vocals"],
            tracks: &["Background Vocal"],
        },
        CreateTemplateSpec {
            command_suffix: "CHOIR",
            folders: &["Choir"],
            tracks: &["Soprano", "Alto", "Tenor", "Bass"],
        },
        CreateTemplateSpec {
            command_suffix: "ORCHESTRA",
            folders: &["Orchestra"],
            tracks: &["Strings", "Brass", "Woodwinds", "Percussion"],
        },
        CreateTemplateSpec {
            command_suffix: "SFX",
            folders: &["SFX"],
            tracks: &["SFX"],
        },
        CreateTemplateSpec {
            command_suffix: "GUIDE",
            folders: &["Guide"],
            tracks: &["Guide"],
        },
        CreateTemplateSpec {
            command_suffix: "REFERENCE",
            folders: &["Reference"],
            tracks: &["Reference"],
        },
        CreateTemplateSpec {
            command_suffix: "STEM_SPLIT",
            folders: &["Stem Split"],
            tracks: &["Vocal", "Drums", "Bass", "Other"],
        },
    ]
}

/// Export the module.
pub fn module() -> Box<dyn DawModule> {
    Box::new(DynamicTemplateModule)
}
