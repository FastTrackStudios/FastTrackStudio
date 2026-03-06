//! Visibility Manager — track-to-group classification and batch visibility toggling.
//!
//! Classifies REAPER tracks into dynamic-template top-level groups (Drums, Bass,
//! Guitars, etc.) using `dynamic_template::monarchy_sort`, then provides toggle/show/hide
//! operations per group with efficient batch REAPER API calls.

use dynamic_template::{default_config, monarchy_sort, ItemMetadata, Structure};
use reaper_high::Reaper;
use reaper_medium::TrackArea;
use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};
use tracing::{debug, info, warn};

/// Top-level group names matching dynamic-template's `default_config()` order.
const ALL_GROUPS: &[&str] = &[
    "Drums",
    "Percussion",
    "Bass",
    "Guitars",
    "Keys",
    "Synths",
    "Horns",
    "Harmonica",
    "Vocals",
    "Choir",
    "Orchestra",
    "SFX",
    "Guide",
    "Reference",
];

/// Cached state for visibility management.
struct VisibilityState {
    /// Track GUID string → top-level group name.
    track_groups: HashMap<String, String>,
    /// Group name → currently visible.
    group_visible: HashMap<String, bool>,
}

impl VisibilityState {
    fn new() -> Self {
        let mut group_visible = HashMap::new();
        for &group in ALL_GROUPS {
            group_visible.insert(group.to_string(), true);
        }
        Self {
            track_groups: HashMap::new(),
            group_visible,
        }
    }
}

static STATE: OnceLock<Mutex<VisibilityState>> = OnceLock::new();

fn state() -> &'static Mutex<VisibilityState> {
    STATE.get_or_init(|| Mutex::new(VisibilityState::new()))
}

/// Walk a `Structure` tree and collect item_name → group_name mappings.
/// Only looks at the top-level children (the groups themselves).
fn collect_group_mappings(structure: &Structure<ItemMetadata>) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for child in &structure.children {
        let group_name = &child.name;
        collect_items_recursive(child, group_name, &mut map);
    }
    map
}

/// Recursively collect all item original names under a group subtree.
fn collect_items_recursive(
    node: &Structure<ItemMetadata>,
    group_name: &str,
    map: &mut HashMap<String, String>,
) {
    for item in &node.items {
        map.insert(item.original.clone(), group_name.to_string());
    }
    for child in &node.children {
        collect_items_recursive(child, group_name, map);
    }
}

/// Per-track info collected during the first pass over REAPER's track list.
struct TrackEntry {
    guid: String,
    name: String,
    /// REAPER folder depth change: 1 = starts folder, 0 = normal, -N = closes N levels
    folder_depth_change: i32,
}

/// Rebuild the track-to-group classification cache.
///
/// Two-pass approach:
/// 1. Classify track names via `monarchy_sort` to get name → group mappings
/// 2. Walk the track list in order using `I_FOLDERDEPTH` to propagate group
///    membership from parent folders to all descendants. This ensures that
///    e.g. "Drums > Kick > Sum > In > Out" all belong to the "Drums" group,
///    even though "Sum", "In", "Out" don't match any monarchy pattern.
pub fn rebuild_cache() {
    let reaper = Reaper::get();
    let project = reaper.current_project();
    let track_count = project.track_count();

    // Pass 1: collect track info (name, GUID, folder depth) from REAPER
    let mut entries: Vec<TrackEntry> = Vec::with_capacity(track_count as usize);
    for i in 0..track_count {
        let Some(track) = project.track_by_index(i) else {
            continue;
        };
        let name = track.name().map(|n| n.to_string()).unwrap_or_default();
        let guid = (*track.guid()).to_string_without_braces();
        let folder_depth_change = track.folder_depth_change();
        if !guid.is_empty() {
            entries.push(TrackEntry {
                guid,
                name,
                folder_depth_change,
            });
        }
    }

    // Run all track names through monarchy_sort to classify known names
    let names: Vec<String> = entries.iter().map(|e| e.name.clone()).collect();
    let config = default_config();

    let name_to_group = match monarchy_sort(names, config) {
        Ok(structure) => collect_group_mappings(&structure),
        Err(e) => {
            warn!("Visibility cache: monarchy_sort failed: {e}");
            HashMap::new()
        }
    };

    // Pass 2: walk tracks in order, propagating group from parent folders
    // to all children via folder depth tracking.
    //
    // REAPER folder model:
    //   folder_depth_change = 1  → this track starts a folder (is a parent)
    //   folder_depth_change = 0  → normal track (child of current folder)
    //   folder_depth_change = -N → this track closes N folder levels
    //
    // We maintain a stack of inherited group names. When a folder track has
    // a group (either from monarchy or direct name match), all descendants
    // inherit that group until the folder closes.
    let mut new_map = HashMap::new();
    let mut group_stack: Vec<Option<String>> = Vec::new(); // stack of inherited groups

    for entry in &entries {
        // Determine this track's own group from monarchy classification
        let own_group = name_to_group.get(&entry.name).cloned().or_else(|| {
            // Fallback: check if the track name itself is a group name (folder tracks)
            let upper = entry.name.to_uppercase();
            ALL_GROUPS
                .iter()
                .find(|&&g| g.to_uppercase() == upper)
                .map(|&g| g.to_string())
        });

        // The effective group is either this track's own classification
        // or the inherited group from the nearest classified ancestor folder
        let inherited_group = group_stack.iter().rev().find_map(|g| g.clone());
        let effective_group = own_group.clone().or(inherited_group);

        // Assign the effective group to this track
        if let Some(ref group) = effective_group {
            new_map.insert(entry.guid.clone(), group.clone());
        }

        // Update the folder stack based on this track's folder depth change
        if entry.folder_depth_change >= 1 {
            // This track starts a folder — push its group onto the stack.
            // Children will inherit this group if they don't have their own.
            group_stack.push(effective_group);
        } else if entry.folder_depth_change < 0 {
            // This track closes N folder levels — pop N entries from the stack
            let levels_to_close = (-entry.folder_depth_change) as usize;
            for _ in 0..levels_to_close {
                group_stack.pop();
            }
        }
        // folder_depth_change == 0 → normal track, no stack change
    }

    let classified = new_map.len();
    if let Ok(mut st) = state().lock() {
        st.track_groups = new_map;
        // Reset all groups to visible when cache is rebuilt
        for &group in ALL_GROUPS {
            st.group_visible.insert(group.to_string(), true);
        }
    }
    info!(classified, track_count, "Visibility cache rebuilt");
}

/// Ensure cache is populated (lazy init on first use).
fn ensure_cache() {
    let needs_rebuild = state()
        .lock()
        .map(|st| st.track_groups.is_empty())
        .unwrap_or(true);
    if needs_rebuild {
        rebuild_cache();
    }
}

/// Toggle visibility for all tracks in the given group.
/// Returns the new visibility state (true = now visible).
pub fn toggle_group(group_name: &str) -> bool {
    ensure_cache();

    let new_visible = {
        let Ok(mut st) = state().lock() else {
            return true;
        };
        let current = st.group_visible.get(group_name).copied().unwrap_or(true);
        let new_val = !current;
        st.group_visible.insert(group_name.to_string(), new_val);
        new_val
    };

    set_group_visibility(group_name, new_visible);
    debug!(group_name, new_visible, "Toggled group visibility");
    new_visible
}

/// Show all tracks (reset all group visibility to true).
pub fn show_all() {
    ensure_cache();

    if let Ok(mut st) = state().lock() {
        for &group in ALL_GROUPS {
            st.group_visible.insert(group.to_string(), true);
        }
    }

    let reaper = Reaper::get();
    let project = reaper.current_project();
    let track_count = project.track_count();

    for i in 0..track_count {
        if let Some(track) = project.track_by_index(i) {
            track.set_shown_without_updating_ui(TrackArea::Tcp, true);
        }
    }
    reaper.medium_reaper().track_list_adjust_windows_minor();
    info!("Show all: made all tracks visible");
}

/// Hide all group tracks (set all group visibility to false).
pub fn hide_all() {
    ensure_cache();

    if let Ok(mut st) = state().lock() {
        for &group in ALL_GROUPS {
            st.group_visible.insert(group.to_string(), false);
        }
    }

    // Hide only classified tracks (leave unclassified tracks alone)
    let guids_to_hide: Vec<String> = {
        let Ok(st) = state().lock() else { return };
        st.track_groups.keys().cloned().collect()
    };

    let reaper = Reaper::get();
    let project = reaper.current_project();
    let track_count = project.track_count();

    for i in 0..track_count {
        if let Some(track) = project.track_by_index(i) {
            let guid = (*track.guid()).to_string_without_braces();
            if guids_to_hide.contains(&guid) {
                track.set_shown_without_updating_ui(TrackArea::Tcp, false);
            }
        }
    }
    reaper.medium_reaper().track_list_adjust_windows_minor();
    info!("Hide all: hid all classified group tracks");
}

/// Query whether a group is currently visible.
pub fn is_group_visible(group_name: &str) -> bool {
    state()
        .lock()
        .map(|st| st.group_visible.get(group_name).copied().unwrap_or(true))
        .unwrap_or(true)
}

/// Set visibility for all tracks belonging to a group.
fn set_group_visibility(group_name: &str, visible: bool) {
    let guids: Vec<String> = {
        let Ok(st) = state().lock() else { return };
        st.track_groups
            .iter()
            .filter(|(_, g)| g.as_str() == group_name)
            .map(|(guid, _)| guid.clone())
            .collect()
    };

    if guids.is_empty() {
        warn!(group_name, "No tracks found for group");
        return;
    }

    let reaper = Reaper::get();
    let project = reaper.current_project();
    let track_count = project.track_count();
    let mut toggled = 0;

    for i in 0..track_count {
        if let Some(track) = project.track_by_index(i) {
            let guid = (*track.guid()).to_string_without_braces();
            if guids.contains(&guid) {
                track.set_shown_without_updating_ui(TrackArea::Tcp, visible);
                toggled += 1;
            }
        }
    }

    reaper.medium_reaper().track_list_adjust_windows_minor();
    debug!(group_name, visible, toggled, "Set group visibility");
}
