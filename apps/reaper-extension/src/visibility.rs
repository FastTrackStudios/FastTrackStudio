//! Visibility Manager — track-to-group classification and batch visibility toggling.
//!
//! Classifies REAPER tracks into dynamic-template top-level groups (Drums, Bass,
//! Guitars, etc.) by parsing track names through monarchy, then provides toggle/show/hide
//! operations per group with efficient batch REAPER API calls.

use monarchy::Parser;
use reaper_high::{Reaper, TrackArea};
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

/// Rebuild the track-to-group classification cache by parsing every track name
/// through the dynamic-template config.
pub fn rebuild_cache() {
    let config = dynamic_template::default_config();
    let parser = Parser::new(config);
    let reaper = Reaper::get();
    let project = reaper.current_project();

    let mut new_map = HashMap::new();
    let track_count = project.track_count();

    for i in 0..track_count {
        let Some(track) = project.track_by_index(i) else {
            continue;
        };
        let Some(name) = track.name().map(|n| n.to_string()) else {
            continue;
        };
        let guid = (*track.guid()).to_string_without_braces();
        if guid.is_empty() {
            continue;
        }

        // Parse track name through monarchy to get its top-level group
        match parser.parse(name.clone()) {
            Ok(item) => {
                if let Some(group) = item.matched_groups.first() {
                    new_map.insert(guid, group.name.clone());
                }
            }
            Err(_) => {
                // Unmatched track — check if it's a folder whose name matches a group
                let upper = name.to_uppercase();
                for &group in ALL_GROUPS {
                    if upper == group.to_uppercase() {
                        new_map.insert(guid.clone(), group.to_string());
                        break;
                    }
                }
            }
        }
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
