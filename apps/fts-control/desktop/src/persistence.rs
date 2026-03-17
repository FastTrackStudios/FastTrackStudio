//! Persistence for Dashboard state.
//!
//! Stores recent projects and setlist definitions as JSON files in
//! the FTS Library directory. This is the same directory used by the
//! signal database (`signal.db`).

use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use tracing::{debug, warn};
const RECENT_PROJECTS_FILE: &str = "recent-projects.json";
const SETLISTS_FILE: &str = "setlists.json";
const MAX_RECENT_PROJECTS: usize = 50;

// ============================================================================
// Types
// ============================================================================

/// A recently-seen REAPER project.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct RecentProject {
    /// Project name (extracted from filename, no extension).
    pub name: String,
    /// Absolute path to the .rpp file.
    pub path: String,
    /// ISO 8601 timestamp of when this project was last seen open.
    pub last_seen: String,
}

/// A saved setlist definition — a named collection of .rpp file paths.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct SetlistDefinition {
    /// Unique identifier.
    pub id: String,
    /// Human-readable name (e.g., "Sunday Gig").
    pub name: String,
    /// Ordered list of absolute .rpp file paths.
    pub projects: Vec<String>,
    /// Which REAPER config to open them in (e.g., "fts-tracks").
    pub reaper_config: String,
}

// ============================================================================
// Path helpers
// ============================================================================

/// Get the Library directory path.
fn library_path() -> Option<PathBuf> {
    Some(utils::paths::library_dir())
}

fn recent_projects_path() -> Option<PathBuf> {
    library_path().map(|lib| lib.join(RECENT_PROJECTS_FILE))
}

fn setlists_path() -> Option<PathBuf> {
    library_path().map(|lib| lib.join(SETLISTS_FILE))
}

// ============================================================================
// Recent Projects
// ============================================================================

/// Load recent projects from disk. Returns empty vec on any error.
pub fn load_recent_projects() -> Vec<RecentProject> {
    let Some(path) = recent_projects_path() else {
        return Vec::new();
    };
    match std::fs::read_to_string(&path) {
        Ok(contents) => serde_json::from_str(&contents).unwrap_or_else(|e| {
            warn!("Failed to parse {}: {e}", path.display());
            Vec::new()
        }),
        Err(_) => Vec::new(), // File doesn't exist yet — that's fine
    }
}

/// Save recent projects to disk.
fn save_recent_projects(projects: &[RecentProject]) {
    let Some(path) = recent_projects_path() else {
        return;
    };
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    match serde_json::to_string_pretty(projects) {
        Ok(json) => {
            if let Err(e) = std::fs::write(&path, json) {
                warn!("Failed to write {}: {e}", path.display());
            }
        }
        Err(e) => warn!("Failed to serialize recent projects: {e}"),
    }
}

/// Add or update a project in the recent list.
///
/// If a project with the same path already exists, its `last_seen` is updated.
/// Otherwise it's inserted at the front. The list is capped at `MAX_RECENT_PROJECTS`.
pub fn upsert_recent_project(name: &str, path: &str) {
    let mut projects = load_recent_projects();
    let now = chrono_now();

    if let Some(existing) = projects.iter_mut().find(|p| p.path == path) {
        existing.name = name.to_string();
        existing.last_seen = now;
    } else {
        projects.insert(
            0,
            RecentProject {
                name: name.to_string(),
                path: path.to_string(),
                last_seen: now,
            },
        );
    }

    // Sort by last_seen descending, truncate
    projects.sort_by(|a, b| b.last_seen.cmp(&a.last_seen));
    projects.truncate(MAX_RECENT_PROJECTS);

    save_recent_projects(&projects);
    debug!("Recent projects updated ({} entries)", projects.len());
}

// ============================================================================
// Setlist Definitions
// ============================================================================

/// Load setlist definitions from disk. Returns empty vec on any error.
pub fn load_setlists() -> Vec<SetlistDefinition> {
    let Some(path) = setlists_path() else {
        return Vec::new();
    };
    match std::fs::read_to_string(&path) {
        Ok(contents) => serde_json::from_str(&contents).unwrap_or_else(|e| {
            warn!("Failed to parse {}: {e}", path.display());
            Vec::new()
        }),
        Err(_) => Vec::new(),
    }
}

/// Save setlist definitions to disk.
pub fn save_setlists(setlists: &[SetlistDefinition]) {
    let Some(path) = setlists_path() else {
        return;
    };
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    match serde_json::to_string_pretty(setlists) {
        Ok(json) => {
            if let Err(e) = std::fs::write(&path, json) {
                warn!("Failed to write {}: {e}", path.display());
            }
        }
        Err(e) => warn!("Failed to serialize setlists: {e}"),
    }
}

// ============================================================================
// RPL Files (REAPER Project Lists)
// ============================================================================

/// Parse an `.RPL` file's contents into a list of `.RPP` paths.
/// Skips empty lines and trims whitespace.
pub fn parse_rpl(contents: &str) -> Vec<String> {
    contents
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(|line| line.to_string())
        .collect()
}

/// Import an `.RPL` file as a new `SetlistDefinition`.
/// Name is derived from the filename (without extension).
pub fn import_rpl(path: &str) -> Option<SetlistDefinition> {
    let file_path = std::path::Path::new(path);
    let contents = std::fs::read_to_string(file_path).ok()?;
    let projects = parse_rpl(&contents);
    if projects.is_empty() {
        return None;
    }

    let name = file_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("Imported Setlist")
        .to_string();

    Some(SetlistDefinition {
        id: format!("rpl-{}", name.to_lowercase().replace(' ', "-")),
        name,
        projects,
        reaper_config: "fts-tracks".to_string(),
    })
}

/// Export a `SetlistDefinition` as an `.RPL` file (one path per line).
pub fn export_rpl(setlist: &SetlistDefinition, path: &str) -> std::io::Result<()> {
    let contents = setlist.projects.join("\n");
    std::fs::write(path, contents)
}

// ============================================================================
// Last Session Auto-Save
// ============================================================================

/// The well-known ID for the auto-saved "Last Session" setlist.
pub const LAST_SESSION_ID: &str = "last-session";

/// Save or update the "Last Session" setlist from the currently open projects.
/// Called by the discovery loop when a session DAW connects with projects.
pub fn save_last_session_setlist(projects: &[(String, String)]) {
    let paths: Vec<String> = projects
        .iter()
        .filter(|(_, p)| !p.is_empty())
        .map(|(_, p)| p.clone())
        .collect();

    if paths.is_empty() {
        return;
    }

    let mut setlists = load_setlists();

    if let Some(existing) = setlists.iter_mut().find(|s| s.id == LAST_SESSION_ID) {
        existing.projects = paths;
    } else {
        setlists.insert(
            0,
            SetlistDefinition {
                id: LAST_SESSION_ID.to_string(),
                name: "Last Session".to_string(),
                projects: paths,
                reaper_config: "fts-tracks".to_string(),
            },
        );
    }

    save_setlists(&setlists);
    debug!("Last Session setlist updated");
}

// ============================================================================
// Rig Setups (saved audio input preferences per device)
// ============================================================================

const RIG_SETUPS_FILE: &str = "rig-setups.json";

/// A saved audio input preference for a specific audio device.
///
/// When a Signal DAW connects and the audio device name matches a saved setup,
/// the input channel is automatically configured (record input, arm, monitoring).
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct RigSetup {
    /// Audio device name as reported by the DAW (e.g. "Galaxy 32").
    pub device_name: String,
    /// 0-based mono hardware input channel index.
    pub channel_index: u32,
    /// Human-readable channel label at time of save (e.g. "In 5").
    pub channel_label: String,
}

fn rig_setups_path() -> Option<PathBuf> {
    library_path().map(|lib| lib.join(RIG_SETUPS_FILE))
}

/// Load rig setups from disk. Returns empty vec on any error.
pub fn load_rig_setups() -> Vec<RigSetup> {
    let Some(path) = rig_setups_path() else {
        return Vec::new();
    };
    match std::fs::read_to_string(&path) {
        Ok(contents) => serde_json::from_str(&contents).unwrap_or_else(|e| {
            warn!("Failed to parse {}: {e}", path.display());
            Vec::new()
        }),
        Err(_) => Vec::new(),
    }
}

/// Save rig setups to disk.
pub fn save_rig_setups(setups: &[RigSetup]) {
    let Some(path) = rig_setups_path() else {
        return;
    };
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    match serde_json::to_string_pretty(setups) {
        Ok(json) => {
            if let Err(e) = std::fs::write(&path, json) {
                warn!("Failed to write {}: {e}", path.display());
            }
        }
        Err(e) => warn!("Failed to serialize rig setups: {e}"),
    }
}

/// Find a saved setup for the given audio device name.
pub fn find_rig_setup(device_name: &str) -> Option<RigSetup> {
    load_rig_setups()
        .into_iter()
        .find(|s| s.device_name == device_name)
}

/// Save or update the setup for a device. One setup per device name.
pub fn upsert_rig_setup(device_name: &str, channel_index: u32, channel_label: &str) {
    let mut setups = load_rig_setups();

    if let Some(existing) = setups.iter_mut().find(|s| s.device_name == device_name) {
        existing.channel_index = channel_index;
        existing.channel_label = channel_label.to_string();
    } else {
        setups.push(RigSetup {
            device_name: device_name.to_string(),
            channel_index,
            channel_label: channel_label.to_string(),
        });
    }

    save_rig_setups(&setups);
    debug!(
        "Rig setup saved: '{}' → channel {} ('{}')",
        device_name, channel_index, channel_label
    );
}

// ============================================================================
// MIDI Config
// ============================================================================

const MIDI_CONFIG_FILE: &str = "midi-config.json";

/// Persistent MIDI configuration: selected device, action map, and enabled state.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct MidiConfig {
    /// Name of the MIDI port to auto-connect on startup.
    pub selected_port_name: Option<String>,
    /// MIDI message → action ID mapping.
    pub action_map: signal::midi_actions::MidiActionMap,
    /// Whether the MIDI service is enabled.
    pub enabled: bool,
}

impl Default for MidiConfig {
    fn default() -> Self {
        Self {
            selected_port_name: None,
            action_map: signal::midi_actions::MidiActionMap::with_defaults(),
            enabled: true,
        }
    }
}

fn midi_config_path() -> Option<PathBuf> {
    library_path().map(|lib| lib.join(MIDI_CONFIG_FILE))
}

/// Load MIDI config from disk. Returns defaults if file is missing or corrupt.
pub fn load_midi_config() -> MidiConfig {
    let Some(path) = midi_config_path() else {
        return MidiConfig::default();
    };
    match std::fs::read_to_string(&path) {
        Ok(contents) => serde_json::from_str(&contents).unwrap_or_else(|e| {
            warn!("Failed to parse {}: {e}", path.display());
            MidiConfig::default()
        }),
        Err(_) => MidiConfig::default(),
    }
}

/// Save MIDI config to disk.
pub fn save_midi_config(config: &MidiConfig) {
    let Some(path) = midi_config_path() else {
        return;
    };
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    match serde_json::to_string_pretty(config) {
        Ok(json) => {
            if let Err(e) = std::fs::write(&path, json) {
                warn!("Failed to write {}: {e}", path.display());
            }
        }
        Err(e) => warn!("Failed to serialize MIDI config: {e}"),
    }
}

// ============================================================================
// App Preferences
// ============================================================================

const APP_PREFS_FILE: &str = "app-preferences.json";

/// Persistent application preferences (e.g., default startup screen).
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct AppPreferences {
    /// Which page to show on startup: "dashboard", "main", "rig", "actions".
    pub default_screen: String,
}

impl Default for AppPreferences {
    fn default() -> Self {
        Self {
            default_screen: "rig".to_string(),
        }
    }
}

fn app_prefs_path() -> Option<PathBuf> {
    library_path().map(|lib| lib.join(APP_PREFS_FILE))
}

/// Load app preferences from disk. Returns defaults if file is missing or corrupt.
pub fn load_app_preferences() -> AppPreferences {
    let Some(path) = app_prefs_path() else {
        return AppPreferences::default();
    };
    match std::fs::read_to_string(&path) {
        Ok(contents) => serde_json::from_str(&contents).unwrap_or_else(|e| {
            warn!("Failed to parse {}: {e}", path.display());
            AppPreferences::default()
        }),
        Err(_) => AppPreferences::default(),
    }
}

/// Save app preferences to disk.
pub fn save_app_preferences(prefs: &AppPreferences) {
    let Some(path) = app_prefs_path() else {
        return;
    };
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    match serde_json::to_string_pretty(prefs) {
        Ok(json) => {
            if let Err(e) = std::fs::write(&path, json) {
                warn!("Failed to write {}: {e}", path.display());
            }
        }
        Err(e) => warn!("Failed to serialize app preferences: {e}"),
    }
}

// ============================================================================
// Helpers
// ============================================================================

/// Simple ISO 8601 timestamp without pulling in chrono.
fn chrono_now() -> String {
    // Use std::time for a monotonic-safe UTC approximation.
    // Format: "2026-02-19T17:30:00Z" (good enough for sorting).
    let now = std::time::SystemTime::now();
    let duration = now
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let secs = duration.as_secs();

    // Manual UTC breakdown (no leap seconds, good enough for display)
    let days = secs / 86400;
    let time_of_day = secs % 86400;
    let hours = time_of_day / 3600;
    let minutes = (time_of_day % 3600) / 60;
    let seconds = time_of_day % 60;

    // Days since epoch to Y-M-D (simplified Gregorian)
    let (year, month, day) = days_to_ymd(days);

    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}Z",
        year, month, day, hours, minutes, seconds
    )
}

fn days_to_ymd(days_since_epoch: u64) -> (u64, u64, u64) {
    // Algorithm from http://howardhinnant.github.io/date_algorithms.html
    let z = days_since_epoch + 719468;
    let era = z / 146097;
    let doe = z - era * 146097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = if m <= 2 { y + 1 } else { y };
    (y, m, d)
}
