//! Multi-DAW Connection Registry
//!
//! Discovers and manages connections to multiple REAPER instances via
//! PID-based Unix sockets (`/tmp/fts-daw-{pid}.sock`).
//!
//! Each REAPER instance is classified by its role:
//! - **Signal**: launched with `FTS_DAW_ROLE=signal` (or has signal project names as fallback)
//! - **Session**: everything else (setlist, transport, navigation)
//!
//! Classification uses ExtState first (`FTS/role` key, set by the extension at
//! startup from the `FTS_DAW_ROLE` env var), falling back to project name
//! heuristics for backwards compatibility.
//!
//! # Discovery
//!
//! A background loop globs `/tmp/fts-daw-*.sock` every 2 seconds, connects to
//! new sockets using roam's auto-reconnect logic, classifies them, and removes
//! entries for vanished sockets.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock, RwLock};
use std::time::Duration;

use daw_control::Daw;
use daw_proto::ProjectInfo;
use roam_session::HandshakeConfig;
use signal::reaper_applier::ReaperPatchApplier;
use signal_live::engine::DawPatchApplier;
use tracing::{debug, info, warn};

use crate::persistence;

// ============================================================================
// Constants
// ============================================================================

const SOCKET_DIR: &str = "/tmp";
const SOCKET_PREFIX: &str = "fts-daw-";
const SOCKET_SUFFIX: &str = ".sock";
const DISCOVERY_INTERVAL: Duration = Duration::from_secs(2);

/// Project titles that identify a REAPER instance as a Signal DAW.
const SIGNAL_PROJECT_TITLES: &[&str] = &["FTS-GUITAR", "FTS-KEYS", "FTS-BASS", "FTS-VOCALS"];

// ============================================================================
// Types
// ============================================================================

/// Role assigned to a DAW connection based on its open projects.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DawRole {
    /// Session DAW — drives setlist, transport, navigation.
    Session,
    /// Signal DAW — has FTS instrument projects for rig/patch application.
    Signal,
}

/// A single DAW connection with metadata.
#[derive(Clone)]
pub struct DawEntry {
    /// The DAW client handle.
    pub daw: Daw,
    /// OS process ID of the REAPER instance (extracted from socket filename).
    pub pid: u32,
    /// Path to the Unix socket.
    pub socket_path: PathBuf,
    /// Project metadata snapshot (captured at connect time).
    pub projects: Vec<ProjectInfo>,
    /// Classified role.
    pub role: DawRole,
}

/// Serializable connection info for UI display.
#[derive(Clone, Debug, PartialEq)]
pub struct DawConnectionInfo {
    pub pid: u32,
    pub role: DawRole,
    pub project_names: Vec<String>,
}

impl From<&DawEntry> for DawConnectionInfo {
    fn from(entry: &DawEntry) -> Self {
        Self {
            pid: entry.pid,
            role: entry.role,
            project_names: entry.projects.iter().map(|p| p.name.clone()).collect(),
        }
    }
}

// ============================================================================
// Registry
// ============================================================================

/// Thread-safe registry of all active DAW connections.
pub struct DawRegistry {
    entries: RwLock<HashMap<u32, DawEntry>>,
    /// PIDs with connection attempts in flight (prevents duplicate spawns).
    connecting: RwLock<std::collections::HashSet<u32>>,
}

static REGISTRY: OnceLock<DawRegistry> = OnceLock::new();

/// Global signal applier, set when a Signal DAW is first discovered.
static SIGNAL_APPLIER: OnceLock<Arc<dyn DawPatchApplier>> = OnceLock::new();

/// Get the signal applier if a Signal DAW has been discovered and wired.
pub fn signal_applier() -> Option<Arc<dyn DawPatchApplier>> {
    SIGNAL_APPLIER.get().cloned()
}

impl DawRegistry {
    /// Initialize the global registry. Call once at startup.
    pub fn init() {
        REGISTRY.get_or_init(|| DawRegistry {
            entries: RwLock::new(HashMap::new()),
            connecting: RwLock::new(std::collections::HashSet::new()),
        });
    }

    /// Get the global registry.
    pub fn global() -> &'static DawRegistry {
        REGISTRY.get().expect("DawRegistry not initialized")
    }

    /// Get the global registry if initialized, or `None`.
    pub fn try_global() -> Option<&'static DawRegistry> {
        REGISTRY.get()
    }

    /// Mark a PID as having a connection attempt in flight.
    /// Returns `false` if already connecting or connected (caller should skip).
    pub fn mark_connecting(&self, pid: u32) -> bool {
        let entries = self.entries.read().expect("registry lock poisoned");
        if entries.contains_key(&pid) {
            return false;
        }
        let mut connecting = self.connecting.write().expect("connecting lock poisoned");
        connecting.insert(pid)
    }

    /// Register a new DAW connection (clears the connecting flag).
    pub fn register(&self, entry: DawEntry) {
        let pid = entry.pid;
        let role = entry.role;
        let project_names: Vec<_> = entry.projects.iter().map(|p| p.name.as_str()).collect();

        info!(
            pid,
            ?role,
            projects = ?project_names,
            "DAW registered"
        );

        // Clear connecting flag
        self.connecting
            .write()
            .expect("connecting lock poisoned")
            .remove(&pid);

        let mut entries = self.entries.write().expect("registry lock poisoned");
        entries.insert(pid, entry);
    }

    /// Remove a DAW connection by PID.
    pub fn unregister(&self, pid: u32) {
        self.connecting
            .write()
            .expect("connecting lock poisoned")
            .remove(&pid);
        let mut entries = self.entries.write().expect("registry lock poisoned");
        if entries.remove(&pid).is_some() {
            info!(pid, "DAW unregistered (socket gone)");
        }
    }

    /// Check if a PID is already registered or being connected.
    pub fn contains(&self, pid: u32) -> bool {
        let entries = self.entries.read().expect("registry lock poisoned");
        if entries.contains_key(&pid) {
            return true;
        }
        let connecting = self.connecting.read().expect("connecting lock poisoned");
        connecting.contains(&pid)
    }

    /// Check if a PID is fully registered (not just connecting).
    pub fn is_registered(&self, pid: u32) -> bool {
        let entries = self.entries.read().expect("registry lock poisoned");
        entries.contains_key(&pid)
    }

    /// Get all registered PIDs.
    pub fn registered_pids(&self) -> Vec<u32> {
        let entries = self.entries.read().expect("registry lock poisoned");
        entries.keys().copied().collect()
    }

    /// Get all session DAW entries.
    pub fn session_daws(&self) -> Vec<DawEntry> {
        let entries = self.entries.read().expect("registry lock poisoned");
        entries
            .values()
            .filter(|e| e.role == DawRole::Session)
            .cloned()
            .collect()
    }

    /// Get all signal DAW entries.
    pub fn signal_daws(&self) -> Vec<DawEntry> {
        let entries = self.entries.read().expect("registry lock poisoned");
        entries
            .values()
            .filter(|e| e.role == DawRole::Signal)
            .cloned()
            .collect()
    }

    /// Get all entries as UI-friendly connection info.
    pub fn connection_info(&self) -> Vec<DawConnectionInfo> {
        let entries = self.entries.read().expect("registry lock poisoned");
        entries.values().map(DawConnectionInfo::from).collect()
    }

    /// Total number of registered DAWs.
    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        let entries = self.entries.read().expect("registry lock poisoned");
        entries.len()
    }
}

// ============================================================================
// Connector (reuses roam auto-reconnect)
// ============================================================================

/// Connector for a specific REAPER socket path.
///
/// Implements `roam_stream::Connector` so `roam_stream::connect()` handles
/// automatic reconnection with exponential backoff.
struct ReaperConnector {
    path: PathBuf,
}

impl roam_stream::Connector for ReaperConnector {
    type Transport = tokio::net::UnixStream;

    async fn connect(&self) -> std::io::Result<Self::Transport> {
        tokio::net::UnixStream::connect(&self.path).await
    }
}

/// Roam handshake config shared across all DAW connections.
fn handshake_config() -> HandshakeConfig {
    HandshakeConfig {
        max_payload_size: 1024 * 1024,            // 1 MiB
        initial_channel_credit: 16 * 1024 * 1024, // 16 MiB — matches server
        max_concurrent_requests: 64,
        ..Default::default()
    }
}

// ============================================================================
// Classification
// ============================================================================

/// ExtState section/key used to declare a DAW's role at startup.
const EXT_STATE_SECTION: &str = "FTS";
const EXT_STATE_ROLE_KEY: &str = "role";

/// Classify a DAW by querying ExtState `FTS/role` first, falling back to
/// project name heuristics for backwards compatibility.
///
/// The REAPER extension writes `FTS_DAW_ROLE` env var to ExtState at startup.
/// If a DAW has `FTS/role = "signal"`, it's classified as Signal regardless of
/// project names. This works even with unsaved/unnamed projects.
pub async fn classify_daw(daw: &Daw, projects: &[ProjectInfo]) -> DawRole {
    // Primary: check ExtState for an explicit role declaration
    if let Ok(Some(role_value)) = daw
        .ext_state()
        .get(EXT_STATE_SECTION, EXT_STATE_ROLE_KEY)
        .await
    {
        if role_value.eq_ignore_ascii_case("signal") {
            return DawRole::Signal;
        }
        // Any other explicit value (including "session") → Session
        return DawRole::Session;
    }

    // Fallback: classify by project name heuristics
    classify_by_project_names(projects)
}

/// Classify a DAW based on project name heuristics (fallback path).
///
/// Used when ExtState `FTS/role` is not set (e.g., older extension version).
fn classify_by_project_names(projects: &[ProjectInfo]) -> DawRole {
    let is_signal = projects
        .iter()
        .any(|p| SIGNAL_PROJECT_TITLES.iter().any(|title| p.name == *title));
    if is_signal {
        DawRole::Signal
    } else {
        DawRole::Session
    }
}

// ============================================================================
// Socket Discovery
// ============================================================================

/// Extract the PID from a socket filename like `fts-daw-12345.sock`.
fn pid_from_socket_path(path: &Path) -> Option<u32> {
    let filename = path.file_name()?.to_str()?;
    let rest = filename.strip_prefix(SOCKET_PREFIX)?;
    let pid_str = rest.strip_suffix(SOCKET_SUFFIX)?;
    pid_str.parse().ok()
}

/// Scan `/tmp` for `fts-daw-*.sock` sockets and return `(pid, path)` pairs.
fn discover_sockets() -> Vec<(u32, PathBuf)> {
    let Ok(entries) = std::fs::read_dir(SOCKET_DIR) else {
        return Vec::new();
    };

    entries
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            let pid = pid_from_socket_path(&path)?;
            Some((pid, path))
        })
        .collect()
}

/// Connect to a DAW socket using roam's auto-reconnect `Connector` pattern.
///
/// This spawns a persistent connection that automatically reconnects with
/// exponential backoff if the DAW restarts. Once connected, it queries
/// project names, classifies the DAW, and registers it.
fn spawn_daw_connection(pid: u32, path: PathBuf) {
    // Mark as connecting before spawning to prevent duplicate spawns
    if !DawRegistry::global().mark_connecting(pid) {
        return;
    }

    tokio::spawn(async move {
        let connector = ReaperConnector { path: path.clone() };
        let config = handshake_config();
        let client = roam_stream::connect(connector, config, roam_session::NoDispatcher);

        // Wait for the initial connection (roam retries with backoff)
        let handle = match client.handle().await {
            Ok(h) => h,
            Err(e) => {
                warn!(pid, "Failed to connect to DAW: {}", e);
                return;
            }
        };

        let daw = Daw::new(handle.clone());
        info!(pid, path = %path.display(), "Connected to DAW");

        // Classify by ExtState role (primary) or project names (fallback)
        let projects = fetch_projects(&daw).await;
        let role = classify_daw(&daw, &projects).await;

        match role {
            DawRole::Session => {
                // If first session DAW, init the global Daw singleton
                if Daw::try_get().is_none() {
                    if let Err(e) = Daw::init(handle) {
                        warn!("Failed to init global DAW singleton: {}", e);
                    } else {
                        info!(pid, "Global DAW singleton initialized (first session DAW)");
                        super::DAW_CONNECTED.store(true, std::sync::atomic::Ordering::Relaxed);
                    }
                }
            }
            DawRole::Signal => {
                // Wire up ReaperPatchApplier for the first matching project
                if SIGNAL_APPLIER.get().is_none() {
                    wire_signal_applier(&daw, &projects).await;
                }
            }
        }

        // Capture project info for recent-projects tracking before moving into entry.
        let project_refs: Vec<(String, String)> = projects
            .iter()
            .map(|p| (p.name.clone(), p.path.clone()))
            .collect();

        let entry = DawEntry {
            daw,
            pid,
            socket_path: path,
            projects,
            role,
        };

        DawRegistry::global().register(entry);

        // Upsert recent projects on disk (file I/O only, no Dioxus signals).
        // The Dashboard component polls persistence on a timer.
        for (name, path) in &project_refs {
            if !path.is_empty() {
                persistence::upsert_recent_project(name, path);
            }
        }

        // Auto-save the "Last Session" setlist when a session DAW has 2+ projects.
        if role == DawRole::Session && project_refs.len() >= 2 {
            persistence::save_last_session_setlist(&project_refs);
        }
    });
}

/// Set up the `ReaperPatchApplier` for the first signal project found.
///
/// Creates a folder-based rig structure in the matching REAPER project
/// and stores the applier globally for `SignalController` to use.
async fn wire_signal_applier(daw: &Daw, projects: &[ProjectInfo]) {
    for info in projects {
        if !SIGNAL_PROJECT_TITLES.iter().any(|t| info.name == *t) {
            continue;
        }

        let project = match daw.project(&info.guid).await {
            Ok(p) => p,
            Err(e) => {
                warn!("Failed to get signal project '{}': {}", info.name, e);
                continue;
            }
        };

        let applier = Arc::new(ReaperPatchApplier::new());
        if let Err(e) = applier.set_target(project, &info.name).await {
            warn!(
                "Failed to set signal applier target '{}': {:?}",
                info.name, e
            );
            continue;
        }

        let _ = SIGNAL_APPLIER.set(applier);
        info!("Signal applier wired to project '{}'", info.name);
        return;
    }
}

/// Fetch project metadata from a connected DAW.
async fn fetch_projects(daw: &Daw) -> Vec<ProjectInfo> {
    match daw.projects().await {
        Ok(projects) => {
            let mut infos = Vec::with_capacity(projects.len());
            for project in projects {
                if let Ok(info) = project.info().await {
                    infos.push(info);
                }
            }
            infos
        }
        Err(e) => {
            warn!("Failed to list projects: {}", e);
            Vec::new()
        }
    }
}

/// The main discovery loop. Runs forever, scanning for new DAW sockets.
///
/// For each new socket found, spawns a persistent auto-reconnecting connection
/// via `roam_stream::connect()`. Removes registry entries when sockets vanish.
///
/// Call once via `tokio::spawn(daw_discovery_loop())`.
pub async fn daw_discovery_loop() {
    info!(
        "DAW discovery loop started — scanning {}/{}*{}",
        SOCKET_DIR, SOCKET_PREFIX, SOCKET_SUFFIX
    );

    loop {
        let found_sockets = discover_sockets();
        let registry = DawRegistry::global();

        // Spawn connections for newly discovered sockets
        for (pid, path) in &found_sockets {
            if !registry.contains(*pid) {
                debug!(pid, path = %path.display(), "Discovered new DAW socket");
                spawn_daw_connection(*pid, path.clone());
            }
        }

        // Remove entries for sockets that no longer exist on disk
        let found_pids: std::collections::HashSet<u32> =
            found_sockets.iter().map(|(pid, _)| *pid).collect();
        for pid in registry.registered_pids() {
            if !found_pids.contains(&pid) {
                registry.unregister(pid);
            }
        }

        tokio::time::sleep(DISCOVERY_INTERVAL).await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pid_extraction() {
        let path = PathBuf::from("/tmp/fts-daw-12345.sock");
        assert_eq!(pid_from_socket_path(&path), Some(12345));

        let path = PathBuf::from("/tmp/fts-daw-1.sock");
        assert_eq!(pid_from_socket_path(&path), Some(1));

        let path = PathBuf::from("/tmp/other-file.sock");
        assert_eq!(pid_from_socket_path(&path), None);

        let path = PathBuf::from("/tmp/fts-daw-abc.sock");
        assert_eq!(pid_from_socket_path(&path), None);
    }

    #[test]
    fn classification_by_project_names() {
        let signal_projects = vec![ProjectInfo {
            guid: "a".into(),
            name: "FTS-GUITAR".into(),
            path: "/tmp/a.rpp".into(),
        }];
        assert_eq!(classify_by_project_names(&signal_projects), DawRole::Signal);

        let session_projects = vec![ProjectInfo {
            guid: "b".into(),
            name: "My Song".into(),
            path: "/tmp/b.rpp".into(),
        }];
        assert_eq!(
            classify_by_project_names(&session_projects),
            DawRole::Session
        );

        let mixed = vec![
            ProjectInfo {
                guid: "c".into(),
                name: "My Song".into(),
                path: "/tmp/c.rpp".into(),
            },
            ProjectInfo {
                guid: "d".into(),
                name: "FTS-BASS".into(),
                path: "/tmp/d.rpp".into(),
            },
        ];
        assert_eq!(classify_by_project_names(&mixed), DawRole::Signal);

        assert_eq!(classify_by_project_names(&[]), DawRole::Session);
    }
}
