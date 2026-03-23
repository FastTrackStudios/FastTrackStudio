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
//! new sockets with retry/backoff, classifies them, and removes entries for
//! vanished sockets.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock, RwLock};
use std::time::Duration;

use daw::service::ProjectInfo;
use daw::Daw;
use vox::ErasedCaller;
use signal::reaper_applier::ReaperPatchApplier;
use signal::rig_scene_manager::RigSceneManager;
use signal::DawPatchApplier;
use signal::RigSceneApplier;
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
const SIGNAL_PROJECT_TITLES: &[&str] = &[
    "FTS-GUITAR",
    "FTS-KEYS",
    "FTS-BASS",
    "FTS-VOCALS",
    "FTS-DRUMS",
    "FTS-DRUM-ENHANCEMENT",
];

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
    /// Rig type for Signal DAWs (e.g., "guitar", "vocals", "drums").
    pub rig_type: Option<String>,
}

/// Serializable connection info for UI display.
#[derive(Clone, Debug, PartialEq)]
pub struct DawConnectionInfo {
    pub pid: u32,
    pub role: DawRole,
    /// Rig type for Signal DAWs (e.g., "guitar", "vocals", "drums").
    pub rig_type: Option<String>,
    pub project_names: Vec<String>,
}

impl From<&DawEntry> for DawConnectionInfo {
    fn from(entry: &DawEntry) -> Self {
        Self {
            pid: entry.pid,
            role: entry.role,
            rig_type: entry.rig_type.clone(),
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

/// Global signal applier — resettable so a new Signal DAW can re-wire after disconnect.
static SIGNAL_APPLIER: RwLock<Option<Arc<dyn DawPatchApplier>>> = RwLock::new(None);

/// Concrete ReaperPatchApplier for input configuration (configure_input, etc.).
static SIGNAL_REAPER_APPLIER: RwLock<Option<Arc<ReaperPatchApplier>>> = RwLock::new(None);

/// The Signal DAW handle for querying audio device info.
static SIGNAL_DAW: RwLock<Option<Daw>> = RwLock::new(None);

/// Global rig scene manager for preloaded rig hierarchy switching.
static SIGNAL_RIG_SCENE_MANAGER: RwLock<Option<Arc<RigSceneManager>>> = RwLock::new(None);

/// Maps preset_id → FX GUID so we can find the REAPER plugin for write-back.
/// Populated at capture time, used by apply and live-push operations.
static BLOCK_FX_MAP: std::sync::LazyLock<RwLock<HashMap<String, String>>> =
    std::sync::LazyLock::new(|| RwLock::new(HashMap::new()));

/// Get the signal applier if a Signal DAW has been discovered and wired.
pub fn signal_applier() -> Option<Arc<dyn DawPatchApplier>> {
    SIGNAL_APPLIER.read().expect("lock poisoned").clone()
}

/// Get the concrete ReaperPatchApplier for input configuration.
pub fn signal_reaper_applier() -> Option<Arc<ReaperPatchApplier>> {
    SIGNAL_REAPER_APPLIER.read().expect("lock poisoned").clone()
}

/// Get the Signal DAW handle for audio device queries.
pub fn signal_daw() -> Option<Daw> {
    SIGNAL_DAW.read().expect("lock poisoned").clone()
}

/// Get the rig scene manager if a Signal DAW has been discovered and wired.
pub fn signal_rig_scene_manager() -> Option<Arc<RigSceneManager>> {
    SIGNAL_RIG_SCENE_MANAGER
        .read()
        .expect("lock poisoned")
        .clone()
}

/// Register a preset_id → FX GUID mapping (called after FX capture).
pub fn register_block_fx(preset_id: &str, fx_guid: &str) {
    BLOCK_FX_MAP
        .write()
        .expect("lock poisoned")
        .insert(preset_id.to_string(), fx_guid.to_string());
}

/// Look up the FX GUID for a previously captured block preset.
pub fn fx_guid_for_block(preset_id: &str) -> Option<String> {
    BLOCK_FX_MAP
        .read()
        .expect("lock poisoned")
        .get(preset_id)
        .cloned()
}

/// Clear all signal statics (called when a Signal DAW disconnects).
fn clear_signal_statics() {
    *SIGNAL_APPLIER.write().expect("lock poisoned") = None;
    *SIGNAL_REAPER_APPLIER.write().expect("lock poisoned") = None;
    *SIGNAL_DAW.write().expect("lock poisoned") = None;
    *SIGNAL_RIG_SCENE_MANAGER.write().expect("lock poisoned") = None;
    BLOCK_FX_MAP.write().expect("lock poisoned").clear();
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
    /// If the removed entry was a Signal DAW, clears the signal statics
    /// so a new Signal DAW can re-wire on next discovery.
    pub fn unregister(&self, pid: u32) {
        self.connecting
            .write()
            .expect("connecting lock poisoned")
            .remove(&pid);
        let mut entries = self.entries.write().expect("registry lock poisoned");
        if let Some(entry) = entries.remove(&pid) {
            info!(pid, role = ?entry.role, "DAW unregistered (socket gone)");
            if entry.role == DawRole::Signal {
                clear_signal_statics();
                info!(
                    pid,
                    "Signal statics cleared — ready to re-wire on next Signal DAW"
                );
            }
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

    /// Get `(pid, daw)` pairs for all registered entries (used for health-checking).
    pub fn daw_handles(&self) -> Vec<(u32, Daw)> {
        let entries = self.entries.read().expect("registry lock poisoned");
        entries.values().map(|e| (e.pid, e.daw.clone())).collect()
    }

    /// Total number of registered DAWs.
    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        let entries = self.entries.read().expect("registry lock poisoned");
        entries.len()
    }
}

// ============================================================================
// Connection Helper
// ============================================================================

/// Connect to a REAPER socket and establish a vox session.
///
/// Returns an `ErasedCaller` suitable for `Daw::new()` / `Daw::init()`.
async fn connect_to_daw(path: &Path) -> eyre::Result<ErasedCaller> {
    let stream = tokio::net::UnixStream::connect(path).await?;
    let link = vox_stream::StreamLink::unix(stream);
    let handshake_result = initiator_handshake_result(64);
    let (caller, _session_handle) = vox::initiator_conduit(vox::BareConduit::new(link), handshake_result)
        .establish::<vox::DriverCaller>(())
        .await?;
    Ok(ErasedCaller::new(caller))
}

/// Construct a synthetic `HandshakeResult` for initiator connections.
fn initiator_handshake_result(max_concurrent_requests: u32) -> vox::HandshakeResult {
    vox::HandshakeResult {
        role: vox::SessionRole::Initiator,
        our_settings: vox::ConnectionSettings {
            parity: vox::Parity::Odd,
            max_concurrent_requests,
        },
        peer_settings: vox::ConnectionSettings {
            parity: vox::Parity::Even,
            max_concurrent_requests,
        },
        peer_supports_retry: true,
        session_resume_key: None,
        peer_resume_key: None,
        our_schema: vec![],
        peer_schema: vec![],
    }
}

// ============================================================================
// Classification
// ============================================================================

/// ExtState section/key used to declare a DAW's role at startup.
const EXT_STATE_SECTION: &str = "FTS";
const EXT_STATE_ROLE_KEY: &str = "role";
const EXT_STATE_RIG_TYPE_KEY: &str = "rig_type";

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

/// Read the rig type from ExtState `FTS/rig_type`.
///
/// Returns `None` for Session DAWs or Signal DAWs launched without a rig type.
pub async fn read_rig_type(daw: &Daw) -> Option<String> {
    daw.ext_state()
        .get(EXT_STATE_SECTION, EXT_STATE_RIG_TYPE_KEY)
        .await
        .ok()
        .flatten()
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

/// Check if a process is still alive by running `kill -0 {pid}`.
/// Returns `false` if the process has exited.
fn is_process_alive(pid: u32) -> bool {
    std::process::Command::new("kill")
        .args(["-0", &pid.to_string()])
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

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

/// Connect to a DAW socket with retry, then classify and register it.
///
/// Retries connection with exponential backoff (up to ~8 s) since the socket
/// may appear on disk before the REAPER extension is ready to accept.
fn spawn_daw_connection(pid: u32, path: PathBuf) {
    // Mark as connecting before spawning to prevent duplicate spawns
    if !DawRegistry::global().mark_connecting(pid) {
        return;
    }

    tokio::spawn(async move {
        // Retry with exponential backoff (250ms, 500ms, 1s, 2s, 4s, 8s)
        let mut delay = Duration::from_millis(250);
        let max_attempts: u32 = 6;
        let mut attempt = 0;

        let caller = loop {
            match connect_to_daw(&path).await {
                Ok(c) => break c,
                Err(e) => {
                    attempt += 1;
                    if attempt >= max_attempts {
                        warn!(
                            pid,
                            "Failed to connect to DAW after {} attempts: {}", attempt, e
                        );
                        DawRegistry::global()
                            .connecting
                            .write()
                            .expect("connecting lock poisoned")
                            .remove(&pid);
                        return;
                    }
                    debug!(
                        pid,
                        attempt,
                        delay_ms = delay.as_millis(),
                        "Connection attempt failed, retrying: {}",
                        e
                    );
                    tokio::time::sleep(delay).await;
                    delay *= 2;
                }
            }
        };

        let daw = Daw::new(caller.clone());
        info!(pid, path = %path.display(), "Connected to DAW");

        // Classify by ExtState role (primary) or project names (fallback)
        let projects = fetch_projects(&daw).await;
        let role = classify_daw(&daw, &projects).await;
        let rig_type = read_rig_type(&daw).await;

        match role {
            DawRole::Session => {
                // If first session DAW, init the global Daw singleton
                if Daw::try_get().is_none() {
                    if let Err(e) = Daw::init(caller) {
                        warn!("Failed to init global DAW singleton: {}", e);
                    } else {
                        info!(pid, "Global DAW singleton initialized (first session DAW)");
                        super::DAW_CONNECTED.store(true, std::sync::atomic::Ordering::Relaxed);
                    }
                }
            }
            DawRole::Signal => {
                // Wire up ReaperPatchApplier (re-wires if previous Signal DAW disconnected)
                let needs_wiring = SIGNAL_APPLIER.read().expect("lock poisoned").is_none();
                if needs_wiring {
                    info!(
                        pid,
                        "Wiring signal applier for {} project(s)...",
                        projects.len()
                    );
                    wire_signal_applier(&daw, &projects).await;
                    if signal_applier().is_some() {
                        info!(pid, "Signal applier successfully wired");
                    } else {
                        warn!(pid, "Signal applier wiring FAILED — applier not set");
                    }
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
            rig_type,
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
    // Use the first project in the Signal DAW — the DAW role is already confirmed
    // via ExtState, so we don't need to filter by project name.
    let Some(info) = projects.first() else {
        warn!("Signal DAW has no projects, cannot wire applier");
        return;
    };

    let project = match daw.project(&info.guid).await {
        Ok(p) => p,
        Err(e) => {
            warn!("Failed to get signal project '{}': {}", info.name, e);
            return;
        }
    };

    let rig_name = if SIGNAL_PROJECT_TITLES.iter().any(|t| info.name == *t) {
        info.name.clone()
    } else {
        "Guitar Rig".to_string()
    };

    let applier = Arc::new(ReaperPatchApplier::new());
    if let Err(e) = applier.set_target(project, &rig_name).await {
        warn!(
            "Failed to set signal applier target '{}': {:?}",
            rig_name, e
        );
        return;
    }

    // Auto-configure input from saved RigSetup before exposing the applier.
    // This ensures record-arm + input monitoring are set even if the user
    // launches from the Dashboard and hasn't visited the Signal page yet.
    if let Ok(audio_info) = daw.audio_engine().get_audio_inputs().await {
        if let Some(setup) = persistence::find_rig_setup(&audio_info.device_name) {
            match applier.configure_input(setup.channel_index).await {
                Ok(()) => info!(
                    "Auto-configured input channel {} ('{}') for '{}'",
                    setup.channel_index, setup.channel_label, audio_info.device_name
                ),
                Err(e) => warn!(
                    "Failed to auto-configure input for '{}': {e:?}",
                    audio_info.device_name
                ),
            }
        }
    }

    *SIGNAL_REAPER_APPLIER.write().expect("lock poisoned") = Some(applier.clone());
    *SIGNAL_DAW.write().expect("lock poisoned") = Some(daw.clone());
    *SIGNAL_APPLIER.write().expect("lock poisoned") = Some(applier);
    info!(
        "Signal applier wired to project '{}' (rig: {})",
        info.name, rig_name
    );

    // Wire rig scene manager for preloaded scene switching
    wire_rig_scene_manager(daw, &info.name, &rig_name).await;
}

/// Set up the `RigSceneManager` for preloaded rig hierarchy switching.
///
/// Creates an input track, stores the manager globally. Background preloading
/// of scenes happens later when a rig is activated via the controller.
async fn wire_rig_scene_manager(daw: &Daw, project_name: &str, rig_name: &str) {
    let project = match daw.projects().await {
        Ok(projects) => {
            let mut found = None;
            for p in projects {
                if let Ok(info) = p.info().await {
                    if info.name == project_name {
                        found = Some(p);
                        break;
                    }
                }
            }
            match found {
                Some(p) => p,
                None => {
                    warn!(
                        "Cannot wire rig scene manager: project '{}' not found",
                        project_name
                    );
                    return;
                }
            }
        }
        Err(e) => {
            warn!(
                "Cannot wire rig scene manager: failed to list projects: {}",
                e
            );
            return;
        }
    };

    // We need a SignalLive instance — construct a minimal one from a temporary in-memory DB.
    // The RigSceneManager only uses load_rig_to_daw which needs repo access.
    // In practice, this will be replaced with the real SignalLive when the controller is connected.
    let signal_live = match signal::bootstrap_in_memory_controller_async().await {
        Ok(ctrl) => ctrl.service().clone(),
        Err(e) => {
            warn!("Cannot wire rig scene manager: bootstrap failed: {}", e);
            return;
        }
    };

    let manager = Arc::new(RigSceneManager::new(signal_live));
    if let Err(e) = manager.set_target(rig_name, project).await {
        warn!("Failed to set rig scene manager target: {e}");
        return;
    }

    *SIGNAL_RIG_SCENE_MANAGER.write().expect("lock poisoned") = Some(manager.clone());
    info!("Rig scene manager wired for '{}'", rig_name);
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
/// For each new socket found, spawns a connection task with retry/backoff.
/// Removes registry entries when sockets vanish or health-checks fail.
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

        // Spawn connections for newly discovered sockets (skip dead processes)
        for (pid, path) in &found_sockets {
            if !is_process_alive(*pid) {
                // Stale socket from a dead process — clean it up
                let _ = std::fs::remove_file(path);
                registry.unregister(*pid);
                continue;
            }
            if !registry.contains(*pid) {
                debug!(pid, path = %path.display(), "Discovered new DAW socket");
                spawn_daw_connection(*pid, path.clone());
            }
        }

        // Health-check all registered DAWs: ping via RPC + check process liveness.
        // A failed ping catches connection-level disconnects (extension unloaded,
        // socket closed) much faster than process-death polling alone.
        for (pid, daw) in registry.daw_handles() {
            if !is_process_alive(pid) {
                info!(pid, "DAW process dead — unregistering");
                registry.unregister(pid);
                let stale_path =
                    PathBuf::from(format!("{SOCKET_DIR}/{SOCKET_PREFIX}{pid}{SOCKET_SUFFIX}"));
                let _ = std::fs::remove_file(&stale_path);
                continue;
            }

            // RPC ping with a short timeout — if it fails, the connection is dead
            let alive = tokio::time::timeout(Duration::from_secs(2), daw.healthcheck())
                .await
                .unwrap_or(false);

            if !alive {
                warn!(pid, "DAW health-check failed — unregistering");
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
