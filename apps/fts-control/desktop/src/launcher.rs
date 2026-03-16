//! REAPER instance launcher.
//!
//! Knows about the REAPER configurations on this machine and can spawn them
//! with fire-and-forget semantics — the `Child` handle is dropped immediately
//! so REAPER survives even if fts-control crashes or exits.
//!
//! For Signal instances, pre-built wrapper `.app` bundles (created by
//! `cargo xtask setup-rigs`) give each rig type its own dock name and icon.

use std::os::unix::process::CommandExt;
use std::path::Path;
use std::process::{Command, Stdio};
use tracing::info;

/// A known REAPER configuration (app bundle + role).
pub struct ReaperConfig {
    /// Short identifier (e.g., "fts-tracks").
    pub id: &'static str,
    /// Human-readable label for the UI.
    pub label: &'static str,
    /// Path to the REAPER binary inside the .app bundle.
    pub executable: &'static str,
    /// Path to the Resources directory (REAPER's working directory).
    pub resources: &'static str,
    /// Role: "session" or "signal". Sets the `FTS_DAW_ROLE` env var.
    pub role: &'static str,
}

/// All known REAPER configurations.
///
/// Both use the same FTS-LIVE.app binary — they differ only in their role
/// (`FTS_DAW_ROLE` env var). Each launch uses `-newinst` so multiple
/// instances coexist, each getting its own PID-based socket.
pub const REAPER_CONFIGS: &[ReaperConfig] = &[
    ReaperConfig {
        id: "fts-tracks",
        label: "FTS-TRACKS (Session)",
        executable: "/Users/codywright/Music/FastTrackStudio/Reaper/FTS-TRACKS/FTS-LIVE.app/Contents/MacOS/REAPER",
        resources: "/Users/codywright/Music/FastTrackStudio/Reaper/FTS-TRACKS/FTS-LIVE.app/Contents/Resources",
        role: "session",
    },
    ReaperConfig {
        id: "fts-signal",
        label: "FTS-SIGNAL (Signal)",
        executable: "/Users/codywright/Music/FastTrackStudio/Reaper/FTS-TRACKS/FTS-LIVE.app/Contents/MacOS/REAPER",
        resources: "/Users/codywright/Music/FastTrackStudio/Reaper/FTS-TRACKS/FTS-LIVE.app/Contents/Resources",
        role: "signal",
    },
];

/// Base directory where wrapper .app bundles live alongside FTS-LIVE.app.
const WRAPPER_BASE_DIR: &str = "/Users/codywright/Music/FastTrackStudio/Reaper/FTS-TRACKS";

/// Find a REAPER config by its id.
pub fn config_by_id(id: &str) -> Option<&'static ReaperConfig> {
    REAPER_CONFIGS.iter().find(|c| c.id == id)
}

/// Spawn a REAPER instance with optional project files.
///
/// For Signal instances with a `rig_type`, a pre-built wrapper `.app` bundle
/// is used so macOS shows the rig-specific name in the dock (e.g., "FTS-GUITAR").
/// These bundles are created by `cargo xtask setup-rigs`.
///
/// Returns the PID of the spawned process. The `Child` handle is intentionally
/// dropped without calling `kill()` — REAPER continues running independently
/// of fts-control's lifecycle. The discovery loop in `daw_registry` will find
/// the new instance via its `/tmp/fts-daw-{pid}.sock` socket.
pub fn spawn_reaper(
    config: &ReaperConfig,
    project_paths: &[&str],
    rig_type: Option<&str>,
) -> eyre::Result<u32> {
    // For signal instances with a rig type, use the wrapper .app bundle
    // so macOS shows the right name and icon in the dock.
    let executable = if let Some(rt) = rig_type {
        wrapper_executable_for_rig(rt, config.executable)
    } else {
        config.executable.to_string()
    };

    let mut cmd = Command::new(&executable);
    cmd.current_dir(config.resources)
        .env("FTS_DAW_ROLE", config.role);

    if let Some(rt) = rig_type {
        cmd.env("FTS_RIG_TYPE", rt);
    }

    cmd
        // Put REAPER in its own process group so it survives when
        // fts-control exits (prevents SIGHUP from killing it).
        .process_group(0)
        // Fully detach stdio so REAPER's output doesn't appear in
        // dx serve logs and broken pipes can't kill the process.
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .arg("-newinst")
        .arg("-nosplash")
        .arg("-ignoreerrors");

    for path in project_paths {
        cmd.arg(path);
    }

    let child = cmd
        .spawn()
        .map_err(|e| eyre::eyre!("Failed to spawn REAPER ({}): {e}", config.label))?;

    let pid = child.id();

    // Intentionally drop the Child handle without killing.
    // On macOS, dropping a Child does NOT kill the process — it just
    // releases our handle. REAPER continues running independently.
    drop(child);

    let label = rig_type
        .map(|rt| format!("FTS-{}", rt.to_uppercase()))
        .unwrap_or_else(|| config.label.to_string());
    info!(
        "Launched {} (PID {}), discovery loop will connect via /tmp/fts-daw-{}.sock",
        label, pid, pid
    );

    Ok(pid)
}

/// Send SIGTERM to a REAPER instance by PID.
///
/// The discovery loop will detect the process death and unregister it
/// automatically. Returns `true` if the signal was sent successfully.
pub fn kill_reaper(pid: u32) -> bool {
    let ok = Command::new("kill")
        .args(["-TERM", &pid.to_string()])
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false);

    if ok {
        info!("Sent SIGTERM to REAPER PID {pid}");
    } else {
        tracing::warn!("Failed to kill REAPER PID {pid}");
    }
    ok
}

// ============================================================================
// Wrapper .app bundle lookup
// ============================================================================

/// Map rig type to the dock-friendly app bundle name.
fn app_name_for_rig(rig_type: &str) -> String {
    match rig_type {
        "guitar" => "FTS-GUITAR".to_string(),
        "bass" => "FTS-BASS".to_string(),
        "keys" => "FTS-KEYS".to_string(),
        "drums" => "FTS-DRUMS".to_string(),
        "drum-enhancement" => "FTS-DRUM-ENHANCEMENT".to_string(),
        "vocals" => "FTS-VOCALS".to_string(),
        other => format!("FTS-{}", other.to_uppercase()),
    }
}

/// Look up the wrapper `.app` bundle for the given rig type.
///
/// Wrapper bundles are created by `cargo xtask setup-rigs` during install.
/// Each has a unique CFBundleName and code signature so macOS shows distinct
/// dock tiles per rig type (e.g., "FTS-GUITAR", "FTS-VOCALS").
///
/// Returns the wrapper executable path, or falls back to the real executable
/// if the wrapper doesn't exist yet.
fn wrapper_executable_for_rig(rig_type: &str, fallback: &str) -> String {
    let app_name = app_name_for_rig(rig_type);
    let wrapper = Path::new(WRAPPER_BASE_DIR)
        .join(format!("{app_name}.app"))
        .join("Contents/MacOS/REAPER");

    if wrapper.exists() {
        info!("Using wrapper bundle {}.app for rig '{}'", app_name, rig_type);
        wrapper.to_string_lossy().to_string()
    } else {
        tracing::warn!(
            "Wrapper bundle {}.app not found — run `cargo xtask setup-rigs` to create it. \
             Falling back to FTS-LIVE.",
            app_name
        );
        fallback.to_string()
    }
}
