//! REAPER instance launcher.
//!
//! Knows about the REAPER configurations on this machine and can spawn them
//! with fire-and-forget semantics — the `Child` handle is dropped immediately
//! so REAPER survives even if fts-control crashes or exits.

use std::process::Command;
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
        id: "fts-guitar",
        label: "FTS-GUITAR (Signal)",
        executable: "/Users/codywright/Music/FastTrackStudio/Reaper/FTS-TRACKS/FTS-LIVE.app/Contents/MacOS/REAPER",
        resources: "/Users/codywright/Music/FastTrackStudio/Reaper/FTS-TRACKS/FTS-LIVE.app/Contents/Resources",
        role: "signal",
    },
];

/// Find a REAPER config by its id.
pub fn config_by_id(id: &str) -> Option<&'static ReaperConfig> {
    REAPER_CONFIGS.iter().find(|c| c.id == id)
}

/// Spawn a REAPER instance with optional project files.
///
/// Returns the PID of the spawned process. The `Child` handle is intentionally
/// dropped without calling `kill()` — REAPER continues running independently
/// of fts-control's lifecycle. The discovery loop in `daw_registry` will find
/// the new instance via its `/tmp/fts-daw-{pid}.sock` socket.
pub fn spawn_reaper(config: &ReaperConfig, project_paths: &[&str]) -> eyre::Result<u32> {
    let mut cmd = Command::new(config.executable);
    cmd.current_dir(config.resources)
        .env("FTS_DAW_ROLE", config.role)
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

    info!(
        "Launched {} (PID {}), discovery loop will connect via /tmp/fts-daw-{}.sock",
        config.label, pid, pid
    );

    Ok(pid)
}
