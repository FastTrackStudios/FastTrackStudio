//! Dashboard view — landing page for fts-control.
//!
//! Three-panel layout:
//! - **Instances**: launch/monitor REAPER configurations
//! - **Setlists**: saved project collections
//! - **Recent Projects**: auto-tracked from discovery loop

use dioxus::prelude::*;

use crate::daw_registry::{DawConnectionInfo, DawRole};
use crate::launcher::{self, REAPER_CONFIGS};
use crate::persistence::{self, RecentProject, SetlistDefinition};

// ============================================================================
// Signals (written by discovery loop, read by Dashboard UI)
// ============================================================================

/// Live DAW connection info, updated by the discovery loop.
pub static DASHBOARD_CONNECTIONS: GlobalSignal<Vec<DawConnectionInfo>> = Signal::global(Vec::new);

/// Recent projects loaded from disk, refreshed when projects are discovered.
pub static RECENT_PROJECTS: GlobalSignal<Vec<RecentProject>> = Signal::global(Vec::new);

/// Setlist definitions loaded from disk.
pub static SETLIST_DEFINITIONS: GlobalSignal<Vec<SetlistDefinition>> = Signal::global(Vec::new);

// ============================================================================
// Dashboard Component
// ============================================================================

#[component]
pub fn Dashboard() -> Element {
    // Load persisted data once on first render.
    use_hook(|| {
        *RECENT_PROJECTS.write() = persistence::load_recent_projects();
        *SETLIST_DEFINITIONS.write() = persistence::load_setlists();
    });

    // Poll the DawRegistry and persistence for changes (runs inside the Dioxus
    // runtime so GlobalSignal writes are safe).
    let _poll_task = use_future(|| async {
        loop {
            tokio::time::sleep(std::time::Duration::from_secs(2)).await;

            // Update connections from registry (if initialized).
            if let Some(registry) = crate::daw_registry::DawRegistry::try_global() {
                *DASHBOARD_CONNECTIONS.write() = registry.connection_info();
            }

            // Refresh recent projects and setlists from disk.
            *RECENT_PROJECTS.write() = persistence::load_recent_projects();
            *SETLIST_DEFINITIONS.write() = persistence::load_setlists();
        }
    });

    rsx! {
        div { class: "flex flex-col h-full bg-zinc-950 text-zinc-100 p-6 gap-6 overflow-y-auto",
            h1 { class: "text-xl font-semibold tracking-tight", "Dashboard" }

            div { class: "grid grid-cols-3 gap-6 flex-1 min-h-0",
                InstancesPanel {}
                SetlistsPanel {}
                RecentProjectsPanel {}
            }
        }
    }
}

// ============================================================================
// Instances Panel
// ============================================================================

#[component]
fn InstancesPanel() -> Element {
    let connections = DASHBOARD_CONNECTIONS.read();

    rsx! {
        div { class: "flex flex-col gap-3",
            h2 { class: "text-sm font-medium text-zinc-400 uppercase tracking-wider", "Instances" }

            for config in REAPER_CONFIGS.iter() {
                {
                    let matching: Vec<&DawConnectionInfo> = connections
                        .iter()
                        .filter(|c| {
                            match (config.role, c.role) {
                                ("session", DawRole::Session) => true,
                                ("signal", DawRole::Signal) => true,
                                _ => false,
                            }
                        })
                        .collect();

                    let is_running = !matching.is_empty();
                    let config_id = config.id;

                    rsx! {
                        div {
                            class: "flex items-center justify-between p-4 rounded-lg border border-zinc-800 bg-zinc-900/50",

                            div { class: "flex items-center gap-3",
                                div {
                                    class: if is_running {
                                        "w-2.5 h-2.5 rounded-full bg-emerald-500"
                                    } else {
                                        "w-2.5 h-2.5 rounded-full bg-zinc-600"
                                    },
                                }

                                div {
                                    p { class: "text-sm font-medium text-zinc-100", "{config.label}" }
                                    if is_running {
                                        {
                                            let pids: Vec<String> = matching.iter().map(|c| format!("PID {}", c.pid)).collect();
                                            rsx! {
                                                p { class: "text-xs text-emerald-400", "Running — {pids.join(\", \")}" }
                                            }
                                        }
                                    } else {
                                        p { class: "text-xs text-zinc-500", "Stopped" }
                                    }
                                }
                            }

                            if !is_running {
                                button {
                                    class: "px-3 py-1.5 text-xs font-medium rounded-md bg-zinc-700 hover:bg-zinc-600 text-zinc-100 transition-colors",
                                    onclick: move |_| {
                                        if let Some(config) = launcher::config_by_id(config_id) {
                                            match launcher::spawn_reaper(config, &[]) {
                                                Ok(pid) => tracing::info!("Launched {} (PID {pid})", config.label),
                                                Err(e) => tracing::error!("Failed to launch {}: {e}", config.label),
                                            }
                                        }
                                    },
                                    "Launch"
                                }
                            } else {
                                span { class: "px-3 py-1.5 text-xs font-medium rounded-md bg-emerald-900/30 text-emerald-400",
                                    "Connected"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ============================================================================
// Setlists Panel
// ============================================================================

#[component]
fn SetlistsPanel() -> Element {
    let setlists = SETLIST_DEFINITIONS.read();

    rsx! {
        div { class: "flex flex-col gap-3",
            // Header with Import button
            div { class: "flex items-center justify-between",
                h2 { class: "text-sm font-medium text-zinc-400 uppercase tracking-wider", "Setlists" }

                button {
                    class: "px-2 py-1 text-xs font-medium rounded-md bg-zinc-800 hover:bg-zinc-700 text-zinc-300 transition-colors",
                    onclick: move |_| {
                        spawn(async {
                            import_rpl_dialog().await;
                        });
                    },
                    "Import RPL"
                }
            }

            if setlists.is_empty() {
                div { class: "flex items-center justify-center p-8 rounded-lg border border-dashed border-zinc-800",
                    p { class: "text-sm text-zinc-500", "No setlists defined yet" }
                }
            } else {
                for setlist in setlists.iter() {
                    {
                        let setlist_clone = setlist.clone();
                        let count = setlist.projects.len();
                        let suffix = if count != 1 { "s" } else { "" };
                        let is_auto = setlist.id == persistence::LAST_SESSION_ID;

                        rsx! {
                            div {
                                class: "flex items-center justify-between p-3 rounded-lg border border-zinc-800 bg-zinc-900/50",

                                div {
                                    div { class: "flex items-center gap-2",
                                        p { class: "text-sm font-medium text-zinc-100", "{setlist.name}" }
                                        if is_auto {
                                            span { class: "px-1.5 py-0.5 text-[10px] font-medium rounded bg-zinc-700 text-zinc-400",
                                                "AUTO"
                                            }
                                        }
                                    }
                                    p { class: "text-xs text-zinc-500",
                                        "{count} project{suffix}"
                                    }
                                }

                                button {
                                    class: "px-3 py-1.5 text-xs font-medium rounded-md bg-zinc-700 hover:bg-zinc-600 text-zinc-100 transition-colors",
                                    onclick: move |_| {
                                        let setlist = setlist_clone.clone();
                                        spawn(async move {
                                            launch_setlist_async(setlist).await;
                                        });
                                    },
                                    "Launch"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Open a native file dialog to import an `.RPL` file as a setlist.
async fn import_rpl_dialog() {
    let file = rfd::AsyncFileDialog::new()
        .add_filter("REAPER Project List", &["RPL", "rpl"])
        .set_title("Import Project List")
        .pick_file()
        .await;

    let Some(file) = file else { return };
    let path = file.path().to_string_lossy().to_string();

    let Some(new_setlist) = persistence::import_rpl(&path) else {
        tracing::warn!("Failed to import RPL from {path}");
        return;
    };

    tracing::info!(
        "Imported setlist '{}' with {} projects from RPL",
        new_setlist.name,
        new_setlist.projects.len()
    );

    // Add to persisted setlists (avoid duplicates by id).
    let mut setlists = persistence::load_setlists();
    if let Some(existing) = setlists.iter_mut().find(|s| s.id == new_setlist.id) {
        *existing = new_setlist;
    } else {
        setlists.push(new_setlist);
    }
    persistence::save_setlists(&setlists);

    // Refresh the UI signal.
    *SETLIST_DEFINITIONS.write() = setlists;
}

/// Launch a setlist: spawn a new REAPER instance, wait for it to connect,
/// then open each project via the DAW API (avoids proxy rendering issues).
async fn launch_setlist_async(setlist: SetlistDefinition) {
    let Some(config) = launcher::config_by_id(&setlist.reaper_config) else {
        tracing::error!(
            "Setlist '{}' references unknown config '{}'",
            setlist.name,
            setlist.reaper_config
        );
        return;
    };

    // Spawn REAPER with no project args — just an empty instance.
    let pid = match launcher::spawn_reaper(config, &[]) {
        Ok(pid) => {
            tracing::info!(
                "Launched REAPER for setlist '{}' (PID {pid}), waiting for connection...",
                setlist.name
            );
            pid
        }
        Err(e) => {
            tracing::error!(
                "Failed to launch REAPER for setlist '{}': {e}",
                setlist.name
            );
            return;
        }
    };

    // Wait for the discovery loop to fully register this PID (up to 15s).
    // Use is_registered() not contains() — contains() returns true when a
    // connection is in-flight but not yet classified/registered.
    let mut connected = false;
    for _ in 0..75 {
        tokio::time::sleep(std::time::Duration::from_millis(200)).await;
        if let Some(registry) = crate::daw_registry::DawRegistry::try_global() {
            if registry.is_registered(pid) {
                connected = true;
                break;
            }
        }
    }

    if !connected {
        tracing::error!(
            "Timed out waiting for REAPER PID {} to connect for setlist '{}'",
            pid,
            setlist.name
        );
        return;
    }

    // Small delay to let the DAW fully initialize after connection.
    tokio::time::sleep(std::time::Duration::from_millis(500)).await;

    // Get the DAW handle for this instance and open each project.
    let registry = crate::daw_registry::DawRegistry::try_global().unwrap();
    let entries = registry.session_daws();
    let Some(entry) = entries.iter().find(|e| e.pid == pid) else {
        tracing::error!("Could not find DAW entry for PID {pid}");
        return;
    };

    let daw = &entry.daw;
    for (i, project_path) in setlist.projects.iter().enumerate() {
        match daw.open_project(project_path).await {
            Ok(proj) => {
                tracing::info!(
                    "Opened project {}/{}: {}",
                    i + 1,
                    setlist.projects.len(),
                    project_path
                );
                let _ = proj;
            }
            Err(e) => {
                tracing::error!("Failed to open project '{}': {e}", project_path);
            }
        }
    }

    tracing::info!(
        "Setlist '{}' loaded with {} projects in REAPER PID {}",
        setlist.name,
        setlist.projects.len(),
        pid
    );
}

// ============================================================================
// Recent Projects Panel
// ============================================================================

#[component]
fn RecentProjectsPanel() -> Element {
    let recent = RECENT_PROJECTS.read();

    rsx! {
        div { class: "flex flex-col gap-3",
            h2 { class: "text-sm font-medium text-zinc-400 uppercase tracking-wider", "Recent Projects" }

            if recent.is_empty() {
                div { class: "flex items-center justify-center p-8 rounded-lg border border-dashed border-zinc-800",
                    p { class: "text-sm text-zinc-500", "Projects will appear here as REAPER opens them" }
                }
            } else {
                div { class: "flex flex-col gap-1 overflow-y-auto max-h-[600px]",
                    for project in recent.iter() {
                        {
                            let path = project.path.clone();
                            let exists = std::path::Path::new(&project.path).exists();

                            rsx! {
                                button {
                                    class: if exists {
                                        "flex flex-col items-start p-3 rounded-lg hover:bg-zinc-800/50 transition-colors text-left w-full"
                                    } else {
                                        "flex flex-col items-start p-3 rounded-lg opacity-40 text-left w-full cursor-default"
                                    },
                                    disabled: !exists,
                                    onclick: move |_| {
                                        let p = path.clone();
                                        spawn(async move {
                                            open_recent_project_async(p).await;
                                        });
                                    },

                                    p { class: "text-sm font-medium text-zinc-100", "{project.name}" }
                                    p { class: "text-xs text-zinc-500 truncate max-w-full",
                                        "{shorten_path(&project.path)}"
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Open a recent project via the DAW API.
/// If a session DAW is already running, opens it there. Otherwise spawns
/// a new instance and opens the project after connection.
async fn open_recent_project_async(path: String) {
    // Check if a session DAW is already running.
    if let Some(registry) = crate::daw_registry::DawRegistry::try_global() {
        let sessions = registry.session_daws();
        if let Some(entry) = sessions.first() {
            match entry.daw.open_project(&path).await {
                Ok(_) => {
                    tracing::info!("Opened project in existing session DAW: {path}");
                    return;
                }
                Err(e) => {
                    tracing::warn!("Failed to open in existing DAW, spawning new: {e}");
                }
            }
        }
    }

    // No running session DAW — launch as a single-project setlist.
    let setlist = SetlistDefinition {
        id: String::new(),
        name: String::new(),
        projects: vec![path],
        reaper_config: "fts-tracks".to_string(),
    };
    launch_setlist_async(setlist).await;
}

/// Shorten a path for display by replacing the home directory with `~`.
fn shorten_path(path: &str) -> String {
    if let Ok(home) = std::env::var("HOME") {
        if let Some(rest) = path.strip_prefix(&home) {
            return format!("~{rest}");
        }
    }
    path.to_string()
}
