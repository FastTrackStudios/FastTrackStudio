//! Dashboard view — landing page for fts-control.
//!
//! Three-panel layout:
//! - **Instances**: launch/monitor REAPER configurations
//! - **Setlists**: saved project collections
//! - **Recent Projects**: auto-tracked from discovery loop

use dioxus::prelude::*;

use session_ui::Session;

use crate::daw_registry::{DawConnectionInfo, DawRole};
use crate::launcher::{self, REAPER_CONFIGS};
use crate::persistence::{self, RecentProject, SetlistDefinition};
use crate::TOP_PAGE;

/// Flag set when a setlist launch navigates to the Signal performance page.
pub(crate) static SIGNAL_LAUNCHING: GlobalSignal<bool> = Signal::global(|| false);

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

            // Quick Launch grid at bottom
            QuickLaunchGrid {}
        }
    }
}

// ============================================================================
// Quick Launch Grid
// ============================================================================

/// A launch card definition.
struct LaunchCard {
    label: &'static str,
    subtitle: &'static str,
    /// Hex color for the card background.
    color: &'static str,
    icon: LaunchIcon,
    /// Launcher config id, or None if not yet available.
    config_id: Option<&'static str>,
    /// Page to navigate to after launch (e.g. "rig" for Signal page).
    navigate_to: Option<&'static str>,
}

#[derive(Clone, Copy, PartialEq)]
enum LaunchIcon {
    Tracks,
    Guitar,
    Keys,
    Bass,
    Drums,
    DrumReplacement,
    Vocals,
    Mixer,
}

const LAUNCH_CARDS: &[LaunchCard] = &[
    LaunchCard {
        label: "Tracks",
        subtitle: "Session timeline",
        color: "#d4d4d8",
        icon: LaunchIcon::Tracks,
        config_id: Some("fts-tracks"),
        navigate_to: Some("main"),
    },
    LaunchCard {
        label: "Guitar",
        subtitle: "Signal rig",
        color: "#3b82f6",
        icon: LaunchIcon::Guitar,
        config_id: Some("fts-guitar"),
        navigate_to: Some("rig"),
    },
    LaunchCard {
        label: "Keys",
        subtitle: "Keyboard stack",
        color: "#22c55e",
        icon: LaunchIcon::Keys,
        config_id: None,
        navigate_to: None,
    },
    LaunchCard {
        label: "Bass",
        subtitle: "Low-end chain",
        color: "#eab308",
        icon: LaunchIcon::Bass,
        config_id: None,
        navigate_to: None,
    },
    LaunchCard {
        label: "Drums",
        subtitle: "Kit processing",
        color: "#ef4444",
        icon: LaunchIcon::Drums,
        config_id: None,
        navigate_to: None,
    },
    LaunchCard {
        label: "Drum Replacement",
        subtitle: "Sample layering",
        color: "#f97316",
        icon: LaunchIcon::DrumReplacement,
        config_id: None,
        navigate_to: None,
    },
    LaunchCard {
        label: "Vocals",
        subtitle: "Voice FX",
        color: "#ec4899",
        icon: LaunchIcon::Vocals,
        config_id: Some("fts-guitar"),
        navigate_to: Some("rig"),
    },
    LaunchCard {
        label: "Mixer",
        subtitle: "Console view",
        color: "#a78bfa",
        icon: LaunchIcon::Mixer,
        config_id: None,
        navigate_to: None,
    },
];

#[component]
fn LaunchCardIcon(icon: LaunchIcon) -> Element {
    match icon {
        LaunchIcon::Tracks => rsx! {
            svg { view_box: "0 0 24 24", class: "w-6 h-6", fill: "none", stroke: "currentColor", stroke_width: "1.8",
                rect { x: "3", y: "4", width: "18", height: "16", rx: "2" }
                line { x1: "8", y1: "8", x2: "8", y2: "16" }
                line { x1: "12", y1: "8", x2: "12", y2: "16" }
                line { x1: "16", y1: "8", x2: "16", y2: "16" }
            }
        },
        LaunchIcon::Guitar | LaunchIcon::Bass => rsx! {
            svg { view_box: "0 0 24 24", class: "w-6 h-6", fill: "none", stroke: "currentColor", stroke_width: "1.8",
                circle { cx: "7", cy: "16", r: "3.5" }
                line { x1: "9.7", y1: "13.3", x2: "18.5", y2: "4.5" }
                circle { cx: "19.2", cy: "3.8", r: "1.2" }
                circle { cx: "20.8", cy: "5.4", r: "1.0" }
            }
        },
        LaunchIcon::Keys => rsx! {
            svg { view_box: "0 0 24 24", class: "w-6 h-6", fill: "none", stroke: "currentColor", stroke_width: "1.8",
                rect { x: "3", y: "6", width: "18", height: "12", rx: "2" }
                line { x1: "7", y1: "6", x2: "7", y2: "13" }
                line { x1: "11", y1: "6", x2: "11", y2: "13" }
                line { x1: "15", y1: "6", x2: "15", y2: "13" }
            }
        },
        LaunchIcon::Drums | LaunchIcon::DrumReplacement => rsx! {
            svg { view_box: "0 0 24 24", class: "w-6 h-6", fill: "none", stroke: "currentColor", stroke_width: "1.8",
                ellipse { cx: "12", cy: "14", rx: "6.5", ry: "3.5" }
                line { x1: "5.5", y1: "14", x2: "5.5", y2: "18" }
                line { x1: "18.5", y1: "14", x2: "18.5", y2: "18" }
                line { x1: "5.5", y1: "18", x2: "18.5", y2: "18" }
                line { x1: "7", y1: "8", x2: "11", y2: "10" }
                line { x1: "17", y1: "8", x2: "13", y2: "10" }
            }
        },
        LaunchIcon::Vocals => rsx! {
            svg { view_box: "0 0 24 24", class: "w-6 h-6", fill: "none", stroke: "currentColor", stroke_width: "1.8",
                rect { x: "9", y: "4", width: "6", height: "10", rx: "3" }
                path { d: "M6 11a6 6 0 0 0 12 0" }
                line { x1: "12", y1: "17", x2: "12", y2: "20" }
                line { x1: "9", y1: "20", x2: "15", y2: "20" }
            }
        },
        LaunchIcon::Mixer => rsx! {
            svg { view_box: "0 0 24 24", class: "w-6 h-6", fill: "none", stroke: "currentColor", stroke_width: "1.8",
                line { x1: "6", y1: "4", x2: "6", y2: "20" }
                circle { cx: "6", cy: "10", r: "2" }
                line { x1: "12", y1: "4", x2: "12", y2: "20" }
                circle { cx: "12", cy: "15", r: "2" }
                line { x1: "18", y1: "4", x2: "18", y2: "20" }
                circle { cx: "18", cy: "8", r: "2" }
            }
        },
    }
}

#[component]
fn QuickLaunchGrid() -> Element {
    let connections = DASHBOARD_CONNECTIONS.read();

    rsx! {
        div { class: "flex flex-col gap-4 flex-shrink-0",
            div { class: "flex items-end justify-between",
                div {
                    h2 { class: "text-sm font-medium text-zinc-400 uppercase tracking-[0.14em]", "Quick Launch" }
                    p { class: "text-xs text-zinc-500 mt-1", "Launch your instrument environment and jump straight into the right workspace." }
                }
            }

            div { class: "grid grid-cols-8 gap-3",
                for card in LAUNCH_CARDS.iter() {
                    {
                        let enabled = card.config_id.is_some();
                        let is_running = card.config_id.map_or(false, |id| {
                            connections.iter().any(|c| {
                                match (id, c.role) {
                                    ("fts-tracks", DawRole::Session) => true,
                                    ("fts-guitar", DawRole::Signal) => true,
                                    _ => false,
                                }
                            })
                        });

                        let text_color = if card.color == "#d4d4d8" { "#18181b" } else { "#ffffff" };

                        let bg_style = if !enabled {
                            format!(
                                "background: linear-gradient(165deg, {} 0%, #111827 100%); opacity: 0.6;",
                                card.color
                            )
                        } else {
                            format!(
                                "background: linear-gradient(165deg, {} 0%, #111827 100%);",
                                card.color
                            )
                        };

                        let config_id = card.config_id;
                        let navigate_to = card.navigate_to;

                        rsx! {
                            button {
                                class: if is_running {
                                    "relative h-88 w-full min-w-0 rounded-2xl border border-white/20 p-4 text-left transition-all duration-200 ring-2 ring-emerald-400 shadow-[0_18px_35px_rgba(16,185,129,0.35)]"
                                } else if enabled {
                                    "relative h-88 w-full min-w-0 rounded-2xl border border-white/15 p-4 text-left transition-all duration-200 hover:-translate-y-0.5 hover:shadow-[0_18px_35px_rgba(0,0,0,0.45)]"
                                } else {
                                    "relative h-88 w-full min-w-0 rounded-2xl border border-white/10 p-4 text-left cursor-default"
                                },
                                style: "{bg_style}",
                                disabled: !enabled || is_running,
                                onclick: move |_| {
                                    if let Some(id) = config_id {
                                        if let Some(config) = launcher::config_by_id(id) {
                                            match launcher::spawn_reaper(config, &[]) {
                                                Ok(pid) => {
                                                    tracing::info!("Launched {} (PID {pid})", config.label);
                                                    // Signal DAW launches set the launching flag
                                                    // so the performance tab shows "Starting..."
                                                    if id == "fts-guitar" {
                                                        *SIGNAL_LAUNCHING.write() = true;
                                                    }
                                                }
                                                Err(e) => tracing::error!("Failed to launch {}: {e}", config.label),
                                            }
                                        }
                                    }
                                    if let Some(page) = navigate_to {
                                        *TOP_PAGE.write() = page;
                                    }
                                },

                                div { class: "flex h-full flex-col justify-between",
                                    div { class: "flex items-start justify-between",
                                        div {
                                            class: "w-11 h-11 rounded-xl bg-black/25 backdrop-blur-sm flex items-center justify-center border border-white/20",
                                            style: "color: {text_color};",
                                            LaunchCardIcon { icon: card.icon }
                                        }
                                        if is_running {
                                            span {
                                                class: "px-2 py-1 text-[10px] font-semibold rounded-full bg-emerald-500/25 border border-emerald-300/40 uppercase tracking-wide",
                                                style: "color: {text_color};",
                                                "Live"
                                            }
                                        } else if !enabled {
                                            span {
                                                class: "px-2 py-1 text-[10px] font-semibold rounded-full bg-black/25 border border-white/20 uppercase tracking-wide",
                                                style: "color: {text_color};",
                                                "Soon"
                                            }
                                        }
                                    }

                                    div {
                                        h3 {
                                            class: "text-lg font-semibold leading-tight",
                                            style: "color: {text_color}; text-shadow: 0 2px 8px rgba(0,0,0,0.35);",
                                            "{card.label}"
                                        }
                                        p {
                                            class: "text-xs mt-1",
                                            style: "color: {text_color}; opacity: 0.82;",
                                            "{card.subtitle}"
                                        }
                                        if enabled && !is_running {
                                            p {
                                                class: "text-[11px] mt-2 font-medium uppercase tracking-[0.12em]",
                                                style: "color: {text_color}; opacity: 0.92;",
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
                                {
                                    let pids: Vec<u32> = matching.iter().map(|c| c.pid).collect();
                                    rsx! {
                                        button {
                                            class: "px-3 py-1.5 text-xs font-medium rounded-md bg-red-900/30 hover:bg-red-900/50 text-red-400 transition-colors",
                                            onclick: move |_| {
                                                for &pid in &pids {
                                                    launcher::kill_reaper(pid);
                                                }
                                            },
                                            "Stop"
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

    // Wait for Session to be ready (run_services initializes it after DAW connects),
    // then rebuild the setlist so the Session tab picks up the newly opened projects.
    for _ in 0..50 {
        if Session::try_get().is_some() {
            break;
        }
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
    if let Some(session) = Session::try_get() {
        // Small delay for REAPER to finish loading all project tabs.
        tokio::time::sleep(std::time::Duration::from_millis(500)).await;
        session.setlist().build_from_open_projects().await;
        tracing::info!("Rebuilt setlist after opening projects");
    }

    // Navigate to the Session tab.
    *TOP_PAGE.write() = "main";
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
                    // Rebuild the setlist so the Session tab picks up the new project.
                    if let Some(session) = Session::try_get() {
                        tokio::time::sleep(std::time::Duration::from_millis(300)).await;
                        session.setlist().build_from_open_projects().await;
                    }
                    *TOP_PAGE.write() = "main";
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
