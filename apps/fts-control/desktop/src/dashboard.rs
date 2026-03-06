//! Dashboard view — landing page for fts-control.
//!
//! Hero layout: Quick Launch cards dominate the viewport with a detail panel
//! that slides open below when a card's options are expanded.

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

/// Which card index currently has its detail panel open (None = closed).
static OPEN_CARD_MENU: GlobalSignal<Option<usize>> = Signal::global(|| None);

// ============================================================================
// Dashboard Component
// ============================================================================

#[component]
pub fn Dashboard() -> Element {
    use_hook(|| {
        *RECENT_PROJECTS.write() = persistence::load_recent_projects();
        *SETLIST_DEFINITIONS.write() = persistence::load_setlists();
    });

    let _poll_task = use_future(|| async {
        loop {
            tokio::time::sleep(std::time::Duration::from_secs(2)).await;

            if let Some(registry) = crate::daw_registry::DawRegistry::try_global() {
                *DASHBOARD_CONNECTIONS.write() = registry.connection_info();
            }

            *RECENT_PROJECTS.write() = persistence::load_recent_projects();
            *SETLIST_DEFINITIONS.write() = persistence::load_setlists();
        }
    });

    rsx! {
        div {
            class: "flex flex-col h-full bg-zinc-950 text-zinc-100 overflow-y-auto",
            style: "background: radial-gradient(ellipse 80% 50% at 50% 35%, rgba(59,130,246,0.05) 0%, transparent 70%), #09090b;",

            DashboardHeader {}

            QuickLaunchGrid {}

            // Detail panel renders between grid and status bar
            CardDetailPanel {}

            StatusBar {}
        }
    }
}

// ============================================================================
// Dashboard Header
// ============================================================================

#[component]
fn DashboardHeader() -> Element {
    let connections = DASHBOARD_CONNECTIONS.read();
    let running_count = connections.len();
    let instance_label = if running_count == 1 {
        format!("{running_count} instance")
    } else {
        format!("{running_count} instances")
    };

    rsx! {
        div { class: "flex items-center justify-between px-6 py-4 border-b border-white/[0.06]",
            span { class: "text-sm font-medium tracking-wide text-zinc-400", "FastTrackStudio" }

            if running_count > 0 {
                div { class: "flex items-center gap-2 px-2.5 py-1 rounded-full bg-emerald-500/10 border border-emerald-500/20",
                    div { class: "w-1.5 h-1.5 rounded-full bg-emerald-400 animate-pulse-glow",
                        style: "color: rgb(52, 211, 153);",
                    }
                    span { class: "text-[11px] font-medium text-emerald-400",
                        "{instance_label}"
                    }
                }
            } else {
                div { class: "flex items-center gap-2 px-2.5 py-1 rounded-full bg-zinc-800/50 border border-zinc-700/30",
                    div { class: "w-1.5 h-1.5 rounded-full bg-zinc-600" }
                    span { class: "text-[11px] font-medium text-zinc-500", "No instances" }
                }
            }
        }
    }
}

// ============================================================================
// Quick Launch Grid
// ============================================================================

struct LaunchCard {
    label: &'static str,
    subtitle: &'static str,
    color: &'static str,
    icon: LaunchIcon,
    config_id: Option<&'static str>,
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
        color: "#71717a",
        icon: LaunchIcon::Mixer,
        config_id: None,
        navigate_to: None,
    },
];

#[component]
fn LaunchCardIcon(icon: LaunchIcon) -> Element {
    match icon {
        LaunchIcon::Tracks => rsx! {
            svg { view_box: "0 0 24 24", class: "w-7 h-7", fill: "none", stroke: "currentColor", stroke_width: "1.8",
                rect { x: "3", y: "4", width: "18", height: "16", rx: "2" }
                line { x1: "8", y1: "8", x2: "8", y2: "16" }
                line { x1: "12", y1: "8", x2: "12", y2: "16" }
                line { x1: "16", y1: "8", x2: "16", y2: "16" }
            }
        },
        LaunchIcon::Guitar | LaunchIcon::Bass => rsx! {
            svg { view_box: "0 0 24 24", class: "w-7 h-7", fill: "none", stroke: "currentColor", stroke_width: "1.8",
                circle { cx: "7", cy: "16", r: "3.5" }
                line { x1: "9.7", y1: "13.3", x2: "18.5", y2: "4.5" }
                circle { cx: "19.2", cy: "3.8", r: "1.2" }
                circle { cx: "20.8", cy: "5.4", r: "1.0" }
            }
        },
        LaunchIcon::Keys => rsx! {
            svg { view_box: "0 0 24 24", class: "w-7 h-7", fill: "none", stroke: "currentColor", stroke_width: "1.8",
                rect { x: "3", y: "6", width: "18", height: "12", rx: "2" }
                line { x1: "7", y1: "6", x2: "7", y2: "13" }
                line { x1: "11", y1: "6", x2: "11", y2: "13" }
                line { x1: "15", y1: "6", x2: "15", y2: "13" }
            }
        },
        LaunchIcon::Drums | LaunchIcon::DrumReplacement => rsx! {
            svg { view_box: "0 0 24 24", class: "w-7 h-7", fill: "none", stroke: "currentColor", stroke_width: "1.8",
                ellipse { cx: "12", cy: "14", rx: "6.5", ry: "3.5" }
                line { x1: "5.5", y1: "14", x2: "5.5", y2: "18" }
                line { x1: "18.5", y1: "14", x2: "18.5", y2: "18" }
                line { x1: "5.5", y1: "18", x2: "18.5", y2: "18" }
                line { x1: "7", y1: "8", x2: "11", y2: "10" }
                line { x1: "17", y1: "8", x2: "13", y2: "10" }
            }
        },
        LaunchIcon::Vocals => rsx! {
            svg { view_box: "0 0 24 24", class: "w-7 h-7", fill: "none", stroke: "currentColor", stroke_width: "1.8",
                rect { x: "9", y: "4", width: "6", height: "10", rx: "3" }
                path { d: "M6 11a6 6 0 0 0 12 0" }
                line { x1: "12", y1: "17", x2: "12", y2: "20" }
                line { x1: "9", y1: "20", x2: "15", y2: "20" }
            }
        },
        LaunchIcon::Mixer => rsx! {
            svg { view_box: "0 0 24 24", class: "w-7 h-7", fill: "none", stroke: "currentColor", stroke_width: "1.8",
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
        div { class: "flex-1 px-6 py-6",
            div { class: "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4",
                for (idx, card) in LAUNCH_CARDS.iter().enumerate() {
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

                        rsx! {
                            LaunchCardView {
                                card_index: idx,
                                label: card.label,
                                subtitle: card.subtitle,
                                color: card.color,
                                icon: card.icon,
                                enabled: enabled,
                                is_running: is_running,
                                config_id: card.config_id,
                                navigate_to: card.navigate_to,
                            }
                        }
                    }
                }
            }
        }
    }
}

// ============================================================================
// Launch Card
// ============================================================================

#[component]
fn LaunchCardView(
    card_index: usize,
    label: &'static str,
    subtitle: &'static str,
    color: &'static str,
    icon: LaunchIcon,
    enabled: bool,
    is_running: bool,
    config_id: Option<&'static str>,
    navigate_to: Option<&'static str>,
) -> Element {
    let panel_open = *OPEN_CARD_MENU.read() == Some(card_index);
    let has_menu = enabled;

    let card_bg = if is_running {
        "background: linear-gradient(170deg, rgba(16,185,129,0.12) 0%, rgba(16,185,129,0.04) 40%, #0a0a0f 100%);".to_string()
    } else {
        format!(
            "background: linear-gradient(170deg, {}15 0%, {}05 40%, #0a0a0f 100%);",
            color, color
        )
    };

    let strip_color = if is_running {
        "rgb(52, 211, 153)".to_string()
    } else {
        color.to_string()
    };

    let icon_bg = if is_running {
        "background: rgba(16,185,129,0.12); box-shadow: 0 0 20px rgba(16,185,129,0.08);".to_string()
    } else {
        format!("background: {}18; box-shadow: 0 0 20px {}10;", color, color)
    };

    let icon_color = if is_running {
        "rgb(110, 231, 183)".to_string()
    } else {
        color.to_string()
    };

    // Border highlights when this card's panel is open
    let outer_class = if panel_open {
        "group relative h-44 w-full rounded-xl border border-white/[0.15] p-5 text-left transition-all duration-300 overflow-hidden"
    } else if is_running {
        "group relative h-44 w-full rounded-xl border border-emerald-500/20 p-5 text-left transition-all duration-300 overflow-hidden"
    } else if enabled {
        "group relative h-44 w-full rounded-xl border border-white/[0.08] p-5 text-left transition-all duration-300 hover:-translate-y-1 hover:border-white/[0.15] overflow-hidden cursor-pointer"
    } else {
        "group relative h-44 w-full rounded-xl border border-white/[0.06] p-5 text-left transition-all duration-300 opacity-60 overflow-hidden cursor-default"
    };

    rsx! {
        div {
            class: outer_class,
            style: "{card_bg}",
            onclick: move |_| {
                if !enabled || is_running {
                    return;
                }
                if let Some(id) = config_id {
                    if let Some(config) = launcher::config_by_id(id) {
                        match launcher::spawn_reaper(config, &[]) {
                            Ok(pid) => {
                                tracing::info!("Launched {} (PID {pid})", config.label);
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

            // Hover glow overlay
            if enabled && !is_running {
                div {
                    class: "absolute inset-0 rounded-xl opacity-0 group-hover:opacity-100 transition-opacity duration-300 pointer-events-none",
                    style: "background: radial-gradient(ellipse at 30% 20%, {color}12 0%, transparent 60%);",
                }
            }

            // Hover shadow
            if enabled && !is_running {
                div {
                    class: "absolute inset-0 rounded-xl opacity-0 group-hover:opacity-100 transition-opacity duration-300 pointer-events-none",
                    style: "box-shadow: 0 2px 8px rgba(0,0,0,0.3), 0 8px 24px rgba(0,0,0,0.15);",
                }
            }

            // Card content
            div { class: "relative flex h-full flex-col justify-between z-10",
                // Top row: icon + badge + options button
                div { class: "flex items-start justify-between",
                    div {
                        class: "w-12 h-12 rounded-xl flex items-center justify-center",
                        style: "{icon_bg}; color: {icon_color};",
                        LaunchCardIcon { icon: icon }
                    }

                    div { class: "flex items-center gap-1.5",
                        if is_running {
                            div { class: "flex items-center gap-1.5 px-2 py-1 rounded-full bg-emerald-500/15 border border-emerald-500/25",
                                div { class: "w-1.5 h-1.5 rounded-full bg-emerald-400 animate-pulse-glow",
                                    style: "color: rgb(52, 211, 153);",
                                }
                                span { class: "text-[10px] font-semibold text-emerald-400 uppercase tracking-wide", "Live" }
                            }
                        } else if !enabled {
                            span { class: "px-2 py-1 text-[10px] font-semibold rounded-full bg-zinc-800/60 border border-white/[0.08] text-zinc-500 uppercase tracking-wide",
                                "Coming Soon"
                            }
                        }

                        // Options button — toggles the detail panel below the grid
                        if has_menu {
                            div {
                                class: if panel_open {
                                    "w-7 h-7 rounded-lg flex items-center justify-center bg-white/10 text-zinc-300 cursor-pointer"
                                } else {
                                    "w-7 h-7 rounded-lg flex items-center justify-center text-zinc-600 hover:text-zinc-400 hover:bg-white/[0.06] cursor-pointer opacity-0 group-hover:opacity-100 transition-all duration-200"
                                },
                                onclick: move |evt| {
                                    evt.stop_propagation();
                                    if panel_open {
                                        *OPEN_CARD_MENU.write() = None;
                                    } else {
                                        *OPEN_CARD_MENU.write() = Some(card_index);
                                    }
                                },
                                svg { view_box: "0 0 16 16", class: "w-4 h-4", fill: "currentColor",
                                    circle { cx: "3", cy: "8", r: "1.5" }
                                    circle { cx: "8", cy: "8", r: "1.5" }
                                    circle { cx: "13", cy: "8", r: "1.5" }
                                }
                            }
                        }
                    }
                }

                // Bottom: label, subtitle, launch hint
                div {
                    h3 { class: "text-base font-semibold text-zinc-100 leading-tight", "{label}" }
                    p { class: "text-xs text-zinc-500 mt-0.5", "{subtitle}" }
                    if enabled && !is_running {
                        p { class: "text-[11px] text-zinc-600 mt-1.5 font-medium uppercase tracking-[0.12em] opacity-0 group-hover:opacity-100 transition-opacity duration-300",
                            "Launch →"
                        }
                    }
                }
            }

            // Bottom accent strip (LED indicator)
            div {
                class: if is_running { "absolute bottom-0 left-0 right-0 h-px animate-pulse-glow" } else { "absolute bottom-0 left-0 right-0 h-px" },
                style: "background: linear-gradient(90deg, transparent, {strip_color}, transparent); color: {strip_color};",
            }
        }
    }
}

// ============================================================================
// Card Detail Panel (Ableton-style detail view below the grid)
// ============================================================================

#[component]
fn CardDetailPanel() -> Element {
    let open_idx = *OPEN_CARD_MENU.read();
    let Some(idx) = open_idx else {
        return rsx! {};
    };

    let Some(card) = LAUNCH_CARDS.get(idx) else {
        return rsx! {};
    };

    let connections = DASHBOARD_CONNECTIONS.read();
    let is_running = card.config_id.map_or(false, |id| {
        connections.iter().any(|c| matches!(
            (id, c.role),
            ("fts-tracks", DawRole::Session) | ("fts-guitar", DawRole::Signal)
        ))
    });
    let is_tracks = card.config_id == Some("fts-tracks");
    let config_id = card.config_id;
    let color = card.color;

    rsx! {
        div { class: "mx-6 mb-4 rounded-xl border border-white/[0.08] overflow-hidden",
            style: "background: linear-gradient(180deg, {color}08 0%, rgba(9,9,11,0.95) 100%);",

            // Panel header
            div { class: "flex items-center justify-between px-5 py-3 border-b border-white/[0.06]",
                div { class: "flex items-center gap-3",
                    div { class: "w-2 h-2 rounded-full",
                        style: "background: {color};",
                    }
                    span { class: "text-xs font-semibold text-zinc-300", "{card.label}" }
                    span { class: "text-[10px] text-zinc-600", "{card.subtitle}" }
                }

                div { class: "flex items-center gap-2",
                    // Stop button (when running)
                    if is_running {
                        div {
                            class: "px-2.5 py-1 text-[11px] font-medium rounded-md bg-red-900/30 hover:bg-red-900/50 text-red-400 cursor-pointer transition-colors",
                            onclick: move |_| {
                                if let Some(id) = config_id {
                                    let connections = DASHBOARD_CONNECTIONS.read();
                                    let pids: Vec<u32> = connections
                                        .iter()
                                        .filter(|c| matches!(
                                            (id, c.role),
                                            ("fts-tracks", DawRole::Session) | ("fts-guitar", DawRole::Signal)
                                        ))
                                        .map(|c| c.pid)
                                        .collect();
                                    for pid in pids {
                                        launcher::kill_reaper(pid);
                                    }
                                }
                            },
                            "Stop"
                        }
                    }

                    // Close button
                    div {
                        class: "w-6 h-6 rounded-md flex items-center justify-center text-zinc-600 hover:text-zinc-400 hover:bg-white/[0.06] cursor-pointer transition-colors",
                        onclick: move |_| {
                            *OPEN_CARD_MENU.write() = None;
                        },
                        svg { view_box: "0 0 16 16", class: "w-3.5 h-3.5", fill: "none", stroke: "currentColor", stroke_width: "2",
                            path { d: "M4 4l8 8M12 4l-8 8" }
                        }
                    }
                }
            }

            // Panel body
            if is_tracks {
                TracksDetailBody {}
            }
        }
    }
}

/// Detail body for the Tracks card — setlists and recent projects side by side.
#[component]
fn TracksDetailBody() -> Element {
    let setlists = SETLIST_DEFINITIONS.read();
    let recent = RECENT_PROJECTS.read();

    rsx! {
        div { class: "grid grid-cols-1 lg:grid-cols-2 divide-y lg:divide-y-0 lg:divide-x divide-white/[0.06]",

            // Left column: Setlists
            div { class: "p-4",
                div { class: "flex items-center justify-between mb-3",
                    span { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-[0.14em]", "Setlists" }
                    div {
                        class: "flex items-center gap-1.5 px-2 py-1 rounded-md text-[11px] font-medium text-zinc-400 hover:text-zinc-300 hover:bg-white/[0.04] cursor-pointer transition-colors",
                        onclick: move |_| {
                            spawn(async {
                                import_rpl_dialog().await;
                            });
                        },
                        svg { view_box: "0 0 16 16", class: "w-3 h-3", fill: "none", stroke: "currentColor", stroke_width: "1.5",
                            path { d: "M8 3v10M3 8h10" }
                        }
                        "Import RPL"
                    }
                }

                if setlists.is_empty() {
                    div { class: "flex items-center justify-center py-8",
                        p { class: "text-[11px] text-zinc-600", "No setlists — import an RPL file to get started" }
                    }
                } else {
                    div { class: "flex flex-col gap-1 max-h-[200px] overflow-y-auto",
                        for setlist in setlists.iter() {
                            {
                                let setlist_clone = setlist.clone();
                                let count = setlist.projects.len();
                                let suffix = if count != 1 { "s" } else { "" };
                                let is_auto = setlist.id == persistence::LAST_SESSION_ID;

                                rsx! {
                                    div {
                                        class: "flex items-center justify-between px-3 py-2 rounded-lg hover:bg-white/[0.03] cursor-pointer transition-colors group/item",
                                        onclick: move |_| {
                                            let setlist = setlist_clone.clone();
                                            spawn(async move {
                                                launch_setlist_async(setlist).await;
                                            });
                                            *OPEN_CARD_MENU.write() = None;
                                        },

                                        div { class: "flex items-center gap-2 min-w-0",
                                            // Folder icon
                                            svg { view_box: "0 0 16 16", class: "w-3.5 h-3.5 text-zinc-600 shrink-0", fill: "none", stroke: "currentColor", stroke_width: "1.3",
                                                path { d: "M2 4.5h4l1.5 1.5H14v7H2z" }
                                            }
                                            span { class: "text-xs text-zinc-300 truncate", "{setlist.name}" }
                                            if is_auto {
                                                span { class: "text-[8px] font-semibold text-zinc-600 bg-zinc-800/80 px-1 rounded shrink-0", "AUTO" }
                                            }
                                        }
                                        div { class: "flex items-center gap-2 shrink-0",
                                            span { class: "text-[10px] text-zinc-600", "{count} project{suffix}" }
                                            // Arrow on hover
                                            span { class: "text-[10px] text-zinc-600 opacity-0 group-hover/item:opacity-100 transition-opacity", "→" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Right column: Recent Projects
            div { class: "p-4",
                div { class: "flex items-center justify-between mb-3",
                    span { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-[0.14em]", "Recent Projects" }
                    if !recent.is_empty() {
                        span { class: "text-[10px] text-zinc-700", "{recent.len()}" }
                    }
                }

                if recent.is_empty() {
                    div { class: "flex items-center justify-center py-8",
                        p { class: "text-[11px] text-zinc-600", "Projects appear as REAPER opens them" }
                    }
                } else {
                    div { class: "flex flex-col gap-0.5 max-h-[200px] overflow-y-auto",
                        for project in recent.iter().take(12) {
                            {
                                let path = project.path.clone();
                                let exists = std::path::Path::new(&project.path).exists();
                                rsx! {
                                    div {
                                        class: if exists {
                                            "flex items-center justify-between px-3 py-1.5 rounded-lg hover:bg-white/[0.03] cursor-pointer transition-colors group/item"
                                        } else {
                                            "flex items-center justify-between px-3 py-1.5 rounded-lg opacity-35 cursor-default"
                                        },
                                        onclick: move |_| {
                                            if exists {
                                                let p = path.clone();
                                                spawn(async move {
                                                    open_recent_project_async(p).await;
                                                });
                                                *OPEN_CARD_MENU.write() = None;
                                            }
                                        },

                                        div { class: "min-w-0",
                                            p { class: "text-xs text-zinc-300 truncate", "{project.name}" }
                                            p { class: "text-[10px] text-zinc-700 truncate", "{shorten_path(&project.path)}" }
                                        }
                                        if exists {
                                            span { class: "text-[10px] text-zinc-600 opacity-0 group-hover/item:opacity-100 transition-opacity shrink-0 ml-2", "→" }
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
// Status Bar
// ============================================================================

#[component]
fn StatusBar() -> Element {
    let connections = DASHBOARD_CONNECTIONS.read();

    let config_statuses: Vec<(&str, Vec<u32>)> = REAPER_CONFIGS
        .iter()
        .map(|config| {
            let pids: Vec<u32> = connections
                .iter()
                .filter(|c| matches!(
                    (config.role, c.role),
                    ("session", DawRole::Session) | ("signal", DawRole::Signal)
                ))
                .map(|c| c.pid)
                .collect();
            (config.label, pids)
        })
        .collect();

    rsx! {
        div { class: "flex items-center justify-between px-6 py-2.5 border-t border-white/[0.06] mt-auto",
            style: "background: rgba(9,9,11,0.8);",

            div { class: "flex items-center gap-3",
                for (label, pids) in config_statuses.iter() {
                    {
                        let is_running = !pids.is_empty();
                        rsx! {
                            div { class: "flex items-center gap-1.5",
                                div {
                                    class: if is_running {
                                        "w-1.5 h-1.5 rounded-full bg-emerald-400"
                                    } else {
                                        "w-1.5 h-1.5 rounded-full bg-zinc-700"
                                    },
                                    style: if is_running { "box-shadow: 0 0 6px rgba(16,185,129,0.4);" } else { "" },
                                }
                                span { class: "text-[10px] text-zinc-500 font-medium", "{label}" }
                                if is_running {
                                    {
                                        let pid_str = pids.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", ");
                                        rsx! {
                                            span { class: "text-[10px] text-zinc-600", "({pid_str})" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            span { class: "text-[10px] text-zinc-700 font-mono", "FTS Control v0.1" }
        }
    }
}

// ============================================================================
// Async Helpers
// ============================================================================

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

    let mut setlists = persistence::load_setlists();
    if let Some(existing) = setlists.iter_mut().find(|s| s.id == new_setlist.id) {
        *existing = new_setlist;
    } else {
        setlists.push(new_setlist);
    }
    persistence::save_setlists(&setlists);

    *SETLIST_DEFINITIONS.write() = setlists;
}

/// Launch a setlist: spawn a new REAPER instance, wait for it to connect,
/// then open each project via the DAW API.
pub(crate) async fn launch_setlist_async(setlist: SetlistDefinition) {
    let Some(config) = launcher::config_by_id(&setlist.reaper_config) else {
        tracing::error!(
            "Setlist '{}' references unknown config '{}'",
            setlist.name,
            setlist.reaper_config
        );
        return;
    };

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

    tokio::time::sleep(std::time::Duration::from_millis(500)).await;

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

    for _ in 0..50 {
        if Session::try_get().is_some() {
            break;
        }
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
    if let Some(session) = Session::try_get() {
        tokio::time::sleep(std::time::Duration::from_millis(500)).await;
        session.setlist().build_from_open_projects().await;
        tracing::info!("Rebuilt setlist after opening projects");
    }

    *TOP_PAGE.write() = "main";
}

/// Open a recent project via the DAW API.
async fn open_recent_project_async(path: String) {
    if let Some(registry) = crate::daw_registry::DawRegistry::try_global() {
        let sessions = registry.session_daws();
        if let Some(entry) = sessions.first() {
            match entry.daw.open_project(&path).await {
                Ok(_) => {
                    tracing::info!("Opened project in existing session DAW: {path}");
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
