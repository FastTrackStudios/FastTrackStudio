//! Director View components
//!
//! This module provides UI components for the Rig Director, which manages
//! multiple rigs of various instrument types with engine-based signal chains
//! and role assignments.
//!
//! Layout: Each rig is displayed as a horizontal row showing signal flow
//! left-to-right. Rigs are grouped by instrument type and stacked vertically
//! in a scrollable list.

use dioxus::prelude::*;
use fts::rig::{
    ModuleInfo, ModuleType, InstrumentType, MacroInfo, RigViewInfo, RoleInfo, DIRECTOR_CONNECTED,
    DIRECTOR_INFO, DIRECTOR_RIG_VIEWS, DIRECTOR_ROLES, DIRECTOR_SELECTED_RIG, DIRECTOR_SENDS_ENGINE,
};
use uuid::Uuid;

use crate::hooks::{use_director_actions, use_director_subscription};

// ============================================================================
// Main Director View
// ============================================================================

/// Main director view showing all rigs grouped by instrument type
#[component]
pub fn DirectorView() -> Element {
    // Subscribe to director state
    use_director_subscription();

    let info = DIRECTOR_INFO.read();
    let rig_views = DIRECTOR_RIG_VIEWS.read();
    let connected = *DIRECTOR_CONNECTED.read();

    // Group rigs by instrument type
    let groups = group_rigs_by_type(&rig_views);

    rsx! {
        div { class: "flex flex-col h-full bg-zinc-900",
            // Header
            DirectorHeader {
                name: info.as_ref().map(|i| i.name.clone()).unwrap_or_default(),
                rig_count: rig_views.len(),
                connected,
            }

            // Scrollable rig list with groups
            div { class: "flex-1 overflow-y-auto p-4 space-y-4",
                for (instrument_type, rigs) in groups.iter() {
                    InstrumentGroup {
                        key: "{instrument_type:?}",
                        instrument_type: *instrument_type,
                        rigs: rigs.clone()
                    }
                }

                if rig_views.is_empty() {
                    div { class: "flex items-center justify-center h-32 text-zinc-500",
                        "No rigs configured"
                    }
                }
            }
        }
    }
}

/// Group rigs by instrument type, maintaining the order
fn group_rigs_by_type(rigs: &[RigViewInfo]) -> Vec<(InstrumentType, Vec<RigViewInfo>)> {
    // Define the order of instrument types
    let type_order = [
        InstrumentType::Vocals,
        InstrumentType::Guitar,
        InstrumentType::Bass,
        InstrumentType::Keys,
        InstrumentType::Drums,
    ];

    let mut groups: Vec<(InstrumentType, Vec<RigViewInfo>)> = Vec::new();

    for instrument_type in type_order {
        let rigs_of_type: Vec<_> = rigs
            .iter()
            .filter(|r| r.instrument_type == instrument_type)
            .cloned()
            .collect();

        if !rigs_of_type.is_empty() {
            groups.push((instrument_type, rigs_of_type));
        }
    }

    groups
}

/// Director header with title and controls
#[component]
fn DirectorHeader(name: String, rig_count: usize, connected: bool) -> Element {
    rsx! {
        div { class: "flex items-center justify-between px-4 py-3 border-b border-zinc-700",
            // Title
            div { class: "flex items-center gap-3",
                h2 { class: "text-lg font-semibold text-white",
                    "DIRECTOR: {name}"
                }
                span { class: "px-2 py-0.5 text-xs rounded bg-zinc-700 text-zinc-300",
                    "{rig_count} rigs"
                }
            }

            // Controls
            div { class: "flex items-center gap-2",
                // Connection indicator
                div {
                    class: if connected { "w-2 h-2 rounded-full bg-green-500" } else { "w-2 h-2 rounded-full bg-red-500" },
                }

                // Add rig button (placeholder)
                button { class: "px-3 py-1 text-sm rounded bg-zinc-700 hover:bg-zinc-600 text-zinc-300",
                    "+ Add Rig"
                }

                // Settings button
                button { class: "p-1 rounded hover:bg-zinc-700 text-zinc-400",
                    "⚙"
                }
            }
        }
    }
}

// ============================================================================
// Instrument Group
// ============================================================================

/// A group of rigs of the same instrument type
#[component]
fn InstrumentGroup(instrument_type: InstrumentType, rigs: Vec<RigViewInfo>) -> Element {
    let group_color = instrument_type_color(instrument_type);
    let group_icon = instrument_type_icon(instrument_type);

    rsx! {
        div { class: "space-y-2",
            // Group header
            div { class: "flex items-center gap-2 px-2 py-1",
                span { class: "text-lg", "{group_icon}" }
                span { class: "text-sm font-medium {group_color}",
                    "{instrument_type.display_name()}"
                }
                span { class: "text-xs text-zinc-500",
                    "({rigs.len()})"
                }
            }

            // Rigs in this group
            div { class: "space-y-2 pl-2 border-l-2 {group_border_color(instrument_type)}",
                for view in rigs.iter() {
                    RigRow { key: "{view.rig_id}", view: view.clone() }
                }
            }
        }
    }
}

fn instrument_type_color(t: InstrumentType) -> &'static str {
    match t {
        InstrumentType::Vocals => "text-red-400",
        InstrumentType::Guitar => "text-teal-400",
        InstrumentType::Bass => "text-purple-400",
        InstrumentType::Keys => "text-blue-400",
        InstrumentType::Drums => "text-orange-400",
    }
}

fn instrument_type_icon(t: InstrumentType) -> &'static str {
    match t {
        InstrumentType::Vocals => "🎤",
        InstrumentType::Guitar => "🎸",
        InstrumentType::Bass => "🎸",
        InstrumentType::Keys => "🎹",
        InstrumentType::Drums => "🥁",
    }
}

fn group_border_color(t: InstrumentType) -> &'static str {
    match t {
        InstrumentType::Vocals => "border-red-800/50",
        InstrumentType::Guitar => "border-teal-800/50",
        InstrumentType::Bass => "border-purple-800/50",
        InstrumentType::Keys => "border-blue-800/50",
        InstrumentType::Drums => "border-orange-800/50",
    }
}

// ============================================================================
// Rig Row - Horizontal signal flow
// ============================================================================

/// A single rig row showing horizontal signal flow
#[component]
fn RigRow(view: RigViewInfo) -> Element {
    let actions = use_director_actions();
    let is_selected = DIRECTOR_SELECTED_RIG.read().is_some_and(|id| id == view.rig_id);

    let role_name = view
        .assigned_role
        .as_ref()
        .map(|r| r.name.clone())
        .unwrap_or_else(|| "Unassigned".to_string());

    let input_name = view
        .input
        .as_ref()
        .map(|i| i.display_name.clone())
        .unwrap_or_else(|| "No Input".to_string());

    let status_color = match view.status {
        fts::rig::RigStatus::Active => "bg-green-500",
        fts::rig::RigStatus::Idle => "bg-zinc-500",
        fts::rig::RigStatus::Muted => "bg-red-500",
        fts::rig::RigStatus::Warning => "bg-yellow-500",
        fts::rig::RigStatus::Offline => "bg-zinc-700",
    };

    let rig_id = view.rig_id;
    let rig_enabled = view.enabled;
    let instrument_type = view.instrument_type;

    rsx! {
        div {
            class: format!(
                "rounded-lg border {} p-3 {}",
                if is_selected { "border-blue-500 bg-zinc-800" } else { "border-zinc-700 bg-zinc-800/50" },
                if !view.enabled { "opacity-50" } else { "" }
            ),
            onclick: move |_| {
                *DIRECTOR_SELECTED_RIG.write() = Some(rig_id);
            },

            // Row header
            div { class: "flex items-center justify-between mb-2",
                div { class: "flex items-center gap-2",
                    // Status indicator
                    div { class: "w-2 h-2 rounded-full {status_color}" }

                    // Rig name and role
                    span { class: "font-medium text-white", "{view.rig_name}" }
                    span { class: "text-zinc-400", "({role_name})" }

                    // Input badge
                    span { class: "px-2 py-0.5 text-xs rounded bg-zinc-700 text-zinc-400",
                        "{input_name}"
                    }
                }

                // Row actions
                div { class: "flex items-center gap-1",
                    // Mute toggle
                    button {
                        class: if view.enabled { "px-2 py-1 text-xs rounded bg-zinc-700 hover:bg-zinc-600 text-zinc-300" } else { "px-2 py-1 text-xs rounded bg-red-600 text-white" },
                        onclick: move |e| {
                            e.stop_propagation();
                            actions.set_rig_enabled.call((rig_id, !rig_enabled));
                        },
                        if view.enabled { "M" } else { "M" }
                    }

                    // Menu
                    button { class: "p-1 rounded hover:bg-zinc-700 text-zinc-400",
                        onclick: move |e| e.stop_propagation(),
                        "⋮"
                    }

                    // Expand to detail dialog
                    button { class: "p-1 rounded hover:bg-zinc-700 text-zinc-400",
                        onclick: move |e| e.stop_propagation(),
                        "↗"
                    }
                }
            }

            // Signal flow - engines left to right
            div { class: "flex items-center gap-2 overflow-x-auto pb-1",
                for (i, engine) in view.engines.iter().enumerate() {
                    Fragment { key: "{engine.id}",
                        if i > 0 {
                            // Arrow between engines
                            span { class: "text-zinc-600 flex-shrink-0", "→" }
                        }
                        EngineCard {
                            rig_id: view.rig_id,
                            engine: engine.clone(),
                            instrument_type,
                        }
                    }
                }

                // Sends card (if we have sends info)
                if let Some(sends) = DIRECTOR_SENDS_ENGINE.read().as_ref() {
                    span { class: "text-zinc-600 flex-shrink-0", "→" }
                    SendsCard { sends: sends.clone(), send_levels: view.send_levels.clone() }
                }
            }
        }
    }
}

// ============================================================================
// Engine Card - Single engine with macros
// ============================================================================

/// A compact engine card showing name and macro parameters
#[component]
fn EngineCard(rig_id: Uuid, engine: ModuleInfo, instrument_type: InstrumentType) -> Element {
    let actions = use_director_actions();
    let module_type = engine.module_type;

    let (bg_color, header_color) = engine_colors(engine.module_type, instrument_type);

    rsx! {
        div {
            class: format!(
                "flex-shrink-0 w-28 rounded border {} p-2 {}",
                bg_color,
                if !engine.enabled { "opacity-40" } else { "" }
            ),
            onclick: move |e| {
                e.stop_propagation();
                // Toggle engine bypass
                actions.set_engine_enabled.call((rig_id, module_type, !engine.enabled));
            },

            // Engine name
            div { class: "text-xs font-medium {header_color} uppercase mb-1 truncate",
                "{engine.module_type}"
            }

            // Macro parameters
            div { class: "space-y-0.5",
                for mac in engine.macros.iter().take(2) {
                    MacroDisplay { mac: mac.clone() }
                }

                if engine.macros.is_empty() {
                    div { class: "text-xs text-zinc-500 italic", "No macros" }
                }
            }
        }
    }
}

/// Get colors for an engine based on type and instrument
fn engine_colors(module_type: ModuleType, instrument_type: InstrumentType) -> (&'static str, &'static str) {
    match module_type {
        // Vocal engines
        ModuleType::Rescue => ("bg-red-900/30 border-red-800", "text-red-400"),
        ModuleType::Correction => ("bg-blue-900/30 border-blue-800", "text-blue-400"),
        ModuleType::VocalModulation => ("bg-purple-900/30 border-purple-800", "text-purple-400"),

        // Shared engines (Tonal, Sends) - color by instrument
        ModuleType::Tonal => match instrument_type {
            InstrumentType::Vocals => ("bg-amber-900/30 border-amber-800", "text-amber-400"),
            InstrumentType::Drums => ("bg-orange-900/30 border-orange-800", "text-orange-400"),
            InstrumentType::Keys => ("bg-blue-900/30 border-blue-800", "text-blue-400"),
            _ => ("bg-amber-900/30 border-amber-800", "text-amber-400"),
        },
        ModuleType::Sends => ("bg-green-900/30 border-green-800", "text-green-400"),

        // Guitar/Bass engines
        ModuleType::Source => ("bg-slate-900/30 border-slate-800", "text-slate-400"),
        ModuleType::Eq => ("bg-cyan-900/30 border-cyan-800", "text-cyan-400"),
        ModuleType::Dynamics => ("bg-yellow-900/30 border-yellow-800", "text-yellow-400"),
        ModuleType::Special => ("bg-pink-900/30 border-pink-800", "text-pink-400"),
        ModuleType::Drive => ("bg-red-900/30 border-red-800", "text-red-400"),
        ModuleType::PreFx => ("bg-indigo-900/30 border-indigo-800", "text-indigo-400"),
        ModuleType::Volume => ("bg-gray-900/30 border-gray-800", "text-gray-400"),
        ModuleType::Amp => match instrument_type {
            InstrumentType::Bass => ("bg-purple-900/30 border-purple-800", "text-purple-400"),
            _ => ("bg-teal-900/30 border-teal-800", "text-teal-400"),
        },
        ModuleType::Cabinet => match instrument_type {
            InstrumentType::Bass => ("bg-purple-800/30 border-purple-700", "text-purple-300"),
            _ => ("bg-teal-800/30 border-teal-700", "text-teal-300"),
        },
        ModuleType::PostEq => ("bg-cyan-800/30 border-cyan-700", "text-cyan-300"),
        ModuleType::Modulation => ("bg-purple-900/30 border-purple-800", "text-purple-400"),
        ModuleType::PostFx => ("bg-indigo-800/30 border-indigo-700", "text-indigo-300"),
        #[allow(deprecated)]
        ModuleType::Compression => ("bg-yellow-900/30 border-yellow-800", "text-yellow-400"),

        // Drum engines
        ModuleType::Transient => ("bg-orange-900/30 border-orange-800", "text-orange-400"),
    }
}

/// Display a single macro value
#[component]
fn MacroDisplay(mac: MacroInfo) -> Element {
    rsx! {
        div { class: "flex justify-between text-xs",
            span { class: "text-zinc-400 truncate", "{mac.name}:" }
            span { class: "text-zinc-200 font-mono", "{mac.display_value}" }
        }
    }
}

// ============================================================================
// Sends Card - Per-rig send levels
// ============================================================================

/// Card showing send levels to the shared sends engine
#[component]
fn SendsCard(sends: ModuleInfo, send_levels: Vec<fts::rig::SendLevelInfo>) -> Element {
    rsx! {
        div { class: "flex-shrink-0 w-28 rounded border bg-green-900/30 border-green-800 p-2",
            // Header
            div { class: "text-xs font-medium text-green-400 uppercase mb-1",
                "SENDS"
            }

            // Send levels
            div { class: "space-y-0.5",
                for mac in sends.macros.iter().take(2) {
                    div { class: "flex justify-between text-xs",
                        span { class: "text-zinc-400 truncate", "{mac.name}:" }
                        span { class: "text-zinc-200 font-mono", "{mac.display_value}" }
                    }
                }

                if sends.macros.is_empty() {
                    for level in send_levels.iter().take(2) {
                        div { class: "flex justify-between text-xs",
                            span { class: "text-zinc-400 truncate", "{level.sends_block_name}:" }
                            span { class: "text-zinc-200 font-mono", "{level.level:.0}%" }
                        }
                    }
                }
            }
        }
    }
}

// ============================================================================
// Role Selector
// ============================================================================

/// Dropdown for selecting a role to assign to a rig
#[component]
pub fn RoleSelector(rig_id: Uuid, current_role_id: Option<Uuid>, on_select: EventHandler<Uuid>) -> Element {
    let roles = DIRECTOR_ROLES.read();
    let mut expanded = use_signal(|| false);

    let current_role = current_role_id.and_then(|id| roles.iter().find(|r| r.id == id));
    let display_name = current_role
        .map(|r| r.name.clone())
        .unwrap_or_else(|| "Select Role".to_string());

    let is_expanded = *expanded.read();

    rsx! {
        div { class: "relative",
            // Trigger button
            button {
                class: "px-3 py-1.5 text-sm rounded bg-zinc-700 hover:bg-zinc-600 text-zinc-200 flex items-center gap-2",
                onclick: move |_| expanded.set(!is_expanded),
                span { "{display_name}" }
                span { class: "text-zinc-400", "▼" }
            }

            // Dropdown
            if is_expanded {
                div { class: "absolute top-full left-0 mt-1 w-48 rounded-lg bg-zinc-800 border border-zinc-700 shadow-lg z-50",
                    for role in roles.iter() {
                        button {
                            class: format!(
                                "w-full px-3 py-2 text-left text-sm hover:bg-zinc-700 {}",
                                if current_role_id == Some(role.id) { "bg-zinc-700 text-blue-400" } else { "text-zinc-200" }
                            ),
                            onclick: {
                                let role_id = role.id;
                                move |_| {
                                    on_select.call(role_id);
                                    expanded.set(false);
                                }
                            },
                            div { class: "flex items-center gap-2",
                                // Color indicator
                                if let Some(color) = role.color {
                                    div {
                                        class: "w-3 h-3 rounded-full",
                                        style: "background-color: #{color:06x};",
                                    }
                                }
                                span { "{role.name}" }
                                span { class: "text-xs text-zinc-500", "({role.abbreviation})" }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ============================================================================
// Compact Director Panel (for embedding in sidebar)
// ============================================================================

/// Compact director panel for embedding in a sidebar
#[component]
pub fn CompactDirectorPanel() -> Element {
    use_director_subscription();

    let rig_views = DIRECTOR_RIG_VIEWS.read();

    rsx! {
        div { class: "space-y-2",
            // Header
            div { class: "flex items-center justify-between px-2 py-1",
                span { class: "text-sm font-medium text-zinc-300", "Director" }
                span { class: "text-xs text-zinc-500", "{rig_views.len()} rigs" }
            }

            // Compact rig list
            for view in rig_views.iter() {
                CompactRigRow { view: view.clone() }
            }
        }
    }
}

/// Compact rig row for sidebar display
#[component]
fn CompactRigRow(view: RigViewInfo) -> Element {
    let role_name = view
        .assigned_role
        .as_ref()
        .map(|r| r.abbreviation.clone())
        .unwrap_or_else(|| "??".to_string());

    let status_color = match view.status {
        fts::rig::RigStatus::Active => "bg-green-500",
        fts::rig::RigStatus::Idle => "bg-zinc-500",
        fts::rig::RigStatus::Muted => "bg-red-500",
        fts::rig::RigStatus::Warning => "bg-yellow-500",
        fts::rig::RigStatus::Offline => "bg-zinc-700",
    };

    let type_icon = instrument_type_icon(view.instrument_type);

    rsx! {
        div { class: "flex items-center gap-2 px-2 py-1 rounded hover:bg-zinc-800",
            // Instrument icon
            span { class: "text-xs", "{type_icon}" }

            // Status
            div { class: "w-1.5 h-1.5 rounded-full {status_color}" }

            // Role abbreviation
            span { class: "w-8 text-xs font-medium text-zinc-400", "{role_name}" }

            // Rig name
            span { class: "flex-1 text-sm text-zinc-200 truncate", "{view.rig_name}" }

            // Engine status indicators
            div { class: "flex gap-0.5",
                for engine in view.engines.iter() {
                    div {
                        class: format!(
                            "w-1.5 h-3 rounded-sm {}",
                            if engine.enabled { "bg-zinc-500" } else { "bg-zinc-700" }
                        ),
                        title: "{engine.module_type}",
                    }
                }
            }
        }
    }
}
