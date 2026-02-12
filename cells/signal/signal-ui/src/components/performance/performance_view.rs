//! Performance View — live performance cockpit for song/scene navigation.
//!
//! Layout:
//! ┌──────────────────────────────────────────────────────┐
//! │  Song: "Amazing Grace"   Scene: Verse 1   ◀  ▶      │  ← Song/Scene nav bar
//! ├────────────────────────────────────────────┬─────────┤
//! │                                            │ Snap-   │
//! │   ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐        │ shots   │
//! │   │ Src │ │ EQ  │ │ Drv │ │ Amp │ ...     │         │
//! │   │  ●  │ │  ●  │ │  ○  │ │  ●  │        │  Scene1 │
//! │   └─────┘ └─────┘ └─────┘ └─────┘        │  Scene2 │
//! │                                            │  Scene3 │
//! │   Module status grid                       │ *Scene4*│
//! │                                            │  Scene5 │
//! ├────────────────────────────────────────────┴─────────┤
//! │  ▐ Intro ▐ Verse 1 ▐ Chorus ▐ Verse 2 ▐ Bridge ▐   │  ← Scene strip
//! └──────────────────────────────────────────────────────┘

use crate::prelude::*;
use crate::signals::{
    RIG_CURRENT_PRESET, RIG_CURRENT_SONG, RIG_LAST_APPLIED_SNAPSHOT, RIG_MODULES,
    RIG_SECTION_INDEX, RIG_SETLIST_SONGS, RIG_SONG_INDEX,
};
use signal_control::module::{Module, ModuleType};
use uuid::Uuid;

/// Main performance view component.
#[component]
pub fn PerformanceView() -> Element {
    // Clone everything out of signals immediately so read guards are dropped
    // before any event handlers can trigger writes (prevents AlreadyBorrowed panics).
    let songs = RIG_SETLIST_SONGS.cloned();
    let current_song = RIG_CURRENT_SONG.cloned();
    let song_index = *RIG_SONG_INDEX.read();
    let scene_index = *RIG_SECTION_INDEX.read();
    let modules = RIG_MODULES.cloned();
    let preset = RIG_CURRENT_PRESET.cloned();
    let active_snapshot_id = *RIG_LAST_APPLIED_SNAPSHOT.read();

    rsx! {
        div { class: "h-full w-full flex flex-col bg-zinc-950 overflow-hidden",
            // ── Song/Scene Navigation Bar ────────────────────────
            SongSceneNav {
                current_song: current_song.clone(),
                song_index,
                scene_index,
                song_count: songs.len(),
            }

            // ── Main content area ────────────────────────────────
            div { class: "flex-1 flex min-h-0 overflow-hidden",
                // ── Module status grid (center) ──────────────────
                div { class: "flex-1 overflow-y-auto p-4",
                    // Preset header
                    if let Some(ref p) = preset {
                        div { class: "mb-4",
                            h2 { class: "text-2xl font-bold text-zinc-100", "{p.name}" }
                            if active_snapshot_id.is_some() {
                                p { class: "text-sm text-emerald-400 mt-0.5",
                                    "Snapshot active"
                                }
                            }
                        }
                    } else {
                        div { class: "mb-4",
                            h2 { class: "text-2xl font-bold text-zinc-500", "No Preset Loaded" }
                        }
                    }

                    // Module grid
                    if modules.is_empty() {
                        div { class: "text-center text-zinc-500 py-12",
                            p { class: "text-lg", "No modules loaded" }
                            p { class: "text-sm mt-1", "Load a preset or bind an FX chain to see modules here" }
                        }
                    } else {
                        div { class: "grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5 gap-3",
                            for module in modules.iter() {
                                ModuleStatusCard {
                                    key: "{module.id}",
                                    module: module.clone(),
                                }
                            }
                        }
                    }
                }

                // ── Snapshot quick-access sidebar (right) ────────
                div { class: "w-52 border-l border-zinc-800 flex flex-col bg-zinc-900/50 flex-shrink-0",
                    div { class: "px-3 py-2 border-b border-zinc-800",
                        h3 { class: "text-xs font-semibold text-zinc-400 uppercase tracking-wider",
                            "Snapshots"
                        }
                    }
                    div { class: "flex-1 overflow-y-auto",
                        if preset.is_some() {
                            div { class: "px-3 py-4 text-xs text-zinc-600 text-center",
                                "Snapshots loaded from DB"
                            }
                        } else {
                            div { class: "px-3 py-4 text-xs text-zinc-600 text-center",
                                "Load a preset to see snapshots"
                            }
                        }
                    }
                }
            }

            // ── Scene strip (bottom) ─────────────────────────────
            SceneStrip {
                current_song: current_song.clone(),
                scene_index,
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Song/Scene Navigation Bar
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct SongSceneNavProps {
    current_song: Option<signal_control::SongInfo>,
    song_index: usize,
    scene_index: usize,
    song_count: usize,
}

#[component]
fn SongSceneNav(props: SongSceneNavProps) -> Element {
    let song_name = props
        .current_song
        .as_ref()
        .map(|s| s.name.as_str())
        .unwrap_or("No Song");

    let scene_name = props
        .current_song
        .as_ref()
        .and_then(|s| s.section_names.get(props.scene_index))
        .map(|n| n.as_str())
        .unwrap_or("—");

    let scene_count = props
        .current_song
        .as_ref()
        .map(|s| s.section_count)
        .unwrap_or(0);

    let song_idx = props.song_index;
    let scene_idx = props.scene_index;
    let song_count = props.song_count;
    let has_prev_song = song_idx > 0;
    let has_next_song = song_idx + 1 < song_count;
    let has_prev_scene = scene_idx > 0;
    let has_next_scene = scene_idx + 1 < scene_count;

    rsx! {
        div { class: "flex items-center gap-4 px-4 py-2.5 border-b border-zinc-800 bg-zinc-900/80 flex-shrink-0",
            // Song section
            div { class: "flex items-center gap-2",
                button {
                    class: "w-8 h-8 flex items-center justify-center rounded-md text-zinc-400 hover:text-zinc-100 hover:bg-zinc-700 disabled:opacity-30 disabled:cursor-not-allowed transition-colors",
                    disabled: !has_prev_song,
                    onclick: move |_| {
                        if has_prev_song {
                            *RIG_SONG_INDEX.write() = song_idx.saturating_sub(1);
                            *RIG_SECTION_INDEX.write() = 0;
                            update_current_song();
                        }
                    },
                    "◀"
                }
                div { class: "min-w-[140px]",
                    span { class: "text-[10px] font-medium text-zinc-500 uppercase tracking-wider block",
                        "Song"
                    }
                    span { class: "text-sm font-semibold text-zinc-100 block truncate",
                        "{song_name}"
                    }
                }
                button {
                    class: "w-8 h-8 flex items-center justify-center rounded-md text-zinc-400 hover:text-zinc-100 hover:bg-zinc-700 disabled:opacity-30 disabled:cursor-not-allowed transition-colors",
                    disabled: !has_next_song,
                    onclick: move |_| {
                        if has_next_song {
                            *RIG_SONG_INDEX.write() = song_idx + 1;
                            *RIG_SECTION_INDEX.write() = 0;
                            update_current_song();
                        }
                    },
                    "▶"
                }
            }

            // Divider
            div { class: "w-px h-8 bg-zinc-700" }

            // Scene section
            div { class: "flex items-center gap-2",
                button {
                    class: "w-8 h-8 flex items-center justify-center rounded-md text-zinc-400 hover:text-zinc-100 hover:bg-zinc-700 disabled:opacity-30 disabled:cursor-not-allowed transition-colors",
                    disabled: !has_prev_scene,
                    onclick: move |_| {
                        if has_prev_scene {
                            *RIG_SECTION_INDEX.write() = scene_idx.saturating_sub(1);
                        }
                    },
                    "◀"
                }
                div { class: "min-w-[120px]",
                    span { class: "text-[10px] font-medium text-zinc-500 uppercase tracking-wider block",
                        "Scene"
                    }
                    span { class: "text-sm font-semibold text-emerald-400 block truncate",
                        "{scene_name}"
                    }
                }
                button {
                    class: "w-8 h-8 flex items-center justify-center rounded-md text-zinc-400 hover:text-zinc-100 hover:bg-zinc-700 disabled:opacity-30 disabled:cursor-not-allowed transition-colors",
                    disabled: !has_next_scene,
                    onclick: move |_| {
                        if has_next_scene {
                            *RIG_SECTION_INDEX.write() = scene_idx + 1;
                        }
                    },
                    "▶"
                }
            }

            // Spacer
            div { class: "flex-1" }

            // Song counter
            if song_count > 0 {
                div { class: "text-xs text-zinc-500",
                    "Song {song_idx + 1}/{song_count}"
                }
            }
        }
    }
}

/// Update `RIG_CURRENT_SONG` to match the current `RIG_SONG_INDEX`.
///
/// Clones the song out before writing to avoid holding a read guard
/// across the write (which would cause an `AlreadyBorrowed` panic).
fn update_current_song() {
    let song = {
        let idx = *RIG_SONG_INDEX.read();
        let songs = RIG_SETLIST_SONGS.read();
        songs.get(idx).cloned()
    }; // read guards dropped here
    *RIG_CURRENT_SONG.write() = song;
}

// ─────────────────────────────────────────────────────────────────────────────
// Module Status Card
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct ModuleStatusCardProps {
    module: Module,
}

#[component]
fn ModuleStatusCard(props: ModuleStatusCardProps) -> Element {
    let m = &props.module;
    let is_enabled = m.enabled;
    let module_id = m.id;
    let block_count = m.blocks.len();
    let block_summary = if block_count == 1 {
        "1 block".to_string()
    } else {
        format!("{block_count} blocks")
    };

    let accent_color = module_type_accent(m.module_type);
    let opacity = if is_enabled {
        "opacity-100"
    } else {
        "opacity-40"
    };

    rsx! {
        div {
            class: "rounded-lg border border-zinc-800 bg-zinc-900/80 p-3 {opacity} transition-opacity hover:border-zinc-600 cursor-pointer",
            // Header: module name + bypass toggle
            div { class: "flex items-center justify-between mb-2",
                div { class: "flex items-center gap-2 min-w-0",
                    // Color dot
                    span {
                        class: "w-2.5 h-2.5 rounded-full flex-shrink-0",
                        style: "background-color: {accent_color}",
                    }
                    span { class: "text-sm font-semibold text-zinc-100 truncate",
                        "{m.name}"
                    }
                }
                // Bypass toggle
                button {
                    class: if is_enabled {
                        "w-6 h-6 rounded-full flex items-center justify-center text-xs bg-emerald-500/20 text-emerald-400 hover:bg-emerald-500/30 transition-colors"
                    } else {
                        "w-6 h-6 rounded-full flex items-center justify-center text-xs bg-zinc-700 text-zinc-500 hover:bg-zinc-600 transition-colors"
                    },
                    onclick: move |evt| {
                        evt.stop_propagation();
                        // Toggle module enabled state
                        let mut modules = RIG_MODULES.write();
                        if let Some(module) = modules.iter_mut().find(|mod_| mod_.id == module_id) {
                            module.enabled = !module.enabled;
                        }
                    },
                    if is_enabled { "ON" } else { "OFF" }
                }
            }
            // Module type label
            span { class: "text-[10px] font-medium text-zinc-500 uppercase tracking-wider",
                "{m.module_type.display_name()}"
            }
            // Block summary
            div { class: "mt-1.5 text-xs text-zinc-400",
                "{block_summary}"
            }
            // Block names (compact list)
            if !m.blocks.is_empty() {
                div { class: "mt-1.5 flex flex-wrap gap-1",
                    for mb in m.blocks.iter().take(4) {
                        span {
                            key: "{mb.block.name}",
                            class: "px-1.5 py-0.5 rounded text-[10px] bg-zinc-800 text-zinc-400",
                            "{mb.block.name}"
                        }
                    }
                    if m.blocks.len() > 4 {
                        {
                            let extra = m.blocks.len() - 4;
                            rsx! {
                                span { class: "px-1.5 py-0.5 rounded text-[10px] bg-zinc-800 text-zinc-500",
                                    "+{extra}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Map module type to an accent color for the status card dot.
fn module_type_accent(mt: ModuleType) -> &'static str {
    match mt {
        ModuleType::Source => "#6366f1",                      // indigo
        ModuleType::Eq | ModuleType::PostEq => "#3b82f6",     // blue
        ModuleType::Dynamics => "#8b5cf6",                    // violet
        ModuleType::Special | ModuleType::PreFx => "#ec4899", // pink
        ModuleType::Drive => "#f97316",                       // orange
        ModuleType::Amp => "#ef4444",                         // red
        ModuleType::Volume => "#6b7280",                      // gray
        ModuleType::Modulation | ModuleType::VocalModulation => "#a855f7", // purple
        ModuleType::Time => "#06b6d4",                        // cyan
        ModuleType::Motion => "#14b8a6",                      // teal
        ModuleType::Master => "#f59e0b",                      // amber
        _ => "#71717a",                                       // zinc
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Snapshot Quick-Access Item
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct SnapshotItemProps {
    id: Uuid,
    name: String,
    is_active: bool,
}

#[component]
fn SnapshotItem(props: SnapshotItemProps) -> Element {
    let snap_id = props.id;

    rsx! {
        button {
            class: if props.is_active {
                "w-full text-left px-3 py-2 text-sm font-medium bg-emerald-500/15 text-emerald-400 border-l-2 border-emerald-500"
            } else {
                "w-full text-left px-3 py-2 text-sm text-zinc-400 hover:bg-zinc-800 hover:text-zinc-200 border-l-2 border-transparent transition-colors"
            },
            onclick: move |_| {
                *RIG_LAST_APPLIED_SNAPSHOT.write() = Some(signal_control::id::RigPresetId::from_uuid(snap_id));
            },
            "{props.name}"
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Scene Strip (Bottom)
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct SceneStripProps {
    current_song: Option<signal_control::SongInfo>,
    scene_index: usize,
}

#[component]
fn SceneStrip(props: SceneStripProps) -> Element {
    let Some(ref song) = props.current_song else {
        return rsx! {
            div { class: "h-12 border-t border-zinc-800 bg-zinc-900/60 flex items-center justify-center flex-shrink-0",
                span { class: "text-xs text-zinc-600", "No song selected" }
            }
        };
    };

    if song.section_names.is_empty() {
        return rsx! {
            div { class: "h-12 border-t border-zinc-800 bg-zinc-900/60 flex items-center justify-center flex-shrink-0",
                span { class: "text-xs text-zinc-600", "No scenes in this song" }
            }
        };
    }

    let scene_idx = props.scene_index;

    rsx! {
        div { class: "border-t border-zinc-800 bg-zinc-900/60 flex-shrink-0",
            div { class: "flex items-center gap-1 px-3 py-2 overflow-x-auto",
                for (i, name) in song.section_names.iter().enumerate() {
                    {
                        let is_current = i == scene_idx;
                        let is_past = i < scene_idx;
                        rsx! {
                            div { class: "flex items-center gap-1",
                                key: "scene-{i}",
                                // Arrow between scenes
                                if i > 0 {
                                    span { class: "text-zinc-700 text-xs flex-shrink-0", "→" }
                                }
                                // Scene chip
                                button {
                                    class: if is_current {
                                        "px-3 py-1.5 rounded-md text-xs font-semibold bg-emerald-500/20 text-emerald-400 border border-emerald-500/40 whitespace-nowrap"
                                    } else if is_past {
                                        "px-3 py-1.5 rounded-md text-xs font-medium bg-zinc-800/60 text-zinc-500 border border-zinc-800 whitespace-nowrap hover:bg-zinc-700/60 hover:text-zinc-300 transition-colors"
                                    } else {
                                        "px-3 py-1.5 rounded-md text-xs font-medium bg-zinc-800/40 text-zinc-400 border border-zinc-800 whitespace-nowrap hover:bg-zinc-700/60 hover:text-zinc-200 transition-colors"
                                    },
                                    onclick: move |_| {
                                        *RIG_SECTION_INDEX.write() = i;
                                    },
                                    "{name}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
