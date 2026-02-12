//! Guitar rig right sidebar component.
//!
//! Fixed sidebar with:
//! - Top panel: Scenes list with navigation
//! - Bottom panel: Songs list with setlist dropdown

use crate::prelude::*;
use crate::signals::{
    RIG_AVAILABLE_SETLISTS, RIG_CURRENT_SETLIST, RIG_CURRENT_SONG, RIG_SECTION_INDEX,
    RIG_SETLIST_SONGS, RIG_SONG_INDEX,
};
use signal_control::SongInfo;
use uuid::Uuid;

/// Props for the guitar rig right sidebar.
#[derive(Props, Clone, PartialEq)]
pub struct GuitarRigRightSidebarProps {
    /// Callback when a scene is clicked.
    pub on_scene_click: Callback<usize>,
    /// Callback when a song is clicked.
    pub on_song_click: Callback<usize>,
    /// Callback to go to previous scene.
    pub on_prev_scene: Callback<()>,
    /// Callback to go to next scene.
    pub on_next_scene: Callback<()>,
    /// Callback to go to previous song.
    pub on_prev_song: Callback<()>,
    /// Callback to go to next song.
    pub on_next_song: Callback<()>,
    /// Callback when setlist is changed.
    pub on_setlist_change: Callback<Uuid>,
    /// Callback to create a new song.
    #[props(default)]
    pub on_create_song: Option<Callback<()>>,
    /// Callback to create a new scene.
    #[props(default)]
    pub on_create_scene: Option<Callback<()>>,
}

/// Right sidebar for the guitar rig page.
///
/// Features:
/// - Scenes panel (top) with current song context
/// - Songs panel (bottom) with setlist dropdown
/// - Navigation buttons for both panels
#[component]
pub fn GuitarRigRightSidebar(props: GuitarRigRightSidebarProps) -> Element {
    rsx! {
        div { class: "h-full w-full flex flex-col bg-card border-l border-border",
            // === SCENES PANEL (top) ===
            div { class: "flex-1 flex flex-col min-h-0 border-b border-zinc-800",
                // Song header with scene info
                SongHeader {}

                // Scene list
                div { class: "flex-1 overflow-y-auto p-2",
                    div { class: "flex items-center justify-between mb-2 px-2",
                        h3 { class: "text-xs font-semibold text-zinc-500 uppercase tracking-wider",
                            "Scenes"
                        }
                        if let Some(ref on_create) = props.on_create_scene {
                            {
                                let on_create = on_create.clone();
                                rsx! {
                                    button {
                                        class: "w-5 h-5 flex items-center justify-center rounded \
                                                text-zinc-500 hover:text-zinc-200 hover:bg-zinc-700 \
                                                transition-colors text-sm leading-none",
                                        title: "New Scene",
                                        onclick: move |_| on_create.call(()),
                                        "+"
                                    }
                                }
                            }
                        }
                    }
                    SceneList { on_scene_click: props.on_scene_click.clone() }
                }

                // Scene navigation buttons
                div { class: "p-2 border-t border-zinc-800 flex gap-2",
                    button {
                        class: "flex-1 px-2 py-1.5 bg-zinc-800 hover:bg-zinc-700 rounded text-xs font-medium \
                                text-zinc-300 transition-colors",
                        onclick: move |_| props.on_prev_scene.call(()),
                        "← Prev"
                    }
                    button {
                        class: "flex-1 px-2 py-1.5 bg-zinc-800 hover:bg-zinc-700 rounded text-xs font-medium \
                                text-zinc-300 transition-colors",
                        onclick: move |_| props.on_next_scene.call(()),
                        "Next →"
                    }
                }
            }

            // === SONGS PANEL (bottom) ===
            div { class: "flex-1 min-h-80 flex flex-col",
                // Songs header with + button and setlist dropdown
                div { class: "p-2 border-b border-zinc-800 bg-zinc-850",
                    div { class: "flex items-center gap-2 px-2",
                        span { class: "text-xs font-semibold text-zinc-500 uppercase tracking-wider",
                            "Songs"
                        }
                        div { class: "flex-1" }
                        if let Some(ref on_create) = props.on_create_song {
                            {
                                let on_create = on_create.clone();
                                rsx! {
                                    button {
                                        class: "w-5 h-5 flex items-center justify-center rounded \
                                                text-zinc-500 hover:text-zinc-200 hover:bg-zinc-700 \
                                                transition-colors text-sm leading-none",
                                        title: "New Song",
                                        onclick: move |_| on_create.call(()),
                                        "+"
                                    }
                                }
                            }
                        }
                    }
                    div { class: "flex items-center gap-2 px-2 mt-1",
                        span { class: "text-xs font-semibold text-zinc-500 uppercase tracking-wider",
                            "Setlist:"
                        }
                        SetlistDropdown { on_select: props.on_setlist_change.clone() }
                    }
                }

                // Song list
                div { class: "flex-1 overflow-y-auto p-2",
                    SongList { on_song_click: props.on_song_click.clone() }
                }

                // Song navigation buttons
                div { class: "p-2 border-t border-zinc-800 flex gap-2",
                    button {
                        class: "flex-1 px-2 py-1.5 bg-zinc-800 hover:bg-zinc-700 rounded text-xs font-medium \
                                text-zinc-300 transition-colors",
                        onclick: move |_| props.on_prev_song.call(()),
                        "← Prev Song"
                    }
                    button {
                        class: "flex-1 px-2 py-1.5 bg-zinc-800 hover:bg-zinc-700 rounded text-xs font-medium \
                                text-zinc-300 transition-colors",
                        onclick: move |_| props.on_next_song.call(()),
                        "Next Song →"
                    }
                }
            }
        }
    }
}

// ─── Standalone panel views (scenes-only, songs-only) ────────────────

/// Scenes-only panel — song header + scene list + navigation.
///
/// Standalone component for the SongParts dock panel.
#[component]
pub fn SceneListPanel(
    on_scene_click: Callback<usize>,
    on_prev_scene: Callback<()>,
    on_next_scene: Callback<()>,
    #[props(default)] on_create_scene: Option<Callback<()>>,
) -> Element {
    rsx! {
        div { class: "h-full w-full flex flex-col bg-card",
            SongHeader {}

            div { class: "flex-1 overflow-y-auto p-2",
                div { class: "flex items-center justify-between mb-2 px-2",
                    h3 { class: "text-xs font-semibold text-muted-foreground uppercase tracking-wider",
                        "Scenes"
                    }
                    if let Some(ref on_create) = on_create_scene {
                        {
                            let on_create = on_create.clone();
                            rsx! {
                                button {
                                    class: "w-5 h-5 flex items-center justify-center rounded \
                                            text-muted-foreground hover:text-foreground hover:bg-accent \
                                            transition-colors text-sm leading-none",
                                    title: "New Scene",
                                    onclick: move |_| on_create.call(()),
                                    "+"
                                }
                            }
                        }
                    }
                }
                SceneList { on_scene_click: on_scene_click.clone() }
            }

            div { class: "p-2 border-t border-border flex gap-2",
                button {
                    class: "flex-1 px-2 py-1.5 bg-muted hover:bg-accent rounded text-xs font-medium \
                            text-muted-foreground hover:text-foreground transition-colors",
                    onclick: move |_| on_prev_scene.call(()),
                    "← Prev"
                }
                button {
                    class: "flex-1 px-2 py-1.5 bg-muted hover:bg-accent rounded text-xs font-medium \
                            text-muted-foreground hover:text-foreground transition-colors",
                    onclick: move |_| on_next_scene.call(()),
                    "Next →"
                }
            }
        }
    }
}

/// Songs-only panel — setlist dropdown + song list + navigation.
///
/// Standalone component for the SongSelector dock panel.
#[component]
pub fn SongListPanel(
    on_song_click: Callback<usize>,
    on_prev_song: Callback<()>,
    on_next_song: Callback<()>,
    on_setlist_change: Callback<Uuid>,
    #[props(default)] on_create_song: Option<Callback<()>>,
) -> Element {
    rsx! {
        div { class: "h-full w-full flex flex-col bg-card",
            // Setlist header
            div { class: "p-2 border-b border-border",
                div { class: "flex items-center gap-2 px-2",
                    span { class: "text-xs font-semibold text-muted-foreground uppercase tracking-wider",
                        "Songs"
                    }
                    div { class: "flex-1" }
                    if let Some(ref on_create) = on_create_song {
                        {
                            let on_create = on_create.clone();
                            rsx! {
                                button {
                                    class: "w-5 h-5 flex items-center justify-center rounded \
                                            text-muted-foreground hover:text-foreground hover:bg-accent \
                                            transition-colors text-sm leading-none",
                                    title: "New Song",
                                    onclick: move |_| on_create.call(()),
                                    "+"
                                }
                            }
                        }
                    }
                }
                div { class: "flex items-center gap-2 px-2 mt-1",
                    span { class: "text-xs font-semibold text-muted-foreground uppercase tracking-wider",
                        "Setlist:"
                    }
                    SetlistDropdown { on_select: on_setlist_change.clone() }
                }
            }

            // Song list
            div { class: "flex-1 overflow-y-auto p-2",
                SongList { on_song_click: on_song_click.clone() }
            }

            // Navigation
            div { class: "p-2 border-t border-border flex gap-2",
                button {
                    class: "flex-1 px-2 py-1.5 bg-muted hover:bg-accent rounded text-xs font-medium \
                            text-muted-foreground hover:text-foreground transition-colors",
                    onclick: move |_| on_prev_song.call(()),
                    "← Prev Song"
                }
                button {
                    class: "flex-1 px-2 py-1.5 bg-muted hover:bg-accent rounded text-xs font-medium \
                            text-muted-foreground hover:text-foreground transition-colors",
                    onclick: move |_| on_next_song.call(()),
                    "Next Song →"
                }
            }
        }
    }
}

// ─── Internal sub-components ─────────────────────────────────────────

/// Song header showing current song context.
#[component]
fn SongHeader() -> Element {
    let song = RIG_CURRENT_SONG.read();

    rsx! {
        if let Some(ref song) = *song {
            div { class: "p-3 border-b border-zinc-800 bg-zinc-850",
                h2 { class: "font-semibold text-base text-zinc-200 truncate", "{song.name}" }
                p { class: "text-xs text-zinc-500",
                    "{song.section_count} scenes"
                }
            }
        } else {
            div { class: "p-3 border-b border-zinc-800",
                h2 { class: "font-semibold text-base text-zinc-500", "No Song" }
            }
        }
    }
}

/// Props for scene list.
#[derive(Props, Clone, PartialEq)]
struct SceneListProps {
    on_scene_click: Callback<usize>,
}

/// List of scenes from current song.
#[component]
fn SceneList(props: SceneListProps) -> Element {
    let song = RIG_CURRENT_SONG.read();
    let scene_index = *RIG_SECTION_INDEX.read();

    rsx! {
        if let Some(ref song) = *song {
            for idx in 0..song.section_count {
                SceneItem {
                    key: "{idx}",
                    index: idx,
                    name: song.section_names.get(idx)
                        .cloned()
                        .unwrap_or_else(|| format!("Scene {}", idx + 1)),
                    is_active: idx == scene_index,
                    on_click: props.on_scene_click.clone(),
                }
            }
        } else {
            div { class: "p-4 text-center text-zinc-500 text-sm",
                "No scenes available"
            }
        }
    }
}

/// Props for scene item.
#[derive(Props, Clone, PartialEq)]
struct SceneItemProps {
    index: usize,
    name: String,
    is_active: bool,
    on_click: Callback<usize>,
}

/// Individual scene item.
#[component]
fn SceneItem(props: SceneItemProps) -> Element {
    let index = props.index;

    rsx! {
        div {
            class: if props.is_active {
                "px-3 py-2 rounded cursor-pointer mb-1 bg-green-500 text-white transition-colors"
            } else {
                "px-3 py-2 rounded cursor-pointer mb-1 hover:bg-zinc-800 text-zinc-300 transition-colors"
            },
            onclick: move |_| props.on_click.call(index),

            div { class: "flex items-center justify-between",
                span { class: "font-medium text-sm truncate", "{props.name}" }
                span { class: "text-xs opacity-60 ml-2", "{index + 1}" }
            }
        }
    }
}

/// Props for song list.
#[derive(Props, Clone, PartialEq)]
struct SongListProps {
    on_song_click: Callback<usize>,
}

/// List of songs from current setlist.
#[component]
fn SongList(props: SongListProps) -> Element {
    let songs = RIG_SETLIST_SONGS.read();
    let song_index = *RIG_SONG_INDEX.read();

    rsx! {
        for song in songs.iter() {
            SongItem {
                key: "{song.index}",
                song: song.clone(),
                is_active: song.index == song_index,
                on_click: props.on_song_click.clone(),
            }
        }
    }
}

/// Props for song item.
#[derive(Props, Clone, PartialEq)]
struct SongItemProps {
    song: SongInfo,
    is_active: bool,
    on_click: Callback<usize>,
}

/// Individual song item.
#[component]
fn SongItem(props: SongItemProps) -> Element {
    let index = props.song.index;

    rsx! {
        div {
            class: if props.is_active {
                "px-3 py-2 rounded cursor-pointer mb-1 bg-zinc-800 border-l-2 border-green-500 text-zinc-200 transition-colors"
            } else {
                "px-3 py-2 rounded cursor-pointer mb-1 hover:bg-zinc-800/50 border-l-2 border-transparent text-zinc-400 transition-colors"
            },
            onclick: move |_| props.on_click.call(index),

            div { class: "flex items-center justify-between",
                div { class: "flex-1 min-w-0",
                    span { class: "font-medium text-sm truncate block", "{props.song.name}" }
                    span { class: "text-xs text-zinc-500",
                        "{props.song.section_count} scenes"
                    }
                }
                span { class: "text-xs opacity-60 ml-2", "{index + 1}" }
            }
        }
    }
}

/// Props for setlist dropdown.
#[derive(Props, Clone, PartialEq)]
struct SetlistDropdownProps {
    on_select: Callback<Uuid>,
}

/// Dropdown for selecting a setlist.
///
/// TODO: Currently display-only since rig-control doesn't have setlist IDs yet.
/// Setlists are just collections of songs, and there's currently only one ("Main Setlist").
#[component]
fn SetlistDropdown(props: SetlistDropdownProps) -> Element {
    let current_setlist = RIG_CURRENT_SETLIST.read();
    let _available_setlists = RIG_AVAILABLE_SETLISTS.read();
    let _is_open = use_signal(|| false);

    let current_name = current_setlist
        .as_ref()
        .map(|s| s.name.clone())
        .unwrap_or_else(|| "No Setlist".to_string());

    let song_count = current_setlist.as_ref().map(|s| s.song_count).unwrap_or(0);

    rsx! {
        div { class: "relative flex-1",
            // Display current setlist (non-interactive for now)
            div {
                class: "w-full flex items-center justify-between px-2 py-1 text-xs font-medium \
                        bg-zinc-800 border border-zinc-700 rounded text-zinc-300",
                span { class: "truncate", "{current_name}" }
                span { class: "ml-2 text-zinc-500 text-[10px]",
                    {format!("{} songs", song_count)}
                }
            }

            // TODO: Add multi-setlist support when rig-control has setlist IDs
            // For now, there's only one setlist so selection isn't needed
        }
    }
}
