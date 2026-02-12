//! Scene Grid Panel — Quad Cortex-style 4x2 scene tile grid.
//!
//! Shows different content depending on `RigViewMode`:
//! - **Song**: Scenes from the current song
//! - **Profile**: Scene templates from the current profile
//! - **Preset**: Snapshots from the current preset

use super::view_mode::RigViewMode;
use crate::prelude::*;
use crate::signals::*;

const SLOT_COUNT: usize = 8;

/// Background color per grid position.
const TILE_COLORS: [&str; 8] = [
    "#92400e", // Amber/warm brown
    "#065f46", // Emerald/teal
    "#0e7490", // Cyan/teal
    "#5b21b6", // Violet/purple
    "#9a3412", // Orange/rust
    "#1e40af", // Blue
    "#7c3aed", // Purple
    "#374151", // Gray (neutral)
];

/// Internal tile model — unified across all three modes.
#[derive(Clone, PartialEq)]
struct SceneTile {
    index: usize,
    name: String,
    active: bool,
    empty: bool,
}

/// Action dispatched when a tile is clicked.
#[derive(Clone)]
enum TileAction {
    /// Go to scene by index (song mode).
    GoToScene(usize),
    /// Load profile scene by (profile_id, scene_index).
    LoadProfileScene(uuid::Uuid, usize),
    /// Activate preset snapshot by ID.
    ActivateSnapshot(uuid::Uuid),
    /// No action (empty tile).
    None,
}

/// Quad Cortex-style 4x2 scene grid.
///
/// Reads existing `RIG_*` signals to populate tiles and dispatches
/// actions through `use_rig_actions()`.
#[component]
pub fn SceneGridPanel(view_mode: RigViewMode) -> Element {
    let actions = crate::hooks::use_rig_actions();

    // Build tiles + action map based on current mode
    let (tiles, tile_actions) = match view_mode {
        RigViewMode::Song => build_song_tiles(),
        RigViewMode::Profile => build_profile_tiles(),
        RigViewMode::Preset => build_preset_tiles(),
    };

    rsx! {
        div { class: "h-full w-full bg-card p-2",
            div { class: "grid grid-cols-4 grid-rows-2 gap-2 h-full",
                for tile in tiles.iter() {
                    {
                        let idx = tile.index;
                        let action = tile_actions[idx].clone();
                        let actions = actions.clone();
                        rsx! {
                            SceneGridTile {
                                key: "{idx}",
                                tile: tile.clone(),
                                on_click: move |_| {
                                    match &action {
                                        TileAction::GoToScene(i) => actions.go_to_scene.call(*i),
                                        TileAction::LoadProfileScene(pid, i) => {
                                            actions.load_profile_scene.call((*pid, *i));
                                        }
                                        TileAction::ActivateSnapshot(sid) => {
                                            actions.activate_snapshot.call(*sid);
                                        }
                                        TileAction::None => {}
                                    }
                                },
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Build tiles from song scenes.
fn build_song_tiles() -> (Vec<SceneTile>, Vec<TileAction>) {
    let current_song = RIG_CURRENT_SONG.read();
    let active_index = *RIG_SECTION_INDEX.read();

    let scene_names: Vec<String> = current_song
        .as_ref()
        .map(|s| s.section_names.clone())
        .unwrap_or_default();

    let mut tiles = Vec::with_capacity(SLOT_COUNT);
    let mut tile_actions = Vec::with_capacity(SLOT_COUNT);

    for i in 0..SLOT_COUNT {
        if i < scene_names.len() {
            tiles.push(SceneTile {
                index: i,
                name: scene_names[i].clone(),
                active: i == active_index && current_song.is_some(),
                empty: false,
            });
            tile_actions.push(TileAction::GoToScene(i));
        } else {
            tiles.push(SceneTile {
                index: i,
                name: String::new(),
                active: false,
                empty: true,
            });
            tile_actions.push(TileAction::None);
        }
    }

    (tiles, tile_actions)
}

/// Build tiles from profile patches.
fn build_profile_tiles() -> (Vec<SceneTile>, Vec<TileAction>) {
    let profile = RIG_PROFILE.read();

    let (profile_id, patches) = match profile.as_ref() {
        Some(p) => (p.id, &p.patches),
        None => {
            return empty_tiles();
        }
    };

    let mut tiles = Vec::with_capacity(SLOT_COUNT);
    let mut tile_actions = Vec::with_capacity(SLOT_COUNT);

    for i in 0..SLOT_COUNT {
        if i < patches.len() {
            let patch = &patches[i];
            tiles.push(SceneTile {
                index: i,
                name: patch.name.clone(),
                active: false,
                empty: false,
            });
            tile_actions.push(TileAction::LoadProfileScene(profile_id, i));
        } else {
            tiles.push(SceneTile {
                index: i,
                name: String::new(),
                active: false,
                empty: true,
            });
            tile_actions.push(TileAction::None);
        }
    }

    (tiles, tile_actions)
}

/// Build tiles from preset snapshots.
///
/// Currently returns empty tiles since `RigPresetInfo` no longer carries inline
/// snapshot data. Snapshot-based tiles will be restored when the preset detail
/// panel loads snapshots from the DB.
fn build_preset_tiles() -> (Vec<SceneTile>, Vec<TileAction>) {
    // RigPresetInfo no longer has snapshot data — the grid shows empty until
    // we wire up DB-backed snapshot loading.
    empty_tiles()
}

/// Produce 8 empty tiles (no data available).
fn empty_tiles() -> (Vec<SceneTile>, Vec<TileAction>) {
    let tiles = (0..SLOT_COUNT)
        .map(|i| SceneTile {
            index: i,
            name: String::new(),
            active: false,
            empty: true,
        })
        .collect();
    let actions = vec![TileAction::None; SLOT_COUNT];
    (tiles, actions)
}

/// Single scene tile in the grid.
#[component]
fn SceneGridTile(tile: SceneTile, on_click: EventHandler<()>) -> Element {
    let bg_color = TILE_COLORS[tile.index % TILE_COLORS.len()];

    let bg_style = if tile.empty {
        "background-color: #374151;".to_string()
    } else {
        format!("background-color: {};", bg_color)
    };

    let active_class = if tile.active {
        "ring-2 ring-green-400 shadow-lg shadow-green-500/30"
    } else {
        ""
    };

    let empty_class = if tile.empty { "opacity-50" } else { "" };

    let cursor_class = if tile.empty {
        "cursor-default"
    } else {
        "cursor-pointer hover:brightness-110"
    };

    rsx! {
        div {
            class: "relative rounded-lg overflow-hidden transition-all duration-150 {active_class} {empty_class} {cursor_class}",
            style: "{bg_style}",
            onclick: move |_| {
                if !tile.empty {
                    on_click.call(());
                }
            },

            // Scene name
            if !tile.empty {
                div {
                    class: "absolute inset-0 flex items-center justify-center text-center px-2",
                    span {
                        class: "text-sm font-bold text-white uppercase tracking-wide leading-tight",
                        style: "text-shadow: 0 1px 3px rgba(0,0,0,0.5);",
                        "{tile.name}"
                    }
                }
            }
        }
    }
}
