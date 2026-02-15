//! Rig scene grid -- domain-aware scene grid wrapping SceneTileGrid.
//!
//! Fetches rig data from the controller and maps [`RigScene`] variants
//! to [`TileData`] for the dumb [`SceneTileGrid`] component.

use dioxus::prelude::*;
use signal::rig::Rig;
use signal::SignalController;

use crate::components::{SceneTileGrid, TileData};

// region: --- RigSceneGrid

/// A domain-aware scene grid for a Rig.
///
/// Fetches the rig from the controller, maps its variants to tile data,
/// and renders them using the dumb `SceneTileGrid` component.
#[component]
pub fn RigSceneGrid(
    /// Controller for fetching rig data.
    controller: SignalController,
    /// Rig collection ID to display scenes for.
    rig_id: String,
    /// Currently active scene ID, if any.
    #[props(default)]
    active_scene_id: Option<String>,
    /// Callback when a scene tile is selected.
    on_scene_select: EventHandler<String>,
) -> Element {
    let mut rig = use_signal(|| None::<Rig>);

    // Fetch rig when rig_id changes.
    {
        let controller = controller.clone();
        let rig_id = rig_id.clone();
        use_effect(move || {
            let controller = controller.clone();
            let rig_id = rig_id.clone();
            spawn(async move {
                rig.set(controller.load_rig_collection(rig_id.as_str()).await);
            });
        });
    }

    let current_rig = rig();

    match current_rig {
        None => rsx! {
            div { class: "flex items-center justify-center h-full text-sm text-zinc-500",
                "Loading rig..."
            }
        },
        Some(r) => {
            let scene_ids: Vec<String> = r.variants.iter().map(|v| v.id.to_string()).collect();

            let tiles: Vec<TileData> = r
                .variants
                .iter()
                .map(|v| TileData {
                    name: v.name.clone(),
                    active: active_scene_id
                        .as_ref()
                        .map_or(false, |aid| aid == &v.id.to_string()),
                    empty: false,
                })
                .collect();

            let slot_count = tiles.len().max(8);

            rsx! {
                SceneTileGrid {
                    tiles,
                    slot_count,
                    on_tile_click: move |idx: usize| {
                        if let Some(scene_id) = scene_ids.get(idx) {
                            on_scene_select.call(scene_id.clone());
                        }
                    },
                }
            }
        }
    }
}

// endregion: --- RigSceneGrid
