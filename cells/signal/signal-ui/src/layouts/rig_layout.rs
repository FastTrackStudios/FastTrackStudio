//! Top-level layout for the Rig tab.
//!
//! Wraps the GuitarRigGrid with service provider, subscription hook,
//! and derives module data from the rig structure.

use crate::components::rig_grid::guitar_rig_grid::GuitarRigGrid;
use crate::context::rig::{RigService, RigServiceProvider};
use crate::hooks::rig_actions::use_rig_actions;
use crate::hooks::rig_state::use_rig_subscription;
use crate::prelude::*;
use crate::signals::{RIG_CURRENT_PRESET, RIG_CURRENT_SONG, RIG_INFO, RIG_SCENE_INDEX};

/// Main layout for the Rig tab.
///
/// Creates a MockRigControlService with guitar defaults, provides it
/// via context, and renders the rig grid.
#[component]
pub fn RigLayout() -> Element {
    // Create mock service once on mount
    let service = use_hook(|| RigService::mock_guitar());

    rsx! {
        RigServiceProvider { service,
            RigContent {}
        }
    }
}

/// Inner content that has access to the RigServiceProvider context.
#[component]
fn RigContent() -> Element {
    // Subscribe to rig service events — populates global signals
    use_rig_subscription();
    let _actions = use_rig_actions();

    // Read current state from signals
    let rig_info = RIG_INFO.read();
    let current_preset = RIG_CURRENT_PRESET.read();
    let current_song = RIG_CURRENT_SONG.read();
    let scene_index = *RIG_SCENE_INDEX.read();

    // Build header info
    let rig_name = rig_info
        .as_ref()
        .map(|r| r.name.clone())
        .unwrap_or_default();
    let preset_name = current_preset
        .as_ref()
        .map(|p| p.name.clone())
        .unwrap_or_else(|| "No preset".to_string());
    let song_name = current_song.as_ref().map(|s| s.name.clone());

    rsx! {
        div { class: "h-full w-full flex flex-col bg-background overflow-hidden",
            // Rig header bar
            div { class: "flex-shrink-0 flex items-center justify-between px-4 py-3 border-b border-border bg-card",
                // Left: rig name + preset
                div { class: "flex items-center gap-4",
                    if !rig_name.is_empty() {
                        span { class: "text-sm font-medium text-muted-foreground", "{rig_name}" }
                    }
                    span { class: "text-sm font-semibold text-foreground", "{preset_name}" }
                    if let Some(ref song) = song_name {
                        span { class: "text-sm text-muted-foreground", "Song: {song}" }
                    }
                    span { class: "text-xs text-muted-foreground font-mono", "Scene {scene_index}" }
                }

                // Right: status
                div { class: "flex items-center gap-2",
                    span { class: "text-xs text-muted-foreground", "Mock Service" }
                }
            }

            // Main content: the guitar rig grid
            div { class: "flex-1 overflow-hidden",
                GuitarRigGrid {
                    on_toggle_module_bypass: move |_id| {},
                    on_toggle_block_bypass: move |_id| {},
                    on_macro_change: move |_args| {},
                    on_param_change: move |_args| {},
                }
            }
        }
    }
}
