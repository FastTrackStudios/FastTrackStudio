//! FTS action definitions.
//!
//! All actions are registered under the `FTS_` namespace.  The display name
//! shown in REAPER's action list is prefixed with "FTS: " in the registration
//! call inside `lib.rs`.

use std::sync::Arc;

use reaper_high::Reaper;
use crate::continuous_action::start_continuous_action;
#[allow(unused_imports)]
use crate::item_actions;
use crate::tempo::{
    MoveGridVariant, register_move_grid_actions, set_move_grid_variant,
    snap_grid_to_transient_constrained_handler, snap_grid_to_transient_fully_constrained_handler,
    snap_grid_to_transient_handler,
};

pub type ActionDefs = Vec<(String, String, Arc<dyn Fn() + Send + Sync>, bool)>;

fn show(msg: impl Into<String>) {
    Reaper::get().show_console_msg(msg.into());
}

/// Build the list of all FTS utility actions.
///
/// Call this **once** at startup (after REAPER high-level API is available).
pub fn build_action_defs() -> ActionDefs {
    // Register continuous actions with the continuous-action timer system.
    register_move_grid_actions();

    let defs = vec![
        // ── Launcher ─────────────────────────────────────────────────────
        // ── Tempo grid — move ────────────────────────────────────────────────
        action(
            "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE",
            "Move closest measure grid line to mouse cursor (perform until shortcut released)",
            || {
                set_move_grid_variant(MoveGridVariant::ClosestMeasure);
                if !start_continuous_action("FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE") {
                    show("Failed to start Move Measure Grid action\n");
                }
            },
        ),
        action(
            "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_CONSTRAINED",
            "Move closest measure grid line to mouse cursor — constrained (perform until shortcut released)",
            || {
                set_move_grid_variant(MoveGridVariant::ClosestMeasureConstrained);
                if !start_continuous_action("FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_CONSTRAINED") {
                    show("Failed to start Move Measure Grid (Constrained) action\n");
                }
            },
        ),
        action(
            "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_FULLY_CONSTRAINED",
            "Move closest measure grid line to mouse cursor — fully constrained (perform until shortcut released)",
            || {
                set_move_grid_variant(MoveGridVariant::ClosestMeasureFullyConstrained);
                if !start_continuous_action(
                    "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_FULLY_CONSTRAINED",
                ) {
                    show("Failed to start Move Measure Grid (Fully Constrained) action\n");
                }
            },
        ),
        action(
            "FTS_TEMPO_MOVE_GRID_TO_MOUSE",
            "Move closest grid line to mouse cursor (perform until shortcut released)",
            || {
                set_move_grid_variant(MoveGridVariant::ClosestGrid);
                if !start_continuous_action("FTS_TEMPO_MOVE_GRID_TO_MOUSE") {
                    show("Failed to start Move Grid action\n");
                }
            },
        ),
        action(
            "FTS_TEMPO_MOVE_MARKER_TO_MOUSE",
            "Move closest tempo marker to mouse cursor (perform until shortcut released)",
            || {
                set_move_grid_variant(MoveGridVariant::ClosestTempo);
                if !start_continuous_action("FTS_TEMPO_MOVE_MARKER_TO_MOUSE") {
                    show("Failed to start Move Tempo action\n");
                }
            },
        ),
        // ── Tempo grid — snap ────────────────────────────────────────────────
        action(
            "FTS_TEMPO_SNAP_GRID_TO_TRANSIENT",
            "Snap closest measure grid line to next transient",
            snap_grid_to_transient_handler,
        ),
        action(
            "FTS_TEMPO_SNAP_GRID_TO_TRANSIENT_CONSTRAINED",
            "Snap closest measure grid line to next transient — constrained",
            snap_grid_to_transient_constrained_handler,
        ),
        action(
            "FTS_TEMPO_SNAP_GRID_TO_TRANSIENT_FULLY_CONSTRAINED",
            "Snap closest measure grid line to next transient — fully constrained",
            snap_grid_to_transient_fully_constrained_handler,
        ),
        // ── Item editing ─────────────────────────────────────────────────────
        action(
            "FTS_SPLIT_ITEMS_CROSSFADE_LEFT",
            "Split selected items at cursor with crossfade on left",
            || item_actions::split_items_with_crossfade_left(),
        ),
        // ── Info ─────────────────────────────────────────────────────────────
        menu_action(
            "FTS_INFO",
            "FastTrackStudio Info",
            || {
                let version = env!("CARGO_PKG_VERSION");
                Reaper::get().show_console_msg(format!(
                    "FastTrackStudio Extensions v{version}\n\
                     https://github.com/FastTrackStudios\n"
                ));
            },
        ),
    ];

    // Module actions (launcher, dynamic-template, session, sync, input, keyflow)
    // are collected via daw::module::collect_actions() in lib.rs — not here.

    defs
}

/// Convenience constructor for a single action entry (not shown in menu).
fn action(
    id: &str,
    display_name: &str,
    handler: impl Fn() + Send + Sync + 'static,
) -> (String, String, Arc<dyn Fn() + Send + Sync>, bool) {
    (id.to_string(), format!("FTS: {display_name}"), Arc::new(handler), false)
}

/// Convenience constructor for a single action entry shown in the Extensions menu.
fn menu_action(
    id: &str,
    display_name: &str,
    handler: impl Fn() + Send + Sync + 'static,
) -> (String, String, Arc<dyn Fn() + Send + Sync>, bool) {
    (id.to_string(), format!("FTS: {display_name}"), Arc::new(handler), true)
}
