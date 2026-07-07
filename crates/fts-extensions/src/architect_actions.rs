//! fts-extensions' own actions declared via `#[architect::actions]`.
//!
//! Additive alongside `actions::build_action_defs()` — this migrates the
//! *statically enumerable, unconditionally-compiled* subset of that list
//! onto the new architect action primitive (real description/category/group
//! metadata instead of the old bare tuple). `build_action_defs()` itself is
//! untouched and still owns REAPER registration for everything until a
//! later cutover phase.
//!
//! **Deliberately NOT migrated here** (stay on the legacy path):
//! - `#[cfg(feature = "mod-session")]` mode-debug actions (7) — mixing
//!   per-method `#[cfg(...)]` with `#[architect::actions]` is untested:
//!   the macro reads `trait_item.items` before rustc's cfg-attribute
//!   stripping runs, so a method compiled out by `cfg` would still get a
//!   generated `ActionMeta` const + register call referencing it,
//!   breaking the `mod-session`-off build. Needs either a macro-level
//!   `#[cfg]` passthrough or per-feature-set trait variants before this
//!   is safe.
//! - `#[cfg(feature = "mod-mirror")]` mirror actions (4) — same reason.
//! - The per-time-signature actions generated in a `for &(num, denom) in
//!   TIME_SIGNATURES` loop (2 actions × however many signatures) —
//!   `#[architect::actions]` requires statically-declared `fn name(&self)`
//!   methods; it has no data-driven/family concept for a runtime-iterated
//!   action set. This is a real gap, not an oversight — flagged for a
//!   future architect feature if this pattern needs to become a first-class
//!   primitive.

use crate::tempo::{
    MoveGridVariant, set_move_grid_variant, snap_grid_to_transient_constrained_handler,
    snap_grid_to_transient_fully_constrained_handler, snap_grid_to_transient_handler,
};

/// Tempo-grid move/snap actions — none shown in REAPER's Extensions menu
/// (matches the original `action()`, not `menu_action()`, in
/// `actions::build_action_defs()`).
#[architect::actions(namespace = "FTS_TEMPO")]
pub trait FtsTempoGridActions {
    #[action(
        description = "Move closest measure grid line to mouse cursor (perform until shortcut released)",
        group = "Move"
    )]
    fn move_measure_grid_to_mouse(&self);

    #[action(
        description = "Move closest measure grid line to mouse cursor — constrained (perform until shortcut released)",
        group = "Move"
    )]
    fn move_measure_grid_to_mouse_constrained(&self);

    #[action(
        description = "Move closest measure grid line to mouse cursor — fully constrained (perform until shortcut released)",
        group = "Move"
    )]
    fn move_measure_grid_to_mouse_fully_constrained(&self);

    #[action(
        description = "Move closest grid line to mouse cursor (perform until shortcut released)",
        group = "Move"
    )]
    fn move_grid_to_mouse(&self);

    #[action(
        description = "Move closest tempo marker to mouse cursor (perform until shortcut released)",
        group = "Move"
    )]
    fn move_marker_to_mouse(&self);

    #[action(
        description = "Snap closest measure grid line to next transient",
        group = "Snap"
    )]
    fn snap_grid_to_transient(&self);

    #[action(
        description = "Snap closest measure grid line to next transient — constrained",
        group = "Snap"
    )]
    fn snap_grid_to_transient_constrained(&self);

    #[action(
        description = "Snap closest measure grid line to next transient — fully constrained",
        group = "Snap"
    )]
    fn snap_grid_to_transient_fully_constrained(&self);
}

/// General FTS utility actions — a mix of hidden and Extensions-menu-visible
/// entries; `category` is set only where the original constructor was
/// `menu_action`/`toggle_menu_action` (empty `category` == not shown in
/// menu, per `daw_reaper`'s `ActionBackend` impl).
#[architect::actions(namespace = "FTS")]
pub trait FtsActions {
    #[action(description = "Move cursor left creating time selection by measure")]
    fn move_cursor_left_creating_time_selection_by_measure(&self);

    #[action(description = "Move cursor right creating time selection by measure")]
    fn move_cursor_right_creating_time_selection_by_measure(&self);

    #[action(description = "Split selected items at cursor with crossfade on left")]
    fn split_items_crossfade_left(&self);

    #[action(description = "Test Toggle", category = "Test", toggleable = true)]
    fn test_toggle(&self);

    #[action(
        description = "Sync: Toggle clock-sync (multicast peer discovery)",
        category = "Sync"
    )]
    fn clock_sync_toggle(&self);

    #[action(
        description = "Sync: Toggle drift correction (auto-rate-change)",
        category = "Sync"
    )]
    fn drift_correction_toggle(&self);

    #[action(
        description = "MIDI mode: Drums (drum-map view + drum keybinds)",
        category = "MIDI Editor"
    )]
    fn midi_mode_drums(&self);

    #[action(description = "MIDI mode: Cycle to next", category = "MIDI Editor")]
    fn midi_mode_cycle(&self);

    #[action(description = "MIDI: Insert flam at mouse cursor")]
    fn midi_insert_flam(&self);

    #[action(description = "FastTrackStudio Info", category = "Info")]
    fn info(&self);

    #[action(description = "Volume Balancer: toggle constant-sum fader linking")]
    fn volbal_toggle(&self);

    #[action(description = "Volume Balancer: link selected tracks (constant total volume)")]
    fn volbal_link_selected(&self);

    #[action(description = "Volume Balancer: unlink groups containing selected tracks")]
    fn volbal_unlink_selected(&self);
}

/// Zero-sized backend for both traits above — every method forwards to the
/// exact same function `actions::build_action_defs()`'s closures call.
#[derive(Clone, Copy, Default)]
pub struct FtsActionsImpl;

impl FtsTempoGridActions for FtsActionsImpl {
    fn move_measure_grid_to_mouse(&self) {
        set_move_grid_variant(MoveGridVariant::ClosestMeasure);
        if !crate::continuous_action::start_continuous_action(
            "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE",
        ) {
            crate::actions::show("Failed to start Move Measure Grid action\n");
        }
    }

    fn move_measure_grid_to_mouse_constrained(&self) {
        set_move_grid_variant(MoveGridVariant::ClosestMeasureConstrained);
        if !crate::continuous_action::start_continuous_action(
            "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_CONSTRAINED",
        ) {
            crate::actions::show("Failed to start Move Measure Grid (Constrained) action\n");
        }
    }

    fn move_measure_grid_to_mouse_fully_constrained(&self) {
        set_move_grid_variant(MoveGridVariant::ClosestMeasureFullyConstrained);
        if !crate::continuous_action::start_continuous_action(
            "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_FULLY_CONSTRAINED",
        ) {
            crate::actions::show("Failed to start Move Measure Grid (Fully Constrained) action\n");
        }
    }

    fn move_grid_to_mouse(&self) {
        set_move_grid_variant(MoveGridVariant::ClosestGrid);
        if !crate::continuous_action::start_continuous_action("FTS_TEMPO_MOVE_GRID_TO_MOUSE") {
            crate::actions::show("Failed to start Move Grid action\n");
        }
    }

    fn move_marker_to_mouse(&self) {
        set_move_grid_variant(MoveGridVariant::ClosestTempo);
        if !crate::continuous_action::start_continuous_action("FTS_TEMPO_MOVE_MARKER_TO_MOUSE") {
            crate::actions::show("Failed to start Move Tempo action\n");
        }
    }

    fn snap_grid_to_transient(&self) {
        snap_grid_to_transient_handler();
    }

    fn snap_grid_to_transient_constrained(&self) {
        snap_grid_to_transient_constrained_handler();
    }

    fn snap_grid_to_transient_fully_constrained(&self) {
        snap_grid_to_transient_fully_constrained_handler();
    }
}

impl FtsActions for FtsActionsImpl {
    fn move_cursor_left_creating_time_selection_by_measure(&self) {
        crate::actions::move_cursor_creating_time_selection_by_measure(40838);
    }

    fn move_cursor_right_creating_time_selection_by_measure(&self) {
        crate::actions::move_cursor_creating_time_selection_by_measure(40837);
    }

    fn split_items_crossfade_left(&self) {
        crate::item_actions::split_items_with_crossfade_left();
    }

    fn test_toggle(&self) {
        crate::actions::toggle_test_toggle_handler();
    }

    fn clock_sync_toggle(&self) {
        crate::sync_settings::toggle_clock_sync();
    }

    fn drift_correction_toggle(&self) {
        crate::sync_settings::toggle_drift_correction();
    }

    fn midi_mode_drums(&self) {
        crate::midi_mode::set_midi_mode(crate::midi_mode::MidiMode::Drums);
    }

    fn midi_mode_cycle(&self) {
        crate::midi_mode::cycle_midi_mode();
    }

    fn midi_insert_flam(&self) {
        crate::midi_flam::insert_flam_at_mouse();
    }

    fn info(&self) {
        let version = env!("CARGO_PKG_VERSION");
        reaper_high::Reaper::get().show_console_msg(format!(
            "FastTrackStudio Extensions v{version}\n\
                 https://github.com/FastTrackStudios\n"
        ));
    }

    fn volbal_toggle(&self) {
        let on = !crate::volume_balancer::is_enabled();
        crate::volume_balancer::set_enabled(on);
        crate::actions::sync_toggle_state("FTS_VOLBAL_TOGGLE", on);
        crate::actions::show(format!(
            "FTS Volume Balancer: {}\n",
            if on { "enabled" } else { "disabled" }
        ));
    }

    fn volbal_link_selected(&self) {
        crate::volume_balancer::link_selected_tracks();
    }

    fn volbal_unlink_selected(&self) {
        crate::volume_balancer::unlink_selected_tracks();
    }
}

/// Register every action declared in this module with `backend` (the
/// REAPER `ActionBackend` impl from `daw_reaper::Reaper`, or any other
/// `ActionBackend` — this function is backend-agnostic).
pub fn register_actions<B: architect::action::ActionBackend + ?Sized>(backend: &B) {
    let imp = std::sync::Arc::new(FtsActionsImpl);
    register_fts_tempo_grid_actions_actions(backend, imp.clone());
    register_fts_actions_actions(backend, imp);
}
