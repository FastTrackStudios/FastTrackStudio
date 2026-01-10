//! Tempo manipulation utilities
//!
//! This module provides utilities for manipulating REAPER's tempo map,
//! including finding grid lines and moving tempo markers.
//!
//! Based on SWS BR_Tempo.cpp and BR_Util.cpp implementations.

pub mod envelope;
pub mod grid;
pub mod move_grid;

pub use envelope::TempoEnvelope;
pub use grid::{get_closest_grid_line, get_closest_measure_grid_line};
pub use move_grid::{
    move_grid_do_undo, move_grid_init, move_grid_to_mouse, set_move_grid_variant, MoveGridVariant,
    register_move_grid_actions,
};
