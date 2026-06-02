// architect's JsonField derive emits cfg-gated blocks; allow
// at crate scope.
#![allow(unexpected_cfgs)]

//! Wasm-clean nutrition shape for the mealplan + fitness slices.
//!
//! [`Nutrition`] is the canonical per-unit / per-serving macro
//! shape. It lives on a `pantry::PantryItem`, sums across a
//! `cookbook::Recipe`, and aggregates an `intake::IntakeLog`.
//! Kept in this wasm-clean proto so downstream wire crates
//! (notably `fitness-proto`, which the web UI binds directly)
//! can reference `Nutrition` without dragging in the
//! vault-backed `cookbook` crate.
//!
//! The sibling `cookbook` crate re-exports [`Nutrition`] so the
//! existing `cookbook::Nutrition` paths keep working.

pub mod model;

pub use model::Nutrition;
