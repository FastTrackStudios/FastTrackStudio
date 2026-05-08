//! `CookingSession` — stateful per-cook walkthrough of a recipe.
//!
//! See [`model::Model`] for the SeaORM entity and [`state`] for the
//! pure-Rust helpers used by the service layer.

pub mod model;
pub mod state;

pub use model::{
    ActiveModel, Column, CookingSession, CookingSessionApi, CookingSessionStatus, Entity, Model,
};
pub use state::{
    StepState, mise_en_place_from_json, mise_en_place_to_json, step_states_from_json,
    step_states_to_json,
};
