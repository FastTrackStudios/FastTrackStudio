//! `Substitution` entity — swappable foods (butter↔ghee, eggs→flax-egg).
//!
//! See [`model::Model`] for the SeaORM entity. Lookups are organization-scoped
//! and use the `from_food_id` / `to_food_id` indexes; the
//! `bidirectional` flag indicates the inverse swap is implicitly valid
//! and is synthesized at query time by the cooking service.

pub mod model;

pub use model::*;
