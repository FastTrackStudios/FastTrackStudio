//! `MealPlanEntry` — one meal slot per day per (organization).
//!
//! Slot uniqueness: `(organization, date, meal_type)` is treated as
//! UNIQUE. `set_meal_plan_entry` upserts on this triple — repeated calls
//! for the same slot replace the row. Use `delete_meal_plan_entry` to
//! clear a slot. The `Other` meal type is the escape hatch when more
//! than one entry per (date, meal_type) is desired.

pub mod model;

pub use model::Model as MealPlanEntry;
pub use model::*;
