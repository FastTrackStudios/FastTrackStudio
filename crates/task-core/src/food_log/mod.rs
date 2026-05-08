//! `FoodLog` — daily food-consumption diary entry.
//!
//! Each row is an immutable snapshot. At log time the service copies
//! the food/product's nutrition into the row so historical totals don't
//! drift if the underlying [`crate::food::Food`]'s
//! `nutrition_per_100g` is later corrected.
//!
//! Soft FKs only: `food_id`, `product_id`, `meal_plan_entry_id`,
//! `recipe_id`. The service layer enforces shape.

pub mod model;

pub use model::{
    ActiveModel as FoodLogActiveModel, Column as FoodLogColumn, Entity as FoodLogEntityRef,
    FoodLogApi, Model as FoodLog,
};

pub use model::*;
