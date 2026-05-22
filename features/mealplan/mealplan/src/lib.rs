//! `mealplan` — feature facade tying cookbook + pantry +
//! the calendar together.
//!
//! Meals live as markdown pages (`type: meal`) under
//! `<vault>/mealplan/`. Each meal references one or more
//! [`cookbook::Recipe`]s by id and, once cooked, records the
//! exact [`pantry::PantryItem`] consumes via
//! [`PantryDeduction`].
//!
//! Consumer crates (apps, future `fitness` feature) should
//! depend on this crate alone — `cookbook` and `pantry` are
//! re-exported so a single import surface stays small.
//!
//! Surface:
//! - [`Meal`] / [`Slot`] / [`Status`] / [`PantryDeduction`] —
//!   the calendar model.
//! - [`parse_page`] / [`looks_like_meal`] /
//!   [`serialize_meal`] / [`write_meal`] — round-trip helpers.
//! - [`scan_vault`] / [`meals_on`] / [`meals_between`] — view
//!   helpers.
//! - [`MealplanService`] — `#[architect::rpc]` trait.
//! - [`Store`] — file-backed impl. Wraps the same vault
//!   mutex used by an embedded [`pantry::Store`] so cook-time
//!   deductions race-free against pantry edits.
//!
//! Re-exports:
//! - [`cookbook`] — `Recipe`, `Ingredient`, `Nutrition`,
//!   `CookbookService`, `Store` (as `cookbook::Store`).
//! - [`pantry`] — `PantryItem`, `PantryService`, `Store`
//!   (as `pantry::Store`).

#![cfg(not(target_arch = "wasm32"))]

pub mod fulfillment;
pub mod model;
pub mod parse;
pub mod scan;
pub mod service;
pub mod store;
pub mod write;

pub use fulfillment::{Fulfillment, Shortage, ShortageReason};
pub use model::{Meal, PantryDeduction, Slot, Status};
pub use parse::{ParseError, looks_like_meal, parse_page};
pub use scan::{meals_between, meals_on, scan_vault};
pub use service::{MealplanError, MealplanService};
pub use store::Store;
pub use write::{WriteError, default_meal_path, serialize_meal, write_meal};

// Re-exports — consumers depend on this crate alone.
pub use cookbook;
pub use pantry;
