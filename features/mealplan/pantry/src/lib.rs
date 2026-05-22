//! `pantry` — typed view of food-on-hand pages in a
//! `vault::Vault`.
//!
//! A pantry page IS an inventory page (`type: item` +
//! `tags: [..., pantry]`). One physical thing maps to one
//! markdown file visible in both the gear inventory list
//! and the food list. The crate adds the food-specific
//! surface — qty / unit / expiry / nutrition / restock
//! threshold — on top of inventory's identity + locator
//! fields, and exposes typed convenience mutators
//! ([`PantryService::consume`], [`PantryService::restock`],
//! [`PantryService::open`]) used by mealplan when meals are
//! cooked or groceries arrive.
//!
//! Nutrition is reused from the cookbook crate so recipes,
//! pantry stock, and (future) fitness calorie logs all
//! speak the same currency.

#![cfg(not(target_arch = "wasm32"))]

pub mod model;
pub mod parse;
pub mod scan;
pub mod service;
pub mod store;
pub mod write;

pub use model::{FoodCategory, PantryItem};
pub use parse::{ParseError, looks_like_pantry_item, parse_page};
pub use scan::{expired, low_stock, scan_vault};
pub use service::{PantryError, PantryService};
pub use store::Store;
pub use write::{WriteError, default_pantry_path, serialize_pantry_item, write_pantry_item};
