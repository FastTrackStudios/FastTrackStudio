//! `MealplanService` — wire surface that ties recipes,
//! pantry stock, and the calendar together.

use facet::Facet;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::model::{Meal, PantryDeduction};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet, Error)]
#[repr(u8)]
pub enum MealplanError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("already exists: {0}")]
    AlreadyExists(String),
    #[error("bad request: {0}")]
    BadRequest(String),
    #[error("pantry: {0}")]
    Pantry(String),
    #[error("io: {0}")]
    Io(String),
}

#[architect::rpc]
pub trait MealplanService {
    fn list(&self) -> Result<Vec<Meal>, MealplanError>;

    fn get(&self, id: &str) -> Result<Meal, MealplanError>;

    fn create(&self, meal: Meal) -> Result<Meal, MealplanError>;

    fn update(&self, meal: Meal) -> Result<Meal, MealplanError>;

    fn rename(&self, id: &str, new_path: &str) -> Result<Meal, MealplanError>;

    fn delete(&self, id: &str) -> Result<(), MealplanError>;

    /// Mark the meal as `cooked`, stamp today's deductions
    /// onto its `pantry_deductions`, and consume each row
    /// from the pantry. Atomic at the meal-row level only —
    /// pantry consumes are sequential, so on partial failure
    /// some stock may already be debited. (Future:
    /// transactional consume across rows.)
    fn cook(&self, id: &str, deductions: Vec<PantryDeduction>) -> Result<Meal, MealplanError>;

    /// Mark a meal as `skipped` without touching the pantry.
    fn skip(&self, id: &str) -> Result<Meal, MealplanError>;
}
