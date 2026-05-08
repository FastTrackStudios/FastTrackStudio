//! `RecipeIngredient` — child rows of `Recipe`, ordered by `sequence`.
//!
//! Mirrors the Mealie ingredient row: optional `quantity`/`unit`, free-text
//! `food`, optional `note`. `is_section = true` rows are rendered as
//! section headers ("For the sauce:") rather than ingredients.

pub mod model;

pub use model::Model as RecipeIngredient;
pub use model::*;

use crate::recipe::RecipeIngredientSpec;

/// Parse a single free-text ingredient line into a [`RecipeIngredientSpec`].
///
/// Heuristic: the first whitespace-delimited token is treated as
/// `quantity` if it parses as `f64`; the second token is treated as a
/// `unit` only when at least one further token follows. Failing both
/// cases the entire string lands in `food`.
///
/// Examples:
/// - `"1.5 cup olive oil"` → `quantity=1.5, unit="cup", food="olive oil"`.
/// - `"2 eggs"` → `quantity=2.0, unit=None, food="eggs"`.
/// - `"salt"` → `quantity=None, unit=None, food="salt"`.
///
/// Schema.org `recipeIngredient` lines are messier than CLI input, so
/// any line we can't crack lands in `food` unchanged. Production-grade
/// NLP is intentionally out of scope.
#[must_use]
pub fn parse_ingredient_line(spec: &str) -> RecipeIngredientSpec {
    let trimmed = spec.trim();
    if trimmed.is_empty() {
        return RecipeIngredientSpec {
            food: String::new(),
            ..Default::default()
        };
    }
    let mut parts = trimmed.split_whitespace();
    let first = parts.next().unwrap_or("");
    if let Ok(q) = first.parse::<f64>() {
        let rest: Vec<&str> = parts.collect();
        if rest.len() >= 2 {
            let unit = rest[0].to_string();
            let food = rest[1..].join(" ");
            RecipeIngredientSpec {
                quantity: Some(q),
                unit: Some(unit),
                food,
                ..Default::default()
            }
        } else {
            RecipeIngredientSpec {
                quantity: Some(q),
                food: rest.join(" "),
                ..Default::default()
            }
        }
    } else {
        RecipeIngredientSpec {
            food: trimmed.to_string(),
            ..Default::default()
        }
    }
}
