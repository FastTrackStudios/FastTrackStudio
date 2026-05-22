//! `Meal` — one cooked-or-planned meal on the calendar.
//!
//! Meals live as markdown pages with YAML frontmatter
//! (`type: meal`) under `<vault>/Projects/Mealplan/meals/`.
//! They reference [`cookbook::Recipe`]s by **vault-relative
//! path** (cooklang `.cook` files have no UUID; the path is
//! identity). Rename a recipe → existing rename pipeline
//! rewrites referencing meals. Once cooked, the meal records
//! which [`pantry::PantryItem`]s it consumed.

use chrono::{DateTime, NaiveDate, Utc};
use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Meal {
    #[serde(skip)]
    pub path: String,

    pub id: Uuid,

    /// Display title — `"Tuesday Dinner"`, `"Post-workout
    /// shake"`, or just the recipe name on simple days.
    pub name: String,

    /// Calendar date this meal is scheduled for (or was
    /// cooked on, once `status` is `cooked`).
    #[serde(rename = "scheduledFor")]
    pub scheduled_for: NaiveDate,

    /// Free-form slot. Canonical set in [`Slot`].
    #[serde(default = "default_slot")]
    pub slot: String,

    /// Servings to make — multiplier on each referenced
    /// recipe's base `servings`. `1` by default; bump for
    /// batch-cook days.
    #[serde(default = "default_servings")]
    pub servings: u32,

    /// Recipes this meal is built from. Vault-relative paths
    /// to `.cook` files (cooklang convention). Multi-recipe
    /// meals supported.
    #[serde(skip_serializing_if = "Vec::is_empty", default, rename = "recipePaths")]
    pub recipe_paths: Vec<String>,

    /// Free-form lifecycle status. Canonical set in
    /// [`Status`]: `planned` / `cooked` / `skipped` /
    /// `eating-out`.
    #[serde(default = "default_status")]
    pub status: String,

    /// What got pulled from the pantry when this meal was
    /// cooked. Populated by [`crate::service::MealplanService::cook`];
    /// `None` while the meal is still `planned`.
    #[serde(
        skip_serializing_if = "Vec::is_empty",
        default,
        rename = "pantryDeductions"
    )]
    pub pantry_deductions: Vec<PantryDeduction>,

    /// Free-form tags — `"meal-prep"`, `"date-night"`,
    /// `"leftovers"`.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub tags: Vec<String>,

    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateCreated"
    )]
    pub date_created: Option<DateTime<Utc>>,

    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateModified"
    )]
    pub date_modified: Option<DateTime<Utc>>,

    /// Markdown body — pre-cook notes ("thaw chicken Tuesday
    /// morning"), post-cook review ("too much salt"),
    /// photos.
    #[serde(skip)]
    pub details: String,
}

/// One row in [`Meal::pantry_deductions`] — what got
/// consumed when the meal was cooked.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct PantryDeduction {
    /// `pantry::PantryItem` id.
    #[serde(rename = "itemId")]
    pub item_id: Uuid,

    pub qty: f64,

    #[serde(default)]
    pub unit: String,
}

fn default_servings() -> u32 {
    1
}

impl Meal {
    /// Total nutrition for this meal — computed by
    /// matching each recipe's cooklang `@ingredient{qty%unit}`
    /// lines against `pantry`'s per-unit nutrition data.
    /// The pantry / wiki page for each ingredient (e.g.
    /// `Wiki/flour.md`) carries `nutritionPerUnit`; we sum
    /// `qty × per_unit_nutrition / unit_basis` for every
    /// ingredient that resolves.
    ///
    /// `recipes` is any superset that includes this meal's
    /// referenced recipes (by path); unmatched paths are
    /// skipped. `pantry` is the current pantry snapshot.
    /// Returns `None` when no ingredient resolves.
    #[must_use]
    pub fn nutrition_total(
        &self,
        recipes: &[cookbook::Recipe],
        pantry: &[pantry::PantryItem],
    ) -> Option<cookbook::Nutrition> {
        use std::collections::HashMap;
        let index: HashMap<&str, &cookbook::Recipe> =
            recipes.iter().map(|r| (r.path.as_str(), r)).collect();

        let mut acc = cookbook::Nutrition::default();
        let mut any = false;
        for path in &self.recipe_paths {
            let Some(recipe) = index.get(path.as_str()) else {
                continue;
            };
            let base = recipe.servings.filter(|s| *s > 0).unwrap_or(1);
            let scale = f64::from(self.servings) / f64::from(base);
            for ing in &recipe.ingredients {
                let Some(qty) = ing.qty else {
                    continue;
                };
                let Some(item) = match_pantry_by_name(&ing.name, pantry) else {
                    continue;
                };
                let Some(per_unit) = &item.nutrition_per_unit else {
                    continue;
                };
                // nutritionUnit on the pantry item is the
                // basis (e.g. "100g" or "1 cup"). We compute
                // factor = (qty * scale, converted to item.unit)
                //          / basis_amount
                let item_qty =
                    pantry::convert_str(qty * scale, &ing.unit, &item.unit).unwrap_or(qty * scale);
                let basis = nutrition_basis_qty(item.nutrition_unit.as_deref().unwrap_or(""));
                if basis <= 0.0 {
                    continue;
                }
                let factor = item_qty / basis;
                any = true;
                acc.calories = sum(acc.calories, per_unit.calories.map(|v| v * factor));
                acc.protein_g = sum(acc.protein_g, per_unit.protein_g.map(|v| v * factor));
                acc.carbs_g = sum(acc.carbs_g, per_unit.carbs_g.map(|v| v * factor));
                acc.fat_g = sum(acc.fat_g, per_unit.fat_g.map(|v| v * factor));
                acc.fiber_g = sum(acc.fiber_g, per_unit.fiber_g.map(|v| v * factor));
                acc.sugar_g = sum(acc.sugar_g, per_unit.sugar_g.map(|v| v * factor));
            }
        }
        any.then_some(acc)
    }
}

fn match_pantry_by_name<'a>(
    needle_raw: &str,
    pantry: &'a [pantry::PantryItem],
) -> Option<&'a pantry::PantryItem> {
    let needle = needle_raw
        .trim()
        .trim_start_matches("[[")
        .trim_end_matches("]]")
        .to_ascii_lowercase();
    if needle.is_empty() {
        return None;
    }
    pantry
        .iter()
        .find(|p| p.name.eq_ignore_ascii_case(&needle))
        .or_else(|| {
            pantry
                .iter()
                .find(|p| p.name.to_ascii_lowercase().contains(&needle))
        })
        .or_else(|| {
            pantry
                .iter()
                .find(|p| needle.contains(&p.name.to_ascii_lowercase()))
        })
}

/// Parse the numeric basis from a `nutritionUnit` field like
/// `"100g"`, `"1 cup"`, `"30 ml"`. Returns the numeric prefix;
/// `1.0` when none parses (treat as a per-unit-of-stock basis).
fn nutrition_basis_qty(unit: &str) -> f64 {
    let trimmed = unit.trim();
    if trimmed.is_empty() {
        return 1.0;
    }
    let n: String = trimmed
        .chars()
        .take_while(|c| c.is_ascii_digit() || *c == '.')
        .collect();
    n.parse().unwrap_or(1.0)
}

fn sum(a: Option<f64>, b: Option<f64>) -> Option<f64> {
    match (a, b) {
        (Some(x), Some(y)) => Some(x + y),
        (Some(x), None) | (None, Some(x)) => Some(x),
        (None, None) => None,
    }
}

fn default_slot() -> String {
    Slot::Dinner.as_str().to_string()
}

fn default_status() -> String {
    Status::Planned.as_str().to_string()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Slot {
    Breakfast,
    Lunch,
    Dinner,
    Snack,
}

impl Slot {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Breakfast => "breakfast",
            Self::Lunch => "lunch",
            Self::Dinner => "dinner",
            Self::Snack => "snack",
        }
    }

    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "breakfast" | "brunch" => Some(Self::Breakfast),
            "lunch" => Some(Self::Lunch),
            "dinner" | "supper" => Some(Self::Dinner),
            "snack" => Some(Self::Snack),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Status {
    Planned,
    Cooked,
    Skipped,
    EatingOut,
}

impl Status {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Planned => "planned",
            Self::Cooked => "cooked",
            Self::Skipped => "skipped",
            Self::EatingOut => "eating-out",
        }
    }

    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "planned" | "scheduled" => Some(Self::Planned),
            "cooked" | "done" | "made" => Some(Self::Cooked),
            "skipped" | "cancelled" => Some(Self::Skipped),
            "eating-out" | "eating_out" | "out" | "restaurant" => Some(Self::EatingOut),
            _ => None,
        }
    }
}
