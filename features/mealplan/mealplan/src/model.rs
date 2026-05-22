//! `Meal` — one cooked-or-planned meal on the calendar.
//!
//! Meals live as markdown pages with YAML frontmatter
//! (`type: meal`) under `<vault>/mealplan/`. They reference
//! [`cookbook::Recipe`]s by id (so renames don't break the
//! link) and, once cooked, record the [`pantry::PantryItem`]s
//! they consumed.

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

    /// Recipes this meal is built from. Multi-recipe meals
    /// are supported (entree + side); single-recipe meals
    /// carry a one-element vec.
    #[serde(skip_serializing_if = "Vec::is_empty", default, rename = "recipeIds")]
    pub recipe_ids: Vec<Uuid>,

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
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Breakfast => "breakfast",
            Self::Lunch => "lunch",
            Self::Dinner => "dinner",
            Self::Snack => "snack",
        }
    }

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
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Planned => "planned",
            Self::Cooked => "cooked",
            Self::Skipped => "skipped",
            Self::EatingOut => "eating-out",
        }
    }

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
