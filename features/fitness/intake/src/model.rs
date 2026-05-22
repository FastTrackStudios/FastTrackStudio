//! Daily food intake log — one page per day with a flat
//! list of consumed entries.

use chrono::{DateTime, NaiveDate, NaiveTime, Utc};
use facet::Facet;
use mealplan::cookbook::Nutrition;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct IntakeLog {
    #[serde(skip)]
    pub path: String,

    pub id: Uuid,

    /// Display label — defaults to `"Intake <date>"` when
    /// auto-created via `log_*` shortcuts.
    pub name: String,

    pub date: NaiveDate,

    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub entries: Vec<IntakeEntry>,

    /// Daily targets — when set, callers can show
    /// progress bars without storing the goal twice. All
    /// fields optional so partial targets ("I track
    /// protein, not calories") work.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub target: Option<Nutrition>,

    /// Free-form tags — `"cut"`, `"bulk"`, `"travel-day"`,
    /// `"reset"`.
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

    /// Markdown body — notes, cravings, hunger ratings.
    #[serde(skip)]
    pub details: String,
}

/// One consumed item.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct IntakeEntry {
    pub id: Uuid,

    /// What this entry references. Drives the
    /// `source_id` lookup; `Freeform` rows skip lookup
    /// and carry their nutrition inline.
    pub source: IntakeSource,

    /// Cached display name — round-trips when the
    /// referenced recipe/pantry page isn't loaded.
    pub name: String,

    /// Qty consumed *in `unit`*. For recipes, qty is
    /// "servings" and unit is conventionally `"serving"`.
    pub qty: f64,

    #[serde(default)]
    pub unit: String,

    /// Time of day, when known. `None` for catch-all
    /// "lunch" entries the user didn't bother timestamping.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub time: Option<NaiveTime>,

    /// Optional meal-slot label — `"breakfast"`,
    /// `"lunch"`, `"dinner"`, `"snack"`. Free-form so
    /// custom slots (`"pre-workout"`) round-trip.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub slot: Option<String>,

    /// Resolved nutrition for this entry. Auto-populated
    /// when the entry is added via `log_recipe` /
    /// `log_pantry` from the source's nutrition fields
    /// (scaled by `qty`). Hand-entered for `Freeform`
    /// rows. Stored on the entry so a renamed/deleted
    /// source doesn't lose the calorie data.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub nutrition: Option<Nutrition>,

    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub note: Option<String>,
}

/// What an [`IntakeEntry`] points at.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[serde(tag = "kind", rename_all = "kebab-case")]
#[repr(u8)]
pub enum IntakeSource {
    /// A `cookbook::Recipe` referenced by vault-relative
    /// `.cook` path. `qty` is "servings of this recipe".
    Recipe { path: String },
    /// A `pantry::PantryItem` id. `qty` is in the
    /// pantry item's `unit` (typically the canonical
    /// stock unit).
    Pantry { id: Uuid },
    /// Hand-entered with no catalog link — restaurant
    /// food, friend's cooking, label-on-the-bag estimates.
    Freeform,
}

impl IntakeLog {
    /// Sum nutrition across every entry. Returns `None`
    /// only when no entry has nutrition data; partial
    /// coverage aggregates what's known.
    pub fn total(&self) -> Option<Nutrition> {
        let mut acc = Nutrition::default();
        let mut any = false;
        for entry in &self.entries {
            let Some(n) = &entry.nutrition else {
                continue;
            };
            any = true;
            acc.calories = sum(acc.calories, n.calories);
            acc.protein_g = sum(acc.protein_g, n.protein_g);
            acc.carbs_g = sum(acc.carbs_g, n.carbs_g);
            acc.fat_g = sum(acc.fat_g, n.fat_g);
            acc.fiber_g = sum(acc.fiber_g, n.fiber_g);
            acc.sugar_g = sum(acc.sugar_g, n.sugar_g);
        }
        any.then_some(acc)
    }

    /// Convenience: every entry inside `slot` (e.g.
    /// `"breakfast"`). Slot match is case-insensitive.
    pub fn entries_in_slot(&self, slot: &str) -> Vec<&IntakeEntry> {
        let needle = slot.to_ascii_lowercase();
        self.entries
            .iter()
            .filter(|e| {
                e.slot
                    .as_ref()
                    .is_some_and(|s| s.eq_ignore_ascii_case(&needle))
            })
            .collect()
    }
}

fn sum(a: Option<f64>, b: Option<f64>) -> Option<f64> {
    match (a, b) {
        (Some(x), Some(y)) => Some(x + y),
        (Some(x), None) | (None, Some(x)) => Some(x),
        (None, None) => None,
    }
}

/// Scale a `Nutrition` by `factor` — used when resolving
/// recipe nutrition (per-serving × servings consumed) or
/// pantry nutrition (per-unit × qty / nutrition_unit-qty).
pub fn scale_nutrition(n: &Nutrition, factor: f64) -> Nutrition {
    Nutrition {
        calories: n.calories.map(|v| v * factor),
        protein_g: n.protein_g.map(|v| v * factor),
        carbs_g: n.carbs_g.map(|v| v * factor),
        fat_g: n.fat_g.map(|v| v * factor),
        fiber_g: n.fiber_g.map(|v| v * factor),
        sugar_g: n.sugar_g.map(|v| v * factor),
    }
}
