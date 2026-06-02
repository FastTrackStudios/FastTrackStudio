//! [`Nutrition`] — the shared macro shape.

use facet::Facet;
use serde::{Deserialize, Serialize};

/// Per-unit nutrition. Lives on a `pantry::PantryItem` (the
/// wiki page for "Flour" carries `nutritionPerUnit` so any
/// recipe using `@flour{...}` can be aggregated at mealprep
/// time). Kept in this proto crate as the shared nutrition shape —
/// consumers (`pantry`, `intake`, `fitness`) all reference
/// `cookbook::Nutrition`. Derives `architect::JsonField` so
/// downstream crates can use it as a `#[architect(json)]`
/// column directly (no `DailyTarget`-style wrapper needed).
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(architect::JsonField, Debug, Clone, Default, PartialEq, Facet, Serialize, Deserialize)]
pub struct Nutrition {
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub calories: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "proteinG")]
    pub protein_g: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "carbsG")]
    pub carbs_g: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "fatG")]
    pub fat_g: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "fiberG")]
    pub fiber_g: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "sugarG")]
    pub sugar_g: Option<f64>,
}
