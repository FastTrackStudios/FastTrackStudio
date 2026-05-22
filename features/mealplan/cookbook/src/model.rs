//! `Recipe` — a cooklang `.cook` file as a typed wire value.
//!
//! Identity is the vault-relative `path` (`Cookbook/<slug>.cook`).
//! The recipe knows nothing about pantry IDs, our nutrition
//! database, or substitutions — those layers join by **ingredient
//! name** at mealprep time. The file is pure cooklang and
//! portable to every cooklang tool (cookcli, the Obsidian
//! plugin, VSCode, HomeAssistant, etc.).

use chrono::{DateTime, Utc};
use facet::Facet;
use serde::{Deserialize, Serialize};

/// Wire shape for a parsed `.cook` file. The original source is
/// preserved verbatim in `source` so editors can round-trip
/// without re-rendering through the cooklang printer.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Recipe {
    /// Vault-relative, forward-slash separated, e.g.
    /// `Cookbook/Truffle Pasta.cook`. Identity.
    pub path: String,

    /// Display title. Pulled from `>> title:` metadata, or
    /// falls back to the filename stem.
    pub name: String,

    /// `>> description:` from metadata.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub description: Option<String>,

    /// `>> course:` from metadata. Free-form; canonical set in
    /// [`Course`].
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub course: Option<String>,

    /// `>> cuisine:` from metadata.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub cuisine: Option<String>,

    /// `>> prep time:` in whole minutes, when parseable.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "prepMinutes"
    )]
    pub prep_minutes: Option<u32>,

    /// `>> cook time:` in whole minutes.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "cookMinutes"
    )]
    pub cook_minutes: Option<u32>,

    /// `>> servings:` — base yield. Drives scaling at mealprep
    /// time.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub servings: Option<u32>,

    /// Ingredients extracted from `@name{qty%unit}` lines, in
    /// document order. Names are wikilink targets.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub ingredients: Vec<Ingredient>,

    /// Rendered step text in document order. Plain string per
    /// step. Authoring goes through [`Recipe::source`].
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub steps: Vec<String>,

    /// Cookware names from `#pan{}`.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub cookware: Vec<String>,

    /// Sub-recipe references — paths from `@@./path/recipe{}`.
    #[serde(
        skip_serializing_if = "Vec::is_empty",
        default,
        rename = "nestedRecipes"
    )]
    pub nested_recipes: Vec<String>,

    /// `>> tags:` (comma-separated in the metadata block).
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub tags: Vec<String>,

    /// `>> source:` — URL, citation, or wikilink.
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "sourceUrl")]
    pub source_url: Option<String>,

    /// File mtime when scanned.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateModified"
    )]
    pub date_modified: Option<DateTime<Utc>>,

    /// Raw cooklang source. The source of truth — editors
    /// mutate this and re-parse.
    pub source: String,
}

/// One ingredient line. `qty` is the numeric quantity for math;
/// `qty_display` keeps the original display form (ranges,
/// fractions, text).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Ingredient {
    /// Cooklang ingredient name. Wikilink target.
    pub name: String,

    /// Optional alias from `@flour|all-purpose flour{}`.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub alias: Option<String>,

    /// Numeric quantity. `None` for `"to taste"` / text values.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub qty: Option<f64>,

    /// Free-form unit string. Empty when no unit.
    #[serde(default)]
    pub unit: String,

    /// Original display form, including ranges / fractions /
    /// text. Use for rendering; use [`Self::qty`] for math.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "qtyDisplay"
    )]
    pub qty_display: Option<String>,

    /// Cooklang note — `@butter{20%g}(softened)`.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub note: Option<String>,

    /// `true` when the ingredient line carries `?`.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub optional: bool,

    /// `true` when the line is a recipe reference (`@@...`).
    /// `name` then holds the recipe path.
    #[serde(
        default,
        skip_serializing_if = "std::ops::Not::not",
        rename = "isRecipeRef"
    )]
    pub is_recipe_ref: bool,
}

/// Canonical course values. Recipes round-trip arbitrary
/// strings; this is a hint for UI grouping.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Course {
    Breakfast,
    Lunch,
    Dinner,
    Main,
    Side,
    Snack,
    Dessert,
    Drink,
}

impl Course {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Breakfast => "breakfast",
            Self::Lunch => "lunch",
            Self::Dinner => "dinner",
            Self::Main => "main",
            Self::Side => "side",
            Self::Snack => "snack",
            Self::Dessert => "dessert",
            Self::Drink => "drink",
        }
    }

    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "breakfast" => Some(Self::Breakfast),
            "lunch" => Some(Self::Lunch),
            "dinner" => Some(Self::Dinner),
            "main" | "entree" | "main-course" => Some(Self::Main),
            "side" | "side-dish" => Some(Self::Side),
            "snack" => Some(Self::Snack),
            "dessert" | "sweet" => Some(Self::Dessert),
            "drink" | "beverage" => Some(Self::Drink),
            _ => None,
        }
    }
}
