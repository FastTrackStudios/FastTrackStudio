//! `Recipe` — typed view of one recipe page.
//!
//! Recipes live as markdown files in a `vault::Vault` under
//! `Wiki/Cookbook/<slug>.md`. They're a *wiki superset* — the
//! body is free-form markdown with wikilinks (`[[saute]]`,
//! `[[mise en place]]`) that resolve into curated concept
//! pages elsewhere in the wiki. The cookbook crate only owns
//! the typed surface (ingredients, steps, nutrition); narrative
//! and links belong on the page body and are handled by the
//! wiki feature.

use chrono::{DateTime, Utc};
use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Recipe {
    #[serde(skip)]
    pub path: String,

    pub id: Uuid,

    /// Display title. Falls back to filename basename when
    /// missing.
    pub name: String,

    /// One-line summary for index views.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub description: Option<String>,

    /// Free-form. Canonical set in [`Course`]
    /// (`breakfast` / `lunch` / `dinner` / `snack` / `dessert`).
    #[serde(default = "default_course")]
    pub course: String,

    /// Cuisine label — `"italian"`, `"japanese"`, `"thai"`. Free
    /// form; convention only.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub cuisine: Option<String>,

    /// Active prep time in minutes (chopping, mixing).
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "prepMinutes"
    )]
    pub prep_minutes: Option<u32>,

    /// Cook time in minutes (oven, stove, simmer).
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "cookMinutes"
    )]
    pub cook_minutes: Option<u32>,

    /// How many servings the recipe yields at the listed
    /// ingredient quantities. Drives mealplan scaling.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub servings: Option<u32>,

    /// Ingredient list. Quantities are stored numerically so
    /// mealplan can scale; the unit is free-form so weird
    /// units (`"pinch"`, `"to taste"`) round-trip.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub ingredients: Vec<Ingredient>,

    /// Ordered steps. Each step is a markdown string; embed
    /// wikilinks (`[[saute]]`) and inline timing freely.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub steps: Vec<String>,

    /// Per-serving nutrition, when known.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub nutrition: Option<Nutrition>,

    /// Free-form tags — `"quick"`, `"weeknight"`, `"vegan"`,
    /// `"meal-prep"`. Drives queries.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub tags: Vec<String>,

    /// Sub-recipes this recipe references. Modeled on
    /// grocy's `recipes_nestings`. Each entry says "use N
    /// servings of recipe X" — pizza references pizza-dough,
    /// taco-night references homemade-salsa. Fulfillment
    /// recurses through nestings with a depth cap.
    #[serde(
        skip_serializing_if = "Vec::is_empty",
        default,
        rename = "nestedRecipes"
    )]
    pub nested_recipes: Vec<NestedRecipe>,

    /// Optional source — URL, cookbook citation, or
    /// `"[[Some Wiki Page]]"` wikilink.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub source: Option<String>,

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

    /// Markdown body — narrative, photos, variations,
    /// wikilinks to technique concept pages.
    #[serde(skip)]
    pub details: String,
}

fn default_course() -> String {
    Course::Main.as_str().to_string()
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Ingredient {
    /// Free-form name. Convention: wikilink the page-worthy
    /// ones (`"[[Olive Oil]]"`) so the wiki graph picks them
    /// up; raw strings are fine for one-offs.
    pub name: String,

    /// Numeric quantity. `None` for `"to taste"` / `"as
    /// needed"` cases.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub qty: Option<f64>,

    /// Free-form unit — `"g"`, `"cup"`, `"tbsp"`, `"clove"`,
    /// `"pinch"`. Recipe fulfillment matches recipe + pantry
    /// units with conversion via `pantry::units::convert_str`
    /// when bases align.
    #[serde(default)]
    pub unit: String,

    /// Explicit link to a `pantry::PantryItem` id. When set,
    /// fulfillment skips name fuzzy-match and goes straight
    /// to the linked row. Curators set this once when the
    /// recipe is committed; agents can resolve it via the
    /// barcode / lookup flow.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "pantryItemId"
    )]
    pub pantry_item_id: Option<uuid::Uuid>,

    /// Free-form prep note — `"finely diced"`, `"room temp"`.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub note: Option<String>,

    /// Skip when scaling pantry deductions or showing the
    /// quick-glance "do I have everything" check.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub optional: bool,
}

/// Sub-recipe reference. Pull in `servings` worth of
/// recipe `recipe_id` when expanding ingredients.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct NestedRecipe {
    #[serde(rename = "recipeId")]
    pub recipe_id: uuid::Uuid,
    /// How many servings of the nested recipe go into one
    /// serving of the parent. Multiplied with parent
    /// servings during fulfillment.
    #[serde(default = "default_nested_servings")]
    pub servings: u32,
}

fn default_nested_servings() -> u32 {
    1
}

/// Per-serving nutrition. All fields optional — partial data
/// is better than none.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize, Facet)]
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

/// Canonical course values. Parsing accepts any string —
/// unknown courses round-trip as the raw string on `Recipe`.
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
