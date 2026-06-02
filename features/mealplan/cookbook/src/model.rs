//! `Recipe` — a cooklang `.cook` file as a typed wire value.
//!
//! Identity is the wiki-relative `path`
//! (`Cookbook/<slug>.cook`, joined with the wiki root the
//! caller provides — typically `<org>/wiki/Knowledge/`).
//! The recipe knows nothing about pantry IDs, our nutrition
//! database, or substitutions — those layers join by **ingredient
//! name** at mealprep time. The file is pure cooklang and
//! portable to every cooklang tool (cookcli, the Obsidian
//! plugin, VSCode, HomeAssistant, etc.).

use chrono::{DateTime, Utc};
use facet::Facet;
use serde::{Deserialize, Serialize};

/// `Vec<String>` newtype — JSON column. Steps / cookware /
/// nested_recipes / tags all share this shape.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(
    architect::JsonField, Debug, Clone, Default, PartialEq, Eq, Facet, Serialize, Deserialize,
)]
#[repr(transparent)]
#[serde(transparent)]
pub struct StringList(pub Vec<String>);

impl StringList {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Vec<String>> for StringList {
    fn from(v: Vec<String>) -> Self {
        Self(v)
    }
}

impl FromIterator<String> for StringList {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl std::ops::Deref for StringList {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// `Vec<Ingredient>` newtype — JSON column.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(architect::JsonField, Debug, Clone, Default, PartialEq, Facet, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct Ingredients(pub Vec<Ingredient>);

impl Ingredients {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Vec<Ingredient>> for Ingredients {
    fn from(v: Vec<Ingredient>) -> Self {
        Self(v)
    }
}

impl FromIterator<Ingredient> for Ingredients {
    fn from_iter<I: IntoIterator<Item = Ingredient>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl std::ops::Deref for Ingredients {
    type Target = Vec<Ingredient>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Wire shape for a parsed `.cook` file. The original source is
/// preserved verbatim in `source` so editors can round-trip
/// without re-rendering through the cooklang printer.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(architect::Entity, Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[architect(table_name = "recipes", repo)]
pub struct Recipe {
    /// Vault-relative, forward-slash separated, e.g.
    /// `Cookbook/Truffle Pasta.cook` (wiki-relative).
    /// Identity — primary key.
    #[architect(primary_key, auto_increment = false)]
    pub path: String,

    /// Display title. Pulled from `>> title:` metadata, or
    /// falls back to the filename stem.
    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    /// `>> description:` from metadata.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub description: Option<String>,

    /// `>> course:` from metadata. Free-form; canonical set in
    /// [`Course`].
    #[serde(skip_serializing_if = "Option::is_none", default)]
    #[architect(filterable)]
    pub course: Option<String>,

    /// `>> cuisine:` from metadata.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    #[architect(filterable)]
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
    #[serde(skip_serializing_if = "Ingredients::is_empty", default)]
    #[architect(json)]
    pub ingredients: Ingredients,

    /// Rendered step text in document order. Plain string per
    /// step. Authoring goes through [`Recipe::source`].
    #[serde(skip_serializing_if = "StringList::is_empty", default)]
    #[architect(json)]
    pub steps: StringList,

    /// Cookware names from `#pan{}`.
    #[serde(skip_serializing_if = "StringList::is_empty", default)]
    #[architect(json)]
    pub cookware: StringList,

    /// Sub-recipe references — paths from `@@./path/recipe{}`.
    #[serde(
        skip_serializing_if = "StringList::is_empty",
        default,
        rename = "nestedRecipes"
    )]
    #[architect(json)]
    pub nested_recipes: StringList,

    /// `>> tags:` (comma-separated in the metadata block).
    #[serde(skip_serializing_if = "StringList::is_empty", default)]
    #[architect(json)]
    pub tags: StringList,

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
/// fractions, text). Held inline as JSON inside
/// [`Recipe::ingredients`].
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
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

/// Per-unit nutrition now lives in the wasm-clean
/// [`cookbook_proto`] crate so downstream wire surfaces
/// (`fitness-proto`, the web UI) can bind to it without pulling
/// the vault-backed `cookbook` crate. Re-exported here so the
/// existing `cookbook::model::Nutrition` /
/// `cookbook::Nutrition` paths keep working.
pub use cookbook_proto::Nutrition;

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
