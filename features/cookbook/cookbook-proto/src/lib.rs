//! `cookbook-proto` — wire contract for the `cookbook` feature.
//!
//! Five top-level entities:
//!
//! - `Cookbook`         — a collection of recipes
//! - `Recipe`           — a recipe belonging to a cookbook
//! - `RecipeIngredient` — a single ingredient line for a recipe
//! - `RecipeStep`       — a single instruction step for a recipe
//! - `MealPlan`         — a planned meal on a date

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Cookbook ──────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "cookbooks", repo)]
pub struct Cookbook {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(filterable)]
    pub author: Option<String>,

    #[architect(fulltext)]
    pub description: Option<String>,

    pub cover_image_url: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Recipe ────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "recipes", repo)]
pub struct Recipe {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub cookbook_id: Option<Uuid>,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(fulltext)]
    pub summary: Option<String>,

    pub servings: Option<u32>,

    pub prep_time_minutes: Option<u32>,

    pub cook_time_minutes: Option<u32>,

    #[architect(sortable)]
    pub total_time_minutes: Option<u32>,

    #[architect(filterable)]
    pub cuisine: Option<String>,

    pub source_url: Option<String>,

    pub image_url: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── RecipeIngredient ──────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "recipe_ingredients", repo)]
pub struct RecipeIngredient {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    pub recipe_id: Uuid,

    #[architect(fulltext)]
    pub name: String,

    pub qty_thousandths: i64,

    pub unit: Option<String>,

    pub notes: Option<String>,

    #[architect(sortable)]
    pub sort_index: i64,

    #[architect(filterable)]
    pub food_product_id: Option<Uuid>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── RecipeStep ────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "recipe_steps", repo)]
pub struct RecipeStep {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    pub recipe_id: Uuid,

    #[architect(sortable)]
    pub step_number: u32,

    #[architect(fulltext)]
    pub instruction: String,

    pub duration_minutes: Option<u32>,

    pub image_url: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── MealPlan ──────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "meal_plans", repo)]
pub struct MealPlan {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub recipe_id: Option<Uuid>,

    #[architect(filterable, fulltext)]
    pub name: String,

    #[architect(filterable, sortable)]
    pub planned_for: DateTime<Utc>,

    #[architect(filterable)]
    pub meal_type: String,

    pub servings: Option<u32>,

    pub notes: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── CookbookService ───────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum CookbookServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait CookbookService {
    /// Duplicate a recipe (including its ingredients and steps) into
    /// the given cookbook (or leave standalone if `None`). Returns the
    /// id of the newly created recipe.
    async fn duplicate_recipe(
        &self,
        recipe_id: Uuid,
        into_cookbook_id: Option<Uuid>,
    ) -> Result<Uuid, CookbookServiceError>;
}
