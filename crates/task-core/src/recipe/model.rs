//! `Recipe` entity — one cooking recipe (Mealie-style).

use chrono::NaiveDate;
use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(
    Clone,
    Debug,
    Default,
    PartialEq,
    Facet,
    DeriveEntityModel,
    EntityToModels,
    Serialize,
    Deserialize,
)]
#[sea_orm(table_name = "recipes")]
#[crudcrate(
    api_struct = "RecipeApi",
    generate_vox_service,
    name_singular = "recipe",
    name_plural = "recipes"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable, fulltext)]
    pub name: String,

    /// URL-friendly slug; unique within an organization. Generated from
    /// `name` if not provided. Collisions are resolved by appending
    /// `-2`, `-3`, … in the service layer.
    #[crudcrate(filterable)]
    pub slug: String,

    pub description: Option<String>,

    /// Owning organization (matches `seed_auth_data` org slugs).
    #[crudcrate(filterable)]
    pub organization: Option<String>,

    pub prep_time_minutes: Option<u32>,
    pub cook_time_minutes: Option<u32>,
    pub total_time_minutes: Option<u32>,
    pub servings: Option<u32>,
    /// Free-form yield string ("12 cookies", "1 loaf"). Useful when
    /// `servings` doesn't capture the unit.
    pub yield_label: Option<String>,
    pub source_url: Option<String>,
    /// Original image URL captured by the recipe importer
    /// (`schema.org` `image` or `og:image`). Bytes are not fetched in
    /// this bead; deferred to a follow-up that PUTs them to Nextcloud.
    pub image_url: Option<String>,
    /// 0.0..=5.0 — service-side validates the range.
    pub rating: Option<f32>,
    pub last_made: Option<NaiveDate>,
    pub notes: Option<String>,
    pub created_by: Option<String>,

    /// Obsidian-style polymorphic properties.
    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub properties: crate::property::JsonObject,

    /// Cached recipe-level nutrition aggregate. Populated by
    /// `CookingService::recompute_recipe_nutrition` after each
    /// `create_recipe` / `update_recipe`. Empty `{}` until first
    /// recompute. JSON-encoded
    /// [`crate::nutrition::AggregatedNutrition`]-shaped payload.
    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub nutrition_summary: crate::property::JsonObject,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,
    #[crudcrate(exclude(create), exclude(update), on_create = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {
    fn new() -> Self {
        Self {
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            nutrition_summary: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

/// Domain spec for a single ingredient row used inside the
/// `ingredients_json` payload of [`crate::service::CreateRecipeRequest`]
/// / [`crate::service::RecipePatch`]. Plain `serde` shape — these flow
/// over the wire as a JSON-encoded `Vec<RecipeIngredientSpec>`.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
pub struct RecipeIngredientSpec {
    pub sequence: Option<u32>,
    pub quantity: Option<f64>,
    pub unit: Option<String>,
    pub food: String,
    pub note: Option<String>,
    pub is_section: Option<bool>,
}

/// Domain spec for a single step inside `steps_json`.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
pub struct RecipeStepSpec {
    pub sequence: Option<u32>,
    pub text: String,
    pub duration_minutes: Option<u32>,
}
