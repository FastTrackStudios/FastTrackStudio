//! `FoodLog` entity — append-only nutrition diary row.

use chrono::{DateTime, NaiveDate, Utc};
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
#[sea_orm(table_name = "food_logs")]
#[crudcrate(
    api_struct = "FoodLogApi",
    generate_vox_service,
    name_singular = "food_log",
    name_plural = "food_logs"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable)]
    pub date: NaiveDate,

    /// Reuses [`crate::meal_plan::MealType`] as the SeaORM enum. The
    /// underlying string column accepts the same values.
    pub meal_type: crate::meal_plan::MealType,

    /// Soft references — at log time we capture `food_id` and
    /// `product_id` when known, but the snapshot fields below are the
    /// immutable source of truth. Historical rows do NOT update if a
    /// linked Food's `nutrition_per_100g` is later corrected.
    #[crudcrate(filterable)]
    pub food_id: Option<Uuid>,
    #[crudcrate(filterable)]
    pub product_id: Option<Uuid>,

    /// Denormalized for display / search ("scrambled eggs", "Vital
    /// Farms eggs"). Always set, even when `food_id` is set.
    pub food_name: String,

    /// Always grams. Service converts from cup/oz/tbsp using the
    /// nutrition::units helpers; volume → mass without a density
    /// returns 0.0 with a warning surfaced via the service response.
    pub quantity_grams: f64,

    // ── Snapshotted nutrition (immutable history) ──
    pub kcal: Option<f64>,
    pub protein_g: Option<f64>,
    pub carbs_g: Option<f64>,
    pub sugars_g: Option<f64>,
    pub fiber_g: Option<f64>,
    pub fat_g: Option<f64>,
    pub saturated_fat_g: Option<f64>,
    pub sodium_mg: Option<f64>,

    pub notes: Option<String>,
    pub created_by: Option<String>,

    /// When this log row was generated from a meal-plan entry being
    /// marked cooked, this links back. `None` for manual logs.
    pub meal_plan_entry_id: Option<Uuid>,

    /// When the log was generated from a recipe (rather than a single
    /// food), this is the recipe id. Useful for "trace these calories
    /// back to the dish I ate".
    pub recipe_id: Option<Uuid>,

    #[crudcrate(filterable)]
    pub organization: Option<String>,

    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub properties: crate::property::JsonObject,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: DateTime<Utc>,
    #[crudcrate(exclude(create), exclude(update), on_create = chrono::Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {
    fn new() -> Self {
        Self {
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}
