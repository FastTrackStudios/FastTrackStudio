//! `MealPlanEntry` entity.

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
#[sea_orm(table_name = "meal_plan_entries")]
#[crudcrate(
    api_struct = "MealPlanEntryApi",
    generate_vox_service,
    name_singular = "meal_plan_entry",
    name_plural = "meal_plan_entries"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable)]
    pub date: NaiveDate,

    #[crudcrate(filterable)]
    pub meal_type: MealType,

    #[crudcrate(filterable)]
    pub organization: Option<String>,

    #[crudcrate(filterable)]
    pub recipe_id: Option<Uuid>,

    /// Free-form title used when `recipe_id` is None
    /// ("leftovers", "takeout", "Sunday brunch").
    pub title: Option<String>,
    pub servings_planned: Option<u32>,
    pub notes: Option<String>,
    pub created_by: Option<String>,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,
    #[crudcrate(exclude(create), exclude(update), on_create = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}

#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    DeriveActiveEnum,
    EnumIter,
    utoipa::ToSchema,
    facet::Facet,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(16))")]
#[repr(u8)]
pub enum MealType {
    #[default]
    #[sea_orm(string_value = "breakfast")]
    Breakfast,
    #[sea_orm(string_value = "lunch")]
    Lunch,
    #[sea_orm(string_value = "dinner")]
    Dinner,
    #[sea_orm(string_value = "snack")]
    Snack,
    #[sea_orm(string_value = "side")]
    Side,
    #[sea_orm(string_value = "other")]
    Other,
}

impl MealType {
    pub fn as_str(&self) -> &'static str {
        match self {
            MealType::Breakfast => "breakfast",
            MealType::Lunch => "lunch",
            MealType::Dinner => "dinner",
            MealType::Snack => "snack",
            MealType::Side => "side",
            MealType::Other => "other",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s.to_ascii_lowercase().as_str() {
            "breakfast" | "bfast" => Some(MealType::Breakfast),
            "lunch" => Some(MealType::Lunch),
            "dinner" | "supper" => Some(MealType::Dinner),
            "snack" => Some(MealType::Snack),
            "side" => Some(MealType::Side),
            "other" => Some(MealType::Other),
            _ => None,
        }
    }
}
