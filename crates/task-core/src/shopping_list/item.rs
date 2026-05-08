//! `ShoppingListItem` entity.

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
#[sea_orm(table_name = "shopping_list_items")]
#[crudcrate(
    api_struct = "ShoppingListItemApi",
    generate_vox_service,
    name_singular = "shopping_list_item",
    name_plural = "shopping_list_items"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable)]
    pub list_id: Uuid,

    pub sequence: u32,
    pub quantity: Option<f64>,
    pub unit: Option<String>,
    pub food: String,
    pub note: Option<String>,

    #[crudcrate(filterable)]
    pub recipe_id: Option<Uuid>,
    #[crudcrate(filterable)]
    pub meal_plan_id: Option<Uuid>,

    pub checked: bool,

    /// Aisle/section label ("produce", "dairy", "frozen").
    #[crudcrate(filterable)]
    pub label: Option<String>,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,
    #[crudcrate(exclude(create), exclude(update), on_create = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}
