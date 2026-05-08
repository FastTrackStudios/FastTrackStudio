//! `PantryItem` entity — stock-on-hand line item.
//!
//! At least one of `food_id` / `product_id` is set (enforced at the
//! service layer; soft FK to [`crate::food::Food`] /
//! [`crate::food_product::FoodProduct`]). `location_id` is a soft FK to
//! [`crate::location::Location`]; rows whose location_id is None are
//! "uncategorized pantry" — useful for transient items.

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
#[sea_orm(table_name = "pantry_items")]
#[crudcrate(
    api_struct = "PantryItemApi",
    generate_vox_service,
    name_singular = "pantry_item",
    name_plural = "pantry_items"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// One of `food_id` / `product_id` MUST be set; the service layer
    /// enforces. When both are set, `food_id` is redundant but useful
    /// for `food_id`-keyed queries.
    #[crudcrate(filterable)]
    pub food_id: Option<Uuid>,

    #[crudcrate(filterable)]
    pub product_id: Option<Uuid>,

    /// Storage location (Location entity, marked with
    /// `venue_type = "pantry-storage"`). `None` = uncategorized.
    #[crudcrate(filterable)]
    pub location_id: Option<Uuid>,

    pub quantity: f64,
    pub unit: String,

    pub expiration_date: Option<NaiveDate>,
    /// When the user first opened this stock — drives shorter shelf-life
    /// calculations on perishables.
    pub opened_at: Option<DateTime<Utc>>,
    /// Threshold for the low-stock report; `None` opts the row out.
    pub min_stock: Option<f64>,
    pub purchased_at: Option<NaiveDate>,
    pub notes: Option<String>,

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

pub type PantryItem = Model;
