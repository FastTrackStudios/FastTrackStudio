//! `FoodProduct` entity — branded, barcode-keyed product.

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
#[sea_orm(table_name = "food_products")]
#[crudcrate(
    api_struct = "FoodProductApi",
    generate_vox_service,
    name_singular = "food_product",
    name_plural = "food_products"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// FK to [`crate::food::Food`]. Required — every product is "a kind
    /// of" Food.
    #[crudcrate(filterable)]
    pub food_id: Uuid,

    /// EAN-13 / UPC-A barcode. String to preserve leading zeros. Unique
    /// within an organization when present.
    #[crudcrate(filterable)]
    pub barcode: Option<String>,

    pub brand: Option<String>,
    pub name: String,
    /// Total grams in the package — drives unit conversion ("how many
    /// of this can I get out of one container?").
    pub package_size_g: Option<f64>,
    /// Display fallback when the package is irregular ("500ml",
    /// "1lb 2oz").
    pub package_size_label: Option<String>,

    /// Source of the product data:
    ///   `"openfoodfacts"` — pulled from Open Food Facts API (cached)
    ///   `"manual"`        — user-entered
    ///   `"barcode-lookup-other"` — reserved for future plugin lookups
    #[crudcrate(filterable)]
    pub source: String,

    /// Open Food Facts product code (matches barcode in their system) or
    /// equivalent external id. Useful for re-syncing.
    pub external_id: Option<String>,

    /// Brand-specific nutrition (overrides
    /// [`crate::food::Food::nutrition_per_100g`] when the product is
    /// what's actually consumed/stocked).
    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub nutrition_per_100g: crate::property::JsonObject,

    pub image_url: Option<String>,
    pub last_synced_at: Option<chrono::DateTime<chrono::Utc>>,

    #[crudcrate(filterable)]
    pub organization: Option<String>,

    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub properties: crate::property::JsonObject,

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
            nutrition_per_100g: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

pub type FoodProduct = Model;
