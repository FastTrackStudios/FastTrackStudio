//! Saved view entity — persistent filter/sort/display configurations.

use crudcrate::EntityToModels;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, EntityToModels, Serialize, Deserialize)]
#[sea_orm(table_name = "saved_views")]
#[crudcrate(
    api_struct = "SavedViewApi",
    generate_vox_service,
    name_singular = "saved_view",
    name_plural = "saved_views"
)]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable, sortable)]
    pub title: String,
    pub description: Option<String>,

    /// Optional project scope (null = workspace-level view).
    #[crudcrate(filterable)]
    pub project: Option<String>,

    /// Filter criteria (JSON).
    pub filters: Json,

    /// Display settings (JSON: layout, group_by, order_by, visible_properties).
    pub display: Json,

    #[crudcrate(filterable)]
    pub created_by: Option<String>,
    #[crudcrate(filterable)]
    pub is_shared: bool,
    pub sort_order: Option<f64>,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,

    #[crudcrate(exclude(create, update), on_create = chrono::Utc::now(), on_update = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}
