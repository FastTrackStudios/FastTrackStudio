//! Cycle/sprint entity.

use crudcrate::EntityToModels;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, EntityToModels, Serialize, Deserialize)]
#[sea_orm(table_name = "cycles")]
#[crudcrate(
    api_struct = "CycleApi",
    generate_vox_service,
    name_singular = "cycle",
    name_plural = "cycles"
)]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable, sortable)]
    pub title: String,
    pub description: Option<String>,
    #[crudcrate(filterable)]
    pub project: String,
    #[crudcrate(filterable, sortable)]
    pub status: String,
    #[crudcrate(filterable)]
    pub owned_by: Option<String>,

    pub start_date: Option<chrono::NaiveDate>,
    pub end_date: Option<chrono::NaiveDate>,

    /// Task IDs in this cycle (JSON array).
    pub tasks: Json,

    pub sort_order: Option<f64>,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,

    #[crudcrate(exclude(create, update), on_create = chrono::Utc::now(), on_update = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}
