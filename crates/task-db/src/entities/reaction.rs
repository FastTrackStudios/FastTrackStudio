//! Reaction entity — emoji reactions on any entity.

use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};
use crudcrate::EntityToModels;
use crudcrate::CRUDResource;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, EntityToModels, Serialize, Deserialize)]
#[sea_orm(table_name = "reactions")]
#[crudcrate(
    api_struct = "ReactionApi",
    generate_vox_service,
    name_singular = "reaction",
    name_plural = "reactions",
)]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Entity this reaction is on.
    #[crudcrate(filterable)]
    pub entity_id: Uuid,
    /// Entity type: "task", "comment".
    #[crudcrate(filterable)]
    pub entity_type: String,

    #[crudcrate(filterable)]
    pub emoji: String,
    #[crudcrate(filterable)]
    pub user: String,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::task::Entity",
        from = "Column::EntityId",
        to = "super::task::Column::Id"
    )]
    Task,
}

impl ActiveModelBehavior for ActiveModel {}
