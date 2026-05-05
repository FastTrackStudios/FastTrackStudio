//! Activity entity — audit log for all changes.

use crudcrate::EntityToModels;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, EntityToModels, Serialize, Deserialize)]
#[sea_orm(table_name = "activities")]
#[crudcrate(
    api_struct = "ActivityApi",
    generate_vox_service,
    name_singular = "activity",
    name_plural = "activities"
)]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Entity that changed.
    #[crudcrate(filterable)]
    pub entity_id: Uuid,
    /// Entity type: "task", "project", "comment".
    #[crudcrate(filterable)]
    pub entity_type: String,

    /// Action: "created", "updated", "deleted", "completed", "commented".
    #[crudcrate(filterable)]
    pub verb: String,
    /// Which field changed (for updates).
    pub field: Option<String>,
    /// Previous value.
    pub old_value: Option<String>,
    /// New value.
    pub new_value: Option<String>,

    /// Who made the change.
    #[crudcrate(filterable)]
    pub actor: Option<String>,

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
