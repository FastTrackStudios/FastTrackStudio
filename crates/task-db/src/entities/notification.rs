//! Notification entity.

use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};
use crudcrate::EntityToModels;
use crudcrate::CRUDResource;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, EntityToModels, Serialize, Deserialize)]
#[sea_orm(table_name = "notifications")]
#[crudcrate(
    api_struct = "NotificationApi",
    generate_vox_service,
    name_singular = "notification",
    name_plural = "notifications",
)]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable)]
    pub recipient: String,
    #[crudcrate(filterable)]
    pub kind: String,
    pub message: String,
    #[crudcrate(filterable)]
    pub actor: Option<String>,

    /// Reference to the entity that triggered this notification.
    #[crudcrate(filterable)]
    pub entity_id: Option<Uuid>,
    #[crudcrate(filterable)]
    pub entity_type: Option<String>,
    #[crudcrate(filterable)]
    pub project: Option<String>,

    pub read_at: Option<chrono::DateTime<chrono::Utc>>,
    pub snoozed_till: Option<chrono::DateTime<chrono::Utc>>,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}
