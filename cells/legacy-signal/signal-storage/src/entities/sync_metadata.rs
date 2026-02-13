//! SyncMetadata entity — tracks last sync state per entity for delta sync.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "sync_metadata")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(50))")]
    pub entity_type: String,
    pub entity_id: Uuid,
    pub last_sync_at: DateTimeWithTimeZone,
    #[sea_orm(column_type = "String(StringLen::N(20))")]
    pub sync_status: String,
    pub local_version: i64,
    pub remote_version: i64,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}
