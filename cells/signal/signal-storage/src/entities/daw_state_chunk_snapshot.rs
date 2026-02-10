//! DAW state chunk snapshot entity — Track Snapshot-style full state captures keyed by track GUID.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "daw_state_chunk_snapshots")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub track_guid: String,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    pub data: Json,
    pub created_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}
