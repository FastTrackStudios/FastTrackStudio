//! Setlist entity — ordered collections of performance songs.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "setlists")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    pub metadata: Json,
    pub tags: Json,
    pub is_deleted: bool,
    pub created_at: DateTimeWithTimeZone,
    pub updated_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(has_many = "super::setlist_song::Entity")]
    SetlistSongs,
}

impl Related<super::setlist_song::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::SetlistSongs.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}
