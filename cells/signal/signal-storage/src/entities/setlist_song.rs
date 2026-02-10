//! Setlist song join entity — links setlists to songs with ordering.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "setlist_songs")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    pub setlist_id: Uuid,
    pub song_id: Uuid,
    pub sort_order: i32,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::setlist::Entity",
        from = "Column::SetlistId",
        to = "super::setlist::Column::Id"
    )]
    Setlist,
    #[sea_orm(
        belongs_to = "super::performance_song::Entity",
        from = "Column::SongId",
        to = "super::performance_song::Column::Id"
    )]
    PerformanceSong,
}

impl Related<super::setlist::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::Setlist.def()
    }
}

impl Related<super::performance_song::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::PerformanceSong.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}
