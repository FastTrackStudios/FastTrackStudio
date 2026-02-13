//! Song role entity — independent section arrangements within a song.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "song_roles")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    pub song_id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    pub is_default: bool,
    pub sort_order: i32,
    pub tags: Json,
    pub created_at: DateTimeWithTimeZone,
    pub updated_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::performance_song::Entity",
        from = "Column::SongId",
        to = "super::performance_song::Column::Id"
    )]
    PerformanceSong,
    #[sea_orm(has_many = "super::song_scene::Entity")]
    SongScenes,
}

impl Related<super::performance_song::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::PerformanceSong.def()
    }
}

impl Related<super::song_scene::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::SongScenes.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}
