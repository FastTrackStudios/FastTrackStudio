//! Performance song entity — songs with ordered scenes for live performance.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "performance_songs")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub artist: Option<String>,
    pub auto_advance: bool,
    pub linked_song_id: Option<Uuid>,
    pub module_overrides: Json,
    pub tags: Json,
    pub is_deleted: bool,
    pub created_at: DateTimeWithTimeZone,
    pub updated_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(has_many = "super::song_scene::Entity")]
    SongScenes,
}

impl Related<super::song_scene::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::SongScenes.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}
