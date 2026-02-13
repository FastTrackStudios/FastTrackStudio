//! Song scene entity — ordered scenes within a performance song.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "song_scenes")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    pub song_id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    pub preset_id: Uuid,
    pub snapshot_id: Option<Uuid>,
    pub transition: Json,
    pub midi_triggers: Json,
    pub module_overrides: Json,
    pub block_overrides: Json,
    pub sort_order: i32,
    pub is_default: bool,
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
}

impl Related<super::performance_song::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::PerformanceSong.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}
