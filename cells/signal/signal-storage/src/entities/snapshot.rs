//! Snapshot entity — named scene variations within a preset.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "snapshots")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    pub preset_id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    pub data: Json,
    pub is_default: bool,
    pub created_at: DateTimeWithTimeZone,
    pub updated_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::preset::Entity",
        from = "Column::PresetId",
        to = "super::preset::Column::Id",
        on_delete = "Cascade"
    )]
    Preset,
}

impl Related<super::preset::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::Preset.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}
