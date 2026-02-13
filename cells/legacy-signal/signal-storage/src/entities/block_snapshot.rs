//! Block snapshot entity — parameter variations within a block preset.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "block_snapshots")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    pub block_preset_id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    pub parameters: Json,
    #[sea_orm(column_type = "Text")]
    pub daw_chunk_data: Option<String>,
    pub is_default: bool,
    pub created_at: DateTimeWithTimeZone,
    pub updated_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::block_preset::Entity",
        from = "Column::BlockPresetId",
        to = "super::block_preset::Column::Id"
    )]
    BlockPreset,
}

impl Related<super::block_preset::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::BlockPreset.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}
