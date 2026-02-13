//! Module snapshot entity — parameter variations across a module's blocks.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "module_snapshots")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    pub module_preset_id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    pub block_overrides: Json,
    pub is_default: bool,
    pub tags: Json,
    pub created_at: DateTimeWithTimeZone,
    pub updated_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::module_preset_entity::Entity",
        from = "Column::ModulePresetId",
        to = "super::module_preset_entity::Column::Id"
    )]
    ModulePreset,
}

impl Related<super::module_preset_entity::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::ModulePreset.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}
