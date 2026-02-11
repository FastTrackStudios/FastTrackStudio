//! Module preset entity — composable block groups (e.g., Drive Module = Boost + Drive1 + Drive2).

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "module_presets")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    #[sea_orm(column_type = "String(StringLen::N(64))")]
    pub module_type: String,
    #[sea_orm(column_type = "Text")]
    pub description: Option<String>,
    pub blocks: Json,
    pub macros: Json,
    pub tags: Json,
    pub is_template: bool,
    pub is_deleted: bool,
    pub created_at: DateTimeWithTimeZone,
    pub updated_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(has_many = "super::module_snapshot::Entity")]
    ModuleSnapshots,
}

impl Related<super::module_snapshot::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::ModuleSnapshots.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}
