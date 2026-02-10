//! Profile entity — named rig configurations with scene templates.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "profiles")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    pub rig_id: Uuid,
    #[sea_orm(column_type = "Text")]
    pub description: Option<String>,
    pub tags: Json,
    pub metadata: Json,
    pub default_scene_template_id: Option<Uuid>,
    pub is_deleted: bool,
    pub created_at: DateTimeWithTimeZone,
    pub updated_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(has_many = "super::scene_template::Entity")]
    SceneTemplates,
}

impl Related<super::scene_template::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::SceneTemplates.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}
