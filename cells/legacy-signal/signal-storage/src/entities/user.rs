//! User entity — tracks preset authors and community members.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "users")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub username: String,
    #[sea_orm(column_type = "String(StringLen::N(255))", unique)]
    pub email: String,
    #[sea_orm(column_type = "String(StringLen::N(255))", nullable)]
    pub display_name: Option<String>,
    #[sea_orm(column_type = "Text", nullable)]
    pub avatar_url: Option<String>,
    pub created_at: DateTimeWithTimeZone,
    pub updated_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(has_many = "super::preset::Entity")]
    Presets,
    #[sea_orm(has_many = "super::rating::Entity")]
    Ratings,
}

impl Related<super::preset::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::Presets.def()
    }
}

impl Related<super::rating::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::Ratings.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}
