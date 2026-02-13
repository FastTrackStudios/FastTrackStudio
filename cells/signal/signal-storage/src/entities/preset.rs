//! Preset entity — stores complete rig presets with metadata.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "presets")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    #[sea_orm(column_type = "Text", nullable)]
    pub description: Option<String>,
    pub author_id: Option<Uuid>,
    pub category: Json,
    pub tags: Json,
    pub data: Json,
    pub is_public: bool,
    pub instrument_type: String,
    pub is_deleted: bool,
    pub is_favorite: bool,
    pub is_template: bool,
    pub version: i64,
    pub created_at: DateTimeWithTimeZone,
    pub updated_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::user::Entity",
        from = "Column::AuthorId",
        to = "super::user::Column::Id"
    )]
    Author,
    #[sea_orm(has_many = "super::snapshot::Entity")]
    Snapshots,
    #[sea_orm(has_many = "super::module_chunk::Entity")]
    ModuleChunks,
    #[sea_orm(has_many = "super::rating::Entity")]
    Ratings,
    #[sea_orm(has_many = "super::preset_version::Entity")]
    Versions,
}

impl Related<super::user::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::Author.def()
    }
}

impl Related<super::snapshot::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::Snapshots.def()
    }
}

impl Related<super::module_chunk::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::ModuleChunks.def()
    }
}

impl Related<super::rating::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::Ratings.def()
    }
}

impl Related<super::preset_version::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::Versions.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}

// ── Typed accessors ──────────────────────────────────────────────────────────

impl Model {
    /// Parse the `category` JSON column into a typed [`PresetCategory`].
    ///
    /// Uses the Facet bridge for deserialization. Returns `PresetCategory::default()`
    /// if the JSON is malformed or missing.
    pub fn category_parsed(&self) -> signal_proto::category::PresetCategory {
        crate::facet_bridge::from_json_value(&self.category).unwrap_or_default()
    }
}
