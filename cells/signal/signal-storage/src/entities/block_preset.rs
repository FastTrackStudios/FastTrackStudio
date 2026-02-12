//! Block preset entity — individual DSP unit library entries (EQ, Drive, Amp, etc.).

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "block_presets")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    #[sea_orm(column_type = "String(StringLen::N(64))")]
    pub block_type: String,
    pub plugin_id: Option<Json>,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub plugin_preset_name: Option<String>,
    #[sea_orm(column_type = "Text")]
    pub description: Option<String>,
    pub tags: Json,
    pub is_template: bool,
    pub is_deleted: bool,
    pub created_at: DateTimeWithTimeZone,
    pub updated_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(has_many = "super::block_snapshot::Entity")]
    BlockSnapshots,
}

impl Related<super::block_snapshot::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::BlockSnapshots.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}

// ── Typed accessors ──────────────────────────────────────────────────────────

impl Model {
    /// Parse the `block_type` column into a typed [`BlockType`].
    ///
    /// Returns `None` if the stored string doesn't match any known variant.
    pub fn block_type_parsed(&self) -> Option<signal_proto::block::BlockType> {
        signal_proto::block::BlockType::from_variant_name(&self.block_type)
    }
}
