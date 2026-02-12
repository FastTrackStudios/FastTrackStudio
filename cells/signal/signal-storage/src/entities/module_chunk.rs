//! ModuleChunk entity — reusable module state snapshots.

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "module_chunks")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: Uuid,
    pub preset_id: Option<Uuid>,
    #[sea_orm(column_type = "String(StringLen::N(100))")]
    pub module_type: String,
    #[sea_orm(column_type = "String(StringLen::N(255))")]
    pub name: String,
    pub data: Json,
    pub created_at: DateTimeWithTimeZone,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::preset::Entity",
        from = "Column::PresetId",
        to = "super::preset::Column::Id",
        on_delete = "SetNull"
    )]
    Preset,
}

impl Related<super::preset::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::Preset.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}

// ── Typed accessors ──────────────────────────────────────────────────────────

impl Model {
    /// Parse the `module_type` column into a typed [`ModuleType`].
    ///
    /// Returns `None` if the stored string doesn't match any known variant.
    /// For DB values written by DAW container names, falls back to
    /// [`ModuleType::from_container_name`].
    pub fn module_type_parsed(&self) -> Option<signal_proto::module::ModuleType> {
        signal_proto::module::ModuleType::from_variant_name(&self.module_type)
            .or_else(|| signal_proto::module::ModuleType::from_container_name(&self.module_type))
    }
}
