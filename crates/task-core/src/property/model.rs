//! `PropertyDefinition` entity — the per-vault registry of property
//! names and their canonical kinds.

use crudcrate::EntityToModels;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, EntityToModels, Serialize, Deserialize)]
#[sea_orm(table_name = "property_definitions")]
#[crudcrate(
    api_struct = "PropertyDefinitionApi",
    generate_vox_service,
    name_singular = "property_definition",
    name_plural = "property_definitions"
)]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable)]
    #[sea_orm(unique)]
    pub name: String,

    #[crudcrate(filterable)]
    pub kind: PropertyKind,

    pub description: Option<String>,

    /// Kind-specific configuration (allowed values, format hints, etc.).
    #[sea_orm(column_type = "Json")]
    #[serde(default)]
    pub options: super::JsonObject,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,
    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

pub type PropertyDefinition = Model;

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {
    fn new() -> Self {
        Self {
            options: sea_orm::ActiveValue::Set(super::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

/// Canonical Obsidian-frontmatter property kinds.
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    DeriveActiveEnum,
    EnumIter,
    utoipa::ToSchema,
    facet::Facet,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(16))")]
#[repr(u8)]
pub enum PropertyKind {
    #[default]
    #[sea_orm(string_value = "text")]
    Text,
    #[sea_orm(string_value = "number")]
    Number,
    #[sea_orm(string_value = "checkbox")]
    Checkbox,
    #[sea_orm(string_value = "date")]
    Date,
    #[sea_orm(string_value = "datetime")]
    Datetime,
    #[sea_orm(string_value = "list")]
    List,
    #[sea_orm(string_value = "tags")]
    Tags,
}

impl PropertyKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            PropertyKind::Text => "text",
            PropertyKind::Number => "number",
            PropertyKind::Checkbox => "checkbox",
            PropertyKind::Date => "date",
            PropertyKind::Datetime => "datetime",
            PropertyKind::List => "list",
            PropertyKind::Tags => "tags",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s.to_ascii_lowercase().as_str() {
            "text" | "string" => Some(PropertyKind::Text),
            "number" | "num" | "int" | "float" => Some(PropertyKind::Number),
            "checkbox" | "bool" | "boolean" => Some(PropertyKind::Checkbox),
            "date" => Some(PropertyKind::Date),
            "datetime" => Some(PropertyKind::Datetime),
            "list" => Some(PropertyKind::List),
            "tags" | "tag" => Some(PropertyKind::Tags),
            _ => None,
        }
    }
}
