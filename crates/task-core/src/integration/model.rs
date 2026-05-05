// r[impl integration.framework]
//! Integration configuration framework.
//!
//! Integrations are declared as TOML files at:
//!   `.config/task/integrations/<name>.toml`
//!
//! Each file describes a named bundle of status definitions, project/task
//! templates, and area/context conventions for a specific domain.

use std::path::Path;

use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use sea_orm::sea_query::{ArrayType, ColumnType, Nullable, Value, ValueType, ValueTypeErr};
use sea_orm::{ColIdx, QueryResult, TryGetError, TryGetable};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use utoipa::ToSchema;
use uuid::Uuid;

macro_rules! json_vec_type {
    ($name:ident, $item:ty) => {
        #[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
        #[facet(transparent)]
        #[serde(transparent)]
        pub struct $name(pub Vec<$item>);

        impl Default for $name {
            fn default() -> Self {
                Self(Vec::new())
            }
        }

        impl Deref for $name {
            type Target = Vec<$item>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }

        impl From<Vec<$item>> for $name {
            fn from(value: Vec<$item>) -> Self {
                Self(value)
            }
        }

        impl IntoIterator for $name {
            type Item = $item;
            type IntoIter = std::vec::IntoIter<$item>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.into_iter()
            }
        }

        impl<'a> IntoIterator for &'a $name {
            type Item = &'a $item;
            type IntoIter = std::slice::Iter<'a, $item>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.iter()
            }
        }

        impl From<$name> for Value {
            fn from(value: $name) -> Self {
                Value::Json(Some(Box::new(
                    serde_json::to_value(value.0).unwrap_or(serde_json::Value::Array(Vec::new())),
                )))
            }
        }

        impl Nullable for $name {
            fn null() -> Value {
                Value::Json(None)
            }
        }

        impl TryGetable for $name {
            fn try_get_by<I: ColIdx>(res: &QueryResult, idx: I) -> Result<Self, TryGetError> {
                let value: serde_json::Value = res.try_get_by(idx)?;
                let items = serde_json::from_value(value).map_err(|err| {
                    TryGetError::DbErr(sea_orm::DbErr::Type(format!(
                        "failed to deserialize JSON array: {err}"
                    )))
                })?;
                Ok(Self(items))
            }
        }

        impl ValueType for $name {
            fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
                match value {
                    Value::Json(Some(value)) => serde_json::from_value(*value)
                        .map(Self)
                        .map_err(|_| ValueTypeErr),
                    _ => Err(ValueTypeErr),
                }
            }

            fn type_name() -> String {
                stringify!($name).to_string()
            }

            fn array_type() -> ArrayType {
                ArrayType::Json
            }

            fn column_type() -> ColumnType {
                ColumnType::Json
            }
        }
    };
}

// r[impl integration.framework]
/// A named integration bundle loaded from a TOML config file.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Default,
    Facet,
    DeriveEntityModel,
    EntityToModels,
    Serialize,
    Deserialize,
    ToSchema,
)]
#[sea_orm(table_name = "integrations")]
#[crudcrate(
    api_struct = "IntegrationApi",
    generate_vox_service,
    name_singular = "integration",
    name_plural = "integrations"
)]
pub struct Model {
    #[facet(default)]
    #[serde(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(
        primary_key,
        exclude(create),
        on_create = uuid::Uuid::new_v4()
    )]
    pub id: Uuid,

    #[crudcrate(filterable, sortable, fulltext)]
    pub name: String,
    /// Custom status definitions for this domain.
    #[serde(default)]
    #[facet(default)]
    pub statuses: StatusDefList,
    /// Named project templates with pre-defined task scaffolding.
    #[serde(default)]
    #[facet(default)]
    pub project_templates: ProjectTemplateList,
    /// Standalone task templates.
    #[serde(default)]
    #[facet(default)]
    pub task_templates: TaskTemplateList,
    /// Suggested area WikiLink names for this domain.
    #[serde(default)]
    #[facet(default)]
    pub area_conventions: IntegrationStringList,
    /// Suggested context names for this domain.
    #[serde(default)]
    #[facet(default)]
    pub context_conventions: IntegrationStringList,
}

pub type Integration = Model;

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}

/// A single status definition for a domain integration.
// r[impl integration.status-set]
#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
pub struct StatusDef {
    pub name: String,
    /// Whether completing a task with this status marks it as done.
    #[serde(default)]
    pub is_completion: bool,
    /// Optional display colour (CSS hex or named colour).
    pub color: Option<String>,
}

json_vec_type!(StatusDefList, StatusDef);

/// A named project template that scaffolds tasks on creation.
// r[impl integration.project-template]
#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
pub struct ProjectTemplate {
    pub name: String,
    pub description: Option<String>,
    /// Tasks to create automatically when a project is instantiated.
    #[serde(default)]
    #[facet(default)]
    pub tasks: TaskTemplateList,
}

json_vec_type!(ProjectTemplateList, ProjectTemplate);

/// A task template with optional `{{variable}}` placeholders in text fields.
// r[impl integration.task-template]
#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
pub struct TaskTemplate {
    pub title: String,
    /// Status name string (may be an integration-specific status).
    pub status: Option<String>,
    /// Priority string: none / low / normal / high / urgent.
    pub priority: Option<String>,
    #[serde(default)]
    #[facet(default)]
    pub contexts: IntegrationStringList,
    #[serde(default)]
    #[facet(default)]
    pub tags: IntegrationStringList,
    /// RFC 5545 RRULE string.
    pub recurrence: Option<String>,
    pub time_estimate_minutes: Option<u32>,
    pub body: Option<String>,
}

json_vec_type!(TaskTemplateList, TaskTemplate);
json_vec_type!(IntegrationStringList, String);

pub trait IntegrationSource {
    fn get_integration(&self, name: &str) -> Option<Integration>;
    fn list_integrations(&self) -> Vec<Integration>;
}

#[derive(Debug, Clone)]
pub struct FsIntegrationSource {
    config_dir: std::path::PathBuf,
}

impl FsIntegrationSource {
    pub fn new(config_dir: impl Into<std::path::PathBuf>) -> Self {
        Self {
            config_dir: config_dir.into(),
        }
    }
}

impl IntegrationSource for FsIntegrationSource {
    fn get_integration(&self, name: &str) -> Option<Integration> {
        self.list_integrations()
            .into_iter()
            .find(|integration| integration.name.eq_ignore_ascii_case(name))
    }

    fn list_integrations(&self) -> Vec<Integration> {
        list_integrations(&self.config_dir)
    }
}

impl Integration {
    /// Look up a status definition by name (case-insensitive).
    pub fn status(&self, name: &str) -> Option<&StatusDef> {
        self.statuses
            .iter()
            .find(|s| s.name.eq_ignore_ascii_case(name))
    }

    /// Return whether `status_name` is a completion status for this integration.
    pub fn is_completion_status(&self, name: &str) -> bool {
        self.status(name).map(|s| s.is_completion).unwrap_or(false)
    }
}

// r[impl integration.framework]
/// Load a single integration from a TOML file.
pub fn load_integration(path: &Path) -> eyre::Result<Integration> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| eyre::eyre!("Failed to read {}: {e}", path.display()))?;
    let integration: Integration = toml::from_str(&content)
        .map_err(|e| eyre::eyre!("Failed to parse {}: {e}", path.display()))?;
    Ok(integration)
}

// r[impl integration.framework]
/// Load all integrations from `<config_dir>/integrations/*.toml`.
/// Files that fail to parse are silently skipped.
pub fn list_integrations(config_dir: &Path) -> Vec<Integration> {
    let dir = config_dir.join("integrations");
    let entries = match std::fs::read_dir(&dir) {
        Ok(e) => e,
        Err(_) => return vec![],
    };
    entries
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("toml"))
        .filter_map(|e| load_integration(&e.path()).ok())
        .collect()
}
