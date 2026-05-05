use chrono::{DateTime, NaiveDate, Utc};
use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use sea_orm::sea_query::{ArrayType, ColumnType, Nullable, Value, ValueType, ValueTypeErr};
use sea_orm::{ColIdx, QueryResult, TryGetError, TryGetable};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use utoipa::ToSchema;
use uuid::Uuid;

use crate::{CalendarEvent, Project, Task};

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

#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
pub struct ProviderRef {
    pub provider: String,
    pub account: Option<String>,
    pub collection: Option<String>,
    pub href: Option<String>,
    pub etag: Option<String>,
    pub uid: Option<String>,
}

json_vec_type!(ProviderRefList, ProviderRef);

#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
pub struct ContactMethod {
    pub kind: String,
    pub value: String,
    pub label: Option<String>,
    pub primary: bool,
}

json_vec_type!(ContactMethodList, ContactMethod);
json_vec_type!(PersonNameList, String);

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
#[sea_orm(table_name = "people")]
#[crudcrate(
    api_struct = "PersonApi",
    generate_vox_service,
    name_singular = "person",
    name_plural = "people"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(
        primary_key,
        exclude(create),
        on_create = uuid::Uuid::new_v4()
    )]
    pub uuid: Uuid,

    #[crudcrate(filterable, sortable)]
    pub id: Option<String>,
    #[crudcrate(filterable, sortable, fulltext)]
    pub display_name: String,
    #[crudcrate(filterable, sortable, fulltext)]
    pub given_name: Option<String>,
    #[crudcrate(filterable, sortable, fulltext)]
    pub family_name: Option<String>,
    #[crudcrate(filterable, sortable)]
    pub organization: Option<String>,
    #[crudcrate(filterable, sortable)]
    pub title: Option<String>,
    #[facet(default)]
    pub contact_methods: ContactMethodList,
    #[facet(default)]
    pub provider_refs: ProviderRefList,
    #[crudcrate(fulltext)]
    pub notes: Option<String>,
    #[crudcrate(filterable, sortable)]
    pub follow_up_on: Option<NaiveDate>,
    #[crudcrate(sortable)]
    pub last_contacted_at: Option<DateTime<Utc>>,
}

pub type Person = Model;

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct OrganizationRecord {
    pub id: Option<String>,
    pub name: String,
    #[facet(default)]
    pub people: PersonNameList,
    #[facet(default)]
    pub contact_methods: ContactMethodList,
    #[facet(default)]
    pub provider_refs: ProviderRefList,
    pub notes: Option<String>,
    pub follow_up_on: Option<NaiveDate>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Relationship {
    pub from: String,
    pub to: String,
    pub kind: String,
    pub since: Option<NaiveDate>,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct CommunicationRef {
    pub kind: String,
    pub external_id: String,
    pub summary: Option<String>,
    pub occurred_at: Option<DateTime<Utc>>,
    pub provider: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Facet)]
pub struct ProviderConflictField {
    pub field: String,
    pub local_value: Option<String>,
    pub remote_value: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Facet)]
pub struct ProviderConflict {
    pub entity_type: String,
    pub entity_id: String,
    pub provider: String,
    pub account: Option<String>,
    pub collection: Option<String>,
    pub href: Option<String>,
    pub uid: Option<String>,
    pub local_etag: Option<String>,
    pub remote_etag: Option<String>,
    #[facet(default)]
    pub fields: Vec<ProviderConflictField>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct PersonContext {
    pub person: Person,
    #[facet(default)]
    pub tasks: Vec<Task>,
    #[facet(default)]
    pub projects: Vec<Project>,
    #[facet(default)]
    pub calendar_events: Vec<CalendarEvent>,
    #[facet(default)]
    pub communications: Vec<CommunicationRef>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct OrganizationContext {
    pub organization: OrganizationRecord,
    #[facet(default)]
    pub people: Vec<Person>,
    #[facet(default)]
    pub tasks: Vec<Task>,
    #[facet(default)]
    pub projects: Vec<Project>,
    #[facet(default)]
    pub calendar_events: Vec<CalendarEvent>,
    #[facet(default)]
    pub communications: Vec<CommunicationRef>,
}
