//! `Glossary` entity — workflow-agnostic catalog of terms.
//!
//! Stores reference definitions that markdown bodies can link to via
//! Obsidian-style `[[wikilink]]` syntax. Cooking is the first user
//! (recipe steps reference `[[simmer]]`, `[[deglaze]]`, ...) but the
//! shape is intentionally domain-neutral: a `category` field tags the
//! domain ("cooking", "audio-production", "fitness", ...) and the
//! `[[wikilink]]` resolver in [`crate::glossary::wikilinks`] accepts
//! a category filter so a recipe step won't incorrectly resolve
//! `[[mastering]]` to the audio-production term.

use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use sea_orm::sea_query::{ArrayType, ColumnType, Nullable, Value, ValueType, ValueTypeErr};
use sea_orm::{ColIdx, QueryResult, TryGetError, TryGetable};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use utoipa::ToSchema;

/// Local mirror of the `json_vec_type!` macro. Kept private to this
/// module per repo convention (the macro lives inline in
/// `crate::asset::model` / `crate::location::model` etc. and isn't
/// exposed publicly).
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

json_vec_type!(GlossaryAliasList, String);
json_vec_type!(GlossaryRelatedList, Uuid);

#[derive(
    Clone,
    Debug,
    Default,
    PartialEq,
    Facet,
    DeriveEntityModel,
    EntityToModels,
    Serialize,
    Deserialize,
)]
#[sea_orm(table_name = "glossary_terms")]
#[crudcrate(
    api_struct = "GlossaryTermApi",
    generate_vox_service,
    name_singular = "glossary_term",
    name_plural = "glossary_terms"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Canonical display name ("Simmer", "Mise en Place").
    #[crudcrate(filterable, fulltext)]
    pub name: String,

    /// URL-friendly slug; unique within (organization, slug).
    /// Auto-derived from name when not provided ("Simmer" -> "simmer").
    /// Used by the [[wikilink]] resolver as the default lookup key.
    #[crudcrate(filterable)]
    pub slug: String,

    /// The term's full explanation; rendered as markdown.
    pub body_markdown: String,

    /// Lower-cased alternative names that also resolve to this term
    /// during [[wikilink]] resolution and search ("simmering" ->
    /// "simmer"). Stored as a JSON array.
    #[sea_orm(column_type = "Json")]
    pub aliases: GlossaryAliasList,

    /// Tag for the term's domain. Free-form: "cooking" /
    /// "audio-production" / "fitness" / "video-production" /
    /// "general". Used to scope the resolver when callers know the
    /// domain (a recipe step shouldn't resolve to "mastering" the
    /// audio term).
    #[crudcrate(filterable)]
    pub category: String,

    /// Cross-references to related terms (other Glossary rows).
    #[sea_orm(column_type = "Json")]
    pub related_term_ids: GlossaryRelatedList,

    /// Owning organization. None = global (every org sees this term).
    #[crudcrate(filterable)]
    pub organization: Option<String>,

    pub created_by: Option<String>,

    /// Polymorphic Obsidian-style properties.
    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub properties: crate::property::JsonObject,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,
    #[crudcrate(exclude(create), exclude(update), on_create = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {
    fn new() -> Self {
        Self {
            aliases: sea_orm::ActiveValue::Set(GlossaryAliasList::default()),
            related_term_ids: sea_orm::ActiveValue::Set(GlossaryRelatedList::default()),
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

pub type GlossaryTerm = Model;

/// Derive a URL-friendly slug from a name. Lowercases ASCII, collapses
/// runs of non-alphanumerics to a single hyphen, trims leading/trailing
/// hyphens. Non-ASCII characters are preserved lowercased so terms
/// like "sauté" → "sauté" (the wikilink parser also normalizes via
/// the same routine — see [`crate::glossary::wikilinks::slugify`]).
#[must_use]
pub fn slugify(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut last_dash = true; // suppress leading dashes
    for ch in input.chars() {
        if ch.is_alphanumeric() {
            for lc in ch.to_lowercase() {
                out.push(lc);
            }
            last_dash = false;
        } else if !last_dash {
            out.push('-');
            last_dash = true;
        }
    }
    while out.ends_with('-') {
        out.pop();
    }
    out
}

#[cfg(test)]
mod tests {
    use super::slugify;

    #[test]
    fn slugify_basic() {
        assert_eq!(slugify("Simmer"), "simmer");
        assert_eq!(slugify("Mise en Place"), "mise-en-place");
        assert_eq!(slugify("  multiple   spaces  "), "multiple-spaces");
        assert_eq!(slugify("Gain Staging"), "gain-staging");
        assert_eq!(slugify("comp/track"), "comp-track");
        assert_eq!(slugify(""), "");
    }
}
