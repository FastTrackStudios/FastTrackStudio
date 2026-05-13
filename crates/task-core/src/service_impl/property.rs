//! SeaORM-backed [`PropertyService`] implementation.
//!
//! Cross-entity service: rather than thread one repo per owner kind, it
//! holds a raw `DatabaseConnection` and dispatches by `owner_type` string
//! to the matching SQLite table. JSON value queries use SQLite's
//! `json_extract` so `find_by_property` is a single round-trip.
//!
//! TODO: realtime broadcast — property edits don't currently emit
//! `TaskOp::FieldChanged` ops; wire that in when the task-server's task
//! op channel is generalized to non-task entities.

use sea_orm::{
    ActiveModelTrait, ActiveValue, ColumnTrait, ConnectionTrait, DatabaseBackend,
    DatabaseConnection, EntityTrait, FromQueryResult, QueryFilter, Statement, Value as SeaValue,
};
use serde_json::Value as JsonValue;
use uuid::Uuid;

use crate::property::{
    self, PropertyDefinitionEntity, PropertyKind, default_properties_json,
    model::Column as DefColumn,
};
use crate::service::{PropertyDefinitionView, PropertyService, VaultError};

/// Typed dependencies for [`PropertyServiceImpl`].
pub struct PropertyServiceDeps {
    pub db: DatabaseConnection,
}

#[derive(Clone)]
pub struct PropertyServiceImpl {
    db: DatabaseConnection,
}

impl PropertyServiceImpl {
    pub fn new(deps: PropertyServiceDeps) -> Self {
        Self { db: deps.db }
    }
}

/// Maps a service-layer `owner_type` string to its SQLite table + primary
/// key column. The mapping is hard-coded because every supported entity
/// has its own table and SeaORM doesn't give us a uniform key column name
/// (some entities use `id`, some `uuid`).
fn resolve_owner(owner_type: &str) -> Result<(&'static str, &'static str), VaultError> {
    match owner_type {
        "task" => Ok(("tasks", "id")),
        "project" => Ok(("projects", "id")),
        "calendar_event" => Ok(("calendar_events", "uuid")),
        "person" | "people" => Ok(("people", "uuid")),
        "comment" => Ok(("comments", "id")),
        "location" => Ok(("locations", "uuid")),
        other => Err(VaultError::ParseError(format!(
            "unknown property owner_type: {other}"
        ))),
    }
}

fn parse_kind(kind: &str) -> Result<PropertyKind, VaultError> {
    PropertyKind::parse(kind).ok_or_else(|| {
        VaultError::ParseError(format!(
            "unknown property kind '{kind}' (expected text|number|checkbox|date|datetime|list|tags)"
        ))
    })
}

fn parse_json(text: &str) -> Result<JsonValue, VaultError> {
    serde_json::from_str(text).map_err(|err| VaultError::ParseError(format!("invalid JSON: {err}")))
}

fn deterministic_definition_id(name: &str) -> Uuid {
    Uuid::new_v5(
        &Uuid::from_u128(0x70_72_6f_70_64_65_66_70_72_6f_70_64_65_66_70_72),
        format!("property:{name}").as_bytes(),
    )
}

fn view_from_model(m: property::PropertyDefinition) -> PropertyDefinitionView {
    PropertyDefinitionView {
        id: m.id,
        name: m.name,
        kind: m.kind.as_str().to_string(),
        description: m.description,
        options: serde_json::to_string(&m.options).unwrap_or_else(|_| "null".to_string()),
        created_at: m.created_at.to_rfc3339(),
        updated_at: m.updated_at.to_rfc3339(),
    }
}

impl PropertyServiceImpl {
    /// Read the `properties` JSON column for one entity.
    async fn read_properties(
        &self,
        table: &str,
        pk: &str,
        id: Uuid,
    ) -> Result<JsonValue, VaultError> {
        let backend = self.db.get_database_backend();
        let sql = format!("SELECT properties FROM {table} WHERE {pk} = ?");
        let stmt = Statement::from_sql_and_values(backend, &sql, [SeaValue::from(id)]);
        let row = self
            .db
            .query_one(stmt)
            .await
            .map_err(|err| VaultError::IoError(err.to_string()))?
            .ok_or_else(|| VaultError::NotFound(format!("{table}:{id}")))?;
        let raw: String = row
            .try_get_by::<String, _>(0)
            .or_else(|_| row.try_get_by::<String, _>("properties"))
            .map_err(|err| VaultError::ParseError(err.to_string()))?;
        parse_json(&raw)
    }

    /// Write the `properties` JSON column for one entity.
    async fn write_properties(
        &self,
        table: &str,
        pk: &str,
        id: Uuid,
        value: &JsonValue,
    ) -> Result<(), VaultError> {
        let backend = self.db.get_database_backend();
        let serialized =
            serde_json::to_string(value).map_err(|err| VaultError::ParseError(err.to_string()))?;
        let sql = format!("UPDATE {table} SET properties = ? WHERE {pk} = ?");
        let stmt = Statement::from_sql_and_values(
            backend,
            &sql,
            [SeaValue::from(serialized), SeaValue::from(id)],
        );
        let result = self
            .db
            .execute(stmt)
            .await
            .map_err(|err| VaultError::IoError(err.to_string()))?;
        if result.rows_affected() == 0 {
            return Err(VaultError::NotFound(format!("{table}:{id}")));
        }
        Ok(())
    }

    async fn ensure_definition(&self, name: &str) -> Result<(), VaultError> {
        let existing = PropertyDefinitionEntity::find()
            .filter(DefColumn::Name.eq(name))
            .one(&self.db)
            .await
            .map_err(|err| VaultError::IoError(err.to_string()))?;
        if existing.is_some() {
            return Ok(());
        }
        let now = chrono::Utc::now();
        let model = property::model::ActiveModel {
            id: ActiveValue::Set(deterministic_definition_id(name)),
            name: ActiveValue::Set(name.to_string()),
            kind: ActiveValue::Set(PropertyKind::Text),
            description: ActiveValue::Set(None),
            options: ActiveValue::Set(default_properties_json()),
            created_at: ActiveValue::Set(now),
            updated_at: ActiveValue::Set(now),
        };
        model
            .insert(&self.db)
            .await
            .map_err(|err| VaultError::IoError(err.to_string()))?;
        Ok(())
    }
}

#[derive(FromQueryResult)]
struct IdRow {
    id: Uuid,
}

impl PropertyService for PropertyServiceImpl {
    async fn list_definitions(&self) -> Result<Vec<PropertyDefinitionView>, VaultError> {
        let rows = PropertyDefinitionEntity::find()
            .all(&self.db)
            .await
            .map_err(|err| VaultError::IoError(err.to_string()))?;
        Ok(rows.into_iter().map(view_from_model).collect())
    }

    async fn define_property(
        &self,
        name: String,
        kind: String,
        description: Option<String>,
        options: String,
    ) -> Result<PropertyDefinitionView, VaultError> {
        let kind = parse_kind(&kind)?;
        let options_json: JsonValue = if options.is_empty() {
            JsonValue::Object(Default::default())
        } else {
            parse_json(&options)?
        };
        let now = chrono::Utc::now();
        let id = deterministic_definition_id(&name);

        let existing = PropertyDefinitionEntity::find()
            .filter(DefColumn::Name.eq(name.clone()))
            .one(&self.db)
            .await
            .map_err(|err| VaultError::IoError(err.to_string()))?;

        let saved = if let Some(existing) = existing {
            let mut active: property::model::ActiveModel = existing.into();
            active.kind = ActiveValue::Set(kind);
            active.description = ActiveValue::Set(description);
            active.options = ActiveValue::Set(options_json.into());
            active.updated_at = ActiveValue::Set(now);
            active
                .update(&self.db)
                .await
                .map_err(|err| VaultError::IoError(err.to_string()))?
        } else {
            let active = property::model::ActiveModel {
                id: ActiveValue::Set(id),
                name: ActiveValue::Set(name),
                kind: ActiveValue::Set(kind),
                description: ActiveValue::Set(description),
                options: ActiveValue::Set(options_json.into()),
                created_at: ActiveValue::Set(now),
                updated_at: ActiveValue::Set(now),
            };
            active
                .insert(&self.db)
                .await
                .map_err(|err| VaultError::IoError(err.to_string()))?
        };
        Ok(view_from_model(saved))
    }

    async fn delete_definition(&self, name: String) -> Result<(), VaultError> {
        PropertyDefinitionEntity::delete_many()
            .filter(DefColumn::Name.eq(name))
            .exec(&self.db)
            .await
            .map_err(|err| VaultError::IoError(err.to_string()))?;
        Ok(())
    }

    async fn get_properties(
        &self,
        owner_type: String,
        owner_id: Uuid,
    ) -> Result<String, VaultError> {
        let (table, pk) = resolve_owner(&owner_type)?;
        let value = self.read_properties(table, pk, owner_id).await?;
        serde_json::to_string(&value).map_err(|err| VaultError::ParseError(err.to_string()))
    }

    async fn set_property(
        &self,
        owner_type: String,
        owner_id: Uuid,
        name: String,
        value: String,
    ) -> Result<(), VaultError> {
        let (table, pk) = resolve_owner(&owner_type)?;
        let new_value = parse_json(&value)?;
        self.ensure_definition(&name).await?;

        let mut current = self
            .read_properties(table, pk, owner_id)
            .await
            .unwrap_or_else(|_| JsonValue::Object(Default::default()));
        if !current.is_object() {
            current = JsonValue::Object(Default::default());
        }
        if let Some(map) = current.as_object_mut() {
            map.insert(name, new_value);
        }
        self.write_properties(table, pk, owner_id, &current).await
    }

    async fn unset_property(
        &self,
        owner_type: String,
        owner_id: Uuid,
        name: String,
    ) -> Result<(), VaultError> {
        let (table, pk) = resolve_owner(&owner_type)?;
        let mut current = self.read_properties(table, pk, owner_id).await?;
        if let Some(map) = current.as_object_mut() {
            map.remove(&name);
        }
        self.write_properties(table, pk, owner_id, &current).await
    }

    async fn find_by_property(
        &self,
        owner_type: String,
        name: String,
        value: String,
    ) -> Result<Vec<Uuid>, VaultError> {
        let (table, pk) = resolve_owner(&owner_type)?;
        // Validate JSON; we re-serialize through serde to canonicalize.
        let _: JsonValue = parse_json(&value)?;
        // SQLite `json_extract` returns the parsed leaf. We compare via
        // `json(?)` to coerce the input string to a JSON value so number
        // ↔ text comparisons line up.
        let backend = self.db.get_database_backend();
        if backend != DatabaseBackend::Sqlite {
            return Err(VaultError::IoError(
                "find_by_property currently only supports SQLite".to_string(),
            ));
        }
        let path = format!("$.{}", json_path_segment(&name));
        let sql =
            format!("SELECT {pk} AS id FROM {table} WHERE json_extract(properties, ?) = json(?)");
        let stmt = Statement::from_sql_and_values(
            backend,
            &sql,
            [SeaValue::from(path), SeaValue::from(value)],
        );
        let rows = IdRow::find_by_statement(stmt)
            .all(&self.db)
            .await
            .map_err(|err| VaultError::IoError(err.to_string()))?;
        Ok(rows.into_iter().map(|r| r.id).collect())
    }
}

/// Escape a property name for inclusion in a SQLite JSON path.
///
/// Property names are user-controlled, so we wrap them in quotes and
/// escape embedded quotes/backslashes. A name containing a literal `"`
/// would otherwise break out of the path string.
fn json_path_segment(name: &str) -> String {
    let escaped = name.replace('\\', "\\\\").replace('"', "\\\"");
    format!("\"{escaped}\"")
}
