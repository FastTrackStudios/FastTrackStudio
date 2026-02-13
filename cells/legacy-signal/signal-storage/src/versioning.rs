//! Preset versioning service — create, list, restore, and diff preset versions.
//!
//! Provides version history management with:
//! - Auto-creation on save with debouncing (minimum interval between versions)
//! - Version limit per preset (keeps last N versions, default 50)
//! - Version diff computation (JSON-level change detection)
//! - Restore to any previous version

use chrono::Utc;
use sea_orm::{
    ActiveModelTrait, ColumnTrait, DatabaseConnection, EntityTrait, IntoActiveModel, Order,
    PaginatorTrait, QueryFilter, QueryOrder, Set,
};
use serde_json::Value as JsonValue;
use uuid::Uuid;

use crate::entities::preset_version::{self, Entity as PresetVersionEntity};
use crate::{StorageError, StorageResult};

// region: --- Constants

/// Default maximum number of versions to keep per preset.
pub const DEFAULT_MAX_VERSIONS: u64 = 50;

/// Default minimum seconds between auto-created versions (debounce).
pub const DEFAULT_DEBOUNCE_SECS: i64 = 30;

// endregion: --- Constants

// region: --- VersionInfo

/// Summary of a preset version (returned by list operations).
#[derive(Debug, Clone, PartialEq)]
pub struct VersionInfo {
    pub id: Uuid,
    pub preset_id: Uuid,
    pub version: i64,
    pub change_summary: Option<String>,
    pub created_by: Option<Uuid>,
    pub created_at: chrono::DateTime<Utc>,
}

impl From<preset_version::Model> for VersionInfo {
    fn from(m: preset_version::Model) -> Self {
        Self {
            id: m.id,
            preset_id: m.preset_id,
            version: m.version,
            change_summary: m.change_summary,
            created_by: m.created_by,
            created_at: m.created_at.with_timezone(&Utc),
        }
    }
}

// endregion: --- VersionInfo

// region: --- VersionDiff

/// Represents a diff between two preset versions.
#[derive(Debug, Clone, PartialEq)]
pub struct VersionDiff {
    pub from_version: i64,
    pub to_version: i64,
    pub changes: Vec<FieldChange>,
}

/// A single field-level change between two versions.
#[derive(Debug, Clone, PartialEq)]
pub struct FieldChange {
    /// Dot-separated JSON path (e.g., "snapshots.0.name").
    pub path: String,
    pub kind: ChangeKind,
}

/// The type of change detected.
#[derive(Debug, Clone, PartialEq)]
pub enum ChangeKind {
    Added { value: JsonValue },
    Removed { value: JsonValue },
    Modified { old: JsonValue, new: JsonValue },
}

// endregion: --- VersionDiff

// region: --- VersioningService

/// Service for managing preset version history.
pub struct VersioningService {
    db: DatabaseConnection,
    max_versions: u64,
    debounce_secs: i64,
}

impl VersioningService {
    /// Create a new versioning service with default settings.
    pub fn new(db: DatabaseConnection) -> Self {
        Self {
            db,
            max_versions: DEFAULT_MAX_VERSIONS,
            debounce_secs: DEFAULT_DEBOUNCE_SECS,
        }
    }

    /// Set maximum number of versions kept per preset.
    #[must_use]
    pub fn with_max_versions(mut self, max: u64) -> Self {
        self.max_versions = max;
        self
    }

    /// Set minimum seconds between auto-created versions.
    #[must_use]
    pub fn with_debounce_secs(mut self, secs: i64) -> Self {
        self.debounce_secs = secs;
        self
    }

    /// Create a new version for a preset. Returns `None` if debounced (too recent).
    ///
    /// The `data` should be the full serialized preset state at this point in time.
    pub async fn create_version(
        &self,
        preset_id: Uuid,
        data: JsonValue,
        message: Option<String>,
        created_by: Option<Uuid>,
    ) -> StorageResult<Option<VersionInfo>> {
        // -- Debounce check: skip if last version is too recent
        if self.debounce_secs > 0 {
            if let Some(latest) = self.latest_version(preset_id).await? {
                let elapsed = Utc::now() - latest.created_at;
                if elapsed.num_seconds() < self.debounce_secs {
                    return Ok(None);
                }
            }
        }

        // -- Determine next version number
        let next_version = self.next_version_number(preset_id).await?;

        // -- Insert new version
        let now = Utc::now();
        let model = preset_version::ActiveModel {
            id: Set(Uuid::new_v4()),
            preset_id: Set(preset_id),
            version: Set(next_version),
            data: Set(data),
            change_summary: Set(message),
            created_by: Set(created_by),
            created_at: Set(now.into()),
        };
        let inserted = model.insert(&self.db).await?;
        let info = VersionInfo::from(inserted);

        // -- Prune old versions (keep last N)
        self.prune_versions(preset_id).await?;

        Ok(Some(info))
    }

    /// Force-create a version bypassing debounce. Always creates.
    pub async fn create_version_force(
        &self,
        preset_id: Uuid,
        data: JsonValue,
        message: Option<String>,
        created_by: Option<Uuid>,
    ) -> StorageResult<VersionInfo> {
        let next_version = self.next_version_number(preset_id).await?;
        let now = Utc::now();
        let model = preset_version::ActiveModel {
            id: Set(Uuid::new_v4()),
            preset_id: Set(preset_id),
            version: Set(next_version),
            data: Set(data),
            change_summary: Set(message),
            created_by: Set(created_by),
            created_at: Set(now.into()),
        };
        let inserted = model.insert(&self.db).await?;
        let info = VersionInfo::from(inserted);

        self.prune_versions(preset_id).await?;

        Ok(info)
    }

    /// List all versions for a preset, ordered by version number descending (newest first).
    pub async fn list_versions(&self, preset_id: Uuid) -> StorageResult<Vec<VersionInfo>> {
        let models = PresetVersionEntity::find()
            .filter(preset_version::Column::PresetId.eq(preset_id))
            .order_by(preset_version::Column::Version, Order::Desc)
            .all(&self.db)
            .await?;

        Ok(models.into_iter().map(VersionInfo::from).collect())
    }

    /// Get a specific version by preset ID and version number.
    pub async fn get_version(
        &self,
        preset_id: Uuid,
        version: i64,
    ) -> StorageResult<preset_version::Model> {
        PresetVersionEntity::find()
            .filter(preset_version::Column::PresetId.eq(preset_id))
            .filter(preset_version::Column::Version.eq(version))
            .one(&self.db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "preset_version",
                id: preset_id,
            })
    }

    /// Restore a preset to a previous version. Creates a new version with the restored data
    /// and returns the version info. The `message` defaults to "Restored from version N".
    pub async fn restore_version(
        &self,
        preset_id: Uuid,
        version: i64,
        restored_by: Option<Uuid>,
    ) -> StorageResult<VersionInfo> {
        let source = self.get_version(preset_id, version).await?;
        let message = format!("Restored from version {version}");

        self.create_version_force(preset_id, source.data, Some(message), restored_by)
            .await
    }

    /// Compute a diff between two versions of a preset.
    pub async fn diff_versions(
        &self,
        preset_id: Uuid,
        from_version: i64,
        to_version: i64,
    ) -> StorageResult<VersionDiff> {
        let from = self.get_version(preset_id, from_version).await?;
        let to = self.get_version(preset_id, to_version).await?;

        let changes = diff_json("", &from.data, &to.data);

        Ok(VersionDiff {
            from_version,
            to_version,
            changes,
        })
    }

    /// Get the most recent version for a preset, if any.
    pub async fn latest_version(&self, preset_id: Uuid) -> StorageResult<Option<VersionInfo>> {
        let model = PresetVersionEntity::find()
            .filter(preset_version::Column::PresetId.eq(preset_id))
            .order_by(preset_version::Column::Version, Order::Desc)
            .one(&self.db)
            .await?;

        Ok(model.map(VersionInfo::from))
    }

    /// Get the total number of versions for a preset.
    pub async fn version_count(&self, preset_id: Uuid) -> StorageResult<u64> {
        let count = PresetVersionEntity::find()
            .filter(preset_version::Column::PresetId.eq(preset_id))
            .count(&self.db)
            .await?;
        Ok(count)
    }

    // region: --- Private Helpers

    /// Determine the next version number for a preset.
    async fn next_version_number(&self, preset_id: Uuid) -> StorageResult<i64> {
        let latest = PresetVersionEntity::find()
            .filter(preset_version::Column::PresetId.eq(preset_id))
            .order_by(preset_version::Column::Version, Order::Desc)
            .one(&self.db)
            .await?;

        Ok(latest.map_or(1, |m| m.version + 1))
    }

    /// Remove oldest versions beyond the max limit for a preset.
    async fn prune_versions(&self, preset_id: Uuid) -> StorageResult<()> {
        let count = self.version_count(preset_id).await?;
        if count <= self.max_versions {
            return Ok(());
        }

        let to_remove = count - self.max_versions;

        // Find the oldest versions to delete
        let oldest = PresetVersionEntity::find()
            .filter(preset_version::Column::PresetId.eq(preset_id))
            .order_by(preset_version::Column::Version, Order::Asc)
            .all(&self.db)
            .await?;

        for model in oldest.into_iter().take(to_remove as usize) {
            model.into_active_model().delete(&self.db).await?;
        }

        Ok(())
    }

    // endregion: --- Private Helpers
}

// endregion: --- VersioningService

// region: --- JSON Diff

/// Recursively diff two JSON values, producing a list of field changes.
fn diff_json(path: &str, old: &JsonValue, new: &JsonValue) -> Vec<FieldChange> {
    let mut changes = Vec::new();

    match (old, new) {
        // Both are objects — compare keys
        (JsonValue::Object(old_map), JsonValue::Object(new_map)) => {
            for (key, old_val) in old_map {
                let child_path = if path.is_empty() {
                    key.clone()
                } else {
                    format!("{path}.{key}")
                };
                match new_map.get(key) {
                    Some(new_val) => {
                        changes.extend(diff_json(&child_path, old_val, new_val));
                    }
                    None => {
                        changes.push(FieldChange {
                            path: child_path,
                            kind: ChangeKind::Removed {
                                value: old_val.clone(),
                            },
                        });
                    }
                }
            }
            for (key, new_val) in new_map {
                if !old_map.contains_key(key) {
                    let child_path = if path.is_empty() {
                        key.clone()
                    } else {
                        format!("{path}.{key}")
                    };
                    changes.push(FieldChange {
                        path: child_path,
                        kind: ChangeKind::Added {
                            value: new_val.clone(),
                        },
                    });
                }
            }
        }
        // Both are arrays — compare element-by-element
        (JsonValue::Array(old_arr), JsonValue::Array(new_arr)) => {
            let max_len = old_arr.len().max(new_arr.len());
            for i in 0..max_len {
                let child_path = if path.is_empty() {
                    format!("{i}")
                } else {
                    format!("{path}.{i}")
                };
                match (old_arr.get(i), new_arr.get(i)) {
                    (Some(old_val), Some(new_val)) => {
                        changes.extend(diff_json(&child_path, old_val, new_val));
                    }
                    (Some(old_val), None) => {
                        changes.push(FieldChange {
                            path: child_path,
                            kind: ChangeKind::Removed {
                                value: old_val.clone(),
                            },
                        });
                    }
                    (None, Some(new_val)) => {
                        changes.push(FieldChange {
                            path: child_path,
                            kind: ChangeKind::Added {
                                value: new_val.clone(),
                            },
                        });
                    }
                    (None, None) => unreachable!(),
                }
            }
        }
        // Leaf values — compare directly
        _ => {
            if old != new {
                let display_path = if path.is_empty() {
                    "(root)".to_string()
                } else {
                    path.to_string()
                };
                changes.push(FieldChange {
                    path: display_path,
                    kind: ChangeKind::Modified {
                        old: old.clone(),
                        new: new.clone(),
                    },
                });
            }
        }
    }

    changes
}

// endregion: --- JSON Diff

// region: --- Tests

#[cfg(test)]
mod tests {
    use super::*;
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;
    use serde_json::json;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    /// Helper: set up an in-memory SQLite database with migrations applied.
    async fn setup_db() -> Result<DatabaseConnection> {
        let db = Database::connect("sqlite::memory:").await?;
        crate::Migrator::up(&db, None).await?;
        Ok(db)
    }

    /// Helper: insert a dummy preset and return its ID.
    async fn insert_preset(db: &DatabaseConnection) -> Result<Uuid> {
        use crate::entities::preset;
        let id = Uuid::new_v4();
        let now = Utc::now();
        let model = preset::ActiveModel {
            id: Set(id),
            name: Set("Test Preset".to_string()),
            description: Set(None),
            author_id: Set(None),
            category: Set(json!({"name": "guitar"})),
            tags: Set(json!([])),
            data: Set(json!({"blocks": []})),
            is_public: Set(false),
            is_deleted: Set(false),
            is_favorite: Set(false),
            is_template: Set(false),
            version: Set(1),
            created_at: Set(now.into()),
            updated_at: Set(now.into()),
        };
        model.insert(db).await?;
        Ok(id)
    }

    // -- Diff unit tests (pure functions, no DB needed)

    #[test]
    fn test_diff_json_no_changes() {
        // -- Setup & Fixtures
        let a = json!({"name": "Lead", "gain": 0.7});
        let b = json!({"name": "Lead", "gain": 0.7});

        // -- Exec
        let changes = diff_json("", &a, &b);

        // -- Check
        assert!(changes.is_empty());
    }

    #[test]
    fn test_diff_json_modified_field() {
        // -- Setup & Fixtures
        let a = json!({"name": "Lead", "gain": 0.7});
        let b = json!({"name": "Lead", "gain": 0.9});

        // -- Exec
        let changes = diff_json("", &a, &b);

        // -- Check
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "gain");
        assert!(matches!(
            &changes[0].kind,
            ChangeKind::Modified { old, new }
            if *old == json!(0.7) && *new == json!(0.9)
        ));
    }

    #[test]
    fn test_diff_json_added_field() {
        // -- Setup & Fixtures
        let a = json!({"name": "Lead"});
        let b = json!({"name": "Lead", "gain": 0.5});

        // -- Exec
        let changes = diff_json("", &a, &b);

        // -- Check
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "gain");
        assert!(matches!(&changes[0].kind, ChangeKind::Added { value } if *value == json!(0.5)));
    }

    #[test]
    fn test_diff_json_removed_field() {
        // -- Setup & Fixtures
        let a = json!({"name": "Lead", "gain": 0.5});
        let b = json!({"name": "Lead"});

        // -- Exec
        let changes = diff_json("", &a, &b);

        // -- Check
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "gain");
        assert!(matches!(
            &changes[0].kind,
            ChangeKind::Removed { value } if *value == json!(0.5)
        ));
    }

    #[test]
    fn test_diff_json_nested_changes() {
        // -- Setup & Fixtures
        let a = json!({"blocks": {"amp": {"gain": 0.5, "volume": 0.8}}});
        let b = json!({"blocks": {"amp": {"gain": 0.7, "volume": 0.8}}});

        // -- Exec
        let changes = diff_json("", &a, &b);

        // -- Check
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "blocks.amp.gain");
    }

    #[test]
    fn test_diff_json_array_element_modified() {
        // -- Setup & Fixtures
        let a = json!({"params": [1, 2, 3]});
        let b = json!({"params": [1, 9, 3]});

        // -- Exec
        let changes = diff_json("", &a, &b);

        // -- Check
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "params.1");
    }

    #[test]
    fn test_diff_json_array_length_change() {
        // -- Setup & Fixtures
        let a = json!({"items": [1, 2]});
        let b = json!({"items": [1, 2, 3]});

        // -- Exec
        let changes = diff_json("", &a, &b);

        // -- Check
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "items.2");
        assert!(matches!(&changes[0].kind, ChangeKind::Added { value } if *value == json!(3)));
    }

    #[test]
    fn test_diff_json_type_change() {
        // -- Setup & Fixtures
        let a = json!({"value": "hello"});
        let b = json!({"value": 42});

        // -- Exec
        let changes = diff_json("", &a, &b);

        // -- Check
        assert_eq!(changes.len(), 1);
        assert!(matches!(
            &changes[0].kind,
            ChangeKind::Modified { old, new }
            if *old == json!("hello") && *new == json!(42)
        ));
    }

    // -- Service integration tests (require in-memory DB)

    #[tokio::test]
    async fn test_versioning_create_version() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(0);

        // -- Exec
        let result = svc
            .create_version(
                preset_id,
                json!({"gain": 0.5}),
                Some("Initial".into()),
                None,
            )
            .await?;

        // -- Check
        let info = result.expect("should not be debounced");
        assert_eq!(info.version, 1);
        assert_eq!(info.preset_id, preset_id);
        assert_eq!(info.change_summary.as_deref(), Some("Initial"));
        Ok(())
    }

    #[tokio::test]
    async fn test_versioning_auto_increment() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(0);

        // -- Exec
        svc.create_version(preset_id, json!({"v": 1}), None, None)
            .await?;
        svc.create_version(preset_id, json!({"v": 2}), None, None)
            .await?;
        let v3 = svc
            .create_version(preset_id, json!({"v": 3}), None, None)
            .await?;

        // -- Check
        assert_eq!(v3.unwrap().version, 3);
        Ok(())
    }

    #[tokio::test]
    async fn test_versioning_debounce() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(60);

        // -- Exec: first version always succeeds
        let v1 = svc
            .create_version(preset_id, json!({"v": 1}), None, None)
            .await?;
        // Second within debounce window should be None
        let v2 = svc
            .create_version(preset_id, json!({"v": 2}), None, None)
            .await?;

        // -- Check
        assert!(v1.is_some());
        assert!(v2.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn test_versioning_force_bypasses_debounce() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(60);

        // -- Exec
        svc.create_version_force(preset_id, json!({"v": 1}), None, None)
            .await?;
        let v2 = svc
            .create_version_force(preset_id, json!({"v": 2}), None, None)
            .await?;

        // -- Check: force always creates
        assert_eq!(v2.version, 2);
        Ok(())
    }

    #[tokio::test]
    async fn test_versioning_list_versions() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(0);

        svc.create_version(preset_id, json!({"v": 1}), Some("First".into()), None)
            .await?;
        svc.create_version(preset_id, json!({"v": 2}), Some("Second".into()), None)
            .await?;

        // -- Exec
        let versions = svc.list_versions(preset_id).await?;

        // -- Check: newest first
        assert_eq!(versions.len(), 2);
        assert_eq!(versions[0].version, 2);
        assert_eq!(versions[1].version, 1);
        Ok(())
    }

    #[tokio::test]
    async fn test_versioning_get_version() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(0);

        let data = json!({"blocks": [{"name": "amp", "gain": 0.7}]});
        svc.create_version(preset_id, data.clone(), None, None)
            .await?;

        // -- Exec
        let model = svc.get_version(preset_id, 1).await?;

        // -- Check
        assert_eq!(model.data, data);
        assert_eq!(model.version, 1);
        Ok(())
    }

    #[tokio::test]
    async fn test_versioning_get_version_not_found() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(0);

        // -- Exec
        let result = svc.get_version(preset_id, 999).await;

        // -- Check
        assert!(matches!(result, Err(StorageError::NotFound { .. })));
        Ok(())
    }

    #[tokio::test]
    async fn test_versioning_restore_version() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(0);

        let original_data = json!({"gain": 0.5});
        svc.create_version(preset_id, original_data.clone(), None, None)
            .await?;
        svc.create_version(preset_id, json!({"gain": 0.9}), None, None)
            .await?;

        // -- Exec: restore to version 1
        let restored = svc.restore_version(preset_id, 1, None).await?;

        // -- Check
        assert_eq!(restored.version, 3); // new version created
        assert_eq!(
            restored.change_summary.as_deref(),
            Some("Restored from version 1")
        );

        let model = svc.get_version(preset_id, 3).await?;
        assert_eq!(model.data, original_data);
        Ok(())
    }

    #[tokio::test]
    async fn test_versioning_prune_old_versions() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let svc = VersioningService::new(db)
            .with_debounce_secs(0)
            .with_max_versions(3);

        // -- Exec: create 5 versions (should prune to 3)
        for i in 1..=5 {
            svc.create_version(preset_id, json!({"v": i}), None, None)
                .await?;
        }

        // -- Check
        let versions = svc.list_versions(preset_id).await?;
        assert_eq!(versions.len(), 3);
        // Oldest should be version 3 (1 and 2 pruned)
        assert_eq!(versions[2].version, 3);
        assert_eq!(versions[0].version, 5);
        Ok(())
    }

    #[tokio::test]
    async fn test_versioning_diff_versions() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(0);

        svc.create_version(
            preset_id,
            json!({"name": "Clean", "gain": 0.3, "reverb": true}),
            None,
            None,
        )
        .await?;
        svc.create_version(
            preset_id,
            json!({"name": "Dirty", "gain": 0.9, "delay": 0.5}),
            None,
            None,
        )
        .await?;

        // -- Exec
        let diff = svc.diff_versions(preset_id, 1, 2).await?;

        // -- Check
        assert_eq!(diff.from_version, 1);
        assert_eq!(diff.to_version, 2);
        // name modified, gain modified, reverb removed, delay added = 4 changes
        assert_eq!(diff.changes.len(), 4);

        let paths: Vec<&str> = diff.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"name"));
        assert!(paths.contains(&"gain"));
        assert!(paths.contains(&"reverb"));
        assert!(paths.contains(&"delay"));
        Ok(())
    }

    #[tokio::test]
    async fn test_versioning_version_count() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(0);

        // -- Check: initially 0
        assert_eq!(svc.version_count(preset_id).await?, 0);

        // -- Exec
        svc.create_version(preset_id, json!({}), None, None).await?;
        svc.create_version(preset_id, json!({}), None, None).await?;

        // -- Check
        assert_eq!(svc.version_count(preset_id).await?, 2);
        Ok(())
    }

    #[tokio::test]
    async fn test_versioning_latest_version() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(0);

        // -- Check: initially None
        assert!(svc.latest_version(preset_id).await?.is_none());

        // -- Exec
        svc.create_version(preset_id, json!({"v": 1}), None, None)
            .await?;
        svc.create_version(preset_id, json!({"v": 2}), None, None)
            .await?;

        // -- Check
        let latest = svc.latest_version(preset_id).await?.unwrap();
        assert_eq!(latest.version, 2);
        Ok(())
    }

    #[tokio::test]
    async fn test_versioning_separate_presets() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_a = insert_preset(&db).await?;
        let preset_b = insert_preset(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(0);

        // -- Exec: each preset gets its own version sequence
        svc.create_version(preset_a, json!({"a": 1}), None, None)
            .await?;
        svc.create_version(preset_a, json!({"a": 2}), None, None)
            .await?;
        svc.create_version(preset_b, json!({"b": 1}), None, None)
            .await?;

        // -- Check
        let versions_a = svc.list_versions(preset_a).await?;
        let versions_b = svc.list_versions(preset_b).await?;
        assert_eq!(versions_a.len(), 2);
        assert_eq!(versions_b.len(), 1);
        // Preset B's version is 1, not 3
        assert_eq!(versions_b[0].version, 1);
        Ok(())
    }

    /// Helper: insert a dummy user and return its ID.
    async fn insert_user(db: &DatabaseConnection) -> Result<Uuid> {
        use crate::entities::user;
        let id = Uuid::new_v4();
        let now = Utc::now();
        let model = user::ActiveModel {
            id: Set(id),
            username: Set(format!("user-{}", &id.to_string()[..8])),
            email: Set(format!("{}@test.com", &id.to_string()[..8])),
            display_name: Set(None),
            avatar_url: Set(None),
            created_at: Set(now.into()),
            updated_at: Set(now.into()),
        };
        model.insert(db).await?;
        Ok(id)
    }

    #[tokio::test]
    async fn test_versioning_with_created_by() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let preset_id = insert_preset(&db).await?;
        let user_id = insert_user(&db).await?;
        let svc = VersioningService::new(db).with_debounce_secs(0);

        // -- Exec
        let info = svc
            .create_version(preset_id, json!({}), None, Some(user_id))
            .await?
            .unwrap();

        // -- Check
        assert_eq!(info.created_by, Some(user_id));
        Ok(())
    }
}

// endregion: --- Tests
