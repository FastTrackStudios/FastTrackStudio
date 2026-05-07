//! SQLite-backed task index for fast queries.
//!
//! Indexes YAML frontmatter fields from .md files into SQLite tables.
//! Rebuilt from files on startup or on demand.

use std::path::Path;

use rusqlite::{Connection, params};

use crate::service::{ProviderSyncState, VaultError};
use crate::task::Task;

/// SQLite index over task .md files.
pub struct TaskIndex {
    pub(crate) conn: Connection,
}

/// Typed payload for [`TaskIndex::record_change`].
#[derive(Debug, Clone, Copy)]
pub struct ChangeRecord<'a> {
    pub entity_type: &'a str,
    pub entity_id: &'a str,
    pub field: Option<&'a str>,
    pub old_value: Option<&'a str>,
    pub new_value: Option<&'a str>,
    pub changed_by: Option<&'a str>,
    pub file_path: Option<&'a str>,
}

/// Typed payload for [`TaskIndex::record_conflict`].
#[derive(Debug, Clone, Copy)]
pub struct ConflictRecord<'a> {
    pub entity_type: &'a str,
    pub entity_id: &'a str,
    pub field: &'a str,
    pub winning_value: Option<&'a str>,
    pub losing_value: Option<&'a str>,
    pub winning_actor: Option<&'a str>,
    pub losing_actor: Option<&'a str>,
    pub file_path: Option<&'a str>,
    /// Short tag: "concurrent" / "timer" / "manual".
    pub kind: &'a str,
}

impl TaskIndex {
    /// Open or create an index database.
    pub fn open(path: &Path) -> Result<Self, VaultError> {
        let conn =
            Connection::open(path).map_err(|e| VaultError::IoError(format!("SQLite open: {e}")))?;

        let index = Self { conn };
        index.create_tables()?;
        Ok(index)
    }

    /// Create an in-memory index (for testing or ephemeral use).
    pub fn in_memory() -> Result<Self, VaultError> {
        let conn = Connection::open_in_memory()
            .map_err(|e| VaultError::IoError(format!("SQLite: {e}")))?;

        let index = Self { conn };
        index.create_tables()?;
        Ok(index)
    }

    fn create_tables(&self) -> Result<(), VaultError> {
        self.conn
            .execute_batch(
                "
            CREATE TABLE IF NOT EXISTS tasks (
                id TEXT PRIMARY KEY,
                title TEXT NOT NULL,
                status TEXT NOT NULL DEFAULT 'Open',
                priority TEXT NOT NULL DEFAULT 'None',
                assignee TEXT,
                due TEXT,
                scheduled TEXT,
                completed_date TEXT,
                recurrence TEXT,
                time_estimate INTEGER,
                urgency INTEGER NOT NULL DEFAULT 0,
                external_source TEXT,
                external_id TEXT,
                created_by TEXT,
                date_created TEXT,
                date_modified TEXT,
                file_path TEXT NOT NULL,
                body_preview TEXT
            );

            CREATE TABLE IF NOT EXISTS task_projects (
                task_id TEXT NOT NULL,
                project TEXT NOT NULL,
                PRIMARY KEY (task_id, project)
            );

            CREATE TABLE IF NOT EXISTS task_tags (
                task_id TEXT NOT NULL,
                tag TEXT NOT NULL,
                PRIMARY KEY (task_id, tag)
            );

            CREATE TABLE IF NOT EXISTS task_contexts (
                task_id TEXT NOT NULL,
                context TEXT NOT NULL,
                PRIMARY KEY (task_id, context)
            );

            CREATE TABLE IF NOT EXISTS projects (
                title TEXT PRIMARY KEY,
                status TEXT NOT NULL DEFAULT 'Active',
                area TEXT,
                project_type TEXT,
                due TEXT,
                start TEXT,
                description TEXT,
                repo TEXT,
                file_path TEXT NOT NULL
            );

            CREATE TABLE IF NOT EXISTS project_team (
                project_title TEXT NOT NULL,
                member TEXT NOT NULL,
                PRIMARY KEY (project_title, member)
            );

            CREATE TABLE IF NOT EXISTS changes (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                entity_type TEXT NOT NULL,
                entity_id TEXT NOT NULL,
                field TEXT,
                old_value TEXT,
                new_value TEXT,
                changed_by TEXT,
                changed_at TEXT NOT NULL DEFAULT (datetime('now')),
                file_path TEXT,
                -- Conflict metadata. NULL = ordinary change.
                -- Otherwise: 'concurrent' (two heads touched same field),
                -- 'timer' (two running timers), 'manual' (user-flagged).
                conflict_kind TEXT,
                -- Concurrent actor ID (for 'concurrent' conflicts): who made
                -- the competing edit that lost out.
                conflict_other_actor TEXT,
                conflict_other_value TEXT,
                -- Resolution: NULL = open, 'resolved' = decided, 'ignored'.
                conflict_resolved TEXT,
                conflict_resolved_by TEXT,
                conflict_resolved_at TEXT
            );

            CREATE TABLE IF NOT EXISTS provider_sync_state (
                provider TEXT NOT NULL,
                account TEXT,
                collection TEXT NOT NULL,
                sync_token TEXT,
                cursor TEXT,
                etag TEXT,
                last_success_at TEXT,
                last_failure_at TEXT,
                last_error TEXT,
                updated_at TEXT NOT NULL DEFAULT (datetime('now')),
                PRIMARY KEY (provider, collection)
            );

            -- Indexes for common queries
            CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status);
            CREATE INDEX IF NOT EXISTS idx_tasks_assignee ON tasks(assignee);
            CREATE INDEX IF NOT EXISTS idx_tasks_due ON tasks(due);
            CREATE INDEX IF NOT EXISTS idx_tasks_urgency ON tasks(urgency);
            CREATE INDEX IF NOT EXISTS idx_task_projects_project ON task_projects(project);
            CREATE INDEX IF NOT EXISTS idx_task_tags_tag ON task_tags(tag);
            CREATE INDEX IF NOT EXISTS idx_changes_entity ON changes(entity_type, entity_id);
            CREATE INDEX IF NOT EXISTS idx_changes_time ON changes(changed_at);
            CREATE INDEX IF NOT EXISTS idx_changes_conflict ON changes(conflict_kind) WHERE conflict_kind IS NOT NULL;
            CREATE INDEX IF NOT EXISTS idx_provider_sync_state_provider ON provider_sync_state(provider);
            ",
            )
            .map_err(|e| VaultError::IoError(format!("SQLite schema: {e}")))?;

        // Lightweight migration: add conflict columns to an existing `changes` table
        // that predates them. `ALTER TABLE ... ADD COLUMN` is idempotent-friendly
        // because we swallow errors for columns that already exist.
        for alter in [
            "ALTER TABLE changes ADD COLUMN conflict_kind TEXT",
            "ALTER TABLE changes ADD COLUMN conflict_other_actor TEXT",
            "ALTER TABLE changes ADD COLUMN conflict_other_value TEXT",
            "ALTER TABLE changes ADD COLUMN conflict_resolved TEXT",
            "ALTER TABLE changes ADD COLUMN conflict_resolved_by TEXT",
            "ALTER TABLE changes ADD COLUMN conflict_resolved_at TEXT",
        ] {
            let _ = self.conn.execute(alter, []);
        }

        Ok(())
    }

    pub fn upsert_sync_state(&self, state: &ProviderSyncState) -> Result<(), VaultError> {
        self.conn
            .execute(
                "INSERT INTO provider_sync_state (
                    provider, account, collection, sync_token, cursor, etag,
                    last_success_at, last_failure_at, last_error, updated_at
                 ) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, datetime('now'))
                 ON CONFLICT(provider, collection) DO UPDATE SET
                    account = excluded.account,
                    sync_token = excluded.sync_token,
                    cursor = excluded.cursor,
                    etag = excluded.etag,
                    last_success_at = COALESCE(excluded.last_success_at, provider_sync_state.last_success_at),
                    last_failure_at = COALESCE(excluded.last_failure_at, provider_sync_state.last_failure_at),
                    last_error = excluded.last_error,
                    updated_at = datetime('now')",
                params![
                    state.provider,
                    state.account,
                    state.collection,
                    state.sync_token,
                    state.cursor,
                    state.etag,
                    state.last_success_at,
                    state.last_failure_at,
                    state.last_error,
                ],
            )
            .map_err(|e| VaultError::IoError(format!("SQLite upsert sync state: {e}")))?;
        Ok(())
    }

    pub fn list_sync_states(&self) -> Result<Vec<ProviderSyncState>, VaultError> {
        let mut stmt = self
            .conn
            .prepare(
                "SELECT provider, account, collection, sync_token, cursor, etag,
                        last_success_at, last_failure_at, last_error, updated_at
                 FROM provider_sync_state
                 ORDER BY provider ASC, collection ASC",
            )
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        let rows = stmt
            .query_map([], |row| {
                Ok(ProviderSyncState {
                    provider: row.get(0)?,
                    account: row.get(1)?,
                    collection: row.get(2)?,
                    sync_token: row.get(3)?,
                    cursor: row.get(4)?,
                    etag: row.get(5)?,
                    last_success_at: row.get(6)?,
                    last_failure_at: row.get(7)?,
                    last_error: row.get(8)?,
                    updated_at: row.get(9)?,
                })
            })
            .map_err(|e| VaultError::IoError(e.to_string()))?
            .filter_map(|row| row.ok())
            .collect();
        Ok(rows)
    }

    // ── Write ────────────────────────────────────────────────────────

    /// Index a task from a parsed .md file.
    pub fn index_task(&self, task: &Task, file_path: &str) -> Result<(), VaultError> {
        let id = task.id_ref();
        let status = format!("{:?}", task.status);
        let priority = format!("{:?}", task.priority);
        let urgency = task.urgency_score();
        let body_preview: String = task.body.chars().take(200).collect();

        self.conn
            .execute(
                "INSERT OR REPLACE INTO tasks
                (id, title, status, priority, assignee, due, scheduled,
                 completed_date, recurrence, time_estimate, urgency,
                 external_source, external_id, created_by,
                 date_created, date_modified, file_path, body_preview)
                VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14, ?15, ?16, ?17, ?18)",
                params![
                    id,
                    task.title,
                    status,
                    priority,
                    task.assignee,
                    task.due.map(|d| d.to_string()),
                    task.scheduled.map(|d| d.to_string()),
                    task.completed_date.map(|d| d.to_string()),
                    task.recurrence,
                    task.time_estimate,
                    urgency,
                    task.external_source,
                    task.external_id,
                    task.created_by,
                    task.date_created.map(|d| d.to_rfc3339()),
                    task.date_modified.map(|d| d.to_rfc3339()),
                    file_path,
                    body_preview,
                ],
            )
            .map_err(|e| VaultError::IoError(format!("SQLite insert task: {e}")))?;

        // Index projects
        self.conn
            .execute("DELETE FROM task_projects WHERE task_id = ?1", params![id])
            .ok();
        for proj in &task.projects {
            self.conn
                .execute(
                    "INSERT OR IGNORE INTO task_projects (task_id, project) VALUES (?1, ?2)",
                    params![id, proj.0],
                )
                .ok();
        }

        // Index tags
        self.conn
            .execute("DELETE FROM task_tags WHERE task_id = ?1", params![id])
            .ok();
        for tag in &task.tags {
            self.conn
                .execute(
                    "INSERT OR IGNORE INTO task_tags (task_id, tag) VALUES (?1, ?2)",
                    params![id, tag],
                )
                .ok();
        }

        // Index contexts
        self.conn
            .execute("DELETE FROM task_contexts WHERE task_id = ?1", params![id])
            .ok();
        for ctx in &task.contexts {
            self.conn
                .execute(
                    "INSERT OR IGNORE INTO task_contexts (task_id, context) VALUES (?1, ?2)",
                    params![id, ctx],
                )
                .ok();
        }

        Ok(())
    }

    /// Remove a task from the index.
    pub fn remove_task(&self, id: &str) -> Result<(), VaultError> {
        self.conn
            .execute("DELETE FROM tasks WHERE id = ?1", params![id])
            .ok();
        self.conn
            .execute("DELETE FROM task_projects WHERE task_id = ?1", params![id])
            .ok();
        self.conn
            .execute("DELETE FROM task_tags WHERE task_id = ?1", params![id])
            .ok();
        self.conn
            .execute("DELETE FROM task_contexts WHERE task_id = ?1", params![id])
            .ok();
        Ok(())
    }

    /// Record a concurrent-edit conflict. Leaves `conflict_resolved` NULL, so the
    /// row shows up in `list_conflicts()` until a caller resolves it.
    ///
    /// `kind` is a short tag: "concurrent" (CRDT heads disagree on a field),
    /// "timer" (two running timers on the same task), "manual" (user flagged).
    pub fn record_conflict(&self, record: ConflictRecord<'_>) -> Result<i64, VaultError> {
        let ConflictRecord {
            entity_type,
            entity_id,
            field,
            winning_value,
            losing_value,
            winning_actor,
            losing_actor,
            file_path,
            kind,
        } = record;
        self.conn
            .execute(
                "INSERT INTO changes (
                    entity_type, entity_id, field, old_value, new_value,
                    changed_by, file_path,
                    conflict_kind, conflict_other_actor, conflict_other_value
                 ) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10)",
                params![
                    entity_type,
                    entity_id,
                    field,
                    losing_value,
                    winning_value,
                    winning_actor,
                    file_path,
                    kind,
                    losing_actor,
                    losing_value,
                ],
            )
            .map_err(|e| VaultError::IoError(format!("SQLite record conflict: {e}")))?;
        Ok(self.conn.last_insert_rowid())
    }

    /// Mark a conflict row resolved. `how` is a free-form tag ("picked-winning",
    /// "picked-losing", "merged", "ignored"). `resolver` identifies the user/agent.
    pub fn resolve_conflict(
        &self,
        conflict_id: i64,
        resolver: Option<&str>,
        how: &str,
    ) -> Result<(), VaultError> {
        self.conn
            .execute(
                "UPDATE changes
                 SET conflict_resolved = ?1,
                     conflict_resolved_by = ?2,
                     conflict_resolved_at = datetime('now')
                 WHERE id = ?3 AND conflict_kind IS NOT NULL",
                params![how, resolver, conflict_id],
            )
            .map_err(|e| VaultError::IoError(format!("SQLite resolve conflict: {e}")))?;
        Ok(())
    }

    /// List conflicts. `open_only = true` returns only unresolved ones.
    pub fn list_conflicts(
        &self,
        open_only: bool,
        limit: u32,
    ) -> Result<Vec<ConflictRow>, VaultError> {
        let sql = if open_only {
            "SELECT id, entity_type, entity_id, field, new_value, old_value,
                    changed_by, conflict_other_actor, conflict_kind,
                    changed_at, file_path, conflict_resolved, conflict_resolved_by
             FROM changes
             WHERE conflict_kind IS NOT NULL AND conflict_resolved IS NULL
             ORDER BY changed_at DESC LIMIT ?1"
        } else {
            "SELECT id, entity_type, entity_id, field, new_value, old_value,
                    changed_by, conflict_other_actor, conflict_kind,
                    changed_at, file_path, conflict_resolved, conflict_resolved_by
             FROM changes
             WHERE conflict_kind IS NOT NULL
             ORDER BY changed_at DESC LIMIT ?1"
        };

        let mut stmt = self
            .conn
            .prepare(sql)
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        let rows = stmt
            .query_map(params![limit], |row| {
                Ok(ConflictRow {
                    id: row.get(0)?,
                    entity_type: row.get(1)?,
                    entity_id: row.get(2)?,
                    field: row.get(3)?,
                    winning_value: row.get(4)?,
                    losing_value: row.get(5)?,
                    winning_actor: row.get(6)?,
                    losing_actor: row.get(7)?,
                    kind: row.get(8)?,
                    changed_at: row.get(9)?,
                    file_path: row.get(10)?,
                    resolved: row.get(11)?,
                    resolved_by: row.get(12)?,
                })
            })
            .map_err(|e| VaultError::IoError(e.to_string()))?
            .filter_map(|r| r.ok())
            .collect();

        Ok(rows)
    }

    /// Record a change for the audit trail.
    pub fn record_change(&self, record: ChangeRecord<'_>) -> Result<(), VaultError> {
        let ChangeRecord {
            entity_type,
            entity_id,
            field,
            old_value,
            new_value,
            changed_by,
            file_path,
        } = record;
        self.conn
            .execute(
                "INSERT INTO changes (entity_type, entity_id, field, old_value, new_value, changed_by, file_path)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
                params![entity_type, entity_id, field, old_value, new_value, changed_by, file_path],
            )
            .map_err(|e| VaultError::IoError(format!("SQLite record change: {e}")))?;
        Ok(())
    }

    /// Clear the entire index (for full rebuild).
    pub fn clear(&self) -> Result<(), VaultError> {
        self.conn
            .execute_batch(
                "DELETE FROM task_contexts;
                 DELETE FROM task_tags;
                 DELETE FROM task_projects;
                 DELETE FROM tasks;
                 DELETE FROM project_team;
                 DELETE FROM projects;",
            )
            .map_err(|e| VaultError::IoError(format!("SQLite clear: {e}")))?;
        Ok(())
    }

    // ── Read ─────────────────────────────────────────────────────────

    /// Count tasks by status.
    pub fn count_by_status(&self) -> Result<Vec<(String, i64)>, VaultError> {
        let mut stmt = self
            .conn
            .prepare("SELECT status, COUNT(*) FROM tasks GROUP BY status ORDER BY COUNT(*) DESC")
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        let rows = stmt
            .query_map([], |row| Ok((row.get(0)?, row.get(1)?)))
            .map_err(|e| VaultError::IoError(e.to_string()))?
            .filter_map(|r| r.ok())
            .collect();

        Ok(rows)
    }

    /// Get tasks assigned to a specific user.
    pub fn tasks_for_user(&self, username: &str) -> Result<Vec<TaskRow>, VaultError> {
        let mut stmt = self
            .conn
            .prepare(
                "SELECT id, title, status, priority, assignee, due, urgency, file_path
                 FROM tasks WHERE assignee = ?1 ORDER BY urgency DESC",
            )
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        let rows = stmt
            .query_map(params![username], |row| {
                Ok(TaskRow {
                    id: row.get(0)?,
                    title: row.get(1)?,
                    status: row.get(2)?,
                    priority: row.get(3)?,
                    assignee: row.get(4)?,
                    due: row.get(5)?,
                    urgency: row.get(6)?,
                    file_path: row.get(7)?,
                })
            })
            .map_err(|e| VaultError::IoError(e.to_string()))?
            .filter_map(|r| r.ok())
            .collect();

        Ok(rows)
    }

    /// Get tasks for a project.
    pub fn tasks_for_project(&self, project: &str) -> Result<Vec<TaskRow>, VaultError> {
        let mut stmt = self.conn
            .prepare(
                "SELECT t.id, t.title, t.status, t.priority, t.assignee, t.due, t.urgency, t.file_path
                 FROM tasks t
                 JOIN task_projects tp ON t.id = tp.task_id
                 WHERE tp.project = ?1
                 ORDER BY t.urgency DESC",
            )
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        let rows = stmt
            .query_map(params![project], |row| {
                Ok(TaskRow {
                    id: row.get(0)?,
                    title: row.get(1)?,
                    status: row.get(2)?,
                    priority: row.get(3)?,
                    assignee: row.get(4)?,
                    due: row.get(5)?,
                    urgency: row.get(6)?,
                    file_path: row.get(7)?,
                })
            })
            .map_err(|e| VaultError::IoError(e.to_string()))?
            .filter_map(|r| r.ok())
            .collect();

        Ok(rows)
    }

    /// Get tasks due on or before a date.
    pub fn tasks_due_by(&self, date: &str) -> Result<Vec<TaskRow>, VaultError> {
        let mut stmt = self.conn
            .prepare(
                "SELECT id, title, status, priority, assignee, due, urgency, file_path
                 FROM tasks
                 WHERE due IS NOT NULL AND due <= ?1 AND status NOT IN ('Done', 'Cancelled', 'Archived')
                 ORDER BY due ASC, urgency DESC",
            )
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        let rows = stmt
            .query_map(params![date], |row| {
                Ok(TaskRow {
                    id: row.get(0)?,
                    title: row.get(1)?,
                    status: row.get(2)?,
                    priority: row.get(3)?,
                    assignee: row.get(4)?,
                    due: row.get(5)?,
                    urgency: row.get(6)?,
                    file_path: row.get(7)?,
                })
            })
            .map_err(|e| VaultError::IoError(e.to_string()))?
            .filter_map(|r| r.ok())
            .collect();

        Ok(rows)
    }

    /// Get tasks with a specific tag.
    pub fn tasks_with_tag(&self, tag: &str) -> Result<Vec<TaskRow>, VaultError> {
        let mut stmt = self.conn
            .prepare(
                "SELECT t.id, t.title, t.status, t.priority, t.assignee, t.due, t.urgency, t.file_path
                 FROM tasks t
                 JOIN task_tags tt ON t.id = tt.task_id
                 WHERE tt.tag = ?1
                 ORDER BY t.urgency DESC",
            )
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        let rows = stmt
            .query_map(params![tag], |row| {
                Ok(TaskRow {
                    id: row.get(0)?,
                    title: row.get(1)?,
                    status: row.get(2)?,
                    priority: row.get(3)?,
                    assignee: row.get(4)?,
                    due: row.get(5)?,
                    urgency: row.get(6)?,
                    file_path: row.get(7)?,
                })
            })
            .map_err(|e| VaultError::IoError(e.to_string()))?
            .filter_map(|r| r.ok())
            .collect();

        Ok(rows)
    }

    /// Get recent changes (audit trail).
    pub fn recent_changes(&self, limit: u32) -> Result<Vec<ChangeRow>, VaultError> {
        let mut stmt = self.conn
            .prepare(
                "SELECT entity_type, entity_id, field, old_value, new_value, changed_by, changed_at, file_path
                 FROM changes ORDER BY changed_at DESC LIMIT ?1",
            )
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        let rows = stmt
            .query_map(params![limit], |row| {
                Ok(ChangeRow {
                    entity_type: row.get(0)?,
                    entity_id: row.get(1)?,
                    field: row.get(2)?,
                    old_value: row.get(3)?,
                    new_value: row.get(4)?,
                    changed_by: row.get(5)?,
                    changed_at: row.get(6)?,
                    file_path: row.get(7)?,
                })
            })
            .map_err(|e| VaultError::IoError(e.to_string()))?
            .filter_map(|r| r.ok())
            .collect();

        Ok(rows)
    }

    /// Get total task count.
    pub fn task_count(&self) -> Result<i64, VaultError> {
        self.conn
            .query_row("SELECT COUNT(*) FROM tasks", [], |row| row.get(0))
            .map_err(|e| VaultError::IoError(e.to_string()))
    }
}

// ── Row types ────────────────────────────────────────────────────────────────

/// A task row from the index (lightweight, no body).
#[derive(Debug, Clone)]
pub struct TaskRow {
    pub id: String,
    pub title: String,
    pub status: String,
    pub priority: String,
    pub assignee: Option<String>,
    pub due: Option<String>,
    pub urgency: i32,
    pub file_path: String,
}

/// A change log entry.
#[derive(Debug, Clone, facet::Facet)]
pub struct ChangeRow {
    pub entity_type: String,
    pub entity_id: String,
    pub field: Option<String>,
    pub old_value: Option<String>,
    pub new_value: Option<String>,
    pub changed_by: Option<String>,
    pub changed_at: String,
    pub file_path: Option<String>,
}

/// A conflict row from the `changes` table.
///
/// Represents a concurrent edit where two replicas touched the same field. The
/// "winning" value is whatever state the local replica currently holds; the
/// "losing" value is what got overwritten. A human or agent must decide what to
/// keep via `resolve_conflict`.
#[derive(Debug, Clone, facet::Facet)]
pub struct ConflictRow {
    pub id: i64,
    pub entity_type: String,
    pub entity_id: String,
    pub field: Option<String>,
    pub winning_value: Option<String>,
    pub losing_value: Option<String>,
    pub winning_actor: Option<String>,
    pub losing_actor: Option<String>,
    pub kind: Option<String>,
    pub changed_at: String,
    pub file_path: Option<String>,
    pub resolved: Option<String>,
    pub resolved_by: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::task::{Priority, Status, Task, WikiLink};

    #[test]
    fn index_and_query() {
        let index = TaskIndex::in_memory().unwrap();

        let task = Task {
            id: uuid::Uuid::parse_str("00000000-0000-4000-8000-000000000201").unwrap(),
            title: "Fix auth bug".into(),
            status: Status::InProgress,
            priority: Priority::High,
            assignee: Some("codywright".into()),
            due: chrono::NaiveDate::from_ymd_opt(2026, 4, 15),
            projects: vec![WikiLink("Task App".into())].into(),
            tags: vec!["backend".into()].into(),
            ..Default::default()
        };

        index.index_task(&task, "tasks/Fix auth bug.md").unwrap();

        // Query by assignee
        let results = index.tasks_for_user("codywright").unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].title, "Fix auth bug");

        // Query by project
        let results = index.tasks_for_project("Task App").unwrap();
        assert_eq!(results.len(), 1);

        // Query by tag
        let results = index.tasks_with_tag("backend").unwrap();
        assert_eq!(results.len(), 1);

        // Count
        assert_eq!(index.task_count().unwrap(), 1);
    }

    #[test]
    fn change_tracking() {
        let index = TaskIndex::in_memory().unwrap();

        index
            .record_change(ChangeRecord {
                entity_type: "task",
                entity_id: "test-1",
                field: Some("status"),
                old_value: Some("Open"),
                new_value: Some("Done"),
                changed_by: Some("codywright"),
                file_path: Some("tasks/test.md"),
            })
            .unwrap();

        let changes = index.recent_changes(10).unwrap();
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].entity_id, "test-1");
        assert_eq!(changes[0].field.as_deref(), Some("status"));
        assert_eq!(changes[0].new_value.as_deref(), Some("Done"));
    }

    #[test]
    fn conflict_round_trip() {
        let index = TaskIndex::in_memory().unwrap();

        let id = index
            .record_conflict(ConflictRecord {
                entity_type: "task",
                entity_id: "t-42",
                field: "assignee",
                winning_value: Some("codywright"),
                losing_value: Some("amy"),
                winning_actor: Some("hermes"),
                losing_actor: Some("tommy"),
                file_path: Some("tasks/Fix bug.md"),
                kind: "concurrent",
            })
            .unwrap();

        let open = index.list_conflicts(true, 10).unwrap();
        assert_eq!(open.len(), 1);
        assert_eq!(open[0].id, id);
        assert_eq!(open[0].kind.as_deref(), Some("concurrent"));
        assert_eq!(open[0].winning_value.as_deref(), Some("codywright"));
        assert_eq!(open[0].losing_value.as_deref(), Some("amy"));
        assert_eq!(open[0].winning_actor.as_deref(), Some("hermes"));
        assert_eq!(open[0].losing_actor.as_deref(), Some("tommy"));
        assert!(open[0].resolved.is_none());

        index
            .resolve_conflict(id, Some("codywright"), "picked-winning")
            .unwrap();

        let open = index.list_conflicts(true, 10).unwrap();
        assert!(
            open.is_empty(),
            "resolved conflict should not appear in open list"
        );

        let all = index.list_conflicts(false, 10).unwrap();
        assert_eq!(all.len(), 1);
        assert_eq!(all[0].resolved.as_deref(), Some("picked-winning"));
        assert_eq!(all[0].resolved_by.as_deref(), Some("codywright"));
    }

    #[test]
    fn provider_sync_state_round_trip() {
        let index = TaskIndex::in_memory().unwrap();
        index
            .upsert_sync_state(&ProviderSyncState {
                provider: "carddav".to_string(),
                account: Some("agent".to_string()),
                collection: "contacts".to_string(),
                sync_token: Some("sync-1".to_string()),
                last_success_at: Some("2026-04-30T08:00:00Z".to_string()),
                ..Default::default()
            })
            .unwrap();
        index
            .upsert_sync_state(&ProviderSyncState {
                provider: "carddav".to_string(),
                account: Some("agent".to_string()),
                collection: "contacts".to_string(),
                sync_token: Some("sync-2".to_string()),
                last_error: Some("temporary failure".to_string()),
                last_failure_at: Some("2026-04-30T08:05:00Z".to_string()),
                ..Default::default()
            })
            .unwrap();

        let states = index.list_sync_states().unwrap();
        assert_eq!(states.len(), 1);
        assert_eq!(states[0].provider, "carddav");
        assert_eq!(states[0].collection, "contacts");
        assert_eq!(states[0].sync_token.as_deref(), Some("sync-2"));
        assert_eq!(states[0].last_error.as_deref(), Some("temporary failure"));
        assert_eq!(
            states[0].last_success_at.as_deref(),
            Some("2026-04-30T08:00:00Z")
        );
    }
}
