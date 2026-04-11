//! Full-text search over indexed tasks.
//!
//! Uses SQLite FTS5 for fast text search across task titles, bodies,
//! tags, and project names.

use rusqlite::params;

use crate::service::VaultError;
use super::sqlite::{TaskIndex, TaskRow};

/// Full-text search index (built on SQLite FTS5).
pub struct SearchIndex;

impl TaskIndex {
    /// Initialize the FTS5 virtual table for full-text search.
    pub fn create_search_index(&self) -> Result<(), VaultError> {
        self.conn
            .execute_batch(
                "CREATE VIRTUAL TABLE IF NOT EXISTS tasks_fts USING fts5(
                    id,
                    title,
                    body_preview,
                    tags,
                    projects,
                    assignee,
                    content='tasks',
                    content_rowid='rowid'
                );

                -- Triggers to keep FTS in sync with the tasks table
                CREATE TRIGGER IF NOT EXISTS tasks_ai AFTER INSERT ON tasks BEGIN
                    INSERT INTO tasks_fts(rowid, id, title, body_preview, assignee)
                    VALUES (new.rowid, new.id, new.title, new.body_preview, new.assignee);
                END;

                CREATE TRIGGER IF NOT EXISTS tasks_ad AFTER DELETE ON tasks BEGIN
                    INSERT INTO tasks_fts(tasks_fts, rowid, id, title, body_preview, assignee)
                    VALUES ('delete', old.rowid, old.id, old.title, old.body_preview, old.assignee);
                END;

                CREATE TRIGGER IF NOT EXISTS tasks_au AFTER UPDATE ON tasks BEGIN
                    INSERT INTO tasks_fts(tasks_fts, rowid, id, title, body_preview, assignee)
                    VALUES ('delete', old.rowid, old.id, old.title, old.body_preview, old.assignee);
                    INSERT INTO tasks_fts(rowid, id, title, body_preview, assignee)
                    VALUES (new.rowid, new.id, new.title, new.body_preview, new.assignee);
                END;",
            )
            .map_err(|e| VaultError::IoError(format!("FTS5 setup: {e}")))?;

        Ok(())
    }

    /// Search tasks by text query. Returns matching tasks ranked by relevance.
    pub fn search(&self, query: &str) -> Result<Vec<TaskRow>, VaultError> {
        let fts_query = format!("{}*", query.replace('"', ""));

        let mut stmt = self.conn
            .prepare(
                "SELECT t.id, t.title, t.status, t.priority, t.assignee, t.due, t.urgency, t.file_path
                 FROM tasks t
                 JOIN tasks_fts fts ON t.id = fts.id
                 WHERE tasks_fts MATCH ?1
                 ORDER BY rank
                 LIMIT 50",
            )
            .map_err(|e| VaultError::IoError(format!("FTS5 search: {e}")))?;

        let rows = stmt
            .query_map(params![fts_query], |row| {
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
            .map_err(|e| VaultError::IoError(format!("FTS5 query: {e}")))?
            .filter_map(|r| r.ok())
            .collect();

        Ok(rows)
    }

    /// Rebuild the FTS index from the tasks table.
    pub fn rebuild_search_index(&self) -> Result<(), VaultError> {
        self.conn
            .execute("INSERT INTO tasks_fts(tasks_fts) VALUES ('rebuild')", [])
            .map_err(|e| VaultError::IoError(format!("FTS5 rebuild: {e}")))?;
        Ok(())
    }
}
