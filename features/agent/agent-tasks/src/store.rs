//! SQLite-backed implementation of `agent_proto::service::tasks::AgentTaskQueue`.
//!
//! One `Store` = one queue DB file. The dispatcher runs server-side
//! and instantiates one of these per active queue; clients reach it
//! through the vox RPC plumbing in `agent-proto`.

use std::path::{Path, PathBuf};
use std::sync::Mutex;

use agent_proto::AgentError;
use agent_proto::tasks::{
    AgentTask, AgentTaskComment, AgentTaskLink, AgentTaskLinkKind, AgentTaskStatus, Queue,
    QueueFilter, QueueSnapshot,
};
use chrono::Utc;
use rusqlite::{Connection, params};
use ulid::Ulid;

use crate::conn::{from_ms, from_ms_opt, now_ms, open, to_ms_opt};
use crate::error::TasksDbError;

pub struct Store {
    db_path: PathBuf,
    conn: Mutex<Connection>,
}

impl Store {
    /// Open (or create) a queue database at `path`.
    pub fn open(path: impl Into<PathBuf>) -> Result<Self, TasksDbError> {
        let path = path.into();
        let conn = open(&path)?;
        Ok(Self {
            db_path: path,
            conn: Mutex::new(conn),
        })
    }

    /// Path of the underlying DB file. Useful for tests + the
    /// "open another store on the same file" scenario.
    #[must_use]
    pub fn db_path(&self) -> &Path {
        &self.db_path
    }

    fn with_conn<R>(
        &self,
        f: impl FnOnce(&Connection) -> Result<R, TasksDbError>,
    ) -> Result<R, TasksDbError> {
        let conn = self
            .conn
            .lock()
            .map_err(|_| TasksDbError::Invalid("connection mutex poisoned".into()))?;
        f(&conn)
    }

    // --------------------------------------------------------------
    // Queues
    // --------------------------------------------------------------

    pub fn list_queues(&self) -> Result<Vec<Queue>, TasksDbError> {
        self.with_conn(|conn| {
            let mut stmt = conn.prepare(
                "SELECT id, label, assignees, created_at, updated_at FROM queues ORDER BY id",
            )?;
            let rows = stmt
                .query_map([], |row| {
                    Ok(Queue {
                        id: row.get(0)?,
                        label: row.get(1)?,
                        assignees: parse_json_array(&row.get::<_, String>(2)?),
                        created_at: from_ms(row.get(3)?),
                        updated_at: from_ms(row.get(4)?),
                    })
                })?
                .collect::<Result<Vec<_>, _>>()?;
            Ok(rows)
        })
    }

    pub fn upsert_queue(&self, queue: Queue) -> Result<Queue, TasksDbError> {
        let now = now_ms();
        let assignees = serde_json::to_string(&queue.assignees)?;
        self.with_conn(|conn| {
            conn.execute(
                "INSERT INTO queues(id, label, assignees, created_at, updated_at) \
                 VALUES (?, ?, ?, ?, ?) \
                 ON CONFLICT(id) DO UPDATE SET \
                   label = excluded.label, \
                   assignees = excluded.assignees, \
                   updated_at = excluded.updated_at",
                params![queue.id, queue.label, assignees, now, now],
            )?;
            Ok(())
        })?;
        // Re-read so created_at reflects DB truth on existing rows.
        self.read_queue_row(&queue.id)
    }

    pub fn remove_queue(&self, queue_id: &str) -> Result<(), TasksDbError> {
        let removed = self.with_conn(|conn| {
            conn.execute("DELETE FROM queues WHERE id = ?", params![queue_id])
                .map_err(Into::into)
        })?;
        if removed == 0 {
            return Err(TasksDbError::NotFound {
                kind: "queue",
                id: queue_id.to_string(),
            });
        }
        Ok(())
    }

    pub fn read_queue(
        &self,
        queue_id: &str,
        filter: QueueFilter,
    ) -> Result<QueueSnapshot, TasksDbError> {
        let queue = self.read_queue_row(queue_id)?;
        let tasks = self.list_agent_tasks(queue_id, &filter)?;
        let latest_event_id = self.with_conn(|conn| {
            let v: Option<i64> = conn
                .query_row(
                    "SELECT MAX(id) FROM queue_events WHERE queue_id = ?",
                    params![queue_id],
                    |row| row.get(0),
                )
                .unwrap_or(None);
            Ok(v.unwrap_or(0).max(0) as u64)
        })?;
        Ok(QueueSnapshot {
            queue,
            tasks,
            latest_event_id,
            read_only: false,
        })
    }

    fn read_queue_row(&self, queue_id: &str) -> Result<Queue, TasksDbError> {
        self.with_conn(|conn| {
            let mut stmt = conn.prepare(
                "SELECT id, label, assignees, created_at, updated_at FROM queues WHERE id = ?",
            )?;
            let q = stmt
                .query_row(params![queue_id], |row| {
                    Ok(Queue {
                        id: row.get(0)?,
                        label: row.get(1)?,
                        assignees: parse_json_array(&row.get::<_, String>(2)?),
                        created_at: from_ms(row.get(3)?),
                        updated_at: from_ms(row.get(4)?),
                    })
                })
                .map_err(|e| match e {
                    rusqlite::Error::QueryReturnedNoRows => TasksDbError::NotFound {
                        kind: "queue",
                        id: queue_id.to_string(),
                    },
                    other => other.into(),
                })?;
            Ok(q)
        })
    }

    // --------------------------------------------------------------
    // Agent tasks
    // --------------------------------------------------------------

    pub fn upsert_agent_task(&self, mut task: AgentTask) -> Result<AgentTask, TasksDbError> {
        // Reject any attempt to set Running outside of claim().
        if task.status == AgentTaskStatus::Running {
            return Err(TasksDbError::InvalidTransition(
                "cannot set status=Running directly; use claim_agent_task".into(),
            ));
        }
        if task.id.is_empty() {
            task.id = format!("at-{}", Ulid::new());
        }
        let now = now_ms();
        if task.created_at.timestamp_millis() == 0 {
            task.created_at = Utc::now();
        }
        task.updated_at = Utc::now();

        let labels = serde_json::to_string(&task.labels)?;
        let claim_expires_ms = to_ms_opt(task.claim_expires_at);
        let archived_ms = to_ms_opt(task.archived_at);

        self.with_conn(|conn| {
            conn.execute(
                "INSERT INTO agent_tasks( \
                    id, queue_id, status, title, description, priority, labels, \
                    source_task, project, agent_profile, claimed_by, claim_expires_at, \
                    worker_pid, result_blob, linked_session_id, created_at, updated_at, archived_at \
                 ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) \
                 ON CONFLICT(id) DO UPDATE SET \
                    status = excluded.status, \
                    title = excluded.title, \
                    description = excluded.description, \
                    priority = excluded.priority, \
                    labels = excluded.labels, \
                    source_task = excluded.source_task, \
                    project = excluded.project, \
                    agent_profile = excluded.agent_profile, \
                    linked_session_id = excluded.linked_session_id, \
                    updated_at = excluded.updated_at, \
                    archived_at = excluded.archived_at",
                params![
                    task.id,
                    task.queue_id,
                    task.status.as_str(),
                    task.title,
                    task.description,
                    task.priority,
                    labels,
                    task.source_task,
                    task.project,
                    task.agent_profile,
                    task.claimed_by,
                    claim_expires_ms,
                    task.worker_pid,
                    task.result_blob,
                    task.linked_session_id,
                    task.created_at.timestamp_millis(),
                    task.updated_at.timestamp_millis(),
                    archived_ms,
                ],
            )?;
            log_event(conn, &task.queue_id, Some(&task.id), "created", "{}", now)?;
            Ok(())
        })?;
        self.read_agent_task(&task.id)
    }

    pub fn read_agent_task(&self, id: &str) -> Result<AgentTask, TasksDbError> {
        self.with_conn(|conn| {
            let mut stmt = conn.prepare(AGENT_TASK_SELECT)?;
            stmt.query_row(params![id], row_to_agent_task)
                .map_err(|e| match e {
                    rusqlite::Error::QueryReturnedNoRows => TasksDbError::NotFound {
                        kind: "agent_task",
                        id: id.to_string(),
                    },
                    other => other.into(),
                })
        })
    }

    pub fn remove_agent_task(&self, id: &str) -> Result<(), TasksDbError> {
        let removed = self.with_conn(|conn| {
            Ok(conn.execute("DELETE FROM agent_tasks WHERE id = ?", params![id])?)
        })?;
        if removed == 0 {
            return Err(TasksDbError::NotFound {
                kind: "agent_task",
                id: id.to_string(),
            });
        }
        Ok(())
    }

    /// Atomic claim. Flips status to `Running` *iff* the row is
    /// currently in `Ready` AND `claimed_by` is empty. Returns
    /// `Conflict` otherwise.
    pub fn claim_agent_task(&self, id: &str, handle: &str) -> Result<AgentTask, TasksDbError> {
        if handle.is_empty() {
            return Err(TasksDbError::Invalid("handle must be non-empty".into()));
        }
        let now = now_ms();
        let claimed = self.with_conn(|conn| {
            let n = conn.execute(
                "UPDATE agent_tasks SET \
                    status = 'running', \
                    claimed_by = ?, \
                    updated_at = ? \
                 WHERE id = ? \
                   AND status = 'ready' \
                   AND (claimed_by = '' OR claimed_by IS NULL)",
                params![handle, now, id],
            )?;
            if n == 1 {
                log_event(
                    conn,
                    &queue_id_of(conn, id)?,
                    Some(id),
                    "claim",
                    &serde_json::json!({ "by": handle }).to_string(),
                    now,
                )?;
            }
            Ok(n)
        })?;
        if claimed == 0 {
            // Disambiguate.
            let current = self.read_agent_task(id)?;
            return Err(TasksDbError::Conflict(format!(
                "agent task {id} is {} (claimed_by={})",
                current.status.as_str(),
                current.claimed_by
            )));
        }
        self.read_agent_task(id)
    }

    pub fn set_agent_task_status(
        &self,
        id: &str,
        new_status: AgentTaskStatus,
    ) -> Result<AgentTask, TasksDbError> {
        if new_status == AgentTaskStatus::Running {
            return Err(TasksDbError::InvalidTransition(
                "use claim_agent_task to transition into Running".into(),
            ));
        }
        let now = now_ms();
        // Block move to Done if any incoming Blocks link is not Done itself.
        if new_status == AgentTaskStatus::Done {
            self.check_unblocked(id)?;
        }
        let archived_ms = (new_status == AgentTaskStatus::Archived).then_some(now);
        let updated = self.with_conn(|conn| {
            let n = conn.execute(
                "UPDATE agent_tasks SET \
                    status = ?, \
                    claimed_by = CASE WHEN ? IN ('done','archived') THEN '' ELSE claimed_by END, \
                    claim_expires_at = CASE WHEN ? IN ('done','archived') THEN NULL ELSE claim_expires_at END, \
                    archived_at = COALESCE(?, archived_at), \
                    updated_at = ? \
                 WHERE id = ?",
                params![new_status.as_str(), new_status.as_str(), new_status.as_str(), archived_ms, now, id],
            )?;
            if n == 1 {
                log_event(
                    conn,
                    &queue_id_of(conn, id)?,
                    Some(id),
                    "status",
                    &serde_json::json!({ "to": new_status.as_str() }).to_string(),
                    now,
                )?;
            }
            Ok(n)
        })?;
        if updated == 0 {
            return Err(TasksDbError::NotFound {
                kind: "agent_task",
                id: id.to_string(),
            });
        }
        self.read_agent_task(id)
    }

    pub fn complete_agent_task(
        &self,
        id: &str,
        result_blob: &str,
    ) -> Result<AgentTask, TasksDbError> {
        self.check_unblocked(id)?;
        let now = now_ms();
        let updated = self.with_conn(|conn| {
            Ok(conn.execute(
                "UPDATE agent_tasks SET \
                    status = 'done', \
                    result_blob = ?, \
                    claimed_by = '', \
                    claim_expires_at = NULL, \
                    updated_at = ? \
                 WHERE id = ?",
                params![result_blob, now, id],
            )?)
        })?;
        if updated == 0 {
            return Err(TasksDbError::NotFound {
                kind: "agent_task",
                id: id.to_string(),
            });
        }
        self.with_conn(|conn| {
            log_event(conn, &queue_id_of(conn, id)?, Some(id), "done", "{}", now)
        })?;
        self.read_agent_task(id)
    }

    pub fn link_agent_task_to_session(
        &self,
        id: &str,
        session_id: &str,
    ) -> Result<AgentTask, TasksDbError> {
        let now = now_ms();
        let updated = self.with_conn(|conn| {
            Ok(conn.execute(
                "UPDATE agent_tasks SET linked_session_id = ?, updated_at = ? WHERE id = ?",
                params![session_id, now, id],
            )?)
        })?;
        if updated == 0 {
            return Err(TasksDbError::NotFound {
                kind: "agent_task",
                id: id.to_string(),
            });
        }
        self.read_agent_task(id)
    }

    fn check_unblocked(&self, id: &str) -> Result<(), TasksDbError> {
        self.with_conn(|conn| {
            let blocking: i64 = conn.query_row(
                "SELECT COUNT(*) FROM agent_task_links l \
                  JOIN agent_tasks t ON t.id = l.from_task \
                  WHERE l.to_task = ? AND l.kind = ? AND t.status != 'done'",
                params![id, AgentTaskLinkKind::Blocks as u8],
                |row| row.get(0),
            )?;
            if blocking > 0 {
                return Err(TasksDbError::InvalidTransition(format!(
                    "agent task {id} has {blocking} unresolved Blocks dependencies"
                )));
            }
            Ok(())
        })
    }

    fn list_agent_tasks(
        &self,
        queue_id: &str,
        filter: &QueueFilter,
    ) -> Result<Vec<AgentTask>, TasksDbError> {
        self.with_conn(|conn| {
            let mut sql = String::from(AGENT_TASK_SELECT_BASE);
            sql.push_str(" WHERE queue_id = ?1");
            if !filter.include_archived {
                sql.push_str(" AND status != 'archived'");
            }
            if !filter.agent_profile.is_empty() {
                sql.push_str(" AND agent_profile = ?2");
            }
            sql.push_str(" ORDER BY priority ASC, created_at ASC");

            let tasks: Vec<AgentTask> = if filter.agent_profile.is_empty() {
                conn.prepare(&sql)?
                    .query_map(params![queue_id], row_to_agent_task)?
                    .collect::<Result<_, _>>()?
            } else {
                conn.prepare(&sql)?
                    .query_map(params![queue_id, filter.agent_profile], row_to_agent_task)?
                    .collect::<Result<_, _>>()?
            };

            let assignee = &filter.assignee;
            let only_handle = &filter.only_handle;
            let linked_session = &filter.linked_session_id;
            Ok(tasks
                .into_iter()
                .filter(|t| assignee.is_empty() || t.claimed_by == *assignee)
                .filter(|t| only_handle.is_empty() || t.claimed_by == *only_handle)
                .filter(|t| linked_session.is_empty() || t.linked_session_id == *linked_session)
                .collect())
        })
    }

    // --------------------------------------------------------------
    // Links
    // --------------------------------------------------------------

    pub fn add_agent_task_link(
        &self,
        from_task: &str,
        to_task: &str,
        kind: AgentTaskLinkKind,
    ) -> Result<AgentTaskLink, TasksDbError> {
        if from_task == to_task {
            return Err(TasksDbError::Invalid("self-link not allowed".into()));
        }
        let now = now_ms();
        self.with_conn(|conn| {
            conn.execute(
                "INSERT OR IGNORE INTO agent_task_links(from_task, to_task, kind, created_at) \
                 VALUES (?, ?, ?, ?)",
                params![from_task, to_task, kind as u8, now],
            )?;
            Ok(())
        })?;
        Ok(AgentTaskLink {
            from_task: from_task.to_string(),
            to_task: to_task.to_string(),
            kind,
            created_at: from_ms(now),
        })
    }

    pub fn remove_agent_task_link(
        &self,
        from_task: &str,
        to_task: &str,
        kind: AgentTaskLinkKind,
    ) -> Result<(), TasksDbError> {
        self.with_conn(|conn| {
            Ok(conn.execute(
                "DELETE FROM agent_task_links WHERE from_task = ? AND to_task = ? AND kind = ?",
                params![from_task, to_task, kind as u8],
            )?)
        })?;
        Ok(())
    }

    pub fn list_agent_task_links(
        &self,
        queue_id: &str,
    ) -> Result<Vec<AgentTaskLink>, TasksDbError> {
        self.with_conn(|conn| {
            let mut stmt = conn.prepare(
                "SELECT l.from_task, l.to_task, l.kind, l.created_at \
                   FROM agent_task_links l \
                   JOIN agent_tasks t ON t.id = l.from_task \
                  WHERE t.queue_id = ?",
            )?;
            let rows = stmt
                .query_map(params![queue_id], |row| {
                    let kind_u8: u8 = row.get(2)?;
                    Ok(AgentTaskLink {
                        from_task: row.get(0)?,
                        to_task: row.get(1)?,
                        kind: kind_from_u8(kind_u8),
                        created_at: from_ms(row.get(3)?),
                    })
                })?
                .collect::<Result<Vec<_>, _>>()?;
            Ok(rows)
        })
    }

    // --------------------------------------------------------------
    // Comments
    // --------------------------------------------------------------

    pub fn add_agent_task_comment(
        &self,
        agent_task_id: &str,
        author: &str,
        body: &str,
    ) -> Result<AgentTaskComment, TasksDbError> {
        let now = now_ms();
        let id = self.with_conn(|conn| {
            conn.execute(
                "INSERT INTO agent_task_comments(agent_task_id, author, body, created_at) \
                 VALUES (?, ?, ?, ?)",
                params![agent_task_id, author, body, now],
            )?;
            log_event(
                conn,
                &queue_id_of(conn, agent_task_id)?,
                Some(agent_task_id),
                "comment",
                &serde_json::json!({ "author": author }).to_string(),
                now,
            )?;
            Ok(conn.last_insert_rowid())
        })?;
        Ok(AgentTaskComment {
            id: id.to_string(),
            agent_task_id: agent_task_id.to_string(),
            author: author.to_string(),
            body: body.to_string(),
            created_at: from_ms(now),
        })
    }

    pub fn list_agent_task_comments(
        &self,
        agent_task_id: &str,
    ) -> Result<Vec<AgentTaskComment>, TasksDbError> {
        self.with_conn(|conn| {
            let mut stmt = conn.prepare(
                "SELECT id, agent_task_id, author, body, created_at \
                   FROM agent_task_comments \
                  WHERE agent_task_id = ? \
                  ORDER BY id",
            )?;
            let rows = stmt
                .query_map(params![agent_task_id], |row| {
                    let id_i64: i64 = row.get(0)?;
                    Ok(AgentTaskComment {
                        id: id_i64.to_string(),
                        agent_task_id: row.get(1)?,
                        author: row.get(2)?,
                        body: row.get(3)?,
                        created_at: from_ms(row.get(4)?),
                    })
                })?
                .collect::<Result<Vec<_>, _>>()?;
            Ok(rows)
        })
    }
}

// ------------------------------------------------------------------
// Trait impl over the proto
// ------------------------------------------------------------------

impl agent_proto::service::tasks::AgentTaskQueue for Store {
    fn list_queues(&self) -> Result<Vec<Queue>, AgentError> {
        Store::list_queues(self).map_err(Into::into)
    }
    fn upsert_queue(&self, queue: Queue) -> Result<Queue, AgentError> {
        Store::upsert_queue(self, queue).map_err(Into::into)
    }
    fn remove_queue(&self, queue_id: &str) -> Result<(), AgentError> {
        Store::remove_queue(self, queue_id).map_err(Into::into)
    }
    fn read_queue(&self, queue_id: &str, filter: QueueFilter) -> Result<QueueSnapshot, AgentError> {
        Store::read_queue(self, queue_id, filter).map_err(Into::into)
    }
    fn upsert_agent_task(&self, task: AgentTask) -> Result<AgentTask, AgentError> {
        Store::upsert_agent_task(self, task).map_err(Into::into)
    }
    fn remove_agent_task(&self, agent_task_id: &str) -> Result<(), AgentError> {
        Store::remove_agent_task(self, agent_task_id).map_err(Into::into)
    }
    fn read_agent_task(&self, agent_task_id: &str) -> Result<AgentTask, AgentError> {
        Store::read_agent_task(self, agent_task_id).map_err(Into::into)
    }
    fn claim_agent_task(&self, agent_task_id: &str, handle: &str) -> Result<AgentTask, AgentError> {
        Store::claim_agent_task(self, agent_task_id, handle).map_err(Into::into)
    }
    fn set_agent_task_status(
        &self,
        agent_task_id: &str,
        new_status: AgentTaskStatus,
    ) -> Result<AgentTask, AgentError> {
        Store::set_agent_task_status(self, agent_task_id, new_status).map_err(Into::into)
    }
    fn link_agent_task_to_session(
        &self,
        agent_task_id: &str,
        session_id: &str,
    ) -> Result<AgentTask, AgentError> {
        Store::link_agent_task_to_session(self, agent_task_id, session_id).map_err(Into::into)
    }
    fn complete_agent_task(
        &self,
        agent_task_id: &str,
        result_blob: &str,
    ) -> Result<AgentTask, AgentError> {
        Store::complete_agent_task(self, agent_task_id, result_blob).map_err(Into::into)
    }
    fn add_agent_task_link(
        &self,
        from_task: &str,
        to_task: &str,
        kind: AgentTaskLinkKind,
    ) -> Result<AgentTaskLink, AgentError> {
        Store::add_agent_task_link(self, from_task, to_task, kind).map_err(Into::into)
    }
    fn remove_agent_task_link(
        &self,
        from_task: &str,
        to_task: &str,
        kind: AgentTaskLinkKind,
    ) -> Result<(), AgentError> {
        Store::remove_agent_task_link(self, from_task, to_task, kind).map_err(Into::into)
    }
    fn list_agent_task_links(&self, queue_id: &str) -> Result<Vec<AgentTaskLink>, AgentError> {
        Store::list_agent_task_links(self, queue_id).map_err(Into::into)
    }
    fn add_agent_task_comment(
        &self,
        agent_task_id: &str,
        author: &str,
        body: &str,
    ) -> Result<AgentTaskComment, AgentError> {
        Store::add_agent_task_comment(self, agent_task_id, author, body).map_err(Into::into)
    }
    fn list_agent_task_comments(
        &self,
        agent_task_id: &str,
    ) -> Result<Vec<AgentTaskComment>, AgentError> {
        Store::list_agent_task_comments(self, agent_task_id).map_err(Into::into)
    }
}

// ------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------

const AGENT_TASK_SELECT_BASE: &str = "SELECT \
    id, queue_id, status, title, description, priority, labels, \
    source_task, project, agent_profile, claimed_by, claim_expires_at, \
    worker_pid, result_blob, linked_session_id, created_at, updated_at, archived_at \
 FROM agent_tasks";

const AGENT_TASK_SELECT: &str = "SELECT \
    id, queue_id, status, title, description, priority, labels, \
    source_task, project, agent_profile, claimed_by, claim_expires_at, \
    worker_pid, result_blob, linked_session_id, created_at, updated_at, archived_at \
 FROM agent_tasks WHERE id = ?";

fn row_to_agent_task(row: &rusqlite::Row<'_>) -> rusqlite::Result<AgentTask> {
    let status_s: String = row.get(2)?;
    Ok(AgentTask {
        id: row.get(0)?,
        queue_id: row.get(1)?,
        status: AgentTaskStatus::from_str(&status_s).unwrap_or(AgentTaskStatus::Triage),
        title: row.get(3)?,
        description: row.get(4)?,
        priority: row.get(5)?,
        labels: parse_json_array(&row.get::<_, String>(6)?),
        source_task: row.get(7)?,
        project: row.get(8)?,
        agent_profile: row.get(9)?,
        claimed_by: row.get(10)?,
        claim_expires_at: from_ms_opt(row.get(11)?),
        worker_pid: row.get(12)?,
        result_blob: row.get(13)?,
        linked_session_id: row.get(14)?,
        created_at: from_ms(row.get(15)?),
        updated_at: from_ms(row.get(16)?),
        archived_at: from_ms_opt(row.get(17)?),
    })
}

fn parse_json_array(s: &str) -> Vec<String> {
    serde_json::from_str(s).unwrap_or_default()
}

fn kind_from_u8(v: u8) -> AgentTaskLinkKind {
    match v {
        0 => AgentTaskLinkKind::Blocks,
        2 => AgentTaskLinkKind::SpawnedFrom,
        _ => AgentTaskLinkKind::Related,
    }
}

fn queue_id_of(conn: &Connection, agent_task_id: &str) -> Result<String, TasksDbError> {
    conn.query_row(
        "SELECT queue_id FROM agent_tasks WHERE id = ?",
        params![agent_task_id],
        |row| row.get::<_, String>(0),
    )
    .map_err(|e| match e {
        rusqlite::Error::QueryReturnedNoRows => TasksDbError::NotFound {
            kind: "agent_task",
            id: agent_task_id.to_string(),
        },
        other => other.into(),
    })
}

fn log_event(
    conn: &Connection,
    queue_id: &str,
    agent_task_id: Option<&str>,
    kind: &str,
    payload: &str,
    ts: i64,
) -> Result<(), TasksDbError> {
    conn.execute(
        "INSERT INTO queue_events(queue_id, agent_task_id, kind, payload, ts) VALUES (?,?,?,?,?)",
        params![queue_id, agent_task_id, kind, payload, ts],
    )?;
    Ok(())
}
