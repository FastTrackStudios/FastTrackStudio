//! Persistence for [`Run`] — every attempt, kept.
//!
//! Attempts are append-only in spirit: a retry creates a new row
//! rather than overwriting the last one, because the history *is*
//! the diagnostic.

use std::time::Duration;

use agent_proto::error::AgentError;
use agent_proto::run::{FinishRun, Run, RunFilter, RunStatus, StartRun};
use agent_proto::service::runs::Runs;
use chrono::{DateTime, Utc};
use sea_orm::{ConnectionTrait, DatabaseConnection, Statement, Value};
use uuid::Uuid;

/// How long an in-progress run may go without a heartbeat before a
/// sweep calls it stale. Matches the runner window — one machine,
/// one notion of "recently".
pub const RUN_STALE_AFTER: Duration = Duration::from_secs(120);

#[derive(Clone, Debug)]
pub struct RunStore {
    conn: DatabaseConnection,
}

fn ts(v: DateTime<Utc>) -> String {
    v.to_rfc3339()
}

fn parse_ts(s: &str) -> Option<DateTime<Utc>> {
    if s.is_empty() {
        return None;
    }
    DateTime::parse_from_rfc3339(s)
        .ok()
        .map(|d| d.with_timezone(&Utc))
}

impl RunStore {
    #[must_use]
    pub fn new(conn: DatabaseConnection) -> Self {
        Self { conn }
    }

    fn backend(&self) -> sea_orm::DatabaseBackend {
        self.conn.get_database_backend()
    }

    async fn exec(&self, sql: &str, values: Vec<Value>) -> Result<(), AgentError> {
        self.conn
            .execute(Statement::from_sql_and_values(self.backend(), sql, values))
            .await
            .map_err(|e| AgentError::Backend(format!("runs: {e}")))?;
        Ok(())
    }

    async fn rows(&self, sql: &str, values: Vec<Value>) -> Result<Vec<Run>, AgentError> {
        let rows = self
            .conn
            .query_all(Statement::from_sql_and_values(self.backend(), sql, values))
            .await
            .map_err(|e| AgentError::Backend(format!("runs: {e}")))?;

        let mut out = Vec::with_capacity(rows.len());
        for r in rows {
            let get = |c: &str| -> Result<String, AgentError> {
                r.try_get::<String>("", c)
                    .map_err(|e| AgentError::Backend(format!("runs column {c}: {e}")))
            };
            let id = Uuid::parse_str(&get("id")?)
                .map_err(|e| AgentError::Backend(format!("run id: {e}")))?;
            let ticket = Uuid::parse_str(&get("ticket")?)
                .map_err(|e| AgentError::Backend(format!("run ticket: {e}")))?;
            let parent = {
                let p = get("parent")?;
                if p.is_empty() {
                    None
                } else {
                    Uuid::parse_str(&p).ok()
                }
            };
            let exit = get("exit_code")?;
            out.push(Run {
                id,
                ticket,
                runner: get("runner")?,
                parent,
                branch: get("branch")?,
                worktree_path: get("worktree_path")?,
                session_path: get("session_path")?,
                status: RunStatus::parse(&get("status")?).unwrap_or(RunStatus::Dead),
                exit_code: if exit.is_empty() {
                    None
                } else {
                    exit.parse().ok()
                },
                started_at: parse_ts(&get("started_at")?).unwrap_or_else(Utc::now),
                heartbeat_at: parse_ts(&get("heartbeat_at")?),
                finished_at: parse_ts(&get("finished_at")?),
            });
        }
        Ok(out)
    }

    async fn one(&self, id: Uuid) -> Result<Run, AgentError> {
        self.rows(
            "SELECT * FROM agent_runs WHERE id = ?",
            vec![id.to_string().into()],
        )
        .await?
        .pop()
        .ok_or_else(|| AgentError::AgentTaskNotFound(id.to_string()))
    }

    /// Attempts for one ticket, newest first.
    ///
    /// # Errors
    ///
    /// [`AgentError`] on a storage failure.
    pub async fn for_ticket(&self, ticket: Uuid) -> Result<Vec<Run>, AgentError> {
        self.rows(
            "SELECT * FROM agent_runs WHERE ticket = ? ORDER BY started_at DESC",
            vec![ticket.to_string().into()],
        )
        .await
    }
}

impl Runs for RunStore {
    async fn start_run(&self, start: StartRun) -> Result<Run, AgentError> {
        let now = Utc::now();
        let id = Uuid::new_v4();
        self.exec(
            "INSERT INTO agent_runs (id, ticket, runner, parent, branch, worktree_path, \
             session_path, status, exit_code, started_at, heartbeat_at, finished_at) \
             VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",
            vec![
                id.to_string().into(),
                start.ticket.to_string().into(),
                start.runner.into(),
                start.parent.map(|p| p.to_string()).unwrap_or_default().into(),
                start.branch.into(),
                start.worktree_path.into(),
                start.session_path.into(),
                RunStatus::InProgress.as_str().into(),
                String::new().into(),
                ts(now).into(),
                ts(now).into(),
                String::new().into(),
            ],
        )
        .await?;
        self.one(id).await
    }

    async fn beat_run(&self, run_id: Uuid) -> Result<(), AgentError> {
        self.exec(
            "UPDATE agent_runs SET heartbeat_at = ?, status = CASE WHEN status = 'stale' \
             THEN 'in-progress' ELSE status END WHERE id = ?",
            vec![ts(Utc::now()).into(), run_id.to_string().into()],
        )
        .await
    }

    async fn finish_run(&self, finish: FinishRun) -> Result<Run, AgentError> {
        // A worktree still on disk is exactly what needs-cleanup
        // means, so the verdict and the disk state are recorded
        // together rather than in two writes that could disagree.
        let status = if finish.worktree_kept {
            RunStatus::NeedsCleanup
        } else if finish.passed {
            RunStatus::Passed
        } else {
            RunStatus::Failed
        };
        self.exec(
            "UPDATE agent_runs SET status = ?, exit_code = ?, finished_at = ? WHERE id = ?",
            vec![
                status.as_str().into(),
                finish
                    .exit_code
                    .map(|c| c.to_string())
                    .unwrap_or_default()
                    .into(),
                ts(Utc::now()).into(),
                finish.run.to_string().into(),
            ],
        )
        .await?;
        self.one(finish.run).await
    }

    async fn get_run(&self, run_id: Uuid) -> Result<Run, AgentError> {
        self.one(run_id).await
    }

    async fn list_runs(&self, filter: RunFilter) -> Result<Vec<Run>, AgentError> {
        let mut sql = String::from("SELECT * FROM agent_runs WHERE 1=1");
        let mut vals: Vec<Value> = Vec::new();
        if let Some(t) = filter.ticket {
            sql.push_str(" AND ticket = ?");
            vals.push(t.to_string().into());
        }
        if !filter.runner.is_empty() {
            sql.push_str(" AND runner = ?");
            vals.push(filter.runner.into());
        }
        if let Some(p) = filter.parent {
            sql.push_str(" AND parent = ?");
            vals.push(p.to_string().into());
        }
        if let Some(s) = filter.status {
            sql.push_str(" AND status = ?");
            vals.push(s.as_str().into());
        }
        sql.push_str(" ORDER BY started_at DESC");
        if filter.limit > 0 {
            sql.push_str(&format!(" LIMIT {}", filter.limit));
        }
        self.rows(&sql, vals).await
    }

    async fn archive_run(&self, run_id: Uuid) -> Result<Run, AgentError> {
        self.exec(
            "UPDATE agent_runs SET status = ? WHERE id = ?",
            vec![
                RunStatus::Archived.as_str().into(),
                run_id.to_string().into(),
            ],
        )
        .await?;
        self.one(run_id).await
    }

    async fn sweep_stale_runs(&self) -> Result<u32, AgentError> {
        let cutoff = Utc::now()
            - chrono::Duration::from_std(RUN_STALE_AFTER)
                .map_err(|e| AgentError::Backend(format!("stale window: {e}")))?;
        let before = self
            .list_runs(RunFilter {
                status: Some(RunStatus::InProgress),
                ..Default::default()
            })
            .await?;
        let lapsed: Vec<&Run> = before
            .iter()
            .filter(|r| r.heartbeat_at.is_none_or(|h| h < cutoff))
            .collect();
        for r in &lapsed {
            self.exec(
                "UPDATE agent_runs SET status = ? WHERE id = ?",
                vec![RunStatus::Stale.as_str().into(), r.id.to_string().into()],
            )
            .await?;
        }
        Ok(u32::try_from(lapsed.len()).unwrap_or(u32::MAX))
    }
}
