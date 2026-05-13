//! Server-side `EventSinkImpl` — the bridge from plugin events back
//! into the Loro doc.
//!
//! Plugins call `sink.patch_run`, `sink.append_log`, and
//! `sink.maybe_mirror_task_status` without knowing which Repo type
//! the server is using. This impl wires them to:
//!
//! - `agent_crdt::AgentRunRepoLoro::update`     — for `patch_run`
//! - `agent_crdt::AgentLogLineRepoLoro::create` — for `append_log`
//! - `project_crdt::TaskRepoLoro::update`       — for `maybe_mirror_task_status`
//!   AND for task-side mirroring (branch_name, pr_urls, commit_refs)
//!   whenever `RunSnapshot` carries those fields.
//!
//! All writes are CRDT-merge-friendly: the underlying Loro repos
//! mutate via `LoroMap` keys, so concurrent webhook + WS writes
//! reconcile cleanly.

use std::sync::Arc;

use agent_crdt::{AgentLogLineRepoLoro, AgentRunRepoLoro};
use agent_proto::integration::{AgentLogLineDto, EventSinkImpl, IntegrationError, RunSnapshot};
use agent_proto::{AgentLogLineCreate, AgentLogLineRepo, AgentRunRepo, AgentRunUpdate};
use async_trait::async_trait;
use project_crdt::TaskRepoLoro;
use project_proto::{CommitRef, TaskRepo, TaskUpdate};
use uuid::Uuid;

pub struct ServerEventSink {
    pub agent_run_repo: Arc<AgentRunRepoLoro>,
    pub log_repo: Arc<AgentLogLineRepoLoro>,
    pub task_repo: Arc<TaskRepoLoro>,
}

impl ServerEventSink {
    pub fn new(
        agent_run_repo: Arc<AgentRunRepoLoro>,
        log_repo: Arc<AgentLogLineRepoLoro>,
        task_repo: Arc<TaskRepoLoro>,
    ) -> Self {
        Self {
            agent_run_repo,
            log_repo,
            task_repo,
        }
    }
}

fn map_err(e: impl std::fmt::Display) -> IntegrationError {
    IntegrationError::Network(e.to_string())
}

#[async_trait]
impl EventSinkImpl for ServerEventSink {
    async fn patch_run(&self, run_id: Uuid, snapshot: RunSnapshot) -> Result<(), IntegrationError> {
        // Fetch existing for task_id linkage and for pr_url / commit_ref
        // dedup. Tolerant: if the run isn't in the doc yet (race with
        // dispatch ack persistence), we just patch what we have.
        let existing = self.agent_run_repo.get(run_id).await.ok();

        let update = AgentRunUpdate {
            status: Some(snapshot.status.as_str().to_string()),
            started_at: Some(snapshot.started_at),
            completed_at: Some(snapshot.completed_at),
            result: Some(snapshot.result_summary.clone()),
            error_message: Some(snapshot.error.clone()),
            tokens_used: Some(snapshot.tokens_used),
            cost_cents: Some(snapshot.cost_cents),
            ..Default::default()
        };
        self.agent_run_repo
            .update(run_id, update)
            .await
            .map_err(map_err)?;

        // Mirror branch_name / pr_urls / commit_refs onto the linked
        // Task, if any.
        if let Some(run) = existing {
            if let Some(task_id) = run.task_id {
                let task = self.task_repo.get(task_id).await.ok();
                if let Some(t) = task {
                    let mut new_pr_urls = t.pr_urls.clone();
                    for u in &snapshot.pr_urls {
                        if !new_pr_urls.contains(u) {
                            new_pr_urls.push(u.clone());
                        }
                    }

                    let mut new_commits = t.commit_refs.clone();
                    for c in &snapshot.commit_refs {
                        if !new_commits.iter().any(|x| x.sha == c.sha) {
                            new_commits.push(CommitRef {
                                sha: c.sha.clone(),
                                message: c.message.clone(),
                                url: c.url.clone(),
                                authored_at: c.authored_at,
                            });
                        }
                    }

                    let branch_update = snapshot
                        .branch_name
                        .as_ref()
                        .filter(|_| t.branch_name.is_none())
                        .cloned();

                    let upd = TaskUpdate {
                        branch_name: branch_update.map(Some),
                        pr_urls: Some(new_pr_urls),
                        commit_refs: Some(new_commits),
                        ..Default::default()
                    };
                    let _ = self.task_repo.update(task_id, upd).await;
                }
            }
        }

        Ok(())
    }

    async fn append_log(
        &self,
        run_id: Uuid,
        line: AgentLogLineDto,
    ) -> Result<(), IntegrationError> {
        let create = AgentLogLineCreate {
            run_id,
            at: line.at,
            level: line.level,
            source: line.source,
            text: line.text,
            external_event_id: line.external_event_id,
        };
        self.log_repo.create(create).await.map_err(map_err)?;
        Ok(())
    }

    async fn maybe_mirror_task_status(
        &self,
        task_id: Uuid,
        new_status: &str,
    ) -> Result<(), IntegrationError> {
        // v1 policy: always mirror. FUTURE: per-project opt-out flag.
        let upd = TaskUpdate {
            status: Some(new_status.to_string()),
            ..Default::default()
        };
        self.task_repo.update(task_id, upd).await.map_err(map_err)?;
        Ok(())
    }
}
