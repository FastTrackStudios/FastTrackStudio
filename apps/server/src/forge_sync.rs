//! Server-driven Task ↔ forge issue sync.
//!
//! Two halves, both living here so `features/task` stays a dumb,
//! git-agnostic leaf (the acyclic-leaf rule) — all forge coupling
//! is in `apps/server`, which already depends on `git-*`:
//!
//! 1. [`ForgeSyncTaskService`] — a decorator around the per-org
//!    [`task::TaskBackend`] that the vox `TaskService` mount wraps.
//!    On `create`, if the task's project is bound to a forge repo
//!    (a [`git_config::RepoBinding`]), it best-effort-creates the
//!    matching forge issue and records the link. On `update`, it
//!    reconciles the linked issue per the `git_config::sync`
//!    per-field model. Forge failures are logged, never fail the
//!    local write — the poll loop reconciles later.
//!
//! 2. [`spawn_poll_loop`] — a background task that periodically
//!    pulls forge-side changes (close / edit) back into the linked
//!    tasks, so the localhost dev server stays in sync without a
//!    publicly reachable webhook. (The webhook receiver in
//!    [`crate::webhooks`] stays available for when a public URL
//!    exists — instant and cheaper than polling.)
//!
//! Scope today: only `Forge::Forgejo` repos are synced server-side
//! (codeberg is Forgejo). GitHub bindings are skipped with a debug
//! log — a per-repo GitHub backend resolver is the obvious
//! follow-up. Only the scalar projection (`title` / `body` /
//! open-closed `state`) is synced, matching `git_config::sync`.

use std::path::PathBuf;
use std::time::Duration;

use chrono::{DateTime, Utc};
use git_config::sync::{ForgeUpdate, SyncedFields, forge_update, reconcile_synced};
use git_config::{BindingStore, FileStore, IssueLink, IssueSnapshot, LinkKind};
use git_proto::issues::IssueTracker;
use git_proto::{Forge, Issue, IssueId, IssueState, IssueUpdate, RepoId};
use task::model::TaskInfo;
use task::service::{ClaimResult, TaskError, TaskService};
use uuid::Uuid;

use crate::{AppState, OrgAppState};

/// `TaskService` decorator that mirrors create/update into a bound
/// forge repo. Wraps the raw [`task::TaskBackend`]; all reads and
/// the non-syncing mutations delegate straight through.
#[derive(Clone)]
pub struct ForgeSyncTaskService {
    inner: task::TaskBackend,
    /// Forge client authenticated as the human identity (the
    /// configured `TASK_FORGEJO_TOKEN`). Used for tasks owned by a
    /// person (or unassigned).
    forge: git_forgejo::Backend,
    /// Forge client authenticated as the agent/bot identity
    /// (`TASK_FORGEJO_BOT_TOKEN`). Used for tasks whose primary
    /// assignee is an agent, so agent-driven issues are attributed
    /// to the bot account and filterable apart from human work.
    /// Falls back to the human client when no bot token is set.
    forge_agent: git_forgejo::Backend,
    slug: String,
    issue_links_path: PathBuf,
}

impl ForgeSyncTaskService {
    pub fn new(
        inner: task::TaskBackend,
        forge: git_forgejo::Backend,
        forge_agent: git_forgejo::Backend,
        slug: String,
        issue_links_path: PathBuf,
    ) -> Self {
        Self {
            inner,
            forge,
            forge_agent,
            slug,
            issue_links_path,
        }
    }

    /// Pick the forge identity for a task: the bot client when the
    /// task's primary assignee is an agent, the human client
    /// otherwise. Attribution on the forge then mirrors who owns
    /// the work locally.
    fn backend_for(&self, task: &TaskInfo) -> &git_forgejo::Backend {
        if task_actor_is_agent(task) {
            &self.forge_agent
        } else {
            &self.forge
        }
    }

    /// Open the per-org link store fresh per call — the CLI writes
    /// the same JSON file, so we never hold a stale in-memory view.
    fn store(&self) -> Result<FileStore, git_config::ConfigError> {
        FileStore::open(&self.issue_links_path)
    }

    /// On create: if the task's project binds to a Forgejo repo and
    /// the task isn't already linked, create the issue + record the
    /// link and a converged baseline snapshot.
    fn push_created(&self, task: &TaskInfo) -> eyre::Result<()> {
        let Some(project_id) = task.project_id else {
            return Ok(()); // no project → nothing bound
        };
        let store = self.store()?;
        let repos = store.repos_for_project(&project_id.to_string())?;
        // Server-side push is Forgejo-only for now (codeberg), and
        // only to the host this server's backend is configured for.
        let Some(repo) = repos.into_iter().find(repo_on_configured_forge) else {
            return Ok(());
        };
        let task_id = task.id.to_string();
        // Idempotency: never create a second issue for a task already
        // linked to this repo (a retry, or a poll racing the create).
        if store
            .issues_for_task(&task_id)?
            .iter()
            .any(|l| l.repo == repo && l.kind == LinkKind::Issue)
        {
            return Ok(());
        }

        let issue = self
            .backend_for(task)
            .create_issue(&repo, task.title.clone(), task.details.clone())
            .map_err(|e| eyre::eyre!("create_issue: {e:?}"))?;
        store.add_issue_link(IssueLink {
            task_id,
            repo: repo.clone(),
            number: issue.id.0,
            kind: LinkKind::Issue,
        })?;
        // Baseline: each side matches its own current value, so the
        // first poll is a guaranteed no-op (no ping-pong).
        store.set_issue_snapshot(
            &repo,
            issue.id.0,
            IssueSnapshot {
                task: task_synced(task),
                forge: issue_synced(&issue),
                forge_updated_at: parse_dt(issue.updated_at.as_deref()),
            },
        )?;
        tracing::info!(
            org = %self.slug,
            task = %task.id,
            issue = issue.id.0,
            repo = %format!("{}/{}", repo.owner, repo.repo),
            "auto-created forge issue for new task",
        );
        Ok(())
    }

    /// On update: reconcile the linked issue. Returns the task to
    /// write back when the forge won a field (the caller writes it
    /// through the raw backend to avoid re-triggering this hook).
    fn reconcile_after_update(&self, task: &TaskInfo) -> eyre::Result<Option<TaskInfo>> {
        let store = self.store()?;
        let links = store.issues_for_task(&task.id.to_string())?;
        let Some(link) = links
            .into_iter()
            .find(|l| l.kind == LinkKind::Issue && repo_on_configured_forge(&l.repo))
        else {
            return Ok(None);
        };
        reconcile_one(self.backend_for(task), &store, &link, task)
    }
}

impl architect::HasDispatcher for ForgeSyncTaskService {
    type Dispatcher = architect::dispatch::TokioBlockingDispatcher;
    fn dispatcher(&self) -> Self::Dispatcher {
        architect::dispatch::TokioBlockingDispatcher
    }
}

impl TaskService for ForgeSyncTaskService {
    fn list(&self) -> Result<Vec<TaskInfo>, TaskError> {
        self.inner.list()
    }

    fn get(&self, id: Uuid) -> Result<TaskInfo, TaskError> {
        self.inner.get(id)
    }

    fn get_by_path(&self, path: &str) -> Result<TaskInfo, TaskError> {
        self.inner.get_by_path(path)
    }

    fn create(&self, task: TaskInfo) -> Result<TaskInfo, TaskError> {
        let created = self.inner.create(task)?;
        if let Err(e) = self.push_created(&created) {
            tracing::warn!(
                org = %self.slug,
                task = %created.id,
                error = %e,
                "forge auto-push (create) failed; poll loop will reconcile",
            );
        }
        Ok(created)
    }

    fn update(&self, task: TaskInfo) -> Result<TaskInfo, TaskError> {
        let updated = self.inner.update(task)?;
        match self.reconcile_after_update(&updated) {
            Ok(Some(merged)) => {
                // Forge won a field — persist it locally via the raw
                // backend so we don't recurse into this hook.
                self.inner.update(merged)
            }
            Ok(None) => Ok(updated),
            Err(e) => {
                tracing::warn!(
                    org = %self.slug,
                    task = %updated.id,
                    error = %e,
                    "forge sync (update) failed; poll loop will reconcile",
                );
                Ok(updated)
            }
        }
    }

    fn try_claim(&self, id: Uuid, agent: String, force: bool) -> Result<ClaimResult, TaskError> {
        self.inner.try_claim(id, agent, force)
    }

    fn reverse_relations(&self, id: Uuid) -> Result<Vec<task::ReverseRelation>, TaskError> {
        self.inner.reverse_relations(id)
    }

    fn rename(&self, id: Uuid, new_path: &str) -> Result<TaskInfo, TaskError> {
        self.inner.rename(id, new_path)
    }

    fn delete(&self, id: Uuid) -> Result<(), TaskError> {
        self.inner.delete(id)
    }
}

// ── Shared reconcile (decorator update + poll loop) ───────────────────

/// Whether a task's primary actor is an agent — the signal that
/// routes its forge actions to the bot identity. Keys on the first
/// assignee's [`workflows_proto::AgentRef`] (set by `try_claim` /
/// the `task code` loop). Unassigned or human-assigned ⇒ human.
fn task_actor_is_agent(t: &TaskInfo) -> bool {
    t.workflow
        .as_ref()
        .and_then(|w| w.assignees.0.first())
        .is_some_and(|a| a.is_agent())
}

/// The scalar projection of a local task: title, body, open/closed.
fn task_synced(t: &TaskInfo) -> SyncedFields {
    SyncedFields {
        title: t.title.clone(),
        body: t.details.clone(),
        closed: t.status == "done",
    }
}

/// The scalar projection of a forge issue.
fn issue_synced(i: &Issue) -> SyncedFields {
    SyncedFields {
        title: i.title.clone(),
        body: i.body.clone(),
        closed: i.state == IssueState::Closed,
    }
}

/// The server's single forge backend carries one base URL + token
/// (`TASK_FORGEJO_BASE_URL`). A repo is syncable by it only when it's
/// a Forgejo repo on that host — otherwise we'd fire requests at a
/// different forge with the wrong token. Critically this skips the
/// org's pre-existing links to other Forgejo hosts (e.g. the old
/// `git.starcommand.live` links) so the poll loop doesn't hammer them
/// every cycle. An empty link base is treated as "the configured
/// host"; an empty configured base disables the guard (dev/test).
fn repo_on_configured_forge(repo: &RepoId) -> bool {
    match &repo.forge {
        Forge::Forgejo { base_url } => {
            let configured = std::env::var("TASK_FORGEJO_BASE_URL").unwrap_or_default();
            let configured = configured.trim_end_matches('/');
            configured.is_empty()
                || base_url.is_empty()
                || base_url.trim_end_matches('/') == configured
        }
        // GitHub bindings aren't synced server-side yet (single
        // Forgejo backend); a per-repo GitHub resolver is follow-up.
        Forge::Github => false,
    }
}

fn parse_dt(s: Option<&str>) -> Option<DateTime<Utc>> {
    s.and_then(|s| DateTime::parse_from_rfc3339(s).ok())
        .map(|dt| dt.with_timezone(&Utc))
}

/// Reconcile one linked `TaskInfo` ↔ forge issue. Pushes any
/// task-won forge-owned fields back to the forge, records the new
/// converged snapshot, and returns the task with forge-won fields
/// applied when the local side needs updating (else `None`).
///
/// Sync (the forge calls `block_on` internally); call from a
/// blocking context — the `TokioBlockingDispatcher` handler thread
/// or `spawn_blocking`.
fn reconcile_one(
    forge: &git_forgejo::Backend,
    store: &FileStore,
    link: &IssueLink,
    task: &TaskInfo,
) -> eyre::Result<Option<TaskInfo>> {
    let task_sf = task_synced(task);
    let issue = forge
        .get_issue(&link.repo, IssueId(link.number))
        .map_err(|e| eyre::eyre!("get_issue #{}: {e:?}", link.number))?;
    let forge_sf = issue_synced(&issue);

    // No snapshot yet ⇒ first contact: seed the baseline from the
    // task on both sides so a divergent forge wins (forge owns these
    // fields), matching the CLI's first-reconcile behaviour.
    let (base_task, base_forge) = match store.get_issue_snapshot(&link.repo, link.number)? {
        Some(s) => (s.task, s.forge),
        None => (task_sf.clone(), task_sf.clone()),
    };

    let merged = reconcile_synced(&base_task, &base_forge, &task_sf, &forge_sf);

    // Push task-won fields back to the forge.
    let mut forge_updated_at = parse_dt(issue.updated_at.as_deref());
    let fu: ForgeUpdate = forge_update(&forge_sf, &merged);
    if !fu.is_empty() {
        let upd = IssueUpdate {
            title: fu.title,
            body: fu.body,
            state: fu.closed.map(|c| {
                if c {
                    IssueState::Closed
                } else {
                    IssueState::Open
                }
            }),
            ..Default::default()
        };
        let issue2 = forge
            .update_issue(&link.repo, IssueId(link.number), upd)
            .map_err(|e| eyre::eyre!("update_issue #{}: {e:?}", link.number))?;
        forge_updated_at = parse_dt(issue2.updated_at.as_deref());
    }

    // Both sides have now converged on `merged`.
    store.set_issue_snapshot(
        &link.repo,
        link.number,
        IssueSnapshot {
            task: merged.clone(),
            forge: merged.clone(),
            forge_updated_at,
        },
    )?;

    if merged == task_sf {
        return Ok(None);
    }

    // Apply forge-won fields to the local task.
    let mut t = task.clone();
    if t.title != merged.title {
        t.title = merged.title.clone();
    }
    if t.details != merged.body {
        t.details = merged.body.clone();
    }
    let is_done = t.status == "done";
    if merged.closed != is_done {
        if merged.closed {
            t.status = "done".to_string();
            t.completed_date = Some(chrono::Local::now().date_naive());
            if let Some(w) = t.workflow.as_mut() {
                w.session = None;
            }
        } else {
            t.status = "open".to_string();
            t.completed_date = None;
        }
    }
    Ok(Some(t))
}

// ── Poll loop (inbound forge → local) ─────────────────────────────────

/// Spawn the background poll loop. Reconciles every linked task in
/// every hosted org on an interval (`TASK_FORGE_POLL_SECS`, default
/// 60; `0` disables). Lives for the process lifetime.
pub fn spawn_poll_loop(state: AppState) {
    let secs = std::env::var("TASK_FORGE_POLL_SECS")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(60);
    if secs == 0 {
        tracing::info!("forge poll loop disabled (TASK_FORGE_POLL_SECS=0)");
        return;
    }
    tracing::info!(interval_secs = secs, "forge poll loop started");
    tokio::spawn(async move {
        let mut tick = tokio::time::interval(Duration::from_secs(secs));
        tick.tick().await; // consume the immediate first tick
        loop {
            tick.tick().await;
            // Scope the read guard so it's dropped before any await —
            // `RwLockReadGuard` isn't `Send` and the future must be.
            let orgs: Vec<OrgAppState> = {
                let Ok(guard) = state.orgs.read() else {
                    tracing::warn!("orgs lock poisoned; skipping poll cycle");
                    continue;
                };
                guard.values().cloned().collect()
            };
            for org in orgs {
                // Forge + vault calls are sync (block_on inside) —
                // run the whole per-org pass on a blocking thread.
                if let Err(e) = tokio::task::spawn_blocking(move || poll_org(&org)).await {
                    tracing::warn!(error = %e, "forge poll task join failed");
                }
            }
        }
    });
}

/// One reconcile pass over a single org's linked tasks. Best-effort:
/// per-task / per-link failures are logged and skipped.
fn poll_org(org: &OrgAppState) {
    let store = match FileStore::open(&org.issue_links_path) {
        Ok(s) => s,
        Err(e) => {
            tracing::warn!(org = %org.slug, error = %e, "poll: open link store failed");
            return;
        }
    };
    let tasks = match org.tasks.list() {
        Ok(t) => t,
        Err(e) => {
            tracing::warn!(org = %org.slug, error = ?e, "poll: list tasks failed");
            return;
        }
    };
    for t in tasks {
        let links = match store.issues_for_task(&t.id.to_string()) {
            Ok(l) => l,
            Err(_) => continue,
        };
        for link in links {
            if link.kind != LinkKind::Issue || !repo_on_configured_forge(&link.repo) {
                continue;
            }
            // Route forge writes through the identity that owns the
            // task, same as the inline push path.
            let backend = if task_actor_is_agent(&t) {
                &org.forge_agent
            } else {
                &org.forge
            };
            match reconcile_one(backend, &store, &link, &t) {
                Ok(Some(merged)) => {
                    let id = merged.id;
                    if let Err(e) = org.tasks.update(merged) {
                        tracing::warn!(org = %org.slug, task = %id, error = ?e, "poll: apply forge change failed");
                    } else {
                        tracing::info!(org = %org.slug, task = %id, issue = link.number, "poll: pulled forge change into task");
                    }
                }
                Ok(None) => {}
                Err(e) => {
                    tracing::warn!(org = %org.slug, issue = link.number, error = %e, "poll: reconcile failed");
                }
            }
        }
    }
}
