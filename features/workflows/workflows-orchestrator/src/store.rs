//! `WorkflowStore` — a per-org JSON file store for the workflow
//! audit log.
//!
//! Deliberately the same shape as the CLI's existing
//! `handoffs.json` store (atomic temp-file + rename), just widened
//! to cover all four row kinds under one directory:
//!
//! ```text
//! ~/.task/orgs/<slug>/workflows/
//!   sessions.json      Vec<WorkSession>
//!   transitions.json   Vec<Transition>
//!   activities.json    Vec<Activity>
//!   handoffs.json      Vec<Handoff>
//! ```
//!
//! Sessions are inherently local to the agent doing the work, so a
//! flat per-org file store is the right weight today. A CRDT/SeaORM
//! backend (the `repo` attribute on the proto entities anticipates
//! it) is deferred until sessions actually need to sync across
//! clients.

use std::path::{Path, PathBuf};

use serde::Serialize;
use serde::de::DeserializeOwned;
use uuid::Uuid;
use workflows_proto::{Activity, GoalSession, Handoff, Transition, WorkSession, WorkflowError};

/// Maps any IO / codec failure onto [`WorkflowError::Backend`].
fn backend<E: std::fmt::Display>(ctx: &str) -> impl Fn(E) -> WorkflowError + '_ {
    move |e| WorkflowError::Backend(format!("{ctx}: {e}"))
}

const SESSIONS: &str = "sessions.json";
const TRANSITIONS: &str = "transitions.json";
const ACTIVITIES: &str = "activities.json";
const HANDOFFS: &str = "handoffs.json";
const GOALS: &str = "goals.json";

/// File-backed store rooted at one org's `workflows/` directory.
#[derive(Debug, Clone)]
pub struct WorkflowStore {
    root: PathBuf,
}

impl WorkflowStore {
    /// Open (lazily — nothing is read until a getter is called) the
    /// store rooted at `dir`. The directory is created on first write.
    pub fn open(dir: impl Into<PathBuf>) -> Self {
        Self { root: dir.into() }
    }

    fn path(&self, file: &str) -> PathBuf {
        self.root.join(file)
    }

    /// Read a whole table. Missing file → empty vec.
    fn load<T: DeserializeOwned>(&self, file: &str) -> Result<Vec<T>, WorkflowError> {
        let p = self.path(file);
        if !p.exists() {
            return Ok(Vec::new());
        }
        let bytes = std::fs::read(&p).map_err(backend("read"))?;
        serde_json::from_slice(&bytes).map_err(backend(file))
    }

    /// Replace a whole table, atomically (temp file + rename).
    fn save<T: Serialize>(&self, file: &str, rows: &[T]) -> Result<(), WorkflowError> {
        std::fs::create_dir_all(&self.root).map_err(backend("mkdir"))?;
        let p = self.path(file);
        let tmp = p.with_extension("json.tmp");
        let json = serde_json::to_vec_pretty(rows).map_err(backend("encode"))?;
        std::fs::write(&tmp, json).map_err(backend("write"))?;
        std::fs::rename(&tmp, &p).map_err(backend("rename"))?;
        Ok(())
    }

    // ── sessions ─────────────────────────────────────────────

    pub fn sessions(&self) -> Result<Vec<WorkSession>, WorkflowError> {
        self.load(SESSIONS)
    }

    pub fn session(&self, id: Uuid) -> Result<WorkSession, WorkflowError> {
        self.sessions()?
            .into_iter()
            .find(|s| s.id == id)
            .ok_or(WorkflowError::SessionNotFound(id))
    }

    /// Insert or replace a session by id.
    pub fn put_session(&self, session: &WorkSession) -> Result<(), WorkflowError> {
        let mut rows = self.sessions()?;
        match rows.iter_mut().find(|s| s.id == session.id) {
            Some(slot) => *slot = session.clone(),
            None => rows.push(session.clone()),
        }
        self.save(SESSIONS, &rows)
    }

    // ── transitions ──────────────────────────────────────────

    /// All transitions for `session`, oldest first.
    pub fn transitions_for(&self, session: Uuid) -> Result<Vec<Transition>, WorkflowError> {
        let mut rows: Vec<Transition> = self
            .load::<Transition>(TRANSITIONS)?
            .into_iter()
            .filter(|t| t.session_id == session)
            .collect();
        rows.sort_by_key(|t| t.at);
        Ok(rows)
    }

    pub fn push_transition(&self, t: &Transition) -> Result<(), WorkflowError> {
        let mut rows = self.load::<Transition>(TRANSITIONS)?;
        rows.push(t.clone());
        self.save(TRANSITIONS, &rows)
    }

    // ── activities ───────────────────────────────────────────

    /// All activity for `session`, newest first.
    pub fn activities_for(&self, session: Uuid) -> Result<Vec<Activity>, WorkflowError> {
        let mut rows: Vec<Activity> = self
            .load::<Activity>(ACTIVITIES)?
            .into_iter()
            .filter(|a| a.session_id == session)
            .collect();
        rows.sort_by(|a, b| b.at.cmp(&a.at));
        Ok(rows)
    }

    pub fn push_activity(&self, a: &Activity) -> Result<(), WorkflowError> {
        let mut rows = self.load::<Activity>(ACTIVITIES)?;
        rows.push(a.clone());
        self.save(ACTIVITIES, &rows)
    }

    // ── goal sessions ────────────────────────────────────────

    /// Every persisted goal-loop row.
    pub fn goals(&self) -> Result<Vec<GoalSession>, WorkflowError> {
        self.load(GOALS)
    }

    /// The goal row for `session`, if the session is a goal loop.
    pub fn goal(&self, session: Uuid) -> Result<GoalSession, WorkflowError> {
        self.goals()?
            .into_iter()
            .find(|g| g.session_id == session)
            .ok_or(WorkflowError::SessionNotFound(session))
    }

    /// Insert or replace a goal row by `session_id`.
    pub fn put_goal(&self, goal: &GoalSession) -> Result<(), WorkflowError> {
        let mut rows = self.goals()?;
        match rows.iter_mut().find(|g| g.session_id == goal.session_id) {
            Some(slot) => *slot = goal.clone(),
            None => rows.push(goal.clone()),
        }
        self.save(GOALS, &rows)
    }

    /// Drop the goal row for `session` (no-op if absent).
    pub fn remove_goal(&self, session: Uuid) -> Result<(), WorkflowError> {
        let mut rows = self.goals()?;
        rows.retain(|g| g.session_id != session);
        self.save(GOALS, &rows)
    }

    // ── handoffs ─────────────────────────────────────────────

    pub fn handoffs(&self) -> Result<Vec<Handoff>, WorkflowError> {
        self.load(HANDOFFS)
    }

    pub fn save_handoffs(&self, rows: &[Handoff]) -> Result<(), WorkflowError> {
        self.save(HANDOFFS, rows)
    }

    /// Convenience: the directory this store writes to.
    #[must_use]
    pub fn root(&self) -> &Path {
        &self.root
    }
}
