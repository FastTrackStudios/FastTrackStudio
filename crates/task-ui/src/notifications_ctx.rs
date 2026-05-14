//! Client-side notifications glue.
//!
//! Mirrors the server-side `NotificationRouter`'s status-transition →
//! notification logic in the browser, sitting on top of the existing
//! Loro-backed `AgentRunRepoLoro`. The dashboard route already polls
//! the repo on every WS sync round-trip; this module watches each
//! refresh, diffs against the previous status snapshot, and pushes
//! `Notification` rows into a shared signal that:
//!
//! - drives the `NotificationBell` unread count
//! - feeds `ToastStack` (top-right toasts)
//! - fires the browser Notification API when permission is granted
//!
//! Server-side bus → client-side bus over WS is the proper long-term
//! transport; this client-only mirror lets us dogfood the feature
//! today without waiting on that wiring.

use std::collections::HashMap;

use agent_proto::AgentRun;
use chrono::Utc;
use dioxus::prelude::*;
use notifications_proto::Notification;
use uuid::Uuid;

/// Shared signals for the inbox + toast stack. Provided as a Dioxus
/// context so the bell, inbox route, and toast mount all see the
/// same data.
#[derive(Clone, Copy)]
pub struct NotificationsCtx {
    pub inbox: Signal<Vec<Notification>>,
    pub toasts: Signal<Vec<Notification>>,
}

impl NotificationsCtx {
    pub fn new() -> Self {
        Self {
            inbox: Signal::new(Vec::new()),
            toasts: Signal::new(Vec::new()),
        }
    }

    pub fn unread_count(&self) -> u32 {
        self.inbox
            .read()
            .iter()
            .filter(|n| n.read_at.is_none())
            .count() as u32
    }

    pub fn push(&mut self, n: Notification) {
        // Newest first in the inbox.
        let mut inbox = self.inbox.write();
        inbox.insert(0, n.clone());
        drop(inbox);

        // Append to toasts; the ToastStack auto-drops via `dismiss`.
        let mut toasts = self.toasts.write();
        toasts.push(n);
    }

    pub fn mark_read(&mut self, id: Uuid) {
        let mut inbox = self.inbox.write();
        if let Some(n) = inbox.iter_mut().find(|n| n.id == id) {
            n.read_at = Some(Utc::now());
            n.updated_at = Utc::now();
        }
    }

    pub fn mark_all_read(&mut self) {
        let now = Utc::now();
        let mut inbox = self.inbox.write();
        for n in inbox.iter_mut() {
            if n.read_at.is_none() {
                n.read_at = Some(now);
                n.updated_at = now;
            }
        }
    }

    pub fn dismiss_toast(&mut self, id: Uuid) {
        let mut toasts = self.toasts.write();
        toasts.retain(|n| n.id != id);
    }

    pub fn dismiss_inbox(&mut self, id: Uuid) {
        let now = Utc::now();
        let mut inbox = self.inbox.write();
        if let Some(n) = inbox.iter_mut().find(|n| n.id == id) {
            n.dismissed_at = Some(now);
            n.updated_at = now;
        }
    }
}

impl Default for NotificationsCtx {
    fn default() -> Self {
        Self::new()
    }
}

/// Map an `AgentRun.status` string to (kind, title prefix, severity)
/// for the subset of statuses that fire notifications.
fn run_status_to_notification(status: &str) -> Option<(&'static str, &'static str, &'static str)> {
    match status {
        "completed" => Some(("run.completed", "Run completed", "info")),
        "failed" => Some(("run.failed", "Run failed", "error")),
        "cancelled" => Some(("run.cancelled", "Run cancelled", "info")),
        "timed-out" => Some(("run.timed-out", "Run timed out", "error")),
        "awaiting-input" => Some(("run.awaiting-input", "Awaiting your input", "warning")),
        "paused" => Some(("run.paused", "Run paused", "info")),
        _ => None,
    }
}

/// Track previous statuses and emit a `Notification` per transition.
/// Returns the new statuses we should remember on the next call.
pub fn diff_runs_for_notifications(
    prev: &HashMap<Uuid, String>,
    current: &[AgentRun],
) -> (Vec<Notification>, HashMap<Uuid, String>) {
    let mut next: HashMap<Uuid, String> = HashMap::with_capacity(current.len());
    let mut emit = Vec::new();
    for run in current {
        next.insert(run.id, run.status.clone());
        let was = prev.get(&run.id);
        if was == Some(&run.status) {
            continue;
        }
        let Some((kind, title_prefix, severity)) = run_status_to_notification(&run.status) else {
            continue;
        };
        let now = Utc::now();
        emit.push(Notification {
            id: Uuid::new_v4(),
            kind: kind.to_string(),
            title: format!("{title_prefix}: {}", run.name),
            body: run.error_message.clone().unwrap_or_default(),
            severity: severity.to_string(),
            entity_kind: "agent_run".to_string(),
            entity_id: Some(run.id),
            action_url: Some(format!("/agent/dashboard/{}", run.id)),
            dedup_key: Some(format!("{kind}.{}", run.id)),
            read_at: None,
            dismissed_at: None,
            created_at: now,
            updated_at: now,
        });
    }
    (emit, next)
}

#[cfg(test)]
mod tests {
    use super::*;
    use agent_proto::AgentRunCreate;

    fn make_run(id: Uuid, status: &str) -> AgentRun {
        let now = Utc::now();
        AgentRun {
            id,
            name: "test run".into(),
            kind: "hermes".into(),
            prompt: "x".into(),
            status: status.into(),
            task_id: None,
            started_at: None,
            completed_at: None,
            result: None,
            error_message: None,
            tokens_used: None,
            cost_cents: None,
            tags: Vec::new(),
            integration: None,
            external_id: None,
            external_url: None,
            log_cursor: None,
            parent_run_id: None,
            worktree_path: None,
            git_repo_connection_id: None,
            spawned_from_message_id: None,
            input_tokens: None,
            output_tokens: None,
            cache_read_tokens: None,
            cache_creation_tokens: None,
            cost_cents_estimate: None,
            tool_call_count: 0,
            assistant_message_count: 0,
            max_tokens: None,
            max_tool_calls: None,
            max_wall_seconds: None,
            created_at: now,
            updated_at: now,
        }
    }

    #[test]
    fn transition_running_to_completed_emits_one() {
        let id = Uuid::new_v4();
        let mut prev = HashMap::new();
        prev.insert(id, "running".into());
        let (out, next) = diff_runs_for_notifications(&prev, &[make_run(id, "completed")]);
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].kind, "run.completed");
        assert_eq!(next.get(&id).map(|s| s.as_str()), Some("completed"));
    }

    #[test]
    fn no_transition_no_emit() {
        let id = Uuid::new_v4();
        let mut prev = HashMap::new();
        prev.insert(id, "completed".into());
        let (out, _) = diff_runs_for_notifications(&prev, &[make_run(id, "completed")]);
        assert!(out.is_empty());
    }

    #[test]
    fn running_to_running_no_emit() {
        let id = Uuid::new_v4();
        let mut prev = HashMap::new();
        prev.insert(id, "queued".into());
        let (out, _) = diff_runs_for_notifications(&prev, &[make_run(id, "running")]);
        assert!(out.is_empty(), "running is not in the notification set");
    }

    #[test]
    fn failed_emits_error_severity() {
        let id = Uuid::new_v4();
        let mut prev = HashMap::new();
        prev.insert(id, "running".into());
        let mut run = make_run(id, "failed");
        run.error_message = Some("boom".into());
        let (out, _) = diff_runs_for_notifications(&prev, &[run]);
        assert_eq!(out[0].severity, "error");
        assert_eq!(out[0].body, "boom");
    }

    #[test]
    fn brand_new_terminal_run_still_emits() {
        // A run we've never seen before that's already completed —
        // happens when the user opens the dashboard fresh and a run
        // finished while they were away.
        let id = Uuid::new_v4();
        let prev = HashMap::new();
        let (out, _) = diff_runs_for_notifications(&prev, &[make_run(id, "completed")]);
        assert_eq!(out.len(), 1);
    }
}
