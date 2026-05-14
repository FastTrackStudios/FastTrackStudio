//! `NotificationRouter` — the server-side pipeline that watches the
//! agent live-update bus, translates status transitions into
//! `Notification` rows, applies user-configured rules, and dispatches
//! each match through its channel.
//!
//! MVP delivery: a single in-process `ToastBus` (broadcast channel)
//! that the dashboard subscribes to for instant browser toasts.
//! Real channel adapters (browser push, libnotify, hermes-relay)
//! plug in via the [`ChannelDeliver`] trait without changing the
//! router.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use agent::LiveUpdateBus;
use agent_proto::AgentEvent;
use chrono::{DateTime, Utc};
use notifications_proto::{
    Notification, NotificationChannel, NotificationRule, kind_matches, severity_meets,
};
use tokio::sync::broadcast;
use tracing::{debug, warn};
use uuid::Uuid;

/// One `Notification` plus the channel it was dispatched through.
/// What `ToastBus` carries to the dashboard.
#[derive(Clone, Debug)]
pub struct DeliveredNotification {
    pub notification: Notification,
    pub channel_kind: String,
}

/// Channel adapter contract. Each registered channel knows how to
/// deliver one notification through its native surface (browser
/// push, libnotify, hermes-relay, …). MVP ships only the toast bus;
/// future channels implement this trait + register on `Router::new`.
pub trait ChannelDeliver: Send + Sync {
    /// Channel kind this adapter handles. The router consults this on
    /// dispatch.
    fn kind(&self) -> &'static str;

    /// Best-effort delivery. Errors are logged and discarded — a
    /// failing channel does NOT block the rest of the dispatch fanout.
    fn deliver(&self, channel: &NotificationChannel, notification: &Notification);
}

/// In-process broadcast for `browser-toast` deliveries. The dashboard
/// subscribes once per session and renders toasts as they arrive.
#[derive(Clone)]
pub struct ToastBus {
    tx: broadcast::Sender<DeliveredNotification>,
}

impl Default for ToastBus {
    fn default() -> Self {
        Self::new()
    }
}

impl ToastBus {
    pub fn new() -> Self {
        let (tx, _) = broadcast::channel(256);
        Self { tx }
    }

    pub fn subscribe(&self) -> ToastSubscription {
        ToastSubscription {
            rx: self.tx.subscribe(),
        }
    }
}

pub struct ToastSubscription {
    rx: broadcast::Receiver<DeliveredNotification>,
}

impl ToastSubscription {
    pub async fn recv(&mut self) -> Result<DeliveredNotification, broadcast::error::RecvError> {
        self.rx.recv().await
    }
}

/// `browser-toast` channel adapter — publishes to a `ToastBus`.
pub struct ToastChannel {
    bus: ToastBus,
}

impl ToastChannel {
    pub fn new(bus: ToastBus) -> Self {
        Self { bus }
    }
}

impl ChannelDeliver for ToastChannel {
    fn kind(&self) -> &'static str {
        "browser-toast"
    }

    fn deliver(&self, _channel: &NotificationChannel, notification: &Notification) {
        let _ = self.bus.tx.send(DeliveredNotification {
            notification: notification.clone(),
            channel_kind: "browser-toast".to_string(),
        });
    }
}

/// Default-rules bundle: ships a single `browser-toast` channel + one
/// `*` rule routing everything to it, so a fresh install fires toasts
/// without the user touching settings.
pub struct DefaultRules {
    pub channel: NotificationChannel,
    pub rule: NotificationRule,
}

impl DefaultRules {
    pub fn new() -> Self {
        let channel_id = Uuid::new_v4();
        let now = Utc::now();
        let channel = NotificationChannel {
            id: channel_id,
            kind: "browser-toast".into(),
            label: "Browser toast".into(),
            enabled: true,
            config_json: "{}".into(),
            created_at: now,
            updated_at: now,
        };
        let rule = NotificationRule {
            id: Uuid::new_v4(),
            when_kind: "*".into(),
            min_severity: "info".into(),
            to_channel_id: channel_id,
            enabled: true,
            priority: 0,
            created_at: now,
            updated_at: now,
        };
        Self { channel, rule }
    }
}

impl Default for DefaultRules {
    fn default() -> Self {
        Self::new()
    }
}

/// Last-seen status per run, so we only emit a notification on the
/// transition (not every patch_run write).
type StatusCache = Arc<Mutex<HashMap<Uuid, String>>>;

/// In-memory dedup map: `dedup_key → created_at`. Suppresses repeat
/// emits within `DEDUP_WINDOW`.
type DedupCache = Arc<Mutex<HashMap<String, DateTime<Utc>>>>;

const DEDUP_WINDOW: Duration = Duration::from_secs(5);

pub struct NotificationRouter {
    channels: Vec<NotificationChannel>,
    rules: Vec<NotificationRule>,
    adapters: HashMap<String, Arc<dyn ChannelDeliver>>,
    status_cache: StatusCache,
    dedup: DedupCache,
}

impl NotificationRouter {
    /// Build a router with the default `browser-toast` channel/rule
    /// pre-installed.
    pub fn with_defaults(toast_bus: ToastBus) -> Self {
        let defaults = DefaultRules::new();
        let mut adapters: HashMap<String, Arc<dyn ChannelDeliver>> = HashMap::new();
        adapters.insert(
            "browser-toast".to_string(),
            Arc::new(ToastChannel::new(toast_bus)),
        );
        Self {
            channels: vec![defaults.channel],
            rules: vec![defaults.rule],
            adapters,
            status_cache: Arc::new(Mutex::new(HashMap::new())),
            dedup: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Register an extra delivery adapter (browser-push, hermes-relay,
    /// etc.) Tests + the server `main` use this to wire real channels.
    pub fn register_adapter(&mut self, adapter: Arc<dyn ChannelDeliver>) {
        self.adapters.insert(adapter.kind().to_string(), adapter);
    }

    /// Add a channel + rule pair the user configured.
    pub fn add_channel(&mut self, channel: NotificationChannel) {
        self.channels.push(channel);
    }
    pub fn add_rule(&mut self, rule: NotificationRule) {
        self.rules.push(rule);
    }

    /// Spawn the long-running task that pulls from the agent
    /// `LiveUpdateBus` and emits notifications on status transitions.
    /// Returns the JoinHandle so the caller can await shutdown.
    pub fn spawn(self: Arc<Self>, bus: &LiveUpdateBus) -> tokio::task::JoinHandle<()> {
        let mut subscription = bus.subscribe_workspace();
        let this = self.clone();
        tokio::spawn(async move {
            loop {
                match subscription.recv_batch().await {
                    Ok(batch) => {
                        for event in batch {
                            this.handle_event(&event);
                        }
                    }
                    Err(e) => {
                        warn!(?e, "notification router subscription closed");
                        break;
                    }
                }
            }
        })
    }

    /// Process one bus event. Public for testing.
    pub fn handle_event(&self, event: &AgentEvent) -> Vec<DeliveredNotification> {
        match event {
            AgentEvent::RunStateChanged { run_id, new_status } => {
                self.handle_status_change(*run_id, new_status)
            }
            // Log + tool-call events don't fire notifications in MVP.
            _ => Vec::new(),
        }
    }

    fn handle_status_change(&self, run_id: Uuid, new_status: &str) -> Vec<DeliveredNotification> {
        // Only emit on actual transitions, not repeat writes.
        let prev = {
            let mut cache = self.status_cache.lock().unwrap();
            cache.insert(run_id, new_status.to_string())
        };
        if prev.as_deref() == Some(new_status) {
            return Vec::new();
        }

        let (kind, title, severity) = match new_status {
            "completed" => ("run.completed", "Run completed", "info"),
            "failed" => ("run.failed", "Run failed", "error"),
            "cancelled" => ("run.cancelled", "Run cancelled", "info"),
            "timed-out" => ("run.timed-out", "Run timed out", "error"),
            "awaiting-input" => ("run.awaiting-input", "Awaiting your input", "warning"),
            "paused" => ("run.paused", "Run paused", "info"),
            // Don't notify for running/queued/starting — too noisy.
            _ => return Vec::new(),
        };

        let dedup_key = format!("{kind}.{run_id}");
        if !self.passes_dedup(&dedup_key) {
            debug!(%dedup_key, "deduped notification within window");
            return Vec::new();
        }

        let now = Utc::now();
        let notification = Notification {
            id: Uuid::new_v4(),
            kind: kind.to_string(),
            title: format!("{title}: {run_id}"),
            body: String::new(),
            severity: severity.to_string(),
            entity_kind: "agent_run".to_string(),
            entity_id: Some(run_id),
            action_url: Some(format!("/agent/dashboard/{run_id}")),
            dedup_key: Some(dedup_key),
            read_at: None,
            dismissed_at: None,
            created_at: now,
            updated_at: now,
        };

        self.dispatch(&notification)
    }

    fn passes_dedup(&self, key: &str) -> bool {
        let mut cache = self.dedup.lock().unwrap();
        let now = Utc::now();
        // Prune old entries opportunistically.
        cache.retain(|_, ts| (now - *ts).num_milliseconds() < DEDUP_WINDOW.as_millis() as i64);
        if cache.contains_key(key) {
            return false;
        }
        cache.insert(key.to_string(), now);
        true
    }

    fn dispatch(&self, notification: &Notification) -> Vec<DeliveredNotification> {
        let mut matched_rules: Vec<&NotificationRule> = self
            .rules
            .iter()
            .filter(|r| r.enabled)
            .filter(|r| kind_matches(&r.when_kind, &notification.kind))
            .filter(|r| severity_meets(&notification.severity, &r.min_severity))
            .collect();
        matched_rules.sort_by_key(|r| r.priority);

        let mut delivered = Vec::new();
        for rule in matched_rules {
            let Some(channel) = self
                .channels
                .iter()
                .find(|c| c.id == rule.to_channel_id && c.enabled)
            else {
                continue;
            };
            let Some(adapter) = self.adapters.get(&channel.kind) else {
                debug!(channel_kind = %channel.kind, "no adapter for channel kind; skipping");
                continue;
            };
            adapter.deliver(channel, notification);
            delivered.push(DeliveredNotification {
                notification: notification.clone(),
                channel_kind: channel.kind.clone(),
            });
        }
        delivered
    }

    /// Read-only views for the inbox / settings UIs.
    pub fn channels(&self) -> &[NotificationChannel] {
        &self.channels
    }
    pub fn rules(&self) -> &[NotificationRule] {
        &self.rules
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_status_event(run_id: Uuid, status: &str) -> AgentEvent {
        AgentEvent::RunStateChanged {
            run_id,
            new_status: status.to_string(),
        }
    }

    #[tokio::test]
    async fn completed_run_emits_one_notification() {
        let bus = ToastBus::new();
        let router = NotificationRouter::with_defaults(bus.clone());
        let run_id = Uuid::new_v4();

        // Bring the run to running first (no notification expected).
        let n1 = router.handle_event(&run_status_event(run_id, "running"));
        assert!(n1.is_empty());

        let n2 = router.handle_event(&run_status_event(run_id, "completed"));
        assert_eq!(n2.len(), 1);
        assert_eq!(n2[0].channel_kind, "browser-toast");
        assert_eq!(n2[0].notification.kind, "run.completed");
        assert_eq!(n2[0].notification.severity, "info");
    }

    #[tokio::test]
    async fn failed_run_is_error_severity() {
        let bus = ToastBus::new();
        let router = NotificationRouter::with_defaults(bus);
        let run_id = Uuid::new_v4();
        let n = router.handle_event(&run_status_event(run_id, "failed"));
        assert_eq!(n.len(), 1);
        assert_eq!(n[0].notification.severity, "error");
    }

    #[tokio::test]
    async fn awaiting_input_emits_warning() {
        let bus = ToastBus::new();
        let router = NotificationRouter::with_defaults(bus);
        let run_id = Uuid::new_v4();
        let n = router.handle_event(&run_status_event(run_id, "awaiting-input"));
        assert_eq!(n.len(), 1);
        assert_eq!(n[0].notification.kind, "run.awaiting-input");
        assert_eq!(n[0].notification.severity, "warning");
    }

    #[tokio::test]
    async fn repeat_emits_dedupe() {
        let bus = ToastBus::new();
        let router = NotificationRouter::with_defaults(bus);
        let run_id = Uuid::new_v4();
        let n1 = router.handle_event(&run_status_event(run_id, "completed"));
        let n2 = router.handle_event(&run_status_event(run_id, "completed"));
        // Second event is a same-status repeat — status_cache filters it
        // even before dedup.
        assert_eq!(n1.len(), 1);
        assert!(n2.is_empty());
    }

    #[tokio::test]
    async fn distinct_runs_emit_independently() {
        let bus = ToastBus::new();
        let router = NotificationRouter::with_defaults(bus);
        let r1 = Uuid::new_v4();
        let r2 = Uuid::new_v4();
        let n1 = router.handle_event(&run_status_event(r1, "completed"));
        let n2 = router.handle_event(&run_status_event(r2, "completed"));
        assert_eq!(n1.len(), 1);
        assert_eq!(n2.len(), 1);
        assert_ne!(n1[0].notification.id, n2[0].notification.id);
    }

    #[tokio::test]
    async fn noisy_statuses_dont_notify() {
        let bus = ToastBus::new();
        let router = NotificationRouter::with_defaults(bus);
        let run_id = Uuid::new_v4();
        for s in ["queued", "starting", "running"] {
            let n = router.handle_event(&run_status_event(run_id, s));
            assert!(n.is_empty(), "{s} should not emit");
        }
    }

    #[tokio::test]
    async fn toast_bus_receives_delivery() {
        let bus = ToastBus::new();
        let mut sub = bus.subscribe();
        let router = NotificationRouter::with_defaults(bus.clone());
        let run_id = Uuid::new_v4();
        router.handle_event(&run_status_event(run_id, "completed"));
        let delivered = tokio::time::timeout(Duration::from_millis(100), sub.recv())
            .await
            .expect("recv timeout")
            .expect("recv ok");
        assert_eq!(delivered.notification.kind, "run.completed");
    }

    #[tokio::test]
    async fn end_to_end_via_live_update_bus() {
        let toast_bus = ToastBus::new();
        let mut sub = toast_bus.subscribe();
        let live = LiveUpdateBus::new();
        let router = Arc::new(NotificationRouter::with_defaults(toast_bus));
        let _handle = router.clone().spawn(&live);

        let run_id = Uuid::new_v4();
        live.publish(AgentEvent::RunStateChanged {
            run_id,
            new_status: "completed".to_string(),
        });

        // Workspace batch window is 50ms; allow a generous timeout.
        let delivered = tokio::time::timeout(Duration::from_millis(500), sub.recv())
            .await
            .expect("router never delivered")
            .expect("recv ok");
        assert_eq!(delivered.notification.kind, "run.completed");
        assert_eq!(delivered.notification.entity_id, Some(run_id));
    }
}
