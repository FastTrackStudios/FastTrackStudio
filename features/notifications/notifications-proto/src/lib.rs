//! Wire contract for the `notifications` feature.
//!
//! Three entities that compose into a delivery pipeline:
//!
//! - [`Notification`] — one row per "something happened that the user
//!   should know about". Created by feature-specific routers (start
//!   with agent run status transitions; calendar / email / etc. plug
//!   in later) and consumed by the inbox UI.
//! - [`NotificationChannel`] — one row per delivery surface
//!   (browser toast, browser push, desktop libnotify, hermes relay).
//!   User configures which are enabled.
//! - [`NotificationRule`] — when a `Notification.kind` matches and
//!   severity ≥ rule threshold, dispatch through `to_channel_id`.
//!   Multiple rules per channel; first-match wins per channel.
//!
//! The model is open: routers don't enumerate every notification
//! shape upfront. `kind` is a free-text string (`run.completed`,
//! `email.received`, …); the UI maps known kinds to icons and
//! falls back to a generic bell for unknowns.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

/// Conventional severity labels. Stored as a string on Notification
/// for forward-compat with future levels.
pub const NOTIFICATION_SEVERITIES: &[&str] = &["info", "warning", "error"];

/// Conventional channel kinds. Free-text on the row so plugins can
/// register new ones without a schema change.
pub const NOTIFICATION_CHANNEL_KINDS: &[&str] = &[
    "browser-toast",
    "browser-push",
    "desktop-libnotify",
    "hermes-relay",
];

// ── Notification ─────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "notifications", repo)]
pub struct Notification {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Event kind in dotted-path form: `run.completed`, `run.failed`,
    /// `run.blocked`, `run.awaiting-input`, `task.due-soon`, etc.
    /// Routers pick the format; the inbox renders unknowns generically.
    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::NotifKind"))]
    pub kind: String,

    #[architect(filterable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::NotifTitle"))]
    pub title: String,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..15)")
    )]
    pub body: String,

    /// `info`, `warning`, `error`. Free-text but the UI's color
    /// mapping only knows the three canonical values.
    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::NotifSeverity"))]
    pub severity: String,

    /// What kind of object triggered this notification: `agent_run`,
    /// `task`, `email`, `calendar_event`. Pair with `entity_id` for
    /// the link target.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::NotifEntityKind"))]
    pub entity_kind: String,

    #[architect(filterable)]
    pub entity_id: Option<Uuid>,

    /// Deep-link URL the UI navigates to on click. Often
    /// `/agent/dashboard/{run_id}`. None when the notification is
    /// purely informational with no detail surface.
    pub action_url: Option<String>,

    /// Dedup key: routers pass a tuple-hash like
    /// `format!("run.{}.completed", run_id)`. If a Notification with
    /// the same key was created within the last 5s the router skips
    /// the new emit. Stored so the inbox can group repeats.
    #[architect(filterable)]
    pub dedup_key: Option<String>,

    #[architect(filterable, sortable)]
    pub read_at: Option<DateTime<Utc>>,

    #[architect(filterable, sortable)]
    pub dismissed_at: Option<DateTime<Utc>>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── NotificationChannel ──────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "notification_channels", repo)]
pub struct NotificationChannel {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// One of `NOTIFICATION_CHANNEL_KINDS` (free-text for plugin
    /// extensibility).
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ChannelKind"))]
    pub kind: String,

    /// Human label shown in settings ("My Mac", "Phone push", "Slack
    /// #ops").
    #[architect(filterable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ChannelLabel"))]
    pub label: String,

    #[architect(filterable)]
    pub enabled: bool,

    /// JSON-encoded per-channel knobs — push subscription endpoint
    /// for `browser-push`, Hermes platform/chat_id for `hermes-relay`,
    /// desktop daemon socket for `desktop-libnotify`, etc.
    pub config_json: String,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── NotificationRule ─────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "notification_rules", repo)]
pub struct NotificationRule {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Glob pattern matched against `Notification.kind`. `*` matches
    /// any one path segment, `**` matches anything. The MVP matcher
    /// supports literal prefix + `*` suffix (`run.*`, `*`); full glob
    /// is a follow-up.
    #[architect(filterable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RuleKindPattern"))]
    pub when_kind: String,

    /// Minimum severity to match: `info` matches all, `warning`
    /// matches warning+error, `error` matches only error.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::NotifSeverity"))]
    pub min_severity: String,

    #[architect(filterable)]
    pub to_channel_id: Uuid,

    #[architect(filterable)]
    pub enabled: bool,

    /// Tiebreaker for rule ordering when multiple rules match the
    /// same notification: lower priority wins (runs first).
    #[architect(sortable)]
    pub priority: i32,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Service ──────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum NotificationServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait NotificationService {
    /// Mark a notification as read (touches `read_at`).
    async fn mark_read(&self, id: Uuid) -> Result<(), NotificationServiceError>;

    /// Dismiss a notification (touches `dismissed_at`). Dismissed
    /// notifications are hidden from the default inbox view but
    /// remain queryable.
    async fn dismiss(&self, id: Uuid) -> Result<(), NotificationServiceError>;

    /// Mark every unread notification as read. Returns count.
    async fn mark_all_read(&self) -> Result<u32, NotificationServiceError>;
}

// ── Rule matching helpers ────────────────────────────────────────────

/// Match a `Notification.kind` string against a rule's `when_kind`
/// pattern. MVP supports literal match plus trailing `*` wildcard:
/// `"run.*"` matches `"run.completed"`, `"*"` matches anything.
pub fn kind_matches(pattern: &str, kind: &str) -> bool {
    if pattern == "*" {
        return true;
    }
    if let Some(prefix) = pattern.strip_suffix(".*") {
        // `run.*` matches `run.completed` and `run.foo.bar`. We accept
        // both shapes — pattern is "everything under this prefix".
        return kind == prefix || kind.starts_with(&format!("{prefix}."));
    }
    if let Some(prefix) = pattern.strip_suffix('*') {
        return kind.starts_with(prefix);
    }
    pattern == kind
}

/// Rank for severity comparison.
pub fn severity_rank(s: &str) -> u8 {
    match s {
        "info" => 0,
        "warning" => 1,
        "error" => 2,
        _ => 0,
    }
}

/// Return true when `notif_severity` meets or exceeds the rule's
/// `min_severity` threshold.
pub fn severity_meets(notif_severity: &str, min_severity: &str) -> bool {
    severity_rank(notif_severity) >= severity_rank(min_severity)
}

// ── Fakers ───────────────────────────────────────────────────────────

#[cfg(feature = "fake")]
pub mod fakers {
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct NotifKind;
    impl Dummy<NotifKind> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &NotifKind, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "run.completed",
                    "run.failed",
                    "run.blocked",
                    "run.awaiting-input",
                    "run.timed-out",
                    "task.due-soon",
                ],
            )
        }
    }

    pub struct NotifTitle;
    impl Dummy<NotifTitle> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &NotifTitle, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Run completed",
                    "Run failed",
                    "Run is blocked",
                    "Awaiting your approval",
                    "Task is due soon",
                ],
            )
        }
    }

    pub struct NotifSeverity;
    impl Dummy<NotifSeverity> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &NotifSeverity, rng: &mut R) -> Self {
            pick(rng, &["info", "warning", "error"])
        }
    }

    pub struct NotifEntityKind;
    impl Dummy<NotifEntityKind> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &NotifEntityKind, rng: &mut R) -> Self {
            pick(rng, &["agent_run", "task", "email", "calendar_event"])
        }
    }

    pub struct ChannelKind;
    impl Dummy<ChannelKind> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ChannelKind, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "browser-toast",
                    "browser-push",
                    "desktop-libnotify",
                    "hermes-relay",
                ],
            )
        }
    }

    pub struct ChannelLabel;
    impl Dummy<ChannelLabel> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ChannelLabel, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Browser toast",
                    "My Mac",
                    "Phone push",
                    "Hermes Slack",
                    "Telegram personal",
                ],
            )
        }
    }

    pub struct RuleKindPattern;
    impl Dummy<RuleKindPattern> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RuleKindPattern, rng: &mut R) -> Self {
            pick(rng, &["*", "run.*", "run.completed", "task.*"])
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn kind_matches_star() {
        assert!(kind_matches("*", "run.completed"));
        assert!(kind_matches("*", ""));
    }

    #[test]
    fn kind_matches_dot_star_prefix() {
        assert!(kind_matches("run.*", "run.completed"));
        assert!(kind_matches("run.*", "run.failed"));
        assert!(kind_matches("run.*", "run.tool.approved"));
        assert!(kind_matches("run.*", "run"));
        assert!(!kind_matches("run.*", "task.due"));
    }

    #[test]
    fn kind_matches_literal() {
        assert!(kind_matches("run.completed", "run.completed"));
        assert!(!kind_matches("run.completed", "run.failed"));
    }

    #[test]
    fn severity_ordering() {
        assert!(severity_meets("error", "warning"));
        assert!(severity_meets("warning", "warning"));
        assert!(!severity_meets("info", "warning"));
        assert!(severity_meets("info", "info"));
    }
}
