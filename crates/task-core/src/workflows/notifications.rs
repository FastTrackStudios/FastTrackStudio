//! Notification system — mentions, approvals, new versions, due dates.
//!
//! Notifications are generated from events (comment mention, approval request,
//! new version uploaded, etc.) and delivered to recipients.
//!
//! ## Nextcloud integration
//! Can sync with Nextcloud's notification API for push delivery.

use chrono::NaiveDateTime;
use facet::Facet;

/// A notification for a user.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Notification {
    /// Unique ID.
    pub id: String,
    /// Who this notification is for.
    pub recipient: String,
    /// What kind of notification.
    pub kind: NotificationKind,
    /// Human-readable summary.
    pub message: String,
    /// Who triggered this notification.
    pub actor: Option<String>,
    /// When this happened.
    pub created_at: Option<NaiveDateTime>,
    /// Whether the recipient has read this.
    pub read: bool,
    /// Reference to the entity (project/song/version).
    pub entity_ref: Option<EntityRef>,
}

/// What triggered the notification.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum NotificationKind {
    #[default]
    Mention,           // @mentioned in a comment
    CommentReply,      // someone replied to your comment
    NewVersion,        // new version uploaded to a stage you're watching
    ApprovalRequest,   // someone wants your approval
    ApprovalGranted,   // your submission was approved
    ChangesRequested,  // reviewer requested changes
    TaskAssigned,      // a task was assigned to you
    DueReminder,       // a task/deliverable is due soon
    ShareAccessed,     // someone accessed your share link
}

/// A reference to an entity for navigation.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct EntityRef {
    /// Project title.
    pub project: String,
    /// Song title (optional).
    pub song: Option<String>,
    /// Stage name (optional).
    pub stage: Option<String>,
    /// Version number (optional).
    pub version: Option<u32>,
    /// Task title (optional).
    pub task: Option<String>,
    /// Comment ID (optional).
    pub comment_id: Option<String>,
}

/// Create a notification from a comment mention.
pub fn mention_notification(
    recipient: &str,
    actor: &str,
    comment_body: &str,
    project: &str,
    song: Option<&str>,
) -> Notification {
    Notification {
        id: format!("n-{}", ShareLink::generate_token_short()),
        recipient: recipient.to_string(),
        kind: NotificationKind::Mention,
        message: format!("@{actor} mentioned you: {}", truncate(comment_body, 80)),
        actor: Some(actor.to_string()),
        created_at: Some(chrono::Local::now().naive_local()),
        entity_ref: Some(EntityRef {
            project: project.to_string(),
            song: song.map(|s| s.to_string()),
            ..Default::default()
        }),
        ..Default::default()
    }
}

/// Create a notification for a new version upload.
pub fn new_version_notification(
    recipient: &str,
    actor: &str,
    project: &str,
    song: &str,
    stage: &str,
    version: u32,
) -> Notification {
    Notification {
        id: format!("n-{}", ShareLink::generate_token_short()),
        recipient: recipient.to_string(),
        kind: NotificationKind::NewVersion,
        message: format!("{song} — {stage} v{version} uploaded by @{actor}"),
        actor: Some(actor.to_string()),
        created_at: Some(chrono::Local::now().naive_local()),
        entity_ref: Some(EntityRef {
            project: project.to_string(),
            song: Some(song.to_string()),
            stage: Some(stage.to_string()),
            version: Some(version),
            ..Default::default()
        }),
        ..Default::default()
    }
}

/// Create a notification for an approval request.
pub fn approval_request_notification(
    recipient: &str,
    actor: &str,
    project: &str,
    song: &str,
    stage: &str,
) -> Notification {
    Notification {
        id: format!("n-{}", ShareLink::generate_token_short()),
        recipient: recipient.to_string(),
        kind: NotificationKind::ApprovalRequest,
        message: format!("@{actor} requested your approval on {song} — {stage}"),
        actor: Some(actor.to_string()),
        created_at: Some(chrono::Local::now().naive_local()),
        entity_ref: Some(EntityRef {
            project: project.to_string(),
            song: Some(song.to_string()),
            stage: Some(stage.to_string()),
            ..Default::default()
        }),
        ..Default::default()
    }
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max { s.to_string() }
    else { format!("{}…", &s[..max]) }
}

// Helper to avoid circular dep — duplicated token gen
struct ShareLink;
impl ShareLink {
    fn generate_token_short() -> String {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        format!("{:08x}", (now & 0xFFFFFFFF) as u32)
    }
}
