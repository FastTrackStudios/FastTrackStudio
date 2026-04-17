//! Share links — limited-access URLs for client review.
//!
//! Generate a unique token that gives access to specific versions
//! of a song/deliverable. Clients can listen, comment, approve
//! without needing a full account.
//!
//! ## Nextcloud integration
//! Share links can optionally be backed by Nextcloud OCS shares
//! with password protection and expiry.

use chrono::{NaiveDate, NaiveDateTime};
use facet::Facet;

/// A share link for external access to project content.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ShareLink {
    /// Unique token (URL-safe, e.g. "a1b2c3d4e5").
    pub token: String,
    /// Human label (e.g. "Sunrise Mix v3 — Client Review").
    pub label: String,
    /// Who created this share.
    pub created_by: String,
    pub created_at: Option<NaiveDateTime>,
    /// When this link expires (None = never).
    pub expires_at: Option<NaiveDate>,
    /// Optional password protection.
    pub password_hash: Option<String>,
    /// What this share gives access to.
    pub scope: ShareScope,
    /// Whether the recipient can leave comments.
    pub allow_comments: bool,
    /// Whether the recipient can approve/reject.
    pub allow_approval: bool,
    /// Whether the recipient can download files.
    pub allow_download: bool,
    /// Access log.
    #[facet(default)]
    pub access_log: Vec<AccessEvent>,
    /// Whether this link is active.
    pub active: bool,
    /// Nextcloud share ID (if backed by OCS share).
    pub nextcloud_share_id: Option<String>,
}

/// What content a share link gives access to.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ShareScope {
    /// Project title.
    pub project: String,
    /// Specific song (None = whole project).
    pub song: Option<String>,
    /// Specific stage (None = all stages in scope).
    pub stage: Option<String>,
    /// Specific version numbers (empty = latest).
    #[facet(default)]
    pub versions: Vec<u32>,
}

/// A logged access event on a share link.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct AccessEvent {
    pub event_type: AccessEventType,
    pub at: Option<NaiveDateTime>,
    /// IP or user identifier.
    pub accessor: Option<String>,
    /// User agent string.
    pub user_agent: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum AccessEventType {
    #[default]
    Viewed,
    Played,
    Downloaded,
    Commented,
    Approved,
    Rejected,
}

impl ShareLink {
    /// Generate a random URL-safe token.
    pub fn generate_token() -> String {
        // Simple hash-based token — in production use a CSPRNG
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        format!("{:016x}", now)
    }

    pub fn is_expired(&self) -> bool {
        if let Some(exp) = self.expires_at {
            exp < chrono::Local::now().date_naive()
        } else {
            false
        }
    }

    pub fn is_valid(&self) -> bool {
        self.active && !self.is_expired()
    }

    pub fn view_count(&self) -> usize {
        self.access_log.iter().filter(|e| matches!(e.event_type, AccessEventType::Viewed)).count()
    }

    pub fn download_count(&self) -> usize {
        self.access_log.iter().filter(|e| matches!(e.event_type, AccessEventType::Downloaded)).count()
    }
}
