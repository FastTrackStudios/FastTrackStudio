//! User domain type for authenticated users

use chrono::{DateTime, Utc};
use uuid::Uuid;

/// A user account for cloud sync
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct User {
    /// Unique user identifier
    pub id: Uuid,
    /// User's email address
    pub email: String,
    /// Display name (from OAuth provider profile)
    pub display_name: String,
    /// When the account was created
    pub created_at: DateTime<Utc>,
    /// When the user last synced presets
    pub last_sync: Option<DateTime<Utc>>,
}

impl User {
    pub fn new(id: Uuid, email: String, display_name: String) -> Self {
        Self {
            id,
            email,
            display_name,
            created_at: Utc::now(),
            last_sync: None,
        }
    }
}
