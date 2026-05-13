//! `caldav-proto` — wire contract for the CalDAV sync feature.
//!
//! Two entities:
//!
//! - `CalDavAccount` — connection metadata for one remote (Nextcloud,
//!   iCloud, owncloud, …). Credentials are NOT here — they live in
//!   `auth-db` keyed by `id`. The wire type carries the display name
//!   and the base URL so a UI can list accounts without unlocking
//!   secrets.
//! - `CalDavCalendar` — one calendar on an account. Maps a remote
//!   CalDAV URL/ETag pair to our local `calendar` feature so the
//!   server-side sync engine can reconcile both directions.
//!
//! Sync itself is the concern of `caldav-sync`, which wraps the
//! `kitchen_fridge` crate.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "caldav_accounts", repo)]
pub struct CalDavAccount {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub display_name: String,

    /// Base CalDAV principal URL, e.g.
    /// `"https://cloud.example.com/remote.php/dav/principals/users/alice/"`.
    #[architect(filterable, fulltext)]
    pub base_url: String,

    /// CalDAV username. The matching password / app-token is stored
    /// out-of-band in `auth-db` keyed by this account's `id`.
    #[architect(filterable)]
    pub username: String,

    /// Free-form `"nextcloud"` / `"icloud"` / `"owncloud"` / `"other"`.
    #[architect(filterable)]
    pub kind: Option<String>,

    /// Most recent successful full-sync wall-clock.
    #[architect(filterable, sortable)]
    pub last_sync_at: Option<DateTime<Utc>>,

    /// Last error string surfaced by the sync engine, if any.
    pub last_error: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "caldav_calendars", repo)]
pub struct CalDavCalendar {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub account_id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    /// The collection URL as returned by CalDAV — e.g.
    /// `"https://cloud.example.com/remote.php/dav/calendars/alice/work/"`.
    #[architect(filterable)]
    pub url: String,

    pub color: Option<String>,

    /// `true` if the user wants this calendar synced.
    #[architect(filterable)]
    pub enabled: bool,

    /// Server-side CTag — bumps when the calendar's contents change.
    pub server_ctag: Option<String>,

    #[architect(filterable, sortable)]
    pub last_sync_at: Option<DateTime<Utc>>,

    pub event_count: u32,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum CalDavServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("auth failed: {0}")]
    AuthFailed(String),
    #[error("network: {0}")]
    Network(String),
    #[error("server error: {0}")]
    Server(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet)]
pub struct SyncSummary {
    pub events_pulled: u32,
    pub events_pushed: u32,
    pub events_deleted: u32,
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait CalDavService {
    /// Confirm the account's credentials resolve to a principal.
    async fn test_connection(&self, account_id: Uuid) -> Result<(), CalDavServiceError>;

    /// Bi-directional sync for one calendar — pull remote changes into
    /// the local `calendar` feature's CRDT, push local edits back.
    async fn sync_calendar(&self, calendar_id: Uuid) -> Result<SyncSummary, CalDavServiceError>;

    /// Discover the remote's calendar list and reconcile with local
    /// `CalDavCalendar` rows.
    async fn refresh_calendar_list(&self, account_id: Uuid) -> Result<u32, CalDavServiceError>;
}
