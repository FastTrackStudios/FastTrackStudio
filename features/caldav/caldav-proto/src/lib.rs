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
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AccountDisplayName"))]
    pub display_name: String,

    /// Base CalDAV principal URL, e.g.
    /// `"https://cloud.example.com/remote.php/dav/principals/users/alice/"`.
    #[architect(filterable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CalDavBaseUrl"))]
    pub base_url: String,

    /// CalDAV username. The matching password / app-token is stored
    /// out-of-band in `auth-db` keyed by this account's `id`.
    #[architect(filterable)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::internet::en::Username()")
    )]
    pub username: String,

    /// Free-form `"nextcloud"` / `"icloud"` / `"owncloud"` / `"other"`.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CalDavKind"))]
    pub kind: Option<String>,

    /// Most recent successful full-sync wall-clock.
    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub last_sync_at: Option<DateTime<Utc>>,

    /// Last error string surfaced by the sync engine, if any.
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..10)")
    )]
    pub last_error: Option<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CalDavTags"))]
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
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CalendarName"))]
    pub name: String,

    /// The collection URL as returned by CalDAV — e.g.
    /// `"https://cloud.example.com/remote.php/dav/calendars/alice/work/"`.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CalDavCalendarUrl"))]
    pub url: String,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CalendarColor"))]
    pub color: Option<String>,

    /// `true` if the user wants this calendar synced.
    #[architect(filterable)]
    pub enabled: bool,

    /// Server-side CTag — bumps when the calendar's contents change.
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CTag"))]
    pub server_ctag: Option<String>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub last_sync_at: Option<DateTime<Utc>>,

    #[cfg_attr(feature = "fake", dummy(faker = "0u32..500"))]
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

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct AccountDisplayName;
    impl Dummy<AccountDisplayName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AccountDisplayName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Personal iCloud",
                    "Work Nextcloud",
                    "Fastmail Calendar",
                    "Family Account",
                    "Studio Schedule",
                    "Client Calendar",
                ],
            )
        }
    }

    pub struct CalDavBaseUrl;
    impl Dummy<CalDavBaseUrl> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CalDavBaseUrl, rng: &mut R) -> Self {
            const HOSTS: &[&str] = &[
                "caldav.fastmail.com/dav/principals/user@example.com",
                "caldav.icloud.com/principals/users/alice",
                "cloud.example.com/remote.php/dav/principals/users/alice",
                "owncloud.example.org/remote.php/dav/principals/users/bob",
            ];
            format!("https://{}/", HOSTS.choose(rng).unwrap())
        }
    }

    pub struct CalDavKind;
    impl Dummy<CalDavKind> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CalDavKind, rng: &mut R) -> Self {
            pick(
                rng,
                &["nextcloud", "icloud", "owncloud", "fastmail", "other"],
            )
        }
    }

    pub struct CalDavTags;
    impl Dummy<CalDavTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CalDavTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "personal",
                "work",
                "family",
                "shared",
                "read-only",
                "primary",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }

    pub struct CalendarName;
    impl Dummy<CalendarName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CalendarName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Work",
                    "Personal",
                    "Family",
                    "Birthdays",
                    "Holidays",
                    "Travel",
                    "Studio",
                    "Sessions",
                    "Deadlines",
                    "Shared with Team",
                ],
            )
        }
    }

    pub struct CalDavCalendarUrl;
    impl Dummy<CalDavCalendarUrl> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CalDavCalendarUrl, rng: &mut R) -> Self {
            const HOSTS: &[&str] = &[
                "caldav.fastmail.com/dav/calendars/user/work",
                "caldav.icloud.com/calendars/alice/personal",
                "cloud.example.com/remote.php/dav/calendars/alice/family",
            ];
            format!("https://{}/", HOSTS.choose(rng).unwrap())
        }
    }

    pub struct CalendarColor;
    impl Dummy<CalendarColor> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CalendarColor, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "#ef4444", "#f97316", "#eab308", "#22c55e", "#06b6d4", "#3b82f6", "#8b5cf6",
                    "#ec4899",
                ],
            )
        }
    }

    pub struct CTag;
    impl Dummy<CTag> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CTag, rng: &mut R) -> Self {
            let n: u64 = rng.random_range(1_000_000..9_999_999_999);
            format!("\"ctag-{:x}\"", n)
        }
    }

    pub struct RecentDateTime;
    impl Dummy<RecentDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecentDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::minutes(rng.random_range(0..43_200))
        }
    }
}
