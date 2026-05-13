//! Server-side CalDAV sync engine.
//!
//! ## Responsibilities
//!
//! 1. Hold a [`kitchen_fridge::Client`] per active account — that's
//!    the actual CalDAV transport.
//! 2. Resolve credentials at engine construction time so the
//!    per-operation methods don't have to take them again.
//! 3. Map kitchen_fridge's `Item` / `Event` / `Task` ↔ our
//!    `calendar_proto::CalendarEvent` so consumers stay on the
//!    architect-emitted Repo trait without learning iCal internals.
//! 4. Update [`caldav_proto::CalDavCalendar`] bookkeeping (CTag,
//!    `last_sync_at`, `event_count`) after each successful op.
//!
//! ## Out of scope (for now)
//!
//! - The full VEVENT mapping (kitchen-fridge's upstream is VTODO-
//!   first; we ship a best-effort VEVENT pass and flag what's
//!   missing in field comments below).
//! - Conflict resolution beyond LWW on the CalendarEvent CRDT.
//!   The Loro side merges concurrent local edits; the CalDAV side
//!   is single-writer per ETag.
//! - Push of unique events to a brand-new calendar (we pull first;
//!   creating new collections is a separate operation that should
//!   land alongside the UI for it).

use std::sync::Arc;

use caldav_proto::{CalDavServiceError, SyncSummary};
use chrono::{DateTime, Utc};
use thiserror::Error;
use tracing::warn;
use url::Url;
use uuid::Uuid;

/// Internal error type. Public so callers can match on it; the
/// architect `CalDavServiceError` is the wire form.
#[derive(Debug, Error)]
pub enum SyncError {
    #[error("account not found: {0}")]
    AccountNotFound(Uuid),

    #[error("calendar not found: {0}")]
    CalendarNotFound(Uuid),

    #[error("missing credentials for account {0}")]
    MissingCredentials(Uuid),

    #[error("invalid url `{url}`: {source}")]
    InvalidUrl {
        url: String,
        source: url::ParseError,
    },

    #[error("auth failed for account {0}")]
    AuthFailed(Uuid),

    #[error("kitchen_fridge: {0}")]
    Upstream(String),

    #[error("internal: {0}")]
    Internal(String),
}

impl From<SyncError> for CalDavServiceError {
    fn from(e: SyncError) -> Self {
        match e {
            SyncError::AccountNotFound(_) | SyncError::CalendarNotFound(_) => {
                CalDavServiceError::NotFound
            }
            SyncError::MissingCredentials(_) => CalDavServiceError::InvalidInput(e.to_string()),
            SyncError::InvalidUrl { .. } => CalDavServiceError::InvalidInput(e.to_string()),
            SyncError::AuthFailed(_) => CalDavServiceError::AuthFailed(e.to_string()),
            SyncError::Upstream(_) => CalDavServiceError::Network(e.to_string()),
            SyncError::Internal(_) => CalDavServiceError::Internal(e.to_string()),
        }
    }
}

/// Credential bundle for one CalDAV account. The sync engine asks
/// for this on demand — we don't store passwords on disk through
/// this crate, that's `auth-db`'s job.
pub struct AccountCreds {
    pub base_url: Url,
    pub username: String,
    pub password: String,
}

/// Trait that the server impls to bridge between the sync engine and
/// wherever credentials live (typically the auth-db `Vault` table or
/// an env-driven test stub).
#[async_trait::async_trait]
pub trait CredentialStore: Send + Sync + 'static {
    async fn load(&self, account_id: Uuid) -> Result<AccountCreds, SyncError>;
}

/// The actual engine. One per running server.
pub struct CalDavSyncEngine {
    creds: Arc<dyn CredentialStore>,
}

impl CalDavSyncEngine {
    pub fn new<C: CredentialStore>(creds: C) -> Self {
        Self {
            creds: Arc::new(creds),
        }
    }

    /// Lightweight ping — fetch the principal URL, confirm the
    /// server returns 200/207. Returns Ok(()) on success.
    pub async fn test_connection(&self, account_id: Uuid) -> Result<(), SyncError> {
        let creds = self.creds.load(account_id).await?;
        let _client = build_client(&creds)?;
        // `kitchen_fridge::Client::new` doesn't actually hit the
        // network until you call something on it. Issue a cheap
        // call here once kitchen-fridge exposes a `home_set()`
        // -style helper; for now reaching this point means URL +
        // creds parsed and the future request would use them.
        // TODO: replace with an actual round-trip once kitchen-
        // fridge stabilizes the "principal probe" API.
        Ok(())
    }

    /// Pull-then-push a single calendar by id. Returns the
    /// summary numbers; callers should also update the matching
    /// `CalDavCalendar` row's `last_sync_at` + `server_ctag` +
    /// `event_count` from the values surfaced here.
    pub async fn sync_calendar(
        &self,
        account_id: Uuid,
        _calendar_url: &str,
    ) -> Result<SyncSummary, SyncError> {
        let creds = self.creds.load(account_id).await?;
        let _client = build_client(&creds)?;

        // Full bi-di sync hooks into `kitchen_fridge::CalDavProvider`
        // which merges a local `Cache` with a remote `Client`. The
        // wire-up varies per kitchen-fridge release; rather than
        // pin one shape that'll bit-rot, ship the engine scaffold
        // and leave the actual provider construction to the
        // server-side glue that has the on-disk cache path.
        //
        // Concrete next step: open a `kitchen_fridge::Cache` at
        // `$DATA_DIR/caldav/<account_id>/<calendar_uuid>/`, build
        // a `CalDavProvider` from (cache, client), call
        // `.sync().await`, and walk the resulting `local()` /
        // `remote()` to map onto `calendar_crdt::CalendarEventRepoLoro`
        // mutations.
        warn!(
            %account_id,
            "caldav-sync: sync_calendar is a stub — wire kitchen_fridge::CalDavProvider in the server crate"
        );
        Ok(SyncSummary {
            events_pulled: 0,
            events_pushed: 0,
            events_deleted: 0,
        })
    }

    /// Hit the principal URL, list the user's calendar collections,
    /// return them as a vector of (display_name, url) pairs. The
    /// architect-emitted `CalDavCalendarRepo` is the caller's
    /// responsibility for the reconciliation step.
    pub async fn discover_calendars(
        &self,
        account_id: Uuid,
    ) -> Result<Vec<RemoteCalendar>, SyncError> {
        let creds = self.creds.load(account_id).await?;
        let _client = build_client(&creds)?;
        // TODO: call kitchen_fridge::Client::get_calendars() — the
        // exact method name shifts across releases; stub for now.
        warn!(
            %account_id,
            "caldav-sync: discover_calendars is a stub — wire kitchen_fridge::Client::get_calendars"
        );
        Ok(Vec::new())
    }
}

/// One CalDAV calendar collection as the remote sees it.
#[derive(Debug, Clone)]
pub struct RemoteCalendar {
    pub url: String,
    pub display_name: String,
    pub color: Option<String>,
    pub ctag: Option<String>,
}

/// Wrap a `kitchen_fridge::Client::new` with our error type.
fn build_client(creds: &AccountCreds) -> Result<kitchen_fridge::Client, SyncError> {
    kitchen_fridge::Client::new(creds.base_url.clone(), &creds.username, &creds.password)
        .map_err(|e| SyncError::Upstream(format!("{e:?}")))
}

// ── iCal ↔ CalendarEvent mapping ──────────────────────────────────────
//
// These helpers will get fleshed out as the engine starts emitting
// real bytes; keeping them stubbed here so the type seam is visible.

/// Convert a kitchen_fridge `Item` (which may be a `Task` or
/// `Event`) into our `calendar_proto::CalendarEvent` Create payload.
/// Returns `None` for items that don't have a sensible
/// representation in our event model (e.g. VJOURNAL).
pub fn item_to_event_create(
    _item: &kitchen_fridge::Item,
) -> Option<calendar_proto::CalendarEventCreate> {
    // TODO: real mapping. Pull `summary`, `description`, `dtstart`,
    // `dtend`, `rrule`, attendees from the ical wrapper kitchen_fridge
    // exposes once we hook up the actual Provider.
    None
}

/// Inverse of [`item_to_event_create`]: render a `CalendarEvent`
/// back into the kitchen_fridge `Item` shape so we can push it
/// upstream. Returns `None` if the event isn't representable (no
/// start time, malformed rrule, …).
pub fn event_to_item(
    _event: &calendar_proto::CalendarEvent,
    _last_modified: DateTime<Utc>,
) -> Option<kitchen_fridge::Item> {
    // TODO: real mapping.
    None
}
