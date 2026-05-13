//! Server-side CalDAV sync engine, backed by `fast-dav-rs`.
//!
//! ## Why fast-dav-rs (over kitchen-fridge)
//!
//! - Implements VEVENT properly (kitchen-fridge's upstream is
//!   VTODO-first with "VEVENT … fairly trivial but not yet").
//! - Sync-token incremental fetches (RFC 6578) — server returns
//!   only the changed objects, perfect for the `CalDavCalendar::server_ctag`
//!   field's intent.
//! - Low-level (no built-in Cache/Provider). Our `calendar` feature
//!   IS the cache — kitchen-fridge's local cache would have been
//!   redundant + a second source of truth to keep in sync.
//!
//! ## What's implemented
//!
//! - [`CalDavSyncEngine::test_connection`] — confirms the credentials
//!   resolve to a principal.
//! - [`CalDavSyncEngine::discover_calendars`] — full
//!   principal → home-set → list-calendars discovery, returns the
//!   collection URLs + display metadata.
//! - [`CalDavSyncEngine::sync_calendar`] — pulls changes via
//!   `sync_collection` using `last_sync_token` (server-side ctag /
//!   sync-token in our [`caldav_proto::CalDavCalendar`]) for
//!   incremental updates.
//!
//! ## iCal mapping
//!
//! The [`ical`] module bridges `BEGIN:VEVENT…` text and
//! `calendar_proto::CalendarEvent`. Pull-side: parsed VEVENTs land
//! as `CalendarEventCreate` payloads with deterministic local
//! `Uuid`s (uuid v5 over the iCal UID), so the same remote event
//! always reconciles back to the same local row.

use std::sync::Arc;

use caldav_proto::{CalDavServiceError, SyncSummary};
use fast_dav_rs::CalDavClient;
use thiserror::Error;
use tracing::info;
use url::Url;
use uuid::Uuid;

pub mod ical;
pub use ical::{ParsedEvent, event_to_ical, local_id_for_uid, parse_calendar_data};

/// Internal error type. Public so callers can match on variants; the
/// architect `CalDavServiceError` is the wire form on the service trait.
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

    #[error("caldav: {0}")]
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
            SyncError::MissingCredentials(_) | SyncError::InvalidUrl { .. } => {
                CalDavServiceError::InvalidInput(e.to_string())
            }
            SyncError::AuthFailed(_) => CalDavServiceError::AuthFailed(e.to_string()),
            SyncError::Upstream(_) => CalDavServiceError::Network(e.to_string()),
            SyncError::Internal(_) => CalDavServiceError::Internal(e.to_string()),
        }
    }
}

/// Credential bundle for one CalDAV account. The sync engine asks
/// for this on demand — we don't store passwords on disk through
/// this crate; that's `auth-db`'s job.
pub struct AccountCreds {
    pub base_url: Url,
    pub username: String,
    pub password: String,
}

/// Bridge to wherever credentials live. Typically the server impls
/// this against the auth-db `Vault` table; tests use an env-driven
/// stub.
#[async_trait::async_trait]
pub trait CredentialStore: Send + Sync + 'static {
    async fn load(&self, account_id: Uuid) -> Result<AccountCreds, SyncError>;
}

/// One CalDAV collection (calendar) as the remote server describes it.
/// Built from fast-dav-rs's [`CalendarInfo`] but stripped to just the
/// fields the architect `CalDavCalendar` cares about.
#[derive(Debug, Clone)]
pub struct RemoteCalendar {
    pub url: String,
    pub display_name: String,
    pub description: Option<String>,
    pub color: Option<String>,
    pub timezone: Option<String>,
}

/// One synced object — an event/todo/journal from the remote. We
/// hand the raw iCal data back so the caller (or a separate
/// `caldav-ical` crate later) does the VEVENT/VTODO parsing.
#[derive(Debug, Clone)]
pub struct SyncedObject {
    pub href: String,
    pub etag: Option<String>,
    pub calendar_data: Option<String>,
    pub is_deleted: bool,
}

/// Result of a single `sync_calendar` call. The returned `next_sync_token`
/// should be stored on `caldav_proto::CalDavCalendar::server_ctag` so
/// the next sync only fetches deltas.
#[derive(Debug, Clone)]
pub struct SyncedCalendar {
    pub objects: Vec<SyncedObject>,
    pub next_sync_token: Option<String>,
}

/// Sync engine — one per running server. Cheap to clone (the
/// `Arc<dyn CredentialStore>` is the only stateful piece).
#[derive(Clone)]
pub struct CalDavSyncEngine {
    creds: Arc<dyn CredentialStore>,
}

impl CalDavSyncEngine {
    pub fn new<C: CredentialStore>(creds: C) -> Self {
        Self {
            creds: Arc::new(creds),
        }
    }

    pub fn with_arc(creds: Arc<dyn CredentialStore>) -> Self {
        Self { creds }
    }

    /// Confirm the account's credentials resolve to a CalDAV
    /// principal. Returns `Ok(())` on success.
    pub async fn test_connection(&self, account_id: Uuid) -> Result<(), SyncError> {
        let client = self.build_client(account_id).await?;
        let principal = client
            .discover_current_user_principal()
            .await
            .map_err(map_err)?;
        if principal.is_none() {
            return Err(SyncError::AuthFailed(account_id));
        }
        Ok(())
    }

    /// Run principal → home-set → list-calendars discovery and
    /// return every remote calendar as a [`RemoteCalendar`]. The
    /// caller is responsible for reconciling these against the
    /// architect-emitted `CalDavCalendarRepo`.
    pub async fn discover_calendars(
        &self,
        account_id: Uuid,
    ) -> Result<Vec<RemoteCalendar>, SyncError> {
        let client = self.build_client(account_id).await?;
        let principal = client
            .discover_current_user_principal()
            .await
            .map_err(map_err)?
            .ok_or(SyncError::AuthFailed(account_id))?;
        let homes = client
            .discover_calendar_home_set(&principal)
            .await
            .map_err(map_err)?;
        let mut out = Vec::new();
        for home in homes {
            let calendars = client.list_calendars(&home).await.map_err(map_err)?;
            for c in calendars {
                out.push(RemoteCalendar {
                    url: c.href,
                    display_name: c.displayname.unwrap_or_default(),
                    description: c.description,
                    color: c.color,
                    timezone: c.timezone,
                });
            }
        }
        info!(%account_id, calendars = out.len(), "discovered remote calendars");
        Ok(out)
    }

    /// Pull changes from one remote calendar via the
    /// [`sync_collection`](fast_dav_rs::CalDavClient::sync_collection)
    /// REPORT. Pass `last_sync_token = None` for the initial sync
    /// (returns all current objects); subsequent runs pass the
    /// previously-returned `next_sync_token` for incremental deltas.
    pub async fn sync_calendar(
        &self,
        account_id: Uuid,
        calendar_url: &str,
        last_sync_token: Option<&str>,
    ) -> Result<SyncedCalendar, SyncError> {
        let client = self.build_client(account_id).await?;
        let resp = client
            .sync_collection(calendar_url, last_sync_token, None, true)
            .await
            .map_err(map_err)?;
        let objects = resp
            .items
            .into_iter()
            .map(|i| SyncedObject {
                href: i.href,
                etag: i.etag,
                calendar_data: i.calendar_data,
                is_deleted: i.is_deleted,
            })
            .collect();
        Ok(SyncedCalendar {
            objects,
            next_sync_token: resp.sync_token,
        })
    }

    /// Wire into the architect `CalDavService` trait — convenience
    /// for callers that want the summary shape. Tracks pulled-only;
    /// push lands when the ical writer is in.
    pub async fn sync_calendar_summary(
        &self,
        account_id: Uuid,
        calendar_url: &str,
        last_sync_token: Option<&str>,
    ) -> Result<SyncSummary, SyncError> {
        let synced = self
            .sync_calendar(account_id, calendar_url, last_sync_token)
            .await?;
        let deleted = synced.objects.iter().filter(|o| o.is_deleted).count() as u32;
        Ok(SyncSummary {
            events_pulled: synced.objects.len() as u32 - deleted,
            events_pushed: 0,
            events_deleted: deleted,
        })
    }

    async fn build_client(&self, account_id: Uuid) -> Result<CalDavClient, SyncError> {
        let creds = self.creds.load(account_id).await?;
        CalDavClient::new(
            creds.base_url.as_str(),
            Some(creds.username.as_str()),
            Some(creds.password.as_str()),
        )
        .map_err(map_err)
    }
}

fn map_err<E: std::fmt::Display>(e: E) -> SyncError {
    SyncError::Upstream(e.to_string())
}
