//! SeaORM-backed [`ActivityService`] implementation.
//!
//! Streams the audit log out of the `activities` SQLite table via
//! [`crate::activity::ActivityRepo`] and shapes it into the
//! [`ChangeRow`] view the rest of the system already speaks.
//!
//! Sync state and conflict tracking still need provider integration
//! that isn't part of the SeaORM repo set yet, so those operations
//! return a `provider not configured` error rather than fabricating
//! empty data.

use super::helpers::{convert_model, provider_not_configured};
use crate::activity::{Activity, ActivityApiList, ActivityRepo};
use crate::index::{ChangeRow, ConflictRow};
use crate::service::{ActivityService, ProviderSyncState, VaultError};

#[derive(Clone)]
pub struct ActivityServiceImpl<R> {
    activity_repo: R,
}

impl<R> ActivityServiceImpl<R> {
    pub fn new(activity_repo: R) -> Self {
        Self { activity_repo }
    }
}

impl<R> ActivityServiceImpl<R>
where
    R: ActivityRepo,
{
    async fn list_activity_models(&self, limit: u64) -> Result<Vec<Activity>, VaultError> {
        // Sort by created_at desc so the caller's `limit` lines up with
        // the "recent" semantics of the index-backed feed.
        self.activity_repo
            .list_activitys(None, Some("-created_at".to_string()), None, Some(limit))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<ActivityApiList, Activity>)
            .collect()
    }
}

impl<R> ActivityService for ActivityServiceImpl<R>
where
    R: ActivityRepo,
{
    async fn recent_activity(&self, limit: u32) -> Result<Vec<ChangeRow>, VaultError> {
        let limit = u64::from(limit.max(1));
        Ok(self
            .list_activity_models(limit)
            .await?
            .into_iter()
            .map(activity_to_change_row)
            .collect())
    }

    async fn list_sync_states(&self) -> Result<Vec<ProviderSyncState>, VaultError> {
        Err(provider_not_configured("list_sync_states"))
    }

    async fn list_conflicts(
        &self,
        _open_only: bool,
        _limit: u32,
    ) -> Result<Vec<ConflictRow>, VaultError> {
        Err(provider_not_configured("list_conflicts"))
    }

    async fn resolve_conflict(
        &self,
        _conflict_id: i64,
        _resolver: Option<String>,
        _how: String,
    ) -> Result<(), VaultError> {
        Err(provider_not_configured("resolve_conflict"))
    }
}

/// Map an `Activity` row from the SeaORM table onto the `ChangeRow`
/// view expected by callers of the index-backed activity feed.
fn activity_to_change_row(activity: Activity) -> ChangeRow {
    ChangeRow {
        entity_type: activity.entity_type,
        entity_id: activity.entity_id.to_string(),
        field: activity.field,
        old_value: activity.old_value,
        new_value: activity.new_value,
        changed_by: activity.actor,
        changed_at: activity.created_at.to_rfc3339(),
        file_path: None,
    }
}
