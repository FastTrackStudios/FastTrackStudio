//! Server-internal persistence for the per-task Loro snapshot column.
//!
//! `Task::crdt_snapshot` is excluded from the crudcrate-generated API surface
//! (it never crosses the wire), so the standard `TaskRepo::update_task` path
//! cannot write it. The [`CrdtSnapshotStore`] trait gives the realtime path a
//! narrow, type-erased accessor that updates *only* the snapshot column.
//!
//! The default implementation is provided directly against
//! [`sea_orm::DatabaseConnection`], so the server can hand a single
//! `Arc<dyn CrdtSnapshotStore>` to every service that mutates tasks.

use std::sync::Arc;

use async_trait::async_trait;
use sea_orm::{ActiveModelTrait, ActiveValue, ConnectionTrait, DatabaseConnection, EntityTrait};
use uuid::Uuid;

use crate::service::VaultError;
use crate::task::{self, Entity as TaskEntity};

/// Read/write access to the `tasks.crdt_snapshot` column.
#[async_trait]
pub trait CrdtSnapshotStore: Send + Sync + 'static {
    /// Load the persisted Loro snapshot bytes for `task_id`, or `None` if the
    /// task has never produced one yet.
    async fn load_snapshot(&self, task_id: Uuid) -> Result<Option<Vec<u8>>, VaultError>;

    /// Persist `bytes` to `tasks.crdt_snapshot` for `task_id`. `None` clears
    /// the column (used by the delete path).
    async fn save_snapshot(&self, task_id: Uuid, bytes: Option<Vec<u8>>) -> Result<(), VaultError>;
}

/// Convenience alias for the `Arc`-erased object the services hold.
pub type SharedCrdtSnapshotStore = Arc<dyn CrdtSnapshotStore>;

/// Default SeaORM-backed store that talks straight to the `tasks` table.
#[derive(Clone)]
pub struct SeaOrmCrdtSnapshotStore<C: ConnectionTrait + Clone + Send + Sync + 'static> {
    db: C,
}

impl<C: ConnectionTrait + Clone + Send + Sync + 'static> SeaOrmCrdtSnapshotStore<C> {
    pub fn new(db: C) -> Self {
        Self { db }
    }
}

impl SeaOrmCrdtSnapshotStore<DatabaseConnection> {
    /// Wrap an existing pool in an `Arc<dyn CrdtSnapshotStore>` for service
    /// deps.
    pub fn shared(db: DatabaseConnection) -> SharedCrdtSnapshotStore {
        Arc::new(Self::new(db))
    }
}

#[async_trait]
impl<C: ConnectionTrait + Clone + Send + Sync + 'static> CrdtSnapshotStore
    for SeaOrmCrdtSnapshotStore<C>
{
    async fn load_snapshot(&self, task_id: Uuid) -> Result<Option<Vec<u8>>, VaultError> {
        let row = TaskEntity::find_by_id(task_id)
            .one(&self.db)
            .await
            .map_err(|err| VaultError::ParseError(format!("load crdt snapshot: {err}")))?;
        Ok(row.and_then(|t| t.crdt_snapshot))
    }

    async fn save_snapshot(&self, task_id: Uuid, bytes: Option<Vec<u8>>) -> Result<(), VaultError> {
        let active = task::ActiveModel {
            id: ActiveValue::Unchanged(task_id),
            crdt_snapshot: ActiveValue::Set(bytes),
            ..Default::default()
        };
        active
            .update(&self.db)
            .await
            .map_err(|err| VaultError::ParseError(format!("save crdt snapshot: {err}")))?;
        Ok(())
    }
}
