//! Sync status types and service definition

use chrono::{DateTime, Utc};
use uuid::Uuid;

/// Current sync operation state
#[repr(u8)]
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum SyncState {
    /// No sync in progress, everything up to date
    Idle,
    /// Currently syncing data
    Syncing,
    /// Sync completed successfully
    Synced,
    /// Sync encountered an error
    Error(String),
    /// Not connected / not authenticated
    #[default]
    Offline,
}

/// Service for querying and subscribing to sync status
#[async_trait::async_trait]
pub trait SyncStatusService: Send + Sync {
    /// Get the current sync state
    async fn get_sync_state(&self) -> SyncState;

    /// Get the last sync timestamp as ISO 8601 string, if any
    async fn get_last_sync_time(&self) -> Option<String>;
}

// region: --- Sync Engine Types

/// Per-entity synchronization status
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyncStatus {
    /// Entity is fully synchronized with the remote
    Synced,
    /// Entity has local changes not yet pushed
    Pending,
    /// Entity has conflicting local and remote changes
    Conflict,
    /// Entity exists only locally (never synced)
    LocalOnly,
}

impl SyncStatus {
    /// String representation for storage in the `sync_metadata` table.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Synced => "synced",
            Self::Pending => "pending",
            Self::Conflict => "conflict",
            Self::LocalOnly => "local_only",
        }
    }

    /// Parse from a database string value.
    pub fn from_str_value(s: &str) -> Self {
        match s {
            "synced" => Self::Synced,
            "pending" => Self::Pending,
            "conflict" => Self::Conflict,
            "local_only" => Self::LocalOnly,
            _ => Self::Pending,
        }
    }
}

/// Strategy for resolving sync conflicts.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum ConflictStrategy {
    /// Most recent `updated_at` wins
    #[default]
    LastWriteWins,
    /// Merge non-overlapping field changes (JSON-level merge)
    Merge,
    /// Prompt the user to choose which version to keep
    AskUser,
    /// Keep both versions: local stays, remote gets a copy with a suffix
    KeepBoth,
}

/// The type of entity being synced.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EntityType {
    Preset,
    Snapshot,
    ModuleChunk,
}

impl EntityType {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Preset => "preset",
            Self::Snapshot => "snapshot",
            Self::ModuleChunk => "module_chunk",
        }
    }

    pub fn from_str_value(s: &str) -> Option<Self> {
        match s {
            "preset" => Some(Self::Preset),
            "snapshot" => Some(Self::Snapshot),
            "module_chunk" => Some(Self::ModuleChunk),
            _ => None,
        }
    }
}

/// A detected change to an entity (for delta sync).
#[derive(Debug, Clone)]
pub struct EntityChange {
    /// Which kind of entity changed
    pub entity_type: EntityType,
    /// ID of the changed entity
    pub entity_id: Uuid,
    /// The local version number
    pub local_version: i64,
    /// The remote version number (0 if never synced)
    pub remote_version: i64,
    /// When the entity was last modified locally
    pub modified_at: DateTime<Utc>,
    /// Current sync status
    pub status: SyncStatus,
}

/// Information about a detected conflict.
#[derive(Debug, Clone)]
pub struct ConflictInfo {
    /// Which kind of entity is in conflict
    pub entity_type: EntityType,
    /// ID of the conflicting entity
    pub entity_id: Uuid,
    /// Local version number
    pub local_version: i64,
    /// Remote version number
    pub remote_version: i64,
    /// When the local copy was last modified
    pub local_modified_at: DateTime<Utc>,
    /// When the remote copy was last modified
    pub remote_modified_at: DateTime<Utc>,
}

/// How a conflict was resolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConflictResolution {
    /// Kept the local version
    KeptLocal,
    /// Kept the remote version
    KeptRemote,
    /// Merged both versions
    Merged,
    /// Kept both as separate entities
    KeptBoth,
    /// User needs to decide (deferred)
    Deferred,
}

/// Progress report emitted during a sync operation.
#[derive(Debug, Clone)]
pub struct SyncProgress {
    /// Total entities to sync
    pub total: usize,
    /// Entities synced so far
    pub completed: usize,
    /// Number of conflicts detected
    pub conflicts: usize,
    /// Number of errors encountered
    pub errors: usize,
    /// Human-readable status message
    pub message: String,
}

impl SyncProgress {
    pub fn new(total: usize) -> Self {
        Self {
            total,
            completed: 0,
            conflicts: 0,
            errors: 0,
            message: String::new(),
        }
    }

    pub fn is_complete(&self) -> bool {
        self.completed >= self.total
    }

    pub fn fraction(&self) -> f64 {
        if self.total == 0 {
            1.0
        } else {
            self.completed as f64 / self.total as f64
        }
    }
}

/// Trait for the sync engine service.
#[async_trait::async_trait]
pub trait SyncEngineService: Send + Sync {
    /// Run a full sync cycle (delta-based).
    async fn sync(&self) -> Result<SyncProgress, String>;

    /// Get all entities with pending changes.
    async fn get_pending_changes(&self) -> Result<Vec<EntityChange>, String>;

    /// Get all detected conflicts.
    async fn get_conflicts(&self) -> Result<Vec<ConflictInfo>, String>;

    /// Resolve a specific conflict with the given strategy.
    async fn resolve_conflict(
        &self,
        entity_type: EntityType,
        entity_id: Uuid,
        strategy: ConflictStrategy,
    ) -> Result<ConflictResolution, String>;

    /// Mark an entity as locally modified (adds to pending queue).
    async fn mark_modified(
        &self,
        entity_type: EntityType,
        entity_id: Uuid,
    ) -> Result<(), String>;

    /// Get the sync status for a specific entity.
    async fn get_entity_status(
        &self,
        entity_type: EntityType,
        entity_id: Uuid,
    ) -> Result<SyncStatus, String>;
}

// endregion: --- Sync Engine Types
