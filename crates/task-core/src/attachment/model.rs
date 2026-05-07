//! Attachment entity — pointers + metadata for files stored in an external
//! provider (Nextcloud is the canonical storage). The bytes never live in
//! SQLite; only the row tying an `owner` (task/project/comment/etc.) to a
//! provider-relative `path` is persisted here.

use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

/// Default `source` value when none is supplied. Nextcloud is the canonical
/// user-storage provider, but rows can also point at `local`/`s3`/external
/// URL schemes — the service layer dispatches on this.
pub const DEFAULT_ATTACHMENT_SOURCE: &str = "nextcloud";

#[derive(
    Clone,
    Debug,
    Default,
    PartialEq,
    Facet,
    DeriveEntityModel,
    EntityToModels,
    Serialize,
    Deserialize,
)]
#[sea_orm(table_name = "attachments")]
#[crudcrate(
    api_struct = "AttachmentApi",
    generate_vox_service,
    name_singular = "attachment",
    name_plural = "attachments"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Owner entity id — the task/project/comment/etc. this attachment is
    /// hung off.
    #[crudcrate(filterable)]
    pub owner_id: Uuid,
    /// Owner entity type: "task" / "project" / "comment" / "calendar_event" /
    /// "person".
    #[crudcrate(filterable)]
    pub owner_type: String,

    /// Storage source: "nextcloud" (default) / "local" / "s3" / external URL
    /// scheme.
    #[crudcrate(filterable)]
    pub source: String,

    /// Provider-relative path (e.g. "Projects/Montreal Album/stems/master.wav").
    pub path: String,
    /// User-friendly label; if `None`, callers should derive it from the
    /// `path` basename.
    pub label: Option<String>,
    pub mime: Option<String>,
    pub size_bytes: Option<i64>,
    /// SHA-256 hex digest of the bytes (lowercase, no separators).
    pub checksum: Option<String>,
    pub uploader: Option<String>,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,
    #[crudcrate(exclude(create), exclude(update), on_create = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}

pub type Attachment = Model;

/// Derive a label from a provider-relative path's basename. Empty paths or
/// paths with no usable basename fall back to the path itself.
pub fn label_from_path(path: &str) -> String {
    path.rsplit('/')
        .find(|seg| !seg.is_empty())
        .map(str::to_string)
        .unwrap_or_else(|| path.to_string())
}

/// Default remote path layout used when a caller doesn't specify one.
pub fn default_remote_path(owner_type: &str, owner_id: Uuid, basename: &str) -> String {
    format!("attachments/{owner_type}/{owner_id}/{basename}")
}
