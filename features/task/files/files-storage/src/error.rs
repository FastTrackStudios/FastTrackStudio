//! This crate's own error type. Every lane maps it onto
//! [`files_storage_proto::StorageError`] at the RPC boundary — see
//! [`to_storage_error`].

use files_storage_proto::StorageError;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("io: {0}")]
    Io(#[from] std::io::Error),
    #[error("registry json: {0}")]
    Json(#[from] serde_json::Error),
    #[error("version store: {0}")]
    VersionStore(#[from] task_files_version_store::Error),
    #[error("chunk store: {0}")]
    ChunkStore(#[from] task_files_chunk_store::Error),
    #[error("jj repo: {0}")]
    Repo(String),
    #[error("not found: {0}")]
    NotFound(String),
    #[error("already exists: {0}")]
    AlreadyExists(String),
    #[error("bad request: {0}")]
    BadRequest(String),
    #[error("no storage grant: {0}")]
    NotGranted(String),
    #[error("capability denied: {0}")]
    CapabilityDenied(String),
    #[error("quota exceeded: {0}")]
    QuotaExceeded(String),
    #[error("agent not approved: {0}")]
    AgentNotApproved(String),
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn to_storage_error(err: Error) -> StorageError {
    match err {
        Error::NotFound(m) => StorageError::NotFound(m),
        Error::AlreadyExists(m) => StorageError::AlreadyExists(m),
        Error::BadRequest(m) => StorageError::BadRequest(m),
        Error::NotGranted(m) => StorageError::NotGranted(m),
        Error::CapabilityDenied(m) => StorageError::CapabilityDenied(m),
        Error::QuotaExceeded(m) => StorageError::QuotaExceeded(m),
        Error::AgentNotApproved(m) => StorageError::AgentNotApproved(m),
        Error::Io(e) => StorageError::Io(e.to_string()),
        Error::Json(e) => StorageError::Io(format!("registry json: {e}")),
        Error::VersionStore(e) => StorageError::Io(format!("version store: {e}")),
        Error::ChunkStore(e) => StorageError::Io(format!("chunk store: {e}")),
        Error::Repo(m) => StorageError::Io(format!("jj repo: {m}")),
    }
}
