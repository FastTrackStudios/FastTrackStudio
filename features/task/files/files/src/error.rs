//! This crate's own error type (repo open/init, registry I/O, live-tree
//! scans). [`FilesBackend`](crate::FilesBackend) maps every variant onto
//! [`files_proto::FilesError`] at the RPC boundary — see
//! `backend::to_files_error`.

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("io: {0}")]
    Io(#[from] std::io::Error),
    #[error("registry json: {0}")]
    Json(#[from] serde_json::Error),
    #[error("version store: {0}")]
    VersionStore(#[from] task_files_version_store::Error),
    #[error("jj repo: {0}")]
    Repo(String),
    #[error("not found: {0}")]
    NotFound(String),
    #[error("already exists: {0}")]
    AlreadyExists(String),
    #[error("bad request: {0}")]
    BadRequest(String),
}

pub type Result<T> = std::result::Result<T, Error>;
