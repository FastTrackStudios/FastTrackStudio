//! Signal storage layer built on SeaORM.

pub mod block_repo;
pub mod entity;

pub use block_repo::{default_seed_module_presets, default_seed_presets, BlockRepo, BlockRepoLive};
pub use sea_orm::{Database, DatabaseConnection, DbErr};

#[derive(Debug, thiserror::Error)]
pub enum StorageError {
    #[error("database error: {0}")]
    Db(#[from] DbErr),
    #[error("data error: {0}")]
    Data(String),
}

pub type StorageResult<T> = Result<T, StorageError>;
