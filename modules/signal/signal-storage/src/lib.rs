//! Signal storage layer built on SeaORM.

pub mod collection_repo;
pub mod entity;

pub use collection_repo::{
    default_seed_block_collections, default_seed_module_collections, CollectionRepo,
    CollectionRepoLive,
};
pub use sea_orm::{Database, DatabaseConnection, DbErr};

#[derive(Debug, thiserror::Error)]
pub enum StorageError {
    #[error("database error: {0}")]
    Db(#[from] DbErr),
    #[error("data error: {0}")]
    Data(String),
}

pub type StorageResult<T> = Result<T, StorageError>;
