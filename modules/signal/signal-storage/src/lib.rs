//! Signal storage layer built on SeaORM.

pub mod collection_repo;
pub mod engine_repo;
pub mod entity;
pub mod layer_repo;
pub mod profile_repo;
pub mod rig_repo;
pub mod song_repo;

pub use collection_repo::{
    default_seed_block_collections, default_seed_module_collections, CollectionRepo,
    CollectionRepoLive,
};
pub use engine_repo::{EngineRepo, EngineRepoLive};
pub use layer_repo::{LayerRepo, LayerRepoLive};
pub use profile_repo::{ProfileRepo, ProfileRepoLive};
pub use rig_repo::{RigRepo, RigRepoLive};
pub use sea_orm::{Database, DatabaseConnection, DbErr};
pub use song_repo::{SongRepo, SongRepoLive};

#[derive(Debug, thiserror::Error)]
pub enum StorageError {
    #[error("database error: {0}")]
    Db(#[from] DbErr),
    #[error("data error: {0}")]
    Data(String),
}

pub type StorageResult<T> = Result<T, StorageError>;
