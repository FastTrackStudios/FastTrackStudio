//! Facade for the `example` feature.
//!
//! ```ignore
//! // pick a backend
//! example = { workspace = true, features = ["backend-db"] }
//! // or
//! example = { workspace = true, features = ["backend-memory"] }
//!
//! // then use the same trait surface regardless:
//! use example::{Example, ExampleRepo, ExampleCreate};
//! let repo = example::backend::new_default();   // backend-specific constructor below
//! repo.create(ExampleCreate { ... }).await?;
//! ```

// Re-export the wire contract unconditionally. Wasm clients can depend
// on this facade without enabling any backend.
pub use example_proto::*;

// ── Backend selection ─────────────────────────────────────────────────

#[cfg(feature = "backend-db")]
pub mod backend {
    //! SeaORM-backed implementation. `new(db)` constructs the storage
    //! from a `sea_orm::DatabaseConnection`.
    pub use example_db::{ExampleRepoStorage, Migrator};

    pub type ExampleBackend<C> = ExampleRepoStorage<C>;
}

#[cfg(feature = "backend-memory")]
pub mod backend {
    //! Pure in-memory implementation. No external services needed.
    pub use example_memory::ExampleRepoMemory;

    pub type ExampleBackend = ExampleRepoMemory;
}

#[cfg(not(any(feature = "backend-db", feature = "backend-memory")))]
compile_error!(
    "example: enable exactly one backend feature: \
     `backend-db` or `backend-memory`"
);
