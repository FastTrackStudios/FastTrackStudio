//! Facade for the `example` feature.
//!
//! Re-exports the wire contract (`example_proto::*`) unconditionally so
//! wasm clients can pull this crate with no backend features at all.
//!
//! Backends live in their own modules — enable whichever feature(s)
//! you need. Both can coexist (e.g. e2e tests that want the in-memory
//! path while a sibling binary uses sqlite), so they DON'T share a
//! single `backend` module name.
//!
//! ```ignore
//! # apps/server/Cargo.toml
//! example = { workspace = true, features = ["backend-db"] }
//!
//! # tests/Cargo.toml
//! example = { workspace = true, features = ["backend-memory"] }
//! ```

pub use example_proto::*;

#[cfg(feature = "backend-db")]
pub mod backend_db {
    //! SeaORM-backed implementation.
    pub use example_db::{ExampleRepoStorage, Migrator};
}

#[cfg(feature = "backend-memory")]
pub mod backend_memory {
    //! Pure in-memory implementation. No external services needed.
    pub use example_memory::ExampleRepoMemory;
}
