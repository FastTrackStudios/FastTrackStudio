//! Facade for the `foo` feature.

pub use foo_proto::*;

#[cfg(feature = "backend-memory")]
pub mod backend_memory {
    pub use foo_memory::FooRepoMemory;
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
