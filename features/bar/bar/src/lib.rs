//! Facade for the `bar` feature.

pub use bar_proto::*;

#[cfg(feature = "backend-memory")]
pub mod backend_memory {
    pub use bar_memory::BarRepoMemory;
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
