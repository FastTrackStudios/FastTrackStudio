//! Host Runtime Library
//!
//! Provides the shared infrastructure for running FastTrackStudio host processes.
//! Both test-extension and reaper-extension use this library.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────┐
//! │                  Host Runtime                        │
//! │                                                      │
//! │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐ │
//! │  │   Host      │  │   Cells     │  │  Tracing    │ │
//! │  │  Singleton  │  │  Registry   │  │  Consumer   │ │
//! │  └─────────────┘  └─────────────┘  └─────────────┘ │
//! │                                                      │
//! │  ┌─────────────┐  ┌─────────────┐                   │
//! │  │ Forwarder   │  │ Unix Server │                   │
//! │  │ (RPC Route) │  │ (Desktop)   │                   │
//! │  └─────────────┘  └─────────────┘                   │
//! └─────────────────────────────────────────────────────┘
//! ```
//!
//! # Usage
//!
//! ```rust,ignore
//! use host_runtime::{Host, CellConfig, init_shm_infrastructure, init_tracing};
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     init_tracing();
//!     let _temp_dir = init_shm_infrastructure().await?;
//!
//!     let cell_dir = host_runtime::default_cell_dir();
//!     CellConfig::new("daw-standalone", &cell_dir).register();
//!
//!     // ... spawn cells and run
//! }
//! ```

pub mod cell_host;
pub mod cells;
pub mod forwarder;
pub mod host;
pub mod hot_reload;
pub mod tracing;
pub mod unix_server;

// Re-export commonly used types
pub use cell_host::CellHost;
pub use cells::{cell_ready_registry, CellReadyRegistry};
pub use forwarder::{MultiForwarder, RebindableHandle};
pub use host::{default_cell_dir, init_shm_infrastructure, CellConfig, Host};
pub use hot_reload::{HotReloadWatcher, WatchEvent};
pub use tracing::{init_tracing, spawn_tracing_consumer};
