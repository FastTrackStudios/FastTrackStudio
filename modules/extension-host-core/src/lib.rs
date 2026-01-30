//! Extension Host Core - Reusable ROAM extension system
//!
//! This crate provides a reusable extension hosting system that works in:
//! - REAPER extension (process isolation via SHM)
//! - Standalone desktop app (process isolation via SHM)
//! - Web/WASM clients (via HTTP gateway)
//!
//! # Features
//!
//! - `shm` - SHM transport for process isolation (requires native OS)
//! - `http` - HTTP gateway for web/mobile access
//! - `default` - All features enabled
//!
//! # Architecture
//!
//! ```text
//! ┌────────────────────────────────────────┐
//! │  Extension Host Core                   │
//! │  ┌──────────────────────────────────┐  │
//! │  │  SHM Hub (optional)              │  │
//! │  │  - Multi-peer driver             │  │
//! │  │  - Lazy spawning                 │  │
//! │  └──────────────────────────────────┘  │
//! │  ┌──────────────────────────────────┐  │
//! │  │  Extension Registry              │  │
//! │  │  - name → peer_id mapping        │  │
//! │  │  - Hot-reload detection          │  │
//! │  └──────────────────────────────────┘  │
//! │  ┌──────────────────────────────────┐  │
//! │  │  Tracing Aggregator              │  │
//! │  │  - Cross-extension logging       │  │
//! │  └──────────────────────────────────┘  │
//! │  ┌──────────────────────────────────┐  │
//! │  │  HTTP Gateway (optional)         │  │
//! │  │  - JSON ↔ postcard bridge        │  │
//! │  └──────────────────────────────────┘  │
//! └────────────────────────────────────────┘
//! ```

#![deny(unsafe_code)]

#[cfg(feature = "shm")]
pub mod shm_hub;

#[cfg(feature = "shm")]
pub mod hot_reload;

pub mod registry;
pub mod tracing_aggregator;
pub mod host_service;

// HTTP gateway is currently a separate extension
// Future: Integrate roam-http-bridge directly into core
// #[cfg(feature = "http")]
// pub mod http_gateway;

// Re-export key types
pub use registry::{ExtensionMetadata, ExtensionRegistry};
pub use tracing_aggregator::TracingAggregator;
pub use host_service::{HostServiceImpl, create_host_dispatcher};

#[cfg(feature = "shm")]
pub use shm_hub::ShmHub;

#[cfg(feature = "shm")]
pub use hot_reload::{HotReloadWatcher, WatchEvent};

// Re-export ROAM types for convenience
pub use roam_session::ConnectionHandle;
