//! DAW Standalone Service - Transport Only
//!
//! Minimal standalone implementation for testing without REAPER.
//! Works in WASM and native.

mod local_client;
pub use local_client::LocalDawClient;
pub use transport::DawStandaloneTransport;

pub mod transport;

use daw_proto::*;
use roam_session::Context;
use std::sync::Arc;
use tracing::info;
