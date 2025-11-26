//! Transport Infrastructure
//!
//! This module contains all the infrastructure adapters for the transport domain.
//! It provides protocol-specific implementations (HTTP, OSC, WebSocket, etc.)
//! that adapt the core transport domain to various communication protocols.
//!
//! ## Architecture
//!
//! The infrastructure layer follows the ports and adapters pattern:
//! - **Ports**: Defined by the core domain (TransportActions trait)
//! - **Adapters**: Protocol-specific implementations in this module
//!
//! ## Protocol Adapters
//!
//! - `http.rs` - HTTP/REST API adapter
//! - `osc.rs` - OSC (Open Sound Control) adapter (future)
//! - `websocket.rs` - WebSocket real-time adapter (future)
//! - `grpc.rs` - gRPC adapter (future)

pub mod http;
#[cfg(not(target_arch = "wasm32"))]
pub mod irpc;
pub mod stream;

// Future protocol adapters
// pub mod midi;

// Re-export all infrastructure adapters
pub use http::create_transport_http_router;

// Future re-exports
// pub use midi::create_transport_midi_service;
