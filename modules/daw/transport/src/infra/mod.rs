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
pub mod websocket;
pub mod osc;

// Future protocol adapters
// pub mod grpc;
// pub mod midi;

// Re-export all infrastructure adapters
pub use http::create_transport_http_router;
pub use websocket::{create_transport_ws_router, WebSocketHandler, TransportMessage, TransportCommand, TransportStatus};
pub use osc::{create_transport_osc_server, OscTransportServer};

// Future re-exports
// pub use grpc::create_transport_grpc_service;
