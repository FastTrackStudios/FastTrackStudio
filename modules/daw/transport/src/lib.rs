//! Transport Module
//!
//! This module provides comprehensive transport control functionality for DAW applications.
//! It follows a clean architecture with separation between core domain logic and
//! infrastructure adapters.
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    INFRASTRUCTURE                           │
//! │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐ │
//! │  │    HTTP     │  │    OSC      │  │    WebSocket        │ │
//! │  │  (REST API) │  │ (Real-time) │  │  (Real-time)        │ │
//! │  └─────────────┘  └─────────────┘  └─────────────────────┘ │
//! └─────────────────────────┬───────────────────────────────────┘
//!                           │ Adapters
//! ┌─────────────────────────▼───────────────────────────────────┐
//! │                      CORE DOMAIN                            │
//! │  ┌─────────────────────────────────────────────────────────┐ │
//! │  │  TransportActions trait (Port)                          │ │
//! │  │  Transport, Tempo, PlayState, RecordMode (Types)       │ │
//! │  │  TransportError (Error Handling)                       │ │
//! │  └─────────────────────────────────────────────────────────┘ │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! ## Usage
//!
//! ### Using Core Domain Types
//! ```rust
//! use transport::core::{Transport, TransportActions, Tempo, PlayState};
//!
//! let mut transport = Transport::new();
//! transport.play()?;
//! transport.set_tempo(Tempo::new(140.0))?;
//! ```
//!
//! ### Using HTTP Infrastructure
//! ```rust
//! use transport::infra::create_transport_http_router;
//! use transport::core::{Transport, TransportActions};
//! use axum::Router;
//! use std::sync::Arc;
//! use tokio::sync::Mutex;
//!
//! let transport = Arc::new(Mutex::new(Transport::new()));
//! let app = Router::new()
//!     .nest("/transport", create_transport_http_router::<Transport>())
//!     .with_state(transport);
//! ```

// Core domain module
pub mod core;

// Application layer module
pub mod application;

// Infrastructure adapters module
pub mod infra;

// Re-export core types for convenience
pub use core::{
    Transport, TransportActions, TransportError,
    Tempo, PlayState, RecordMode
};

// Re-export application types for convenience
pub use application::mock::MockTransportService;

// Re-export infrastructure adapters for convenience
pub use infra::{
    create_transport_http_router,
    create_transport_ws_router,
    WebSocketHandler,
    TransportMessage,
    TransportCommand,
    TransportStatus,
};

// Future infrastructure exports
// pub use infra::{
//     create_transport_osc_handler,
//     create_transport_grpc_service,
// };
