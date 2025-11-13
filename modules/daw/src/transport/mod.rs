//! Universal Transport Module
//!
//! This module implements the Universal Network Architecture for transport control,
//! enabling any device to communicate with any other device using any protocol.
//!
//! ## Architecture Overview
//!
//! The transport module is organized into 4 distinct layers that mirror the
//! Universal Network Architecture:
//!
//! ```
//! ┌─────────────────────────────────────────────────────────────────┐
//! │          LAYER 1: INFRASTRUCTURE (Physical Network)             │
//! │  • Protocol servers (HTTP, OSC, gRPC, WebSocket)               │
//! │  • Multi-protocol routing and failover                         │
//! │  • Device discovery and network topology                       │
//! │  • Connection health monitoring                                 │
//! └─────────────────────────────────────────────────────────────────┘
//!                                   ↓
//! ┌─────────────────────────────────────────────────────────────────┐
//! │            LAYER 2: ADAPTERS (Protocol Layer)                  │
//! │  • Tower Services for each protocol                            │
//! │  • Protocol-specific message conversion                        │
//! │  • HTTP ↔ OSC ↔ gRPC ↔ MIDI → Domain Types                   │
//! └─────────────────────────────────────────────────────────────────┘
//!                                   ↓
//! ┌─────────────────────────────────────────────────────────────────┐
//! │              LAYER 3: CORE (Domain Logic)                      │
//! │  • Protocol-agnostic DomainRequest/DomainResponse types        │
//! │  • Domain routing logic and business rules                     │
//! │  • TransportActions trait and core types                       │
//! └─────────────────────────────────────────────────────────────────┘
//!                                   ↓
//! ┌─────────────────────────────────────────────────────────────────┐
//! │          LAYER 4: APPLICATION (Device Implementation)          │
//! │  • Concrete transport implementations                          │
//! │  • MockTransport, ReaperTransport, StandaloneTransport         │
//! │  • Device-specific API integration                             │
//! └─────────────────────────────────────────────────────────────────┘
//! ```
//!
//! ## Module Organization
//!
//! ### `core/` - Domain Logic and Core Types
//! - `actions.rs` - TransportActions trait definition
//! - `transport.rs` - Core transport types (Transport, Tempo, PlayState)
//! - `error.rs` - TransportError definitions
//! - `domain.rs` - DomainRequest/DomainResponse types
//! - `domain_router.rs` - Domain routing logic
//!
//! ### `application/` - Device Implementations
//! - `mock.rs` - MockTransport for testing
//! - `standalone.rs` - StandaloneTransport for self-contained apps
//! - `reaper.rs` - ReaperTransport for REAPER DAW integration
//! - `web.rs` - WebTransport for browser-based applications
//!
//! ### `adapters/` - Protocol Layer (Tower Services)
//! - `osc/` - OSC protocol adapter
//! - `http/` - HTTP/REST protocol adapter
//! - `grpc/` - gRPC protocol adapter
//! - `midi/` - MIDI protocol adapter
//! - `websocket/` - WebSocket protocol adapter
//!
//! ### `infrastructure/` - Network Layer
//! - `servers/` - Actual protocol servers
//! - `multi_protocol_router.rs` - Multi-protocol routing
//! - `discovery/` - Device discovery mechanisms
//! - `routing/` - Advanced network routing
//!
//! ## Key Benefits
//!
//! 1. **Protocol Independence**: Any protocol can control any device
//! 2. **Device Independence**: Same code works with any transport implementation
//! 3. **Network Intelligence**: Automatic routing, failover, and optimization
//! 4. **Easy Extension**: Add new protocols or devices without changing existing code
//! 5. **Type Safety**: Compile-time guarantees for all operations
//! 6. **Testing**: Mock implementations for comprehensive testing
//!
//! ## Quick Start
//!
//! ```rust,ignore
//! use daw::transport::{
//!     // Application layer
//!     application::MockTransport,
//!
//!     // Core domain layer
//!     core::{TransportActions, DomainRequest, DomainResponse},
//!     core::domain_router::TransportDomainHandler,
//!
//!     // Protocol adapters
//!     adapters::osc::{OscService, OscRequest, OscArg},
//!
//!     // Infrastructure
//!     infrastructure::InfrastructureBuilder,
//! };
//! use tower::ServiceExt;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     // 1. Create transport implementation (Layer 4)
//!     let transport = MockTransport::new();
//!
//!     // 2. Create domain handler (Layer 3)
//!     let domain_handler = TransportDomainHandler::new(
//!         transport,
//!         "my_node".to_string()
//!     );
//!
//!     // 3. Create protocol adapter (Layer 2)
//!     let mut osc_service = OscService::new(domain_handler);
//!
//!     // 4. Process requests through the stack
//!     let request = OscRequest::new("/transport/play".to_string(), vec![]);
//!     let response = osc_service.ready().await?.call(request).await?;
//!
//!     println!("Response: {} {:?}", response.address, response.args);
//!
//!     // 5. Or use full infrastructure (Layer 1)
//!     let mut infrastructure = InfrastructureBuilder::new()
//!         .node_id("studio_node".to_string())
//!         .http_port(8000)
//!         .osc_port(7000)
//!         .build(MockTransport::new());
//!
//!     infrastructure.start().await?;
//!
//!     // Now HTTP and OSC servers are running, both controlling the same transport!
//!
//!     Ok(())
//! }
//! ```

// Layer 4: Application (Device Implementation)
pub mod application;

// Layer 3: Core (Domain Logic)
pub mod core;


// ============================================================================
// PUBLIC RE-EXPORTS
// ============================================================================

// Core domain types and traits (most commonly used)
pub use core::{
    TransportActions, TransportAction, TransportError,
    Transport, Tempo, PlayState, RecordMode,
};

// Application implementations
pub use application::{
    MockTransport,
    ApplicationTransport,
    TransportBuilder, TransportType, TransportConfig,
    mock_transport, mock_transport_with_id,
};


// ============================================================================
// CONVENIENCE FUNCTIONS
// ============================================================================

/// Create a complete transport system with OSC and HTTP support
///
/// This is the easiest way to get started with the universal transport architecture.
/// It creates a full stack from application layer to infrastructure layer.


/// Create a mock transport system for testing


/// Create a development transport system with logging


// ============================================================================
// INTEGRATION HELPERS
// ============================================================================

/// Helper struct for building complete transport solutions
pub struct UniversalTransportBuilder<T: TransportActions + Send + Sync + 'static> {
    transport: Option<T>,
    node_id: String,
    enable_osc: bool,
    enable_http: bool,
    enable_grpc: bool,
    osc_port: u16,
    http_port: u16,
    grpc_port: u16,
}

impl<T: TransportActions + Send + Sync + 'static> UniversalTransportBuilder<T> {
    /// Create a new universal transport builder
    pub fn new() -> Self {
        Self {
            transport: None,
            node_id: "universal_node".to_string(),
            enable_osc: true,
            enable_http: true,
            enable_grpc: false,
            osc_port: 7000,
            http_port: 8000,
            grpc_port: 9000,
        }
    }

    /// Set the transport implementation
    pub fn transport(mut self, transport: T) -> Self {
        self.transport = Some(transport);
        self
    }

    /// Set the node identifier
    pub fn node_id(mut self, node_id: String) -> Self {
        self.node_id = node_id;
        self
    }

    /// Enable/disable OSC protocol
    pub fn enable_osc(mut self, enabled: bool) -> Self {
        self.enable_osc = enabled;
        self
    }

    /// Enable/disable HTTP protocol
    pub fn enable_http(mut self, enabled: bool) -> Self {
        self.enable_http = enabled;
        self
    }

    /// Enable/disable gRPC protocol (future)
    pub fn enable_grpc(mut self, enabled: bool) -> Self {
        self.enable_grpc = enabled;
        self
    }

    /// Set OSC port
    pub fn osc_port(mut self, port: u16) -> Self {
        self.osc_port = port;
        self
    }

    /// Set HTTP port
    pub fn http_port(mut self, port: u16) -> Self {
        self.http_port = port;
        self
    }

    /// Set gRPC port
    pub fn grpc_port(mut self, port: u16) -> Self {
        self.grpc_port = port;
        self
    }


}

impl<T: TransportActions + Send + Sync + 'static> Default for UniversalTransportBuilder<T> {
    fn default() -> Self {
        Self::new()
    }
}
