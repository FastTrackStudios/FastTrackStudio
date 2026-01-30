//! Extension Host for WASM - In-Memory Service Bundling
//!
//! This crate provides a WASM-compatible extension host that bundles services
//! into a single binary. Services are instantiated directly and wrapped with
//! in-memory clients.
//!
//! Unlike the native SHM-based extension host which spawns separate processes,
//! this runs all services in the same WASM instance for browser compatibility.
//!
//! # Architecture
//!
//! ```text
//! ┌──────────────────────────────────────────┐
//! │  WASM Bundle (Single Binary)            │
//! │  ┌────────────────────────────────────┐ │
//! │  │  Service Implementations           │ │
//! │  │  - DawMockTransport                │ │
//! │  │  - HelloWorldImpl                  │ │
//! │  │  (Direct method calls)             │ │
//! │  └────────┬───────────────────────────┘ │
//! │           │                              │
//! │  ┌────────▼───────────────────────────┐ │
//! │  │  Dioxus UI (same as desktop!)      │ │
//! │  └────────────────────────────────────┘ │
//! └──────────────────────────────────────────┘
//! ```
//!
//! For true WASM compatibility, we use direct method calls instead of
//! tunnels/transports since those require tokio features not available in browser.

use std::collections::HashMap;
use std::marker::PhantomData;

/// WASM extension host that bundles services in-memory
///
/// This is a simple registry that stores service implementations.
/// Clients call services directly (no RPC overhead in WASM).
pub struct WasmExtensionHost {
    _marker: PhantomData<()>,
}

impl WasmExtensionHost {
    /// Create a new WASM extension host
    pub fn new() -> Self {
        tracing::info!("Creating WASM extension host");
        Self {
            _marker: PhantomData,
        }
    }
}

impl Default for WasmExtensionHost {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper to create service clients for WASM
///
/// In WASM, we can use the service implementations directly without RPC.
/// This is much simpler and faster than the tunnel-based approach.
pub fn create_service_clients() -> WasmServiceClients {
    WasmServiceClients::new()
}

/// Container for WASM service clients
pub struct WasmServiceClients {
    services: HashMap<String, String>,
}

impl WasmServiceClients {
    pub fn new() -> Self {
        Self {
            services: HashMap::new(),
        }
    }

    pub fn register(&mut self, name: impl Into<String>) {
        let name = name.into();
        tracing::info!("Registered WASM service: {}", name);
        self.services.insert(name.clone(), name);
    }

    pub fn list(&self) -> Vec<String> {
        self.services.keys().cloned().collect()
    }
}

impl Default for WasmServiceClients {
    fn default() -> Self {
        Self::new()
    }
}
