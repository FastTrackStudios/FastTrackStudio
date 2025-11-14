//! Transport Application Layer
//!
//! This module contains concrete implementations of the TransportActions trait
//! for different types of applications and devices. It represents Layer 4
//! (Application Layer) from the Universal Network Architecture.
//!
//! Each implementation maps domain requests to actual device-specific functionality:
//! - MockTransport: Testing and development
//! - TransportApp: Self-contained applications with real timing
//! - ReaperTransport: REAPER DAW integration (future)
//! - ProToolsTransport: Avid Pro Tools integration (future)
//! - WebTransport: Browser-based DAW integration (future)
//!
//! The application layer is where the rubber meets the road - these implementations
//! actually control real DAWs, hardware devices, or provide mock functionality
//! for testing. They are completely isolated from protocol concerns and work
//! with any protocol adapter in the system.

pub mod app;
pub mod mock;

// Future implementations
// pub mod standalone;
// pub mod reaper;
// pub mod protools;
// pub mod web;

// Re-export implementations with proper aliasing to avoid conflicts
pub use app::{TransportApp, TransportConfig as AppTransportConfig, TransportEvent, TransportStats};
pub use mock::MockTransportService;

// Re-export for convenience
pub use crate::core::TransportActions;

/// Trait alias for transport implementations that can be used across the system
pub trait ApplicationTransport: TransportActions + Send + Sync + 'static {}

// Implement the trait alias for all concrete types
impl ApplicationTransport for MockTransportService {}
impl ApplicationTransport for TransportApp {}

/// Helper function to create a transport implementation (removed boxed version due to async trait incompatibility)
// Note: Async traits are not dyn-compatible, so we work with concrete types

// ============================================================================
// BUILDER PATTERNS FOR COMMON SETUPS
// ============================================================================

/// Builder for creating transport implementations with common configurations
pub struct TransportBuilder {
    transport_type: TransportType,
    config: BuilderConfig,
}

/// Types of transport implementations available

#[derive(Debug, Clone)]
pub enum TransportType {
    Mock,
    App,
    // Standalone,
    // Reaper { path: String },
    // ProTools { session: String },
    // Web { endpoint: String },
}

/// Configuration options for transport builder (separate from TransportApp config)
#[derive(Debug, Clone, Default)]
pub struct BuilderConfig {
    pub node_id: Option<String>,
    pub auto_start: bool,
    pub buffer_size: Option<usize>,
    pub sample_rate: Option<u32>,
}

impl TransportBuilder {
    /// Create a new transport builder
    pub fn new() -> Self {
        Self {
            transport_type: TransportType::Mock,
            config: BuilderConfig::default(),
        }
    }

    /// Set the transport type
    pub fn transport_type(mut self, transport_type: TransportType) -> Self {
        self.transport_type = transport_type;
        self
    }

    /// Set the node ID
    pub fn node_id(mut self, node_id: String) -> Self {
        self.config.node_id = Some(node_id);
        self
    }

    /// Enable auto-start functionality
    pub fn auto_start(mut self, enabled: bool) -> Self {
        self.config.auto_start = enabled;
        self
    }

    /// Set buffer size
    pub fn buffer_size(mut self, size: usize) -> Self {
        self.config.buffer_size = Some(size);
        self
    }

    /// Set sample rate
    pub fn sample_rate(mut self, rate: u32) -> Self {
        self.config.sample_rate = Some(rate);
        self
    }

    /// Build a mock transport implementation
    pub fn build_mock(self) -> Result<MockTransportService, String> {
        match self.transport_type {
            TransportType::Mock => {
                let transport = MockTransportService::new();
                Ok(transport)
            }
            _ => Err("TransportType mismatch for mock transport".to_string()),
        }
    }

    /// Build an app transport implementation
    pub fn build_app(self) -> Result<TransportApp, String> {
        match self.transport_type {
            TransportType::App => {
                let config = AppTransportConfig::default();
                let transport = TransportApp::new(config);
                Ok(transport)
            }
            _ => Err("TransportType mismatch for app transport".to_string()),
        }
    }
}

impl Default for TransportBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// CONVENIENCE FUNCTIONS
// ============================================================================

/// Create a mock transport for testing
pub fn mock_transport() -> MockTransportService {
    MockTransportService::new()
}

/// Create a mock transport with a specific node ID
pub fn mock_transport_with_id(_node_id: String) -> MockTransportService {
    // Note: MockTransport doesn't currently support node_id parameter
    MockTransportService::new()
}

/// Create an app transport with default configuration
pub fn app_transport() -> TransportApp {
    TransportApp::with_defaults()
}

/// Create an app transport with custom configuration
pub fn app_transport_with_config(config: AppTransportConfig) -> TransportApp {
    TransportApp::new(config)
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transport_builder_mock() {
        let transport = TransportBuilder::new()
            .transport_type(TransportType::Mock)
            .node_id("test_node".to_string())
            .auto_start(true)
            .build_mock()
            .unwrap();

        // The transport should be created successfully
        assert!(true);
    }

    #[test]
    fn test_transport_builder_app() {
        let transport = TransportBuilder::new()
            .transport_type(TransportType::App)
            .node_id("test_node".to_string())
            .auto_start(true)
            .build_app()
            .unwrap();

        // The transport should be created successfully
        assert!(true);
    }

    #[test]
    fn test_convenience_functions() {
        let _mock1 = mock_transport();
        let _mock2 = mock_transport_with_id("test_id".to_string());
        let _app1 = app_transport();
        let _app2 = app_transport_with_config(AppTransportConfig::default());
    }
}
