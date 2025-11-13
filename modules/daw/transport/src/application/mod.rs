//! Transport Application Layer
//!
//! This module contains concrete implementations of the TransportActions trait
//! for different types of applications and devices. It represents Layer 4
//! (Application Layer) from the Universal Network Architecture.
//!
//! Each implementation maps domain requests to actual device-specific functionality:
//! - MockTransport: Testing and development
//! - StandaloneTransport: Self-contained applications
//! - ReaperTransport: REAPER DAW integration (future)
//! - ProToolsTransport: Avid Pro Tools integration (future)
//! - WebTransport: Browser-based DAW integration (future)
//!
//! The application layer is where the rubber meets the road - these implementations
//! actually control real DAWs, hardware devices, or provide mock functionality
//! for testing. They are completely isolated from protocol concerns and work
//! with any protocol adapter in the system.

pub mod mock;

// Future implementations
// pub mod standalone;
// pub mod reaper;
// pub mod protools;
// pub mod web;

// Re-export implementations
pub use mock::MockTransport;

// Re-export for convenience
pub use crate::transport::core::actions::TransportActions;

/// Trait alias for transport implementations that can be used across the system
pub trait ApplicationTransport: TransportActions + Send + Sync + 'static {}

// Implement the trait alias for all concrete types
impl ApplicationTransport for MockTransport {}

/// Helper function to create a transport implementation (removed boxed version due to async trait incompatibility)
// Note: Async traits are not dyn-compatible, so we work with concrete types

// ============================================================================
// BUILDER PATTERNS FOR COMMON SETUPS
// ============================================================================

/// Builder for creating transport implementations with common configurations
pub struct TransportBuilder {
    transport_type: TransportType,
    config: TransportConfig,
}

/// Types of transport implementations available
#[derive(Debug, Clone)]
pub enum TransportType {
    Mock,
    // Standalone,
    // Reaper { path: String },
    // ProTools { session: String },
    // Web { endpoint: String },
}

/// Configuration options for transport implementations
#[derive(Debug, Clone, Default)]
pub struct TransportConfig {
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
            config: TransportConfig::default(),
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

    /// Build the transport implementation
    pub fn build(self) -> Result<MockTransport, String> {
        match self.transport_type {
            TransportType::Mock => {
                let transport = MockTransport::new();
                Ok(transport)
            },
            // Future implementations would be handled here
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
pub fn mock_transport() -> MockTransport {
    MockTransport::new()
}

/// Create a mock transport with a specific node ID
pub fn mock_transport_with_id(_node_id: String) -> MockTransport {
    // Note: MockTransport doesn't currently support node_id parameter
    MockTransport::new()
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transport_builder() {
        let transport = TransportBuilder::new()
            .transport_type(TransportType::Mock)
            .node_id("test_node".to_string())
            .auto_start(true)
            .build()
            .unwrap();

        // The transport should be created successfully
        assert!(true);
    }

    #[test]
    fn test_convenience_functions() {
        let _mock1 = mock_transport();
        let _mock2 = mock_transport_with_id("test_id".to_string());
    }
}
