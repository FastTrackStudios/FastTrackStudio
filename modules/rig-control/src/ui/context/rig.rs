//! Rig service context for dependency injection
//!
//! Following ROAM patterns from roam_test.rs:
//! - For local/in-process: Just call service methods directly with roam::Context
//! - For remote/networked: Use ReconnectingClient (future)
//! - To swap implementations: Use generics or concrete types

use std::sync::Arc;
use dioxus::prelude::*;

use crate::MockRigControlService;

/// Rig service wrapper for local/in-process use
/// In the future, this could be extended to support remote connections via ReconnectingClient
#[derive(Clone)]
pub struct RigService {
    /// The concrete service implementation
    /// For local use, we just hold the service and call it directly
    service: Arc<MockRigControlService>,
}

impl RigService {
    /// Create with a mock service (for development/testing)
    pub fn mock(service: MockRigControlService) -> Self {
        Self {
            service: Arc::new(service),
        }
    }

    /// Create with default guitar rig
    pub fn mock_guitar() -> Self {
        Self::mock(MockRigControlService::with_guitar_defaults())
    }

    /// Get reference to the underlying service for direct method calls
    ///
    /// Hooks use this to call service methods directly with roam::Context
    pub(crate) fn inner(&self) -> &Arc<MockRigControlService> {
        &self.service
    }

    // Future: Add variant for remote service
    // pub fn remote(client: Arc<ReconnectingClient<DaemonConnector>>) -> Self
}

/// Rig service context for dependency injection
#[derive(Clone)]
pub struct RigServiceCtx {
    pub service: RigService,
}

/// Hook to access the rig service from context
///
/// # Panics
/// Panics if called outside of a `RigServiceProvider`
pub fn use_rig_service() -> RigServiceCtx {
    use_context::<RigServiceCtx>()
}

/// Provider component that injects the rig service into context
#[component]
pub fn RigServiceProvider(
    service: RigService,
    children: Element
) -> Element {
    use_context_provider(move || RigServiceCtx {
        service: service.clone(),
    });
    children
}

/// Helper to create a rig service context from a mock service (for testing/demos)
pub fn mock_rig_service_ctx() -> RigServiceCtx {
    RigServiceCtx {
        service: RigService::mock_guitar(),
    }
}
