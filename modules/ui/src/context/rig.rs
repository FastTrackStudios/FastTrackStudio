//! Rig service context for dependency injection
//!
//! This module provides a Dioxus context for injecting rig service implementations.
//! Components can use `use_rig_service()` to access the service without knowing
//! which implementation (Mock, Reaper, etc.) is being used.
//!
//! The `LocalRigClient` wrapper allows direct in-process calls to any service
//! implementation via ROAM's async interface, without needing network transport.

use dioxus::prelude::*;
use fts::rig::{LocalRigClient, MockRig};

/// Context holding the rig service client
///
/// Uses `LocalRigClient` which wraps any `RigService` implementation
/// and provides direct async method calls via ROAM's interface.
#[derive(Clone)]
pub struct RigServiceCtx {
    /// The local client wrapping the service implementation
    pub client: LocalRigClient<MockRig>,
}

/// Hook to access the rig service from context
///
/// # Panics
/// Panics if called outside of a `RigServiceProvider`
pub fn use_rig_service() -> RigServiceCtx {
    use_context::<RigServiceCtx>()
}

/// Provider component that injects the rig service into context
///
/// Wrap your component tree with this provider to make the service available
/// to all child components via `use_rig_service()`.
///
/// # Example
/// ```ignore
/// use std::sync::Arc;
/// use fts::rig::{LocalRigClient, MockRig};
///
/// let service = Arc::new(MockRig::with_sample_guitar_data());
/// let client = LocalRigClient::new(service);
///
/// rsx! {
///     RigServiceProvider { client,
///         RigControlView {}
///     }
/// }
/// ```
#[component]
pub fn RigServiceProvider(client: LocalRigClient<MockRig>, children: Element) -> Element {
    use_context_provider(|| RigServiceCtx {
        client: client.clone(),
    });
    rsx! { {children} }
}
