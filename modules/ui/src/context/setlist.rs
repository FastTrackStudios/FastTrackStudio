//! Setlist service context for dependency injection
//!
//! This module provides a Dioxus context for injecting setlist service implementations.
//! Components can use `use_setlist_service()` to access the service without knowing
//! which implementation (Mock, Reaper, etc.) is being used.
//!
//! The `LocalSetlistClient` wrapper allows direct in-process calls to any service
//! implementation via ROAM's async interface, without needing network transport.

use dioxus::prelude::*;
use fts::setlist::{LocalSetlistClient, MockSetlist};

/// Context holding the setlist service client
///
/// Uses `LocalSetlistClient` which wraps any `SetlistService` implementation
/// and provides direct async method calls via ROAM's interface.
#[derive(Clone)]
pub struct SetlistServiceCtx {
    /// The local client wrapping the service implementation
    pub client: LocalSetlistClient<MockSetlist>,
}

/// Hook to access the setlist service from context
///
/// # Panics
/// Panics if called outside of a `SetlistServiceProvider`
pub fn use_setlist_service() -> SetlistServiceCtx {
    use_context::<SetlistServiceCtx>()
}

/// Provider component that injects the setlist service into context
///
/// Wrap your component tree with this provider to make the service available
/// to all child components via `use_setlist_service()`.
///
/// # Example
/// ```ignore
/// use std::sync::Arc;
/// use fts::setlist::{LocalSetlistClient, MockSetlist};
///
/// let service = Arc::new(MockSetlist::with_sample_data());
/// let client = LocalSetlistClient::new(service);
///
/// rsx! {
///     SetlistServiceProvider { client,
///         PerformanceView {}
///     }
/// }
/// ```
#[component]
pub fn SetlistServiceProvider(client: LocalSetlistClient<MockSetlist>, children: Element) -> Element {
    use_context_provider(|| SetlistServiceCtx {
        client: client.clone(),
    });
    rsx! { {children} }
}
