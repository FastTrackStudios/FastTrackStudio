//! Rig service context for dependency injection.
//!
//! Wraps `SignalControl` (the ergonomic facade over ROAM services) and
//! provides it to the Dioxus component tree via context.

use crate::prelude::*;
use signal_control::SignalControl;

/// Rig service wrapper — the entry point for all rig operations.
///
/// Components obtain this via `use_rig_service()` and call methods on
/// the inner `SignalControl` instance.
#[derive(Clone, PartialEq)]
pub struct RigService {
    ctl: SignalControl,
}

impl RigService {
    /// Create with a pre-built `SignalControl`.
    pub fn new(ctl: SignalControl) -> Self {
        Self { ctl }
    }

    /// Create with default guitar rig mock data.
    pub fn mock_guitar() -> Self {
        Self::new(SignalControl::mock_guitar())
    }

    /// Get the underlying `SignalControl` for direct method calls.
    pub fn ctl(&self) -> &SignalControl {
        &self.ctl
    }
}

/// Context wrapper for dependency injection.
#[derive(Clone)]
pub struct RigServiceCtx {
    pub service: RigService,
}

/// Hook to access the rig service from context.
///
/// # Panics
/// Panics if called outside of a `RigServiceProvider`.
pub fn use_rig_service() -> RigServiceCtx {
    use_context::<RigServiceCtx>()
}

/// Provider component that injects the rig service into context.
#[component]
pub fn RigServiceProvider(service: RigService, children: Element) -> Element {
    use_context_provider(move || RigServiceCtx {
        service: service.clone(),
    });
    children
}
