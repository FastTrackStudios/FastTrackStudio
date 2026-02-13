//! Rig service context for dependency injection.
//!
//! Wraps `SignalControl` (the ergonomic facade over ROAM services) and
//! provides it to the Dioxus component tree via context.
//!
//! ## Service Modes
//!
//! The [`RigServiceMode`] enum tracks the connection state of the rig backend:
//! - `Mock` -- running against in-memory mock data (no real hardware)
//! - `Real` -- connected to a live rig control backend (placeholder for future)
//! - `Disconnected` -- no backend available (e.g. service lost)
//!
//! The current mode is stored in the [`RIG_SERVICE_MODE`](crate::signals::RIG_SERVICE_MODE)
//! global signal and updated by [`RigServiceProvider`] and the service discovery
//! logic in [`detect_service_mode`].

use crate::prelude::*;
use signal_control::SignalControl;

// ─────────────────────────────────────────────────────────────────────────────
// Service Mode
// ─────────────────────────────────────────────────────────────────────────────

/// Describes which rig control backend is currently active.
///
/// Written to [`RIG_SERVICE_MODE`](crate::signals::RIG_SERVICE_MODE) so any
/// component can read the current connection state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RigServiceMode {
    /// Running against in-memory mock data -- no real hardware attached.
    Mock,
    /// Connected to a live rig control backend.
    ///
    /// This variant is a placeholder; once a real `RigControlService`
    /// implementation exists, it will carry the connection handle.
    Real,
    /// Backend unavailable -- service was lost or never discovered.
    Disconnected,
}

impl RigServiceMode {
    /// Human-readable label for the current mode.
    pub const fn label(self) -> &'static str {
        match self {
            Self::Mock => "Mock",
            Self::Real => "Connected",
            Self::Disconnected => "Disconnected",
        }
    }

    /// CSS color class suffix used by the status indicator dot.
    pub const fn dot_color(self) -> &'static str {
        match self {
            Self::Real => "bg-green-500",
            Self::Mock => "bg-yellow-500",
            Self::Disconnected => "bg-red-500",
        }
    }

    /// Whether the service is usable (Mock or Real).
    pub const fn is_available(self) -> bool {
        matches!(self, Self::Mock | Self::Real)
    }
}

impl std::fmt::Display for RigServiceMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.label())
    }
}

/// Probe the environment for a real rig control backend.
///
/// Currently always returns [`RigServiceMode::Mock`] since no real backend
/// exists yet. When a hardware/network backend is added, this function will
/// attempt discovery and return `Real` on success.
///
/// This is intentionally synchronous today; a future version may be `async`
/// to perform network probes.
pub fn detect_service_mode() -> RigServiceMode {
    // TODO(US-006): When a real RigControlService exists, probe for it here.
    //   e.g. check for a running daemon socket, USB device, etc.
    tracing::debug!("detect_service_mode: no real backend available, using Mock");
    RigServiceMode::Mock
}

// ─────────────────────────────────────────────────────────────────────────────
// Rig Service
// ─────────────────────────────────────────────────────────────────────────────

/// Rig service wrapper -- the entry point for all rig operations.
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
///
/// On mount, detects the service mode via [`detect_service_mode`] and writes
/// it to the [`RIG_SERVICE_MODE`](crate::signals::RIG_SERVICE_MODE) global
/// signal. Also spawns a background task that periodically re-checks for
/// service availability changes (auto-reconnect).
#[component]
pub fn RigServiceProvider(service: RigService, children: Element) -> Element {
    use crate::signals::RIG_SERVICE_MODE;

    // Provide the service to the component tree.
    use_context_provider(move || RigServiceCtx {
        service: service.clone(),
    });

    // On first render, detect mode and start monitoring.
    use_effect(move || {
        let mode = detect_service_mode();
        tracing::info!("RigServiceProvider: initial service mode = {mode}");
        *RIG_SERVICE_MODE.write() = mode;

        // Spawn a background task that periodically re-probes for service
        // availability. If the mode changes, update the signal and log it.
        spawn(async move {
            let mut current = mode;
            loop {
                tokio::time::sleep(std::time::Duration::from_secs(5)).await;
                let probed = detect_service_mode();
                if probed != current {
                    tracing::info!(
                        "RigServiceProvider: service mode changed {current} -> {probed}"
                    );
                    *RIG_SERVICE_MODE.write() = probed;
                    current = probed;
                }
            }
        });
    });

    children
}
