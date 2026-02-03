//! Cell management and lifecycle for the DAW system.
//!
//! This module provides:
//! - `CellReadyRegistry` for tracking which cells have signaled ready
//! - `HostServiceImpl` that implements the cell-host-proto HostService

use std::sync::{Arc, OnceLock};
use std::time::Duration;

use cell_host_proto::{HostService, PollReadyResponse, ReadyAck, ReadyMsg, WaitPolicy};
use dashmap::DashMap;
use roam::session::Context;
use tracing::{debug, info};

// ============================================================================
// Cell Ready Registry
// ============================================================================

/// Registry for tracking cell readiness (RPC-ready state).
#[derive(Clone)]
pub struct CellReadyRegistry {
    ready: Arc<DashMap<String, ReadyMsg>>,
}

impl CellReadyRegistry {
    fn new() -> Self {
        Self {
            ready: Arc::new(DashMap::new()),
        }
    }

    /// Mark a cell as ready.
    pub fn mark_ready(&self, msg: ReadyMsg) {
        // Normalize: cells might report with underscores, we use hyphens
        let cell_name = msg.cell_name.replace('_', "-");
        debug!(cell = %cell_name, peer_id = msg.peer_id, "Cell marked ready");
        self.ready.insert(cell_name, msg);
    }

    /// Mark a cell as not ready (for respawning after death).
    pub fn mark_not_ready(&self, cell_name: &str) {
        let cell_name = cell_name.replace('_', "-");
        debug!(cell = %cell_name, "Cell marked not ready");
        self.ready.remove(&cell_name);
    }

    /// Check if a cell is ready.
    pub fn is_ready(&self, cell_name: &str) -> bool {
        self.ready.contains_key(cell_name)
    }

    /// Get the number of ready cells.
    #[allow(dead_code)]
    pub fn ready_count(&self) -> usize {
        self.ready.len()
    }
}

static CELL_READY_REGISTRY: OnceLock<CellReadyRegistry> = OnceLock::new();

/// Get the global cell ready registry.
pub fn cell_ready_registry() -> &'static CellReadyRegistry {
    CELL_READY_REGISTRY.get_or_init(CellReadyRegistry::new)
}

// ============================================================================
// Host Service Implementation
// ============================================================================

/// Host implementation of the HostService trait from cell-host-proto.
///
/// This handles the `ready()` call from cells to signal they're ready for RPC.
#[derive(Clone)]
pub struct HostServiceImpl {
    registry: CellReadyRegistry,
}

impl HostServiceImpl {
    /// Create a new HostServiceImpl with the given ready registry.
    pub fn new(registry: CellReadyRegistry) -> Self {
        Self { registry }
    }
}

impl HostService for HostServiceImpl {
    async fn ready(&self, _cx: &Context, msg: ReadyMsg) -> ReadyAck {
        info!(
            cell = %msg.cell_name,
            peer_id = msg.peer_id,
            pid = ?msg.pid,
            "Cell signaled ready"
        );
        self.registry.mark_ready(msg);
        ReadyAck
    }

    async fn poll_ready(
        &self,
        _cx: &Context,
        cell_name: String,
        policy: WaitPolicy,
    ) -> PollReadyResponse {
        debug!(
            cell = %cell_name,
            max_attempts = policy.max_attempts,
            initial_backoff_ms = policy.initial_backoff_ms,
            "poll_ready started"
        );

        for attempt in 0..policy.max_attempts {
            if self.registry.is_ready(&cell_name) {
                debug!(
                    cell = %cell_name,
                    attempt,
                    "poll_ready: cell is ready"
                );
                return PollReadyResponse { ready: true };
            }

            // Don't sleep after the last attempt
            if attempt + 1 < policy.max_attempts {
                let backoff_ms = policy.backoff_for_attempt(attempt);
                debug!(
                    cell = %cell_name,
                    attempt,
                    backoff_ms,
                    "poll_ready: waiting before next attempt"
                );
                tokio::time::sleep(Duration::from_millis(backoff_ms)).await;
            }
        }

        debug!(
            cell = %cell_name,
            max_attempts = policy.max_attempts,
            "poll_ready: exhausted all attempts"
        );
        PollReadyResponse { ready: false }
    }
}
