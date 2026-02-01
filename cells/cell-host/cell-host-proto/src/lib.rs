//! Host service protocol for cells.
//!
//! This crate defines the `HostService` trait that cells use to communicate
//! with the host process.

use facet::Facet;

/// Message sent by a cell to signal it's ready for RPC requests.
#[derive(Debug, Clone, Facet)]
pub struct ReadyMsg {
    /// The peer ID assigned to this cell.
    pub peer_id: u16,
    /// The name of this cell.
    pub cell_name: String,
    /// The process ID of this cell (for debugging).
    pub pid: Option<u32>,
}

/// Acknowledgment sent by the host after receiving a ready message.
#[derive(Debug, Clone, Facet)]
pub struct ReadyAck;

/// Response for polling cell readiness.
///
/// Follows the `tower::Service::poll_ready` pattern.
#[derive(Debug, Clone, Facet)]
pub struct PollReadyResponse {
    /// Whether the cell is ready to receive requests.
    pub ready: bool,
}

/// Policy for waiting on cell readiness with exponential backoff.
///
/// Based on roam's `RetryPolicy` pattern. This configures how `poll_ready`
/// waits for a cell to become available.
///
/// # Example
///
/// ```ignore
/// let policy = WaitPolicy::default();
/// let response = host.poll_ready("my-cell".to_string(), policy).await?;
/// ```
#[derive(Debug, Clone, Facet)]
pub struct WaitPolicy {
    /// Maximum number of poll attempts before giving up.
    pub max_attempts: u32,
    /// Initial delay between poll attempts.
    pub initial_backoff_ms: u64,
    /// Maximum delay between poll attempts.
    pub max_backoff_ms: u64,
    /// Backoff multiplier (e.g., 2.0 for exponential backoff).
    pub backoff_multiplier: f64,
}

impl Default for WaitPolicy {
    fn default() -> Self {
        Self {
            max_attempts: 50,        // ~5s with default backoff
            initial_backoff_ms: 50,  // Start with 50ms
            max_backoff_ms: 500,     // Cap at 500ms between attempts
            backoff_multiplier: 1.5, // Gentler than 2x for polling
        }
    }
}

impl WaitPolicy {
    /// Create a policy for immediate (non-blocking) check.
    pub fn immediate() -> Self {
        Self {
            max_attempts: 1,
            initial_backoff_ms: 0,
            max_backoff_ms: 0,
            backoff_multiplier: 1.0,
        }
    }

    /// Create a policy with a fixed delay between attempts.
    pub fn fixed(attempts: u32, delay_ms: u64) -> Self {
        Self {
            max_attempts: attempts,
            initial_backoff_ms: delay_ms,
            max_backoff_ms: delay_ms,
            backoff_multiplier: 1.0,
        }
    }

    /// Calculate the backoff duration in milliseconds for a given attempt number.
    pub fn backoff_for_attempt(&self, attempt: u32) -> u64 {
        let multiplier = self
            .backoff_multiplier
            .powi(attempt.saturating_sub(1) as i32);
        let backoff = (self.initial_backoff_ms as f64 * multiplier) as u64;
        backoff.min(self.max_backoff_ms)
    }
}

/// Host service that all cells can call.
///
/// This provides the cell lifecycle protocol - cells call `ready()` after
/// starting their driver to signal they're ready for RPC requests.
///
/// The `poll_ready` method follows the `tower::Service` pattern for checking
/// if a dependent cell is available.
#[allow(async_fn_in_trait)]
#[roam::service]
pub trait HostService {
    /// Cell calls this after starting its driver to signal it's ready for RPC requests.
    async fn ready(&self, msg: ReadyMsg) -> ReadyAck;

    /// Poll whether another cell is ready to receive requests.
    ///
    /// This follows the `tower::Service::poll_ready` pattern. Cells should call
    /// this before attempting to communicate with another cell.
    ///
    /// The `policy` configures the wait behavior with exponential backoff.
    /// Use `WaitPolicy::immediate()` for non-blocking checks.
    async fn poll_ready(&self, cell_name: String, policy: WaitPolicy) -> PollReadyResponse;
}
