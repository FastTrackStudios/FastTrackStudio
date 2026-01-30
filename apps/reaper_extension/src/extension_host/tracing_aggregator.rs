//! Tracing Aggregator - Collects tracing events from all SHM guest extensions
//!
//! Uses roam-tracing to aggregate tracing events from extensions and forward
//! them to the host's tracing subscriber.

use roam_tracing::{dispatch_record, HostTracingDispatcher, HostTracingService, HostTracingState};
use roam_session::{ConnectionHandle, ServiceDispatcher};
use std::sync::Arc;
use parking_lot::Mutex;
use tokio::task::JoinHandle;
use tracing::{info, debug, warn};

/// Tracing aggregator for all extensions
pub struct TracingAggregator {
    /// Shared tracing state for all extensions
    state: Arc<HostTracingState>,

    /// Background task that consumes tracing records
    consumer_task: Mutex<Option<JoinHandle<()>>>,
}

impl TracingAggregator {
    /// Create a new tracing aggregator
    pub fn new(buffer_size: usize) -> Self {
        info!("Creating tracing aggregator with buffer_size: {}", buffer_size);

        // HostTracingState::new already returns Arc<Self>
        let state = HostTracingState::new(buffer_size);

        Self {
            state,
            consumer_task: Mutex::new(None),
        }
    }

    /// Start the consumer task that logs tracing events from all extensions
    pub fn start_consumer(&self, runtime_handle: &tokio::runtime::Handle) {
        // Take the receiver (only works once)
        let mut receiver = match self.state.take_receiver() {
            Some(rx) => rx,
            None => {
                warn!("Tracing receiver already taken, skipping consumer task");
                return;
            }
        };

        info!("Starting tracing consumer task");

        let task = runtime_handle.spawn(async move {
            debug!("Tracing consumer task started");

            while let Some(tagged_record) = receiver.recv().await {
                // Use roam-tracing's dispatch_record to properly forward the record
                // to the host's tracing subscriber with all metadata preserved
                dispatch_record(&tagged_record);
            }

            debug!("Tracing consumer task exited");
        });

        *self.consumer_task.lock() = Some(task);
    }

    /// Create a tracing dispatcher for a specific peer/extension
    ///
    /// This should be called when registering a new peer and combined with
    /// other host services using RoutedDispatcher.
    pub fn dispatcher_for_peer(&self, peer_id: u8, extension_name: Option<String>) -> HostTracingDispatcher<HostTracingService> {
        debug!("Creating tracing dispatcher for peer {} (name: {:?})", peer_id, extension_name);

        let service = self.state.service_for_peer(peer_id.into(), extension_name);
        HostTracingDispatcher::new(service)
    }
}

impl Drop for TracingAggregator {
    fn drop(&mut self) {
        if let Some(task) = self.consumer_task.lock().take() {
            task.abort();
            info!("Tracing consumer task aborted");
        }
    }
}
