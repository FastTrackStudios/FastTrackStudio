//! DAW Test Extension
//!
//! Tests calling DAW services via SHM using the unified HostService pattern.
//! Following dodeca's architecture - all cross-extension RPC goes through the host.

use extension_runtime::{run_extension, NoDispatcher};
use host_proto::HostServiceClient;
use tracing::{error, info, warn};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run_extension!("daw-test", |handle_cell| {
        // Spawn test task that waits for handle to be ready
        let handle_cell_clone: std::sync::Arc<std::sync::OnceLock<extension_runtime::ConnectionHandle>> = handle_cell.clone();
        tokio::spawn(async move {
            // Wait for handle to be initialized
            while handle_cell_clone.get().is_none() {
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
            }

            let handle = handle_cell_clone.get().expect("Handle not initialized").clone();

            info!("🎯 DAW Test Extension started successfully!");
            info!("Testing cross-extension RPC calls via unified HostService...");

            // Create host service client (dodeca pattern: all extensions call HostService)
            let host = HostServiceClient::new(handle.clone());

            // Test 1: Call TransportService.play() on startup
            info!("📞 Calling HostService::play()...");
            match host.play().await {
                Ok(host_proto::TransportResult::Success) => {
                    info!("✅ HostService::play() succeeded!");
                }
                Ok(host_proto::TransportResult::Error { message }) => {
                    error!("❌ HostService::play() failed: {}", message);
                }
                Err(e) => {
                    error!("❌ RPC call failed: {:?}", e);
                }
            }

            // Test 2: Log every 2 seconds via HostService
            info!("Starting periodic logging test (every 2 seconds)...");
            let mut counter = 0;
            loop {
                tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

                counter += 1;

                // Log via tracing (goes through tracing aggregation)
                info!("✨ Heartbeat #{} (via tracing)", counter);

                // Log via HostService (tests cross-extension RPC to daw-reaper)
                let message = format!("Heartbeat #{} (via HostService RPC)", counter);
                match host.log(message.clone()).await {
                    Ok(host_proto::LoggerResult::Success) => {
                        info!("✅ HostService::log() succeeded");
                    }
                    Ok(host_proto::LoggerResult::Error { message: err }) => {
                        warn!("⚠️  HostService::log() failed: {}", err);
                    }
                    Err(e) => {
                        warn!("⚠️  RPC call failed: {:?}", e);
                    }
                }
            }
        });

        // Return empty dispatcher (this extension only calls services, doesn't provide any)
        NoDispatcher
    })
}
