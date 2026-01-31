//! DAW Test Extension
//!
//! Tests calling DAW services directly via the DAW host.
//! Extensions call daw.Transport.play() which is handled by the REAPER extension.

use extension_runtime::{run_extension, ConnectionHandle, NoDispatcher};
use host_proto::{TransportClient, TransportResult};
use std::sync::Arc;
use std::sync::OnceLock;
use tracing::{error, info, warn};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run_extension!("daw-test", |handle_cell| {
        // Spawn test task that waits for handle to be ready
        let handle_cell_clone: Arc<OnceLock<ConnectionHandle>> = handle_cell.clone();
        tokio::spawn(async move {
            // Wait for handle to be initialized
            while handle_cell_clone.get().is_none() {
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
            }

            let daw_handle = handle_cell_clone.get().expect("Handle not initialized").clone();

            info!("🎯 DAW Test Extension started successfully!");
            info!("Connecting to DAW services...");

            // Create DAW service client
            // The DAW host (REAPER extension) provides Transport, Tracks, etc.
            let transport = TransportClient::new(daw_handle);

            // Test 1: Call daw.Transport.play()
            info!("📞 Calling daw.Transport.play()...");
            match transport.play().await {
                Ok(TransportResult::Success) => {
                    info!("✅ daw.Transport.play() succeeded!");
                }
                Ok(TransportResult::Error { message }) => {
                    error!("❌ daw.Transport.play() failed: {}", message);
                }
                Err(e) => {
                    error!("❌ RPC call failed: {:?}", e);
                }
            }

            // Test 2: Get transport state
            info!("📞 Calling daw.Transport.get_state()...");
            match transport.get_state().await {
                Ok(state) => {
                    info!("✅ Transport state: playing={}, recording={}, pos={:.2}s, tempo={:.1} BPM",
                        state.is_playing,
                        state.is_recording,
                        state.position_seconds,
                        state.tempo_bpm
                    );
                }
                Err(e) => {
                    error!("❌ Failed to get transport state: {:?}", e);
                }
            }

            // Test 3: Heartbeat every 2 seconds
            info!("Starting heartbeat test (every 2 seconds)...");
            let mut counter = 0;
            loop {
                tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

                counter += 1;

                // Log via tracing (goes through tracing aggregation)
                info!("✨ Heartbeat #{} (via tracing)", counter);

                // Get transport state to verify connection
                match transport.get_state().await {
                    Ok(state) => {
                        info!("✅ Heartbeat #{} - DAW active: pos={:.2}s", counter, state.position_seconds);
                    }
                    Err(e) => {
                        warn!("⚠️  Heartbeat #{} - RPC call failed: {:?}", counter, e);
                    }
                }
            }
        });

        // Return empty dispatcher (this extension only calls services, doesn't provide any)
        NoDispatcher
    })
}
