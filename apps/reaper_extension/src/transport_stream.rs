//! REAPER Transport Stream Implementation
//!
//! Implements the transport stream backend for REAPER extension.
//! Uses the protocol definition from the transport crate.

use anyhow::Result;
use iroh::{protocol::ProtocolHandler, Endpoint};
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::info;
use transport::{
    Transport, TransportStreamApi, TransportStateProvider,
};

use crate::reaper_transport::ReaperTransport;
use std::sync::atomic::{AtomicBool, Ordering};

/// Shared state for transport updates
/// Updated from REAPER's main thread, read from async tasks
static LATEST_TRANSPORT: std::sync::OnceLock<Arc<Mutex<Option<Transport>>>> = std::sync::OnceLock::new();
static TRANSPORT_UPDATED: AtomicBool = AtomicBool::new(false);

/// Initialize the transport state storage
pub fn init_transport_state() -> Arc<Mutex<Option<Transport>>> {
    let state = Arc::new(Mutex::new(None));
    LATEST_TRANSPORT.set(state.clone()).expect("Transport state already initialized");
    state
}

/// Update transport state from REAPER's main thread
/// This should be called from REAPER's timer callback at 30Hz
pub fn update_transport_state() {
    let Some(state) = LATEST_TRANSPORT.get() else {
        // Log only once to avoid spam
        use std::sync::atomic::{AtomicBool, Ordering};
        static LOGGED: AtomicBool = AtomicBool::new(false);
        if !LOGGED.swap(true, Ordering::Relaxed) {
            tracing::error!("[REAPER Transport] Transport state not initialized! Call init_transport_state() first.");
        }
        return;
    };
    
    let reaper = reaper_high::Reaper::get();
    let project = reaper.current_project();
    let transport_adapter = ReaperTransport::new(project);
    
    match transport_adapter.read_transport() {
        Ok(transport) => {
            // Update the state (we'll use a blocking lock since we're on main thread)
            if let Ok(mut guard) = state.try_lock() {
                *guard = Some(transport.clone());
                TRANSPORT_UPDATED.store(true, Ordering::Release);
                
                // Log transport update occasionally (every 30 ticks = once per second at 30Hz)
                // Use a static counter to track ticks
                use std::sync::atomic::{AtomicU64, Ordering};
                static LOG_TICK: AtomicU64 = AtomicU64::new(0);
                let tick = LOG_TICK.fetch_add(1, Ordering::Relaxed);
                
                    // Log once per second (every 30 ticks at 30Hz)
                    if tick % 30 == 0 {
                        let position_seconds = transport.playhead_position.time.to_seconds();
                        let tempo_bpm = transport.tempo.bpm;
                        tracing::info!(
                            "[REAPER Transport] Update (tick {}): {:?} | Position: {:.2}s | Tempo: {:.1} BPM | Rate: {:.2}x",
                            tick,
                            transport.play_state,
                            position_seconds,
                            tempo_bpm,
                            transport.playrate
                        );
                    }
            } else {
                // Lock is held by async task - this is fine, just skip this update
                // We'll try again on the next timer tick
            }
        }
        Err(e) => {
            // Log errors occasionally to avoid spam
            use std::sync::atomic::{AtomicU64, Ordering};
            static ERROR_TICK: AtomicU64 = AtomicU64::new(0);
            let tick = ERROR_TICK.fetch_add(1, Ordering::Relaxed);
            if tick % 300 == 0 {
                tracing::warn!("[REAPER Transport] Failed to read transport (tick {}): {}", tick, e);
            }
        }
    }
}

/// REAPER implementation of TransportStateProvider
struct ReaperTransportStateProvider;

#[async_trait::async_trait]
impl TransportStateProvider for ReaperTransportStateProvider {
    async fn get_transport(&self) -> Result<Transport, String> {
        let Some(state) = LATEST_TRANSPORT.get() else {
            return Err("Transport state not initialized".to_string());
        };
        
        // Get the latest transport state (don't wait, just read what's available)
        let guard = state.lock().await;
        guard.clone().ok_or_else(|| {
            // If no transport state is available yet, return an error
            // The polling loop will retry on the next tick
            "No transport state available yet".to_string()
        })
    }
}

/// Create and expose the REAPER transport stream service
/// 
/// This creates a transport stream service with REAPER's specific
/// TransportStateProvider implementation. Only the server (REAPER extension)
/// needs to know about this implementation - clients just connect via
/// TransportStreamApi::connect().
pub fn create_reaper_transport_stream_service() -> Result<TransportStreamApi> {
    let state_provider = Arc::new(ReaperTransportStateProvider);
    let api = TransportStreamApi::spawn(state_provider);
    Ok(api)
}

