//! Timer Infrastructure
//!
//! Manages the REAPER timer callback registration and execution.

use reaper_medium::ReaperSession;
use tracing::{debug, info, error};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;

/// Timer callback state
static TICK_COUNT: AtomicU64 = AtomicU64::new(0);
static FIRST_CALL: OnceLock<std::sync::atomic::AtomicBool> = OnceLock::new();

/// Timer callback function type
pub type TimerCallback = extern "C" fn();

/// Register a timer callback with REAPER
pub fn register_timer(session: &mut ReaperSession, callback: TimerCallback) -> Result<(), Box<dyn std::error::Error>> {
    let timer_result = session.plugin_register_add_timer(callback);
    
    match timer_result {
        Ok(_) => {
            debug!("Registered polling timer callback with REAPER (target: 30Hz)");
            Ok(())
        }
        Err(e) => {
            error!("❌❌❌ FAILED to register polling timer callback: {}", e);
            error!("❌ Timer will NOT work! Error details: {:?}", e);
            Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Failed to register timer: {}", e),
            )))
        }
    }
}

/// Log first timer call (for debugging)
pub fn log_first_timer_call() {
    let first_call = FIRST_CALL.get_or_init(|| std::sync::atomic::AtomicBool::new(true));
    if first_call.swap(false, Ordering::Relaxed) {
        info!("⏰⏰⏰ Timer callback FIRST CALL - timer is working! ⏰⏰⏰");
    }
}

/// Increment tick count (for debugging)
pub fn increment_tick_count() {
    TICK_COUNT.fetch_add(1, Ordering::Relaxed);
}

