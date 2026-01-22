//! Timer Infrastructure
//!
//! Manages the REAPER timer callback registration and execution.

use reaper_high::Reaper;
use reaper_low::Swell;
use reaper_medium::ReaperSession;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use tracing::{debug, error, info};

/// Timer callback state
static TICK_COUNT: AtomicU64 = AtomicU64::new(0);
static FIRST_CALL: OnceLock<AtomicBool> = OnceLock::new();
static TIMER_UPGRADED: AtomicBool = AtomicBool::new(false);

/// REAPER's misc timer ID (used internally by REAPER for UI updates)
const MISC_TIMER_ID: usize = 666;

/// Target timer rate in milliseconds (60 FPS = ~16.67ms)
const TIMER_RATE_60FPS: u32 = 1000 / 60; // ~16ms

/// Timer callback function type
pub type TimerCallback = extern "C" fn();

/// Register a timer callback with REAPER
pub fn register_timer(
    session: &mut ReaperSession,
    callback: TimerCallback,
) -> Result<(), Box<dyn std::error::Error>> {
    let timer_result = session.plugin_register_add_timer(callback);

    match timer_result {
        Ok(_) => {
            debug!("Registered polling timer callback with REAPER (target: 30Hz)");
            Ok(())
        }
        Err(e) => {
            error!("❌❌❌ FAILED to register polling timer callback: {}", e);
            error!("❌ Timer will NOT work! Error details: {:?}", e);
            Err(Box::new(std::io::Error::other(format!(
                "Failed to register timer: {}",
                e
            ))))
        }
    }
}

/// Upgrade REAPER's misc timer to 60 FPS
///
/// This modifies REAPER's internal timer (ID 666) from the default ~30Hz to ~60Hz.
/// Should be called once after the extension is fully initialized.
///
/// Based on reaper_60fps by cfillion:
/// https://github.com/cfillion/reaper_60fps
pub fn upgrade_to_60fps() {
    // Only upgrade once
    if TIMER_UPGRADED.swap(true, Ordering::SeqCst) {
        debug!("Timer already upgraded to 60fps");
        return;
    }

    let reaper = Reaper::get();
    let main_hwnd = reaper.main_window().as_ptr();
    let swell = Swell::get();

    unsafe {
        // SetTimer with the same timer ID replaces the existing timer
        // This changes REAPER's misc timer from ~33ms (30Hz) to ~16ms (60Hz)
        let result = swell.SetTimer(
            main_hwnd,
            MISC_TIMER_ID,
            TIMER_RATE_60FPS,
            None, // Use default window procedure
        );

        if result != 0 {
            info!(
                "⚡ Upgraded REAPER timer to 60 FPS ({}ms interval)",
                TIMER_RATE_60FPS
            );
        } else {
            error!("Failed to upgrade timer to 60fps");
        }
    }
}

/// Log first timer call (for debugging)
pub fn log_first_timer_call() {
    let first_call = FIRST_CALL.get_or_init(|| AtomicBool::new(true));
    if first_call.swap(false, Ordering::Relaxed) {
        info!("⏰⏰⏰ Timer callback FIRST CALL - timer is working! ⏰⏰⏰");
    }
}

/// Increment tick count (for debugging)
pub fn increment_tick_count() {
    TICK_COUNT.fetch_add(1, Ordering::Relaxed);
}
