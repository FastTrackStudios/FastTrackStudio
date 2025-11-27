mod actions;
mod implementation;

mod core;
mod services;
mod infrastructure;
mod app;

mod live;
mod lyrics;

/// Polling state management for continuous updates
pub mod polling_state {
    use std::sync::atomic::{AtomicBool, Ordering};
    static POLLING_ENABLED: AtomicBool = AtomicBool::new(true);
    
    pub fn is_enabled() -> bool {
        POLLING_ENABLED.load(Ordering::Relaxed)
    }
    
    pub fn set_enabled(enabled: bool) {
        POLLING_ENABLED.store(enabled, Ordering::Relaxed);
    }
    
    pub fn toggle() -> bool {
        let new_state = !POLLING_ENABLED.load(Ordering::Relaxed);
        POLLING_ENABLED.store(new_state, Ordering::Relaxed);
        new_state
    }
}

use reaper_macros::reaper_extension_plugin;
use std::error::Error;
use reaper_low::{PluginContext, Swell};
use reaper_medium::ReaperSession;
use reaper_high::Reaper as HighReaper;
use tracing::{debug, error, info, warn};
use crate::infrastructure::action_registry::MidiEditorActionHook;
use crate::app::App;
use fragile::Fragile;
use std::sync::OnceLock;

/// Global App instance (wrapped in Fragile to ensure main-thread-only access)
static APP_INSTANCE: OnceLock<Fragile<App>> = OnceLock::new();

/// Get the global App instance (for timer callback)
/// Fragile ensures this can only be called from the main thread
fn get_app() -> Option<&'static Fragile<App>> {
    APP_INSTANCE.get()
}

/// REAPER will call this extension entry-point function once when it's starting.
#[reaper_extension_plugin]
fn plugin_main(context: PluginContext) -> Result<(), Box<dyn Error>> {
    // Initialize tracing FIRST, before anything else
    crate::infrastructure::tracing_config::init_tracing();
    info!("FastTrackStudio REAPER Extension starting...");
    
    // Make Swell available globally (required for swell-ui menu operations)
    // This must be done before registering menus, as swell-ui functions call Swell::get()
    let _ = Swell::make_available_globally(Swell::load(context));
    
    // Initialize REAPER high-level API (required for action registration)
    match HighReaper::load(context).setup() {
        Ok(_) => {
            info!("REAPER high-level API initialized");
        }
        Err(_) => {
            // API already initialized (e.g., by another extension)
            debug!("REAPER high-level API already initialized");
        }
    }
    
    // Create a medium-level API session (keep it alive for timer registration)
    let mut session = ReaperSession::load(context);
    
    // Register hookcommand2 for MIDI editor actions (must be done before action registration)
    if let Err(e) = session.plugin_register_add_hook_command_2::<MidiEditorActionHook>() {
        warn!(error = %e, "Failed to register hookcommand2 for MIDI editor actions");
    } else {
        info!("Registered hookcommand2 for MIDI editor actions");
    }
    
    let reaper = session.reaper();
    
    // Print message to the ReaScript console
    reaper.show_console_msg("FastTrackStudio REAPER Extension loaded!\n");
    
    // Create and initialize App
    debug!("Creating application container...");
    let app = App::new(session)?;
    
    // Initialize the application (register actions, menu, IROH server, etc.)
    app.initialize()?;
    
    // Store app globally wrapped in Fragile (ensures main-thread-only access)
    // Fragile will panic if accessed from wrong thread, which is safe
    APP_INSTANCE.set(Fragile::new(app))
        .expect("App already initialized");
    
    // Register REAPER timer callback for 60Hz polling (~16.67ms intervals)
    // This runs on REAPER's main thread automatically, so we can safely call REAPER APIs
    extern "C" fn polling_timer_callback() {
        use crate::infrastructure::timer::{log_first_timer_call, increment_tick_count};
        use std::sync::atomic::{AtomicU64, Ordering};
        use std::sync::OnceLock;
        
        // Log first call and increment tick count
        log_first_timer_call();
        increment_tick_count();
        
        // Periodic logging to verify timer is running (every 1000 ticks = ~16 seconds at 60Hz)
        static LOG_COUNTER: AtomicU64 = AtomicU64::new(0);
        let count = LOG_COUNTER.fetch_add(1, Ordering::Relaxed);
        if count % 1000 == 0 && count > 0 {
            debug!("⏰ Timer running: {} ticks processed", count);
        }
        
        // Get app instance (Fragile ensures this is main thread)
        if let Some(app_fragile) = get_app() {
            let app = app_fragile.get();
            
            // Update setlist state - ALWAYS runs every timer tick
            // This reads transport info and updates active song/section indices
            // The setlist is rebuilt with fresh transport info for each song
            // Now it will also update reactive polling with active indices
            let setlist_was_ready = app.setlist_service.get_setlist().is_some();
            match app.setlist_service.update_setlist_with_polling(app.reactive_polling.clone()) {
                Ok(_) => {
                    // Check if this is the first time setlist became ready
                    let setlist_is_now_ready = app.setlist_service.get_setlist().is_some();
                    if !setlist_was_ready && setlist_is_now_ready {
                        // First project is loaded - initialize deferred reactive loggers
                        // This avoids RefCell borrow panics during initial setup
                        app.change_detection.init_deferred_logger();
                        crate::infrastructure::reactive_logger::init_reactive_polling_logger(
                            app.reactive_polling.streams()
                        );
                        info!("✅ All reactive loggers initialized (after first project loaded)");
                    }
                    
                    // Success - only log periodically to avoid spam
                    if count % 1000 == 0 {
                        debug!("✅ Setlist state updated successfully (tick {})", count);
                    }
                }
                Err(e) => {
                    warn!(error = %e, "Failed to update setlist state");
                }
            }
            
            // Poll cursor positions and transport - emits reactive events only when values change
            // Do this AFTER setlist update to avoid nested borrows
            // Only poll after setlist is ready to avoid RefCell borrow issues
            if app.setlist_service.get_setlist().is_some() {
                app.reactive_polling.poll();
            }
            
            // Process pending seek requests from async tasks
            app.seek_service.process_pending_seeks();
            
            // Process pending command execution requests from async tasks
            app.command_service.process_pending_commands();
            
            // Process smooth seek queue (check if we should execute queued seeks)
            app.smooth_seek_service.process_smooth_seek_queue();
        } else {
            warn!("⚠️ App instance not available in timer callback");
        }
    }
    
    // Register the timer callback with REAPER (runs on main thread automatically)
    use crate::infrastructure::timer::register_timer;
    let app = APP_INSTANCE.get().expect("App should be initialized").get();
    register_timer(&mut app.session_mut(), polling_timer_callback)?;
    
    info!("✅ FastTrackStudio REAPER Extension initialized successfully");
    info!("🔧 Final step: Returning from plugin_main");
    Ok(())
}
