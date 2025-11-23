mod action_registry;
mod tracing_config;
mod actions;
mod menu;
mod reaper_transport;
mod reaper_project;
mod reaper_markers;
mod reaper_setlist;
mod color_utils;
mod command_handler;
mod setlist_stream;

mod live;

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
use std::sync::Arc;
use reaper_low::{PluginContext, Swell};
use reaper_medium::ReaperSession;
use reaper_high::Reaper as HighReaper;
use tracing::{debug, error, info, warn};
use crate::reaper_setlist::build_setlist_from_open_projects;

/// REAPER will call this extension entry-point function once when it's starting.
#[reaper_extension_plugin]
fn plugin_main(context: PluginContext) -> Result<(), Box<dyn Error>> {
    // Initialize tracing FIRST, before anything else
    tracing_config::init_tracing();
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
    let session = ReaperSession::load(context);
    let reaper = session.reaper();
    
    // Print message to the ReaScript console
    reaper.show_console_msg("FastTrackStudio REAPER Extension loaded!\n");
    
    // Leak the session to keep it alive (timer callbacks need the session to persist)
    // This is safe because the extension lives for the lifetime of REAPER
    let session_ptr = Box::leak(Box::new(session));
    info!("FastTrackStudio REAPER Extension loaded");
    
    // Register actions
    info!("Registering actions...");
    actions::register_all_actions();
    
    // Register live tracks actions
    info!("Registering live tracks actions...");
    live::tracks::actions::register_all_actions();
    
    // Wake up REAPER so actions are fully registered and available
    // This must be done before registering the menu so command IDs can be found
    match HighReaper::get().wake_up() {
        Ok(_) => {
            debug!("REAPER woken up (actions ready)");
        }
        Err(e) => {
            warn!("Failed to wake up REAPER: {}", e);
        }
    }
    
    // Register extension menu (must be after actions are registered and REAPER is woken up)
    info!("🔧 About to register extension menu...");
    if let Err(e) = menu::register_extension_menu() {
        warn!("Failed to register extension menu: {:#}", e);
    } else {
        info!("FastTrackStudio menu registered successfully");
    }
    
    // Initialize setlist state storage for setlist stream service
    info!("🔧 About to initialize setlist state storage...");
    setlist_stream::init_setlist_state();
    info!("🔧 Setlist state storage initialized");
    
    // Start irpc server for IPC with desktop app
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime");
        
        rt.block_on(async {
            info!("Starting IROH router with setlist stream in tokio runtime...");
            
            // Create setlist stream service
            let setlist_api = match setlist_stream::create_reaper_setlist_stream_service() {
                Ok(api) => api,
                Err(e) => {
                    warn!("Failed to create setlist stream service: {}", e);
                    return;
                }
            };
            
            // Expose setlist stream handler
            let setlist_handler = match setlist_api.expose() {
                Ok(handler) => handler,
                Err(e) => {
                    warn!("Failed to expose setlist stream service: {}", e);
                    return;
                }
            };
            
            // Create IROH endpoint and router with ALL protocols
            let endpoint = match iroh::Endpoint::bind().await {
                Ok(ep) => ep,
                Err(e) => {
                    error!("Failed to create IROH endpoint: {}", e);
                    return;
                }
            };
            let endpoint_id = endpoint.id();
                    
            // Build router with REAPER_ALPN - all protocols route through this single ALPN
            let _router = iroh::protocol::Router::builder(endpoint.clone())
                .accept(peer_2_peer::iroh_connection::REAPER_ALPN.to_vec(), setlist_handler)
                .spawn();
            
            // Store endpoint ID for client discovery
            if let Err(e) = peer_2_peer::iroh_connection::store_endpoint_id(endpoint_id) {
                warn!("Failed to store endpoint ID: {}", e);
            }
            
            info!("IROH router started successfully");
            info!("Router endpoint ID: {}", endpoint_id);
            info!("REAPER ALPN: {:?}", String::from_utf8_lossy(peer_2_peer::iroh_connection::REAPER_ALPN));
                    
                    // Keep router alive - it handles all connections
                    // The router will run until shutdown is called
            loop {
                tokio::time::sleep(tokio::time::Duration::from_secs(60)).await;
                        info!("IROH router runtime still active");
            }
        });
    });
    
    // Handle transport read requests in the timer callback
    // We'll integrate this into the existing timer callback
    
    // Register REAPER timer callback for 60Hz polling (~16.67ms intervals)
    // This runs on REAPER's main thread automatically, so we can safely call REAPER APIs
    // Note: REAPER's timer may run at its own rate, but we'll update state on every call
    extern "C" fn polling_timer_callback() {
        use std::sync::atomic::{AtomicU64, Ordering};
        use std::time::Instant;
        
        static TICK_COUNT: AtomicU64 = AtomicU64::new(0);
        static LAST_LOG: std::sync::OnceLock<std::sync::Mutex<Instant>> = std::sync::OnceLock::new();
        static FIRST_CALL: std::sync::OnceLock<std::sync::atomic::AtomicBool> = std::sync::OnceLock::new();
        
        // Always log first call to confirm timer is being called
        let first_call = FIRST_CALL.get_or_init(|| std::sync::atomic::AtomicBool::new(true));
        if first_call.swap(false, Ordering::Relaxed) {
            info!("⏰⏰⏰ Timer callback FIRST CALL - timer is working! ⏰⏰⏰");
        }
        
        // ALWAYS run - no polling state check
        TICK_COUNT.fetch_add(1, Ordering::Relaxed);
        
        // Update setlist state on every tick (target: 60Hz)
        setlist_stream::update_setlist_state();
        
        // Log performance metrics every 10 seconds
        let last_log = LAST_LOG.get_or_init(|| std::sync::Mutex::new(Instant::now()));
        if let Ok(mut last_log_guard) = last_log.lock() {
            if last_log_guard.elapsed().as_secs() >= 10 {
                let poll_rate = TICK_COUNT.swap(0, Ordering::Relaxed) / 10;
                info!("[Performance] REAPER API polling: {} ticks/s (target: 60Hz)", poll_rate);
                *last_log_guard = Instant::now();
            }
        }
    }
    
    // Register the timer callback with REAPER (runs on main thread automatically)
    // REAPER typically calls this at ~30Hz, but we'll update on every call
    // To achieve 60Hz, we may need to use a different approach or REAPER may call it faster
    info!("🔧 About to register timer callback with REAPER...");
    
    let timer_result = session_ptr.plugin_register_add_timer(polling_timer_callback);
    
    match timer_result {
        Ok(_) => {
            info!("✅✅✅ SUCCESS: Registered polling timer callback with REAPER (target: 60Hz)");
            info!("⏰ Timer callback registered - waiting for REAPER to call it...");
            info!("📊 Timer will ALWAYS run - no polling state checks");
            info!("📊 If you don't see 'Timer callback FIRST CALL' logs, REAPER is not calling the timer");
        }
        Err(e) => {
            error!("❌❌❌ FAILED to register polling timer callback: {}", e);
            error!("❌ Timer will NOT work! Error details: {:?}", e);
        }
    }
    
    info!("✅ FastTrackStudio REAPER Extension initialized successfully");
    info!("🔧 Final step: Returning from plugin_main");
    Ok(())
}

