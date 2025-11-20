mod action_registry;
mod tracing_config;
mod actions;
mod menu;
mod reaper_transport;
mod reaper_project;
mod reaper_markers;
mod reaper_setlist;
mod websocket_client;
mod websocket_state;
mod websocket_server;
mod color_utils;

use reaper_macros::reaper_extension_plugin;
use std::error::Error;
use std::sync::Arc;
use reaper_low::{PluginContext, Swell};
use reaper_medium::ReaperSession;
use reaper_high::Reaper as HighReaper;
use tracing::{debug, error, info, warn};
use crate::websocket_server::start_websocket_server;
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
    if let Err(e) = menu::register_extension_menu() {
        warn!("Failed to register extension menu: {:#}", e);
    } else {
        info!("FastTrackStudio menu registered successfully");
    }
    
    // Build initial setlist on main thread (REAPER APIs must be called from main thread)
    let initial_setlist = build_setlist_from_open_projects(None).ok();
    
    // Create channel for setlist rebuild requests from WebSocket server
    let (rebuild_tx, rebuild_rx) = tokio::sync::mpsc::unbounded_channel();
    
    // Wrap receiver in Arc<Mutex> so we can share it between threads
    let rebuild_rx_shared = Arc::new(tokio::sync::Mutex::new(rebuild_rx));
    let rebuild_rx_for_polling = rebuild_rx_shared.clone();
    
    // Start WebSocket server in background
    // REAPER extensions run synchronously, so we spawn a tokio runtime for async operations
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime");
        let rt_handle = rt.handle().clone();
        
        rt.block_on(async {
            match start_websocket_server(8080).await {
                Ok((handle, ws_state)) => {
                    info!("WebSocket server started successfully on port 8080");
                    
                    // Store the WebSocket state globally for broadcasting
                    let _ = crate::websocket_state::set_global_ws_state(ws_state.clone());
                    
                    // Store the runtime handle globally so we can spawn tasks from REAPER's main thread
                    let _ = crate::websocket_state::set_global_rt_handle(rt_handle);
                    
                    // Store the rebuild request sender
                    let _ = crate::websocket_state::set_setlist_rebuild_sender(rebuild_tx);
                    
                    // Cache initial setlist if we built one on the main thread
                    if let Some(setlist) = initial_setlist {
                        ws_state.update_cached_setlist(setlist).await;
                        info!("Initial setlist cached for new connections");
                    }
                    
                    if let Err(e) = handle.await {
                        error!(error = %e, "WebSocket server task failed");
                    }
                }
                Err(e) => {
                    error!(error = %e, "Failed to start WebSocket server");
                }
            }
        });
    });
    
    // Create channel for action execution requests from background threads
    let (action_exec_tx, action_exec_rx) = std::sync::mpsc::channel();
    let _ = crate::websocket_state::set_action_execution_sender(action_exec_tx);
    
    // Store receiver globally so polling action can process it
    let _ = crate::websocket_state::set_action_execution_receiver(action_exec_rx);
    
    // Poll for setlist rebuild requests and trigger the action via channel
    let rebuild_rx_for_polling_clone = rebuild_rx_for_polling.clone();
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime for rebuild polling");
        rt.block_on(async {
            while let Some(_) = rebuild_rx_for_polling_clone.lock().await.recv().await {
                info!("Setlist rebuild requested by WebSocket client - triggering action");
                // Use stored command ID (looked up on main thread, safe to use from background thread)
                if let Some(cmd_id) = crate::action_registry::get_command_id("FTS_BUILD_SETLIST") {
                    // Send command ID to main thread via channel
                    crate::websocket_state::request_action_execution(cmd_id);
                    info!(
                        command_id = "FTS_BUILD_SETLIST",
                        "Requested Build Setlist action execution via channel"
                    );
                    } else {
                    warn!("Could not find stored command ID for FTS_BUILD_SETLIST - action may not be registered yet");
                }
            }
        });
    });
    
    // Create channel for project switch requests from WebSocket server to main thread
    // Bridge: tokio::sync::mpsc (WebSocket) -> std::sync::mpsc (main thread)
    let (project_switch_tx_tokio, mut project_switch_rx_tokio) = tokio::sync::mpsc::unbounded_channel::<String>();
    let (project_switch_tx_std, project_switch_rx_std) = std::sync::mpsc::channel::<String>();
    
    // Store sender for WebSocket server
    let _ = crate::websocket_state::set_project_switch_sender(project_switch_tx_tokio);
    
    // Store receiver for main thread
    let _ = crate::websocket_state::set_project_switch_receiver(project_switch_rx_std);
    
    // Bridge thread: forward from tokio channel to std channel
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime for project switch bridge");
        rt.block_on(async {
            while let Some(project_name) = project_switch_rx_tokio.recv().await {
                let _ = project_switch_tx_std.send(project_name);
            }
        });
    });
    
    // Create channel for section seek requests from WebSocket server to main thread
    // Bridge: tokio::sync::mpsc (WebSocket) -> std::sync::mpsc (main thread)
    let (section_seek_tx_tokio, mut section_seek_rx_tokio) = tokio::sync::mpsc::unbounded_channel::<(String, String, String)>();
    let (section_seek_tx_std, section_seek_rx_std) = std::sync::mpsc::channel::<(String, String, String)>();
    
    // Store sender for WebSocket server
    let _ = crate::websocket_state::set_section_seek_sender(section_seek_tx_tokio);
    
    // Store receiver for main thread
    let _ = crate::websocket_state::set_section_seek_receiver(section_seek_rx_std);
    
    // Bridge thread: forward from tokio channel to std channel
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime for section seek bridge");
        rt.block_on(async {
            while let Some(request) = section_seek_rx_tokio.recv().await {
                let _ = section_seek_tx_std.send(request);
            }
        });
    });
    
    // Create channel for transport updates from main thread to WebSocket server
    let (transport_tx, mut transport_rx) = tokio::sync::mpsc::unbounded_channel::<(String, bool, bool, f64, f64, std::collections::HashMap<String, f64>, std::collections::HashMap<String, f64>)>();
    let _ = crate::websocket_state::set_transport_update_sender(transport_tx);
    
    // Start transport polling task that receives updates from main thread and broadcasts them
    // Process messages as fast as possible - spawn each broadcast as a separate task for parallelism
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime for transport polling");
        rt.block_on(async {
            loop {
                match transport_rx.recv().await {
                    Some((project_name, is_active, playing, position, tempo, song_progress, section_progress)) => {
                        // Broadcast immediately - this is fast (just sends to broadcast channel)
                        if let Some(state) = crate::websocket_state::get_global_ws_state() {
                            crate::websocket_server::broadcast_transport(&state, &project_name, is_active, playing, position, tempo, song_progress, section_progress);
                        }
                    }
                    None => {
                        warn!("Transport update channel closed");
                        break;
                    }
                }
            }
        });
    });
    
    // Poll transport and setlist state immediately to get initial state
    crate::websocket_state::poll_and_broadcast_transport();
    crate::websocket_state::poll_and_broadcast_setlist();
    
    // Register REAPER timer callback for 30Hz polling (~33ms intervals)
    // This runs on REAPER's main thread automatically, so we can safely call REAPER APIs
    extern "C" fn polling_timer_callback() {
        use std::sync::atomic::{AtomicU64, Ordering};
        use std::time::Instant;
        
        static TICK_COUNT: AtomicU64 = AtomicU64::new(0);
        static LAST_LOG: std::sync::OnceLock<std::sync::Mutex<Instant>> = std::sync::OnceLock::new();
        static FIRST_CALL: std::sync::OnceLock<std::sync::atomic::AtomicBool> = std::sync::OnceLock::new();
        
        let tick_count = TICK_COUNT.fetch_add(1, Ordering::Relaxed) + 1;
        
        // Log first call immediately to confirm timer is working
        let first_call = FIRST_CALL.get_or_init(|| std::sync::atomic::AtomicBool::new(true));
        if first_call.swap(false, Ordering::Relaxed) {
            info!("⏰⏰⏰ Timer callback FIRST CALL - timer is working! ⏰⏰⏰");
        }
        
        // Process any pending action execution requests from background threads
        crate::websocket_state::process_action_execution_requests();
        
        // Process any pending project switch requests from background threads
        let switch_count = crate::websocket_state::process_project_switch_requests();
        if switch_count > 0 {
            tracing::info!(count = switch_count, "Processed {} project switch request(s)", switch_count);
        }
        
        // Process any pending section seek requests from background threads
        let seek_count = crate::websocket_state::process_section_seek_requests();
        if seek_count > 0 {
            tracing::info!(count = seek_count, "Processed {} section seek request(s)", seek_count);
        }
        
        // Poll and broadcast transport state for all projects (30Hz for smooth progress)
        crate::websocket_state::poll_and_broadcast_transport();
        
        // Poll setlist state less frequently (every 5 seconds = 150 ticks at 30Hz)
        // Setlist structure changes infrequently, so we don't need frequent updates
        if tick_count % 150 == 0 {
            crate::websocket_state::poll_and_broadcast_setlist();
        }
        
        // Log every second (30 ticks) to avoid spam
        let last_log = LAST_LOG.get_or_init(|| std::sync::Mutex::new(Instant::now()));
        if let Ok(mut last) = last_log.lock() {
            if last.elapsed().as_secs() >= 1 {
                info!(
                    tick_count = tick_count,
                    "🔄 30Hz timer polling active - {} ticks completed (transport + setlist)",
                    tick_count
                );
                *last = Instant::now();
            }
        }
    }
    
    // Register the timer callback with REAPER (runs on main thread automatically)
    info!("Attempting to register timer callback with REAPER...");
    match session_ptr.plugin_register_add_timer(polling_timer_callback) {
        Ok(_) => {
            info!("✅ Successfully registered 30Hz polling timer callback with REAPER - timer should start automatically");
            info!("⏰ Timer callback registered - waiting for REAPER to call it...");
        }
        Err(e) => {
            error!(error = %e, "❌ Failed to register polling timer callback - timer will NOT work!");
        }
    }
    
    info!("✅ FastTrackStudio REAPER Extension initialized successfully");
    Ok(())
}

