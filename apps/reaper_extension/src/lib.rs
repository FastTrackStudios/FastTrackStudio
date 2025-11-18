mod action_registry;
mod tracing_config;
mod actions;
mod menu;
mod reaper_transport;
mod reaper_project;
mod reaper_markers;
mod reaper_setlist;

use reaper_macros::reaper_extension_plugin;
use std::error::Error;
use reaper_low::{PluginContext, Swell};
use reaper_medium::ReaperSession;
use reaper_high::Reaper as HighReaper;
use tracing::{debug, info, warn};

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
    
    // Create a medium-level API session
    let _session = ReaperSession::load(context);
    let reaper = _session.reaper();
    
    // Print message to the ReaScript console
    reaper.show_console_msg("FastTrackStudio REAPER Extension loaded!\n");
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
    
    info!("✅ FastTrackStudio REAPER Extension initialized successfully");
    Ok(())
}

