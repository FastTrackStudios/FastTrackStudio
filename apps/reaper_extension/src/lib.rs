mod action_registry;
mod tracing_config;
mod actions;
mod menu;

use reaper_macros::reaper_extension_plugin;
use std::error::Error;
use std::sync::OnceLock;
use fragile::Fragile;
use reaper_low::{PluginContext, Swell};
use reaper_medium::{ProjectRef, Reaper, ReaperSession};
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
    let mut session = ReaperSession::load(context);
    let reaper = session.reaper();
    
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
    
    // Print the file path of the currently loaded project
    print_project_file_path(reaper);
    
    // Register a timer to get a place in REAPER's main event loop
    // This allows us to defer stuff for later execution
    session.plugin_register_add_timer(timer_callback)?;
    
    // Save the session in a static variable so it lives longer than plugin_main
    // This is required because the timer callback needs access to the session
    let _ = REAPER_SESSION.set(Fragile::new(session));
    
    info!("✅ FastTrackStudio REAPER Extension initialized successfully");
    Ok(())
}

/// Timer callback that runs periodically in REAPER's main event loop
extern "C" fn timer_callback() {
    let reaper_session = reaper_session();
    let reaper = reaper_session.reaper();
    
    // Example: Print project path periodically
    // In a real implementation, this would check for transport state changes
    // and broadcast them via the message router
    print_project_file_path(reaper);
}

/// Print the current project file path to the console
fn print_project_file_path(reaper: &Reaper) {
    let project_path = reaper
        .enum_projects(ProjectRef::Current, 256)
        .map(|project| project.file_path);
    
    if let Some(path) = project_path {
        reaper.show_console_msg(format!("Current project: {path:?}\n"));
    } else {
        reaper.show_console_msg("No project loaded\n");
    }
}

/// Get the static REAPER session
fn reaper_session() -> &'static ReaperSession {
    REAPER_SESSION
        .get()
        .expect("ReaperSession hasn't been set as static variable yet")
        .get()
}

/// The static variable that keeps the reaper-medium [`ReaperSession`] in memory while REAPER
/// is running.
///
/// - `OnceLock` is the standard Rust way to set static variables that will be initialized once and never again.
/// - `Fragile` comes from a third-party crate and makes sure that the session is only ever accessed from the main
///   thread. This is important because [`ReaperSession`] is not thread-safe. Without [`Fragile`], it wouldn't even
///   compile, thanks to Rust's safety rules :)
static REAPER_SESSION: OnceLock<Fragile<ReaperSession>> = OnceLock::new();

