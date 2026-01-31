//! FastTrackStudio - REAPER Extension (Minimal)
//!
//! Bare minimum implementation for testing the architecture.

use reaper_high::{PluginContext, Reaper, ReaperPlugin};
use reaper_medium::{HookCommand, WindowHandle};
use std::sync::Arc;

mod app;
mod command_executor;
mod services;

use app::App;

/// Plugin entry point
#[reaper_high::reaper_extension_plugin]
fn plugin_main(context: PluginContext) -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    tracing_subscriber::fmt::init();

    // Create and initialize the app
    let app = App::new(context);

    // Register control surface for timer callbacks
    Reaper::get().plugin_register_add_control_surface(&app);

    // Register actions
    register_actions();

    Ok(())
}

fn register_actions() {
    // TODO: Register minimal set of actions
    // For now, we'll just have the basic transport commands
}

/// Control surface implementation
struct ControlSurface {
    app: Arc<App>,
}

impl reaper_high::ControlSurface for ControlSurface {
    fn run(&mut self) {
        // Called by REAPER on main thread
        // Process command queue
        self.app.command_executor.process_pending();

        // Update transport state from REAPER
        self.app.transport_service.update_from_reaper();
    }
}
