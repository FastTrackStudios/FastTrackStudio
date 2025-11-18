// Prevents additional console window on Windows in release builds
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod web_server;

use std::path::PathBuf;
use tracing::{info, error};
use tracing_subscriber;

fn main() {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info")),
        )
        .init();

    info!("Starting FastTrackStudio Desktop Application");

    // Get the path to the dist directory (relative to the executable)
    // In Tauri, the dist folder is typically in the parent directory of src-tauri
    let dist_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("dist");

    // Configure web server
    let web_config = web_server::WebServerConfig {
        port: 8080,
        dist_path,
        enable_cors: true, // Enable CORS for network access
        allowed_origins: Vec::new(), // Empty = allow all origins
    };

    // Create a tokio runtime for the web server in a separate thread
    // This runs independently of Tauri's runtime and keeps the runtime alive
    let web_config_clone = web_config.clone();
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime");
        rt.block_on(async {
            match web_server::start_web_server(web_config_clone).await {
                Ok(handle) => {
                    info!("Web server started successfully");
                    if let Err(e) = handle.await {
                        error!(error = %e, "Web server task failed");
                    }
                }
                Err(e) => {
                    error!(error = %e, "Failed to start web server");
                }
            }
        });
    });
    
    // Get local IP for display
    if let Some(ip) = web_server::get_local_ip() {
        info!("Access the web UI from any device on your network:");
        info!("  http://{}:8080", ip);
    } else {
        info!("Access the web UI from any device on your network:");
        info!("  http://<your-ip>:8080");
    }

    // Start Tauri application
    tauri::Builder::default()
        .setup(|_app| {
            info!("Tauri application setup complete");
            
            // Wait a moment for the web server to start before the window loads
            std::thread::sleep(std::time::Duration::from_millis(500));
            
            info!("Window should be loading http://localhost:8080");
            
            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
