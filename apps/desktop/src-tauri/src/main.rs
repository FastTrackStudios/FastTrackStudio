// Prevents additional console window on Windows in release builds
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

fn main() {
    // Start Tauri application
    // In dev mode, Vite dev server runs directly (via beforeDevCommand)
    // WebSocket server is handled by REAPER extension
    tauri::Builder::default()
        .setup(|_app| {
            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
