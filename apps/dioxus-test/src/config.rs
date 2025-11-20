//! Configuration for WebSocket connection modes
//!
//! Supports connecting to:
//! - Local: Client-side state management only (no WebSocket)
//! - Server: Native Dioxus server (default)
//! - REAPER extension: External REAPER WebSocket server
//! - Custom: Custom WebSocket URL

use serde::{Deserialize, Serialize};

/// WebSocket connection mode
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum WebSocketMode {
    /// Client-side state management only (no WebSocket connection)
    Local,
    /// Connect to native Dioxus server (default)
    Server,
    /// Connect to REAPER extension
    Reaper { url: String },
    /// Custom WebSocket URL
    Custom { url: String },
}

impl Default for WebSocketMode {
    fn default() -> Self {
        Self::Server
    }
}

impl WebSocketMode {
    /// Get the WebSocket URL for this mode
    /// Returns None for Local mode (no WebSocket connection)
    pub fn url(&self, local_port: u16) -> Option<String> {
        match self {
            WebSocketMode::Local => {
                // Local mode doesn't use WebSocket
                None
            }
            WebSocketMode::Server => {
                // Native Dioxus server - use relative path for WASM, localhost for desktop
                #[cfg(target_arch = "wasm32")]
                {
                    // For WASM, use relative path that gets resolved by the browser
                    Some("/api/websocket".to_string())
                }
                #[cfg(not(target_arch = "wasm32"))]
                {
                    Some(format!("ws://localhost:{}/api/websocket", local_port))
                }
            }
            WebSocketMode::Reaper { url } => Some(url.clone()),
            WebSocketMode::Custom { url } => Some(url.clone()),
        }
    }

    /// Parse from environment variable or config
    pub fn from_env() -> Self {
        // Check for WS_URL environment variable
        if let Ok(url) = std::env::var("WS_URL") {
            if url.starts_with("ws://") || url.starts_with("wss://") {
                return WebSocketMode::Custom { url };
            }
        }

        // Check for WS_MODE environment variable
        if let Ok(mode) = std::env::var("WS_MODE") {
            match mode.as_str() {
                "local" => return WebSocketMode::Local,
                "server" => return WebSocketMode::Server,
                "reaper" => {
                    // Default REAPER extension URL
                    let url = std::env::var("WS_URL").unwrap_or_else(|_| "ws://localhost:8081".to_string());
                    return WebSocketMode::Reaper { url };
                }
                _ => {}
            }
        }

        // Default to server (native Dioxus server)
        WebSocketMode::Server
    }
}

/// Application configuration
#[derive(Debug, Clone, PartialEq)]
pub struct AppConfig {
    pub ws_mode: WebSocketMode,
    pub local_port: u16,
}

impl Default for AppConfig {
    fn default() -> Self {
        Self {
            ws_mode: WebSocketMode::from_env(),
            local_port: 3000, // Default Dioxus port (set DEVSERVER_PORT env var to change)
        }
    }
}

impl AppConfig {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_mode(mode: WebSocketMode) -> Self {
        Self {
            ws_mode: mode,
            local_port: 3000,
        }
    }

    pub fn with_port(port: u16) -> Self {
        Self {
            ws_mode: WebSocketMode::Server,
            local_port: port,
        }
    }

    pub fn ws_url(&self) -> Option<String> {
        self.ws_mode.url(self.local_port)
    }
}

