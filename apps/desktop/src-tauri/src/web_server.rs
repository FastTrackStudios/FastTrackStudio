//! Web server for serving the Vite application
//!
//! This module provides an Axum web server that serves the built Vite application,
//! allowing network access to the UI from any device on the same network.

use axum::{
    http::{HeaderValue, StatusCode},
    response::{Html, IntoResponse},
    routing::get,
    Router,
};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::net::TcpListener;
use tower::ServiceBuilder;
use tower_http::{
    cors::{Any, AllowOrigin, CorsLayer},
    services::ServeDir,
    trace::TraceLayer,
};
use tracing::{info, warn};

/// Configuration for the web server
#[derive(Debug, Clone)]
pub struct WebServerConfig {
    /// Port to bind the server to
    pub port: u16,
    /// Path to the Vite dist directory
    pub dist_path: PathBuf,
    /// Whether to enable CORS (should be true for network access)
    pub enable_cors: bool,
    /// Allowed origins for CORS (empty = allow all)
    pub allowed_origins: Vec<String>,
}

impl Default for WebServerConfig {
    fn default() -> Self {
        Self {
            port: 8080,
            dist_path: PathBuf::from("dist"),
            enable_cors: true,
            allowed_origins: Vec::new(),
        }
    }
}

/// Create the Axum router for serving the Vite application
pub fn create_web_router(config: Arc<WebServerConfig>) -> Router {
    let dist_path = config.dist_path.clone();
    let index_path = dist_path.join("index.html");

    // Create CORS layer
    let cors = if config.enable_cors {
        if config.allowed_origins.is_empty() {
            // Allow all origins
            CorsLayer::new()
                .allow_origin(Any)
                .allow_methods(Any)
                .allow_headers(Any)
        } else {
            // Allow specific origins
            let mut cors_builder = CorsLayer::new();
            for origin in &config.allowed_origins {
                if let Ok(parsed) = origin.parse::<HeaderValue>() {
                    cors_builder = cors_builder.allow_origin(AllowOrigin::exact(parsed));
                }
            }
            cors_builder.allow_methods(Any).allow_headers(Any)
        }
    } else {
        CorsLayer::new()
    };

    Router::new()
        // API routes (if any) go here first
        // .nest("/api", api_router())
        
        // Serve static files from assets directory
        .nest_service(
            "/assets",
            ServeDir::new(dist_path.join("assets")),
        )
        .layer(
            ServiceBuilder::new()
                .layer(TraceLayer::new_for_http())
                .layer(cors),
        )
        // SPA fallback: serve index.html for all routes (after trying static files)
        .fallback_service(get(move || {
            let index_path = index_path.clone();
            async move {
                match tokio::fs::read_to_string(&index_path).await {
                    Ok(html) => Html(html).into_response(),
                    Err(e) => {
                        warn!(error = %e, path = %index_path.display(), "Failed to read index.html");
                        (
                            StatusCode::INTERNAL_SERVER_ERROR,
                            format!("Failed to load index.html: {}", e),
                        )
                            .into_response()
                    }
                }
            }
        }))
}

/// Start the web server
pub async fn start_web_server(config: WebServerConfig) -> anyhow::Result<tokio::task::JoinHandle<anyhow::Result<()>>> {
    let router = create_web_router(Arc::new(config.clone()));
    
    let addr = std::net::SocketAddr::from(([0, 0, 0, 0], config.port));
    let listener = TcpListener::bind(&addr).await?;
    
    info!(
        port = config.port,
        dist_path = %config.dist_path.display(),
        "Starting web server for Vite application"
    );
    
    let handle = tokio::spawn(async move {
        info!("Web server listening on http://0.0.0.0:{}", config.port);
        info!("Access the UI from any device on your network at http://<your-ip>:{}", config.port);
        
        if let Err(e) = axum::serve(listener, router).await {
            anyhow::bail!("Web server error: {}", e);
        }
        
        Ok(())
    });
    
    Ok(handle)
}

/// Get the local network IP address (for display purposes)
pub fn get_local_ip() -> Option<String> {
    // Try to get the local IP address
    // This is a simple implementation - you might want to use a crate like `local_ip_address`
    use std::net::UdpSocket;
    
    let socket = UdpSocket::bind("0.0.0.0:0").ok()?;
    socket.connect("8.8.8.8:80").ok()?;
    socket.local_addr().ok()?.ip().to_string().into()
}

