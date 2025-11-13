//! Multi-Protocol Transport Server Example
//!
//! This example demonstrates how to run HTTP, WebSocket, and OSC servers
//! simultaneously for transport operations. This showcases the flexibility
//! of the transport architecture where the same core logic can be accessed
//! through multiple protocols.
//!
//! ## Features
//!
//! - HTTP REST API on port 3000
//! - WebSocket server on port 3001
//! - OSC server on UDP port 9000
//! - All protocols control the same transport instance
//! - Real-time status updates via WebSocket
//!
//! ## Usage
//!
//! Run the server:
//! ```bash
//! cargo run --example multi_protocol_server
//! ```
//!
//! Then interact via different protocols:
//!
//! ### HTTP API
//! ```bash
//! # Start playback
//! curl -X POST http://localhost:3000/play
//!
//! # Set tempo
//! curl -X POST http://localhost:3000/set_tempo \
//!      -H "Content-Type: application/json" \
//!      -d '{"bpm": 140.0}'
//!
//! # Get status
//! curl http://localhost:3000/status
//! ```
//!
//! ### OSC Commands
//! ```bash
//! # Start playback
//! oscsend localhost 9000 /transport/play
//!
//! # Set tempo
//! oscsend localhost 9000 /transport/tempo f 140.0
//!
//! # Get status
//! oscsend localhost 9000 /transport/status
//! ```
//!
//! ### WebSocket
//! Connect to ws://localhost:3001/ws for real-time updates

use std::sync::Arc;
use tokio::sync::Mutex;
use axum::{
    Router,
    routing::get,
};
use tower::ServiceBuilder;
use tower_http::cors::CorsLayer;
use transport::{
    MockTransportService,
    TransportActions,
    infra::{
        http::create_transport_http_router,
        websocket::{create_transport_ws_router, WebSocketHandler},
        osc::create_transport_osc_server,
    },
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    println!("🎵 Multi-Protocol Transport Server");
    println!("==================================");

    // Create shared transport service
    println!("📦 Creating shared transport service...");
    let transport_service = MockTransportService::new();
    let transport = Arc::new(Mutex::new(transport_service));

    // Print initial status
    {
        let t = transport.lock().await;
        match t.get_transport() {
            Ok(transport_state) => {
                println!("🎼 Initial Transport State:");
                println!("   - Play State: {:?}", transport_state.play_state);
                println!("   - Tempo: {:.1} BPM", transport_state.tempo.bpm);
                println!("   - Time Signature: {}/{}",
                    transport_state.time_signature.numerator,
                    transport_state.time_signature.denominator);
                println!("   - Position: {:.2}s", transport_state.playhead_position.time.to_seconds());
                println!("   - Record Mode: {:?}", transport_state.record_mode);
            }
            Err(e) => println!("❌ Error getting initial state: {}", e),
        }
    }

    println!();
    println!("🚀 Starting servers...");

    // Clone transport for each server
    let transport_http = Arc::clone(&transport);
    let transport_ws = Arc::clone(&transport);
    let transport_osc = Arc::clone(&transport);

    // Start HTTP server
    let http_handle = tokio::spawn(async move {
        println!("🌐 HTTP server starting on http://localhost:3000");

        let http_router = create_transport_http_router();

        let app = Router::new()
            .nest("/", http_router)
            .route("/health", get(|| async { "OK" }))
            .layer(
                ServiceBuilder::new()
                    .layer(CorsLayer::permissive())
            )
            .with_state(transport_http);

        let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await
            .expect("Failed to bind HTTP server");

        println!("✅ HTTP server listening on http://localhost:3000");

        if let Err(e) = axum::serve(listener, app).await {
            eprintln!("❌ HTTP server error: {}", e);
        }
    });

    // Start WebSocket server
    let ws_handle = tokio::spawn(async move {
        println!("🔌 WebSocket server starting on ws://localhost:3001");

        // Create WebSocket handler
        let ws_handler = WebSocketHandler::new(transport_ws);
        let ws_router = create_transport_ws_router();

        let app = Router::new()
            .nest("/", ws_router)
            .route("/health", get(|| async { "WebSocket OK" }))
            .with_state(ws_handler);

        let listener = tokio::net::TcpListener::bind("0.0.0.0:3001").await
            .expect("Failed to bind WebSocket server");

        println!("✅ WebSocket server listening on ws://localhost:3001");

        if let Err(e) = axum::serve(listener, app).await {
            eprintln!("❌ WebSocket server error: {}", e);
        }
    });

    // Start OSC server
    let osc_handle = tokio::spawn(async move {
        println!("📡 OSC server starting on UDP port 9000");

        if let Err(e) = create_transport_osc_server(transport_osc, "0.0.0.0:9000").await {
            eprintln!("❌ OSC server error: {}", e);
        }
    });

    // Give servers a moment to start
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    println!();
    println!("🎯 All servers are running!");
    println!();
    println!("📋 Available Endpoints:");
    println!();
    println!("   🌐 HTTP API (port 3000):");
    println!("   - GET  http://localhost:3000/status");
    println!("   - POST http://localhost:3000/play");
    println!("   - POST http://localhost:3000/pause");
    println!("   - POST http://localhost:3000/stop");
    println!("   - POST http://localhost:3000/set_tempo");
    println!("   - GET  http://localhost:3000/health");
    println!();
    println!("   🔌 WebSocket (port 3001):");
    println!("   - ws://localhost:3001/ws (real-time updates)");
    println!("   - GET http://localhost:3001/health");
    println!();
    println!("   📡 OSC Server (UDP port 9000):");
    println!("   - /transport/play");
    println!("   - /transport/pause");
    println!("   - /transport/stop");
    println!("   - /transport/tempo [float bpm]");
    println!("   - /transport/status");
    println!();
    println!("💡 Example Commands:");
    println!();
    println!("   HTTP:");
    println!("   curl -X POST http://localhost:3000/play");
    println!("   curl http://localhost:3000/status");
    println!("   curl -X POST http://localhost:3000/set_tempo \\");
    println!("        -H 'Content-Type: application/json' \\");
    println!("        -d '{{\"bpm\": 140.0}}'");
    println!();
    println!("   OSC:");
    println!("   oscsend localhost 9000 /transport/play");
    println!("   oscsend localhost 9000 /transport/tempo f 140.0");
    println!("   oscsend localhost 9000 /transport/status");
    println!();
    println!("   WebSocket (JavaScript):");
    println!("   const ws = new WebSocket('ws://localhost:3001/ws');");
    println!("   ws.send(JSON.stringify({{command: 'play'}}));");
    println!();
    println!("🔧 Press Ctrl+C to stop all servers");
    println!("====================================");

    // Wait for all servers (they run indefinitely)
    tokio::select! {
        _ = http_handle => println!("HTTP server stopped"),
        _ = ws_handle => println!("WebSocket server stopped"),
        _ = osc_handle => println!("OSC server stopped"),
    }

    println!("✅ All servers stopped");
    Ok(())
}
