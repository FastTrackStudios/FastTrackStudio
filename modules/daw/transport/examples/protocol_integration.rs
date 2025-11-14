//! Protocol Integration Example
//!
//! This example demonstrates the core pattern for integrating HTTP, WebSocket,
//! and OSC protocols with a shared transport service. This is a simplified
//! version that focuses on the integration architecture rather than a full
//! web interface.
//!
//! ## Architecture Pattern
//!
//! ```
//! ┌─────────────┐  ┌─────────────┐  ┌─────────────┐
//! │    HTTP     │  │  WebSocket  │  │    OSC      │
//! │   :3000     │  │   :3001     │  │   :9000     │
//! └──────┬──────┘  └──────┬──────┘  └──────┬──────┘
//!        │                │                │
//!        └────────────────┼────────────────┘
//!                         │
//!               ┌─────────▼──────────┐
//!               │  Shared Transport  │
//!               │      Service       │
//!               └────────────────────┘
//! ```
//!
//! ## Key Integration Points
//!
//! 1. **Shared State**: All protocols share the same transport service instance
//! 2. **Arc<Mutex<T>>**: Thread-safe shared ownership and mutation
//! 3. **Async Tasks**: Each protocol runs in its own async task
//! 4. **Unified Interface**: All protocols implement the same `TransportActions` trait
//!
//! ## Usage
//!
//! ```bash
//! cargo run --example protocol_integration
//! ```
//!
//! Test each protocol:
//!
//! ### HTTP
//! ```bash
//! curl -X POST http://localhost:3000/play
//! curl http://localhost:3000/status
//! ```
//!
//! ### OSC
//! ```bash
//! oscsend localhost 9000 /transport/play
//! oscsend localhost 9000 /transport/status
//! ```
//!
//! ### WebSocket (JavaScript in browser console)
//! ```javascript
//! const ws = new WebSocket('ws://localhost:3001/ws');
//! ws.onmessage = (e) => console.log('Received:', e.data);
//! ws.send(JSON.stringify({command: 'play'}));
//! ```

use axum::routing::get;
use std::sync::Arc;
use tokio::sync::Mutex;
use tower::ServiceBuilder;
use tower_http::cors::CorsLayer;
use transport::{
    MockTransportService, TransportActions,
    core::Tempo,
    infra::{
        http::create_transport_http_router,
        osc::create_transport_osc_server,
        websocket::{WebSocketHandler, create_transport_ws_router},
    },
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    println!("🔗 Protocol Integration Example");
    println!("==============================");

    // Step 1: Create the shared transport service
    // This is the core business logic that all protocols will interact with
    let transport_service = MockTransportService::new();
    let shared_transport = Arc::new(Mutex::new(transport_service));

    // Display initial state
    print_transport_state(&shared_transport, "Initial").await;

    // Step 2: Clone the shared transport for each protocol
    // Each protocol gets its own Arc clone, but they all point to the same underlying data
    let http_transport = Arc::clone(&shared_transport);
    let websocket_transport = Arc::clone(&shared_transport);
    let osc_transport = Arc::clone(&shared_transport);

    println!("🚀 Starting protocol servers...");

    // Step 3: Start each protocol server in its own async task

    // HTTP Server Task
    let http_task = tokio::spawn(async move {
        println!("🌐 Starting HTTP server on port 3000...");

        let router = create_transport_http_router();
        let app = router
            .route("/health", get(|| async { "HTTP OK" }))
            .layer(ServiceBuilder::new().layer(CorsLayer::permissive()))
            .with_state(http_transport);

        match tokio::net::TcpListener::bind("0.0.0.0:3000").await {
            Ok(listener) => {
                println!("✅ HTTP server listening on http://localhost:3000");
                if let Err(e) = axum::serve(listener, app).await {
                    eprintln!("❌ HTTP server error: {}", e);
                }
            }
            Err(e) => eprintln!("❌ Failed to bind HTTP server: {}", e),
        }
    });

    // WebSocket Server Task
    let websocket_task = tokio::spawn(async move {
        println!("🔌 Starting WebSocket server on port 3001...");

        let ws_handler = WebSocketHandler::new(websocket_transport);
        let ws_router = create_transport_ws_router();

        let app = ws_router
            .route("/health", get(|| async { "WebSocket OK" }))
            .layer(ServiceBuilder::new().layer(CorsLayer::permissive()))
            .with_state(ws_handler);

        match tokio::net::TcpListener::bind("0.0.0.0:3001").await {
            Ok(listener) => {
                println!("✅ WebSocket server listening on ws://localhost:3001/ws");
                if let Err(e) = axum::serve(listener, app).await {
                    eprintln!("❌ WebSocket server error: {}", e);
                }
            }
            Err(e) => eprintln!("❌ Failed to bind WebSocket server: {}", e),
        }
    });

    // OSC Server Task
    let osc_task = tokio::spawn(async move {
        println!("📡 Starting OSC server on UDP port 9000...");

        match create_transport_osc_server(osc_transport, "0.0.0.0:9000").await {
            Ok(()) => println!("✅ OSC server listening on UDP port 9000"),
            Err(e) => eprintln!("❌ OSC server error: {}", e),
        }
    });

    // Give servers time to start up
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    print_usage_examples();

    // Step 4: Demonstration of shared state
    // Let's show that all protocols share the same state by making changes
    // through the shared transport and observing the effects
    tokio::spawn({
        let demo_transport = Arc::clone(&shared_transport);
        async move {
            // Wait for servers to be ready
            tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

            println!("\n🔄 Demonstrating shared state...");

            // Make changes through the shared transport
            {
                let mut transport = demo_transport.lock().await;
                println!("📝 Setting tempo to 130 BPM via shared instance...");
                let _ = transport.set_tempo(Tempo::new(130.0));

                println!("📝 Starting playback via shared instance...");
                let _ = transport.play();
            }

            print_transport_state(&demo_transport, "After direct changes").await;

            println!("\n💡 Now test the same operations via HTTP, WebSocket, or OSC!");
            println!(
                "   All protocols will reflect these changes because they share the same state."
            );
        }
    });

    // Step 5: Wait for any server to complete (they run indefinitely)
    tokio::select! {
        _ = http_task => println!("🌐 HTTP server stopped"),
        _ = websocket_task => println!("🔌 WebSocket server stopped"),
        _ = osc_task => println!("📡 OSC server stopped"),
        _ = tokio::signal::ctrl_c() => {
            println!("\n🛑 Shutdown signal received");
        }
    }

    println!("✅ Protocol integration example completed");
    Ok(())
}

async fn print_transport_state(transport: &Arc<Mutex<MockTransportService>>, label: &str) {
    let t = transport.lock().await;
    match t.get_transport() {
        Ok(state) => {
            println!("\n🎼 {} Transport State:", label);
            println!("   ▶️  Play State: {:?}", state.play_state);
            println!("   🎵 Tempo: {:.1} BPM", state.tempo.bpm);
            println!(
                "   🎶 Time Signature: {}/{}",
                state.time_signature.numerator, state.time_signature.denominator
            );
            println!(
                "   📍 Position: {:.2}s",
                state.playhead_position.time.to_seconds()
            );
            println!("   🔴 Record Mode: {:?}", state.record_mode);
        }
        Err(e) => println!("❌ Error getting transport state: {}", e),
    }
}

fn print_usage_examples() {
    println!("\n🎯 All servers are running! Test the shared state integration:");
    println!();
    println!("┌─────────────────────────────────────────────────────────────┐");
    println!("│ 🌐 HTTP REST API (Port 3000)                              │");
    println!("├─────────────────────────────────────────────────────────────┤");
    println!("│ curl -X POST http://localhost:3000/play                    │");
    println!("│ curl -X POST http://localhost:3000/set_tempo \\             │");
    println!("│      -H 'Content-Type: application/json' \\                │");
    println!("│      -d '{{\"bpm\": 140.0}}'                                 │");
    println!("│ curl http://localhost:3000/status                          │");
    println!("├─────────────────────────────────────────────────────────────┤");
    println!("│ 📡 OSC Protocol (UDP Port 9000)                           │");
    println!("├─────────────────────────────────────────────────────────────┤");
    println!("│ oscsend localhost 9000 /transport/play                     │");
    println!("│ oscsend localhost 9000 /transport/tempo f 140.0            │");
    println!("│ oscsend localhost 9000 /transport/status                   │");
    println!("├─────────────────────────────────────────────────────────────┤");
    println!("│ 🔌 WebSocket (Port 3001)                                  │");
    println!("├─────────────────────────────────────────────────────────────┤");
    println!("│ # In browser console:                                      │");
    println!("│ const ws = new WebSocket('ws://localhost:3001/ws');        │");
    println!("│ ws.onmessage = (e) => console.log('Received:', e.data);    │");
    println!("│ ws.send(JSON.stringify({{command: 'play'}}));              │");
    println!("│ ws.send(JSON.stringify({{command: 'status'}}));            │");
    println!("└─────────────────────────────────────────────────────────────┘");
    println!();
    println!("🔍 Key Integration Points:");
    println!("   • All protocols share the same transport service instance");
    println!("   • Changes made through one protocol are visible in all others");
    println!("   • Thread-safe access using Arc<Mutex<T>>");
    println!("   • Each protocol runs in its own async task");
    println!("   • Unified error handling across all protocols");
    println!();
    println!("🔧 Press Ctrl+C to stop all servers");
    println!("==============================");
}
