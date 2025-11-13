//! Core and Infrastructure Separation Example
//!
//! This example demonstrates the clean separation between core domain logic
//! and infrastructure adapters in the transport module.

use transport::{
    // Core domain types and traits
    core::{Transport, TransportActions, Tempo},
    // Infrastructure HTTP adapter
    infra::create_transport_http_router,
};
use std::sync::Arc;
use tokio::sync::Mutex;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 Transport Core/Infrastructure Separation Example");
    println!("==================================================");

    // 1. Create core domain instance
    println!("\n📦 CREATING CORE DOMAIN INSTANCE");
    let mut transport = Transport::new();
    println!("✅ Created Transport with default settings");

    // 2. Test core domain operations directly
    println!("\n🧪 TESTING CORE DOMAIN DIRECTLY");

    // Test playback controls
    let result = transport.play()?;
    println!("🎵 CORE: {}", result);

    // Test configuration
    let new_tempo = Tempo::new(140.0);
    match transport.set_tempo(new_tempo) {
        Ok(()) => println!("🎼 CORE: Tempo set to {} BPM", new_tempo.bpm),
        Err(e) => println!("🎼 CORE: Failed to set tempo: {}", e),
    }

    // Test queries
    let current_tempo = transport.get_tempo()?;
    println!("📊 CORE: Current tempo is {} BPM", current_tempo.bpm);

    let is_playing = transport.is_playing();
    println!("📊 CORE: Is playing: {}", is_playing);

    println!("✅ Direct core operations completed successfully");

    // 3. Create infrastructure HTTP adapter
    println!("\n🌐 CREATING HTTP INFRASTRUCTURE ADAPTER");
    let transport_state = Arc::new(Mutex::new(transport));

    let app = axum::Router::new()
        .nest("/transport", create_transport_http_router::<Transport>())
        .with_state(transport_state.clone());

    println!("✅ Created HTTP router that adapts Transport core domain");

    // 4. Demonstrate the separation
    println!("\n🏗️  ARCHITECTURE DEMONSTRATION");
    println!("   ┌─────────────────────────────────────┐");
    println!("   │          INFRASTRUCTURE             │");
    println!("   │  ┌─────────────────────────────────┐ │");
    println!("   │  │         HTTP Adapter            │ │");
    println!("   │  │  POST /transport/play           │ │");
    println!("   │  │  GET  /transport/tempo          │ │");
    println!("   │  │  POST /transport/set_tempo      │ │");
    println!("   │  └─────────────────────────────────┘ │");
    println!("   └─────────────┬───────────────────────┘");
    println!("                 │ Adapter Pattern");
    println!("   ┌─────────────▼───────────────────────┐");
    println!("   │           CORE DOMAIN               │");
    println!("   │  ┌─────────────────────────────────┐ │");
    println!("   │  │      TransportActions           │ │");
    println!("   │  │  play(), pause(), stop()        │ │");
    println!("   │  │  set_tempo(), get_tempo()       │ │");
    println!("   │  │  Transport, Tempo, PlayState    │ │");
    println!("   │  └─────────────────────────────────┘ │");
    println!("   └─────────────────────────────────────┘");

    // 5. Start HTTP server
    println!("\n🚀 STARTING HTTP SERVER");
    println!("Server running on http://localhost:3002");
    println!();
    println!("🔧 Available Infrastructure Endpoints:");
    println!("   Core Domain ────→ HTTP Infrastructure");
    println!("   GET  /transport/status        ← Full transport state");
    println!("   POST /transport/play          ← core.play()");
    println!("   POST /transport/pause         ← core.pause()");
    println!("   POST /transport/stop          ← core.stop()");
    println!("   POST /transport/set_tempo     ← core.set_tempo()");
    println!("   GET  /transport/tempo         ← core.get_tempo()");
    println!("   GET  /transport/is_playing    ← core.is_playing()");
    println!();
    println!("🧪 Test with curl:");
    println!("   curl -X POST http://localhost:3002/transport/play");
    println!("   curl http://localhost:3002/transport/status");
    println!("   curl -X POST http://localhost:3002/transport/set_tempo \\");
    println!("     -H 'Content-Type: application/json' \\");
    println!("     -d '{{\"bpm\": 128.0}}'");
    println!();
    println!("✨ Benefits of this architecture:");
    println!("   • Core domain is protocol-agnostic");
    println!("   • Can add OSC, WebSocket, gRPC adapters easily");
    println!("   • Core logic is testable without HTTP");
    println!("   • Infrastructure can be swapped without changing core");
    println!();
    println!("Press Ctrl+C to shutdown");

    // Start server
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3002").await?;
    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await?;

    println!("🛑 Server shutdown complete");
    Ok(())
}

async fn shutdown_signal() {
    tokio::signal::ctrl_c()
        .await
        .expect("Failed to install CTRL+C signal handler");
    println!("\n🛑 Shutdown signal received, shutting down gracefully...");
}
