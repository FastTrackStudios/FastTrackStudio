//! OSC Transport Server Example
//!
//! This example demonstrates how to create and run an OSC server for transport operations.
//! It creates a mock transport instance and serves it over OSC on UDP port 9000.
//!
//! ## Usage
//!
//! Run the server:
//! ```bash
//! cargo run --example osc_server
//! ```
//!
//! Then send OSC messages to control transport:
//!
//! ### Transport Control
//! ```bash
//! # Start playback
//! oscsend localhost 9000 /transport/play
//!
//! # Pause playback
//! oscsend localhost 9000 /transport/pause
//!
//! # Stop playback
//! oscsend localhost 9000 /transport/stop
//!
//! # Toggle play/pause
//! oscsend localhost 9000 /transport/play_pause
//! ```
//!
//! ### Recording Control
//! ```bash
//! # Start recording
//! oscsend localhost 9000 /transport/record/start
//!
//! # Stop recording
//! oscsend localhost 9000 /transport/record/stop
//!
//! # Toggle recording
//! oscsend localhost 9000 /transport/record/toggle
//! ```
//!
//! ### Configuration
//! ```bash
//! # Set tempo to 140 BPM
//! oscsend localhost 9000 /transport/tempo f 140.0
//!
//! # Set time signature to 3/4
//! oscsend localhost 9000 /transport/time_signature i 3 i 4
//!
//! # Set position to 30.5 seconds
//! oscsend localhost 9000 /transport/position f 30.5
//!
//! # Set record mode to time selection
//! oscsend localhost 9000 /transport/record_mode s time_selection
//! ```
//!
//! ### Queries
//! ```bash
//! # Get complete status
//! oscsend localhost 9000 /transport/status
//!
//! # Check if playing
//! oscsend localhost 9000 /transport/is_playing
//!
//! # Get current tempo
//! oscsend localhost 9000 /transport/get_tempo
//! ```

use std::sync::Arc;
use tokio::sync::Mutex;
use transport::{
    application::mock::MockTransportService,
    infra::osc::create_transport_osc_server,
    TransportActions,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    println!("🎵 Starting OSC Transport Server Example");
    println!("=========================================");

    // Create a mock transport service
    println!("📦 Creating mock transport service...");
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
    println!("🚀 Starting OSC server on 0.0.0.0:9000");
    println!("📡 Listening for OSC messages...");
    println!();
    println!("📋 Available OSC addresses:");
    println!("   Transport Control:");
    println!("   - /transport/play");
    println!("   - /transport/pause");
    println!("   - /transport/stop");
    println!("   - /transport/play_pause");
    println!("   - /transport/play_stop");
    println!();
    println!("   Recording Control:");
    println!("   - /transport/record/start");
    println!("   - /transport/record/stop");
    println!("   - /transport/record/toggle");
    println!();
    println!("   Configuration:");
    println!("   - /transport/tempo [float bpm]");
    println!("   - /transport/time_signature [int num] [int den]");
    println!("   - /transport/position [float seconds]");
    println!("   - /transport/record_mode [string mode]");
    println!();
    println!("   Queries:");
    println!("   - /transport/status");
    println!("   - /transport/is_playing");
    println!("   - /transport/is_recording");
    println!("   - /transport/get_tempo");
    println!("   - /transport/get_position");
    println!("   - /transport/get_time_signature");
    println!("   - /transport/get_record_mode");
    println!("   - /transport/is_ready");
    println!();
    println!("💡 Example commands (using oscsend):");
    println!("   oscsend localhost 9000 /transport/play");
    println!("   oscsend localhost 9000 /transport/tempo f 140.0");
    println!("   oscsend localhost 9000 /transport/time_signature i 3 i 4");
    println!("   oscsend localhost 9000 /transport/status");
    println!();
    println!("🔧 Press Ctrl+C to stop the server");
    println!("==========================================");

    // Start the OSC server
    match create_transport_osc_server(transport, "0.0.0.0:9000").await {
        Ok(()) => {
            println!("✅ OSC server stopped normally");
        }
        Err(e) => {
            eprintln!("❌ OSC server error: {}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}
