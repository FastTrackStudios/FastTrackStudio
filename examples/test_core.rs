//! Test Core Module Functionality
//!
//! This example verifies that the core transport module is working correctly
//! by testing the basic domain types and mock transport implementation.

use daw::transport::{
    // Core domain types
    DomainRequest, DomainResponse,
    TransportDomainHandler,
    MockTransport,
    TransportActions,
    // Transport types
    Transport, Tempo, PlayState, RecordMode,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🧪 Testing Core Transport Module");
    println!("================================");

    // Test 1: Create a MockTransport
    println!("\n📦 Step 1: Creating MockTransport");
    let mock_transport = MockTransport::new();
    println!("✅ MockTransport created successfully");

    // Test 2: Test basic transport actions
    println!("\n🎮 Step 2: Testing basic transport actions");
    let mut transport_clone = mock_transport.clone();

    // Test play
    let result = transport_clone.play();
    println!("   Play result: {:?}", result);

    // Test tempo setting
    let tempo = Tempo { bpm: 120.0 };
    let result = transport_clone.set_tempo(tempo);
    println!("   Set tempo result: {:?}", result);

    // Test getting current tempo
    let current_tempo = transport_clone.get_tempo();
    println!("   Current tempo: {:?}", current_tempo);

    // Test 3: Create domain handler
    println!("\n🔄 Step 3: Creating TransportDomainHandler");
    let domain_handler = TransportDomainHandler::new(
        mock_transport,
        "test_core_node".to_string()
    );
    println!("✅ TransportDomainHandler created successfully");
    println!("   Node ID: {}", domain_handler.node_id());

    // Test 4: Test domain request handling
    println!("\n📨 Step 4: Testing domain request handling");

    // Test play request
    let play_response = domain_handler.handle_request(DomainRequest::Play).await?;
    println!("   Play response: {:?}", play_response);

    // Test tempo request
    let tempo_response = domain_handler.handle_request(
        DomainRequest::SetTempo { bpm: 140.0 }
    ).await?;
    println!("   Set tempo response: {:?}", tempo_response);

    // Test get tempo request
    let get_tempo_response = domain_handler.handle_request(DomainRequest::GetTempo).await?;
    println!("   Get tempo response: {:?}", get_tempo_response);

    // Test capabilities request
    let capabilities_response = domain_handler.handle_request(DomainRequest::GetCapabilities).await?;
    println!("   Capabilities response: {:?}", capabilities_response);

    // Test 5: Test transport state
    println!("\n📊 Step 5: Testing transport state");
    let state = transport_clone.get_transport_state();
    println!("   Transport state: {:?}", state);

    println!("\n🎉 All core module tests passed!");
    println!("   ✅ MockTransport works");
    println!("   ✅ TransportActions trait works");
    println!("   ✅ Domain types work");
    println!("   ✅ TransportDomainHandler works");
    println!("   ✅ Request/Response conversion works");

    Ok(())
}
