//! Simple OSC Integration Example
//!
//! This example demonstrates the complete OSC integration with the universal domain architecture.
//! It shows how OSC messages are converted to domain requests, processed through business logic,
//! and converted back to OSC responses - all while keeping Tower dependencies isolated to the
//! protocol layer only.
//!
//! Run with: cargo run --example simple_osc_example

use daw::transport::{
    MockTransport,
    adapters::osc::{OscService, OscRequest, OscResponse, OscArg, OscSource, DomainHandler},
    TransportDomainHandler,
    TransportRequest, TransportResponse,
};

use std::net::SocketAddr;
use std::time::Duration;
use tower::{Service, ServiceExt};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🎵 Simple OSC Integration Example");
    println!("=================================\n");

    // Step 1: Create business logic (completely Tower-independent)
    println!("📋 Step 1: Creating domain handler with MockTransport");
    let mock_transport = MockTransport::new();
    let domain_handler = TransportDomainHandler::new(
        mock_transport,
        "osc_example_node".to_string()
    );

    println!("   • Node ID: {}", domain_handler.node_id());
    println!("   • Capabilities: {:?}", domain_handler.capabilities());

    // Step 2: Create OSC Tower Service (Tower integration ends here)
    println!("\n🔌 Step 2: Creating OSC Tower Service");
    let mut osc_service = OscService::new(domain_handler);
    println!("   • OSC Service ready to process messages");

    // Step 3: Demonstrate OSC message processing
    println!("\n📡 Step 3: Processing OSC Messages");

    let test_cases = vec![
        ("Play Command", "/transport/play", vec![]),
        ("Set Tempo", "/transport/tempo/set", vec![OscArg::Float(140.0)]),
        ("Get Tempo", "/transport/tempo", vec![]),
        ("Set Position", "/transport/position/set", vec![OscArg::Float(30.5)]),
        ("Get Position", "/transport/position", vec![]),
        ("Get State", "/transport/state", vec![]),
        ("System Ping", "/ping", vec![]),
        ("Get Capabilities", "/capabilities", vec![]),
    ];

    for (description, address, args) in test_cases {
        println!("\n🔄 Processing: {}", description);

        // Create OSC request
        let osc_request = OscRequest::new(address.to_string(), args.clone());
        println!("   📤 OSC In:  {} {:?}", osc_request.address, osc_request.args);

        // Process through Tower service
        let osc_response = osc_service
            .ready()
            .await?
            .call(osc_request)
            .await?;

        println!("   📥 OSC Out: {} {:?}", osc_response.address, osc_response.args);
    }

    // Step 4: Demonstrate error handling
    println!("\n❌ Step 4: Error Handling");

    let error_cases = vec![
        ("Invalid Address", "/invalid/address", vec![]),
        ("Missing Arguments", "/transport/tempo/set", vec![]),
        ("Wrong Argument Type", "/transport/tempo/set", vec![OscArg::String("not_a_number".to_string())]),
    ];

    for (description, address, args) in error_cases {
        println!("\n⚠️  Testing: {}", description);

        let osc_request = OscRequest::new(address.to_string(), args);
        let osc_response = osc_service
            .ready()
            .await?
            .call(osc_request)
            .await?;

        println!("   📥 Error Response: {} {:?}", osc_response.address, osc_response.args);
    }

    // Step 5: Demonstrate workflow sequence
    println!("\n🎬 Step 5: Complete Workflow Sequence");

    let workflow = vec![
        ("Set tempo to 128 BPM", "/transport/tempo/set", vec![OscArg::Float(128.0)]),
        ("Set position to start", "/transport/position/set", vec![OscArg::Float(0.0)]),
        ("Start playback", "/transport/play", vec![]),
        ("Check status", "/transport/state", vec![]),
        ("Pause playback", "/transport/pause", vec![]),
        ("Check status again", "/transport/state", vec![]),
        ("Stop playback", "/transport/stop", vec![]),
    ];

    for (step, address, args) in workflow {
        println!("\n🎯 {}", step);

        let osc_request = OscRequest::new(address.to_string(), args);
        let osc_response = osc_service
            .ready()
            .await?
            .call(osc_request)
            .await?;

        // Add a small delay to simulate real workflow
        tokio::time::sleep(Duration::from_millis(100)).await;

        // Show condensed response
        match osc_response.address.as_str() {
            "/transport/success" => {
                if let Some(OscArg::String(msg)) = osc_response.args.first() {
                    println!("   ✅ {}", msg);
                }
            },
            "/transport/tempo" => {
                if let Some(OscArg::Float(bpm)) = osc_response.args.first() {
                    println!("   🎵 Tempo: {} BPM", bpm);
                }
            },
            "/transport/state" => {
                if let Some(OscArg::String(state)) = osc_response.args.first() {
                    println!("   📊 State: {}", state);
                }
            },
            _ => {
                println!("   📥 {}: {:?}", osc_response.address, osc_response.args);
            }
        }
    }

    // Step 6: Architecture summary
    println!("\n🏗️  Step 6: Architecture Summary");
    println!("================================");
    println!("✅ OSC Protocol Layer (Tower Service):");
    println!("   • Converts OSC messages ↔ DomainRequest/Response");
    println!("   • Handles OSC-specific addressing and type conversion");
    println!("   • Tower integration ENDS here");
    println!();
    println!("✅ Domain Layer (Business Logic):");
    println!("   • Protocol-agnostic request/response types");
    println!("   • Pure business logic - no Tower dependencies");
    println!("   • Maps to TransportActions trait methods");
    println!();
    println!("✅ Application Layer (Device Implementation):");
    println!("   • MockTransport for testing");
    println!("   • Could be ReaperTransport, ProToolsTransport, etc.");
    println!("   • Same logic works with any implementation");

    println!("\n🎉 Example completed successfully!");
    println!("\n🚀 Next steps:");
    println!("   • Add UDP socket I/O for real OSC communication");
    println!("   • Integrate with root routing for multi-device networks");
    println!("   • Add other protocol services (HTTP, gRPC, MIDI)");
    println!("   • Connect to real DAW implementations");

    Ok(())
}

/// Example of how to extend with UDP socket integration
#[allow(dead_code)]
async fn udp_socket_example() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n🌐 UDP Socket Integration Example");
    println!("==================================");

    // This would be a real UDP socket implementation
    use tokio::net::UdpSocket;

    let socket = UdpSocket::bind("127.0.0.1:7000").await?;
    println!("🔌 OSC server listening on 127.0.0.1:7000");

    // Create the same service stack
    let domain_handler = TransportDomainHandler::new(
        MockTransport::new(),
        "udp_osc_node".to_string()
    );
    let mut osc_service = OscService::new(domain_handler);

    println!("📡 Ready to receive OSC messages...");
    println!("   Example: Send to this address using oscsend or similar tool:");
    println!("   oscsend 127.0.0.1 7000 /transport/play");
    println!("   oscsend 127.0.0.1 7000 /transport/tempo/set f 140.0");

    // In a real implementation, this would be an event loop:
    // loop {
    //     let mut buf = [0; 1024];
    //     let (len, addr) = socket.recv_from(&mut buf).await?;
    //
    //     // Parse OSC packet from bytes
    //     let osc_packet = rosc::decoder::decode(&buf[..len])?;
    //
    //     // Convert to OscRequest
    //     let osc_request = osc_packet_to_request(osc_packet, addr);
    //
    //     // Process through service
    //     let osc_response = osc_service.ready().await?.call(osc_request).await?;
    //
    //     // Convert response back to bytes and send
    //     let response_bytes = osc_response_to_bytes(osc_response)?;
    //     socket.send_to(&response_bytes, addr).await?;
    // }

    Ok(())
}

/// Example of multi-protocol integration
#[allow(dead_code)]
async fn multi_protocol_example() {
    println!("\n🔄 Multi-Protocol Integration Vision");
    println!("====================================");
    println!("The same domain handler can be used with multiple protocols:");
    println!();

    // Same domain handler for all protocols
    let domain_handler = TransportDomainHandler::new(
        MockTransport::new(),
        "multi_protocol_node".to_string()
    );

    println!("📡 OSC Service:");
    let _osc_service = OscService::new(domain_handler.clone());
    println!("   • Handles /transport/play → DomainRequest::Play");
    println!("   • Returns OSC messages");
    println!();

    println!("🌐 HTTP Service (future):");
    // let http_service = HttpService::new(domain_handler.clone());
    println!("   • Handles POST /transport/play → DomainRequest::Play");
    println!("   • Returns JSON responses");
    println!();

    println!("📱 gRPC Service (future):");
    // let grpc_service = GrpcService::new(domain_handler.clone());
    println!("   • Handles TransportPlayRequest → DomainRequest::Play");
    println!("   • Returns protobuf responses");
    println!();

    println!("🎹 MIDI Service (future):");
    // let midi_service = MidiService::new(domain_handler.clone());
    println!("   • Handles MIDI CC messages → DomainRequest::Play");
    println!("   • Returns MIDI responses");
    println!();

    println!("✨ All protocols use the SAME business logic!");
    println!("   • Add new protocol = implement one Tower Service");
    println!("   • Business logic never changes");
    println!("   • Device implementations work with any protocol");
}
