//! OSC Client Test Example
//!
//! This example demonstrates how to send OSC messages to the transport server
//! and receive responses. It can be used to test the OSC server implementation.
//!
//! ## Usage
//!
//! First, start the OSC server in another terminal:
//! ```bash
//! cargo run --example osc_server
//! ```
//!
//! Then run this client test:
//! ```bash
//! cargo run --example test_osc_client
//! ```
//!
//! This will send a series of OSC commands to test various transport functions.

use std::net::UdpSocket;
use std::time::Duration;
use rosc::{encoder, decoder, OscPacket, OscMessage, OscType};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🎵 OSC Transport Client Test");
    println!("============================");

    // Create UDP socket for sending/receiving OSC messages
    let socket = UdpSocket::bind("127.0.0.1:0")?;
    socket.set_read_timeout(Some(Duration::from_millis(1000)))?;

    let server_addr = "127.0.0.1:9000";
    println!("📡 Connecting to OSC server at {}", server_addr);

    // Test basic transport control
    println!("\n🎮 Testing Transport Control:");

    // Test play command
    send_osc_command(&socket, server_addr, "/transport/play", vec![]).await?;
    wait_for_response(&socket, "play").await;

    // Test pause command
    send_osc_command(&socket, server_addr, "/transport/pause", vec![]).await?;
    wait_for_response(&socket, "pause").await;

    // Test stop command
    send_osc_command(&socket, server_addr, "/transport/stop", vec![]).await?;
    wait_for_response(&socket, "stop").await;

    // Test tempo setting
    println!("\n🎵 Testing Tempo Control:");
    send_osc_command(&socket, server_addr, "/transport/tempo", vec![OscType::Float(140.0)]).await?;
    wait_for_response(&socket, "set tempo").await;

    // Test time signature setting
    println!("\n🎼 Testing Time Signature:");
    send_osc_command(&socket, server_addr, "/transport/time_signature", vec![
        OscType::Int(3),
        OscType::Int(4),
    ]).await?;
    wait_for_response(&socket, "set time signature").await;

    // Test position setting
    println!("\n⏱️  Testing Position Control:");
    send_osc_command(&socket, server_addr, "/transport/position", vec![OscType::Float(30.5)]).await?;
    wait_for_response(&socket, "set position").await;

    // Test record mode setting
    println!("\n🔴 Testing Record Mode:");
    send_osc_command(&socket, server_addr, "/transport/record_mode", vec![
        OscType::String("time_selection".to_string())
    ]).await?;
    wait_for_response(&socket, "set record mode").await;

    // Test recording control
    println!("\n📹 Testing Recording Control:");
    send_osc_command(&socket, server_addr, "/transport/record/start", vec![]).await?;
    wait_for_response(&socket, "start recording").await;

    send_osc_command(&socket, server_addr, "/transport/record/stop", vec![]).await?;
    wait_for_response(&socket, "stop recording").await;

    // Test query commands
    println!("\n❓ Testing Query Commands:");

    // Test status query
    send_osc_command(&socket, server_addr, "/transport/status", vec![]).await?;
    wait_for_status_response(&socket).await;

    // Test individual queries
    send_osc_command(&socket, server_addr, "/transport/is_playing", vec![]).await?;
    wait_for_bool_response(&socket, "is_playing").await;

    send_osc_command(&socket, server_addr, "/transport/get_tempo", vec![]).await?;
    wait_for_tempo_response(&socket).await;

    send_osc_command(&socket, server_addr, "/transport/is_ready", vec![]).await?;
    wait_for_bool_response(&socket, "is_ready").await;

    println!("\n✅ All OSC tests completed successfully!");
    println!("   The transport OSC server is working correctly.");

    Ok(())
}

async fn send_osc_command(
    socket: &UdpSocket,
    server_addr: &str,
    address: &str,
    args: Vec<OscType>,
) -> Result<(), Box<dyn std::error::Error>> {
    let msg = OscMessage {
        addr: address.to_string(),
        args,
    };

    let packet = OscPacket::Message(msg);
    let encoded = encoder::encode(&packet)?;

    socket.send_to(&encoded, server_addr)?;
    println!("   📤 Sent: {}", address);

    Ok(())
}

async fn wait_for_response(socket: &UdpSocket, command_name: &str) {
    match receive_osc_response(socket).await {
        Ok(response) => {
            match response {
                OscPacket::Message(msg) => {
                    if msg.addr == "/transport/response" {
                        if let (Some(OscType::String(status)), Some(OscType::String(message))) =
                            (msg.args.get(0), msg.args.get(1)) {
                            if status == "success" {
                                println!("   ✅ {}: {}", command_name, message);
                            } else {
                                println!("   ❌ {}: {}", command_name, message);
                            }
                        }
                    } else if msg.addr == "/transport/error" {
                        if let Some(OscType::String(error)) = msg.args.get(0) {
                            println!("   ❌ {}: Error - {}", command_name, error);
                        }
                    } else {
                        println!("   📨 Unexpected response: {}", msg.addr);
                    }
                }
                _ => println!("   📨 Received bundle (unexpected)"),
            }
        }
        Err(_) => println!("   ⏰ {} - No response received (timeout)", command_name),
    }
}

async fn wait_for_status_response(socket: &UdpSocket) {
    match receive_osc_response(socket).await {
        Ok(response) => {
            match response {
                OscPacket::Message(msg) => {
                    if msg.addr == "/transport/status/response" && msg.args.len() >= 7 {
                        if let (
                            Some(OscType::Bool(is_playing)),
                            Some(OscType::Bool(is_recording)),
                            Some(OscType::Double(tempo)),
                            Some(OscType::Double(position)),
                            Some(OscType::Int(time_num)),
                            Some(OscType::Int(time_den)),
                            Some(OscType::String(record_mode)),
                        ) = (
                            msg.args.get(0),
                            msg.args.get(1),
                            msg.args.get(2),
                            msg.args.get(3),
                            msg.args.get(4),
                            msg.args.get(5),
                            msg.args.get(6),
                        ) {
                            println!("   ✅ Status received:");
                            println!("      - Playing: {}", is_playing);
                            println!("      - Recording: {}", is_recording);
                            println!("      - Tempo: {:.1} BPM", tempo);
                            println!("      - Position: {:.2}s", position);
                            println!("      - Time Signature: {}/{}", time_num, time_den);
                            println!("      - Record Mode: {}", record_mode);
                        }
                    } else {
                        println!("   📨 Unexpected status response format");
                    }
                }
                _ => println!("   📨 Received bundle (unexpected)"),
            }
        }
        Err(_) => println!("   ⏰ Status query - No response received (timeout)"),
    }
}

async fn wait_for_bool_response(socket: &UdpSocket, query_name: &str) {
    match receive_osc_response(socket).await {
        Ok(response) => {
            match response {
                OscPacket::Message(msg) => {
                    if let Some(OscType::Bool(value)) = msg.args.get(0) {
                        println!("   ✅ {}: {}", query_name, value);
                    } else {
                        println!("   📨 Unexpected {} response format", query_name);
                    }
                }
                _ => println!("   📨 Received bundle (unexpected)"),
            }
        }
        Err(_) => println!("   ⏰ {} - No response received (timeout)", query_name),
    }
}

async fn wait_for_tempo_response(socket: &UdpSocket) {
    match receive_osc_response(socket).await {
        Ok(response) => {
            match response {
                OscPacket::Message(msg) => {
                    if msg.addr == "/transport/tempo/response" {
                        if let Some(OscType::Double(tempo)) = msg.args.get(0) {
                            println!("   ✅ Current tempo: {:.1} BPM", tempo);
                        } else {
                            println!("   📨 Unexpected tempo response format");
                        }
                    }
                }
                _ => println!("   📨 Received bundle (unexpected)"),
            }
        }
        Err(_) => println!("   ⏰ Get tempo - No response received (timeout)"),
    }
}

async fn receive_osc_response(socket: &UdpSocket) -> Result<OscPacket, Box<dyn std::error::Error>> {
    let mut buf = [0u8; 1024];
    let (size, _) = socket.recv_from(&mut buf)?;
    let (_, packet) = decoder::decode_udp(&buf[..size])?;
    Ok(packet)
}
