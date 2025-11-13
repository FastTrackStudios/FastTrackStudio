//! OSC Transport Infrastructure Adapter
//!
//! This module implements an OSC (Open Sound Control) server for transport operations.
//! It provides a UDP-based OSC server that can receive OSC messages and translate
//! them to transport actions following the same pattern as the HTTP adapter.
//!
//! ## OSC Address Patterns
//!
//! ### Transport Control
//! - `/transport/play` - Start playback
//! - `/transport/pause` - Pause playback
//! - `/transport/stop` - Stop playback
//! - `/transport/play_pause` - Toggle play/pause
//! - `/transport/play_stop` - Toggle play/stop
//!
//! ### Recording Control
//! - `/transport/record/start` - Start recording
//! - `/transport/record/stop` - Stop recording
//! - `/transport/record/toggle` - Toggle recording
//!
//! ### Configuration
//! - `/transport/tempo` [float bpm] - Set tempo
//! - `/transport/time_signature` [int numerator, int denominator] - Set time signature
//! - `/transport/position` [float seconds] - Set position
//! - `/transport/record_mode` [string mode] - Set record mode ("normal", "time_selection", "item")
//!
//! ### Queries (with response)
//! - `/transport/status` - Get complete status
//! - `/transport/is_playing` - Get playing state
//! - `/transport/is_recording` - Get recording state
//! - `/transport/get_tempo` - Get current tempo
//! - `/transport/get_position` - Get current position

use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::net::UdpSocket;
use rosc::{OscPacket, OscMessage, OscType, encoder, decoder};
use crate::core::{TransportActions, Tempo, RecordMode};
use primitives::TimeSignature;

/// OSC Server for Transport operations
pub struct OscTransportServer<T>
where
    T: TransportActions + Send + Sync + 'static,
{
    transport: Arc<Mutex<T>>,
    socket: UdpSocket,
    response_addr: Option<std::net::SocketAddr>,
}

impl<T> OscTransportServer<T>
where
    T: TransportActions + Send + Sync + 'static,
{
    /// Create a new OSC transport server
    pub async fn new(transport: Arc<Mutex<T>>, bind_addr: &str) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        let socket = UdpSocket::bind(bind_addr).await?;
        println!("OSC Transport Server listening on {}", bind_addr);

        Ok(Self {
            transport,
            socket,
            response_addr: None,
        })
    }

    /// Start the OSC server and listen for messages
    pub async fn run(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let mut buf = [0u8; 1024];

        loop {
            match self.socket.recv_from(&mut buf).await {
                Ok((size, addr)) => {
                    self.response_addr = Some(addr);

                    // Parse OSC packet
                    match decoder::decode_udp(&buf[..size]) {
                        Ok((_, packet)) => {
                            if let Err(e) = self.handle_osc_packet(packet).await {
                                eprintln!("Error handling OSC packet: {}", e);
                                self.send_error_response(&format!("Error: {}", e)).await;
                            }
                        }
                        Err(e) => {
                            eprintln!("Failed to decode OSC packet: {}", e);
                            self.send_error_response("Invalid OSC packet").await;
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Error receiving UDP packet: {}", e);
                }
            }
        }
    }

    /// Handle an incoming OSC packet
    async fn handle_osc_packet(&mut self, packet: OscPacket) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        match packet {
            OscPacket::Message(msg) => {
                self.handle_osc_message(msg).await?;
            }
            OscPacket::Bundle(bundle) => {
                // Handle bundles by processing each contained packet
                for packet in bundle.content {
                    Box::pin(self.handle_osc_packet(packet)).await?;
                }
            }
        }
        Ok(())
    }

    /// Handle an individual OSC message
    async fn handle_osc_message(&mut self, msg: OscMessage) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let addr = msg.addr.as_str();

        match addr {
            // Transport control
            "/transport/play" => self.handle_play().await,
            "/transport/pause" => self.handle_pause().await,
            "/transport/stop" => self.handle_stop().await,
            "/transport/play_pause" => self.handle_play_pause().await,
            "/transport/play_stop" => self.handle_play_stop().await,

            // Recording control
            "/transport/record/start" => self.handle_start_recording().await,
            "/transport/record/stop" => self.handle_stop_recording().await,
            "/transport/record/toggle" => self.handle_toggle_recording().await,

            // Configuration
            "/transport/tempo" => self.handle_set_tempo(&msg.args).await,
            "/transport/time_signature" => self.handle_set_time_signature(&msg.args).await,
            "/transport/position" => self.handle_set_position(&msg.args).await,
            "/transport/record_mode" => self.handle_set_record_mode(&msg.args).await,

            // Queries
            "/transport/status" => self.handle_get_status().await,
            "/transport/is_playing" => self.handle_is_playing().await,
            "/transport/is_recording" => self.handle_is_recording().await,
            "/transport/get_tempo" => self.handle_get_tempo().await,
            "/transport/get_position" => self.handle_get_position().await,
            "/transport/get_time_signature" => self.handle_get_time_signature().await,
            "/transport/get_record_mode" => self.handle_get_record_mode().await,
            "/transport/is_ready" => self.handle_is_ready().await,

            _ => {
                self.send_error_response(&format!("Unknown OSC address: {}", addr)).await;
                Ok(())
            }
        }
    }

    // Transport control handlers
    async fn handle_play(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let mut transport = self.transport.lock().await;
            transport.play()
        };
        match result {
            Ok(message) => self.send_success_response(&message).await,
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    async fn handle_pause(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let mut transport = self.transport.lock().await;
            transport.pause()
        };
        match result {
            Ok(message) => self.send_success_response(&message).await,
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    async fn handle_stop(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let mut transport = self.transport.lock().await;
            transport.stop()
        };
        match result {
            Ok(message) => self.send_success_response(&message).await,
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    async fn handle_play_pause(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let mut transport = self.transport.lock().await;
            transport.play_pause()
        };
        match result {
            Ok(message) => self.send_success_response(&message).await,
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    async fn handle_play_stop(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let mut transport = self.transport.lock().await;
            transport.play_stop()
        };
        match result {
            Ok(message) => self.send_success_response(&message).await,
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    // Recording control handlers
    async fn handle_start_recording(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let mut transport = self.transport.lock().await;
            transport.start_recording()
        };
        match result {
            Ok(message) => self.send_success_response(&message).await,
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    async fn handle_stop_recording(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let mut transport = self.transport.lock().await;
            transport.stop_recording()
        };
        match result {
            Ok(message) => self.send_success_response(&message).await,
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    async fn handle_toggle_recording(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let mut transport = self.transport.lock().await;
            transport.toggle_recording()
        };
        match result {
            Ok(message) => self.send_success_response(&message).await,
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    // Configuration handlers
    async fn handle_set_tempo(&mut self, args: &[OscType]) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        if let Some(OscType::Float(bpm)) = args.first() {
            let result = {
                let mut transport = self.transport.lock().await;
                let tempo = Tempo::new(*bpm as f64);
                transport.set_tempo(tempo)
            };
            match result {
                Ok(message) => self.send_success_response(&message).await,
                Err(e) => self.send_error_response(&e.to_string()).await,
            }
        } else if let Some(OscType::Double(bpm)) = args.first() {
            let result = {
                let mut transport = self.transport.lock().await;
                let tempo = Tempo::new(*bpm);
                transport.set_tempo(tempo)
            };
            match result {
                Ok(message) => self.send_success_response(&message).await,
                Err(e) => self.send_error_response(&e.to_string()).await,
            }
        } else {
            self.send_error_response("Invalid tempo argument. Expected float BPM value.").await;
        }
        Ok(())
    }

    async fn handle_set_time_signature(&mut self, args: &[OscType]) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        if args.len() >= 2 {
            let numerator = match &args[0] {
                OscType::Int(n) => *n,
                _ => {
                    self.send_error_response("Invalid numerator argument. Expected integer.").await;
                    return Ok(());
                }
            };

            let denominator = match &args[1] {
                OscType::Int(d) => *d,
                _ => {
                    self.send_error_response("Invalid denominator argument. Expected integer.").await;
                    return Ok(());
                }
            };

            let result = {
                let mut transport = self.transport.lock().await;
                let time_sig = TimeSignature::new(numerator, denominator);
                transport.set_time_signature(time_sig)
            };
            match result {
                Ok(message) => self.send_success_response(&message).await,
                Err(e) => self.send_error_response(&e.to_string()).await,
            }
        } else {
            self.send_error_response("Invalid time signature arguments. Expected [int numerator, int denominator].").await;
        }
        Ok(())
    }

    async fn handle_set_position(&mut self, args: &[OscType]) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        if let Some(seconds) = args.first() {
            let seconds_val = match seconds {
                OscType::Float(s) => *s as f64,
                OscType::Double(s) => *s,
                _ => {
                    self.send_error_response("Invalid position argument. Expected float seconds value.").await;
                    return Ok(());
                }
            };

            let result = {
                let mut transport = self.transport.lock().await;
                transport.set_position(seconds_val)
            };
            match result {
                Ok(message) => self.send_success_response(&message).await,
                Err(e) => self.send_error_response(&e.to_string()).await,
            }
        } else {
            self.send_error_response("Missing position argument. Expected float seconds value.").await;
        }
        Ok(())
    }

    async fn handle_set_record_mode(&mut self, args: &[OscType]) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        if let Some(OscType::String(mode_str)) = args.first() {
            let record_mode = match mode_str.to_lowercase().as_str() {
                "normal" => RecordMode::Normal,
                "time_selection" => RecordMode::TimeSelection,
                "item" => RecordMode::Item,
                _ => {
                    self.send_error_response("Invalid record mode. Valid options: normal, time_selection, item").await;
                    return Ok(());
                }
            };

            let result = {
                let mut transport = self.transport.lock().await;
                transport.set_record_mode(record_mode)
            };
            match result {
                Ok(message) => self.send_success_response(&message).await,
                Err(e) => self.send_error_response(&e.to_string()).await,
            }
        } else {
            self.send_error_response("Invalid record mode argument. Expected string.").await;
        }
        Ok(())
    }

    // Query handlers
    async fn handle_get_status(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let status_result = {
            let transport = self.transport.lock().await;
            (
                transport.is_playing(),
                transport.is_recording(),
                transport.get_tempo(),
                transport.get_position(),
                transport.get_time_signature(),
                transport.get_record_mode(),
            )
        };

        match status_result {
            (Ok(is_playing), Ok(is_recording), Ok(tempo), Ok(position), Ok(time_sig), Ok(record_mode)) => {
                let record_mode_str = match record_mode {
                    RecordMode::Normal => "normal",
                    RecordMode::TimeSelection => "time_selection",
                    RecordMode::Item => "item",
                };

                let msg = OscMessage {
                    addr: "/transport/status/response".to_string(),
                    args: vec![
                        OscType::Bool(is_playing),
                        OscType::Bool(is_recording),
                        OscType::Double(tempo.bpm),
                        OscType::Double(position),
                        OscType::Int(time_sig.numerator),
                        OscType::Int(time_sig.denominator),
                        OscType::String(record_mode_str.to_string()),
                    ],
                };
                self.send_osc_message(msg).await;
            }
            _ => {
                self.send_error_response("Failed to get transport status").await;
            }
        }
        Ok(())
    }

    async fn handle_is_playing(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let transport = self.transport.lock().await;
            transport.is_playing()
        };
        match result {
            Ok(is_playing) => {
                let msg = OscMessage {
                    addr: "/transport/is_playing/response".to_string(),
                    args: vec![OscType::Bool(is_playing)],
                };
                self.send_osc_message(msg).await;
            }
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    async fn handle_is_recording(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let transport = self.transport.lock().await;
            transport.is_recording()
        };
        match result {
            Ok(is_recording) => {
                let msg = OscMessage {
                    addr: "/transport/is_recording/response".to_string(),
                    args: vec![OscType::Bool(is_recording)],
                };
                self.send_osc_message(msg).await;
            }
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    async fn handle_get_tempo(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let transport = self.transport.lock().await;
            transport.get_tempo()
        };
        match result {
            Ok(tempo) => {
                let msg = OscMessage {
                    addr: "/transport/tempo/response".to_string(),
                    args: vec![OscType::Double(tempo.bpm)],
                };
                self.send_osc_message(msg).await;
            }
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    async fn handle_get_position(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let transport = self.transport.lock().await;
            transport.get_position()
        };
        match result {
            Ok(position) => {
                let msg = OscMessage {
                    addr: "/transport/position/response".to_string(),
                    args: vec![OscType::Double(position)],
                };
                self.send_osc_message(msg).await;
            }
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    async fn handle_get_time_signature(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let transport = self.transport.lock().await;
            transport.get_time_signature()
        };
        match result {
            Ok(time_sig) => {
                let msg = OscMessage {
                    addr: "/transport/time_signature/response".to_string(),
                    args: vec![
                        OscType::Int(time_sig.numerator),
                        OscType::Int(time_sig.denominator),
                    ],
                };
                self.send_osc_message(msg).await;
            }
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    async fn handle_get_record_mode(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let transport = self.transport.lock().await;
            transport.get_record_mode()
        };
        match result {
            Ok(record_mode) => {
                let mode_str = match record_mode {
                    RecordMode::Normal => "normal",
                    RecordMode::TimeSelection => "time_selection",
                    RecordMode::Item => "item",
                };

                let msg = OscMessage {
                    addr: "/transport/record_mode/response".to_string(),
                    args: vec![OscType::String(mode_str.to_string())],
                };
                self.send_osc_message(msg).await;
            }
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    async fn handle_is_ready(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let result = {
            let transport = self.transport.lock().await;
            transport.is_ready()
        };
        match result {
            Ok(is_ready) => {
                let msg = OscMessage {
                    addr: "/transport/is_ready/response".to_string(),
                    args: vec![OscType::Bool(is_ready)],
                };
                self.send_osc_message(msg).await;
            }
            Err(e) => self.send_error_response(&e.to_string()).await,
        }
        Ok(())
    }

    // Response helpers
    async fn send_success_response(&mut self, message: &str) {
        let msg = OscMessage {
            addr: "/transport/response".to_string(),
            args: vec![
                OscType::String("success".to_string()),
                OscType::String(message.to_string()),
            ],
        };
        self.send_osc_message(msg).await;
    }

    async fn send_error_response(&mut self, error: &str) {
        let msg = OscMessage {
            addr: "/transport/error".to_string(),
            args: vec![OscType::String(error.to_string())],
        };
        self.send_osc_message(msg).await;
    }

    async fn send_osc_message(&mut self, msg: OscMessage) {
        if let Some(addr) = self.response_addr {
            let packet = OscPacket::Message(msg);
            match encoder::encode(&packet) {
                Ok(encoded) => {
                    if let Err(e) = self.socket.send_to(&encoded, addr).await {
                        eprintln!("Failed to send OSC response: {}", e);
                    }
                }
                Err(e) => {
                    eprintln!("Failed to encode OSC message: {}", e);
                }
            }
        }
    }
}

/// Convenience function to create and run an OSC transport server
pub async fn create_transport_osc_server<T>(
    transport: Arc<Mutex<T>>,
    bind_addr: &str,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>>
where
    T: TransportActions + Send + Sync + 'static,
{
    let mut server = OscTransportServer::new(transport, bind_addr).await?;
    server.run().await
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Transport;
    use std::time::Duration;
    use tokio::time::timeout;

    #[tokio::test]
    async fn test_osc_server_creation() {
        let transport = Arc::new(Mutex::new(Transport::new()));
        let server = OscTransportServer::new(transport, "127.0.0.1:0").await;
        assert!(server.is_ok());
    }

    // Note: More comprehensive tests would require an OSC client to send test messages
    // This would be a good place to add integration tests with actual OSC communication
}
