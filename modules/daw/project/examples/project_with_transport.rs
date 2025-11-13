//! Example demonstrating how a project depends on transport functionality
//!
//! This shows the clean separation of concerns where:
//! - Transport provides the core transport actions trait
//! - Project composes transport functionality via trait bounds
//! - The example implements both traits on a concrete type

use project::{ProjectActions, create_project_http_router};
use transport::{Transport, TransportActions, TransportError, Tempo, RecordMode};
use primitives::TimeSignature;
use std::sync::Arc;
use tokio::sync::Mutex;

/// A concrete DAW project that implements both project and transport actions
#[derive(Clone)]
struct DawProject {
    name: String,
    transport: Transport,
}

impl DawProject {
    fn new(name: String) -> Self {
        Self {
            name,
            transport: Transport::new(),
        }
    }
}

// Implement project actions
impl ProjectActions for DawProject {
    fn get_name(&self) -> &str {
        &self.name
    }

    fn set_name(&mut self, name: String) {
        self.name = name;
    }
}

// Implement transport actions by delegating to the transport component
impl TransportActions for DawProject {
    fn play(&mut self) -> Result<String, TransportError> {
        self.transport.play()
    }

    fn pause(&mut self) -> Result<String, TransportError> {
        self.transport.pause()
    }

    fn stop(&mut self) -> Result<String, TransportError> {
        self.transport.stop()
    }

    fn play_pause(&mut self) -> Result<String, TransportError> {
        self.transport.play_pause()
    }

    fn play_stop(&mut self) -> Result<String, TransportError> {
        self.transport.play_stop()
    }

    fn start_recording(&mut self) -> Result<String, TransportError> {
        self.transport.start_recording()
    }

    fn stop_recording(&mut self) -> Result<String, TransportError> {
        self.transport.stop_recording()
    }

    fn toggle_recording(&mut self) -> Result<String, TransportError> {
        self.transport.toggle_recording()
    }

    fn set_tempo(&mut self, tempo: Tempo) -> Result<String, TransportError> {
        match self.transport.set_tempo(tempo) {
            Ok(()) => Ok(format!("Tempo set to {} BPM", tempo.bpm)),
            Err(e) => Err(TransportError::InvalidTempo(e)),
        }
    }

    fn set_time_signature(&mut self, time_signature: TimeSignature) -> Result<String, TransportError> {
        self.transport.time_signature = time_signature;
        Ok(format!("Time signature set to {}/{}", time_signature.numerator, time_signature.denominator))
    }

    fn set_record_mode(&mut self, record_mode: RecordMode) -> Result<String, TransportError> {
        self.transport.record_mode = record_mode;
        Ok(format!("Record mode set to {:?}", record_mode))
    }

    fn set_position(&mut self, seconds: f64) -> Result<String, TransportError> {
        use primitives::Position;
        self.transport.playhead_position = Position::from_seconds(seconds);
        Ok(format!("Position set to {} seconds", seconds))
    }

    fn get_tempo(&self) -> Result<Tempo, TransportError> {
        Ok(self.transport.tempo)
    }

    fn get_time_signature(&self) -> Result<TimeSignature, TransportError> {
        Ok(self.transport.time_signature)
    }

    fn get_record_mode(&self) -> Result<RecordMode, TransportError> {
        Ok(self.transport.record_mode)
    }

    fn get_position(&self) -> Result<f64, TransportError> {
        Ok(self.transport.playhead_position.time.to_seconds())
    }

    fn is_playing(&self) -> Result<bool, TransportError> {
        Ok(self.transport.is_playing())
    }

    fn is_recording(&self) -> Result<bool, TransportError> {
        Ok(self.transport.is_recording())
    }

    fn get_transport(&self) -> Result<Transport, TransportError> {
        Ok(self.transport.clone())
    }

    fn is_ready(&self) -> Result<bool, TransportError> {
        Ok(true)
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🎵 DAW Project with Transport Example");
    println!("=====================================");

    // Create a project instance
    let project = DawProject::new("My Awesome Song".to_string());

    // Demonstrate project functionality
    println!("📁 Project name: {}", project.get_name());
    println!("🎮 Transport playing: {}", project.is_playing()?);
    println!("⏺️  Transport recording: {}", project.is_recording()?);
    println!("🎵 Current tempo: {} BPM", project.get_tempo()?.bpm);

    // Wrap in Arc<Mutex<_>> for shared state
    let project_state = Arc::new(Mutex::new(project));

    // Create the project HTTP router - this demonstrates the dependency relationship
    // The router requires both ProjectActions AND TransportActions to be implemented
    let app = create_project_http_router::<DawProject>()
        .with_state(project_state.clone());

    println!("\n🌐 Starting HTTP server on http://localhost:3001");
    println!("Available endpoints:");
    println!("  GET  /info                    - Project information");
    println!("  GET  /transport/status        - Complete transport status");
    println!("  GET  /transport/is_playing    - Transport playback state");
    println!("  GET  /transport/is_recording  - Transport recording state");
    println!("  GET  /transport/tempo         - Current tempo");
    println!("  GET  /transport/position      - Current position");
    println!("  POST /transport/play          - Start playback");
    println!("  POST /transport/pause         - Pause playback");
    println!("  POST /transport/stop          - Stop playback");
    println!("  POST /transport/start_recording - Start recording");
    println!("  POST /transport/set_tempo     - Set tempo (JSON: {{\"bpm\": 140.0}})");
    println!("  POST /transport/set_position  - Set position (JSON: {{\"seconds\": 30.0}})");
    println!("\nPress Ctrl+C to shutdown");

    // Start the server
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3001").await?;

    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await?;

    println!("Server shutdown complete");
    Ok(())
}

async fn shutdown_signal() {
    tokio::signal::ctrl_c()
        .await
        .expect("Failed to install CTRL+C signal handler");
    println!("\nShutdown signal received, shutting down gracefully...");
}
