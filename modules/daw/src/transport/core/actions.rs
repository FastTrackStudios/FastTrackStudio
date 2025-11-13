//! Transport Actions Trait
//!
//! This module defines the transport actions using the trait-based approach.
//! The #[actions] macro will generate the TransportAction enum automatically.

use crate::primitives::TimeSignature;
use crate::transport::core::transport::{Transport, Tempo, RecordMode};
use proc_macros::actions;
use crate::transport::core::error::TransportError;



/// Transport actions trait with rich domain methods
///
/// This trait defines the complete transport interface. The #[actions] macro
/// will automatically generate:
/// - TransportAction enum with variants for each method
/// - JSON serialization for protocol compatibility
///
/// Uses native async trait methods supported in Rust 1.75+
#[actions]
pub trait TransportActions: Send + Sync {

    // Transport control commands (sync - immediate state changes)

    /// Start playback
    fn play(&mut self) -> Result<String, TransportError>;

    /// Pause playback
    fn pause(&mut self) -> Result<String, TransportError>;

    /// Stop playback and return to edit position
    fn stop(&mut self) -> Result<String, TransportError>;

    /// Toggle between play and pause
    fn play_pause(&mut self) -> Result<String, TransportError>;

    /// Toggle between play and stop
    fn play_stop(&mut self) -> Result<String, TransportError>;

    /// Start recording
    fn start_recording(&mut self) -> Result<String, TransportError>;

    /// Stop recording
    fn stop_recording(&mut self) -> Result<String, TransportError>;

    /// Toggle recording on/off
    fn toggle_recording(&mut self) -> Result<String, TransportError>;

    // Configuration commands (sync - immediate setting changes)

    /// Set the tempo in BPM
    fn set_tempo(&mut self, tempo: Tempo) -> Result<String, TransportError>;

    /// Set the time signature
    fn set_time_signature(&mut self, time_signature: TimeSignature) -> Result<String, TransportError>;

    /// Set the recording mode
    fn set_record_mode(&mut self, record_mode: RecordMode) -> Result<String, TransportError>;

    /// Set playhead position in seconds
    fn set_position(&mut self, seconds: f64) -> Result<String, TransportError>;

    // Async operations (async trait methods - use async fn for cleaner syntax)

    /// Open a project file (async - file I/O + UI interaction)
    async fn open_project(&mut self, path: String) -> Result<String, TransportError>;

    /// Load a project file (async - file I/O)
    async fn load_project(&mut self, path: String) -> Result<String, TransportError>;

    /// Save current project (async - file I/O)
    async fn save_project(&mut self, path: String) -> Result<String, TransportError>;

    /// Initialize audio engine (async - hardware setup)
    async fn initialize_audio(&mut self) -> Result<String, TransportError>;

    // Query methods (sync - immediate state reads)

    /// Get current tempo
    fn get_tempo(&self) -> Result<Tempo, TransportError>;

    /// Get current time signature
    fn get_time_signature(&self) -> Result<TimeSignature, TransportError>;

    /// Get current record mode
    fn get_record_mode(&self) -> Result<RecordMode, TransportError>;

    /// Get current playhead position in seconds
    fn get_position(&self) -> Result<f64, TransportError>;

    /// Check if currently playing
    fn is_playing(&self) -> Result<bool, TransportError>;

    /// Check if currently recording
    fn is_recording(&self) -> Result<bool, TransportError>;

    /// Get complete transport state
    fn get_transport(&self) -> Result<Transport, TransportError>;

    /// Check if transport is ready for operations
    fn is_ready(&self) -> Result<bool, TransportError>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::TransportAction;

    #[test]
    fn test_transport_error_display() {
        let error = TransportError::InvalidTempo("BPM too high".to_string());
        assert!(error.to_string().contains("Invalid tempo"));

        let error = TransportError::Internal("Test error".to_string());
        assert!(error.to_string().contains("Test error"));
    }

    #[test]
    fn test_transport_action_enum_generated() {
        // Test that the macro-generated TransportAction enum works
        let action = TransportAction::Play;

        // Test serialization
        let json = serde_json::to_string(&action).unwrap();
        assert!(json.contains("Play"));

        // Test deserialization
        let deserialized: TransportAction = serde_json::from_str(&json).unwrap();
        match deserialized {
            TransportAction::Play => (),
            _ => panic!("Expected Play variant"),
        }
    }

    #[test]
    fn test_transport_action_with_params() {
        // Test actions with parameters
        let tempo = Tempo { bpm: 120.0 };
        let action = TransportAction::SetTempo(tempo);

        // Test serialization with params
        let json = serde_json::to_string(&action).unwrap();
        assert!(json.contains("SetTempo"));
        assert!(json.contains("120"));

        // Test deserialization with params
        let deserialized: TransportAction = serde_json::from_str(&json).unwrap();
        match deserialized {
            TransportAction::SetTempo(t) => {
                assert_eq!(t.bpm, 120.0);
            },
            _ => panic!("Expected SetTempo variant"),
        }
    }
}

// Example usage patterns demonstrating the new approach
#[cfg(test)]
mod examples {
    use super::*;

    // Example 1: Generic function that works with any TransportActions implementation
    async fn control_any_daw<T: TransportActions>(transport: &mut T) -> Result<String, TransportError> {
        // Start playback
        transport.play()?;

        // Set tempo
        transport.set_tempo(Tempo { bpm: 140.0 })?;

        // Async operation
        transport.initialize_audio().await?;

        // Get current state
        let tempo = transport.get_tempo()?;
        let is_playing = transport.is_playing()?;

        Ok(format!("DAW state: playing={}, tempo={:.1} BPM", is_playing, tempo.bpm))
    }

    // Example 2: Protocol handler that accepts any implementation
    struct HttpTransportHandler<T: TransportActions> {
        transport: T,
    }

    impl<T: TransportActions> HttpTransportHandler<T> {
        fn new(transport: T) -> Self {
            Self { transport }
        }

        async fn handle_action(&mut self, action: TransportAction) -> Result<String, TransportError> {
            match action {
                TransportAction::Play => self.transport.play(),
                TransportAction::Pause => self.transport.pause(),
                TransportAction::Stop => self.transport.stop(),
                TransportAction::SetTempo(tempo) => self.transport.set_tempo(tempo),
                TransportAction::OpenProject(path) => self.transport.open_project(path).await,
                TransportAction::LoadProject(path) => self.transport.load_project(path).await,
                // Handle other actions...
                _ => Ok("Action not implemented".to_string()),
            }
        }
    }

    #[tokio::test]
    async fn test_polymorphic_usage() {
        use crate::transport::application::MockTransport;

        // Works with any implementation
        let mut mock_transport = MockTransport::new();
        let result = control_any_daw(&mut mock_transport).await.unwrap();
        assert!(result.contains("playing=true"));
        assert!(result.contains("tempo=140.0"));
    }

    #[tokio::test]
    async fn test_protocol_handler() {
        use crate::transport::application::MockTransport;

        // Protocol handler can work with any TransportActions implementation
        let mock_transport = MockTransport::new();
        let mut handler = HttpTransportHandler::new(mock_transport);

        // Handle different action types
        let play_result = handler.handle_action(TransportAction::Play).await.unwrap();
        assert!(play_result.contains("started"));

        let tempo_action = TransportAction::SetTempo(Tempo { bpm: 128.0 });
        let tempo_result = handler.handle_action(tempo_action).await.unwrap();
        assert!(tempo_result.contains("128"));
    }

    // Note: TransportActions cannot be used as trait objects (dyn TransportActions)
    // because it contains async methods. For runtime polymorphism with async traits,
    // you would need to use enums or other patterns.
    #[test]
    fn test_multiple_implementations() {
        use crate::transport::application::MockTransport;

        // Instead, we can work with concrete types or use generics
        let mock1 = MockTransport::new();
        let mock2 = MockTransport::new();

        // This works fine with concrete types
        let transports = vec![mock1, mock2];
        assert_eq!(transports.len(), 2);
    }

    #[test]
    fn test_transport_actions_work_directly_on_implementations() {
        use crate::transport::application::MockTransport;

        // ✅ Simple pattern: TransportActions implementations contain their own state
        let mut transport = MockTransport::new();

        // Transport actions work directly on the implementation
        transport.play().unwrap();
        assert!(transport.is_playing().unwrap());

        let current_tempo = transport.get_tempo().unwrap();
        assert_eq!(current_tempo.bpm, 120.0);

        transport.set_tempo(Tempo { bpm: 150.0 }).unwrap();
        let updated_tempo = transport.get_tempo().unwrap();
        assert_eq!(updated_tempo.bpm, 150.0);

        println!("✅ Implementations manage their own state however they want!");
    }

}
