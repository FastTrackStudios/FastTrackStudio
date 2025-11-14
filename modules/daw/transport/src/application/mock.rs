//! Mock implementation of TransportActions trait
//!
//! This provides a testing implementation that simulates transport behavior
//! without requiring actual DAW integration.

use crate::core::{PlayState, RecordMode, Tempo, Transport, TransportActions, TransportError};
use primitives::{Position, TimeSignature};

/// Mock transport service implementation for testing
#[derive(Debug)]
pub struct MockTransportService {
    transport: Transport,
}

impl MockTransportService {
    /// Create a new mock transport service with default state
    pub fn new() -> Self {
        Self {
            transport: Transport::new(),
        }
    }

    /// Create a mock transport with custom initial state
    pub fn with_state(transport: Transport) -> Self {
        Self { transport }
    }
}

impl Default for MockTransportService {
    fn default() -> Self {
        Self::new()
    }
}

impl TransportActions for MockTransportService {
    // Sync transport control commands

    fn play(&mut self) -> Result<String, TransportError> {
        self.transport.play_state = PlayState::Playing;
        Ok("Mock: Playback started".to_string())
    }

    fn pause(&mut self) -> Result<String, TransportError> {
        self.transport.play_state = PlayState::Paused;
        Ok("Mock: Playback paused".to_string())
    }

    fn stop(&mut self) -> Result<String, TransportError> {
        self.transport.reset();
        Ok("Mock: Playback stopped".to_string())
    }

    fn play_pause(&mut self) -> Result<String, TransportError> {
        if self.transport.is_playing() {
            self.transport.play_state = PlayState::Paused;
        } else {
            self.transport.play_state = PlayState::Playing;
        }
        let action = if self.transport.is_playing() {
            "started"
        } else {
            "paused"
        };
        Ok(format!("Mock: Playback {}", action))
    }

    fn play_stop(&mut self) -> Result<String, TransportError> {
        if self.transport.is_playing() {
            self.transport.reset();
            Ok("Mock: Playback stopped".to_string())
        } else {
            self.transport.play_state = PlayState::Playing;
            Ok("Mock: Playback started".to_string())
        }
    }

    fn start_recording(&mut self) -> Result<String, TransportError> {
        self.transport.play_state = PlayState::Recording;
        Ok("Mock: Recording started".to_string())
    }

    fn stop_recording(&mut self) -> Result<String, TransportError> {
        self.transport.play_state = PlayState::Playing;
        Ok("Mock: Recording stopped".to_string())
    }

    fn toggle_recording(&mut self) -> Result<String, TransportError> {
        if self.transport.is_recording() {
            self.transport.play_state = PlayState::Playing;
        } else {
            self.transport.play_state = PlayState::Recording;
        }
        let action = if self.transport.is_recording() {
            "started"
        } else {
            "stopped"
        };
        Ok(format!("Mock: Recording {}", action))
    }

    // Configuration commands

    fn set_tempo(&mut self, tempo: Tempo) -> Result<String, TransportError> {
        self.transport.tempo = tempo;
        Ok(format!("Mock: Tempo set to {} BPM", tempo.bpm))
    }

    fn set_time_signature(
        &mut self,
        time_signature: TimeSignature,
    ) -> Result<String, TransportError> {
        self.transport.time_signature = time_signature;
        Ok(format!(
            "Mock: Time signature set to {}/{}",
            time_signature.numerator, time_signature.denominator
        ))
    }

    fn set_record_mode(&mut self, record_mode: RecordMode) -> Result<String, TransportError> {
        self.transport.record_mode = record_mode;
        Ok(format!("Mock: Record mode set to {:?}", record_mode))
    }

    fn set_position(&mut self, seconds: f64) -> Result<String, TransportError> {
        self.transport.playhead_position = Position::from_seconds(seconds);
        Ok(format!("Mock: Position set to {} seconds", seconds))
    }

    // Query methods

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
        // Mock is always ready
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_mock_transport_playback() {
        let mut transport = MockTransportService::new();

        // Test initial state
        assert!(!transport.is_playing().unwrap());
        assert!(!transport.is_recording().unwrap());
        assert_eq!(transport.get_position().unwrap(), 0.0);

        // Test play
        let result = transport.play().unwrap();
        assert!(result.contains("started"));
        assert!(transport.is_playing().unwrap());

        // Test pause
        let result = transport.pause().unwrap();
        assert!(result.contains("paused"));
        assert!(!transport.is_playing().unwrap());

        // Test stop
        let result = transport.stop().unwrap();
        assert!(result.contains("stopped"));
        assert!(!transport.is_playing().unwrap());
    }

    #[tokio::test]
    async fn test_mock_transport_recording() {
        let mut transport = MockTransportService::new();

        // Test start recording
        let result = transport.start_recording().unwrap();
        assert!(result.contains("started"));
        assert!(transport.is_recording().unwrap());

        // Test stop recording
        let result = transport.stop_recording().unwrap();
        assert!(result.contains("stopped"));
        assert!(!transport.is_recording().unwrap());
        assert!(transport.is_playing().unwrap()); // Should be playing after stopping recording
    }

    #[tokio::test]
    async fn test_mock_transport_tempo() {
        let mut transport = MockTransportService::new();

        let new_tempo = Tempo { bpm: 140.0 };
        let result = transport.set_tempo(new_tempo).unwrap();
        assert!(result.contains("140"));

        let current_tempo = transport.get_tempo().unwrap();
        assert_eq!(current_tempo.bpm, 140.0);
    }

    #[tokio::test]
    async fn test_mock_transport_time_signature() {
        let mut transport = MockTransportService::new();

        let new_time_sig = TimeSignature {
            numerator: 3,
            denominator: 4,
        };
        let result = transport.set_time_signature(new_time_sig).unwrap();
        assert!(result.contains("3/4"));

        let current_time_sig = transport.get_time_signature().unwrap();
        assert_eq!(current_time_sig.numerator, 3);
        assert_eq!(current_time_sig.denominator, 4);
    }

    #[tokio::test]
    async fn test_mock_transport_position() {
        let mut transport = MockTransportService::new();

        let result = transport.set_position(30.5).unwrap();
        assert!(result.contains("30.5"));

        let current_position = transport.get_position().unwrap();
        assert_eq!(current_position, 30.5);
    }

    #[tokio::test]
    async fn test_mock_transport_state() {
        let mut transport = MockTransportService::new();

        // Modify state
        transport.set_tempo(Tempo { bpm: 160.0 }).unwrap();
        transport.set_position(45.0).unwrap();

        // Get complete state
        let state = transport.get_transport().unwrap();
        assert_eq!(state.tempo.bpm, 160.0);
        assert_eq!(state.playhead_position.time.to_seconds(), 45.0);
    }

    #[tokio::test]
    async fn test_record_mode_settings() {
        let mut transport = MockTransportService::new();

        // Test different record modes
        transport.set_record_mode(RecordMode::Normal).unwrap();
        assert!(matches!(
            transport.get_record_mode().unwrap(),
            RecordMode::Normal
        ));

        transport
            .set_record_mode(RecordMode::TimeSelection)
            .unwrap();
        assert!(matches!(
            transport.get_record_mode().unwrap(),
            RecordMode::TimeSelection
        ));

        transport.set_record_mode(RecordMode::Item).unwrap();
        assert!(matches!(
            transport.get_record_mode().unwrap(),
            RecordMode::Item
        ));
    }
}
