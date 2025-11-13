//! Mock implementation of TransportActions trait
//!
//! This provides a testing implementation that simulates transport behavior
//! without requiring actual DAW integration.

use crate::transport::core::actions::TransportActions;
use crate::transport::core::error::TransportError;
use crate::transport::core::transport::{Transport, Tempo, RecordMode, PlayState};
use crate::primitives::{TimeSignature, Position};
use std::sync::Arc;
use tokio::sync::RwLock;

/// Mock transport implementation for testing
#[derive(Debug, Clone)]
pub struct MockTransport {
    state: Arc<RwLock<Transport>>,
}

impl MockTransport {
    /// Create a new mock transport with default state
    pub fn new() -> Self {
        Self {
            state: Arc::new(RwLock::new(Transport::default())),
        }
    }

    /// Create a mock transport with custom initial state
    pub fn with_state(state: Transport) -> Self {
        Self {
            state: Arc::new(RwLock::new(state)),
        }
    }

    /// Get a clone of the current state for testing
    pub async fn get_current_state(&self) -> Transport {
        self.state.read().await.clone()
    }
}

impl Default for MockTransport {
    fn default() -> Self {
        Self::new()
    }
}

impl TransportActions for MockTransport {
    // Sync transport control commands

    fn play(&mut self) -> Result<String, TransportError> {
        let mut state = self.state.try_write().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        state.play_state = PlayState::Playing;
        Ok("Mock: Playback started".to_string())
    }

    fn pause(&mut self) -> Result<String, TransportError> {
        let mut state = self.state.try_write().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        state.play_state = PlayState::Paused;
        Ok("Mock: Playback paused".to_string())
    }

    fn stop(&mut self) -> Result<String, TransportError> {
        let mut state = self.state.try_write().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        state.reset();
        Ok("Mock: Playback stopped".to_string())
    }

    fn play_pause(&mut self) -> Result<String, TransportError> {
        let mut state = self.state.try_write().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        if state.is_playing() {
            state.play_state = PlayState::Paused;
        } else {
            state.play_state = PlayState::Playing;
        }
        let action = if state.is_playing() { "started" } else { "paused" };
        Ok(format!("Mock: Playback {}", action))
    }

    fn play_stop(&mut self) -> Result<String, TransportError> {
        let mut state = self.state.try_write().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        if state.is_playing() {
            state.reset();
            Ok("Mock: Playback stopped".to_string())
        } else {
            state.play_state = PlayState::Playing;
            Ok("Mock: Playback started".to_string())
        }
    }

    fn start_recording(&mut self) -> Result<String, TransportError> {
        let mut state = self.state.try_write().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        state.play_state = PlayState::Recording;
        Ok("Mock: Recording started".to_string())
    }

    fn stop_recording(&mut self) -> Result<String, TransportError> {
        let mut state = self.state.try_write().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        state.play_state = PlayState::Playing;
        Ok("Mock: Recording stopped".to_string())
    }

    fn toggle_recording(&mut self) -> Result<String, TransportError> {
        let mut state = self.state.try_write().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        if state.is_recording() {
            state.play_state = PlayState::Playing;
        } else {
            state.play_state = PlayState::Recording;
        }
        let action = if state.is_recording() { "started" } else { "stopped" };
        Ok(format!("Mock: Recording {}", action))
    }

    // Configuration commands

    fn set_tempo(&mut self, tempo: Tempo) -> Result<String, TransportError> {
        let mut state = self.state.try_write().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        state.tempo = tempo;
        Ok(format!("Mock: Tempo set to {} BPM", tempo.bpm))
    }

    fn set_time_signature(&mut self, time_signature: TimeSignature) -> Result<String, TransportError> {
        let mut state = self.state.try_write().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        state.time_signature = time_signature;
        Ok(format!("Mock: Time signature set to {}/{}", time_signature.numerator, time_signature.denominator))
    }

    fn set_record_mode(&mut self, record_mode: RecordMode) -> Result<String, TransportError> {
        let mut state = self.state.try_write().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        state.record_mode = record_mode;
        Ok(format!("Mock: Record mode set to {:?}", record_mode))
    }

    fn set_position(&mut self, seconds: f64) -> Result<String, TransportError> {
        let mut state = self.state.try_write().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        state.playhead_position = Position::from_seconds(seconds);
        Ok(format!("Mock: Position set to {} seconds", seconds))
    }

    // Async operations

    async fn open_project(&mut self, path: String) -> Result<String, TransportError> {
        // Simulate opening a project with UI interaction and file I/O
        tokio::time::sleep(tokio::time::Duration::from_millis(300)).await;

        // Reset transport state and simulate loading project data
        let mut state = self.state.write().await;
        state.reset();
        state.tempo = Tempo { bpm: 120.0 }; // Default project tempo

        Ok(format!("Mock: Project opened from '{}'", path))
    }

    async fn load_project(&mut self, path: String) -> Result<String, TransportError> {
        // Simulate file I/O delay
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

        // Reset transport state for new project
        let mut state = self.state.write().await;
        state.reset();

        Ok(format!("Mock: Project loaded from '{}'", path))
    }

    async fn save_project(&mut self, path: String) -> Result<String, TransportError> {
        // Simulate file I/O delay
        tokio::time::sleep(tokio::time::Duration::from_millis(150)).await;

        // In a real implementation, current state would be serialized to file
        Ok(format!("Mock: Project saved to '{}'", path))
    }

    async fn initialize_audio(&mut self) -> Result<String, TransportError> {
        // Simulate audio hardware initialization delay
        tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;

        Ok("Mock: Audio engine initialized".to_string())
    }

    // Query methods

    fn get_tempo(&self) -> Result<Tempo, TransportError> {
        let state = self.state.try_read().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        Ok(state.tempo)
    }

    fn get_time_signature(&self) -> Result<TimeSignature, TransportError> {
        let state = self.state.try_read().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        Ok(state.time_signature)
    }

    fn get_record_mode(&self) -> Result<RecordMode, TransportError> {
        let state = self.state.try_read().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        Ok(state.record_mode)
    }

    fn get_position(&self) -> Result<f64, TransportError> {
        let state = self.state.try_read().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        Ok(state.playhead_position.time.to_seconds())
    }

    fn is_playing(&self) -> Result<bool, TransportError> {
        let state = self.state.try_read().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        Ok(state.is_playing())
    }

    fn is_recording(&self) -> Result<bool, TransportError> {
        let state = self.state.try_read().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        Ok(state.is_recording())
    }

    fn get_transport(&self) -> Result<Transport, TransportError> {
        let state = self.state.try_read().map_err(|_|
            TransportError::Internal("Failed to acquire state lock".to_string()))?;
        Ok(state.clone())
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
        let mut transport = MockTransport::new();

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
        let mut transport = MockTransport::new();

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
        let mut transport = MockTransport::new();

        let new_tempo = Tempo { bpm: 140.0 };
        let result = transport.set_tempo(new_tempo).unwrap();
        assert!(result.contains("140"));

        let current_tempo = transport.get_tempo().unwrap();
        assert_eq!(current_tempo.bpm, 140.0);
    }

    #[tokio::test]
    async fn test_mock_transport_time_signature() {
        let mut transport = MockTransport::new();

        let new_time_sig = TimeSignature { numerator: 3, denominator: 4 };
        let result = transport.set_time_signature(new_time_sig).unwrap();
        assert!(result.contains("3/4"));

        let current_time_sig = transport.get_time_signature().unwrap();
        assert_eq!(current_time_sig.numerator, 3);
        assert_eq!(current_time_sig.denominator, 4);
    }

    #[tokio::test]
    async fn test_mock_transport_position() {
        let mut transport = MockTransport::new();

        let result = transport.set_position(30.5).unwrap();
        assert!(result.contains("30.5"));

        let current_position = transport.get_position().unwrap();
        assert_eq!(current_position, 30.5);
    }

    #[tokio::test]
    async fn test_mock_transport_state() {
        let mut transport = MockTransport::new();

        // Modify state
        transport.set_tempo(Tempo { bpm: 160.0 }).unwrap();
        transport.set_position(45.0).unwrap();

        // Get complete state
        let state = transport.get_transport().unwrap();
        assert_eq!(state.tempo.bpm, 160.0);
        assert_eq!(state.playhead_position.time.to_seconds(), 45.0);
    }

    #[tokio::test]
    async fn test_async_operations() {
        let mut transport = MockTransport::new();

        // Test all async operations
        let result = transport.open_project("test.daw".to_string()).await.unwrap();
        assert!(result.contains("opened"));

        let result = transport.load_project("project.daw".to_string()).await.unwrap();
        assert!(result.contains("loaded"));

        let result = transport.save_project("output.daw".to_string()).await.unwrap();
        assert!(result.contains("saved"));

        let result = transport.initialize_audio().await.unwrap();
        assert!(result.contains("initialized"));
    }

    #[tokio::test]
    async fn test_mixed_sync_async_usage() {
        let mut transport = MockTransport::new();

        // Mix sync and async operations
        transport.play().unwrap();
        assert!(transport.is_playing().unwrap());

        let result = transport.open_project("song.daw".to_string()).await.unwrap();
        assert!(result.contains("opened"));

        transport.set_tempo(Tempo { bpm: 128.0 }).unwrap();
        assert_eq!(transport.get_tempo().unwrap().bpm, 128.0);

        let result = transport.initialize_audio().await.unwrap();
        assert!(result.contains("initialized"));
    }

    #[tokio::test]
    async fn test_open_project_vs_load_project() {
        let mut transport = MockTransport::new();

        // open_project should take longer (includes UI interaction)
        let start = std::time::Instant::now();
        transport.open_project("interactive.daw".to_string()).await.unwrap();
        let open_duration = start.elapsed();

        let start = std::time::Instant::now();
        transport.load_project("batch.daw".to_string()).await.unwrap();
        let load_duration = start.elapsed();

        // open_project should take longer than load_project
        assert!(open_duration > load_duration);
    }

    #[tokio::test]
    async fn test_record_mode_settings() {
        let mut transport = MockTransport::new();

        // Test different record modes
        transport.set_record_mode(RecordMode::Normal).unwrap();
        assert!(matches!(transport.get_record_mode().unwrap(), RecordMode::Normal));

        transport.set_record_mode(RecordMode::TimeSelection).unwrap();
        assert!(matches!(transport.get_record_mode().unwrap(), RecordMode::TimeSelection));

        transport.set_record_mode(RecordMode::Item).unwrap();
        assert!(matches!(transport.get_record_mode().unwrap(), RecordMode::Item));
    }
}
