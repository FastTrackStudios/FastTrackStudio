use marker_region::core::Marker;
use primitives::TimeSignature;
use serde::{Deserialize, Serialize};
use specta::Type;
use std::collections::HashMap;
use transport::{RecordMode, Tempo, Transport, TransportActions, TransportError};

/// Embeddable project state that carries its own transport.
#[derive(Debug, Clone, Serialize, Deserialize, Type)]
pub struct Project<T> {
    id: uuid::Uuid,
    name: String,
    description: Option<String>,
    path: Option<String>,
    metadata: HashMap<String, String>,
    transport: T,
    markers: Vec<Marker>,
}

/// Default project type backed by the core `Transport`.
pub type TransportProject = Project<Transport>;

impl<T> Project<T> {
    /// Create a new project with a specific transport implementation.
    pub fn new(name: impl Into<String>, transport: T) -> Self {
        Self {
            id: uuid::Uuid::new_v4(),
            name: name.into(),
            description: None,
            path: None,
            metadata: HashMap::new(),
            transport,
            markers: Vec::new(),
        }
    }

    pub fn id(&self) -> uuid::Uuid {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn set_name(&mut self, name: impl Into<String>) {
        self.name = name.into();
    }

    pub fn description(&self) -> Option<&String> {
        self.description.as_ref()
    }

    pub fn set_description(&mut self, description: impl Into<String>) {
        self.description = Some(description.into());
    }

    pub fn path(&self) -> Option<&String> {
        self.path.as_ref()
    }

    pub fn set_path(&mut self, path: impl Into<String>) {
        self.path = Some(path.into());
    }

    pub fn metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }

    pub fn set_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }

    pub fn transport(&self) -> &T {
        &self.transport
    }

    pub fn transport_mut(&mut self) -> &mut T {
        &mut self.transport
    }

    pub fn replace_transport(&mut self, transport: T) {
        self.transport = transport;
    }

    pub fn markers(&self) -> &[Marker] {
        &self.markers
    }

    pub fn markers_mut(&mut self) -> &mut Vec<Marker> {
        &mut self.markers
    }

    pub fn set_markers(&mut self, markers: Vec<Marker>) {
        self.markers = markers;
    }

    pub fn add_marker(&mut self, marker: Marker) {
        self.markers.push(marker);
    }
}

impl Default for Project<Transport> {
    fn default() -> Self {
        Self::new("Untitled Project", Transport::new())
    }
}

impl Project<Transport> {
    /// Convenience constructor using a default `Transport`.
    pub fn with_default_transport(name: impl Into<String>) -> Self {
        Self::new(name, Transport::new())
    }
}

impl<T> TransportActions for Project<T>
where
    T: TransportActions,
{
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
        self.transport.set_tempo(tempo)
    }

    fn set_time_signature(
        &mut self,
        time_signature: TimeSignature,
    ) -> Result<String, TransportError> {
        self.transport.set_time_signature(time_signature)
    }

    fn set_record_mode(
        &mut self,
        record_mode: RecordMode,
    ) -> Result<String, TransportError> {
        self.transport.set_record_mode(record_mode)
    }

    fn set_position(&mut self, seconds: f64) -> Result<String, TransportError> {
        self.transport.set_position(seconds)
    }

    fn get_tempo(&self) -> Result<Tempo, TransportError> {
        self.transport.get_tempo()
    }

    fn get_time_signature(&self) -> Result<TimeSignature, TransportError> {
        self.transport.get_time_signature()
    }

    fn get_record_mode(&self) -> Result<RecordMode, TransportError> {
        self.transport.get_record_mode()
    }

    fn get_position(&self) -> Result<f64, TransportError> {
        self.transport.get_position()
    }

    fn is_playing(&self) -> Result<bool, TransportError> {
        self.transport.is_playing()
    }

    fn is_recording(&self) -> Result<bool, TransportError> {
        self.transport.is_recording()
    }

    fn get_transport(&self) -> Result<Transport, TransportError> {
        self.transport.get_transport()
    }

    fn is_ready(&self) -> Result<bool, TransportError> {
        self.transport.is_ready()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use transport::TransportActions as _;

    #[test]
    fn project_embeds_transport() {
        let mut project = Project::with_default_transport("Demo");
        assert_eq!(project.name(), "Demo");
        assert!(project.transport().is_stopped());

        project.play().unwrap();
        assert!(project.transport().is_playing());

        project.stop().unwrap();
        assert!(project.transport().is_stopped());
    }

    #[test]
    fn metadata_helpers() {
        let mut project = Project::with_default_transport("Meta");
        project.set_metadata("key", "value");
        assert_eq!(project.metadata("key"), Some(&"value".to_string()));
    }
}
