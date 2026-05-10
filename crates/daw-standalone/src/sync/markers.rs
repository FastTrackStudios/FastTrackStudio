//! `StandaloneMarkers` — sync markers sub-handle.

use daw_proto::{DawError, DawResult, Marker, Position, PositionInSeconds, sync::Markers};

use super::daw::Standalone;

pub struct StandaloneMarkers<'a> {
    daw: &'a Standalone,
    guid: String,
}

impl<'a> StandaloneMarkers<'a> {
    pub(crate) fn new(daw: &'a Standalone, guid: String) -> Self {
        Self { daw, guid }
    }
}

impl<'a> Markers for StandaloneMarkers<'a> {
    fn all(&self) -> Vec<Marker> {
        self.daw
            .with_project(&self.guid, |p| p.markers.values().cloned().collect())
            .unwrap_or_default()
    }

    fn get(&self, id: u32) -> Option<Marker> {
        self.daw
            .with_project(&self.guid, |p| p.markers.get(&id).cloned())
            .ok()
            .flatten()
    }

    fn count(&self) -> u32 {
        self.daw
            .with_project(&self.guid, |p| p.markers.len() as u32)
            .unwrap_or(0)
    }

    fn add(&self, position: f64, name: &str) -> DawResult<u32> {
        self.daw.with_project_mut(&self.guid, |p| {
            let id = p.next_marker_id;
            p.next_marker_id += 1;
            let mut marker = Marker::new(
                Position::from_time(PositionInSeconds::from_seconds(position)),
                name.to_string(),
            );
            marker.id = Some(id);
            p.markers.insert(id, marker);
            id
        })
    }

    fn remove(&self, id: u32) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            p.markers
                .remove(&id)
                .map(|_| ())
                .ok_or_else(|| DawError::not_found("Marker", &id.to_string()))
        })?
    }

    fn set_position(&self, id: u32, position: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            let m = p
                .markers
                .get_mut(&id)
                .ok_or_else(|| DawError::not_found("Marker", &id.to_string()))?;
            m.position = Position::from_time(PositionInSeconds::from_seconds(position));
            Ok::<(), DawError>(())
        })?
    }

    fn rename(&self, id: u32, name: &str) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            let m = p
                .markers
                .get_mut(&id)
                .ok_or_else(|| DawError::not_found("Marker", &id.to_string()))?;
            m.name = name.to_string();
            Ok::<(), DawError>(())
        })?
    }

    fn set_color(&self, id: u32, color: u32) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            let m = p
                .markers
                .get_mut(&id)
                .ok_or_else(|| DawError::not_found("Marker", &id.to_string()))?;
            m.color = Some(color);
            Ok::<(), DawError>(())
        })?
    }
}
