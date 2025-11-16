//! Marker Domain Model
//!
//! This module defines the core Marker struct and related types for managing
//! timeline markers in a DAW. Markers represent specific points in time
//! that can be used for navigation, arrangement, and workflow organization.

use primitives::Position;
use crate::core::MarkerRegionError;

/// A marker represents a named point in time on the timeline
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Marker {
    /// Unique identifier for this marker (if supported by the source)
    pub id: Option<u32>,

    /// Position in time where this marker is located
    pub position: Position,

    /// Human-readable name for this marker
    pub name: String,

    /// Color for visual representation (RGB value, if supported)
    pub color: Option<u32>,

    /// Implementation-specific flags (e.g., REAPER's marker flags)
    pub flags: Option<u32>,

    /// Whether this marker is locked from editing
    pub locked: Option<bool>,

    /// Globally unique identifier (if supported by the source)
    pub guid: Option<String>,
}

impl Marker {
    /// Create a new marker at the specified position with a name
    pub fn new(position: Position, name: String) -> Self {
        Self {
            id: None,
            position,
            name,
            color: None,
            flags: None,
            locked: None,
            guid: None,
        }
    }

    /// Create a marker from a time position in seconds
    pub fn from_seconds(seconds: f64, name: String) -> Self {
        Self::new(Position::from_seconds(seconds), name)
    }

    /// Create a marker with full metadata
    pub fn new_full(
        id: Option<u32>,
        position: Position,
        name: String,
        color: Option<u32>,
        flags: Option<u32>,
        locked: Option<bool>,
        guid: Option<String>,
    ) -> Self {
        Self {
            id,
            position,
            name,
            color,
            flags,
            locked,
            guid,
        }
    }

    /// Get the position in seconds
    pub fn position_seconds(&self) -> f64 {
        self.position.time.to_seconds()
    }

    /// Get the musical position (measure.beat.subdivision)
    pub fn musical_position(&self) -> String {
        self.position.musical_position_string()
    }

    /// Check if this marker is at the specified time position (within tolerance)
    pub fn is_at_position(&self, seconds: f64, tolerance: f64) -> bool {
        (self.position_seconds() - seconds).abs() <= tolerance
    }

    /// Check if this marker is within a time range
    pub fn is_in_range(&self, start_seconds: f64, end_seconds: f64) -> bool {
        let pos = self.position_seconds();
        pos >= start_seconds && pos <= end_seconds
    }

    /// Validate the marker data
    pub fn validate(&self) -> Result<(), MarkerRegionError> {
        // Check position is not negative
        if self.position_seconds() < 0.0 {
            return Err(MarkerRegionError::InvalidMarkerPosition(self.position_seconds()));
        }

        // Check name is not empty
        if self.name.trim().is_empty() {
            return Err(MarkerRegionError::InvalidMarkerName(self.name.clone()));
        }

        Ok(())
    }

    /// Update the marker's position
    pub fn set_position(&mut self, position: Position) -> Result<(), MarkerRegionError> {
        if position.time.to_seconds() < 0.0 {
            return Err(MarkerRegionError::InvalidMarkerPosition(position.time.to_seconds()));
        }
        self.position = position;
        Ok(())
    }

    /// Update the marker's name
    pub fn set_name(&mut self, name: String) -> Result<(), MarkerRegionError> {
        if name.trim().is_empty() {
            return Err(MarkerRegionError::InvalidMarkerName(name));
        }
        self.name = name;
        Ok(())
    }

    /// Get a display string for this marker
    pub fn display_string(&self) -> String {
        format!("{} @ {:.3}s ({})",
                self.name,
                self.position_seconds(),
                self.musical_position())
    }

    /// Check if this marker has the same position as another (within tolerance)
    pub fn same_position(&self, other: &Marker, tolerance: f64) -> bool {
        (self.position_seconds() - other.position_seconds()).abs() <= tolerance
    }

    /// Get color as RGB components (if available)
    pub fn rgb_color(&self) -> Option<(u8, u8, u8)> {
        self.color.map(|c| {
            let r = ((c >> 16) & 0xFF) as u8;
            let g = ((c >> 8) & 0xFF) as u8;
            let b = (c & 0xFF) as u8;
            (r, g, b)
        })
    }

    /// Set color from RGB components
    pub fn set_rgb_color(&mut self, r: u8, g: u8, b: u8) {
        self.color = Some(((r as u32) << 16) | ((g as u32) << 8) | (b as u32));
    }
}

impl Default for Marker {
    fn default() -> Self {
        Self::new(Position::start(), "Untitled Marker".to_string())
    }
}

impl std::fmt::Display for Marker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display_string())
    }
}

/// Trait for sources that can provide markers
pub trait MarkerSource: Send + Sync {
    /// Get all markers from the source
    fn get_markers(&self) -> Result<Vec<Marker>, MarkerRegionError>;

    /// Get a specific marker by ID (if supported)
    fn get_marker_by_id(&self, id: u32) -> Result<Option<Marker>, MarkerRegionError> {
        let markers = self.get_markers()?;
        Ok(markers.into_iter().find(|m| m.id == Some(id)))
    }

    /// Get markers within a time range
    fn get_markers_in_range(&self, start_seconds: f64, end_seconds: f64) -> Result<Vec<Marker>, MarkerRegionError> {
        let markers = self.get_markers()?;
        Ok(markers.into_iter()
           .filter(|m| m.is_in_range(start_seconds, end_seconds))
           .collect())
    }

    /// Get the marker closest to a given time position
    fn get_nearest_marker(&self, seconds: f64) -> Result<Option<Marker>, MarkerRegionError> {
        let markers = self.get_markers()?;
        Ok(markers.into_iter()
           .min_by(|a, b| {
               let diff_a = (a.position_seconds() - seconds).abs();
               let diff_b = (b.position_seconds() - seconds).abs();
               diff_a.partial_cmp(&diff_b).unwrap_or(std::cmp::Ordering::Equal)
           }))
    }

    /// Get the next marker after the given position
    fn get_next_marker(&self, after_seconds: f64) -> Result<Option<Marker>, MarkerRegionError> {
        let markers = self.get_markers()?;
        Ok(markers.into_iter()
           .filter(|m| m.position_seconds() > after_seconds)
           .min_by(|a, b| a.position_seconds().partial_cmp(&b.position_seconds()).unwrap_or(std::cmp::Ordering::Equal)))
    }

    /// Get the previous marker before the given position
    fn get_previous_marker(&self, before_seconds: f64) -> Result<Option<Marker>, MarkerRegionError> {
        let markers = self.get_markers()?;
        Ok(markers.into_iter()
           .filter(|m| m.position_seconds() < before_seconds)
           .max_by(|a, b| a.position_seconds().partial_cmp(&b.position_seconds()).unwrap_or(std::cmp::Ordering::Equal)))
    }

    /// Get a human-readable name for this marker source
    fn source_name(&self) -> &'static str;

    /// Check if this source supports creating/modifying markers
    fn is_writable(&self) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_marker_creation() {
        let marker = Marker::from_seconds(30.5, "Chorus Start".to_string());

        assert_eq!(marker.position_seconds(), 30.5);
        assert_eq!(marker.name, "Chorus Start");
        assert!(marker.id.is_none());
        assert!(marker.color.is_none());
    }

    #[test]
    fn test_marker_validation() {
        let mut marker = Marker::from_seconds(-1.0, "Invalid".to_string());
        assert!(marker.validate().is_err());

        marker.position = Position::from_seconds(10.0);
        assert!(marker.validate().is_ok());

        marker.name = "".to_string();
        assert!(marker.validate().is_err());

        marker.name = "   ".to_string();
        assert!(marker.validate().is_err());

        marker.name = "Valid Name".to_string();
        assert!(marker.validate().is_ok());
    }

    #[test]
    fn test_marker_position_queries() {
        let marker = Marker::from_seconds(60.0, "Test".to_string());

        assert!(marker.is_at_position(60.0, 0.1));
        assert!(marker.is_at_position(59.95, 0.1));
        assert!(!marker.is_at_position(60.2, 0.1));

        assert!(marker.is_in_range(50.0, 70.0));
        assert!(!marker.is_in_range(0.0, 50.0));
        assert!(!marker.is_in_range(70.0, 100.0));
    }

    #[test]
    fn test_marker_color() {
        let mut marker = Marker::default();

        marker.set_rgb_color(255, 128, 64);
        assert_eq!(marker.rgb_color(), Some((255, 128, 64)));

        // Test color conversion
        assert_eq!(marker.color, Some(0xFF8040));
    }

    #[test]
    fn test_marker_display() {
        let marker = Marker::from_seconds(123.456, "Test Marker".to_string());
        let display = marker.display_string();

        assert!(display.contains("Test Marker"));
        assert!(display.contains("123.456"));
    }

    #[test]
    fn test_same_position() {
        let marker1 = Marker::from_seconds(60.0, "A".to_string());
        let marker2 = Marker::from_seconds(60.05, "B".to_string());
        let marker3 = Marker::from_seconds(61.0, "C".to_string());

        assert!(marker1.same_position(&marker2, 0.1));
        assert!(!marker1.same_position(&marker3, 0.1));
    }
}
