//! Mock Marker Region Service
//!
//! This module provides a mock implementation of MarkerRegionSource for testing
//! and development purposes. It maintains an in-memory collection of markers
//! and regions that can be manipulated for testing scenarios.

use crate::core::{
    Marker, Region, MarkerRegionError, MarkerSource, RegionSource, MarkerRegionSource
};
use std::sync::{Arc, Mutex};

/// Mock implementation of MarkerRegionSource for testing
pub struct MockMarkerRegionService {
    /// Internal state protected by mutex for thread safety
    state: Arc<Mutex<MockState>>,
}

#[derive(Debug, Clone)]
struct MockState {
    markers: Vec<Marker>,
    regions: Vec<Region>,
    next_marker_id: u32,
    next_region_id: u32,
    is_writable: bool,
    simulate_errors: bool,
}

impl MockMarkerRegionService {
    /// Create a new empty mock service
    pub fn new() -> Self {
        Self {
            state: Arc::new(Mutex::new(MockState {
                markers: Vec::new(),
                regions: Vec::new(),
                next_marker_id: 1,
                next_region_id: 1,
                is_writable: true,
                simulate_errors: false,
            })),
        }
    }

    /// Create a mock service with sample data for testing
    pub fn with_sample_data() -> Self {
        let service = Self::new();

        // Add sample markers
        service.add_marker(Marker::from_seconds(0.0, "Song Start".to_string())).ok();
        service.add_marker(Marker::from_seconds(30.0, "Verse 1".to_string())).ok();
        service.add_marker(Marker::from_seconds(60.0, "Chorus 1".to_string())).ok();
        service.add_marker(Marker::from_seconds(90.0, "Verse 2".to_string())).ok();
        service.add_marker(Marker::from_seconds(120.0, "Chorus 2".to_string())).ok();
        service.add_marker(Marker::from_seconds(150.0, "Bridge".to_string())).ok();
        service.add_marker(Marker::from_seconds(180.0, "Final Chorus".to_string())).ok();
        service.add_marker(Marker::from_seconds(210.0, "Outro".to_string())).ok();

        // Add sample regions
        service.add_region(Region::from_seconds(0.0, 30.0, "Intro".to_string()).unwrap()).ok();
        service.add_region(Region::from_seconds(30.0, 90.0, "Verse Section".to_string()).unwrap()).ok();
        service.add_region(Region::from_seconds(90.0, 150.0, "Chorus Section".to_string()).unwrap()).ok();
        service.add_region(Region::from_seconds(150.0, 180.0, "Bridge Section".to_string()).unwrap()).ok();
        service.add_region(Region::from_seconds(180.0, 240.0, "Outro Section".to_string()).unwrap()).ok();

        service
    }

    /// Create a mock service with complex overlapping data for testing edge cases
    pub fn with_complex_data() -> Self {
        let service = Self::new();

        // Add markers with some close together
        service.add_marker(Marker::from_seconds(10.0, "A".to_string())).ok();
        service.add_marker(Marker::from_seconds(10.1, "B".to_string())).ok();
        service.add_marker(Marker::from_seconds(25.0, "C".to_string())).ok();
        service.add_marker(Marker::from_seconds(50.0, "D".to_string())).ok();
        service.add_marker(Marker::from_seconds(75.0, "E".to_string())).ok();

        // Add overlapping regions
        service.add_region(Region::from_seconds(0.0, 30.0, "Region 1".to_string()).unwrap()).ok();
        service.add_region(Region::from_seconds(20.0, 60.0, "Region 2".to_string()).unwrap()).ok();
        service.add_region(Region::from_seconds(40.0, 80.0, "Region 3".to_string()).unwrap()).ok();

        service
    }

    /// Add a marker to the mock service
    pub fn add_marker(&self, mut marker: Marker) -> Result<u32, MarkerRegionError> {
        let mut state = self.state.lock().unwrap();

        if state.simulate_errors {
            return Err(MarkerRegionError::OperationError("Simulated error".to_string()));
        }

        marker.validate()?;

        // Assign ID if not already set
        if marker.id.is_none() {
            marker.id = Some(state.next_marker_id);
            state.next_marker_id += 1;
        }

        let id = marker.id.unwrap();
        state.markers.push(marker);

        // Keep markers sorted by position
        state.markers.sort_by(|a, b| {
            a.position_seconds()
                .partial_cmp(&b.position_seconds())
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        Ok(id)
    }

    /// Add a region to the mock service
    pub fn add_region(&self, mut region: Region) -> Result<u32, MarkerRegionError> {
        let mut state = self.state.lock().unwrap();

        if state.simulate_errors {
            return Err(MarkerRegionError::OperationError("Simulated error".to_string()));
        }

        region.validate()?;

        // Assign ID if not already set
        if region.id.is_none() {
            region.id = Some(state.next_region_id);
            state.next_region_id += 1;
        }

        let id = region.id.unwrap();
        state.regions.push(region);

        // Keep regions sorted by start position
        state.regions.sort_by(|a, b| {
            a.start_seconds()
                .partial_cmp(&b.start_seconds())
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        Ok(id)
    }

    /// Remove a marker by ID
    pub fn remove_marker(&self, id: u32) -> Result<bool, MarkerRegionError> {
        let mut state = self.state.lock().unwrap();

        if state.simulate_errors {
            return Err(MarkerRegionError::OperationError("Simulated error".to_string()));
        }

        let initial_len = state.markers.len();
        state.markers.retain(|m| m.id != Some(id));
        Ok(state.markers.len() < initial_len)
    }

    /// Remove a region by ID
    pub fn remove_region(&self, id: u32) -> Result<bool, MarkerRegionError> {
        let mut state = self.state.lock().unwrap();

        if state.simulate_errors {
            return Err(MarkerRegionError::OperationError("Simulated error".to_string()));
        }

        let initial_len = state.regions.len();
        state.regions.retain(|r| r.id != Some(id));
        Ok(state.regions.len() < initial_len)
    }

    /// Clear all markers and regions
    pub fn clear_all(&self) -> Result<(), MarkerRegionError> {
        let mut state = self.state.lock().unwrap();

        if state.simulate_errors {
            return Err(MarkerRegionError::OperationError("Simulated error".to_string()));
        }

        state.markers.clear();
        state.regions.clear();
        state.next_marker_id = 1;
        state.next_region_id = 1;

        Ok(())
    }

    /// Set whether this service should simulate errors
    pub fn set_simulate_errors(&self, simulate: bool) {
        let mut state = self.state.lock().unwrap();
        state.simulate_errors = simulate;
    }

    /// Set whether this service is writable
    pub fn set_writable(&self, writable: bool) {
        let mut state = self.state.lock().unwrap();
        state.is_writable = writable;
    }

    /// Get the current marker count
    pub fn marker_count(&self) -> usize {
        let state = self.state.lock().unwrap();
        state.markers.len()
    }

    /// Get the current region count
    pub fn region_count(&self) -> usize {
        let state = self.state.lock().unwrap();
        state.regions.len()
    }

    /// Update a marker by ID
    pub fn update_marker(&self, id: u32, updated_marker: Marker) -> Result<bool, MarkerRegionError> {
        let mut state = self.state.lock().unwrap();

        if state.simulate_errors {
            return Err(MarkerRegionError::OperationError("Simulated error".to_string()));
        }

        updated_marker.validate()?;

        if let Some(marker) = state.markers.iter_mut().find(|m| m.id == Some(id)) {
            *marker = updated_marker;
            marker.id = Some(id); // Preserve the ID

            // Re-sort markers
            state.markers.sort_by(|a, b| {
                a.position_seconds()
                    .partial_cmp(&b.position_seconds())
                    .unwrap_or(std::cmp::Ordering::Equal)
            });

            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Update a region by ID
    pub fn update_region(&self, id: u32, updated_region: Region) -> Result<bool, MarkerRegionError> {
        let mut state = self.state.lock().unwrap();

        if state.simulate_errors {
            return Err(MarkerRegionError::OperationError("Simulated error".to_string()));
        }

        updated_region.validate()?;

        if let Some(region) = state.regions.iter_mut().find(|r| r.id == Some(id)) {
            *region = updated_region;
            region.id = Some(id); // Preserve the ID

            // Re-sort regions
            state.regions.sort_by(|a, b| {
                a.start_seconds()
                    .partial_cmp(&b.start_seconds())
                    .unwrap_or(std::cmp::Ordering::Equal)
            });

            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl Default for MockMarkerRegionService {
    fn default() -> Self {
        Self::new()
    }
}

impl MarkerSource for MockMarkerRegionService {
    fn get_markers(&self) -> Result<Vec<Marker>, MarkerRegionError> {
        let state = self.state.lock().unwrap();

        if state.simulate_errors {
            return Err(MarkerRegionError::SourceError("Simulated marker fetch error".to_string()));
        }

        Ok(state.markers.clone())
    }

    fn source_name(&self) -> &'static str {
        "Mock Marker Source"
    }

    fn is_writable(&self) -> bool {
        let state = self.state.lock().unwrap();
        state.is_writable
    }
}

impl RegionSource for MockMarkerRegionService {
    fn get_regions(&self) -> Result<Vec<Region>, MarkerRegionError> {
        let state = self.state.lock().unwrap();

        if state.simulate_errors {
            return Err(MarkerRegionError::SourceError("Simulated region fetch error".to_string()));
        }

        Ok(state.regions.clone())
    }

    fn source_name(&self) -> &'static str {
        "Mock Region Source"
    }

    fn is_writable(&self) -> bool {
        let state = self.state.lock().unwrap();
        state.is_writable
    }
}

impl MarkerRegionSource for MockMarkerRegionService {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_service_creation() {
        let service = MockMarkerRegionService::new();
        assert_eq!(service.marker_count(), 0);
        assert_eq!(service.region_count(), 0);

        let sample_service = MockMarkerRegionService::with_sample_data();
        assert!(sample_service.marker_count() > 0);
        assert!(sample_service.region_count() > 0);
    }

    #[test]
    fn test_add_marker() {
        let service = MockMarkerRegionService::new();
        let marker = Marker::from_seconds(30.0, "Test Marker".to_string());

        let id = service.add_marker(marker).unwrap();
        assert_eq!(service.marker_count(), 1);

        let markers = service.get_markers().unwrap();
        assert_eq!(markers[0].id, Some(id));
        assert_eq!(markers[0].name, "Test Marker");
    }

    #[test]
    fn test_add_region() {
        let service = MockMarkerRegionService::new();
        let region = Region::from_seconds(10.0, 40.0, "Test Region".to_string()).unwrap();

        let id = service.add_region(region).unwrap();
        assert_eq!(service.region_count(), 1);

        let regions = service.get_regions().unwrap();
        assert_eq!(regions[0].id, Some(id));
        assert_eq!(regions[0].name, "Test Region");
    }

    #[test]
    fn test_marker_sorting() {
        let service = MockMarkerRegionService::new();

        // Add markers out of order
        service.add_marker(Marker::from_seconds(60.0, "Third".to_string())).unwrap();
        service.add_marker(Marker::from_seconds(20.0, "First".to_string())).unwrap();
        service.add_marker(Marker::from_seconds(40.0, "Second".to_string())).unwrap();

        let markers = service.get_markers().unwrap();
        assert_eq!(markers[0].name, "First");
        assert_eq!(markers[1].name, "Second");
        assert_eq!(markers[2].name, "Third");
    }

    #[test]
    fn test_remove_marker() {
        let service = MockMarkerRegionService::new();
        let marker = Marker::from_seconds(30.0, "Test".to_string());

        let id = service.add_marker(marker).unwrap();
        assert_eq!(service.marker_count(), 1);

        assert!(service.remove_marker(id).unwrap());
        assert_eq!(service.marker_count(), 0);

        // Removing non-existent marker should return false
        assert!(!service.remove_marker(999).unwrap());
    }

    #[test]
    fn test_error_simulation() {
        let service = MockMarkerRegionService::new();
        service.set_simulate_errors(true);

        let marker = Marker::from_seconds(30.0, "Test".to_string());
        assert!(service.add_marker(marker).is_err());
        assert!(service.get_markers().is_err());
    }

    #[test]
    fn test_marker_region_source_trait() {
        let service = MockMarkerRegionService::with_sample_data();

        let summary = service.get_summary().unwrap();
        assert!(summary.marker_count > 0);
        assert!(summary.region_count > 0);
        assert!(summary.is_writable);

        let nav_points = service.get_navigation_points().unwrap();
        assert!(!nav_points.is_empty());

        // Navigation points should be sorted
        for i in 1..nav_points.len() {
            assert!(nav_points[i-1].position_seconds() <= nav_points[i].position_seconds());
        }
    }

    #[test]
    fn test_complex_data() {
        let service = MockMarkerRegionService::with_complex_data();

        let _markers = service.get_markers().unwrap();
        let regions = service.get_regions().unwrap();

        // Should have overlapping regions
        assert!(regions[0].overlaps_with(&regions[1]));

        // Validation should catch overlaps
        let validation_errors = service.validate_all().unwrap();
        assert!(!validation_errors.is_empty());
    }

    #[test]
    fn test_update_operations() {
        let service = MockMarkerRegionService::new();
        let marker = Marker::from_seconds(30.0, "Original".to_string());

        let id = service.add_marker(marker).unwrap();

        let updated_marker = Marker::from_seconds(45.0, "Updated".to_string());
        assert!(service.update_marker(id, updated_marker).unwrap());

        let markers = service.get_markers().unwrap();
        assert_eq!(markers[0].name, "Updated");
        assert_eq!(markers[0].position_seconds(), 45.0);
    }
}
