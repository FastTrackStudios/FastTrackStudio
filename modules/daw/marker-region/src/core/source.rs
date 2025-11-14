//! Marker Region Source Trait
//!
//! This module defines the core trait for sources that can provide both markers
//! and regions. This is the primary interface for accessing timeline navigation
//! data from different DAW implementations.

use crate::core::{Marker, Region, MarkerRegionError, MarkerSource, RegionSource};

/// Combined trait for sources that provide both markers and regions
pub trait MarkerRegionSource: MarkerSource + RegionSource + Send + Sync {
    /// Get all markers and regions from the source
    fn get_all(&self) -> Result<(Vec<Marker>, Vec<Region>), MarkerRegionError> {
        let markers = self.get_markers()?;
        let regions = self.get_regions()?;
        Ok((markers, regions))
    }

    /// Get all navigation points (markers and region boundaries) sorted by time
    fn get_navigation_points(&self) -> Result<Vec<NavigationPoint>, MarkerRegionError> {
        let markers = self.get_markers()?;
        let regions = self.get_regions()?;

        let mut points = Vec::new();

        // Add markers as navigation points
        for marker in markers {
            points.push(NavigationPoint::Marker(marker));
        }

        // Add region start and end points as navigation points
        for region in regions {
            points.push(NavigationPoint::RegionStart(region.clone()));
            points.push(NavigationPoint::RegionEnd(region));
        }

        // Sort by time position
        points.sort_by(|a, b| {
            a.position_seconds()
                .partial_cmp(&b.position_seconds())
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        Ok(points)
    }

    /// Find the next navigation point after the given time
    fn get_next_navigation_point(&self, after_seconds: f64) -> Result<Option<NavigationPoint>, MarkerRegionError> {
        let points = self.get_navigation_points()?;
        Ok(points.into_iter()
           .find(|p| p.position_seconds() > after_seconds))
    }

    /// Find the previous navigation point before the given time
    fn get_previous_navigation_point(&self, before_seconds: f64) -> Result<Option<NavigationPoint>, MarkerRegionError> {
        let points = self.get_navigation_points()?;
        Ok(points.into_iter()
           .filter(|p| p.position_seconds() < before_seconds)
           .last())
    }

    /// Get markers and regions within a specific time range
    fn get_all_in_range(&self, start_seconds: f64, end_seconds: f64) -> Result<(Vec<Marker>, Vec<Region>), MarkerRegionError> {
        let markers = self.get_markers_in_range(start_seconds, end_seconds)?;
        let regions = self.get_regions_in_range(start_seconds, end_seconds)?;
        Ok((markers, regions))
    }

    /// Check if the source has any markers or regions
    fn has_content(&self) -> Result<bool, MarkerRegionError> {
        let markers = self.get_markers()?;
        let regions = self.get_regions()?;
        Ok(!markers.is_empty() || !regions.is_empty())
    }

    /// Get a summary of the content in this source
    fn get_summary(&self) -> Result<SourceSummary, MarkerRegionError> {
        let markers = self.get_markers()?;
        let regions = self.get_regions()?;

        let total_duration = {
            let mut max_time = 0.0f64;

            for marker in &markers {
                max_time = max_time.max(marker.position_seconds());
            }

            for region in &regions {
                max_time = max_time.max(region.end_seconds());
            }

            max_time
        };

        Ok(SourceSummary {
            source_name: MarkerSource::source_name(self).to_string(),
            marker_count: markers.len(),
            region_count: regions.len(),
            total_duration,
            is_writable: MarkerSource::is_writable(self),
        })
    }

    /// Validate all markers and regions in the source
    fn validate_all(&self) -> Result<Vec<String>, MarkerRegionError> {
        let mut validation_errors = Vec::new();

        // Validate markers
        let markers = self.get_markers()?;
        for (i, marker) in markers.iter().enumerate() {
            if let Err(error) = marker.validate() {
                validation_errors.push(format!("Marker {}: {}", i + 1, error));
            }
        }

        // Validate regions
        let regions = self.get_regions()?;
        for (i, region) in regions.iter().enumerate() {
            if let Err(error) = region.validate() {
                validation_errors.push(format!("Region {}: {}", i + 1, error));
            }
        }

        // Check for overlapping regions (if that's considered invalid for this source)
        for (i, region1) in regions.iter().enumerate() {
            for (_j, region2) in regions.iter().enumerate().skip(i + 1) {
                if region1.overlaps_with(region2) {
                    validation_errors.push(format!(
                        "Regions '{}' and '{}' overlap: [{}-{}] and [{}-{}]",
                        region1.name, region2.name,
                        region1.start_seconds(), region1.end_seconds(),
                        region2.start_seconds(), region2.end_seconds()
                    ));
                }
            }
        }

        Ok(validation_errors)
    }
}

/// Navigation point that can be either a marker or region boundary
#[derive(Debug, Clone, PartialEq)]
pub enum NavigationPoint {
    /// A marker at a specific time
    Marker(Marker),
    /// The start of a region
    RegionStart(Region),
    /// The end of a region
    RegionEnd(Region),
}

impl NavigationPoint {
    /// Get the time position of this navigation point
    pub fn position_seconds(&self) -> f64 {
        match self {
            NavigationPoint::Marker(marker) => marker.position_seconds(),
            NavigationPoint::RegionStart(region) => region.start_seconds(),
            NavigationPoint::RegionEnd(region) => region.end_seconds(),
        }
    }

    /// Get a display name for this navigation point
    pub fn name(&self) -> String {
        match self {
            NavigationPoint::Marker(marker) => marker.name.clone(),
            NavigationPoint::RegionStart(region) => format!("{} (start)", region.name),
            NavigationPoint::RegionEnd(region) => format!("{} (end)", region.name),
        }
    }

    /// Get a display string for this navigation point
    pub fn display_string(&self) -> String {
        match self {
            NavigationPoint::Marker(marker) => format!("Marker: {}", marker.display_string()),
            NavigationPoint::RegionStart(region) => format!("Region Start: {} @ {:.3}s", region.name, region.start_seconds()),
            NavigationPoint::RegionEnd(region) => format!("Region End: {} @ {:.3}s", region.name, region.end_seconds()),
        }
    }

    /// Check if this navigation point is a marker
    pub fn is_marker(&self) -> bool {
        matches!(self, NavigationPoint::Marker(_))
    }

    /// Check if this navigation point is a region boundary
    pub fn is_region_boundary(&self) -> bool {
        matches!(self, NavigationPoint::RegionStart(_) | NavigationPoint::RegionEnd(_))
    }

    /// Get the marker if this is a marker navigation point
    pub fn as_marker(&self) -> Option<&Marker> {
        match self {
            NavigationPoint::Marker(marker) => Some(marker),
            _ => None,
        }
    }

    /// Get the region if this is a region navigation point
    pub fn as_region(&self) -> Option<&Region> {
        match self {
            NavigationPoint::RegionStart(region) | NavigationPoint::RegionEnd(region) => Some(region),
            _ => None,
        }
    }
}

impl std::fmt::Display for NavigationPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display_string())
    }
}

/// Summary information about a marker/region source
#[derive(Debug, Clone, PartialEq)]
pub struct SourceSummary {
    /// Name of the source
    pub source_name: String,
    /// Number of markers in the source
    pub marker_count: usize,
    /// Number of regions in the source
    pub region_count: usize,
    /// Total duration covered by markers and regions
    pub total_duration: f64,
    /// Whether the source supports writing
    pub is_writable: bool,
}

impl std::fmt::Display for SourceSummary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Source: {}", self.source_name)?;
        writeln!(f, "  Markers: {}", self.marker_count)?;
        writeln!(f, "  Regions: {}", self.region_count)?;
        writeln!(f, "  Duration: {:.3}s", self.total_duration)?;
        writeln!(f, "  Writable: {}", self.is_writable)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // use primitives::{Position, TimeRange}; // unused in tests

    // Mock implementation for testing
    struct MockMarkerRegionSource {
        markers: Vec<Marker>,
        regions: Vec<Region>,
    }

    impl MockMarkerRegionSource {
        fn new() -> Self {
            Self {
                markers: vec![
                    Marker::from_seconds(10.0, "Intro".to_string()),
                    Marker::from_seconds(30.0, "Verse".to_string()),
                    Marker::from_seconds(60.0, "Chorus".to_string()),
                ],
                regions: vec![
                    Region::from_seconds(5.0, 25.0, "Section A".to_string()).unwrap(),
                    Region::from_seconds(40.0, 80.0, "Section B".to_string()).unwrap(),
                ],
            }
        }
    }

    impl MarkerSource for MockMarkerRegionSource {
        fn get_markers(&self) -> Result<Vec<Marker>, MarkerRegionError> {
            Ok(self.markers.clone())
        }

        fn source_name(&self) -> &'static str {
            "Mock Source"
        }
    }

    impl RegionSource for MockMarkerRegionSource {
        fn get_regions(&self) -> Result<Vec<Region>, MarkerRegionError> {
            Ok(self.regions.clone())
        }

        fn source_name(&self) -> &'static str {
            "Mock Source"
        }
    }

    impl MarkerRegionSource for MockMarkerRegionSource {}

    #[test]
    fn test_navigation_points() {
        let source = MockMarkerRegionSource::new();
        let points = source.get_navigation_points().unwrap();

        // Should have 3 markers + 4 region boundaries = 7 points
        assert_eq!(points.len(), 7);

        // Check that they're sorted by time
        for i in 1..points.len() {
            assert!(points[i-1].position_seconds() <= points[i].position_seconds());
        }

        // First point should be region start at 5.0s
        assert_eq!(points[0].position_seconds(), 5.0);
        assert!(points[0].is_region_boundary());
    }

    #[test]
    fn test_next_previous_navigation() {
        let source = MockMarkerRegionSource::new();

        let next = source.get_next_navigation_point(15.0).unwrap().unwrap();
        assert_eq!(next.position_seconds(), 25.0); // End of Section A

        let prev = source.get_previous_navigation_point(15.0).unwrap().unwrap();
        assert_eq!(prev.position_seconds(), 10.0); // Intro marker
    }

    #[test]
    fn test_source_summary() {
        let source = MockMarkerRegionSource::new();
        let summary = source.get_summary().unwrap();

        assert_eq!(summary.marker_count, 3);
        assert_eq!(summary.region_count, 2);
        assert_eq!(summary.total_duration, 80.0); // Max time from Section B end
        assert!(!summary.is_writable);
    }

    #[test]
    fn test_navigation_point_display() {
        let marker = Marker::from_seconds(30.0, "Test Marker".to_string());
        let nav_point = NavigationPoint::Marker(marker);

        let display = nav_point.display_string();
        assert!(display.contains("Marker:"));
        assert!(display.contains("Test Marker"));
        assert!(display.contains("30"));
    }
}
