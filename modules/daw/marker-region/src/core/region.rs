//! Region Domain Model
//!
//! This module defines the core Region struct and related types for managing
//! timeline regions in a DAW. Regions represent spans of time that can be
//! used for arrangement sections, loop ranges, and workflow organization.

use crate::core::MarkerRegionError;
use primitives::TimeRange;
use specta::Type;

/// A region represents a named span of time on the timeline
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, Type)]
pub struct Region {
    /// Unique identifier for this region (if supported by the source)
    pub id: Option<u32>,

    /// Time range that this region spans
    pub time_range: TimeRange,

    /// Human-readable name for this region
    pub name: String,

    /// Color for visual representation (RGB value, if supported)
    pub color: Option<u32>,

    /// Implementation-specific flags (e.g., REAPER's region flags)
    pub flags: Option<u32>,

    /// Whether this region is locked from editing
    pub locked: Option<bool>,

    /// Globally unique identifier (if supported by the source)
    pub guid: Option<String>,
}

impl Region {
    /// Create a new region with the specified time range and name
    pub fn new(time_range: TimeRange, name: String) -> Result<Self, MarkerRegionError> {
        if time_range.start.time.to_seconds() >= time_range.end.time.to_seconds() {
            return Err(MarkerRegionError::InvalidRegion {
                start: time_range.start.time.to_seconds(),
                end: time_range.end.time.to_seconds(),
            });
        }

        Ok(Self {
            id: None,
            time_range,
            name,
            color: None,
            flags: None,
            locked: None,
            guid: None,
        })
    }

    /// Create a region from start and end times in seconds
    pub fn from_seconds(
        start_seconds: f64,
        end_seconds: f64,
        name: String,
    ) -> Result<Self, MarkerRegionError> {
        if start_seconds >= end_seconds {
            return Err(MarkerRegionError::InvalidRegion {
                start: start_seconds,
                end: end_seconds,
            });
        }

        let time_range = TimeRange::from_seconds(start_seconds, end_seconds);
        Self::new(time_range, name)
    }

    /// Create a region with full metadata
    pub fn new_full(
        id: Option<u32>,
        time_range: TimeRange,
        name: String,
        color: Option<u32>,
        flags: Option<u32>,
        locked: Option<bool>,
        guid: Option<String>,
    ) -> Result<Self, MarkerRegionError> {
        if time_range.start.time.to_seconds() >= time_range.end.time.to_seconds() {
            return Err(MarkerRegionError::InvalidRegion {
                start: time_range.start.time.to_seconds(),
                end: time_range.end.time.to_seconds(),
            });
        }

        Ok(Self {
            id,
            time_range,
            name,
            color,
            flags,
            locked,
            guid,
        })
    }

    /// Get the start position in seconds
    pub fn start_seconds(&self) -> f64 {
        self.time_range.start.time.to_seconds()
    }

    /// Get the end position in seconds
    pub fn end_seconds(&self) -> f64 {
        self.time_range.end.time.to_seconds()
    }

    /// Get the duration in seconds
    pub fn duration_seconds(&self) -> f64 {
        self.end_seconds() - self.start_seconds()
    }

    /// Get the musical start position (measure.beat.subdivision)
    pub fn musical_start_position(&self) -> String {
        self.time_range.start.musical_position_string()
    }

    /// Get the musical end position (measure.beat.subdivision)
    pub fn musical_end_position(&self) -> String {
        self.time_range.end.musical_position_string()
    }

    /// Check if this region contains the specified time position
    pub fn contains_position(&self, seconds: f64) -> bool {
        seconds >= self.start_seconds() && seconds <= self.end_seconds()
    }

    /// Check if this region overlaps with another region
    pub fn overlaps_with(&self, other: &Region) -> bool {
        self.start_seconds() < other.end_seconds() && self.end_seconds() > other.start_seconds()
    }

    /// Check if this region is completely within another region
    pub fn is_within(&self, other: &Region) -> bool {
        self.start_seconds() >= other.start_seconds() && self.end_seconds() <= other.end_seconds()
    }

    /// Check if this region completely contains another region
    pub fn contains_region(&self, other: &Region) -> bool {
        other.is_within(self)
    }

    /// Check if this region intersects with a time range
    pub fn intersects_range(&self, start_seconds: f64, end_seconds: f64) -> bool {
        self.start_seconds() < end_seconds && self.end_seconds() > start_seconds
    }

    /// Get the intersection with another region (if any)
    pub fn intersection_with(&self, other: &Region) -> Option<Region> {
        if !self.overlaps_with(other) {
            return None;
        }

        let start_seconds = self.start_seconds().max(other.start_seconds());
        let end_seconds = self.end_seconds().min(other.end_seconds());

        let intersection_name = format!("{} ∩ {}", self.name, other.name);

        Region::from_seconds(start_seconds, end_seconds, intersection_name).ok()
    }

    /// Validate the region data
    pub fn validate(&self) -> Result<(), MarkerRegionError> {
        // Check start is before end
        if self.start_seconds() >= self.end_seconds() {
            return Err(MarkerRegionError::InvalidRegion {
                start: self.start_seconds(),
                end: self.end_seconds(),
            });
        }

        // Check positions are not negative
        if self.start_seconds() < 0.0 {
            return Err(MarkerRegionError::InvalidMarkerPosition(
                self.start_seconds(),
            ));
        }

        // Check name is not empty
        if self.name.trim().is_empty() {
            return Err(MarkerRegionError::InvalidRegionName(self.name.clone()));
        }

        Ok(())
    }

    /// Update the region's time range
    pub fn set_time_range(&mut self, time_range: TimeRange) -> Result<(), MarkerRegionError> {
        if time_range.start.time.to_seconds() >= time_range.end.time.to_seconds() {
            return Err(MarkerRegionError::InvalidRegion {
                start: time_range.start.time.to_seconds(),
                end: time_range.end.time.to_seconds(),
            });
        }
        self.time_range = time_range;
        Ok(())
    }

    /// Update the region's name
    pub fn set_name(&mut self, name: String) -> Result<(), MarkerRegionError> {
        if name.trim().is_empty() {
            return Err(MarkerRegionError::InvalidRegionName(name));
        }
        self.name = name;
        Ok(())
    }

    /// Move the region by a time offset (preserving duration)
    pub fn move_by_offset(&mut self, offset_seconds: f64) -> Result<(), MarkerRegionError> {
        let new_start = self.start_seconds() + offset_seconds;
        let new_end = self.end_seconds() + offset_seconds;

        if new_start < 0.0 {
            return Err(MarkerRegionError::InvalidMarkerPosition(new_start));
        }

        self.time_range = TimeRange::from_seconds(new_start, new_end);
        Ok(())
    }

    /// Resize the region by changing the end position
    pub fn resize_to(&mut self, new_end_seconds: f64) -> Result<(), MarkerRegionError> {
        if self.start_seconds() >= new_end_seconds {
            return Err(MarkerRegionError::InvalidRegion {
                start: self.start_seconds(),
                end: new_end_seconds,
            });
        }

        self.time_range = TimeRange::from_seconds(self.start_seconds(), new_end_seconds);
        Ok(())
    }

    /// Get a display string for this region
    pub fn display_string(&self) -> String {
        format!(
            "{} [{:.3}s - {:.3}s] ({} - {}) [Δ{:.3}s]",
            self.name,
            self.start_seconds(),
            self.end_seconds(),
            self.musical_start_position(),
            self.musical_end_position(),
            self.duration_seconds()
        )
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

    /// Split the region at a given position, creating two regions
    pub fn split_at(&self, split_seconds: f64) -> Result<(Region, Region), MarkerRegionError> {
        if !self.contains_position(split_seconds) {
            return Err(MarkerRegionError::PositionOutOfBounds {
                position: split_seconds,
                max_position: self.end_seconds(),
            });
        }

        let first_name = format!("{} (1)", self.name);
        let second_name = format!("{} (2)", self.name);

        let first_region = Region::from_seconds(self.start_seconds(), split_seconds, first_name)?;
        let second_region = Region::from_seconds(split_seconds, self.end_seconds(), second_name)?;

        Ok((first_region, second_region))
    }
}

impl Default for Region {
    fn default() -> Self {
        // Safe unwrap because we know 0.0 < 1.0
        Self::from_seconds(0.0, 1.0, "Untitled Region".to_string()).unwrap()
    }
}

impl std::fmt::Display for Region {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display_string())
    }
}

/// Trait for sources that can provide regions
pub trait RegionSource: Send + Sync {
    /// Get all regions from the source
    fn get_regions(&self) -> Result<Vec<Region>, MarkerRegionError>;

    /// Get a specific region by ID (if supported)
    fn get_region_by_id(&self, id: u32) -> Result<Option<Region>, MarkerRegionError> {
        let regions = self.get_regions()?;
        Ok(regions.into_iter().find(|r| r.id == Some(id)))
    }

    /// Get regions that intersect with a time range
    fn get_regions_in_range(
        &self,
        start_seconds: f64,
        end_seconds: f64,
    ) -> Result<Vec<Region>, MarkerRegionError> {
        let regions = self.get_regions()?;
        Ok(regions
            .into_iter()
            .filter(|r| r.intersects_range(start_seconds, end_seconds))
            .collect())
    }

    /// Get the region that contains a specific time position
    fn get_region_containing(&self, seconds: f64) -> Result<Option<Region>, MarkerRegionError> {
        let regions = self.get_regions()?;
        Ok(regions.into_iter().find(|r| r.contains_position(seconds)))
    }

    /// Get all regions that overlap with a given region
    fn get_overlapping_regions(&self, region: &Region) -> Result<Vec<Region>, MarkerRegionError> {
        let regions = self.get_regions()?;
        Ok(regions
            .into_iter()
            .filter(|r| r.overlaps_with(region))
            .collect())
    }

    /// Get a human-readable name for this region source
    fn source_name(&self) -> &'static str;

    /// Check if this source supports creating/modifying regions
    fn is_writable(&self) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_region_creation() {
        let region = Region::from_seconds(10.0, 30.0, "Verse 1".to_string()).unwrap();

        assert_eq!(region.start_seconds(), 10.0);
        assert_eq!(region.end_seconds(), 30.0);
        assert_eq!(region.duration_seconds(), 20.0);
        assert_eq!(region.name, "Verse 1");
        assert!(region.id.is_none());
    }

    #[test]
    fn test_invalid_region() {
        // Start after end should fail
        assert!(Region::from_seconds(30.0, 10.0, "Invalid".to_string()).is_err());

        // Start equals end should fail
        assert!(Region::from_seconds(10.0, 10.0, "Invalid".to_string()).is_err());
    }

    #[test]
    fn test_region_contains() {
        let region = Region::from_seconds(10.0, 30.0, "Test".to_string()).unwrap();

        assert!(region.contains_position(15.0));
        assert!(region.contains_position(10.0)); // inclusive start
        assert!(region.contains_position(30.0)); // inclusive end
        assert!(!region.contains_position(5.0));
        assert!(!region.contains_position(35.0));
    }

    #[test]
    fn test_region_overlap() {
        let region1 = Region::from_seconds(10.0, 30.0, "A".to_string()).unwrap();
        let region2 = Region::from_seconds(20.0, 40.0, "B".to_string()).unwrap();
        let region3 = Region::from_seconds(50.0, 60.0, "C".to_string()).unwrap();

        assert!(region1.overlaps_with(&region2));
        assert!(region2.overlaps_with(&region1));
        assert!(!region1.overlaps_with(&region3));
    }

    #[test]
    fn test_region_within() {
        let outer = Region::from_seconds(10.0, 50.0, "Outer".to_string()).unwrap();
        let inner = Region::from_seconds(20.0, 30.0, "Inner".to_string()).unwrap();
        let overlapping = Region::from_seconds(5.0, 25.0, "Overlapping".to_string()).unwrap();

        assert!(inner.is_within(&outer));
        assert!(outer.contains_region(&inner));
        assert!(!overlapping.is_within(&outer));
        assert!(!outer.contains_region(&overlapping));
    }

    #[test]
    fn test_region_intersection() {
        let region1 = Region::from_seconds(10.0, 30.0, "A".to_string()).unwrap();
        let region2 = Region::from_seconds(20.0, 40.0, "B".to_string()).unwrap();
        let region3 = Region::from_seconds(50.0, 60.0, "C".to_string()).unwrap();

        let intersection = region1.intersection_with(&region2).unwrap();
        assert_eq!(intersection.start_seconds(), 20.0);
        assert_eq!(intersection.end_seconds(), 30.0);

        assert!(region1.intersection_with(&region3).is_none());
    }

    #[test]
    fn test_region_move() {
        let mut region = Region::from_seconds(10.0, 30.0, "Test".to_string()).unwrap();

        region.move_by_offset(5.0).unwrap();
        assert_eq!(region.start_seconds(), 15.0);
        assert_eq!(region.end_seconds(), 35.0);
        assert_eq!(region.duration_seconds(), 20.0);

        // Moving to negative position should fail
        assert!(region.move_by_offset(-20.0).is_err());
    }

    #[test]
    fn test_region_resize() {
        let mut region = Region::from_seconds(10.0, 30.0, "Test".to_string()).unwrap();

        region.resize_to(40.0).unwrap();
        assert_eq!(region.start_seconds(), 10.0);
        assert_eq!(region.end_seconds(), 40.0);
        assert_eq!(region.duration_seconds(), 30.0);

        // Resizing to before start should fail
        assert!(region.resize_to(5.0).is_err());
    }

    #[test]
    fn test_region_split() {
        let region = Region::from_seconds(10.0, 30.0, "Test".to_string()).unwrap();

        let (first, second) = region.split_at(20.0).unwrap();
        assert_eq!(first.start_seconds(), 10.0);
        assert_eq!(first.end_seconds(), 20.0);
        assert_eq!(second.start_seconds(), 20.0);
        assert_eq!(second.end_seconds(), 30.0);

        // Split outside region should fail
        assert!(region.split_at(5.0).is_err());
        assert!(region.split_at(35.0).is_err());
    }

    #[test]
    fn test_region_color() {
        let mut region = Region::default();

        region.set_rgb_color(255, 128, 64);
        assert_eq!(region.rgb_color(), Some((255, 128, 64)));

        // Test color conversion
        assert_eq!(region.color, Some(0xFF8040));
    }
}
