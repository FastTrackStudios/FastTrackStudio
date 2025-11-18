//! REAPER API implementation of MarkerRegionSource
//!
//! This module provides a REAPER-specific implementation of MarkerRegionSource.
//! Currently implemented as a stub that returns "not supported" errors until
//! REAPER dependencies are available.

use crate::core::{
    Marker, MarkerRegionError, MarkerRegionSource, MarkerSource, Region, RegionSource,
};

/// REAPER API implementation of MarkerRegionSource (stub)
pub struct ReaperMarkerRegionSource;

impl ReaperMarkerRegionSource {
    /// Create a new REAPER marker region source
    pub fn new() -> Self {
        Self
    }
}

impl MarkerSource for ReaperMarkerRegionSource {
    fn get_markers(&self) -> Result<Vec<Marker>, MarkerRegionError> {
        Err(MarkerRegionError::NotSupported(
            "REAPER integration not yet implemented".to_string(),
        ))
    }

    fn source_name(&self) -> &'static str {
        "REAPER API (stub)"
    }

    fn is_writable(&self) -> bool {
        false
    }
}

impl RegionSource for ReaperMarkerRegionSource {
    fn get_regions(&self) -> Result<Vec<Region>, MarkerRegionError> {
        Err(MarkerRegionError::NotSupported(
            "REAPER integration not yet implemented".to_string(),
        ))
    }

    fn source_name(&self) -> &'static str {
        "REAPER API (stub)"
    }

    fn is_writable(&self) -> bool {
        false
    }
}

impl MarkerRegionSource for ReaperMarkerRegionSource {}

impl Default for ReaperMarkerRegionSource {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reaper_source_creation() {
        let source = ReaperMarkerRegionSource::new();
        assert_eq!(MarkerSource::source_name(&source), "REAPER API (stub)");
        assert!(!MarkerSource::is_writable(&source));
    }

    #[test]
    fn test_reaper_source_not_supported() {
        let source = ReaperMarkerRegionSource::new();

        let markers_result = source.get_markers();
        assert!(matches!(
            markers_result,
            Err(MarkerRegionError::NotSupported(_))
        ));

        let regions_result = source.get_regions();
        assert!(matches!(
            regions_result,
            Err(MarkerRegionError::NotSupported(_))
        ));
    }
}
