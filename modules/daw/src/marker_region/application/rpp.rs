//! RPP file implementation of MarkerRegionSource
//!
//! This module provides an RPP file-based implementation of MarkerRegionSource
//! using the rpp-parser crate.

use crate::marker_region::core::{
    Marker, MarkerRegionError, MarkerRegionSource, MarkerSource, Region, RegionSource,
};
use std::path::Path;

#[cfg(feature = "rpp")]
use rpp_parser::{ReaperProject, parse_rpp_file};

/// RPP file-based implementation of MarkerRegionSource
#[cfg(feature = "rpp")]
pub struct RppMarkerRegionSource {
    project: ReaperProject,
}

#[cfg(feature = "rpp")]
impl RppMarkerRegionSource {
    /// Create a new RPP marker region source from a file path
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, Box<dyn std::error::Error>> {
        let content = std::fs::read_to_string(path.as_ref())?;
        Self::from_string(&content)
    }

    /// Create a new RPP marker region source from file content
    pub fn from_string(content: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let rpp_project = parse_rpp_file(content)?;
        let project = ReaperProject::from_rpp_project(&rpp_project)?;
        Ok(Self { project })
    }

    /// Convert parser MarkerRegion to domain Marker
    fn parser_marker_to_domain(mr: &rpp_parser::MarkerRegion) -> Marker {
        Marker::new_full(
            Some(mr.id),
            primitives::Position::from_seconds(mr.position),
            mr.name.clone(),
            Some(mr.color),
            Some(mr.flags),
            Some(mr.locked != 0),
            if mr.guid.is_empty() {
                None
            } else {
                Some(mr.guid.clone())
            },
        )
    }

    /// Convert parser MarkerRegion to domain Region
    fn parser_marker_to_domain_region(mr: &rpp_parser::MarkerRegion) -> Option<Region> {
        let end_position = mr.end_position?;

        let range = primitives::TimeRange::from_seconds(mr.position, end_position);

        Region::new_full(
            Some(mr.id),
            range,
            mr.name.clone(),
            Some(mr.color),
            Some(mr.flags),
            Some(mr.locked != 0),
            if mr.guid.is_empty() {
                None
            } else {
                Some(mr.guid.clone())
            },
        )
        .ok()
    }
}

#[cfg(feature = "rpp")]
impl MarkerSource for RppMarkerRegionSource {
    fn get_markers(&self) -> Result<Vec<Marker>, MarkerRegionError> {
        let mut markers = Vec::new();

        for parser_marker in &self.project.markers_regions.markers {
            markers.push(Self::parser_marker_to_domain(parser_marker));
        }

        Ok(markers)
    }

    fn source_name(&self) -> &'static str {
        "RPP File"
    }

    fn is_writable(&self) -> bool {
        false // RPP files are read-only in this implementation
    }
}

#[cfg(feature = "rpp")]
impl RegionSource for RppMarkerRegionSource {
    fn get_regions(&self) -> Result<Vec<Region>, MarkerRegionError> {
        let mut regions = Vec::new();

        for parser_region in &self.project.markers_regions.regions {
            if let Some(domain_region) = Self::parser_marker_to_domain_region(parser_region) {
                regions.push(domain_region);
            }
        }

        Ok(regions)
    }

    fn source_name(&self) -> &'static str {
        "RPP File"
    }

    fn is_writable(&self) -> bool {
        false // RPP files are read-only in this implementation
    }
}

#[cfg(feature = "rpp")]
impl MarkerRegionSource for RppMarkerRegionSource {}

#[cfg(not(feature = "rpp"))]
/// Placeholder implementation when rpp feature is not enabled
pub struct RppMarkerRegionSource;

#[cfg(not(feature = "rpp"))]
impl RppMarkerRegionSource {
    pub fn from_file<P: AsRef<Path>>(_path: P) -> Result<Self, MarkerRegionError> {
        Err(MarkerRegionError::NotSupported(
            "rpp feature not enabled".to_string(),
        ))
    }

    pub fn from_string(_content: &str) -> Result<Self, MarkerRegionError> {
        Err(MarkerRegionError::NotSupported(
            "rpp feature not enabled".to_string(),
        ))
    }
}

#[cfg(not(feature = "rpp"))]
impl MarkerSource for RppMarkerRegionSource {
    fn get_markers(&self) -> Result<Vec<Marker>, MarkerRegionError> {
        Err(MarkerRegionError::NotSupported(
            "rpp feature not enabled".to_string(),
        ))
    }

    fn source_name(&self) -> &'static str {
        "RPP File (not enabled)"
    }
}

#[cfg(not(feature = "rpp"))]
impl RegionSource for RppMarkerRegionSource {
    fn get_regions(&self) -> Result<Vec<Region>, MarkerRegionError> {
        Err(MarkerRegionError::NotSupported(
            "rpp feature not enabled".to_string(),
        ))
    }

    fn source_name(&self) -> &'static str {
        "RPP File (not enabled)"
    }
}

#[cfg(not(feature = "rpp"))]
impl MarkerRegionSource for RppMarkerRegionSource {}
